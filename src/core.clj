(ns core
  (:require [clj-http.lite.client :as http]
            [cheshire.core :as json]
            [clojure.string :as str])
  (:import [java.util.concurrent Executors TimeUnit ScheduledThreadPoolExecutor])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn do-get [ctx path & [params]]
  (let [res (http/request
             {:url (str (:base-url ctx) path)
              :method :get
              :throw-exceptions false
              :basic-auth [(:client-id ctx) (:client-secret ctx)]
              :query-params (merge {:_format "json"} (or params {}))})]
    {:status (:status res)
     :body (json/parse-string (:body res) keyword)}))

(defn do-patch [ctx path body]
  (let [res (http/request
             {:url (str (:base-url ctx) path)
              :method :post
              :throw-exceptions false
              :headers {"x-http-method-override" "patch"
                        "content-type" "application/json"}
              :basic-auth [(:client-id ctx) (:client-secret ctx)]
              ;; :query-params {:_method "merge-patch"}
              :body (json/generate-string body)})]
    {:status (:status res)
     :body (json/parse-string (:body res) keyword)}))

(defn do-post [ctx path body]
  (let [res (http/request
             {:url (str (:base-url ctx) path)
              :method :post
              :throw-exceptions false
              :headers {"content-type" "application/json"}
              :basic-auth [(:client-id ctx) (:client-secret ctx)]
              ;; :query-params {:_method "merge-patch"}
              :body (json/generate-string body)})]
    {:status (:status res)
     :body (json/parse-string (:body res) keyword)}))

(defn do-put [ctx path body]
  (let [res (http/request
             {:url (str (:base-url ctx) path)
              :method :put
              :throw-exceptions false
              :headers {"content-type" "application/json"}
              :basic-auth [(:client-id ctx) (:client-secret ctx)]
              :body (json/generate-string body)})]
    {:status (:status res)
     :body (json/parse-string (:body res) keyword)}))


(defn telegram-notify [ctx u]
  (let [link (str "https://aidbox.app/static/console.html#/entities/User/" (:id u))
        title (->>
               [(:userName u)
                (:email u)
                (get-in u [:name :formatted])]
               (filterv identity)
               (str/join "; "))

        photo (if-let [p (:photo u)]
                (str "\n" p) "")

        msg (str "New user: "
                 "txid(" (get-in u [:meta :versionId]) ") "
                 "[" title "](" link ")"
                 photo)]
    (http/request
     {:url "https://api.telegram.org/bot653513017:AAFMeJogI-0LfFRE082f-bOCPtNGyRwmc-g/sendMessage"
      :method :get
      :headers {"content-type" "application/json"}
      :throw-exceptions false
      :body (json/generate-string
             {:chat_id "-270652015"
              :text msg
              :disable_notification true})})))

;; "{:email_address 'niquola@health-samurai.io'
;;     :status 'subscribed'
;;     :merge_fields {:FNAME 'Nikolai'
;;                    :LNAME 'Ryzhikov'}}"
(defn mailchimp-subscribe [ctx u]
  (if-let [secret (ctx :mailchimp-key)]
    (when-let [email (:email u)]
      (let [nm (or (get-in u [:name :formatted]) (:userName u))
            [f l] (str/split nm #"\s+" 2)
            f (or (get-in u [:name :givenName]) f)
            l (or (get-in u [:name :familyName]) l)
            msg {:email_address email
                 :status "subscribed"
                 :merge_fields (cond-> {}
                                 f (assoc :FNAME f)
                                 l (assoc :LNAME l))}]
        (println "Send to mailchimp: " msg)
        (println 
         (select-keys
          (http/request
           {:url "https://us19.api.mailchimp.com/3.0/lists/7fa57cfd06/members"
            :method :post
            :throw-exceptions false
            :basic-auth ["HealthSamurai" secret]
            :body (json/generate-string msg)})
          [:status :body]))))
    (println "Mailchimp is not configured. Please provide MAILCHIMP_KEY")))

(defn users [ctx txid]
  (:body (do-get ctx "/User/_history" (cond-> {:_format "json"}
                           txid (assoc :_txid txid)))))

(def watch-url "/CrmWatch/new-users")

(defn config [ctx ]
  (:body (do-get ctx watch-url)))

(defn update-config [ctx cfg]
  (:body (do-patch ctx watch-url cfg)))

(defn log-new-user  [r]
  (println (str "NEW USER: txid(" (get-in r [:meta :versionId]) ") "
                (->>
                 [(:userName r)
                  (:email r)
                  (get-in r [:name :formatted])]
                 (filterv identity)
                 (str/join "; ")))))

(defn do-job [ctx]
  (try
    (let [cfg (config ctx)
          txid (or (:last_txid cfg) 0)
          users (users ctx txid)]
      (when (:debug cfg)
        (print "Get users from " txid))
      (if (and (empty? (:entry users)) (:debug cfg))
        (println " .")
        (println (.toString (java.time.LocalDateTime/now)) " "  (count (:entry users))))
      (doseq [{r :resource req :request} (:entry users)]
        (when (= "POST" (:method req))
          (log-new-user r)
          (telegram-notify ctx r)
          (mailchimp-subscribe ctx r)))
      (when-let [new-txid (let [new-txid (:id users)] (when (not (= (str txid) (str new-txid))) new-txid))]
        (println "Update last_txid = " new-txid)
        (println (update-config ctx {:last_txid (Integer/parseInt ^String new-txid)}))))
    (catch Exception e
      (println "ERROR:" e))))

(defn stop [{ex :executor}]
  (when ex
    (println "Stopping..." )
    (.shutdown ^ScheduledThreadPoolExecutor ex)))

(defn init [ctx]
  (let [resp (do-post ctx "/App"
                      {:resourceType "App"
                       :id "crm"
                       :apiVersion 1
                       :type "app"
                       :entities {:CrmWatch {:attrs {:last_txid {:type "integer"}}}}})]
    (when (> (:status resp) 299)
      (throw (Exception. (pr-str resp)))))

  (let [cfg-resp (do-get ctx watch-url)]
    (if (= 404 (:status cfg-resp))
      (let [resp (do-put ctx watch-url {:last_txid 10000})]
        (when (> (:status resp) 299)
          (throw (Exception. (pr-str resp))))
        resp)
      cfg-resp)))

(defn validate-ctx [ctx]
  (assert (:base-url ctx) "BASE_URL is required")
  (assert (:client-id ctx) "CLIENT_ID is required")
  (assert (:client-secret ctx) "CLIENT_SECRET is required")
  (assert (:mailchimp-key ctx) "MAILCHIMP_KEY is required"))

(defn start [ctx]
  (validate-ctx ctx)
  (println "Start with " (:base-url ctx) " timeout " (:timeout ctx))
  (init ctx)
  (let [executor (Executors/newScheduledThreadPool 1)
        fut (.scheduleAtFixedRate executor #(do-job ctx) 0 (or (:timeout ctx) 30) TimeUnit/SECONDS)]
    {:executor executor :future fut}))

(defn -main [& [args]]
  (let [ctx {:base-url      (System/getenv "BASE_URL")
             :client-id     (System/getenv "CLIENT_ID")
             :timeout       (Integer/parseInt (or (System/getenv "POLL_TIMEOUT") "30"))
             :client-secret (System/getenv "CLIENT_SECRET")
             :mailchimp-key (System/getenv "MAILCHIMP_KEY")}]
    (validate-ctx ctx)
    (println "Start with " (:base-url ctx) " timeout " (:timeout ctx))
    (init ctx)
    (loop []
      (do-job ctx)
      (Thread/sleep (* 1000 (:timeout ctx)))
      (recur))))

(comment
  (def ctx
    {:base-url "https://aidbox.app"
     :client-id "crm"
     :debug true
     :timeout 5
     :mailchimp-key "9d998737c7e145a9494b7609c0e066ef-us19"
     :client-secret "nesolonohlebavshi"})

  (validate-ctx ctx)

  (do-put ctx watch-url {:last_txid 2500})

  (init ctx)

  (def state
    (start ctx))

  (stop state)

  (config ctx)

  (do-job ctx)

  (def u (:body (do-get ctx "/User/cac2e7a2-a7bb-44de-9a09-508b8d3692ef")))

  (telegram-notify ctx u)

  (:name u)

  (update-config ctx {:last_txid 2939})

  (mailchimp-subscribe ctx u)

  (update-config ctx {:last_txid 2489})

  )
