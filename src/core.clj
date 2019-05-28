(ns core
  (:require [clj-http.lite.client :as http]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clj-yaml.core :as yaml]
            [matcho.core :refer :all :as matcho])
  (:import [java.util.concurrent Executors TimeUnit ScheduledThreadPoolExecutor])
  (:gen-class))

(set! *warn-on-reflection* true)

(do

(defn => [x & fns]
  (loop [acc {}
         x x
         [f & fs] fns]
    (if (map? f)

      (let [acc (map-assoc (fn [k] ((get f k) x)) acc (keys f))]
        (if (empty? fs) (merge {:_result x} acc) (recur acc (:_result acc x) fs)))

      (let [r (f x)
            acc' (merge acc {:_result r})]
         (if (empty? fs) acc' (recur acc' r fs)))

      )))

(defn =>> [x & fns]
  (loop [acc (if (map? x) x (:_result x))
         x x
         [f & fs] fns]
    (if (map? f)

      (let [acc (map-assoc (fn [k] ((get f k) x)) acc (keys f))]
        (if (empty? fs) (merge {:_result x} acc) (recur acc (:_result acc x) fs)))

      (let [r (f x)
            acc' (merge acc {:_result r})]
        (if (empty? fs) acc' (recur acc' r fs)))

      )))

(defn symbol-str [s]
  (try
  (let [r (symbol s)]
     r
     )
  (catch Exception _
      (str s)))
  )

(defmacro >>=
  ([k f] {k f :_result f})
  ([f] {(keyword (symbol-str f)) f :_result f})
  )

(defmacro >>
  ([k f] {k f})
  ([f] {(keyword (symbol-str f)) f})
  )
) ;; end of combinators definitions

(=>> (=>> (=>> 5 (>> sqr) (>>= sqr) (>>= (partial - 6)))
          :sqr (>> :sqr2 sqr) (partial - 7))
     :sqr2)

( do
;; yaml dummy tests
(= (yaml/generate-string
 [{:name "John Smith", :age 33}
  {:name "Mary Smith", :age 27}])
"- {name: John Smith, age: 33}\n- {name: Mary Smith, age: 27}\n"
)

;; check if we write the same keyword with different value
(= (yaml/parse-string "
- {name: John Smith, age: 33}
- name: Mary Smith
  age: 27
  age: 28
") [{:name "John Smith", :age 33}
    {:name "Mary Smith", :age 28}])
;; the last wins


;; no keywordize
(yaml/parse-string "
- {name: John Smith}
"  :keywords false)
)

(do 
;; define the match testing function
(defn load-matching-test [s]
  (let [res (apply (:command s) (:args s))
        proc (:process s identity)
        res' (proc res)]
     (map (fn [r] (matcho/match res' r)) (:asserts-match s))
     )
  )

(matcho/match {:bar {:foo "Bar" :baz "Baz"}} {:bar {:foo "Bar"}})

(defn print-matching-test [s]
  (let [res (apply (:command s) (:args s))
        proc (:process s identity)
        res' (proc res)]
    (map (fn [r] (println res' "; " r)) (:asserts-match s))
    )
  )

) ;; end of testing functions definitions

(do

;; just to get know it is alive
(defn sqr [x] (* x x))
(= 25 (sqr 5))

(load-matching-test {:command sqr
                     :args [5]
                     :process sqr
                     :asserts-match [625 number?]})
)


;; prepare empty environment

;; clear current database

(do

(defn get-entry' [resp]
  (=> resp
       :body
       #(json/parse-string % true)
       #(:entry % (identity %))
       )
  )

(defn get-entry [resp]
  (:_result (get-entry' resp))
  )

(defn get-entries [url]
  (get-entry (http/get url))
  )

(defn clear-entries [entries]
    (do
      (doseq [ent entries] (http/delete (:fullUrl ent)))
      (count entries))
  )

(defn clear-entries-url [url]
  (clear-entries (get-entries url))
  )

);; end of get/clear definitions

(loop [n 1000]
  (if (> n 0)
    (do
     (->> test-spec1 (insert-data))
     (recur (- n 1)))
    true
 ))

(=> (get-entries "http://localhost:8765/Patient?_count=10000") count)


(#_comment do

  (get-entries "http://localhost:8765/Patient?_count=10000")

  (clear-entries-url  "http://localhost:8765/Patient/")

  (load-matching-test {:command get-entries
                       :args ["http://localhost:8765/Patient/"]
                       :asserts-match [empty?]})
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; parse yaml spec

;; should be read from file

(do

(def test1
"id: simple
desc: Basic one parameter searches
fhir: \"4.0.0\"
base-url: \"http://localhost:8765/\"

data:
    Patient:
      pt-1:
        name: [{family: 'Doe'}]
      pt-2:
        name: [{family: 'John'}]
      pt-3:
        name: [{family: 'Ivan'}]

cases:
  count-correct:
    method: get
    url: \"Patient\"
    asserts:
      as-1:
         - _result: '#(= (count %) 3)'
         - _result: '#(= (empty? %) false)'
      as-2:
         - _result: '#(> (count %) 2)'
         - _result: '#(< (count %) 4)'
  names-correct:
    method: get
    url: \"Patient\"
    id: $pt-1
    asserts:
      as-5:
       - _result: '#(= (map? %) true)'
       - _result:
             name: [{family: 'Doe'}]
      as-7:
       _result:
          name: '#(= (count %) 1)'
      as-8:
       _result:
          name:
             - family: 'Doe'
      as-9:
       _result:
          name:
             - family: '#(string? %)'")


(def test-spec1 (yaml/parse-string test1 :keywords true))

)

(->> (=>> (=> test-spec1 :cases :count-correct :asserts :as-1) :_result get-matcher) :_result)

;; assoc with list
;; map-assoc f {} [:a :b :c] =  {:a (f :a) :b (f :b) :c (f :c)}
(defn map-assoc [f val coll]
  (reduce (fn [m k] (assoc m k (f k))) val coll)
  )

;; prepare modeling environment, insert data

(do

(defn insert-data [spec]
  (let [dt (:data spec)
        burl (:base-url spec)]
    (map-assoc (fn [t] 
     (let [entries (get dt t)] ;; entries of this type
       (map-assoc (fn [eid]
         (let [bdstr (json/generate-string (merge {:resourceType t} (get entries eid)))]
           #_(println (str burl "/" (name t)))
           #_(println bdstr)
           (http/post (str burl "/" (name t)) {:headers {"Content-Type" "application/json; charset=utf-8"}
                                               :body bdstr})
           )) {} (keys entries)))) {} (keys dt))
     )
    )

;; run cases

(defn getCtxKey [ctx ks d]
   (loop [c ctx
          [kx & kss] ks]
     (let [newc (get c kx)]
       (if (nil? newc) d (if (empty? kss) newc (recur newc kss)))
       )
     )
  )

;; (defn get-string-function [k s]
;;   (let [[f & r] (str/split (str/trim (subs (str/trim s) 1)) #"\s+")]
;;      (load-string (format "(fn [res] (%s (get res :%s) %s))" f (name k) (str/join " " r)))
;;      )
;;   )

;; (get-string-function :foo "#  > 0")

(defn get-matcher [m]
  (if (map? m)
    (map-assoc (fn [k] (get-matcher (get m k))) {} (keys m))
    (if (string? m)
      (if (str/starts-with? m "#") (load-string m) m)
      (if (sequential? m)
        (mapv get-matcher m)
        m
        )
      ))
  )

(get-matcher  {:foo {:bar "#(> % 0)"}})

(flatten [1 2 3])

(defn run-cases [spec ctx]
  (let [cases (:cases spec)
        burl (:base-url spec)]
    (map-assoc (fn [casekey] 
         (let [case (get cases casekey)
               method (:method case)
               url (:url case)
               ass (:asserts case)
               caseid (:id case "")
               caseid' (if (str/starts-with? caseid "$")
                         (->> (=> ctx (keyword url) (keyword (subs caseid 1)) :body #(json/parse-string % true) :id) :_result)
                         caseid)]
           ;;(println "id: " caseid')
           (map-assoc (fn [k] (let [a (get ass k)]
                           (load-matching-test {:command (resolve (symbol (str "http/" method)))
                                                :args [(str burl "/" url "/" (str caseid'))]
                                                :process get-entry'
                                                :asserts-match (flatten [(get-matcher a)])})))
                                                   {} (keys ass)))) {} (keys cases))
    ))

);; end of insert-run definitions

;; (let [c (get a ka)
;;                                                                                    b (string? c)
;;                                                                                    c' (if b (str/split c #"\s+") c)
;;                                                                                    b' (if b (empty? (rest c')) true)]
;;                                                                                (if (or (not b) b')
;;                                                                                  #_(assoc {} ka (map-assoc (partial get c) {} (keys c)))
;;                                                                                  (assoc {} ka c)
;;                                                                                  (load-string (format "(fn [res] (%s (%s res) %s))"
;;                                                                                                    (first c')
;;                                                                                                    (name ka)
;;                                                                                                    (str/join " " (rest c'))))))

(matcho/match {:foo [{:bar "Bar"}, {:baz "Baz"}]} {:foo [{:bar "Bar"}]})
(run-cases test-spec1 insdata)

;; (defn func-bind [x & fns]
;;   (loop [acc {}
;;          x x
;;          [f & fs] fns]
;;     (let [r ((:_init f f) x)]
;;      (if (empty? fs) {:_result r :_init (:_init acc r)}
;;         (if (nil? (:_init f)) (recur acc r fs) (recur {:_init x} r fs))))
;;     )
;;   )



;; (func-bind (func-bind 5
;;                       {:i identity :_result identity}
;;                       {:sqr sqr}
;;                       sqr
;;                       sqr
;;                       {:sqr1 sqr})
;;            :i)
;;            sqr)

(def insdata (insert-data test-spec1))

(do
  (def s (=>> (=>> (=> test-spec1 insert-data
                       (>>= :Patient) :pt-1 :body #(json/parse-string % true) (>>= :id-1 :id))
                   :Patient :pt-2 :body #(json/parse-string % true) (>>= :id-2 :id))
              :Patient :pt-3 :body #(json/parse-string % true) (>>= :id-3 :id)))

  (list (:id-1 s) (:id-2 s) (:id-3 s))

  (run-cases insdata test-spec1)
)

;; (defn resolve-first [s]
;;   (let [[f & args] (str/split s #" ")
;;         rf (resolve (symbol f))]
;;     #((partial rf rest) %&)
;;     )
;;   )

;; (>  2 3)
;; (apply (read-string "> 2") 1) 

;; ((resolve-first ">" 2) 1)


