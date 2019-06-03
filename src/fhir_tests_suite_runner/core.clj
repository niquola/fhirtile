(ns fhir-tests-suite-runner.core
  (:require [clj-http.lite.client :as http]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clj-yaml.core :as yaml]
            [matcho.core :refer :all :as matcho :exclude [assert]]
            [clojure.test :only [is] :as test]
            [clojure.set :as set])
  (:import [java.util.concurrent Executors TimeUnit ScheduledThreadPoolExecutor])
  (:gen-class))

(set! *warn-on-reflection* true)

;; assoc with list
;; map-assoc f {} [:a :b :c] =  {:a (f :a) :b (f :b) :c (f :c)}
(defn map-assoc [f val coll]
  (reduce (fn [m k] (assoc m k (f k))) val coll)
  )

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
    (let [r (symbol s)] r )
    (catch Exception _
      (str s)
      )
    )
  )

(defmacro >>=
  ([k f] {k f :_result f})
  ([f] {(keyword (symbol-str f)) f :_result f})
  )

(defmacro >>
  ([k f] {k f})
  ([f] {(keyword (symbol-str f)) f})
  )

(comment

  (=>> (=>> (=>> 5 (>> sqr) (>>= sqr) (>>= (partial - 6)))
            :sqr (>> :sqr2 sqr) (partial - 7))
       :sqr2)
  )


(comment

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

 
;; define the match testing function

(defn load-matching-test [s & {:keys [dry-run] :or {dry-run false}}]
  (let [res (apply (:command s) (:args s))
        proc (:process s identity)
        res' (proc res)]
    (if dry-run
      (doseq [r (:asserts-match s)] (println res' r))
      (map (fn [r] (matcho/match res' r)) (:asserts-match s))
      )
    )
  )

(comment

  (matcho/match {:bar {:foo "Bar" :baz "Baz"}} {:bar {:foo "Bar"}})

  (defn sqr [x] (* x x))
  (= 25 (sqr 5))

  (defn foo [& {:keys [bar] :or {bar 5}}]
    bar
    )

  (foo :bar 6)

  (load-matching-test {:command sqr
                       :args [5]
                       :process sqr
                       :asserts-match [625 number?]} :dry-run false)
  ((comp sqr sqr) 5 )

  )

;; prepare empty environment

(comment :not-implemented)

;; clear current database

;; get body, parse json and take :entry if any
;; caution!: returns in :_result key
(defn get-entry' [resp]
  (=> resp
       :body
       #(json/parse-string % true)
       #(:entry % (identity %))
       )
  )

(def get-entry (comp :_result get-entry'))

(def get-entries (comp get-entry http/get))

(defn clear-entries [entries]
  (do
    (doseq [ent entries] (http/delete (:fullUrl ent)))
    (count entries))
  )

(def clear-entries-url (comp clear-entries get-entries))

(comment

  (loop [n 1000]
    (if (> n 0)
      (do
        (->> test-spec1 (insert-data))
        (recur (- n 1)))
      true
      ))
  
  (=> (get-entries "http://localhost:8765/Patient?_count=10000") count)

  )


(comment

  (get-entries "http://localhost:8765/Patient?_count=10000")

  (clear-entries-url  "http://localhost:8765/Patient/")

  (load-matching-test {:command get-entries
                       :args ["http://localhost:8765/Patient/"]
                       :asserts-match [sequential? empty?]})
  )

;; parse yaml spec
;; should be read from file

(comment

  (def test1 (slurp "/Users/andruiman/devel/fhir-tests-suite-runner/tests/test1.yaml"))
  (def test-spec1 (yaml/parse-string test1 :keywords true))

  )


(comment

  (->> (=>> (=> test-spec1 :cases :count-correct :asserts :as-1) :_result get-matcher) :_result)

  )

;; prepare modeling environment, insert data

(defn insert-data [spec]
  (let [{:keys [data base-url]} spec]
    (map-assoc (fn [t] 
                 (let [entries (get data t) ;; entries of this type
                       _ (println "Clearing database in" (name t) "...")
                       url (str base-url "/" (name t))
                       _ (clear-entries-url  url)
                       _ (assert (every? true? (load-matching-test {:command get-entries
                                                                    :args [url]
                                                                    :asserts-match [empty?]}))
                                 "Error: DB is not cleared")]
                   (println "Inserting database in" (name t) "...")
                   (map-assoc (fn [eid]
                                (let [bdstr
                                      (json/generate-string (merge {:resourceType t} (get entries eid)))]
                                  (http/post url {:headers {"Content-Type" "application/json; charset=utf-8"}
                                                  :body bdstr}))) {} (keys entries)))) {} (keys data))
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

(defn get-matcher [m]
  (if (map? m)
    (map-assoc (fn [k] (get-matcher (get m k))) {} (keys m))
    (if (string? m)
      (if (str/starts-with? m "#") (load-string m) m)
      (if (sequential? m) (mapv get-matcher m) m)))
  )

(comment
  (get-matcher  {:foo {:bar "#(> % 0)"}})
  (flatten [1 2 3])

  (let [{:keys [a b]} {:a 5 :b 6}]
    b)

  (def s (=>> (=>> (=> test-spec1 insert-data
                       (>>= :Patient) :pt-1 :body #(json/parse-string % true) (>>= :id-1 :id))
                   :Patient :pt-2 :body #(json/parse-string % true) (>>= :id-2 :id))
              :Patient :pt-3 :body #(json/parse-string % true) (>>= :id-3 :id)))

  (list (:id-1 s) (:id-2 s) (:id-3 s))

  )

(defn run-cases [spec ctx]
  (let [{:keys [cases base-url]} spec]
    (map-assoc (fn [casekey] 
                 (let [case (get cases casekey)
                       {:keys [method url asserts id] :or {id ""}} case
                       caseid (if (str/starts-with? id "$")
                                (:_result (=> ctx
                                              (keyword url)
                                              (keyword (subs id 1))
                                              :body
                                              #(json/parse-string % true)
                                              :id)) id)]
                   (map-assoc (fn [k] (let [a (get asserts k)]
                                        (load-matching-test
                                         {:command (ns-resolve 'fhir-tests-suite-runner.core (symbol (str "http/" method)))
                                          :args [(str base-url "/" url "/" (str caseid))]
                                          :process get-entry'
                                          :asserts-match (flatten [(get-matcher a)])}))) {} (keys asserts))
                   )) {} (keys cases))
    ))




(defn final-vals [m]
  (if (map? m)
    (final-vals (vals m))
    (if (sequential? m)
      (flatten (mapv final-vals m))
      m))
  )


(final-vals {:a :A :b {:bb :BB :cc {:ccc :CCC :ddd :DDD}}})


(defn -main [ & args]
  (doseq [tfname args]
    (let [
          _ (println "Processing file: " tfname)

          ;; _ (print "Clearing database...")
          ;; _ (clear-entries-url  "http://localhost:8765/Patient/")

          ;; _ (assert (every? true? (load-matching-test {:command get-entries
          ;;                                               :args ["http://localhost:8765/Patient/"]
          ;;                                               :asserts-match [empty?]})) "Error: DB is not cleared")

          ;; _ (println "done")

          ;; ;; change to real path
          ;; test1 (slurp "/Users/andruiman/devel/fhir-tests-suite-runner/tests/test1.yaml")

          _ (print "Reading and parsing file with test cases...")


          test1 (try (slurp tfname)
                     (catch Exception _
                       nil))
          _ (assert (some? test1) "Error: Test file is not read")

          test-spec1 (try (yaml/parse-string test1 :keywords true)
                          (catch Exception _
                            nil))
          _ (assert (some? test-spec1) "Error: Test data is nil")
          _ (println "done")
          ;; _ (println test-spec1)

          _ (println "Inserting modeling environment into DB...")
          insdata (insert-data test-spec1)
          _ (assert (some? insdata) "Error: Inserted data retur is nil")
          _ (println "done")
          ;; _ (println insdata)


          _ (println "Running test cases...")
          tested (run-cases test-spec1 insdata)
          _ (println "The result is: " tested)

          _ (cond
              (some false? (final-vals tested)) (println "Some assert failed")
              :else (println "All asserts held")
              )

          _(println "done")]

      ))
  )

#_(-main "/Users/andruiman/devel/fhir-tests-suite-runner/tests/test1.yaml"
         "/Users/andruiman/devel/fhir-tests-suite-runner/tests/test2.yaml")



