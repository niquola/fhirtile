(ns matcho.core)

(defn simple-value? [x]
  (not (or (map? x) (vector? x) (set? x))))

(defn match-compare [p s path]
  (cond

    (and (string? s) (= java.util.regex.Pattern (type p)))
    (when-not (re-find p s)
      {:path path :expected (str "Match regexp: " p) :but s})

    (fn? p)
    (when-not (p s)
      {:path path :expected (pr-str p) :but s})

    :else (when-not (= p s)
            {:path path :expected p :but s})))

(defn match-recur [errors path example pattern]
  (cond
    (and (map? example)
         (map? pattern))
    (reduce (fn [errors [k v]]
              (let [path  (conj path k)
                    ev (get example k)]
                (match-recur errors path ev v)))
            errors pattern)

    (and (vector? pattern)
         (seqable? example))
    (reduce (fn [errors [k v]]
              (let [path (conj path k)
                    ev  (nth (vec example) k nil)]
                (match-recur errors path ev v)))
            errors
            (map (fn [x i] [i x]) pattern (range)))

    :else (if-let [err (match-compare pattern example path)]
            (conj errors err)
            errors)))

(defn match* [example & patterns]
  (reduce (fn [acc pattern] (match-recur acc [] example pattern)) [] patterns))
