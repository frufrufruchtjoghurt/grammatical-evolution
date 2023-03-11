(ns markusfruhmann.regular-grammar.grammar
  (:require
   [clojure.string :as str]
   [clojure.zip :as zip]
   [instaparse.core :as insta]
   [markusfruhmann.utils :as utils]))

(defn get-nt-and-refs [tree]
  (loop [node (zip/vector-zip tree)
         non-terminals #{}
         references #{}]
    (if (zip/end? node)
      {:non-terminals non-terminals
       :references (disj references nil)}
      (let [node-val (zip/node node)] (if (or (zip/branch? node) (keyword? node-val))
                                        (recur (zip/next node) non-terminals references)
                                        (let [{:keys [non-terminal reference]} node-val]
                                          (recur (zip/next node) (conj non-terminals non-terminal) (conj references reference))))))))

(defn remove-unresolved-references [tree]
  (let [{:keys [non-terminals
                references]} (get-nt-and-refs tree)
        unresolved (reduce #(if (contains? non-terminals %2) %1 (conj %1 %2)) #{} references)]
    (utils/map-tree #(if (contains? unresolved (:reference %)) (assoc % :reference nil) %) tree)))

(defn get-rules-by-non-terminal [tree]
  (as-> tree t
    (flatten t)
    (group-by :non-terminal t)
    (dissoc t nil)))

(defn reduce-rules [rules]
  (reduce (fn [m {:keys [non-terminal terminal reference]}]
            (-> m
                (assoc :non-terminal non-terminal)
                (assoc :rules (conj (:rules m) {:terminal terminal
                                                :reference reference}))))
          {:non-terminal nil
           :rules #{}}
          rules))

(defn format-rules [rules]
  (map (fn [{:keys [terminal reference]}]
         (cond reference
               (format "'%s'%s" terminal (name reference))
               (= "epsilon" terminal)
               terminal
               :else
               (format "'%s'" terminal)))
       rules))

(defn node->rule [{:keys [non-terminal rules]}]
  (format "%s = %s;" (name non-terminal) (str/join "|" (format-rules rules))))

(defn tree->grammar [tree]
  (let [clean-tree (remove-unresolved-references tree)]
    (->> clean-tree
         get-rules-by-non-terminal
         (map (fn [[_ r]] (reduce-rules r)))
         (into [])
         (map #(node->rule %))
         (str/join "\n"))))

(defn find-matches
  [grammar strings]
  (let [parser (insta/parser grammar)]
    (pmap #(-> % parser insta/failure? not) strings)))

(defn grammar-fitness
  "Scores an individual by applying it to the predefined word-map and calculating the f1-score."
  [individual word-map]
  (let [grammar (tree->grammar individual)]
    (-> word-map
        (assoc :valid-words (find-matches grammar (:valid-words word-map)))
        (assoc :invalid-words (find-matches grammar (:invalid-words word-map)))
        (utils/f1-score utils/boolean-reducer))))

(defn grammar-terminate?
  "Is true if the best individual has a score of 99% and is smaller than the median of the generation."
  [best-of-gen population]
  (when (>= (:score best-of-gen) 0.99)
    (->> population
         (map #(:mass %))
         (utils/median)
         (<= (:mass best-of-gen)))))

(defn grammar-pretty-print [{:keys [prog mass score]}]
  (println "Grammar:\n" (tree->grammar prog) "\nSize:" mass "Score:" score))
