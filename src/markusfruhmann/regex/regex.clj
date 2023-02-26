(ns markusfruhmann.regex.regex
  (:require
   [clojure.string :as str]
   [markusfruhmann.utils :as utils]
   [clojure.zip :as zip]))

(defn group-node
  "Inserts round braces as the leftmost and rightmost sibling.
  Return position is the sibling after the opening brace!"
  [node]
  (when (not (zip/branch? node))
    (-> node
        zip/rightmost (zip/insert-right ")")
        zip/leftmost (zip/insert-left "(?:"))))

(defn tree->regex-str
  [tree]
  (loop [node (zip/vector-zip tree)]
    (if (zip/end? node)
      (-> node zip/root flatten str/join)
      (if (zip/branch? node)
        (recur (zip/next node))
        (let [value (zip/node node)]
          (if (string? value)
            (recur (zip/next node))
            (case value
              :& (-> node group-node zip/remove recur)
              :| (-> node group-node zip/remove zip/right (zip/insert-right "|") zip/leftmost recur)
              (-> node group-node zip/remove zip/rightmost (zip/insert-left (name value)) zip/leftmost recur))))))))

(defn find-matches
  "Returns a list of the full match or nil for every word in string-list matched by regex."
  [regex-str string-list]
  (let [pattern (re-pattern regex-str)]
    (map #(-> pattern (re-matches %) (nth 0 nil)) string-list)))

(defn regex-fitness
  "Scores an individual by applying it to the predefined word-map and calculating the f1-score."
  [individual word-map]
  (let [regex-str (tree->regex-str individual)]
    (-> word-map
        (assoc :valid-words (find-matches regex-str (:valid-words word-map)))
        (assoc :invalid-words (find-matches regex-str (:invalid-words word-map)))
        (utils/f1-score utils/boolean-reducer))))

(defn regex-terminate?
  "Is true if the best individual has a score higher than 91% and is smaller than the median of the generation."
  [sorted-generation]
  (let [best-of-gen (first sorted-generation)]
    (when (>= (:score best-of-gen) 0.91)
      (->> sorted-generation
           (map #(:size %))
           (utils/median)
           (<= (:size best-of-gen))))))
