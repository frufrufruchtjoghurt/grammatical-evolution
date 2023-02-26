(ns markusfruhmann.regular-grammar.grammar
  (:require
   [clojure.string :as str]
   [clojure.zip :as zip]
   [instaparse.core :as insta]
   [markusfruhmann.utils :as utils]))

(defn tree->grammar [tree]
  (loop [node (zip/vector-zip tree)]
    (if (zip/end? node)
      (-> node zip/root flatten str/join)
      (if (zip/branch? node)
        (recur (zip/next node))
        (let [value (zip/node node)]
          (if (string? value)
            (recur (zip/next node))
            (-> node zip/remove recur)))))))

(defn find-matches
  [grammar strings]
  (let [parser (insta/parser grammar)]
    (pmap #(-> % parser insta/failure?) strings)))

(defn grammar-fitness
  "Scores an individual by applying it to the predefined word-map and calculating the f1-score."
  [individual word-map]
  (let [grammar (tree->grammar individual)]
    (-> word-map
        (assoc :valid-words (find-matches grammar (:valid-words word-map)))
        (assoc :invalid-words (find-matches grammar (:invalid-words word-map)))
        (utils/f1-score utils/boolean-reducer))))

(defn grammar-terminate?
  "Is true if the best individual has a score of 91% and is smaller than the median of the generation."
  [sorted-generation]
  (let [best-of-gen (first sorted-generation)]
    (when (>= (:score best-of-gen) 0.91)
      (->> sorted-generation
           (map #(:size %))
           (utils/median)
           (<= (:size best-of-gen))))))
