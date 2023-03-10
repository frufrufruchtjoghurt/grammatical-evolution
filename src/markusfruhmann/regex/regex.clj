(ns markusfruhmann.regex.regex
  (:require
   [clojure.string :as str]
   [markusfruhmann.regex.simplify :as s]
   [markusfruhmann.utils :as utils]))

(defn tree->regex-str
  [tree]
  (-> tree s/tree->regex-vector flatten str/join))

(defn find-matches
  "Returns a list of the full match or nil for every word in string-list matched by regex."
  [regex-str string-list]
  (let [pattern (re-pattern regex-str)]
    ;; Error within pattern matcher due to large regex. Return nil for low scoring.
    (map #(try (re-matches pattern %) (catch java.lang.StackOverflowError _ nil)) string-list)))

(defn regex-fitness
  "Scores an individual by applying it to the predefined word-map and calculating the f1-score."
  [individual word-map]
  (let [regex-str (-> individual tree->regex-str)]
    (-> word-map
        (assoc :valid-words (find-matches regex-str (:valid-words word-map)))
        (assoc :invalid-words (find-matches regex-str (:invalid-words word-map)))
        (utils/f1-score utils/boolean-reducer))))

(defn regex-terminate?
  "Is true if the best individual has a score higher than 99% and is smaller than the median of the generation."
  [best-of-gen population]
  (when (>= (:score best-of-gen) 0.99)
    (->> population
         (map #(:mass %))
         (utils/median)
         (<= (:mass best-of-gen)))))

(defn regex-pretty-print [{:keys [prog mass score]}]
  (println "Regex:" (tree->regex-str prog) "Size:" mass "Score:" score))
