(ns markusfruhmann.regex
  (:require [clojure.string :as str]
            [markusfruhmann.constants :as c]
            [markusfruhmann.generic :as g]
            [markusfruhmann.utils :as utils]))

(defn tree->regex
  [tree]
  (loop [remaining [tree]
         result ""]
    (let [[tree & tree-tail] remaining
          [node & node-tail] tree]
      (if (nil? tree)
        result
        (case node
          :& (recur (concat ["("] node-tail [")"] tree-tail) result)
          :| (let [[arg1 arg2] node-tail]
               (recur (concat ["(" arg1 "|" arg2 ")"] tree-tail) result))
          (:* :+ :?) (recur (concat ["("] node-tail [(name node) ")"] tree-tail) result)
          (recur (concat node-tail tree-tail) (str/join [result node])))))))

(defn create-individual-program-from-tomita
  "Uses a tomita word list to generate an individual program.
  Used for easier development, ALL PARAMETERS ARE PREDEFINED!"
  [tomita]
  (as-> tomita t
    (utils/get-terminals-from-map t)
    (g/create-individual-program c/functions-regex c/arities-regex t 5 true false)))

(defn find-matches
  "Returns a list of the full match or nil for every word in string-list matched by regex."
  [regex string-list]
  (loop [remaining string-list
         results []]
    (let [[string & tail] remaining]
      (if (nil? string)
        results
        (recur tail (as-> regex r
                      (re-pattern r)
                      (re-matches r string)
                      (conj results r)))))))

(defn regex-reducer
  "Updates the counter of a map corresponding to value."
  [k1 k2 map value]
  (if value
    (assoc map k1 (inc (map k1)))
    (assoc map k2 (inc (map k2)))))

(defn regex-fitness
  [individual word-map]
  (let [regex (tree->regex individual)]
    (-> word-map
        (assoc :valid-words (find-matches regex (:valid-words word-map)))
        (assoc :invalid-words (find-matches regex (:invalid-words word-map)))
        (utils/f1-score regex-reducer))))
