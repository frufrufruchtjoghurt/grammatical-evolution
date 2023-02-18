(ns markusfruhmann.utils
  (:require [clojure.string :as str]))

(defn get-words-from-tomita
  "Concatenates a map of :valid and :invalid words."
  [tomita]
  (reduce (fn [words [_ v]] (concat words v)) [] tomita))

(defn get-terminals
  "Accepts a list of words as input and returns all unique terminals."
  [words]
  (->> words
       (apply concat)
       (set)
       (map str)
       (into [])))

(defn get-terminals-from-tomita
  "Returns all terminals from a tomita word map."
  [tomita]
  (-> tomita
      (get-words-from-tomita)
      (get-terminals)))

(defn in?
  "Returns true if the collection contains the given element."
  [coll element]
  (not= (some #(= element %) coll) nil))

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
