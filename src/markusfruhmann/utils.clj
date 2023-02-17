(ns markusfruhmann.utils
  (:require [clojure.string :as str]))

(defn get-terminals
  "Accepts a list of words as input and returns all unique terminal."
  [all-words]
  (into [] (set (apply concat all-words))))

(defn in?
  "Returns true if the collection contains the given element."
  [coll element]
  (not= (some #(= element %) coll) nil))

(defn string->regex-group
  "The input string is wrapped with '(' and ')'"
  [s]
  (str/join ""
            (-> (list s)
                (conj \()
                (concat '(\))))))

(defn resolve-|
  [[arg1 arg2]]
  (string->regex-group
   (str/join "" [arg1 \| arg2])))

(defn resolve-ops
  [[op arg]]
  (string->regex-group
   (str/join "" [arg op])))

(defn tree->regex
  [[head & tail] functions]
  (if (in? functions head)
    (let [func head
          [arg1 arg2] tail]
      (case func
        \& (str/join "" [(tree->regex arg1 functions) (tree->regex arg2 functions)])
        \| (resolve-| [(tree->regex arg1 functions) (tree->regex arg2 functions)])
        (resolve-ops [func (tree->regex arg1 functions)])))
    head))
