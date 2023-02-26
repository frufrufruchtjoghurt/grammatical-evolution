(ns markusfruhmann.utils
  (:require
   [clojure.test.check.generators :as gen]
   [miner.strgen :as sg]))

(defn get-words-from-map
  "Concatenates a map of :valid and :invalid words."
  [word-map]
  (reduce (fn [words [_ v]] (concat words v)) [] word-map))

(defn get-terminals
  "Accepts a list of words as input and returns all unique terminals."
  [words]
  (->> words
       (apply concat)
       (set)
       (map str)
       (into [])))

(defn get-terminals-from-map
  "Returns all terminals from a tomita word map."
  [word-map]
  (-> word-map
      (get-words-from-map)
      (get-terminals)))

(defn in?
  "Returns true if the collection contains the given element."
  [coll element]
  (not= (some #(= element %) coll) nil))

(defn f1-score
  "Calculates the f1-score for the result after evaluating :valid-words and :invalid-words of word-map.
  func has to be a function which counts the positive and negative values of a list and returns them as a map.
  f1-score will define the keys for the map, so func should accept [k1 k2 map value]."
  [word-map func]
  (let [{:keys [valid-words invalid-words]} word-map
        valid-func                          (partial func :true-pos :false-neg)
        {^int true-pos  :true-pos
         ^int false-neg :false-neg}              (reduce valid-func
                                                         {:true-pos 0 :false-neg 0}
                                                         valid-words)
        invalid-func                        (partial func :false-pos :true-neg)
        {^int false-pos :false-pos}              (reduce invalid-func
                                                         {:false-pos 0 :true-neg 0}
                                                         invalid-words)
        ;; it is important to catch a division by 0
        ^float precision                           (if (> (+ true-pos false-pos) 0)
                                                     (/ true-pos
                                                        (+ true-pos false-pos))
                                                     0)
        ^float recall                              (if (> (+ true-pos false-neg) 0)
                                                     (/ true-pos
                                                        (+ true-pos false-neg))
                                                     0)]
    (if (> (+ precision recall) 0)
      (* 2 (/ (* precision recall)
              (+ precision recall)))
      0)))

(defn median
  [coll]
  (when (> (count coll) 0)
    (let [sorted (sort coll)
          cnt    (count coll)
          middle (quot cnt 2)]
      (if (odd? cnt)
        (nth sorted middle)
        (as-> [(nth sorted middle) (nth sorted (dec middle))] m
          (reduce + m)
          (/ m 2))))))

(defn generate-word-map
  ([valid-regex invalid-regex amount]
   {:valid-words   (gen/sample (sg/string-generator valid-regex) amount)
    :invalid-words (gen/sample (sg/string-generator invalid-regex) amount)})

  ([valid-regex invalid-regex word-map amount]
   (let [valid   (:valid-words word-map)
         invalid (:invalid-words word-map)]
     {:valid-words   (concat valid (gen/sample (sg/string-generator valid-regex) (- amount (count valid))))
      :invalid-words (concat invalid (gen/sample (sg/string-generator invalid-regex) (- amount (count invalid))))})))

(defn boolean-reducer
  "Updates the counter of a map corresponding to value."
  [k1 k2 map value]
  (if value
    (assoc map k1 (inc (map k1)))
    (assoc map k2 (inc (map k2)))))
