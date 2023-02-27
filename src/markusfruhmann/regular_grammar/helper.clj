(ns markusfruhmann.regular-grammar.helper
  (:require
   [clojure.string :as str]
   [markusfruhmann.utils :as utils]))

(defn generate-non-terminals
  "Generates a list of non-terminal symbols according to amount starting with 'A'.
  This function will not produce more than the latin alphabet which consists of 26 characters."
  [amount]
  (let [amount (if (> amount 26) 26 amount)
        start (int \A)]
    (into [] (map char (range start (+ start amount))))))

(defn create-rule
  ([non-terminal terminal]
   {:non-terminal (keyword (str non-terminal))
    :terminal terminal})

  ([non-terminal terminal non-terminal-ref
    & {:keys [epsilon?]}]
   {:non-terminal (keyword (str non-terminal))
    :terminal terminal
    :reference (keyword (str non-terminal-ref))
    :epsilon? epsilon?}))

(defn create-rules-for-refs [non-terminal terminal non-terminal-refs]
  (apply conj
         (map #(create-rule non-terminal terminal %) non-terminal-refs)
         (map #(create-rule non-terminal terminal % :epsilon? true) non-terminal-refs)))

(defn generate-rule-set [non-terminal-set terminal-set]
  (loop [[nt-head & nt-tail] non-terminal-set
         [t-head & t-tail]   terminal-set
         result              []]
    (let [basic-rules [(create-rule nt-head t-head)]
          ref-rules   (create-rules-for-refs nt-head t-head non-terminal-set)
          new-rules   (apply conj basic-rules ref-rules)]
      (cond (and nt-tail t-tail)
            (recur nt-tail t-tail (apply conj result new-rules))
            nt-tail
            ;; if the terminal set is empty, start from its beginning!
            (recur nt-tail terminal-set (apply conj result new-rules))
            :else
            (apply conj result new-rules)))))

(defn create-terminal-set [non-terminal-set word-map]
  (->> word-map utils/get-terminals-from-map (generate-rule-set non-terminal-set)))