(ns markusfruhmann.regular-grammar.helper
  (:require
   [markusfruhmann.utils :as utils]))

(defn generate-non-terminals
  "Generates a list of non-terminal symbols according to amount starting with 'A'.
  This function will not produce more than the latin alphabet which consists of 26 characters."
  [amount]
  (let [amount (if (> amount 26) 26 amount)
        start (int \A)]
    (into [] (map char (range start (+ start amount))))))

(defn generate-rules
  ([non-terminal-set terminal-set]
   (into [] (for [nt  non-terminal-set
                  t   terminal-set]
              {:non-terminal (keyword (str nt))
               :terminal     t})))
  ([non-terminal-set references terminal-set]
   (into [] (for [nt  non-terminal-set
                  ref references
                  t   terminal-set]
              {:non-terminal (keyword (str nt))
               :reference    (keyword (str ref))
               :terminal     t}))))

(defn generate-rule-set [non-terminal-set terminal-set]
  (let [eps-set (conj terminal-set "epsilon")]
    (apply conj
           (generate-rules non-terminal-set eps-set)
           (generate-rules non-terminal-set non-terminal-set terminal-set))))

(defn create-terminal-set [non-terminal-set word-map]
  (->> word-map utils/get-terminals-from-map (generate-rule-set non-terminal-set)))
