(ns markusfruhmann.grammatical-evolution
  (:gen-class)
  (:require [clojure.string :as str]
            [markusfruhmann.generic.algorithm :as g]
            [markusfruhmann.tomita :as t]
            [markusfruhmann.utils :as utils]
            [markusfruhmann.regular-grammar.data :as gramd]
            [markusfruhmann.regex.data :as regd]))

;; Workaround to fix performance issues with meander
(ns meander.strategy.epsilon)

(defn iall? [x]
  (or
   (instance? clojure.lang.IPersistentMap x)
   (instance? clojure.lang.IPersistentSet x)
   (instance? clojure.lang.IPersistentVector x)
   (instance? clojure.lang.ISeq x)))

(ns markusfruhmann.grammatical-evolution)

(def tomita-word-lists
  [[t/tomita-1 gramd/tomita-1-terminal-set]
   [t/tomita-2 gramd/tomita-2-terminal-set]
   [t/tomita-3 gramd/tomita-3-terminal-set]
   [t/tomita-4 gramd/tomita-4-terminal-set]
   [t/tomita-5 gramd/tomita-5-terminal-set]
   [t/tomita-6 gramd/tomita-6-terminal-set]
   [t/tomita-7 gramd/tomita-7-terminal-set]])

(defn print-results [best-of-regex best-of-grammar]
  (println "====================================================================================================")
  (println "RESULTS:")
  (println "\nREGEX")
  ((.pretty-print regd/default-regex-config) best-of-regex)
  (println "\nGRAMMAR")
  ((.pretty-print gramd/default-grammar-config) best-of-grammar))

(defn execute-regex [tomita-words]
  (g/run-genetic-programming regd/default-regex-config 50 500 6 tomita-words
                             :max-crossover-depth 17 :method-of-mutation :delete))

(defn execute-grammar [tomita-words terminal-set]
  (g/run-genetic-programming gramd/default-grammar-config 50 500 6 tomita-words
                             :max-crossover-depth 17 :terminal-set terminal-set))

(defn execute-all-and-print []
  (let [results (map (fn [[tom gram-t]] {:regex   (execute-regex tom)
                                         :grammar (execute-grammar tom gram-t)}) tomita-word-lists)]
    (loop [results results
           cnt 1]
      (println "====================================================================================================")
      (println "Tomita Grammar" cnt)
      (let [[{:keys [regex grammar]} & rest] results]
        (print-results regex grammar)
        (when rest
          (recur rest (inc cnt)))))))

(defn -main
  "Main entrypoint. Accepts a tomita grammar like 'tomita-1' and starts execution with the
  specified parameters."
  [& args]
  (if (seq? args)
    (let [tomita (first args)]
      (case tomita
        "tomita-1" (print-results (execute-regex t/tomita-1)
                                  (execute-grammar t/tomita-1 gramd/tomita-1-terminal-set))
        "tomita-2" (print-results (execute-regex t/tomita-2)
                                  (execute-grammar t/tomita-2 gramd/tomita-2-terminal-set))
        "tomita-3" (print-results (execute-regex t/tomita-3)
                                  (execute-grammar t/tomita-3 gramd/tomita-3-terminal-set))
        "tomita-4" (print-results (execute-regex t/tomita-4)
                                  (execute-grammar t/tomita-4 gramd/tomita-4-terminal-set))
        "tomita-5" (print-results (execute-regex t/tomita-5)
                                  (execute-grammar t/tomita-5 gramd/tomita-5-terminal-set))
        "tomita-6" (print-results (execute-regex t/tomita-6)
                                  (execute-grammar t/tomita-6 gramd/tomita-6-terminal-set))
        "tomita-7" (print-results (execute-regex t/tomita-7)
                                  (execute-grammar t/tomita-7 gramd/tomita-7-terminal-set))
        "all" (execute-all-and-print)
        (println "Must select a tomita grammar such as 'tomita-1'!")))
    (println "Must select tomita grammar!")))
