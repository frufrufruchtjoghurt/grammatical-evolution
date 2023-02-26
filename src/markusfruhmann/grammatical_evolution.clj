(ns markusfruhmann.grammatical-evolution
  (:gen-class)
  (:require [clojure.string :as str]
            [markusfruhmann.generic.algorithm :as g]
            [markusfruhmann.tomita :as t]
            [markusfruhmann.utils :as utils]
            [markusfruhmann.regular-grammar.data :as gramd]
            [markusfruhmann.regex.data :as regd]))

(defn -main
  "Main entrypoint. Accepts a struct with a list of :valid-words and :invalid-words"
  [& args]
  (if (seq? args)
    (let [tomita (first args)]
      (case tomita
        "tomita-1" (g/run-genetic-programming regd/default-regex-config 30 50 5 t/tomita-1 :method-of-mutation :delete)
        (println "Must select a tomita grammar such as 'tomita-1'!")))
    (println "Must select tomita grammar!")))
