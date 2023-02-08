(ns markusfruhmann.grammatical-evolution
  (:gen-class)
  (:require [clojure.string :as str]))

(def all-non-terminals ["A" "B" "C" "D" "E" "F" "G"])

(def tomita-1 {:valid-words ["0" "000000000" "0000"]
               :invalid-words ["00001" "01000100001" "11111" "1" "100000" "0000100000"]})

(defn -main
  "Main entrypoint. Accepts a struct with a list of :valid-words and :invalid-words"
  [& args]
  (if (seq args)
    (let [{:keys [valid-words invalid-words]} (first args)]
      (println "Valid words: " (str/join ", " valid-words))
      (println "Invalid words: " (str/join ", " invalid-words)))
    (println "Must provide at least one set of allowed and not allowed words!")))

(defn get-terminals
  "Accepts a list of words as input and returns all unique terminal symbols."
  [all-words]
  (set (apply concat all-words)))
