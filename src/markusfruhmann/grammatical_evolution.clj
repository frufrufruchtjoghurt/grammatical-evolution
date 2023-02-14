(ns markusfruhmann.grammatical-evolution
  (:gen-class)
  (:require [clojure.string :as str]
            [markusfruhmann.generic :as gen]
            [markusfruhmann.constants :as const]
            [markusfruhmann.utils :as utils]))

(defn -main
  "Main entrypoint. Accepts a struct with a list of :valid-words and :invalid-words"
  [& args]
  (if (seq args)
    (let [{:keys [valid-words invalid-words]} (first args)]
      (println "Valid words: " (str/join ", " valid-words))
      (println "Invalid words: " (str/join ", " invalid-words)))
    (println "Must provide at least one set of allowed and not allowed words!")))
