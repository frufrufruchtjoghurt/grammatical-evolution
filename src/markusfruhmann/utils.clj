(ns markusfruhmann.utils)

(defn get-terminals
  "Accepts a list of words as input and returns all unique terminal."
  [all-words]
  (vec (set (apply concat all-words))))
