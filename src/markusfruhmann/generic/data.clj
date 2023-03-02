(ns markusfruhmann.generic.data)

(defrecord GPConfig [function-set
                     arity-map
                     seeded-programs
                     fitness-fn
                     terminate?
                     optimizer
                     pretty-print])

(defrecord GPResult [prog
                     ^long size
                     ^double score]
  Comparable
  (compareTo [_ other]
    (let [score2 (.score ^GPResult other)]
      (cond (> score score2)
            1
            (== score score2)
            (let [size2 (.size ^GPResult other)]
              (cond (< size size2)
                    1
                    (== size size2)
                    0
                    :else
                    -1))
            :else
            -1))))
