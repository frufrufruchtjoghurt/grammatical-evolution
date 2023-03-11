(ns markusfruhmann.generic.data)

(defrecord GPConfig [function-set
                     arity-map
                     seeded-programs
                     fitness-fn
                     terminate?
                     optimizer
                     pretty-print])

(defrecord GPResult [prog
                     ^long mass
                     ^double score]
  Comparable
  (compareTo [_ other]
    (let [score2 (.score ^GPResult other)]
      (cond (> score score2)
            1
            (== score score2)
            (let [mass2 (.mass ^GPResult other)]
              (cond (< mass mass2)
                    1
                    (== mass mass2)
                    0
                    :else
                    -1))
            :else
            -1))))
