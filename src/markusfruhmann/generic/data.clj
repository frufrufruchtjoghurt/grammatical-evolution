(ns markusfruhmann.generic.data)

(defrecord GPConfig [function-set
                     arity-map
                     seeded-programs
                     fitness-fn
                     terminate?])
