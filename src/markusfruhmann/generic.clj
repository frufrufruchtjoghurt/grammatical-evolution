(ns markusfruhmann.generic)

(defn choose-from-terminal-set
  "Chooses a random terminal from the terminal set."
  [terminal-set]
  (rand-nth terminal-set))

(declare create-arguments-for-function)

(defn create-individual-program
  "Creates an individual using the given function-set and terminal-set.
  Aritiy-map is used to determine the number of arguments for each function. Remaining-depth
  is the remaining depth of the tree we can create. When remaining-depth reaches
  0, only terminals are selected. Top-node? is only true, when we are the top node of the tree.
  Full? indicates if this individual should be maximum bushy or not."
  [function-set arity-map terminal-set
   remaining-depth top-node? full?]
  (cond (<= remaining-depth 0)
         ;; Maximum depth reached, only terminals at this point!
        (choose-from-terminal-set terminal-set)
        (or top-node? full?)
        ;; We only select functions if the current node is the top node or the full method
        ;; is used.
        (let [function (rand-nth function-set)
              arity (arity-map function)]
          (into []
                (cons
                 function
                 (create-arguments-for-function
                  arity
                  function-set arity-map terminal-set
                  (- remaining-depth 1) full?))))
        :else
        (let [choice (rand-int (+ (count function-set)
                                  (count terminal-set)))]
          (if (< choice (count function-set))
            (let [function (nth function-set choice)
                  arity (arity-map function)]
              (into []
                    (cons
                     function
                     (create-arguments-for-function
                      arity
                      function-set arity-map terminal-set
                      (- remaining-depth 1) full?))))
             ;; Select a terminal from the terminal set
            (choose-from-terminal-set terminal-set)))))

(defn create-arguments-for-function
  "Creates the argument list for a node.
  Number-of-args is the number of arguments still remaining to be created.
  Each argument is created by calling create-individual-program."
  [number-of-arguments
   function-set arity-map terminal-set
   remaining-depth full?]
  (when (> number-of-arguments 0)
    (into []
          (cons
           (create-individual-program
            function-set arity-map terminal-set
            remaining-depth false full?)
           (create-arguments-for-function
            (- number-of-arguments 1)
            function-set arity-map terminal-set
            remaining-depth full?)))))

(defn create-population
  "Creates a population of programs for the given size.
  With seeded-programs predefined programs can be supplied. Useful for debugging.
  Method specifies the method for generating programs. :full, :grow and :ramped are supported.
  Max-tree-depth sets an initial maximum tree depth for all programs."
  [size-of-population
   function-set arity-set terminal-set
   & {:keys [seeded-programs method max-tree-depth]
      :or {seeded-programs nil
           method :ramped
           max-tree-depth 10}}]
  (loop [population   (if seeded-programs
                        (set seeded-programs)
                        #{})
         remaining    (- size-of-population (count seeded-programs))
         full-method? false
         min-tree-depth   1
         max-tree-depth max-tree-depth
         current-attempts 0]
    (if (> remaining 0)
      (let [program (create-individual-program
                     function-set arity-set terminal-set
                     (case method
                       (:full :grow) max-tree-depth
                       (+ min-tree-depth
                          (mod remaining
                               (- max-tree-depth min-tree-depth))))
                     true
                     (case method
                       :full true
                       :grow false
                       full-method?))]
        (if (contains? population program)
          ;; if the program already exists, we have to generate a new one
          (recur population remaining full-method?
                 ;; if more than 20 attempts fail, the minimum depth should be increased
                 (if (> current-attempts 20) (inc min-tree-depth) min-tree-depth)
                 ;; account for adaptation of min-tree-depth
                 (max max-tree-depth (inc min-tree-depth))
                 (inc current-attempts))
          (recur (conj population program)
                 (dec remaining)
                 ;; switch the generation method for ramped-half-and-half
                 (not full-method?)
                 min-tree-depth max-tree-depth 0)))
      population)))

(defn count-tree-elements
  [tree]
  (-> tree
      (flatten)
      (count)))

(defn by-score-size
  [m1 m2]
  (compare [(:score m2) (:size m1)]
           [(:score m1) (:size m2)]))

(defn fitness-of-population
  "Evaluates the population with the given fitness-func for a word-map."
  [population fitness-func word-map]
  (loop [population population
         fitness []]
    (if (> (count population) 0)
      (let [[individual & rest] population
            size                  (count-tree-elements individual)
            score                 (fitness-func individual word-map)]
        (recur rest (conj fitness {:prog individual :size size :score score})))
      fitness)))

(defn find-with-tournament-selection
  "Chooses a random amount of programs between 2 and (/ (count population) 3)."
  [population]
  (let [shuffled (shuffle population)]
    (as-> population p
      (count p)
      (/ p 3)
      (rand-int p)
      (max p 2)
      (subvec shuffled 0 p)
      (sort by-score-size p)
      (first p))))

(defn find-individual
  "Retrieve an individual from the population with the defined selection method."
  [population]
  (find-with-tournament-selection population))

(defn breed-new-population
  [old-population
   function-set arity-set terminal-set]
  (let [population-size (count old-population)]
    (loop [population []]
      (let [index (count population)]
        (if (< index population-size)
          (let [individual (find-individual population)
                frac       (/ index population-size)]
            (cond (and (< index (- population-size 1))
                       ;; TODO: percentage of crossover should be configurable
                       (< frac (* 0.95 population-size)))
                  ;; TODO: implement crossover
                  nil
                  ;; TODO: percentage of reproduction (in this case 4%, since 0.99 - 0.95 = 4) should be configurable
                  (< frac (* 0.99 population-size))
                  (recur (conj population individual))
                  :else
                  ;; TODO: implement mutation
                  nil))
          population)))))

(defn execute-generations
  [population max-generations
   fitness-func terminate?
   function-set arity-set terminal-set word-map]
  (loop [generation 0
         current-population population
         best-of-run (first population)]
    (if (or (>= generation max-generations) (terminate? best-of-run))
      best-of-run
      (let [fitness-sorted (as-> current-population p
                             (fitness-of-population p fitness-func
                                                    word-map)
                             (sort by-score-size p))
            best-of-gen (first fitness-sorted)
            best-of-run (first (sort by-score-size [best-of-run best-of-gen]))]
        (println "Generation" generation "'s best individual:")
        (println best-of-gen)
        (let [next-population nil])))))

(defn run-genetic-programming
  [max-generations size-of-population
   fitness-function terminate?
   function-set arity-set word-map
   & {:keys [seeded-programs method max-tree-depth]
      :or {seeded-programs nil
           method :ramped
           max-tree-depth 10}}]
  (let [terminal-set (utils/get-terminals-from-map word-map)
        population (generic/create-population size-of-population
                                              function-set
                                              arity-set
                                              terminal-set
                                              :seeded-programs seeded-programs
                                              :method method
                                              :max-tree-depth max-tree-depth)]
    (execute-generations population max-generations
                         fitness-function terminate?
                         function-set arity-set terminal-set word-map)))
