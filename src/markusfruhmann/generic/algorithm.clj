(ns markusfruhmann.generic.algorithm
  (:require
   [markusfruhmann.generic.helper :as h]
   [markusfruhmann.utils :as utils]))

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
        (h/choose-from-terminal-set terminal-set)
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
            ;; Select a function from the function set
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
            (h/choose-from-terminal-set terminal-set)))))

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
  [{:keys [method-of-generation
           function-set arity-map terminal-set
           seeded-programs max-individual-depth]}
   size-of-population]
  (loop [population   (if seeded-programs
                        (set seeded-programs)
                        #{})
         remaining    (- size-of-population (count population))
         full-method? false
         min-tree-depth   1
         max-tree-depth max-individual-depth
         current-attempts 0]
    (if (> remaining 0)
      (let [program (create-individual-program
                     function-set arity-map terminal-set
                     (case method-of-generation
                       (:full :grow) max-tree-depth
                       (+ min-tree-depth
                          (mod remaining
                               (- max-tree-depth min-tree-depth))))
                     true
                     (case method-of-generation
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
      (into [] population))))

(defn fitness-of-population
  "Evaluates the population with the given fitness-fn for a word-map."
  [fitness-fn word-map population]
  (pmap (fn [e] {:prog e
                 :size (h/count-tree-elements e)
                 :score (fitness-fn e word-map)})
        population))

(defn find-individual
  "Retrieve an individual from the population with the defined selection method.
  Extracts the individual from the fitness map."
  [scored-population]
  (:prog (h/find-with-tournament-selection scored-population)))

(defn crossover
  [male male-point
   female female-point
   max-crossover-depth]
  (let [male-subtree (h/get-subtree male male-point)
        female-subtree (h/get-subtree female female-point)
        new-male (h/insert-subtree male male-point female-subtree)
        new-female (h/insert-subtree female female-point male-subtree)]
    (h/validate-crossover male new-male female new-female max-crossover-depth)))

(defn crossover-at-function [male female function-set max-crossover-depth]
  (let [male-point (rand-nth (h/count-function-points male function-set))
        female-point (rand-nth (h/count-function-points female function-set))]
    (crossover male male-point female female-point max-crossover-depth)))

(defn crossover-at-any-point [male female max-crossover-depth]
  (let [male-point (rand-int (h/count-tree-elements male))
        female-point (rand-int (h/count-tree-elements female))]
    (crossover male male-point female female-point max-crossover-depth)))

(defn mutate-subtree
  [{:keys [function-set arity-map terminal-set
           max-mutation-subtree-depth]}
   individual]
  (let [mutation-point (rand-int (h/count-tree-elements individual))
        mutation-tree  (create-individual-program function-set arity-map terminal-set
                                                  max-mutation-subtree-depth true false)]
    (h/insert-subtree individual mutation-point mutation-tree)))

(defn mutate-replace-with-terminal
  [{:keys [terminal-set]} individual]
  (let [mutation-point (rand-int (h/count-tree-elements individual))
        mutant (h/insert-subtree individual mutation-point (h/choose-from-terminal-set terminal-set))]
    (if (= 1 (h/max-tree-depth mutant)) individual mutant)))

(defn mutate [{:keys [method-of-mutation] :as config} individual]
  (case method-of-mutation
    :subtree (mutate-subtree config individual)
    :delete (mutate-replace-with-terminal config individual)))

(defn breed-new-population
  [{:keys [function-set
           max-crossover-depth
           crossover-at-function-frac
           crossover-at-any-point-frac
           reproduction-frac]
    :as config}
   scored-population]
  (let [population-size (count scored-population)]
    (loop [new-population []]
      (let [index (count new-population)]
        (if (< index population-size)
          (let [individual (find-individual scored-population)
                frac (/ index population-size)]
            (cond (and (< index (- population-size 1))
                       (< frac (+ crossover-at-function-frac
                                  crossover-at-any-point-frac)))
                  (let [individuals (if (< frac crossover-at-function-frac)
                                      (crossover-at-function individual (find-individual scored-population)
                                                             function-set max-crossover-depth)
                                      (crossover-at-any-point individual (find-individual scored-population) max-crossover-depth))]
                    (recur (concat new-population individuals)))
                  (< frac (+ reproduction-frac
                             crossover-at-function-frac
                             crossover-at-any-point-frac))
                  (recur (conj new-population individual))
                  :else
                  (->> individual
                       (mutate config)
                       (conj new-population)
                       recur)))
          new-population)))))

(defn execute-generations
  [{:keys [fitness-fn terminate?] :as config}
   population max-generations
   word-map]
  (loop [generation 0
         current-population population
         best-of-run (nth population 0)]
    (let [fitness-sorted (->> current-population
                              (fitness-of-population fitness-fn
                                                     word-map)
                              (sort h/by-score-size))
          best-of-gen (nth fitness-sorted 0)
          best-of-run (nth (sort h/by-score-size [best-of-run best-of-gen]) 0)]
      (println "Generation" generation "'s best individual:")
      (println best-of-gen)
      (if (or (>= generation max-generations) (terminate? fitness-sorted))
        best-of-run
        (let [next-population (breed-new-population config fitness-sorted)]
          (recur (inc generation) next-population best-of-run))))))

(defn run-genetic-programming
  [config
   max-generations size-of-population
   max-individual-depth word-map
   & {:keys [method-of-generation
             method-of-mutation
             max-crossover-depth max-mutation-subtree-depth
             crossover-at-function-frac
             crossover-at-any-point-frac
             reproduction-frac]
      :or {method-of-generation :ramped
           method-of-mutation :subtree
           max-crossover-depth max-individual-depth
           max-mutation-subtree-depth (-> max-individual-depth (* 0.5) (int))
           crossover-at-function-frac 0.45
           crossover-at-any-point-frac 0.45
           reproduction-frac 0.09}}]
  (let [terminal-set (utils/get-terminals-from-map word-map)
        optional {:method-of-generation method-of-generation
                  :method-of-mutation method-of-mutation
                  :max-crossover-depth max-crossover-depth
                  :max-mutation-subtree-depth max-mutation-subtree-depth
                  :crossover-at-any-point-frac crossover-at-any-point-frac
                  :crossover-at-function-frac crossover-at-function-frac
                  :reproduction-frac reproduction-frac}
        gp-config (-> config
                      (merge optional)
                      (assoc :max-individual-depth max-individual-depth)
                      (assoc :terminal-set terminal-set))
        population (create-population gp-config
                                      size-of-population)
        best-of-run (execute-generations gp-config population max-generations word-map)]
    (println "Best individual of this run:")
    (println best-of-run)))
