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
      :or {seeded-programs nil,
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
