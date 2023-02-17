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
