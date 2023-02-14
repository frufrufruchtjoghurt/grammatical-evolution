(ns markusfruhmann.grammatical-evolution
  (:gen-class)
  (:require [clojure.string :as str]))

(declare choose-from-terminal-set
         create-individual-program
         create-arguments-for-function)

(def functions
  ["*" "+" "?" "&" "|"])

(def arities
  {"*" 1
   "+" 1
   "?" 1
   "&" 2
   "|" 2})

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
  "Accepts a list of words as input and returns all unique terminal."
  [all-words]
  (vec (set (apply concat all-words))))

(defn choose-from-terminal-set
  "Chooses a random terminal from the terminal set."
  [terminal-set]
  (rand-nth terminal-set))

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
          (cons
           function
           (create-arguments-for-function
            arity
            function-set arity-map terminal-set
            (- remaining-depth 1) full?)))
        :else
        (let [choice (rand-int (+ (count function-set)
                                  (count terminal-set)))]
          (if (< choice (count function-set))
            (let [function (nth function-set choice)
                  arity (arity-map function)]
              (cons
               function
               (create-arguments-for-function
                arity
                function-set arity-map terminal-set
                (- remaining-depth 1) full?)))
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
    (cons
     (create-individual-program
      function-set arity-map terminal-set
      remaining-depth false full?)
     (create-arguments-for-function
      (- number-of-arguments 1)
      function-set arity-map terminal-set
      remaining-depth full?))))
