(ns markusfruhmann.generic.helper
  (:require
   [clojure.zip :as zip]
   [markusfruhmann.utils :as utils]))

(defn choose-from-terminal-set
  "Chooses a random terminal from the terminal set."
  [terminal-set]
  (rand-nth terminal-set))

(defn count-tree-elements [tree]
  (-> tree
      (flatten)
      (count)))

(defn count-function-points [tree function-set]
  (into []
        (keep-indexed #(when (utils/in? function-set %2) %1) (flatten tree))))

(defn max-by-compare [r1 r2]
  (if (pos? (compare r1 r2)) r1 r2))

(defn get-best-individual [population]
  (reduce max-by-compare population))

(defn find-with-tournament-selection
  "Chooses a random amount of programs between 2 and (/ (count population) 3)."
  [scored-population]
  (when (> (count scored-population) 0)
    (if (= 1 (count scored-population))
      (first scored-population)
      (let [shuffled (shuffle scored-population)]
        (as-> scored-population p
          (count p)
          (/ p 3)
          (rand-int p)
          (max p 2)
          (subvec shuffled 0 p)
          (get-best-individual p))))))

(defn apply-at-tree
  "Applies func at the given index.
  The node at the index is passed in as the first parameter."
  [func index tree]
  (if (or (< index (count-tree-elements tree))
          (not= 0 (count-tree-elements tree)))
    (loop [rem  index
           node (-> tree zip/vector-zip zip/down)]
      (if (= 0 rem)
        (if (-> node zip/prev zip/branch?)
          (-> node zip/prev func)
          (-> node func))
        (if (zip/branch? node)
          (recur rem (zip/next node))
          (recur (dec rem) (zip/next node)))))
    tree))

(defn get-subtree
  "Retrieves the subtree at the given index"
  [tree index]
  (when (< index (count-tree-elements tree))
    (apply-at-tree zip/node index tree)))

(defn insert-subtree
  "Inserts the subtree at the given index.
  This replaces the current subtree at the index."
  [tree index subtree]
  (apply-at-tree #(->> subtree (zip/replace %) zip/root) index tree))

(defn max-tree-depth [tree]
  (loop [node (zip/vector-zip tree)
         depth 0]
    (if (zip/end? node)
      depth
      (recur (zip/next node)
             (if (zip/branch? node)
               depth
               (-> node zip/path count (max depth)))))))

(defn validate-crossover
  [male new-male female new-female
   max-crossover-depth]
  (let [new-m-depth (max-tree-depth new-male)
        new-f-depth (max-tree-depth new-female)]
    [(if (or (>= 1 new-m-depth)
             (> new-m-depth max-crossover-depth))
       male
       new-male)
     (if (or (>= 1 new-f-depth)
             (> new-f-depth max-crossover-depth))
       female
       new-female)]))
