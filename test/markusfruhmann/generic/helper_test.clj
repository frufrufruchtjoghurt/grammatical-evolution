(ns markusfruhmann.generic.helper-test
  (:require
   [clojure.test :refer [are deftest]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [clojure.test.check.results :as results]
   [clojure.test.check :as check]
   [markusfruhmann.generic.helper :as subject]
   [clojure.zip :as zip]))

(def count-function-points-prop
  (prop/for-all [fset (gen/not-empty (gen/set gen/keyword))]
                (prop/for-all [k (gen/vector (gen/elements fset))
                               v (gen/vector gen/string)]
                              (let [tree (concat k v)]
                                (prop/for-all [_tree (gen/shuffle tree)]
                                              (= (count k) (count (subject/count-function-points tree fset))))))))

(deftest count-function-points-test-check-props
  (are [pass?] (= true pass?)
    (results/pass? (check/quick-check 100 count-function-points-prop))))

(def fitness-map (gen/hash-map :score (gen/double* {:NaN? false :min 0.0 :max 1N})
                               :size gen/nat))

(def find-with-tournament-selection-prop
  (prop/for-all [v (gen/vector fitness-map)]
                (let [res (subject/find-with-tournament-selection v)]
                  (or (and (= 0 (count v)) (nil? res))
                      (and (map? res)
                           (some #(= res %) v))))))

(deftest find-with-tournament-selection-test-check-props
  (are [pass?] (= true pass?)
    (results/pass? (check/quick-check 100 find-with-tournament-selection-prop))))

(def apply-at-tree-prop
  (prop/for-all [v (gen/vector (gen/tuple (gen/vector gen/string) (gen/tuple gen/string (gen/vector gen/string))))
                 s gen/nat]
                (let [pos (-> v flatten count rand-int)
                      res (subject/apply-at-tree #(->> s (zip/replace %) zip/root) pos v)]
                  (= s (-> res flatten (nth pos))))))

(deftest apply-at-tree-test-check-props
  (are [pass?] (= true pass?)
    (results/pass? (check/quick-check 100 apply-at-tree-prop))))

(def max-tree-depth-prop
  (prop/for-all [depth gen/nat]
                (let [tree (-> (iterate (partial conj []) "a") (nth depth))]
                  (= depth (subject/max-tree-depth tree)))))

(def max-tree-depth-multiple-prop
  (prop/for-all [depths (gen/not-empty (gen/vector gen/nat))
                 s gen/string]
                (let [tree (map #(-> (iterate (partial conj []) s) (nth %)) depths)]
                  (= (reduce max depths) (subject/max-tree-depth tree)))))

(deftest max-tree-depth-check-props
  (are [pass?] (= true pass?)
    (results/pass? (check/quick-check 100 max-tree-depth-prop))
    (results/pass? (check/quick-check 100 max-tree-depth-multiple-prop))))

