(ns markusfruhmann.generic.algorithm-test
  (:require
   [clojure.test :refer [are deftest]]
   [clojure.test.check :as check]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [clojure.test.check.results :as result]
   [markusfruhmann.generic.algorithm :as subject]
   [markusfruhmann.generic.helper :as h]))

(def create-individual-program-prop
  (prop/for-all [arity-map (gen/not-empty (gen/map gen/keyword gen/nat))
                 terminal-set (gen/not-empty (gen/vector gen/string))
                 max-depth (gen/such-that #(> % 0) gen/nat)
                 full? gen/boolean]
                (let [function-set (map (fn [[_ v]] v) arity-map)
                      res (subject/create-individual-program function-set arity-map terminal-set max-depth true full?)]
                  (and (->> res flatten (reduce (fn [b v] (and b (some #(= v %) (concat function-set terminal-set))))))
                       (if full?
                         ;; max-depth has to be reached
                         (= max-depth (h/max-tree-depth res))
                         ;; can be smaller than max-depth
                         (< max-depth (h/max-tree-depth res)))))))

(deftest create-individual-program-test-check-props
  (are [pass?] (= true pass?)
    (result/pass? (check/quick-check 100 create-individual-program-prop))))

(def create-arguments-for-function-prop
  (prop/for-all [arity-map (gen/not-empty (gen/map gen/keyword gen/nat))
                 terminal-set (gen/not-empty (gen/vector gen/string))
                 max-depth (gen/such-that #(> % 0) gen/nat)
                 full? gen/boolean
                 number-of-args gen/nat]
                (let [function-set (map (fn [[_ v]] v) arity-map)
                      res (subject/create-arguments-for-function number-of-args function-set arity-map terminal-set max-depth full?)]
                  (or (and (= 0 number-of-args) (nil? res))
                      (= number-of-args (count res))))))

(deftest create-arguments-for-function-test-check-props
  (are [pass?] (= true pass?)
    (result/pass? (check/quick-check 100 create-arguments-for-function-prop))))

(def create-population-config (gen/hash-map :method-of-generation (gen/elements [:full :grow :ramped])
                                            :seeded-programs (gen/set (gen/tuple gen/keyword gen/string gen/string))
                                            :max-individual-depth (gen/such-that #(< 0 %) gen/nat)))
(def create-population-prop
  (prop/for-all [c create-population-config
                 arity-map (gen/not-empty (gen/map gen/keyword gen/nat))
                 terminal-set (gen/not-empty (gen/vector gen/string))
                 size gen/nat]
                (let [function-set (map (fn [[_ v]] v) arity-map)
                      config (-> c
                                 (assoc :function-set function-set)
                                 (assoc :terminal-set terminal-set)
                                 (assoc :arity-map arity-map))
                      res (subject/create-population config size)]
                  (or (and (= (count (:seeded-programs config)) (count res))
                           (> (count (:seeded-programs config)) size))
                      (= (count res) size)))))

(deftest create-population-test-check-props
  (are [pass?] (= true pass?)
    (result/pass? (check/quick-check 100 create-population-prop))))

(def fitness-of-population-prop
  (prop/for-all [p (gen/vector (gen/vector gen/string))]
                (let [res (subject/fitness-of-population (fn [_ _] (rand)) {} p)]
                  (and (= (count p) (count res))
                       (when (not= 0 (count p))
                         (reduce (fn [b m] (and b (contains? m :prog)
                                                (contains? m :score)
                                                (contains? m :mass)
                                                (int? (:mass m))
                                                (or (int? (:score m))
                                                    (float? (:score m))
                                                    (ratio? (:score m))))) true res))))))

(deftest fitness-of-population-test-check-props
  (are [pass?] (= true pass?)
    (result/pass? (check/quick-check 100 fitness-of-population-prop))))
