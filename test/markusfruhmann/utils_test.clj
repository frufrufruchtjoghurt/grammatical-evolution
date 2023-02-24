(ns markusfruhmann.utils-test
  (:require
   [clojure.test :refer [deftest is are]]
   [clojure.test.check :as check]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [markusfruhmann.utils :as subject]))

(def get-words-from-map-prop
  (prop/for-all [m (gen/map gen/keyword (gen/vector gen/string))]
                (= (count (subject/get-words-from-map m))
                   (reduce-kv (fn [c _ v] (+ c (count v))) 0 m))))

(deftest get-words-from-map-test-check-props
  (are [pass?] (= true pass?)
    (:pass? (check/quick-check 100 get-words-from-map-prop))))

(def get-terminals-prop
  (prop/for-all [v (gen/vector gen/string)]
                (let [r (subject/get-terminals v)]
                  (and (reduce (fn [b v] (and b (= 1 (count v)))) true r)
                       (= (count r) (count (set r)))))))

(deftest get-terminals-test-check-props
  (are [pass?] (= true pass?)
    (:pass? (check/quick-check 100 get-terminals-prop))))

(def in?-prop
  (prop/for-all [v (gen/vector gen/any-equatable)
                 e gen/any-equatable]
                (= (subject/in? v e)
                   (loop [[h & t] v]
                     (cond (nil? h) false
                           (= h e) true
                           :else (recur t))))))

(deftest in?-test-check-props
  (are [pass?] (= true pass?)
    (:pass? (check/quick-check 100 in?-prop))))

(defn- boolean-reducer [k1 k2 map value]
  (if value
    (assoc map k1 (inc (map k1)))
    (assoc map k2 (inc (map k2)))))

(def f1-score-prop
  (prop/for-all [t-pos (gen/vector (gen/return true))
                 f-neg (gen/vector (gen/return false))
                 f-pos (gen/vector (gen/return true))
                 t-neg (gen/vector (gen/return false))]
                (let [valid (concat t-pos f-neg)
                      invalid (concat f-pos t-neg)
                      m {:valid-words valid :invalid-words invalid}
                      ms {:valid-words (shuffle valid) :invalid-words (shuffle invalid)}]
                  (and (= (subject/f1-score m boolean-reducer) (subject/f1-score ms boolean-reducer))
                       (= (subject/f1-score m boolean-reducer)
                          (let [tpc (count t-pos) fpc (count f-pos) fnc (count f-neg)
                                precision (if (> (+ tpc fpc) 0) (/ tpc (+ tpc fpc)) 0)
                                recall (if (> (+ tpc fnc) 0) (/ tpc (+ tpc fnc)) 0)]
                            (if (> (+ precision recall) 0)
                              (* 2 (/ (* precision recall) (+ precision recall)))
                              0)))))))

(deftest f1-score-test-check-props
  (are [pass?] (= true pass?)
    (:pass? (check/quick-check 100 f1-score-prop))))

(deftest median-test
  (let [numbers (shuffle [4 3 1 2 5])]
    (is (and (= 3
                (subject/median numbers))
             (= 7/2
                (subject/median (conj numbers 6)))))))

(deftest median-test-empty
  (is (nil? (subject/median []))))

(def list-with-odd-size
  (gen/such-that #(= 1 (mod (count %) 2)) (gen/vector gen/large-integer)))

(def median-prop
  (prop/for-all [v (gen/vector gen/large-integer)]
                (= (subject/median v) (subject/median (shuffle v)))))

(def median-contains-value-prop
  (prop/for-all [v list-with-odd-size]
                (let [m (subject/median v)]
                  (if (count v)
                    (loop [[h & t] v]
                      (if (not= m h)
                        (recur t)
                        true))
                    (= nil m)))))

(deftest median-test-check-props
  (are [pass?] (= true pass?)
    (:pass? (check/quick-check 100 median-prop))
    (:pass? (check/quick-check 100 median-contains-value-prop))))

(def select-regex (gen/elements [#"a*" #"(b|ab)+" #"(ab)*" #"(a|b)*" #"b*(aa)+b*" #"(a|ba|bba)*" #"(ab|ba)+" #"(aa|bb)*"]))

(def generate-word-map-prop
  (prop/for-all [i gen/nat
                 reg1 select-regex
                 reg2 select-regex]
                (let [m (subject/generate-word-map reg1 reg2 i)]
                  (and
                   ;; The generated arrays should be of size i
                   (reduce-kv (fn [b _ v] (and b (= i (count v)))) true m)
                   ;; All elements of the generated arrays should be strings
                   (reduce-kv
                    (fn [b _ v] (and b
                                     (reduce
                                      (fn [b v] (and b (string? v))) true v))) true m)))))

(def generate-word-map-with-seed-prop
  (prop/for-all [i (gen/large-integer* {:min 10 :max 1000})
                 reg1 select-regex
                 reg2 select-regex
                 valid (gen/vector gen/string)
                 invalid (gen/vector gen/string)]
                (let [m (subject/generate-word-map reg1 reg2 {:valid-words valid :invalid-words invalid} i)]
                  ;; The size of the arrays in m should either be i or equal to
                  ;; the respective seeded vector if it was already larger than i
                  (reduce-kv (fn [b k v] (and b (or (= (count v) i)
                                                    (if (= k :valid-words)
                                                      (= (count v) (count valid))
                                                      (= (count v) (count invalid)))))) true m))))

(deftest generate-word-map-test-check-props
  (are [pass?] (= true pass?)
    (:pass? (check/quick-check 100 generate-word-map-prop))
    (:pass? (check/quick-check 100 generate-word-map-with-seed-prop))))
