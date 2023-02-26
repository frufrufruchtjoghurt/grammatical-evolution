(ns markusfruhmann.regular-grammar.helper-test
  (:require
   [clojure.test :refer [are deftest]]
   [clojure.test.check :as check]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [clojure.test.check.results :as results]
   [markusfruhmann.regular-grammar.helper :as subject]))

(def generate-non-terminals-prop
  (prop/for-all [n gen/nat]
                (let [res (subject/generate-non-terminals n)]
                  (and (= n (count res))
                       (loop [[r1 r2 & rs] res]
                         (if r2
                           (when (< (int r1) (int r2))
                             (recur rs))
                           true))))))

(deftest generate-non-terminals-test-check-props
  (are [pass?] pass?
    (results/pass? (check/quick-check 100 generate-non-terminals-prop))))

(def create-rule-prop
  (prop/for-all [nt (gen/not-empty gen/string)
                 t (gen/not-empty gen/string)]
                (let [res (subject/create-rule nt t)]
                  (= (+ (count nt) (count t) 2) (count res)))))

(def create-rule-with-ref-prop
  (prop/for-all [nt (gen/not-empty gen/string)
                 t (gen/not-empty gen/string)
                 ref (gen/not-empty gen/string)]
                (let [res (subject/create-rule nt t ref)]
                  (= (+ (count nt) (count t) (count ref) 3) (count res)))))

(deftest create-rule-test-check-props
  (are [pass?] pass?
    (results/pass? (check/quick-check 100 create-rule-prop))
    (results/pass? (check/quick-check 100 create-rule-with-ref-prop))))

(def create-rules-for-refs-prop
  (prop/for-all [nt (gen/not-empty gen/string)
                 t (gen/not-empty gen/string)
                 refs (gen/vector (gen/not-empty gen/string))]
                (= (count refs) (count (subject/create-rules-for-refs nt t refs)))))

(deftest create-rules-for-refs-test-check-props
  (are [pass?] pass?
    (results/pass? (check/quick-check 100 create-rules-for-refs-prop))))

(def generate-rule-set-prop
  (prop/for-all [nt (gen/vector gen/string)
                 t (gen/vector gen/char)]
                (let [res (subject/generate-rule-set nt t)]
                  (= (count res) (* nt (+ nt 2))))))

(deftest generate-rule-set-test-check-props
  (are [pass?] pass?
    (results/pass? (check/quick-check 100 generate-rule-set-prop))))
