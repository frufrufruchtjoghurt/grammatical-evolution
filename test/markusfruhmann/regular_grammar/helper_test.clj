(ns markusfruhmann.regular-grammar.helper-test
  (:require
   [clojure.test :refer [are deftest is]]
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

(def generate-rule-set-prop
  (prop/for-all [nt (gen/vector gen/string)
                 t (gen/vector gen/char)]
                (let [res (subject/generate-rule-set nt t)
                      t-cnt (count t)
                      nt-cnt (count nt)]
                  (= (count res) (+ (* nt-cnt (+ t-cnt 1)) (* nt-cnt nt-cnt t-cnt))))))

(deftest generate-rule-set-test-check-props
  (are [pass?] pass?
    (results/pass? (check/quick-check 100 generate-rule-set-prop))))
