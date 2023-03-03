(ns markusfruhmann.regex.regex-test
  (:require
   [clojure.test :refer [are deftest is]]
   [clojure.test.check :as check]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [markusfruhmann.tomita :as tom]
   [markusfruhmann.regex.regex :as subject]
   [testutils.generators :as testgen]
   [miner.strgen :as sg]))

(deftest tree->regex-test
  (let [simple-trees [{:tree ["a"] :reg "a"}
                      {:tree [:& "a" "b"] :reg "(?:ab)"}
                      {:tree [:| "a" "b"] :reg "(?:a|b)"}
                      {:tree [:+ "a"] :reg "a+"}
                      {:tree [:* "a"] :reg "a*"}
                      {:tree [:? "a"] :reg "a?"}]]
    (is (= true (reduce (fn [b m] (let [{:keys [tree reg]} m]
                                    (and b (= reg (subject/tree->regex-str tree)))))
                        true
                        simple-trees)))))

(deftest tree->regex-test-nested-trees
  (let [trees [{:tree [:| [:+ "a"] [:& "b" [:* "a"]]] :reg "(?:a+|(?:ba*))"}
               {:tree [:+ [:& [:& [:? "a"] [:| "b" [:* "c"]]] [:+ "b"]]] :reg "(?:(?:a?(?:b|c*))b+)+"}]]
    (is (= true (reduce (fn [b m] (let [{:keys [tree reg]} m]
                                    (and b (= reg (subject/tree->regex-str tree)))))
                        true
                        trees)))))

(deftest find-matches-test
  (let [reg #"(a|ba|bba)*(bb|b)?"
        word-map tom/tomita-4-base]
    (is (and (reduce #(and %1 (vector? %2)) true (subject/find-matches reg (:valid-words word-map)))
             (reduce #(and %1 (nil? %2)) true (subject/find-matches reg (:invalid-words word-map)))))))

(def find-matches-prop
  (prop/for-all [reg testgen/select-regex]
                (prop/for-all [valid (gen/vector (sg/string-generator reg))]
                              (reduce #(and %1 (string? (first %2))) true (subject/find-matches reg valid)))))

(deftest find-matches-test-check-props
  (are [pass?] (= true pass?)
    (:pass? (check/quick-check 100 find-matches-prop))))
