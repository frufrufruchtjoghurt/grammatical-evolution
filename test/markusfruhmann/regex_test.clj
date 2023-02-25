(ns markusfruhmann.regex-test
  (:require
   [clojure.test :refer [are deftest is]]
   [clojure.test.check :as check]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [markusfruhmann.constants :as const]
   [markusfruhmann.regex :as subject]
   [testutils.generators :as testgen]
   [miner.strgen :as sg]
   [clojure.zip :as zip]))

(def group-node-prop
  (prop/for-all [v (gen/not-empty (gen/vector gen/string))]
                (let [z (-> v zip/vector-zip zip/next)
                      pos (rand-int (count v))
                      node (-> (iterate zip/next z) (nth pos))
                      res (-> node subject/group-node zip/root)]
                  (and (= (+ 2 (count v)) (count res))
                       (= "(" (first res))
                       (= ")" (last res))))))

(deftest group-node-test-check-props
  (are [pass?] (= true pass?)
    (:pass? (check/quick-check 100 group-node-prop))))

(deftest tree->regex-test
  (let [simple-trees [{:tree ["a"] :reg "a"}
                      {:tree [:& "a" "b"] :reg "(ab)"}
                      {:tree [:| "a" "b"] :reg "(a|b)"}
                      {:tree [:+ "a"] :reg "(a+)"}
                      {:tree [:* "a"] :reg "(a*)"}
                      {:tree [:? "a"] :reg "(a?)"}]]
    (reduce (fn [b m] (let [{:keys [tree reg]} m]
                        (and b (= reg (subject/tree->regex-str tree)))))
            true
            simple-trees)))

(deftest tree->regex-test-nested-trees
  (let [trees [{:tree [:| [:+ "a"] [:& "b" [:* "a"]]] :reg "((a+)|(b(a*)))"}
               {:tree [:+ [:& [:& [:? "a"] [:| "b" [:* "c"]]] [:+ "b"]]] :reg "((((a?)(b|(c*)))(b+))+)"}]]
    (reduce (fn [b m] (let [{:keys [tree reg]} m]
                        (and b (= reg (subject/tree->regex-str tree)))))
            true
            trees)))

(deftest find-matches-test
  (let [reg #"(a|ba|bba)*(bb|b)?"
        word-map const/tomita-4-base]
    (is (and (reduce #(and %1 (string? (first %2))) true (subject/find-matches reg (:valid-words word-map)))
             (reduce #(and %1 (nil? %2)) true (subject/find-matches reg (:invalid-words word-map)))))))

(def find-matches-prop
  (prop/for-all [reg testgen/select-regex]
                (prop/for-all [valid (gen/vector (sg/string-generator reg))]
                              (reduce #(and %1 (string? (first %2))) true (subject/find-matches reg valid)))))

(deftest find-matches-test-check-props
  (are [pass?] (= true pass?)
    (:pass? (check/quick-check 100 find-matches-prop))))

