(ns markusfruhmann.regular-grammar.grammar-test
  (:require
   [clojure.test :refer [deftest is]]
   [markusfruhmann.regular-grammar.grammar :as subject]))

(deftest get-nt-and-refs-test
  (let [tree [:+ {:non-terminal :A :reference nil} {:non-terminal :B :reference :A}]]
    (is (= {:non-terminals #{:A :B} :references #{:A}}
           (subject/get-nt-and-refs tree)))))

(deftest get-nt-and-refs-test-no-refs
  (let [tree [:= {:non-terminal :A :reference nil}]]
    (is (= {:non-terminals #{:A} :references #{}}
           (subject/get-nt-and-refs tree)))))

(deftest get-nt-and-refs-test-duplicates
  (let [tree [:+ {:non-terminal :A :reference :B} {:non-terminal :A :reference :B}]]
    (is (= {:non-terminals #{:A} :references #{:B}}
           (subject/get-nt-and-refs tree)))))

(deftest remove-unsolved-references-test
  (let [tree [:+ {:non-terminal :A :reference :B} {:non-terminal :B :reference :A}]]
    (is (= [:+ {:non-terminal :A :reference :B} {:non-terminal :B :reference :A}]
           (subject/remove-unsolved-references tree)))))

(deftest remove-unsolved-references-test-unresolved
  (let [tree [:+ {:non-terminal :A :reference nil} {:non-terminal :B :reference :C}]]
    (is (= [:+ {:non-terminal :A :reference nil} {:non-terminal :B :reference nil}]
           (subject/remove-unsolved-references tree)))))

(deftest node->rule-test
  (let [node {:non-terminal :A :terminal "a" :reference :A :epsilon? nil}]
    (is (= "A = 'a'A;"
           (subject/node->rule node)))))

(deftest node->rule-test-epsilon
  (let [node {:non-terminal :A :terminal "a" :reference :A :epsilon? true}]
    (is (= "A = 'a'A | epsilon;"
           (subject/node->rule node)))))

(deftest node->rule-test-no-reference
  (let [node {:non-terminal :A :terminal "a" :reference nil :epsilon? true}]
    (is (= "A = 'a' | epsilon;"
           (subject/node->rule node)))))

(deftest tree->grammar-test
  (let [tree [:= {:non-terminal :A :terminal "a" :reference :A :epsilon? true}]]
    (is (= "A = 'a'A | epsilon;"
           (subject/tree->grammar tree)))))

(deftest tree->grammar-test-multiple
  (let [tree [:+ {:non-terminal :A :terminal "a" :reference :A :epsilon? true} [:= {:non-terminal :B :terminal "b" :reference :A :epsilon? nil}]]]
    (is (= "A = 'a'A | epsilon;\nB = 'b'A;"
           (subject/tree->grammar tree)))))
