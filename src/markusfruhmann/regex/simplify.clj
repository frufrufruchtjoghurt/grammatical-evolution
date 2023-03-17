(ns markusfruhmann.regex.simplify
  (:require [meander.strategy.epsilon :as r]))

(def simplify-operator-rules
  (r/rewrite
    ;; Remove unnecessary duplicates
    [:+ [:+ ?t]] [:+ ?t]
    [:* [:* ?t]] [:* ?t]
    [:? [:? ?t]] [:? ?t]
    ;; Simplify espressions that result in 0-to-n
    [:+ [:? ?t]] [:* ?t]
    [:? [:+ ?t]] [:* ?t]
    [:+ [:* ?t]] [:* ?t]
    [:* [:+ ?t]] [:* ?t]
    [:? [:* ?t]] [:* ?t]
    [:* [:? ?t]] [:* ?t]
    [:& [:+ ?t] [:? ?t]] [:+ ?t]
    [:& [:? ?t] [:+ ?t]] [:+ ?t]
    [:& [:+ ?t] [:* ?t]] [:+ ?t]
    [:& [:* ?t] [:+ ?t]] [:+ ?t]
    [:& [:? ?t] [:* ?t]] [:* ?t]
    [:& [:* ?t] [:? ?t]] [:* ?t]
    [:| [:+ ?t] [:? ?t]] [:* ?t]
    [:| [:? ?t] [:+ ?t]] [:* ?t]
    [:| [:+ ?t] [:* ?t]] [:* ?t]
    [:| [:* ?t] [:+ ?t]] [:* ?t]
    [:| [:? ?t] [:* ?t]] [:* ?t]
    [:| [:* ?t] [:? ?t]] [:* ?t]
    ;; simplify expressions from & and |
    [:& [:* ?t] ?t] [:+ ?t]
    [:& ?t [:* ?t]] [:+ ?t]
    [:| [?o ?t] ?t] [?o ?t]
    [:| ?t [?o ?t]] [?o ?t]
    ;; remove redundancy
    [:| ?t ?t] ?t
    [:| ?a [:| ?a ?b]] [:| ?a ?b]
    [:| ?a [:| ?b ?a]] [:| ?a ?b]
    [:| [:| ?a ?b] ?a] [:| ?a ?b]
    [:| [:| ?b ?a] ?a] [:| ?a ?b]
    [:| [:| ?a ?b] [:| ?a ?c]] [:| ?a [:| ?b ?c]]
    [:| [:| ?b ?a] [:| ?c ?a]] [:| ?a [:| ?b ?c]]
    [:| [:| ?b ?a] [:| ?a ?c]] [:| ?a [:| ?b ?c]]
    [:| [:| ?a ?b] [:| ?c ?a]] [:| ?a [:| ?b ?c]]
    ;; pull into logical expression
    [:* [?o ?a ?b]] [?o [:* ?a] [:* ?b]]
    [:+ [?o ?a ?b]] [?o [:+ ?a] [:+ ?b]]
    [:? [?o ?a ?b]] [?o [:? ?a] [:? ?b]]
    ;; simplify logical expressions
    [:| [:& ?a ?b] [:& ?a ?c]] [:& ?a [:| ?b ?c]]
    [:| [:& ?b ?a] [:& ?c ?a]] [:& ?a [:| ?b ?c]]
    [:| [:& ?b ?a] [:& ?a ?c]] [:& ?a [:| ?b ?c]]
    [:| [:& ?a ?b] [:& ?c ?a]] [:& ?a [:| ?b ?c]]))

(def simplify-operators
  (r/until = (r/bottom-up
              (r/attempt simplify-operator-rules))))

(def extract-operator-rules
  (r/rewrite
    ;; extract operators
    [:| [?o ?a] [?o ?b]] [?o [:| ?a ?b]]))

(def extract-operators
  (r/until = (r/bottom-up
              (r/attempt extract-operator-rules))))

(defn simplify-tree [tree]
  (-> tree simplify-operators extract-operators))

(def tree->regex-rules
  (r/rewrite
    [?o1 [?o2 ?t]] [?o1 ["(?:" [?o2 ?t] ")"]]
    [:* ?t] [?t "*"]
    [:+ ?t] [?t "+"]
    [:? ?t] [?t "?"]
    [:& ?a ?b] ["(?:" ?a ?b ")"]
    [:| ?a ?b] ["(?:" ?a "|" ?b ")"]))

(def tree->regex-vector
  (r/until = (r/bottom-up
              (r/attempt tree->regex-rules))))
