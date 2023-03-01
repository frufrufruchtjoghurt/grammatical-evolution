(ns markusfruhmann.regex.data
  (:require
   [markusfruhmann.generic.data :as d]
   [markusfruhmann.regex.simplify :as s]
   [markusfruhmann.regex.regex :as reg]))

(def functions-regex
  [:* :+ :? :& :|])

(def arities-regex
  {:* 1
   :+ 1
   :? 1
   :& 2
   :| 2})

(def default-regex-config (d/->GPConfig functions-regex ;; function set
                                        arities-regex   ;; arity set
                                        nil               ;; seeded programs
                                        #'reg/regex-fitness     ;; fitness function
                                        #'reg/regex-terminate?  ;; termination predicate
                                        #'s/simplify-tree
                                        #'reg/regex-pretty-print))
