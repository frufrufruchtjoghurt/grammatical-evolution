(ns markusfruhmann.regex.data
  (:require
   [markusfruhmann.generic.data :as d]
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
                                        #'reg/regex-pretty-print))
