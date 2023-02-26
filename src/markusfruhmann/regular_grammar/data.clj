(ns markusfruhmann.regular-grammar.data
  (:require
   [markusfruhmann.generic.data :as d]
   [markusfruhmann.regular-grammar.grammar :as gram]
   [markusfruhmann.regular-grammar.helper :as helper]
   [markusfruhmann.tomita :as tom]))

(def functions-grammar
  [:+])

(def arities-grammar
  {:+ 2})

(def t1-non-terminals (helper/generate-non-terminals 2))

(def t2-non-terminals (helper/generate-non-terminals 2))

(def tomita-1-terminal-set (helper/create-terminal-set t1-non-terminals tom/tomita-1-base))

(def tomita-2-terminal-set (helper/create-terminal-set t2-non-terminals tom/tomita-2-base))

(def default-grammar-config (d/->GPConfig functions-grammar         ;; function set
                                          arities-grammar           ;; arity set
                                          nil                       ;; seeded programs
                                          #'gram/grammar-fitness    ;; fitness function
                                          #'gram/grammar-terminate? ;; termination predicate
                                          ))
