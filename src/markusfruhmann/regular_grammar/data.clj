(ns markusfruhmann.regular-grammar.data
  (:require
   [markusfruhmann.generic.data :as d]
   [markusfruhmann.regular-grammar.grammar :as gram]
   [markusfruhmann.regular-grammar.helper :as helper]
   [markusfruhmann.tomita :as tom]))

(def functions-grammar
  [:+ :=])

(def arities-grammar
  {:+ 2
   := 1})

(def t1-non-terminals (helper/generate-non-terminals 1))
(def t2-non-terminals (helper/generate-non-terminals 3))
(def t3-non-terminals (helper/generate-non-terminals 4))
(def t4-non-terminals (helper/generate-non-terminals 3))
(def t5-non-terminals (helper/generate-non-terminals 4))
(def t6-non-terminals (helper/generate-non-terminals 3))
(def t7-non-terminals (helper/generate-non-terminals 4))

(def tomita-1-terminal-set (helper/create-terminal-set t1-non-terminals tom/tomita-1-base))
(def tomita-2-terminal-set (helper/create-terminal-set t2-non-terminals tom/tomita-2-base))
(def tomita-3-terminal-set (helper/create-terminal-set t3-non-terminals tom/tomita-3-base))
(def tomita-4-terminal-set (helper/create-terminal-set t4-non-terminals tom/tomita-3-base))
(def tomita-5-terminal-set (helper/create-terminal-set t5-non-terminals tom/tomita-3-base))
(def tomita-6-terminal-set (helper/create-terminal-set t6-non-terminals tom/tomita-3-base))
(def tomita-7-terminal-set (helper/create-terminal-set t7-non-terminals tom/tomita-3-base))

(def default-grammar-config (d/->GPConfig functions-grammar         ;; function set
                                          arities-grammar           ;; arity set
                                          nil                       ;; seeded programs
                                          #'gram/grammar-fitness    ;; fitness function
                                          #'gram/grammar-terminate? ;; termination predicate
                                          #'identity
                                          #'gram/grammar-pretty-print))
