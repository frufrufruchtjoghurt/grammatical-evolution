(ns markusfruhmann.constants
  (:require [miner.strgen :as sg]
            [clojure.test.check.generators :as gen]))

(def functions-regex
  [:* :+ :? :& :|])

(def arities-regex
  {:* 1
   :+ 1
   :? 1
   :& 2
   :| 2})

;; Each tomita grammars valid-words are defined using a regex representation from
;; Artificial Neural Networks and Machine Learning – ICANN 2019: Theoretical Neural Computation
;; by Igor V. Tetko, Věra Kůrková, Pavel Karpov, Fabian Theis
;; on page 315, table 1.
;; Each grammar contains 100 valid-words and 100 invalid-words.
;; The repetition of each regex function is increased to 50.

(def tomita-1 {:valid-words (gen/sample (sg/string-generator #"a*" 50) 100)
               :invalid-words (gen/sample (sg/string-generator #"(b+|a+b)+" 50) 100)})

(def tomita-2 {:valid-words (gen/sample (sg/string-generator #"(ab)*" 50) 100)
               :invalid-words (gen/sample (sg/string-generator #"(a+|b+)*a" 50) 100)})

(def tomita-3 {:valid-words (gen/sample (sg/string-generator #"b*(((aa)+b*)|(a(aa)*(bb)+))*a?" 50) 100)
               :invalid-words (gen/sample (sg/string-generator #"b*((((aa)+b*)|(a(aa)*(bb)+))*(a(aa)*b(bb)*)+)+a?" 50) 100)})

(def tomita-4 {:valid-words (gen/sample (sg/string-generator #"(a|ba|bba)*(bb|b)?" 50) 100)
               :invalid-words (gen/sample (sg/string-generator #"(b*a*)*bbb(b*a*)*" 50) 100)})

(def tomita-5 {:valid-words (gen/sample (sg/string-generator #"(aa|bb|(ab|ba)(aa|bb)*(ab|ba))*" 50) 100)
               :invalid-words (gen/sample (sg/string-generator #"(aa|bb|(ab|ba)(aa|bb)*(ab|ba))*(a|b)" 50) 100)})

(def tomita-6 {:valid-words (gen/sample (sg/string-generator #"(ba|(a|bb)(ab)*(b|aa))*" 50) 100)
               :invalid-words (gen/sample (sg/string-generator #"(ba|(a|bb)(ab)*(b|aa))*(b|a)" 50) 100)})

(def tomita-7 {:valid-words (gen/sample (sg/string-generator #"b*a*b*a*" 50) 100)
               :invalid-words (gen/sample (sg/string-generator #"(b+a+){2,}|(a+b+){2,}" 50) 100)})
