(ns markusfruhmann.constants
  (:require [miner.strgen :as sg]
            [clojure.test.check.generators :as gen]))

(def functions-regex
  ["*" "+" "?" "&" "|"])

(def arities-regex
  {"*" 1
   "+" 1
   "?" 1
   "&" 2
   "|" 2})

;; Each tomita grammars valid-words are defined using a regex representation from
;; Artificial Neural Networks and Machine Learning – ICANN 2019: Theoretical Neural Computation
;; by Igor V. Tetko, Věra Kůrková, Pavel Karpov, Fabian Theis
;; on page 315, table 1.
;; Each grammar contains 100 valid-words and 100 invalid-words.
;; The repetition of each regex function is increased to 50.

(def tomita-1 {:valid-words (gen/sample (sg/string-generator #"1*" 50) 100)
               :invalid-words (gen/sample (sg/string-generator #"(0+|1+0)+" 50) 100)})

(def tomita-2 {:valid-words (gen/sample (sg/string-generator #"(10)*" 50) 100)
               :invalid-words (gen/sample (sg/string-generator #"(1+|0+)*1" 50) 100)})

(def tomita-3 {:valid-words (gen/sample (sg/string-generator #"0*(((11)+0*)|(1(11)*(00)+))*1?" 50) 100)
               :invalid-words (gen/sample (sg/string-generator #"0*((((11)+0*)|(1(11)*(00)+))*(1(11)*0(00)*)+)+1?" 50) 100)})

(def tomita-4 {:valid-words (gen/sample (sg/string-generator #"(1|01|001)*(00|0)?" 50) 100)
               :invalid-words (gen/sample (sg/string-generator #"(0*1*)*000(0*1*)*" 50) 100)})

(def tomita-5 {:valid-words (gen/sample (sg/string-generator #"(11|00|(10|01)(11|00)*(10|01))*" 50) 100)
               :invalid-words (gen/sample (sg/string-generator #"(11|00|(10|01)(11|00)*(10|01))*(1|0)" 50) 100)})

(def tomita-6 {:valid-words (gen/sample (sg/string-generator #"(01|(1|00)(10)*(0|11))*" 50) 100)
               :invalid-words (gen/sample (sg/string-generator #"(01|(1|00)(10)*(0|11))*(0|1)" 50) 100)})

(def tomita-7 {:valid-words (gen/sample (sg/string-generator #"0*1*0*1*" 50) 100)
               :invalid-words (gen/sample (sg/string-generator #"(0+1+){2,}|(1+0+){2,}" 50) 100)})
