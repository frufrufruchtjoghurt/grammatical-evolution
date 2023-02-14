(ns markusfruhmann.constants)

(def functions-regex
  ["*" "+" "?" "&" "|"])

(def arities-regex
  {"*" 1
   "+" 1
   "?" 1
   "&" 2
   "|" 2})

(def tomita-1 {:valid-words ["0" "000000000" "0000"]
               :invalid-words ["00001" "01000100001" "11111" "1" "100000" "0000100000"]})
