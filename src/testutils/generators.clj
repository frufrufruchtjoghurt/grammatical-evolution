(ns testutils.generators
  (:require
   [clojure.test.check.generators :as gen]))

(def select-regex (gen/elements [#"a*" #"(b|ab)+" #"(ab)*" #"(a|b)*" #"b*(aa)+b*" #"(a|ba|bba)*" #"(ab|ba)+" #"(aa|bb)*"]))
