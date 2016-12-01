(defproject advent-of-code "0.1.0-SNAPSHOT"
  :description "Advent of code"
  :url "http://adventofcode.com"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [digest "1.4.4"]]
  :main ^:skip-aot advent-of-code.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
