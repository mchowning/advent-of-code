(defproject advent-of-code "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"] 
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.clojure/algo.generic "0.1.3"]]
  :main ^:skip-aot advent-of-code.day3
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
