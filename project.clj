(defproject clj-tuple "0.1.7"
  :description "Efficient small collections."
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies []
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.7.0-alpha2"]
                                  [criterium "0.4.1"]
                                  [collection-check "0.1.3"]]}
             :1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}}
  :global-vars {*warn-on-reflection* true}
  :test-selectors {:benchmark :benchmark
                   :stress :stress
                   :default #(every? (complement #{:benchmark :stress}) (keys %))}
  :jvm-opts ^:replace ["-server" "-Xmx500m" "-XX:NewSize=200m"])
