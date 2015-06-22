(defproject clj-tuple "0.2.2"
  :description "Efficient small collections."
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies []
  :profiles {:dev {:dependencies [[criterium "0.4.1"]
                                  [collection-check "0.1.5"]]}
             :provided {:dependencies [[org.clojure/clojure "1.7.0-alpha5"]]}}
  :global-vars {*warn-on-reflection* true}
  :test-selectors {:benchmark :benchmark
                   :stress :stress
                   :default #(every? (complement #{:benchmark :stress}) (keys %))}
  :java-source-paths ["src"]
  :javac-options ["-target" "1.5" "-source" "1.5"]
  :jvm-opts ^:replace ["-server" "-Xmx500m" "-XX:NewSize=200m"])
