;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defproject io.czlab/wabbit-plugs "1.0.1"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :description "Built-in pluggables for the wabbit framework"
  :url "https://github.com/llnek/wabbit-plugs"

  :dependencies
  [[org.apache.geronimo.specs/geronimo-jms_1.1_spec "1.1.1"]
   [org.freemarker/freemarker "2.3.26-incubating"]
   [commons-net/commons-net "3.6"]
   [org.apache.shiro/shiro-core "1.4.0"]
   [io.czlab/nettio "1.1.1"]
   [io.czlab/wabbit "1.0.0"]]

  :plugins [[cider/cider-nrepl "0.14.0"]
            [lein-codox "0.10.3"]
            [lein-cprint "1.2.0"]]

  :profiles {:provided {:dependencies
                        [[org.clojure/clojure "1.8.0" :scope "provided"]
                         [io.czlab/proto "1.0.0" :scope "test"]]}
             :uberjar {:aot :all}}

  :global-vars {*warn-on-reflection* true}
  :target-path "out/%s"
  :aot :all

  :coordinate! "czlab/wabbit/plugs"
  :omit-source true

  :java-source-paths ["src/main/java" "src/test/java"]
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"]
  :resource-paths ["src/main/resources"]

  :jvm-opts ["-Dlog4j.configurationFile=file:attic/log4j2.xml"]
  :javac-options ["-source" "8"
                  "-Xlint:unchecked" "-Xlint:-options" "-Xlint:deprecation"])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


