;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defproject io.czlab/wabbit-pugs "0.1.0"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :description ""
  :url "https://github.com/llnek/wabbit-pugs"

  :dependencies
  [[org.apache.geronimo.specs/geronimo-jms_1.1_spec "1.1.1"]
   ;;[com.github.spullara.mustache.java/compiler "0.9.4" ]
   ;;[com.google.code.findbugs/jsr305 "3.0.1"]
   [org.apache.shiro/shiro-core "1.3.2"]
   [io.czlab/wabbit "0.1.0"]]

  :plugins [[lein-codox "0.10.2"]
            [lein-czlab "0.1.1"]
            [lein-pprint "1.1.2"]]
  :hooks [leiningen.lein-czlab]

  :profiles {:provided {:dependencies
                        [[org.clojure/clojure "1.8.0" :scope "provided"]
                         [net.mikera/cljunit "0.6.0" :scope "test"]
                         [junit/junit "4.12" :scope "test"]
                         [codox/codox "0.10.2" :scope "provided"]]}
             :uberjar {:aot :all}}

  :global-vars {*warn-on-reflection* true}
  :target-path "out/%s"
  :aot :all

  ;;:jar-exclusions [#"(?:^|/).svn/"]
  :coordinate! "czlab/wabbit/pugs"
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


