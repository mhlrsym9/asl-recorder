(defproject asl-recorder "0.1.9-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/tools.logging "0.4.1"]
                 [org.slf4j/slf4j-log4j12 "1.7.26"]
                 [org.clojure/data.xml "0.0.8"]
                 [org.clojure/data.zip "0.1.1"]
                 [seesaw "1.5.0"]                           ; UI
                 [com.github.cjwizard/cjwizard "1.0.9"]
                 ]
  :repositories [["cjwizard" "https://dl.bintray.com/cjwizard/CJWizard"]]
  :main ^:skip-aot asl-recorder.core
  :aot [asl-recorder.swing-worker]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
