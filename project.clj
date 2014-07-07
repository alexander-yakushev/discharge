(defproject discharge "0.1.0"
  :description "Simple static site generator."
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.cli "0.3.1"]
                 [org.clojure/tools.logging "0.2.6"]
                 [org.clojure/core.cache "0.6.3"]
                 [hiccup "1.0.4"]
                 [org.pegdown/pegdown "1.4.2"]
                 [org.clojars.amit/commons-io "1.4.0"]
                 [ring/ring-core "1.2.1"]
                 [ring/ring-jetty-adapter "1.2.1"]
                 [clojure-watch "0.1.9"]
                 [com.cemerick/url "0.1.1"]]
  :profiles {:uberjar {:aot :all}}
  :main discharge.core
  :jar-name "discharge.jar"
  :uberjar-name "discharge-app.jar")
