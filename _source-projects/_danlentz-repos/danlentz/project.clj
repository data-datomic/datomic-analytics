(defproject danlentz/danlentz "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://danlentz.herokuapp.com"
  :license {:name "FIXME: choose"
            :url "http://example.com/FIXME"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [compojure "1.1.6"]
                 [ring/ring-jetty-adapter "1.2.1"]
                 [ring/ring-devel "1.2.1"]
                 [ring-basic-authentication "1.0.3"]
                 [environ "0.4.0"]
                 [com.cemerick/drawbridge "0.0.6"]
                 [clj-tuples "0.3.3"]
                 [danlentz/clj-uuid "0.0.4-SNAPSHOT"]
                 [hiccup "1.0.2"]
                 [korma "0.3.0-RC6"]
                 [pg-hstore "2.0.1"]
                 [postgresql "9.1-901-1.jdbc4"]
                 [org.clojure/java.jdbc "0.2.3"]
                 [org.asciidoctor/asciidoctor-java-integration "0.1.4"]
                 [org.clojure/clojurescript "0.0-1978"]                 
                 ]
  :repl-options {:port 50000}
  :min-lein-version "2.0.0"
  :plugins  [[environ/environ.lein "0.3.1"]
             [lein-cljsbuild "0.3.4"]]
  :hooks    [environ.leiningen.hooks]
  :profiles {:production {:env {:production true}}}
  :source-paths ["src" "cljs"]
  :cljsbuild { 
    :builds [{:id "danlentz"
              :source-paths ["cljs"]
              :compiler {
                :output-to "danlentz.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}]})
  
