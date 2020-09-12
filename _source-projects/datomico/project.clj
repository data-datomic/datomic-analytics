(defproject datomico "0.2.0"
  :description "Use datomic with intention revealing names"
  :url "http://github.com/cldwalker/datomico"
  :license {:name "The MIT License"
            :url "https://en.wikipedia.org/wiki/MIT_License"}
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :profiles {:dev {:dependencies [[com.datomic/datomic-free "0.8.3789"]]}
             :1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}}
  :aliases {"all" ["with-profile" "dev:dev,1.5"]}
  :test-selectors {:focus :focus :default (constantly true)})
