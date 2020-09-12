(defproject im.chit/spirit "0.1.0-SNAPSHOT"
  :description "the essence of data"
  :url "https://github.com/zcaudate/spirit"
  :license {:name "The MIT License"
            :url "http://http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [im.chit/hara.data.nested "2.2.17"]
                 [prismatic/schema "1.1.0"]]
  :profiles {:dev {:plugins [[lein-midje "3.1.3"]
                             [lein-hydrox "0.1.16"]]
                   :dependencies [[midje "1.6.3"]
                                  [helpshift/hydrox "0.1.16"]
                                  [com.datomic/datomic-free "0.9.5350"]
                                  [com.rethinkdb/rethinkdb-driver "2.2-beta-6"]
                                  [cheshire "5.5.0"]]}})