(ns spirit.essence.datomic.analyse-test
  (:use midje.sweet)
  (:require [spirit.essence.datomic.analyse :refer :all]))

^{:refer spirit.essence.datomic.analyse/analyse-single :added "0.1"}
(fact "creates the path and the attribute from datomic"
  (analyse-single {:db/ident :node/link,
                   :db/index false,
                   :db/fulltext true,
                   :db/noHistory true,
                   :db/valueType :db.type/ref,
                   :db/cardinality :db.cardinality/many})
  => [[:node :link]
      {:fulltext true,
       :noHistory true,
       :type :ref,
       :cardinality :many}])

^{:refer spirit.essence.datomic.analyse/analyse :added "0.1"}
(fact "creates the schema from datomic nodes"
  (analyse [{:db/ident :node/link,
             :db/index false,
             :db/fulltext false,
             :db/noHistory false,
             :db/valueType :db.type/ref,
             :db/cardinality :db.cardinality/one}
            {:db/ident :person/gender,
             :db/index false,
             :db/fulltext false,
             :db/noHistory false,
             :db/valueType :db.type/ref,
             :db/cardinality :db.cardinality/one}])
  => {:node   {:link [{:type :ref}]},
      :person {:gender [{:type :ref}]}})
