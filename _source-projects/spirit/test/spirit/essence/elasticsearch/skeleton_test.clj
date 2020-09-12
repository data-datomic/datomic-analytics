(ns spirit.essence.elasticsearch.skeleton-test
  (:use midje.sweet)
  (:require [spirit.essence.elasticsearch.skeleton :refer :all]))

^{:refer spirit.essence.elasticsearch.skeleton/skeleton-fields :added "0.1"}
(fact "filters the fields based on options"
  (skeleton-fields [{:ident :id
                     :type :long}
                    {:ident :message
                     :type :string,
                     :search {:analyser "trans_cs"}}]
                   {:include #{:message}})
  => [{:ident :message, :type :string, :search {:analyser "trans_cs"}}])

^{:refer spirit.essence.elasticsearch.skeleton/skeleton-refs :added "0.1"}
(fact "builds the tree structure for data model")

^{:refer spirit.essence.elasticsearch.skeleton/skeleton :added "0.1"}
(fact "creates a document db skeleton"
  (skeleton
   {:tweet {:id      [{:type :string
                       :search {:index "no"}}]
            :tweet   [{:type :string
                       :search {:analyser "trans_cs"}}]
            :user    [{:type :ref
                       :ref {:ns :user}}]
            :tags    [{:type :ref
                       :cardinality :many
                       :ref {:ns :tags}}]}
    :user {:id       [{:type :string}]
           :gender   [{:type :string}]
           :age      [{:type :long}]
           :name     [{:type :ref
                       :ref {:ns :name}}]}
    :name {:full     [{:type :string}]
           :first    [{:type :string}]
           :last     [{:type :string}]}
    :tags {:slug     [{:type :string}]
           :category [{:type :string}]}}

   [:tweet [{:exclude #{:id}
             :refer   {:tweet :message
                       :user  :account}}]]
   {:tweet {:user :all
            :tags [{:refer {:category :section}}]}
    :user {:name :all}})
  =>  {:ident :tweet,
       :type :ref,
       :elements [{:type :string,
                   :search {:analyser "trans_cs"}, :ident :message}
                  {:ident :account, :type :ref,
                   :elements [{:type :string, :ident :id}
                              {:type :string, :ident :gender}
                              {:type :long, :ident :age}
                              {:ident :name, :type :ref,
                               :elements [{:type :string, :ident :full}
                                          {:type :string, :ident :first}
                                          {:type :string, :ident :last}]}]}
                  {:ident :tags, :type :ref,
                   :elements [{:type :string, :ident :slug}
                              {:type :string, :ident :section}]}]})


