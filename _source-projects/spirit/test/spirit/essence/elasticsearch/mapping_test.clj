(ns spirit.essence.elasticsearch.mapping-test
  (:use midje.sweet)
  (:require [spirit.essence.elasticsearch.mapping :refer :all]))

^{:refer spirit.essence.elasticsearch.mapping/mapping :added "0.1"}
(fact "convert the skeleton to an elasticsearch mapping"
  (mapping
   {:ident :tweet,
    :type :ref,
    :elements [{:type :long
                :search {:index nil}, :ident :id}
               {:type :string, 
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
  => {:tweet {:type :object,
              :properties {:id {:index "no", :type :long},
                           :message {:analyser "trans_cs", :type :string},
                           :account {:type :object,
                                     :properties {:id {:type :string},
                                                  :gender {:type :string},
                                                  :age {:type :long},
                                                  :name {:type :object,
                                                         :properties {:full {:type :string},
                                                                      :first {:type :string},
                                                                      :last {:type :string}}}}},
                           :tags {:type :object,
                                  :properties {:slug {:type :string},
                                               :section {:type :string}}}}}})
