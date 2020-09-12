(ns spirit.core)


(comment
  (def opts {:host "localhost"
             :port 28015
             :schema {:system {:created [{:type :instant}]
                               :preview [{:type :boolean}]}
                      :post {:title  [{}]
                             :author [{:type :ref
                                       :ref {:ns :author}}]
                             :tags   [{:type :ref
                                       :cardinality :multi
                                       :ref {:ns :tag}}]}
                      :author {:name     [{}]
                               :salary   [{:type :long}]
                               :location [{:type :ref
                                           :ref {:ns :location}}]}
                      :tag {:name [{}]}
                      :location {:name [{}]
                                 :position [{:type :geo}]}}

             :mapping {:article  {:top #{:post :system}
                                  :embed {:post {:author [{:as :user
                                                           :exclude [:salary]
                                                           :refer   {:name :id}}]
                                                 :tags   :key}
                                          :system [{:only [:created]}]}}}})


  
  (defn document-schema [schema mapping]
    ;; get top nodes
    ;; look for non-ref types
    ;; put them in
    )

  

  (document-mapping (:schema opts) {:top #{:post}})
  => {:title [{:type :string}]}

  
  (document-mapping (:schema opts) (-> opts :mapping :article))

  {:user {:id   [{:type :string}]
          :location  {:name     [{:type :string}]
                      :position [{:type :geo}]}}
   :tags [{:type :string
           :cardinality :multi}]
   :created [{:type :instant}]}

  {:user {:id "zcaudate"
          :location {:name      "Kunming"
                     :position "10.3445L"}}
   :tags [12, 23, 45]})
