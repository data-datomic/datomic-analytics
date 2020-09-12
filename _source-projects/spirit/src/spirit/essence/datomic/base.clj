(ns spirit.essence.datomic.base
  (:require [spirit.essence.common :as essence]
            [hara.common.checks :refer [boolean?]]
            [hara.data.nested :as nested]))

(defn schema-property
  [val ns]
  (keyword (str "db." (name ns) "/" (name val))))

(def datomic-additions
  {:ident        {}
   :type         {:check #{:keyword :string :boolean :long :bigint :float :enum
                           :double :bigdec :ref :instant :uuid :uri :bytes}
                  :attr :valueType
                  :fn schema-property}
   :cardinality  {:fn schema-property}
   :doc          {}
   :unique       {:fn schema-property}
   :index        {:default false}
   :fulltext     {:default false}
   :isComponent  {:check boolean?}
   :noHistory    {:check boolean?
                  :default false}})

(def datomic-meta
  (->> datomic-additions
       (reduce-kv (fn [out k v]
                    (assoc out k (assoc v :schema true)))
                  {})
       (nested/merge-nested essence/base)))

(def datomic-specific
  (->> datomic-meta
       (reduce-kv (fn [out k v]
                    (if (:schema v)
                      (assoc out k v)
                      out))
                  {})))
