(ns spirit.essence.elasticsearch
  (:require [spirit.essence.common :as essence]
            [spirit.essence.elasticsearch.skeleton :as skeleton]
            [spirit.essence.elasticsearch.mapping :as mapping]))

(defmethod essence/imbue :elasticsearch
  [_ essence {:keys [mapping]}]
  ;;(generate/datomic essence top embed)
  )

(defmethod essence/transcend :elasticsearch
  [_ corporeal _]
  ;;(analyse/analyse corporeal)
  )
