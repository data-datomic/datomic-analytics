(ns spirit.essence.datomic
  (:require [spirit.essence.common :as essence]
            [spirit.essence.datomic.generate :as generate]
            [spirit.essence.datomic.analyse :as analyse]))

(defmethod essence/imbue :datomic
  [_ essence _]
  (generate/datomic essence))

(defmethod essence/transcend :datomic
  [_ corporeal _]
  (analyse/analyse corporeal))
