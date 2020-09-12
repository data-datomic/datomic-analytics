(ns spirit.essence
  (:require [spirit.essence.common :as essence]))

(defn imbue
  ([realm essence]
   (imbue realm essence {}))
  ([realm essence opts]
   (essence/imbue realm essence opts)))

(defn transcend
  ([realm corporeal]
   (transcend realm corporeal {}))
  ([realm corporeal opts]
   (essence/transcend realm corporeal opts)))
