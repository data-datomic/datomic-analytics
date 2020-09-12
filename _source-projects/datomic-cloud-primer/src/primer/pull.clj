(ns primer.pull
  (:require [clojure.pprint :as p]
            [datomic.client.api :as d]
            [primer.mbrainz :as mb]))

(defn pull
  [db eid]
  (d/pull db
          {:selector '[*]
           :eid eid}))

(defn pull-with-q
  [db artist-name]
  (d/q '[:find (pull ?r [:release/name
                         :release/year
                         {:release/media [{:medium/tracks [:track/name]}]}])
         :in $ ?artist-name
         :where
         [?a :artist/name ?artist-name]
         [?t :track/artists ?a]
         [?m :medium/tracks ?t]
         [?r :release/media ?m]]
       db artist-name))

(comment
  (def db (d/db mb/conn))
  (pull db 63446218969195742)
  (p/pprint (pull-with-q db "荒井由実"))
  (p/pprint (pull-with-q db "はっぴいえんど"))
  (p/pprint (pull-with-q db "The Beach Boys")))
