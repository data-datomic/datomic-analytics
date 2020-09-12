(ns primer.rule
  (:require [clojure.pprint :as p]
            [datomic.client.api :as d]
            [primer.mbrainz :as mb]))

(def rules
  '[[(track-info ?artist ?name ?duration)
     [?track :track/artists ?artist]
     [?track :track/name ?name]
     [?track :track/duration ?duration]]])

(comment
  (p/pprint (d/q '[:find ?name ?duration
                   :in $ % ?aname
                   :where [?artist :artist/name ?aname]
                   (track-info ?artist ?name ?duration)]
                 (d/db mb/conn) rules "The Beatles")))

(def benelux-rules
  ;;ルールを併記した場合は論理和となる
  '[[(benelux ?artist)
     [?artist :artist/country :country/BE]]
    [(benelux ?artist)
     [?artist :artist/country :country/NL]]
    [(benelux ?artist)
     [?artist :artist/country :country/LU]]])

(comment
  (p/pprint (d/q '[:find ?name ?country-name
                   :in $ %
                   :where
                   (benelux ?artist)
                   [?artist :artist/name ?name]
                   [?artist :artist/country ?country]
                   [?country :country/name ?country-name]]
                 (d/db mb/conn) benelux-rules)))
