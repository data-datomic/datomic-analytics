(ns primer.datalog
  (:require [clojure.pprint :as p]
            [datomic.client.api :as d]
            [primer.mbrainz :as mb]))

(defn find-titles-for-artist
  [db artist-name]
  (d/q '[:find ?title
         :in $ ?artist-name
         :where
         [?a :artist/name ?artist-name]
         [?t :track/artists ?a]
         [?t :track/name ?title]] db artist-name))

(defn list-artists
  [db]
  (d/q '[:find ?e ?artist-name
         :where
         [?e :artist/name ?artist-name]]
       db))

(defn attributes
  [db attribute]
  (d/q '[:find ?attribute-name
         :in $ ?attribute
         :where
         [?a ?attribute]
         [?a ?attr]
         [?attr :db/ident ?attribute-name]]
       db attribute))

(defn artists-in
  [db country-code]
  (d/q '[:find ?a ?artist-name
         :in $ ?country-code
         :where
         [?a :artist/country ?country-code]
         [?a :artist/name ?artist-name]]
       db country-code))

(comment
  (def db (d/db mb/conn))
  (p/pprint (attributes db :artist/name))
  (p/pprint (attributes db :release/name))
  (p/pprint (attributes db :track/name))
  (p/pprint (list-artists db))
  (p/pprint (artists-in db :country/JP))
  (p/pprint (find-titles-for-artist (d/db mb/conn) "The Beatles"))
  (p/pprint (find-titles-for-artist (d/db mb/conn) "Carpenters"))
  (p/pprint (find-titles-for-artist (d/db mb/conn) "The Beach Boys"))
  (p/pprint (find-titles-for-artist (d/db mb/conn) "オフコース")))
