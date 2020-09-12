(ns primer.mbrainz
  (:require [datomic.client.api :as d]
            [environ.core :refer [env]]))

(def cfg {:server-type :cloud
          :region      (env :datomic-aws-region)
          :system      (env :datomic-system)
          :query-group (env :datomic-query-group)
          :endpoint    (format "http://entry.%s.%s.datomic.net:8182/"
                               (env :datomic-system) (env :datomic-aws-region))
          :proxy-port  (Integer/parseInt (or (env :datomic-proxy-port)
                                             "8182"))})

(def client (d/client cfg))
(def conn (d/connect client {:db-name "mbrainz-subset"}))

(comment
  (-> (d/db-stats (d/db conn))
      :datoms
      ((partial format "%,d"))))

#_(d/delete-database client {:db-name "mbrainz-subset"})
