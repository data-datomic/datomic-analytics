(ns datomic.tools.ops.analytics.specs
  (:require
    [clojure.spec.alpha :as s]
    [datomic.tools.ops.specs :as specs]))

(s/def ::s3-sync-args (s/merge (s/keys :req-un [::specs/system
                                                ::specs/directory]
                                       :opt-un [::specs/query-group])
                               ::specs/client-args))