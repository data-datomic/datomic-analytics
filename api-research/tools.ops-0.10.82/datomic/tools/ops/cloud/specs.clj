(ns datomic.tools.ops.cloud.specs
  (:require
    [clojure.spec.alpha :as s]
    [datomic.tools.ops.specs :as specs]))

(s/def ::list-systems-args ::specs/client-args)

(s/def ::name ::specs/system)
(s/def ::system-desc (s/keys :req-un [::name ::specs/storage-cft-version ::specs/topology]))
(s/def ::list-systems (s/coll-of ::system-desc))