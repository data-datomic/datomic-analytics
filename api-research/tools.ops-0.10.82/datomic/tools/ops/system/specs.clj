(ns datomic.tools.ops.system.specs
  (:require
    [clojure.spec.alpha :as s]
    [datomic.tools.ops.specs :as specs]))

(s/def ::group-name ::specs/group)
(s/def ::status string?)
(s/def ::group-cloud-version string?)
(s/def ::instances (s/keys :req-un [::group-cloud-version
                                    ::group-name
                                    ::specs/group-cft-version
                                    ::specs/instance-id
                                    ::status]))
(s/def ::list-instances-args (s/merge (s/keys :req-un [::specs/system])
                                      ::specs/client-args))