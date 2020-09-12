(ns datomic.tools.ops.gateway.specs
  (:require
    [clojure.spec.alpha :as s]
    [datomic.tools.ops.specs :as specs]))

(s/def ::cmd-args (s/merge (s/keys :req-un [::specs/system]
                                   :opt-un [::specs/wait])
                           ::specs/client-args))