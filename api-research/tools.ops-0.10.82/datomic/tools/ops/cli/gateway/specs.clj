(ns datomic.tools.ops.cli.gateway.specs
  (:require
    [clojure.spec.alpha :as s]
    [datomic.tools.ops.specs :as specs]))


(s/def ::gateway-opts (s/merge (s/keys :req-un [::specs/system]
                                       :opt-un [::specs/wait])
                               ::specs/cli-opts))