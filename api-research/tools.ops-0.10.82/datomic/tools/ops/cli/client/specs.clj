(ns datomic.tools.ops.cli.client.specs
  (:require
    [clojure.spec.alpha :as s]
    [datomic.tools.ops.specs :as specs]))

(s/def ::access-opts (s/merge (s/keys :req-un [::specs/system]
                                      :opt-un [::specs/port
                                               ::specs/ssho])
                              ::specs/cli-opts))