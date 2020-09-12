(ns datomic.tools.ops.cli.analytics.specs
  (:require
    [clojure.spec.alpha :as s]
    [datomic.tools.ops.specs :as specs]))

(s/def ::access-opts (s/merge (s/keys :req-un [::specs/system]
                                      :opt-un [::specs/port
                                               ::specs/ssho])
                              ::specs/cli-opts))

(s/def ::sync-opts (s/merge (s/keys :req-un [::specs/system
                                             ::specs/directory]
                                    :opt-un [::specs/query-group])
                            ::specs/cli-opts))