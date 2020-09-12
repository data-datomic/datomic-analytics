(ns datomic.tools.ops.cli.log.specs
  (:require
    [clojure.spec.alpha :as s]
    [datomic.tools.ops.specs :as specs]))

(s/def ::list-opts (s/merge (s/keys :req-un [::specs/group
                                             ::specs/tod
                                             ::specs/filter
                                             ::specs/minutes-back])
                            ::specs/cli-opts))