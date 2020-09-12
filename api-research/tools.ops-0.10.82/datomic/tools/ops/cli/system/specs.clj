(ns datomic.tools.ops.cli.system.specs
  (:require
    [clojure.spec.alpha :as s]
    [datomic.tools.ops.specs :as specs]))

(s/def ::list-instances-opts (s/merge (s/keys :req-un [::specs/system])
                                      ::specs/cli-opts))