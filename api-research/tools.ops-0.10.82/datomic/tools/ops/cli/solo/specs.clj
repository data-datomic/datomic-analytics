(ns datomic.tools.ops.cli.solo.specs
  (:require
    [clojure.spec.alpha :as s]
    [datomic.tools.ops.specs :as specs]))

(s/def ::cmd-opts (s/merge (s/keys :req-un [::specs/system]
                                   :opt-un [::specs/wait])
                           ::specs/cli-opts))