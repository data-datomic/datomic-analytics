(ns datomic.tools.ops.cli.cloud.specs
  (:require
    [clojure.spec.alpha :as s]
    [datomic.tools.ops.specs :as specs]))

(s/def ::list-systems-opts ::specs/cli-opts)