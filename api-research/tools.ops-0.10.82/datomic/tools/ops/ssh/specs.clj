(ns datomic.tools.ops.ssh.specs
  (:require
    [clojure.spec.alpha :as s]
    [datomic.tools.ops.specs :as specs]))

(s/def ::access-type keyword?)
(s/def ::access-args (s/merge (s/keys :req-un [::specs/system
                                               ::access-type]
                                      :opt-un [::specs/port
                                               ::specs/ssho])
                              ::specs/client-args))