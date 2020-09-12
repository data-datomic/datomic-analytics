(ns datomic.tools.ops.log.specs
  (:require
    [clojure.spec.alpha :as s]
    [datomic.tools.ops.specs :as specs]))

(s/def ::minutes-back int?)
(s/def ::msg pos-int?)
(s/def ::tod inst?)

(s/def ::Msg string?)
(s/def ::Timestamp int?)
(s/def ::ex-map (s/keys :req-un [::Type]
                        :opt-un [::Message]))
(s/def ::Cause string?)
(s/def ::Via (s/coll-of ::ex-map))
(s/def ::Ex (s/keys :req-un [::Via ::Cause]))

(s/def ::detail-args (s/merge (s/keys :req-un [::specs/group
                                               ::msg]
                                      :opt-un [::specs/instance-id])
                              ::specs/client-args))

(s/def ::list-args (s/merge (s/keys :req-un [::specs/group
                                             ::tod
                                             ::minutes-back]
                                    :opt-un [::specs/instance-id])
                            ::specs/client-args))

(s/def ::alert-message (s/keys :req-un [::Msg
                                        ::Timestamp
                                        ::specs/datetime
                                        ::specs/group
                                        ::specs/system
                                        ::specs/instance-id]
                               :opt-un [::Ex]))
(s/def ::detail-message (s/keys :req-un [::Msg
                                         ::Timestamp
                                         ::specs/datetime]))
(s/def ::event-message (s/keys :req-un [::Msg
                                        ::Timestamp
                                        ::specs/datetime
                                        ::specs/group
                                        ::specs/system
                                        ::specs/instance-id]))