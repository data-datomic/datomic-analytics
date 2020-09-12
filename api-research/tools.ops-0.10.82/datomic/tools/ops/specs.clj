;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

(ns datomic.tools.ops.specs
  (:require
    [clojure.spec.alpha :as s]
    [cognitect.aws.credentials :as aws-creds]))

(s/def ::directory string?)
(s/def ::group string?)
(s/def ::filter string?)
(s/def ::instance-id string?)
(s/def ::port pos-int?)
(s/def ::profile string?)
(s/def ::region string?)
(s/def ::ssho (s/coll-of string?))
(s/def ::subcommand keyword?)
(s/def ::system string?)
(s/def ::cft-version string?)
(s/def ::group-cft-version ::cft-version)
(s/def ::storage-cft-version ::cft-version)
(s/def ::topology string?)
(s/def ::query-group string?)
(s/def ::wait boolean?)

(s/def ::datetime string?)
(s/def ::message string?)
(s/def ::type string?)

(s/def ::region string?)
(s/def ::profile string?)

(s/def ::credentials-provider #(satisfies? aws-creds/CredentialsProvider %))

(s/def ::cli-opts (s/keys :req-un [::subcommand]
                          :opt-un [::region ::profile]))

(s/def ::client-args (s/keys :opt-un [::region
                                      ::profile]))

(s/def ::aws-client-args (s/keys :opt-un [::credentials-provider
                                          ::region]))

(defn conform!
  ([spec x] (conform! spec x "Data did not conform"))
  ([spec x context]
   (if (s/valid? spec x)
     (s/conform spec x)
     (throw (ex-info context (s/explain-data spec x))))))