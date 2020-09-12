;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

(ns datomic.tools.ops.cloud
  (:require
    [clojure.spec.alpha :as s]
    [cognitect.aws.client.api :as aws]
    [datomic.tools.ops.aws :as ops-aws]
    [datomic.tools.ops.cloud.specs :as cloud.specs]
    [datomic.tools.ops.specs :as specs]))

(set! *warn-on-reflection* true)

(s/fdef list-systems
        :args (s/cat :args ::cloud.specs/list-systems-args))
(defn list-systems
  "Returns a map description of systems:
  :name                 The name of the system
  :topology             solo or prod
  :storage-cft-version  Cloudformation version of storage template

  Optional args:
  :profile      Named profile from AWS credentials file
  :region       AWS Region of the Datomic system"
  [args]
  (specs/conform! ::cloud.specs/list-systems-args args)
  (let [client-map (ops-aws/client-args args)
        asg-client (aws/client (merge {:api :autoscaling} client-map))
        cf-client (aws/client (merge {:api :cloudformation} client-map))
        results (ops-aws/results-seq asg-client {:op :DescribeAutoScalingGroups})]
    (into
      []
      (comp
        ops-aws/describe-asg-xf
        (filter (fn [{:keys [Tags]}]
                  (= (Tags "aws:cloudformation:logical-id") "TxAutoScalingGroup")))
        (map (fn [{:keys [Tags]}]
               (let [system (Tags "datomic:system")
                     outputs (when system (->> (ops-aws/invoke! cf-client {:op :DescribeStacks
                                                                           :request {:StackName system}})
                                               :Stacks
                                               (sequence ops-aws/outputs-xf)
                                               first
                                               :Outputs))]
                 {:name system
                  :storage-cft-version (or (get outputs "DatomicCFTVersion")
                                           "Unknown")
                  :topology (or (get Tags "datomic:topology")
                                "Unknown")}))))
      results)))