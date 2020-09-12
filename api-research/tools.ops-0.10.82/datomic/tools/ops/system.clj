;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

(ns datomic.tools.ops.system
  (:require
    [clojure.spec.alpha :as s]
    [cognitect.aws.client.api :as aws]
    [datomic.tools.ops.aws :as ops-aws]
    [datomic.tools.ops.system.specs :as system.specs]
    [datomic.tools.ops.specs :as specs]))

(set! *warn-on-reflection* true)

(defn- tags->group-name
  "Returns group name from tags, special casing access-gateway"
  [tags]
  (if (= (tags "aws:cloudformation:logical-id")
         "BastionAutoScalingGroup")
    "access-gateway"
    (tags "aws:cloudformation:stack-name")))

(s/fdef list-instances
        :args (s/cat :args ::system.specs/list-instances-args))
(defn list-instances
  "Returns a map with instance information
  :group-name   Name of the Datomic group of this instance,
                or \"access-gateway\" for the access gateway
  :instance-id  EC2 instance id
  :status       EC2 instance status

  Required args:
  :system  Datomic system

  Optional args:
  :profile      Named profile from AWS credentials file
  :region       AWS Region of the Datomic system"
  [{:keys [system] :as args}]
  (specs/conform! ::system.specs/list-instances-args args)
  (let [client-map (ops-aws/client-args args)
        ec2-client (aws/client (merge {:api :ec2} client-map))
        cf-client (aws/client (merge {:api :cloudformation} client-map))
        request {:Filters [{:Name "tag:datomic:system"
                            :Values [system]}]}
        results (ops-aws/results-seq ec2-client {:op :DescribeInstances
                                                 :request request})
        stack-cache (atom {})]
    (into
      []
      (comp
        ops-aws/describe-instances-xf
        (map (fn [{:keys [InstanceId State Tags]}]
               (let [group-name (tags->group-name Tags)
                     stack-id (Tags "aws:cloudformation:stack-id")
                     outputs (when stack-id
                               (or (@stack-cache stack-id)
                                   (try (->> (ops-aws/invoke! cf-client {:op :DescribeStacks
                                                                         :request {:StackName stack-id}})
                                             :Stacks
                                             (sequence ops-aws/outputs-xf)
                                             first
                                             :Outputs)
                                        (catch Throwable _
                                          nil))))]
                 (when outputs (swap! stack-cache assoc stack-id outputs))
                 {:group-name group-name
                  :group-cft-version (or (get outputs "DatomicCFTVersion")
                                         "Unknown")
                  :group-cloud-version (or (get outputs "DatomicCloudVersion")
                                           "Unknown")
                  :instance-id InstanceId
                  :status (:Name State)}))))
      results)))