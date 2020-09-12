;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

(ns datomic.tools.ops.aws.ec2
  (:require
    [datomic.tools.ops.aws :as ops-aws]
    [datomic.tools.ops.process :as process]))

(set! *warn-on-reflection* true)

(defn instance-id
  "Returns running instance id w/instance-name"
  [ec2-client system instance-name]
  (or (->> (ops-aws/invoke! ec2-client
                            {:op :DescribeInstances
                             :request {:Filters [{:Name "tag:Name"
                                                  :Values [instance-name]}
                                                 {:Name "instance-state-name"
                                                  :Values ["running"]}
                                                 {:Name "tag:datomic:system"
                                                  :Values [system]}]}})
           :Reservations
           (mapcat :Instances)
           (map :InstanceId)
           first)
      (throw (RuntimeException. (str "Error finding instance " instance-name)))))

(defn wait-for-instance-start
  "Wait for instance to start"
  [ec2-client {:keys [instance-name system] :as args}]
  (let [wait-running
        (conj (process/aws-profile-region args)
              "ec2"
              "wait"
              "instance-running"
              "--filters"
              (str "Name=tag:Name,Values=" instance-name)
              (str "Name=tag:datomic:system,Values=" system)
              "Name=instance-state-name,Values=running")]
    (apply process/sh! wait-running))
  (let [iid (instance-id ec2-client system instance-name)
        wait-ok (conj (process/aws-profile-region args)
                      "ec2"
                      "wait"
                      "instance-status-ok"
                      "--instance-id" iid)]

    (apply process/sh! wait-ok)))

(defn wait-for-instance-terminate
  "Wait for instance to terminate"
  [{:keys [instance-id] :as args}]
  (let [wait-terminated (conj (process/aws-profile-region args)
                              "ec2"
                              "wait"
                              "instance-terminated"
                              "--instance-id"
                              instance-id)]
    (apply process/sh! wait-terminated)))