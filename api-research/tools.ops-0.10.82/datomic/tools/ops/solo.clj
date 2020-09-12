;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

(ns datomic.tools.ops.solo
  (:require
    [cognitect.aws.client.api :as aws]
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [datomic.tools.ops.aws :as ops-aws]
    [datomic.tools.ops.aws.ec2 :as aws.ec2]
    [datomic.tools.ops.solo.specs :as solo.specs]
    [datomic.tools.ops.specs :as specs]))

(set! *warn-on-reflection* true)

(defn- system->asg-names
  "Returns ASG ResourceIds for a system"
  [asg-client system]
  (->> (ops-aws/invoke! asg-client {:op :DescribeTags
                                    :request {:Filters [{:Name "key"
                                                         :Values ["datomic:system"]}
                                                        {:Name "value"
                                                         :Values [system]}]}})
       :Tags
       (map :ResourceId)
       (into #{})))

(defn get-asgs
  "Returns collection of ASG descriptions given collection of ASG names"
  [asg-client asg-names]
  (let [results (ops-aws/results-seq asg-client {:op :DescribeAutoScalingGroups
                                                 :request {:AutoScalingGroupNames asg-names}})]
    (into
      []
      ops-aws/describe-asg-xf
      results)))

(defn- verify-solo-asg!
  "Throws if any asgs are not solo"
  [asgs]
  (let [solo-systems (reduce
                       (fn [acc {:keys [Tags]}]
                         (if (= (Tags "datomic:topology") "solo")
                           (conj acc (Tags "datomic:system"))
                           acc))
                       #{}
                       asgs)]

    (when-let [prod-asgs
               (seq
                 (reduce
                   (fn [acc {:keys [Tags]}]
                     (if-not (contains? solo-systems (Tags "datomic:system"))
                       (conj acc (Tags "datomic:system"))
                       acc))
                   #{}
                   asgs))]
      (throw (RuntimeException. (str "Non solo systems provided: " (str/join "," prod-asgs)))))))

(defn- system->instances
  "Returns non-terminated instances for system"
  [ec2-client system]
  (->> (ops-aws/invoke! ec2-client
                        {:op :DescribeInstances
                         :request {:Filters [{:Name "tag:datomic:system"
                                              :Values [system]}
                                             {:Name "instance-state-name"
                                              :Values ["running"
                                                       "pending"
                                                       "stopped"
                                                       "stopping"]}]}})
       :Reservations
       (mapcat :Instances)
       (map :InstanceId)))

(s/fdef down
        :args (s/cat :args ::solo.specs/cmd-args))
(defn down
  "Stop solo node and access gateway instances, setting the ASGs to 0

  Required args:
    :system  Datomic system

  Optional args:
    :profile   Named profile from AWS credentials file
    :region    AWS Region of the Datomic system
    :progress  callback called with every instance
    :wait      Wait for instances to transition"
  [{:keys [system progress wait] :as args
    :or {progress println}}]
  (specs/conform! ::solo.specs/cmd-args args)
  (let [client-map (ops-aws/client-args args)
        asg-client (aws/client (merge {:api :autoscaling} client-map))
        asg-names (system->asg-names asg-client system)
        asgs (when (seq asg-names) (get-asgs asg-client asg-names))
        ec2-client (aws/client (merge {:api :ec2} client-map))
        gateway-id (aws.ec2/instance-id ec2-client system (str system "-bastion"))
        solo-id (aws.ec2/instance-id ec2-client system system)]
    (when-not (seq asgs)
      (throw (RuntimeException. (str "Error finding system " system))))
    (verify-solo-asg! asgs)
    (doseq [asg asg-names]
      (progress (str "Downing " asg))
      (ops-aws/set-asg-size asg-client asg 0))

    (when wait
      (progress "Waiting for instances to terminate.")
      (aws.ec2/wait-for-instance-terminate
        (merge args
               {:instance-id gateway-id}))
      (aws.ec2/wait-for-instance-terminate
        (merge args
               {:instance-id solo-id}))
      (progress "Done"))))

(s/fdef reset
        :args (s/cat :args ::solo.specs/cmd-args))
(defn reset
  "Terminate solo node and access gateway instances if they are running, allowing the ASGs to start new instances

  Required args:
    :system  Datomic system

  Optional args:
    :profile  Named profile from AWS credentials file
    :region   AWS Region of the Datomic system
    :wait     Wait for instances to transition"
  [{:keys [system progress wait] :as args
    :or {progress println}}]
  (specs/conform! ::solo.specs/cmd-args args)
  (let [client-map (ops-aws/client-args args)
        asg-client (aws/client (merge {:api :autoscaling} client-map))
        asg-names (system->asg-names asg-client system)
        asgs (when (seq asg-names) (get-asgs asg-client asg-names))
        _ (when-not (seq asgs)
            (throw (RuntimeException. (str "Error finding system " system))))
        _ (verify-solo-asg! asgs)
        ec2-client (aws/client (merge {:api :ec2} client-map))
        instances (system->instances ec2-client system)]
    (if (seq instances)
      (do
        (progress (str "Terminating instances: " (str/join ", " instances)))
        (ops-aws/invoke! ec2-client
                         {:op :TerminateInstances
                          :request {:InstanceIds instances}})
        (when wait
          (progress "Waiting for instances to terminate.")
          (doseq [instance instances]
            (aws.ec2/wait-for-instance-terminate
              (merge args
                     {:instance-id instance})))
          (progress "Waiting for instances to start.")
          (aws.ec2/wait-for-instance-start ec2-client
                                           (merge args
                                                  {:instance-name (str system "-bastion")
                                                   :system system}))
          (aws.ec2/wait-for-instance-start ec2-client
                                           (merge args
                                                  {:instance-name system
                                                   :system system}))
          (progress "Done")))
      (progress (str "No instances found in system " system)))))

(s/fdef up
        :args (s/cat :args ::solo.specs/cmd-args))
(defn up
  "Starts solo node and access gateway instances, setting the ASGs to 1

  Required args:
    :system  Datomic system

  Optional args:
    :profile  Named profile from AWS credentials file
    :region   AWS Region of the Datomic system
    :wait     Wait for instances to transition"
  [{:keys [system progress wait] :as args
    :or {progress println}}]
  (specs/conform! ::solo.specs/cmd-args args)
  (let [client-map (ops-aws/client-args args)
        asg-client (aws/client (merge {:api :autoscaling} client-map))
        asg-names (system->asg-names asg-client system)
        asgs (when (seq asg-names) (get-asgs asg-client asg-names))
        _ (verify-solo-asg! asgs)]
    (when-not (seq asgs)
      (throw (RuntimeException. (str "Error finding system " system))))
    (doseq [asg asg-names]
      (progress (str "Upping " asg))
      (ops-aws/set-asg-size asg-client asg 1))

    (if wait
      (let [ec2-client (aws/client (merge {:api :ec2} client-map))
            gateway-name (str system "-bastion")]
        (progress "Waiting for gateway to start.")
        (aws.ec2/wait-for-instance-start ec2-client
                                         (merge args
                                                {:instance-name gateway-name
                                                 :system system}))
        (progress "Waiting for solo node to start.")
        (aws.ec2/wait-for-instance-start ec2-client
                                         (merge args
                                                {:instance-name system
                                                 :system system}))
        (progress "Done")))))