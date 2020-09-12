;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

(ns datomic.tools.ops.gateway
  (:require
    [cognitect.aws.client.api :as aws]
    [clojure.spec.alpha :as s]
    [datomic.tools.ops.aws :as ops-aws]
    [datomic.tools.ops.aws.ec2 :as aws.ec2]
    [datomic.tools.ops.gateway.specs :as gateway.specs]
    [datomic.tools.ops.specs :as specs]
    [datomic.tools.ops.process :as process]
    [datomic.tools.ops.ssh :as ssh]))

(set! *warn-on-reflection* true)

(defn- asg-name
  "Returns asg from asg name tag"
  [asg-client asg-tag-name]
  (or (->> (ops-aws/invoke! asg-client {:op :DescribeTags
                                        :request {:Filters [{:Name "value"
                                                             :Values [asg-tag-name]}]}})
           :Tags
           (map :ResourceId)
           first)
      (throw (RuntimeException. (str "Error finding access gateway " asg-tag-name)))))

(s/fdef disable
        :args (s/cat :args ::gateway.specs/cmd-args))
(defn disable
  "Disables the access gateway, setting the ASG to 0

  Required args:
    :system  Datomic system

  Optional args:
    :profile   Named profile from AWS credentials file
    :region    AWS Region of the Datomic system
    :wait      Wait for instance to transition"
  [{:keys [progress system wait] :as args
    :or {progress println}}]
  (specs/conform! ::gateway.specs/cmd-args args)
  (let [gateway-name (str system "-bastion")
        client-map (ops-aws/client-args args)
        asg-client (aws/client (merge {:api :autoscaling} client-map))
        asg-name (asg-name asg-client gateway-name)
        ec2-client (aws/client (merge {:api :ec2} client-map))]
    (ops-aws/set-asg-size asg-client asg-name 0)

    (if wait
      (let [iid (aws.ec2/instance-id ec2-client system gateway-name)]
        (progress "Waiting for gateway to terminate.")
        (aws.ec2/wait-for-instance-terminate
          (merge args
                 {:instance-id iid}))
        (progress "Done"))
      (progress "Access Gateway is scheduled for termination"))))

(s/fdef enable
        :args (s/cat :args ::gateway.specs/cmd-args))
(defn enable
  "Enable the access gateway, setting the ASG to 1

  Required args:
    :system  Datomic system

  Optional args:
    :profile   Named profile from AWS credentials file
    :region    AWS Region of the Datomic system
    :wait      Wait for instance to transition"
  [{:keys [progress system wait] :as args
    :or {progress println}}]
  (specs/conform! ::gateway.specs/cmd-args args)
  (let [gateway-name (str system "-bastion")
        client-map (ops-aws/client-args args)
        asg-client (aws/client (merge {:api :autoscaling} client-map))
        asg-name (asg-name asg-client gateway-name)
        ec2-client (aws/client (merge {:api :ec2} client-map))]
    (ops-aws/set-asg-size asg-client asg-name 1)

    (if wait
      (do
        (progress "Waiting for gateway to start.")
        (aws.ec2/wait-for-instance-start ec2-client
                                 (merge args
                                        {:instance-name gateway-name
                                         :system system}))
        (progress "Done"))
      (progress "Access Gateway is starting, it may take up to a few minutes before it is available for establishing connections"))))

(s/fdef restart
        :args (s/cat :args ::gateway.specs/cmd-args))
(defn restart
  "Restarts the analytics support on the access gateway

  Required args:
    :system  Datomic system

  Optional args:
    :profile   Named profile from AWS credentials file
    :region    AWS Region of the Datomic system"
  [{:keys [progress region system ssho] :as args
    :or {progress println}}]
  (let [client-map (ops-aws/client-args args)
        tagging-client (aws/client (merge {:api :resourcegroupstaggingapi} client-map))
        bucket (ops-aws/get-bucket tagging-client system)
        s3-client (aws/client (merge {:api :s3} client-map))
        {:keys [host-key private-key]} (ssh/retrieve-keys s3-client bucket system region)
        ec2-client (aws/client (merge {:api :ec2} client-map))
        ip (ops-aws/get-ip ec2-client (str system "-bastion"))
        ssh-options (interleave (repeat "-o") ssho)
        cli-params (into [] (flatten ["ssh" "-v" "-o" (str "UserKnownHostsFile=" host-key) ssh-options "-i" private-key "-CT" (str "ec2-user@" ip) "(sudo \"/opt/datomic/presto-stop\")"]))]
    (apply process/sh! cli-params))

  (progress "Access Gateway is restarting"))