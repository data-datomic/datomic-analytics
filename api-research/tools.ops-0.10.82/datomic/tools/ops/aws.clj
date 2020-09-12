;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

(ns datomic.tools.ops.aws
  (:require
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [cognitect.aws.client.api :as aws]
    [cognitect.aws.credentials :as aws-creds]
    [datomic.tools.ops.specs :as specs]))

(set! *warn-on-reflection* true)

(defn- aws-key-xf
  "Returns transducer from AWS map structure under stucture-key
   {:k k :v v} to map of k v"
  [structure-key k v]
  (map
    (fn [m]
      (update m structure-key
              (fn [items]
                (let [new-map (into {}
                                    (map
                                      (fn [{key k value v}]
                                        {key value})
                                      items))]
                  new-map))))))

(def tags-xf
  "Transducer from AWS Tags from {:Key k :Value v} to map of k v"
  (aws-key-xf :Tags :Key :Value))

(def outputs-xf
  "Transducer from CFT Outputs from {:Key k :Value v} to map of k v"
  (aws-key-xf :Outputs :OutputKey :OutputValue))

(def describe-asg-xf
  "Transducer from DescribeAutoScalingGroups to tagged sequence of results"
  (comp
    (map :AutoScalingGroups)
    cat
    tags-xf))

(def describe-instances-xf
  "Transducer from DescribeInstances to tagged sequence of results"
  (comp
    (map :Reservations)
    cat
    (map :Instances)
    cat
    tags-xf))

(s/fdef client-args
        :args (s/cat :args ::specs/client-args))
(defn client-args
  "Builds client config map for AWS lib

  Optional keys

  :region AWS Region of the Datomic system
  :profile Named profile from AWS credentials file"
  [{:keys [profile region] :as args}]
  (specs/conform! ::specs/client-args args)
  (cond-> {}
          profile
          (assoc :credentials-provider (aws-creds/profile-credentials-provider profile))
          region
          (assoc :region region)))

(defn invoke!
  "Calls aws/invoke, throw on anomaly"
  [aws-client args]
  (let [result (aws/invoke aws-client args)]
    (when (:cognitect.anomalies/category result)
      (let [message (or (:cognitect.anomalies/message result)
                        (:message result)
                        (:Message result)
                        (get-in result [:Error :Message])
                        (get-in result [:ErrorResponse :Error :Message]))
            str-message (cond-> "AWS Error"
                                message
                                (str ": " message))]
        (throw (ex-info str-message result))))
    result))

(defn results-seq
  "Returns a lazy seq of results from invoking AWS aws-client with args, chasing
down the nextToken on each request."
  [aws-client args]
  (lazy-seq
    (let [result (invoke! aws-client args)
          ;; :nextToken == Logs
          ;; :NextToken == ASG
          next-request (select-keys result [:nextToken :NextToken])]

      (if (empty? next-request)
        [result]
        (cons result (results-seq aws-client (update args :request merge next-request)))))))

(defn get-bucket
  "Returns bucket with datomic:system tag"
  [tagging-client system]
  (let [s3-arn (or (->> (invoke! tagging-client {:op :GetResources
                                                 :request {:ResourceTypeFilters ["s3"]
                                                           :TagFilters [{:Key "datomic:system"
                                                                         :Values [system]}]}})
                        :ResourceTagMappingList
                        (map :ResourceARN)
                        first)
                   (throw (RuntimeException. (str "Error finding S3 bucket for " system))))]
    (last (str/split s3-arn #":"))))

(defn get-ip
  "Returns ip of instance w/name"
  [ec2-client instance-name]
  (or (->> (invoke! ec2-client {:op :DescribeInstances
                                :request {:Filters [{:Name "tag:Name"
                                                     :Values [instance-name]}
                                                    {:Name "instance-state-name"
                                                     :Values ["running"]}]}})
           :Reservations
           (mapcat :Instances)
           first
           :PublicIpAddress)
      (throw (RuntimeException. "Gateway not found, make sure gateway is running."))))

(defn set-asg-size
  "Sets ASG size"
  [asg-client asg size]
  (invoke! asg-client {:op :UpdateAutoScalingGroup
                       :request {:AutoScalingGroupName asg
                                 :DesiredCapacity size
                                 :MinSize size}}))