;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

(ns datomic.tools.ops.log
  (:require
    [clojure.data.json :as json]
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [cognitect.aws.client.api :as aws]
    [datomic.tools.ops.aws :as ops-aws]
    [datomic.tools.ops.instant :as instant]
    [datomic.tools.ops.log.specs :as log.specs]
    [datomic.tools.ops.specs :as specs])
  (:import
    [java.util Date]))

(set! *warn-on-reflection* true)

(defn- maybe-json-string?
  [s]
  (str/starts-with? s "{"))

(defn- tag->asg
  "Returns an ASG from results of DescribeTags"
  [tags]
  (-> (into
        []
        (comp (map :Tags)
              cat
              (map :ResourceId))
        tags)
      first))

(defn- group->asg
  "Returns an ASG for group, could be Bastion or Tx"
  [asg-client group]
  (-> (ops-aws/invoke! asg-client {:op :DescribeTags
                                   :request {:Filters [{:Name "key"
                                                        :Values ["aws:cloudformation:stack-name"]}
                                                       {:Name "value"
                                                        :Values [group]}]}})
      :Tags
      first
      :ResourceId))

(defn- asg->system
  "Returns system for asg"
  [asg-client asg]
  (let [tags (->> (ops-aws/invoke! asg-client {:op :DescribeAutoScalingGroups
                                               :request {:AutoScalingGroupNames [asg]}})
                  :AutoScalingGroups
                  (sequence ops-aws/tags-xf)
                  first
                  :Tags)]
    (get tags "datomic:system")))

(defn- group->system
  "Returns system name for group, using optional client-map"
  [{:keys [group client-map]}]
  (let [asg-client (aws/client (merge {:api :autoscaling} client-map))
        asg (group->asg asg-client group)
        system (when asg (asg->system asg-client asg))]

    (when-not system
      (throw (RuntimeException. (str "Error finding system for " group))))
    system))

(def ^:private json-xf
  "Transducer from Datomic Cloud AWS log results to events as Clojure data."
  (comp (map :events)
        cat
        (filter maybe-json-string?)
        (map #(assoc
                (json/read-str (:message %) :key-fn keyword)
                :logStreamName (:logStreamName %)))))

(defn- common-request
  "Parts of the request that are common to alerts and events"
  [{:keys [system group minutes-back ^Date tod instance-id]}]
  (let [end-time (.getTime tod)
        start-time (- end-time (* minutes-back 60 1000))]
    {:endTime end-time
     :logGroupName (str "datomic-" system)
     :logStreamNamePrefix (str/join "-" (cond-> [system group]
                                                instance-id
                                                (conj instance-id)))
     :startTime start-time}))

(defn- alerts-request
  "Returns a map suitable for FilterLogRequests filtered by Alert type.

  Required args:
  :system        Datomic system
  :group         Datomic compute group
  :tod           Time of Day (Date) for events
  :minutes-back  Number of minutes of history to return

  Optional args:
  :instance-id  Limit results to this EC2 instance id"
  [args]
  (merge (common-request args)
         {:filterPattern "{$.Type=Alert}"}))

(defn- messages-request
  "Returns a map suitable for FilterLogRequests with Msg keys.

  Required args:
  :system        Datomic system
  :group         Datomic compute group
  :minutes-back  Number of minutes of history to return
  :tod           Time of Day (Date) for events

  Optional args:
  :instance-id  Limit results to this EC2 instance id"
  [args]
  (merge (common-request args)
         {:filterPattern "{$.Msg=*}"}))

(defn- detail-request
  "Returns a map suitable for FilterLogRequests limited to a specific timestamp.

  Required args:
  :system  Datomic system
  :group   Datomic compute group
  :msg     The timestamp of the log event

  Optional args:
  :instance-id  Limit results to this EC2 instance id"
  [{:keys [^long msg system group instance-id] :as args}]
  ;; Older versions of cloud potentially had different :startTime and log timestamp.
  ;; So, here we search in a time range for the timestamp we are looking for.
  (let [tod (Date. (+ msg (* 5 60 1000)))]
    (merge (common-request (merge args
                                  {:tod tod
                                   :minutes-back 5}))
           {:filterPattern (format "{$.Msg=* && $.Timestamp=%s}" (str msg))})))

(defn- to-datetime
  "Adds :datetime to map"
  [m]
  (let [ts (:Timestamp m)]
    (assoc m
      :datetime (instant/ts->datetime ts))))

(defn- enhance-data
  "Adds to map:
  :datetime
  :group
  :instance-id
  :system"
  [system group m]
  (let [instance-id (last (re-find (re-pattern (str system "-" group "-(i-[^-]*)")) (:logStreamName m)))]
    (merge m
           (to-datetime m)
           {:group group
            :instance-id instance-id
            :system system})))

(s/fdef alerts
        :args (s/cat :args ::log.specs/list-args))
(defn alerts
  "Returns a vector of log results as Clojure data for Datomic log alerts query

  Required args:
  :group   Datomic compute group
  :minutes-back  Number of minutes of history to return
  :tod           Time of Day (Date) for alerts\n

  Optional args:
  :profile  Named profile from AWS credentials file
  :region  AWS Region of the Datomic system
  :instance-id  Limit results to this EC2 instance id"
  [{:keys [group] :as args}]
  (specs/conform! ::log.specs/list-args args)
  (let [client-map (ops-aws/client-args args)
        logs-client (aws/client (merge {:api :logs} client-map))
        system (group->system (merge args
                                     {:client-map client-map}))
        request (alerts-request (merge args {:client-map client-map
                                             :system system}))]
    (into
      []
      (comp json-xf
            (map (partial enhance-data system group)))
      (ops-aws/results-seq logs-client {:op :FilterLogEvents
                                        :request request}))))

(s/fdef events
        :args (s/cat :args ::log.specs/list-args))
(defn events
  "Returns a vector of log results as Clojure data for Datomic log query

  Arg map requires one of the following, mutually exclusive keys:

  Required args:
  :group           Datomic compute group
  :minutes-back    Number of minutes of history to return
  :tod             Time of Day (Date) for events\n

  Optional args:
  :profile  Named profile from AWS credentials file
  :region  AWS Region of the Datomic system
  :instance-id  Limit results to this EC2 instance id"
  [{:keys [group] :as args}]
  (specs/conform! ::log.specs/list-args args)
  (let [client-map (ops-aws/client-args args)
        logs-client (aws/client (merge {:api :logs} client-map))
        system (group->system (merge args
                                     {:client-map client-map}))
        request (messages-request (merge args {:client-map client-map
                                               :system system}))]
    (into
      []
      (comp json-xf
            (map (partial enhance-data system group)))
      (ops-aws/results-seq logs-client {:op :FilterLogEvents
                                        :request request}))))

(s/fdef detail
        :args (s/cat :args ::log.specs/detail-args))
(defn detail
  "Returns a vector of log results as Clojure data for Datomic log query

  Required args:
  :group  Datomic compute group
  :msg     Msg Id of the log event

  Optional args:
  :instance-id  Limit results to this EC2 instance id
  :profile      Named profile from AWS credentials file
  :region       AWS Region of the Datomic system"
  [args]
  (specs/conform! ::log.specs/detail-args args)
  (let [client-map (ops-aws/client-args args)
        logs-client (aws/client (merge {:api :logs} client-map))
        system (group->system (merge args
                                     {:client-map client-map}))
        request (detail-request (merge args {:client-map client-map
                                             :system system}))]
    (into
      []
      (comp json-xf
            (map to-datetime))
      (ops-aws/results-seq logs-client {:op :FilterLogEvents
                                        :request request}))))