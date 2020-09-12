;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

(ns datomic.tools.ops.cli.log
  (:require
    [clojure.data.json :as json]
    [clojure.edn :as edn]
    [clojure.instant :as inst]
    [clojure.string :as str]
    [clojure.tools.cli :as cli]
    [datomic.tools.ops.cli.common :as common]
    [datomic.tools.ops.log :as log]))

(set! *warn-on-reflection* true)

(defn- detail-usage
  [options-summary]
  (->> ["detail - Details of Datomic logs, returns JSON array"
        ""
        "Synopsis:"
        "  detail <group> [parameters]"
        ""
        "Required:"
        "  group: Datomic Compute Group"
        ""
        "Options:"
        options-summary]
       (str/join \newline)))

(def ^:private log-usage
  (->> ["log - Query Datomic logs"
        ""
        "Available subcommands:"
        "  list:    List logs for a time range"
        "  detail:  Log details for a specific message"]
       (str/join \newline)))

(defn- list-usage
  [options-summary]
  (->> ["list - Query Datomic logs for a time range"
        ""
        "Synopsis:"
        "  list <group> [parameters]"
        ""
        "Required:"
        "  group: Datomic Compute Group"
        ""
        "Options:"
        options-summary]
       (str/join \newline)))

(def ^:private list-opts
  [["-f" "--filter FILTER" "The type of logs to display: \"alerts\" (default) or \"all\""
    :validate-fn [#{"all" "alerts"}]
    :default "alerts"]
   ["-t" "--tod TIME_OF_DAY" "End time range of logs. Formatted as a Clojure #inst: yyyy-mm-ddThh:mm:ss+hh:mm."
    :parse-fn inst/read-instant-date
    :default-desc "Current time"
    :default (java.util.Date.)]
   ["-m" "--minutes-back MINUTES_BACK" "Preceding minutes of history to display"
    :parse-fn edn/read-string
    :default 20
    :validate-fn int?]
   ["-i" "--instance-id INSTANCE_ID_PREFIX" "Limit log results to EC2 instances with this prefix"]])

(def ^:private detail-opts
  [["-m" "--msg MSG" "Message id (provided by log list command)"
    :parse-fn edn/read-string
    :validate-fn int?
    :missing "Message id required"]
   ["-i" "--instance-id INSTANCE_ID_PREFIX" "Limit log results to EC2 instances with this prefix"]])

(defn- parse-list-opts
  [{:keys [args]}]
  (let [{:keys [arguments exit-message options] :as parsed}
        (common/parse-opts {:args args
                            :opts list-opts
                            :required-params 1
                            :usage-fn list-usage})]
    (if exit-message
      parsed
      (merge options
             {:group (first arguments)
              :subcommand ::list}))))

(defn- parse-detail-opts
  [{:keys [args]}]
  (let [{:keys [arguments exit-message options] :as parsed}
        (common/parse-opts {:args args
                            :opts detail-opts
                            :required-params 1
                            :usage-fn detail-usage})]
    (if
      exit-message
      parsed
      (merge options
             {:group (first arguments)
              :subcommand ::detail}))))

(defmethod common/parse-command-parameters :log
  [{:keys [args base-options]}]
  (common/parse-subcommand-parameters
    {:args args
     :base-options base-options
     :subcommands {:list parse-list-opts
                   :detail parse-detail-opts}
     :usage-fn log-usage}))

(defn- format-nested-exception
  [{:keys [Via Cause]}]
  (cond-> []
          Cause
          (conj Cause)
          (seq Via)
          (concat (mapv (fn [{:keys [Type Message]}]
                          (str/join ": " [Type Message]))
                        Via))))

(defn- format-alert
  [alert]
  (let [msg (cond-> [(str "========== " (:datetime alert) " IID: " (:instance-id alert) " MSG: " (:Timestamp alert) " ==========")
                     (:Msg alert)]
                    (:Ex alert)
                    (concat (format-nested-exception (:Ex alert))))]
    (str/join \newline msg)))

(defn- format-event
  [event]
  (->> [(str "========== " (:datetime event) " IID: " (:instance-id event) " MSG: " (:Timestamp event) " ==========")
        (:Msg event)]
       (str/join \newline)))

(defn- print-alert-logs
  [cfg]
  (if-let [alerts (seq (log/alerts cfg))]
    (doseq [alert alerts]
      (println (format-alert alert)))
    (println "No alerts found")))

(defn- print-all-logs
  [cfg]
  (if-let [events (seq (log/events cfg))]
    (doseq [event events]
      (println (format-event event)))
    (println "No logs found")))

(defmethod common/run-subcommand! ::list
  [{:keys [filter] :as cfg}]
  (if (= filter "alerts")
    (print-alert-logs cfg)
    (print-all-logs cfg)))

(defmethod common/run-subcommand! ::detail
  [cfg]
  (json/pprint (log/detail cfg)))
