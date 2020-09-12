;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

(ns datomic.tools.ops.cli.analytics
  (:require
    [clojure.edn :as edn]
    [clojure.string :as str]
    [datomic.tools.ops.cli.common :as common]
    [datomic.tools.ops.analytics :as analytics]
    [datomic.tools.ops.ssh :as ssh]))

(set! *warn-on-reflection* true)

(def ^:private analytics-usage
  (->> ["analytics - Analytics commands"
        ""
        "Available subcommands:"
        "  access:  Open an SSH tunnel to the access gateway in the system for analytics support"
        "  sync:    Sync analytics config files from local directory to S3"]
       (str/join \newline)))

(defn- access-usage
  [options-summary]
  (->> ["access - Open an SSH tunnel to the access gateway in the system for analytics support"
        ""
        "Synopsis:"
        "  access <system> [parameters]"
        ""
        "Required:"
        "  system: Datomic System"
        ""
        "Options:"
        options-summary]
       (str/join \newline)))

(defn- sync-usage
  [options-summary]
  (->> ["sync - Sync analytics config files from local directory to S3"
        ""
        "Synopsis:"
        "  sync <system> <directory> [parameters]"
        ""
        "Required:"
        "  system:     Datomic System"
        "  directory:  Local directory to sync"
        ""
        "Options:"
        options-summary]
       (str/join \newline)))

(def ^:private access-opts
  [[nil "--port PORT" "Local port to use for forwarding"
    :default 8989
    :parse-fn edn/read-string
    :validate-fn int?]
   [nil "--ssho OPTION" "Pass ssh configuration options as if using ssh -o <option>"
    :assoc-fn (fn [m k v] (update m k conj v))
    :default ["IdentitiesOnly=yes"]
    :default-desc ""]])

(def ^:private sync-opts
  [["-q" "--query-group QUERY_GROUP" "Datomic Query Group to target for sync (defaults to the primary compute group)"]])

(defn- parse-access-opts
  [{:keys [args]}]
  (let [{:keys [arguments exit-message options] :as parsed}
        (common/parse-opts {:args args
                            :opts access-opts
                            :required-params 1
                            :usage-fn access-usage})]
    (if exit-message
      parsed
      (merge options
             {:subcommand ::access
              :system (first arguments)}))))

(defn- parse-sync-opts
  [{:keys [args]}]
  (let [{:keys [arguments exit-message options] :as parsed}
        (common/parse-opts {:args args
                            :opts sync-opts
                            :required-params 2
                            :usage-fn sync-usage})]
    (if exit-message
      parsed
      (merge options
             {:subcommand ::sync}
             (zipmap [:system :directory] arguments)))))

(defmethod common/parse-command-parameters :analytics
  [{:keys [args base-options]}]
  (common/parse-subcommand-parameters
    {:args args
     :base-options base-options
     :subcommands {:access parse-access-opts
                   :sync parse-sync-opts}
     :usage-fn analytics-usage}))

(defmethod common/run-subcommand! ::access
  [cfg]
  (ssh/access (assoc cfg :access-type :analytics)))

(defmethod common/run-subcommand! ::sync
  [cfg]
  (analytics/s3-sync cfg))