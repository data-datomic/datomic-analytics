;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

(ns datomic.tools.ops.cli.system
  (:require
    [clojure.data.json :as json]
    [clojure.string :as str]
    [clojure.tools.cli :as cli]
    [datomic.tools.ops.cli.common :as common]
    [datomic.tools.ops.system :as system]))

(set! *warn-on-reflection* true)

(defn- list-instances-usage
  [options-summary]
  (->> ["list-instances - List instances in a Datomic Cloud system"
        ""
        "Synopsis:"
        "  list-instances <system> [parameters]"
        ""
        "Required:"
        "  system: Datomic System"
        ""
        options-summary]
       (str/join \newline)))

(def ^:private system-usage
  (->> ["system - Datomic Cloud system commands"
        ""
        "Available subcommands:"
        "  list-instances:  List instances in a Datomic Cloud system"
        "Options:"
        "  -p, --profile PROFILE                 Named profile from AWS credentials file"]
       (str/join \newline)))

(def ^:private list-instances-opts
  [])

(defn- parse-list-instances-opts
  [{:keys [args]}]
  (let [{:keys [arguments exit-message options] :as parsed}
        (common/parse-opts {:args args
                            :opts list-instances-opts
                            :required-params 1
                            :usage-fn list-instances-usage})]
    (if exit-message
      parsed
      (merge options
             {:subcommand ::list-instances
              :system (first arguments)}))))

(defmethod common/parse-command-parameters :system
  [{:keys [args base-options]}]
  (common/parse-subcommand-parameters
    {:args args
     :base-options base-options
     :subcommands {:list-instances parse-list-instances-opts}
     :usage-fn system-usage}))

(defmethod common/run-subcommand! ::list-instances
  [cfg]
  (json/pprint (system/list-instances cfg)))
