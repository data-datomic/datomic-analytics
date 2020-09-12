;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

(ns datomic.tools.ops.cli.cloud
  (:require
    [clojure.data.json :as json]
    [clojure.string :as str]
    [datomic.tools.ops.cli.common :as common]
    [clojure.tools.cli :as cli]
    [datomic.tools.ops.cloud :as cloud]))

(set! *warn-on-reflection* true)

(def ^:private cloud-usage
  (->> ["cloud - Datomic Cloud commands"
        ""
        "Available subcommands:"
        "  list-systems:  List Datomic Cloud systems"]
       (str/join \newline)))

(defn- list-systems-usage
  [options-summary]
  (->> ["list-systems - list Datomic systems for the specified or sourced AWS profile."
        ""
        "Synopsis:"
        "  list-systems [parameters]"
        ""
        "Required: none"
        ""
        "Options:"
        options-summary]
       (str/join \newline)))

;; support future options
(def ^:private list-systems-opts
  [])

(defn- parse-list-system-opts
  [{:keys [args]}]
  (let [{:keys [exit-message options] :as response}
        (common/parse-opts {:args args
                            :opts list-systems-opts
                            :usage-fn list-systems-usage})]
    (if exit-message
      response
      (merge
        options
        {:subcommand ::list-systems}))))

(defmethod common/parse-command-parameters :cloud
  [{:keys [args base-options]}]
  (common/parse-subcommand-parameters
    {:args args
     :base-options base-options
     :subcommands {:list-systems parse-list-system-opts}
     :usage-fn cloud-usage}))

(defmethod common/run-subcommand! ::list-systems
  [cfg]
  (json/pprint (cloud/list-systems cfg)))
