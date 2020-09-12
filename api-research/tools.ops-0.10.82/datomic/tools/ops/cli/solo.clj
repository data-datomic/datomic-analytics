;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

(ns datomic.tools.ops.cli.solo
  (:require
    [clojure.string :as str]
    [clojure.tools.cli :as cli]
    [datomic.tools.ops.cli.common :as common]
    [datomic.tools.ops.solo :as solo]))

(set! *warn-on-reflection* true)

(def ^:private solo-usage
  (->> ["solo - Solo system commands"
        ""
        "Available subcommands:"
        "  down:   Stop solo system instances"
        "  reset:  Terminate the solo node and access gateway instances if either is running, allowing the ASGs to start new instances"
        "  up:     Start solo system instances"]
       (str/join \newline)))

(defn- down-usage
  [options-summary]
  (->> ["down - Stop solo node and access gateway instances, setting the ASGs to 0"
        ""
        "Synopsis:"
        "  down <system> [parameters]"
        ""
        "Required:"
        "  system: Datomic System"
        ""
        options-summary]
       (str/join \newline)))

(defn- reset-usage
  [options-summary]
  (->> ["reset - Terminate solo node and access gateway instances, allowing the ASGs to start new instances"
        ""
        "Synopsis:"
        "  reset <system> [parameters]"
        ""
        "Required:"
        "  system: Datomic System"
        ""
        options-summary]
       (str/join \newline)))

(defn- up-usage
  [options-summary]
  (->> ["up - Start solo solo node and access gateway instances, setting the ASGs to 1"
        ""
        "Synopsis:"
        "  up <system> [parameters]"
        ""
        "Required:"
        "  system: Datomic System"
        ""
        options-summary]
       (str/join \newline)))

(def ^:private solo-opts
  [[nil "--wait" "Wait until the command against the instances finished."]])

(defn- parse-up-opts
  [{:keys [args]}]
  (let [{:keys [arguments exit-message options] :as parsed}
        (common/parse-opts {:args args
                            :opts solo-opts
                            :required-params 1
                            :usage-fn up-usage})]
    (if exit-message
      parsed
      (merge options
             {:subcommand ::up
              :system (first arguments)}))))

(defn- parse-down-opts
  [{:keys [args]}]
  (let [{:keys [arguments exit-message options] :as parsed}
        (common/parse-opts {:args args
                            :opts solo-opts
                            :required-params 1
                            :usage-fn down-usage})]
    (if exit-message
      parsed
      (merge options
             {:subcommand ::down
              :system (first arguments)}))))

(defn- parse-reset-opts
  [{:keys [args]}]
  (let [{:keys [arguments exit-message options] :as parsed}
        (common/parse-opts {:args args
                            :opts solo-opts
                            :required-params 1
                            :usage-fn reset-usage})]
    (if exit-message
      parsed
      (merge options
             {:subcommand ::reset
              :system (first arguments)}))))

(defmethod common/parse-command-parameters :solo
  [{:keys [args base-options]}]
  (common/parse-subcommand-parameters
    {:args args
     :base-options base-options
     :subcommands {:up parse-up-opts
                   :down parse-down-opts
                   :reset parse-reset-opts}
     :usage-fn solo-usage}))

(defmethod common/run-subcommand! ::down
  [cfg]
  (solo/down cfg))

(defmethod common/run-subcommand! ::reset
  [cfg]
  (solo/reset cfg))

(defmethod common/run-subcommand! ::up
  [cfg]
  (solo/up cfg))
