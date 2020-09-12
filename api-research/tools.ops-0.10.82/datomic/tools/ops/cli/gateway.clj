;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

(ns datomic.tools.ops.cli.gateway
  (:require
    [clojure.string :as str]
    [clojure.tools.cli :as cli]
    [datomic.tools.ops.cli.common :as common]
    [datomic.tools.ops.gateway :as gateway]))

(set! *warn-on-reflection* true)

(def ^:private gateway-usage
  (->> ["gateway - Access Gateway commands"
        ""
        "Available subcommands:"
        "  disable:  Disable the access gateway, setting the ASG to 0"
        "  enable:   Enable the access gateway, setting the ASG to 1"
        "  restart:  Restarts the analytics support on the access gateway"]
       (str/join \newline)))

(defn- disable-usage
  [options-summary]
  (->> ["disable:  Disable the access gateway, setting the ASG to 0"
        ""
        "Synopsis:"
        "  disable <system> [parameters]"
        ""
        "Required:"
        "  system: Datomic System"
        ""
        options-summary]
       (str/join \newline)))

(defn- enable-usage
  [options-summary]
  (->> ["enable:  Enable the access gateway, setting the ASG to 1"
        ""
        "Synopsis:"
        "  enable <system> [parameters]"
        ""
        "Required:"
        "  system: Datomic System"
        ""
        options-summary]
       (str/join \newline)))

(defn- restart-usage
  [options-summary]
  (->> ["restart:  Restart the analytics support on the access gateway"
        ""
        "Synopsis:"
        "  restart <system> [parameters]"
        ""
        "Required:"
        "  system: Datomic System"
        ""
        options-summary]
       (str/join \newline)))

(def ^:private gateway-opts
  [[nil "--wait" "Wait until the command against the access gateway has finished."]])

(def ^:private restart-opts
  [[nil "--ssho OPTION" "Pass ssh configuration options as if using ssh -o <option>"
    :assoc-fn (fn [m k v] (update m k conj v))
    :default ["IdentitiesOnly=yes"]
    :default-desc ""]])

(defn- parse-enable-opts
  [{:keys [args]}]
  (let [{:keys [arguments exit-message options] :as parsed}
        (common/parse-opts {:args args
                            :opts gateway-opts
                            :required-params 1
                            :usage-fn enable-usage})]
    (if exit-message
      parsed
      (merge options
             {:subcommand ::enable
              :system (first arguments)}))))

(defn- parse-disable-opts
  [{:keys [args]}]
  (let [{:keys [arguments exit-message options] :as parsed}
        (common/parse-opts {:args args
                            :opts gateway-opts
                            :required-params 1
                            :usage-fn disable-usage})]
    (if exit-message
      parsed
      (merge options
             {:subcommand ::disable
              :system (first arguments)}))))

(defn- parse-restart-opts
  [{:keys [args]}]
  (let [{:keys [arguments exit-message options] :as parsed}
        (common/parse-opts {:args args
                            :opts restart-opts
                            :required-params 1
                            :usage-fn restart-usage})]
    (if exit-message
      parsed
      (merge options
             {:subcommand ::restart
              :system (first arguments)}))))

(defmethod common/parse-command-parameters :gateway
  [{:keys [args base-options]}]
  (common/parse-subcommand-parameters
    {:args args
     :base-options base-options
     :subcommands {:disable parse-disable-opts
                   :enable parse-enable-opts
                   :restart parse-restart-opts}
     :usage-fn gateway-usage}))

(defmethod common/run-subcommand! ::disable
  [cfg]
  (gateway/disable cfg))

(defmethod common/run-subcommand! ::enable
  [cfg]
  (gateway/enable cfg))

(defmethod common/run-subcommand! ::restart
  [cfg]
  (gateway/restart cfg))
