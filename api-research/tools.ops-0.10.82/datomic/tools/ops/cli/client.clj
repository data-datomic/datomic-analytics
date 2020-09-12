;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

(ns datomic.tools.ops.cli.client
  (:require
    [clojure.edn :as edn]
    [clojure.string :as str]
    [clojure.tools.cli :as cli]
    [datomic.tools.ops.cli.common :as common]
    [datomic.tools.ops.ssh :as ssh]))

(set! *warn-on-reflection* true)

(def ^:private client-usage
  (->> ["client - Client commands"
        ""
        "Available subcommands:"
        "  access:  Opens a SOCKS proxy to the access gateway in the system for Datomic client access"]
       (str/join \newline)))

(defn- access-usage
  [options-summary]
  (->> ["access - Opens a SOCKS proxy to the access gateway in the system for Datomic client access"
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

(def ^:private access-opts
  [[nil "--port PORT" "Local port to use for forwarding"
    :default 8182
    :parse-fn edn/read-string
    :validate-fn int?]
   [nil "--ssho OPTION" "Pass ssh configuration options as if using ssh -o <option>"
    :assoc-fn (fn [m k v] (update m k conj v))
    :default ["IdentitiesOnly=yes"]
    :default-desc ""]])

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

(defmethod common/parse-command-parameters :client
  [{:keys [args base-options]}]
  (common/parse-subcommand-parameters
    {:args args
     :base-options base-options
     :subcommands {:access parse-access-opts}
     :usage-fn client-usage}))

(defmethod common/run-subcommand! ::access
  [cfg]
  (ssh/access (assoc cfg :access-type :client)))