;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

(ns datomic.tools.ops
  (:require
    [clojure.string :as str]
    [clojure.tools.cli :as cli]
    [datomic.tools.ops.cli.common :as common]

    ;; parse-command-parameters implementations
    datomic.tools.ops.cli.analytics
    datomic.tools.ops.cli.client
    datomic.tools.ops.cli.cloud
    datomic.tools.ops.cli.gateway
    datomic.tools.ops.cli.log
    datomic.tools.ops.cli.solo
    datomic.tools.ops.cli.system
    datomic.tools.ops.solo))

(set! *warn-on-reflection* true)

(defn- usage
  [options-summary]
  (->> [common/basic-usage-string
        ""
        "Command line tools for managing Datomic"
        ""
        "For more help:"
        ""
        "  datomic help"
        "  datomic <command> help"
        "  datomic <command> <subcommand> help"
        ""
        "Options:"
        options-summary
        ""
        "Available Commands:"
        "  analytics"
        "  client"
        "  cloud"
        "  gateway"
        "  log"
        "  solo"
        "  system"]
       (str/join \newline)))

(defn parse-command-line
  "Parse the command line arguments"
  [args]
  (let [{:keys [options errors arguments summary]}
        (cli/parse-opts args common/command-opts :in-order true)
        command (first arguments)]
    (cond
      (or (= "help" command)
          (:help options))
      {:exit-message (usage summary) :ok? true}

      (seq errors)
      {:exit-message (common/error-msg errors)}

      (= (count arguments) 0)
      {:exit-message (common/error-msg ["too few arguments"
                                        (usage summary)])}

      :default
      (common/parse-command-parameters {:args (rest arguments)
                                        :base-options options
                                        :command command
                                        :options-summary summary}))))

(defmethod common/parse-command-parameters :default
  [{:keys [command summary]}]
  {:exit-message (common/error-msg [(str "unrecognized command: " command)
                                    (usage summary)])})

(defn -main
  [& args]
  (let [{:keys [ok? exit-message] :as cfg} (parse-command-line args)]
    (if exit-message
      (do
        (println exit-message)
        (System/exit (if ok? 0 1)))
      (do (common/run-subcommand! cfg)
          (System/exit 0)))))
