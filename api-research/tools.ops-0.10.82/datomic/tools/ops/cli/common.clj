;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

(ns datomic.tools.ops.cli.common
  (:require
    [clojure.string :as str]
    [clojure.tools.cli :as cli]))

(set! *warn-on-reflection* true)

(def command-opts
  [["-h" "--help" "This help message. Specify `<command> help` for help about a command"]
   ["-r" "--region REGION" "AWS Region of the Datomic system"]
   ["-p" "--profile PROFILE" "Named profile from AWS credentials file"]])

(def basic-usage-string
  "usage: datomic [options] <command> <subcommand> [parameters]")

(defn error-msg
  [errors]
  (str "datomic error: "
       (str/join \newline errors)))

(defn parse-opts
  "Helper fn that parses args, handles help and parse error cases

  Required params
    args:      are the CLI args
    opts:      option-specs for tools.cli
    usage-fn:  fn that gets called with cli summary

  Optional params
    required-params:  Number of required params

  Returns
    on success, result of calling cli/parse-opts
    on failure, map with :exit-message and optionally ok?
  "
  [{:keys [args opts required-params usage-fn]}]
  (let [{:keys [arguments errors summary] :as results}
        (cli/parse-opts args
                        (concat command-opts opts))]
    (cond
      (= "help" (first arguments))
      (merge results
             {:exit-message (usage-fn summary) :ok? true})

      (seq errors)
      (merge results
             {:exit-message (error-msg errors)})

      (and
        required-params
        (< (count arguments) required-params))
      {:exit-message (error-msg ["Missing required parameters"
                                 (usage-fn summary)])}

      :default
      results)))

(defn parse-subcommand-parameters
  "Parses command line for subcommands

  Required parameters
    args:         Command line args
    base-options  options from parse-command-line
    usage-fn      function to call for help/errors
    subcommands   map of :subcommand to parse fn"
  [{:keys [args base-options usage-fn subcommands]}]
  (let [subcommand (keyword (first args))
        parse-subcommand-fn (get subcommands subcommand)]
    (cond
      (= :help subcommand)
      {:exit-message usage-fn :ok? true}

      (= (count args) 0)
      {:exit-message (error-msg ["Missing subcommand"
                                 usage-fn])}

      parse-subcommand-fn
      (merge base-options
             (parse-subcommand-fn {:args (rest args)}))

      :default
      {:exit-message (error-msg [(str "Unknown command - " (first args))
                                 usage-fn])})))

(defmulti parse-command-parameters
  (fn [{:keys [command]}]
    (keyword command)))

(defmulti run-subcommand! :subcommand)