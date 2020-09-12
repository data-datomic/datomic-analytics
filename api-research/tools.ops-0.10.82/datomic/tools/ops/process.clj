;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

(ns datomic.tools.ops.process
  (:require
    [clojure.core.async :refer (>!! <!!) :as a]
    [clojure.java.io :as io]
    [clojure.java.shell :as sh])
  (:import
    [java.io BufferedReader]))

(set! *warn-on-reflection* true)

(defn- stdout-printer
  [stdout-ch]
  (a/thread
    (loop []
      (when-let [v (<!! stdout-ch)]
        (do (println v) (recur))))))

(defn- stderr-printer
  [stderr-ch]
  (a/thread
    (binding [*out* *err*]
      (loop []
        (when-let [v (<!! stderr-ch)]
          (do (println v) (recur)))))))

(defn reader->ch
  "Blocking loop copying reader to channel. Does not close channel."
  [^BufferedReader reader ch]
  (loop []
    (when-let [line (.readLine reader)]
      (>!! ch line)
      (recur))))

(defn areader->ch
  "Launches a thread around reader->ch. Closes reader. Does not close channel."
  [^BufferedReader reader ch]
  (a/thread
    (with-open [rdr reader]
      (reader->ch rdr ch))))

(defn embed
  "Runs cmd, a seqable of Java strings, with streams piped line-at-a-time
to *out* and *err*. Returns a Java Process."
  [cmd]
  (let [pb (ProcessBuilder. ^"[Ljava.lang.String;" (into-array String cmd))
        proc (.start pb)
        stdout-ch (a/chan 1000)
        stderr-ch (a/chan 1000)]
    (stdout-printer stdout-ch)
    (areader->ch (io/reader (.getInputStream proc)) stdout-ch)
    (stderr-printer stderr-ch)
    (areader->ch (io/reader (.getErrorStream proc)) stderr-ch)
    proc))

(defn sh!
  "clojure.java.shell/sh, throw on error"
  [& args]
  (let [{:keys [exit] :as result} (apply sh/sh args)]
    (if (zero? exit)
      result
      (throw (ex-info "Shell command failed" {:args args :result result})))))

(defn aws-profile-region
  "Helper for building aws cli calls with optionsl --profile profile --region region params"
  [{:keys [profile region]}]
  (cond-> ["aws"]
          profile
          (conj
            "--profile" profile)
          region
          (conj
            "--region" region)))