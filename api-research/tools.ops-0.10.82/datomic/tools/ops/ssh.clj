;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

(ns datomic.tools.ops.ssh
  (:require
    [clojure.spec.alpha :as s]
    [cognitect.aws.client.api :as aws]
    [datomic.tools.ops.aws :as ops-aws]
    [datomic.tools.ops.ssh.specs :as ssh.specs]
    [datomic.tools.ops.specs :as specs]
    [datomic.tools.ops.process :as process]))

(set! *warn-on-reflection* true)

(defn- retrieve-key
  "Retrieves key from S3 bucket, writes to output-file, sets it executable"
  [s3-client bucket key output-file]
  (let [result (ops-aws/invoke! s3-client {:op :GetObject
                                           :request {:Bucket bucket
                                                     :Key key}})]
    (spit output-file (slurp (:Body result)))
    (process/sh! "chmod" "0600" output-file)))

(defn retrieve-keys
  [s3-client bucket system region]
  "Retrieves private key and host key from S3"
  (let [home-dir (System/getProperty "user.home")
        pk (format "%s/.ssh/datomic-%s-%s-bastion" home-dir (or region "default") system)
        hk (format "%s/.ssh/datomic-%s-%s-bastion.hostkey" home-dir (or region "default") system)]
    (retrieve-key s3-client
                  bucket
                  (format "%s/datomic/access/private-keys/bastion" system)
                  pk)
    (retrieve-key s3-client
                  bucket
                  (format "%s/datomic/access/private-keys/bastion.hostkey" system)
                  hk)
    {:host-key hk
     :private-key pk}))

(defn- build-ssh-command
  "Assembles all the options into an vector of ssh commands"
  [{:keys [access-type host-key ip private-key port ssho]}]
  (let [ssh-options (interleave (repeat "-o") ssho)
        cli-params (cond
                     (= access-type :analytics)
                     ["-CNT" "-L" (str port ":localhost:8989")]
                     (= access-type :client)
                     ["-CND" (str port)]
                     :default
                     (throw (RuntimeException. (str "Invalid access type: " access-type))))]
    (into [] (flatten ["ssh" "-v" "-o" (str "UserKnownHostsFile=" host-key) ssh-options "-i" private-key cli-params (str "ec2-user@" ip)]))))

(s/fdef access
        :args (s/cat :args ::ssh.specs/access-args))
(defn access
  "Opens a SOCKS proxy to the access gateway in the system for Datomic client access
  Blocks until process exits.

  Required args:
  :system       Datomic system
  :access-type  :client or :analytics

  Optional args:
  :port         Local forwarded port
  :profile      Named profile from AWS credentials file
  :region       AWS Region of the Datomic system
  :ssho         Parameters passed to SSH as -o"
  [{:keys [region system] :as args}]
  (specs/conform! ::ssh.specs/access-args args)
  (let [client-map (ops-aws/client-args args)
        tagging-client (aws/client (merge {:api :resourcegroupstaggingapi} client-map))
        bucket (ops-aws/get-bucket tagging-client system)
        s3-client (aws/client (merge {:api :s3} client-map))
        keys (retrieve-keys s3-client bucket system region)
        ec2-client (aws/client (merge {:api :ec2} client-map))
        ip (ops-aws/get-ip ec2-client (str system "-bastion"))
        cmd (build-ssh-command (merge args
                                      keys
                                      {:ip ip}))]
    (-> ^Process (process/embed cmd)
        .waitFor)))

