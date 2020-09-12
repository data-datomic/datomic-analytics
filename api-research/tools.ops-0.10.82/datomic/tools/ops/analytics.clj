;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

(ns datomic.tools.ops.analytics
  (:require
    [clojure.spec.alpha :as s]
    [cognitect.aws.client.api :as aws]
    [datomic.tools.ops.analytics.specs :as analytics.specs]
    [datomic.tools.ops.aws :as ops-aws]
    [datomic.tools.ops.specs :as specs]
    [datomic.tools.ops.process :as process]))

(set! *warn-on-reflection* true)

(s/fdef s3-sync
        :args (s/cat :args ::analytics.specs/s3-sync-args))
(defn s3-sync
  "Sync analytics config files from local directory to S3

  Required args:
  :system     Datomic system
  :directory  local directory to sync

  Optional args:
  :profile      Named profile from AWS credentials file
  :query-group  Datomic Query Group to target for sync
  :region       AWS Region of the Datomic system"
  [{:keys [system query-group directory] :as args}]
  (specs/conform! ::analytics.specs/s3-sync-args args)
  (let [client-map (ops-aws/client-args args)
        tagging-client (aws/client (merge {:api :resourcegroupstaggingapi} client-map))
        bucket (ops-aws/get-bucket tagging-client system)
        s3-key (format "s3://%s/%s/datomic/analytics/%s/etc"
                       bucket
                       system
                       (or query-group system))
        cmd (conj (process/aws-profile-region args)
                  "s3"
                  "sync"
                  directory
                  s3-key
                  "--delete"
                  "--exclude"
                  "datomic/views/*")]
    (-> ^Process (process/embed cmd)
        .waitFor)))