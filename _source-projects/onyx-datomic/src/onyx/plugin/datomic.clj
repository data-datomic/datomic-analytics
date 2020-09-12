(ns onyx.plugin.datomic
  (:require [clojure.core.async :refer [chan >! >!! <!! poll! offer! close!
                                        thread timeout alts!! go-loop
                                        sliding-buffer]]
            [onyx.types :as t]
            [onyx.plugin.protocols :as p]
            [onyx.static.default-vals :refer [default-vals]]
            [clojure.core.async.impl.protocols :refer [closed?]]
            [onyx.static.uuid :refer [random-uuid]]
            [onyx.extensions :as extensions]
            [onyx.schema :as os]
            [onyx.datomic.api :as d]
            [schema.core :as s]
            [taoensso.timbre :refer [info debug fatal]])
  (:import [java.util.concurrent.locks LockSupport]
           [java.util.concurrent.atomic AtomicLong]))

;;; Helpers

(defn safe-connect [task-map]
  (d/safe-connect task-map))

(defn safe-as-of [task-map conn]
  (d/safe-as-of task-map conn))

(defn safe-datoms-per-segment [task-map]
  (or (:datomic/datoms-per-segment task-map)
      (throw (ex-info ":datomic/datoms-per-segment missing from write-datoms task-map." task-map))))

; ;;;;;;;;;;;;;
; ;;;;;;;;;;;;;
; ;; input plugins

(defn unroll-datom
  "Turns a datom into a vector of :eavt+op."
  [db datom]
  [(:e datom)
   (d/ident db (:a datom))
   (:v datom)
   (:tx datom)
   (:added datom)])

(defn datoms-sequence [db task-map]
  (case (:onyx/plugin task-map)
    ::read-datoms
    (let [datoms-components (or (:datomic/datoms-components task-map) [])
          datoms-index (:datomic/datoms-index task-map)]
      (apply d/datoms db datoms-index datoms-components))
    ::read-index-range
    (let [attribute (:datomic/index-attribute task-map)
          range-start (:datomic/index-range-start task-map)
          range-end (:datomic/index-range-end task-map)]
      (d/index-range db attribute range-start range-end))))

(defn close-read-datoms-resources
  [event lifecycle]
  {})

(defn inject-read-datoms-resources
  [{:keys [onyx.core/task-map onyx.core/log onyx.core/task-id onyx.core/job-id onyx.core/pipeline] :as event} lifecycle]
  (when-not (or (= 1 (:onyx/max-peers task-map))
                (= 1 (:onyx/n-peers task-map)))
    (throw (ex-info "Read datoms tasks must set :onyx/max-peers 1" task-map)))
  {})

(defrecord DatomicInput [db task-map datoms-per-segment datoms segment offset drained?]
  p/Plugin
  (start [this event]
    this)

  (stop [this event]
    this)

  p/Checkpointed
  (checkpoint [this]
    @offset)

  (recover! [this replica-version checkpoint]
    (vreset! drained? false)
    (vreset! offset (or checkpoint 0))
    (vreset! datoms (drop (or checkpoint 0) (datoms-sequence db task-map)))
    this)

  (checkpointed! [this ep]
    true)

  p/BarrierSynchronization
  (synced? [this ep]
    true)

  p/Input
  (completed? [this]
    @drained?)

  (poll! [this _ _]
    (let [read-datoms (mapv #(unroll-datom db %) (take datoms-per-segment @datoms))]
      (vswap! datoms #(drop datoms-per-segment %))
      (if (empty? read-datoms)
        (do (vreset! drained? true)
            nil)
        (do (vswap! offset #(+ % (count read-datoms)))
            {:datoms read-datoms})))))

(defn shared-input-builder [{:keys [onyx.core/task-map] :as event}]
  (let [batch-size (:onyx/batch-size task-map)
        datoms-per-segment (:datomic/datoms-per-segment task-map)
        conn (safe-connect task-map)
        db (safe-as-of task-map conn)]
    (assert datoms-per-segment)
    (->DatomicInput db task-map datoms-per-segment (volatile! nil) (volatile! nil)
                    (volatile! 0) (volatile! false))))

(defn read-datoms [pipeline-data]
  (shared-input-builder pipeline-data))

(defn read-index-range [pipeline-data]
  (shared-input-builder pipeline-data))

; ;;;;;;;;;;;;;
; ;;;;;;;;;;;;;
; ;; read log plugin

(defn unroll-log-datom
  "Turns a log datom into a vector of :eavt+op."
  [datom]
  ;; strip out transaction functions as they are not serializable in onyx
  (if-not (d/instance-of-datomic-function? (:v datom))
    [(:e datom)
     (:a datom)
     (:v datom)
     (:tx datom)
     (:added datom)]))

(defn close-read-log-resources
  [{:keys [] :as event} lifecycle]
  {})

(defn check-completed [task-map checkpointed]
  (when (and (not (:checkpoint/key task-map))
             (= :complete (:status checkpointed)))
    (throw (Exception. "Restarted task, however it was already completed for this job.
                       This is currently unhandled."))))

(defn log-entry->segment [entry]
  (update (into {} entry)
          (if (d/peer?) :data :tx-data)
          (fn [vs] (keep unroll-log-datom vs))))

(defn inject-read-log-resources
  [{:keys [onyx.core/task-map] :as event} lifecycle]
  {})

(defn tx-range [conn start-tx]
  (d/tx-range conn start-tx))

(defrecord DatomicLogInput
           [task-map task-id batch-size batch-timeout conn start-tx end-tx read-offset txes top-read-tx completed?]
  p/Plugin
  (start [this event]
    this)

  (stop [this event]
    this)

  p/Checkpointed
  (checkpoint [this]
    {:largest @top-read-tx
     :status :incomplete})

  (recover! [this replica-version checkpoint]
    (if (= :completed (:status checkpoint))
      (vreset! completed? true)
      (let [start-tx (or (:largest checkpoint)
                         (some-> task-map :datomic/log-start-tx dec)
                         ;; datomic databases are initialised with transactions up to 100
                         (dec 1000))]
        (vreset! txes (tx-range conn (inc start-tx)))
        (vreset! completed? false)
        (vreset! top-read-tx start-tx)))
    this)

  (checkpointed! [this epoch]
    true)

  p/BarrierSynchronization
  (synced? [this ep]
    true)

  (completed? [this]
    @completed?)

  p/Input
  (poll! [this _ _]
    (if-let [tx (first @txes)]
      (let [t (:t tx)]
        (if (and end-tx (> t end-tx))
          (do
            (vreset! completed? true)
            (vreset! txes nil)
            nil)
          (do
            (.set ^AtomicLong read-offset t)
            (vreset! top-read-tx t)
            (vswap! txes rest)
            (log-entry->segment tx))))
      (if (and end-tx (>= @top-read-tx end-tx))
        (do
          (vreset! completed? true)
          (vreset! txes nil)
          nil)
        (do
         ;; Poll for more messages
          (when-not @completed?
            (when (empty? (vreset! txes (tx-range conn (inc @top-read-tx))))
              (LockSupport/parkNanos (* batch-timeout 1000000))))
          nil)))))

(defn read-log [{:keys [onyx.core/task-map onyx.core/task-id onyx.core/monitoring] :as event}]
  (let [conn (safe-connect task-map)
        batch-size (:onyx/batch-size task-map)
        batch-timeout (or (:onyx/batch-timeout task-map) (:onyx/batch-timeout default-vals))
        start-tx (:datomic/log-start-tx task-map)
        end-tx (:datomic/log-end-tx task-map)
        read-offset (:read-offset monitoring)]
    (->DatomicLogInput task-map task-id batch-size batch-timeout conn start-tx end-tx
                       read-offset (volatile! nil) (volatile! nil) (volatile! false))))

(def read-log-calls
  {:lifecycle/before-task-start inject-read-log-resources
   :lifecycle/handle-exception (constantly :restart)
   :lifecycle/after-task-stop close-read-log-resources})

;;;;;;;;;;;;;
;;;;;;;;;;;;;
;; output plugins

(defn inject-write-tx-resources
  [{:keys [onyx.core/pipeline onyx.core/task-map]} lifecycle]
  {:datomic/conn (:conn pipeline)})

(defn inject-write-bulk-tx-resources
  [{:keys [onyx.core/pipeline]} lifecycle]
  {:datomic/conn (:conn pipeline)})

(defn inject-write-bulk-tx-async-resources
  [{:keys [onyx.core/pipeline]} lifecycle]
  {:datomic/conn (:conn pipeline)})

(defrecord DatomicWriteDatoms [conn partition]
  p/Plugin
  (start [this event]
    this)

  (stop [this event]
    this)

  p/BarrierSynchronization
  (synced? [this epoch]
    true)

  (completed? [this]
    true)

  p/Checkpointed
  (checkpoint [this])

  (recover! [this replica-version checkpoint]
    this)

  (checkpointed! [this epoch]
    true)

  p/Output
  (prepare-batch [this event replica _]
    true)

  (write-batch [this {:keys [onyx.core/write-batch]} replica _]
    (d/transact conn
                (map (fn [segment]
                       (if (and partition (not (sequential? segment)) (d/peer?))
                         (assoc segment :db/id (d/tempid partition))
                         segment))
                     write-batch))
    true))

(defn write-datoms [pipeline-data]
  (let [task-map (:onyx.core/task-map pipeline-data)
        conn (safe-connect task-map)
        partition (:datomic/partition task-map)]
    (->DatomicWriteDatoms conn partition)))

(defrecord DatomicWriteBulkDatoms [conn]
  p/Plugin
  (start [this event]
    this)

  (stop [this event]
    this)

  p/Checkpointed
  (checkpoint [this])

  (recover! [this replica-version checkpoint]
    this)

  (checkpointed! [this epoch]
    true)

  p/BarrierSynchronization
  (synced? [this epoch]
    true)

  (completed? [this]
    true)

  p/Output
  (prepare-batch [this event replica _]
    true)

  (write-batch [this {:keys [onyx.core/write-batch]} replica _]
    (run! (fn [segment]
            (d/transact conn (:tx segment)))
          write-batch)
    true))

(defn write-bulk-datoms [pipeline-data]
  (let [task-map (:onyx.core/task-map pipeline-data)
        conn (safe-connect task-map)]
    (->DatomicWriteBulkDatoms conn)))

(defrecord DatomicWriteBulkDatomsAsync [conn]
  p/Plugin
  (start [this event]
    this)

  (stop [this event]
    this)

  p/BarrierSynchronization
  (synced? [this epoch]
    true)
  (completed? [this]
    true)

  p/Checkpointed

  (checkpoint [this])
  (recover! [this replica-version checkpoint]
    this)
  (checkpointed! [this epoch]
    true)

  p/Output
  (prepare-batch [this event replica _]
    true)

  (write-batch [this {:keys [onyx.core/write-batch]} replica _]
    (->> write-batch
         (map (fn [tx] (d/transact-async conn (:tx tx))))
         (run! deref))
    true))

(defn write-bulk-datoms-async [pipeline-data]
  (let [task-map (:onyx.core/task-map pipeline-data)
        conn (safe-connect task-map)]
    (->DatomicWriteBulkDatomsAsync conn)))

(def read-datoms-calls
  {:lifecycle/before-task-start inject-read-datoms-resources
   :lifecycle/handle-exception (constantly :restart)
   :lifecycle/after-task-stop close-read-datoms-resources})

(def read-index-range-calls
  {:lifecycle/before-task-start inject-read-datoms-resources
   :lifecycle/handle-exception (constantly :restart)
   :lifecycle/after-task-stop close-read-datoms-resources})

(def write-tx-calls
  {:lifecycle/handle-exception (constantly :restart)
   :lifecycle/before-task-start inject-write-tx-resources})

(def write-bulk-tx-calls
  {:lifecycle/handle-exception (constantly :restart)
   :lifecycle/before-task-start inject-write-bulk-tx-resources})

(def write-bulk-tx-async-calls
  {:lifecycle/handle-exception (constantly :restart)
   :lifecycle/before-task-start inject-write-bulk-tx-async-resources})

;;;;;;;;;
;;; params lifecycles

(defn inject-db [{:keys [onyx.core/params] :as event} {:keys [datomic/basis-t datomic/uri onyx/param?] :as lifecycle}]
  (when-not uri
    (throw (ex-info "Missing :datomic/uri in inject-db-calls lifecycle." lifecycle)))
  (let [conn (safe-connect lifecycle)
        db (cond-> (d/db conn)
             basis-t (d/as-of basis-t))]
    {:datomic/conn conn
     :datomic/db db
     :onyx.core/params (if param?
                         (conj params db)
                         params)}))

(def inject-db-calls
  {:lifecycle/before-task-start inject-db})

(defn inject-conn [{:keys [onyx.core/params] :as event} {:keys [datomic/uri onyx/param?] :as lifecycle}]
  (let [conn (safe-connect lifecycle)]
    {:datomic/conn conn
     :onyx.core/params (if param?
                         (conj params conn)
                         params)}))

(def inject-conn-calls
  {:lifecycle/before-task-start inject-conn})
