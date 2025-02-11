;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;      http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS-IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns datomic.client.api.async
  "Async client API for Datomic. See also datomic.client.api
for the corresponding synchronous API.

Functions in this namespace that communicate with a separate process
take an arg-map with the following optional keys:

  :timeout      timeout in msec

Asynchronous functions return a core.async channel. In the case of an
error, the channel will get an error map with contents specified by
cognitect.anomalies. See https://github.com/cognitect-labs/anomalies.

Functions that return chunked results will return a succession of
vectors of values in a channel. The channel will be closed when the
results are exhausted.  If there is an error it will be in the channel
instead of the chunk. Chunked functions support the following optional
keys:

  :chunk     Optional. Maximum number of results that will be returned
             for each chunk. Defaults to 1000.
  :offset    Number of results to omit from the beginning
             of the returned data.
  :limit     Maximum total number of results to return.
             Specify -1 for no limit. Defaults to 1000.

Functions that return datoms return values of a type that supports
indexed (count/nth) access of [e a v t added] as well as
lookup (keyword) access via :e :a :v :t :added."
  (:import java.io.FileNotFoundException)
  (:refer-clojure :exclude [sync])
  (:require
   [clojure.core.async :refer (<!!) :as async]
   [datomic.client.api.impl :as impl]
   [datomic.client.impl.shared.protocols :as p]
   [datomic.query.support :as qs])
  (:import datomic.query.support.MapOnIndexed))

(defn- ares
  [ch]
  (let [result (<!! ch)]
    (if (:cognitect.anomalies/category result)
      (throw (ex-info (or (:cognitect.anomalies/message result)
                          (-> result :datomic.client/http-error-body :cause)
                          "Datomic Client Exception")
                      result))
      result)))

(defn client
  "Create a client for a Datomic system. This function does not
communicate with a server and returns immediately.

For a cloud system, arg-map requires:

  :server-type   - :cloud
  :region        - AWS region, e.g. \"us-east-1\"
  :system        - your system name
  :endpoint      - IP address of your system or query group

Optionally, a cloud system arg-map accepts:

  :creds-provider  - instance of com.amazonaws.auth.AWSCredentialsProvider. Defaults to DefaultAWSCredentialsProviderChain
  :creds-profile   - name of an AWS Named Profile. See http://docs.aws.amazon.com/cli/latest/userguide/cli-multiple-profiles.html
  :proxy-port      - local port for SSH tunnel to bastion 

  Note: :creds-provider and :creds-profile are mutually exclusive, providing both will result in an error.

For a dev-local system, arg-map comprises:

  :server-type   - :dev-local (required)
  :system        - a system name (required)
  :storage-dir   - optional, overrides :storage-dir in ~/.datomic/dev-local.edn

dev-local stores databases under ${storage-dir}/${system}/${db-name}.

For a peer-server system, arg-map requires:

  :server-type   - :peer-server
  :access-key    - access-key from peer server launch
  :secret        - secret from peer server launch
  :endpoint      - peer server host:port
  :validate-hostnames  - false

Returns a client object."
  [arg-map]
  (case (:server-type arg-map)
        :cloud (impl/dynacall 'com.datomic/client-impl-cloud
                              'datomic.client.impl.cloud/create-client
                              arg-map)
        :peer-server (impl/dynacall 'com.datomic/client-impl-pro
                                    'datomic.client.impl.pro/create-client
                                    arg-map)
        :dev-local (impl/dynacall 'com.datomic/dev-local-bundle
                                  'datomic.dev-local.impl/ensure-client
                                  arg-map)

        (throw (impl/incorrect ":server-type must be one of :cloud, :dev-local, :peer-server"))))

(defn administer-system
  "Run :action on system.

Currently the only supported action is:

:upgrade-schema        upgrade an existing database to use the latest base schema

:upgrade-schema takes the following map

:db-name               database name

Returns a diganostic value on success, throws on failure."
  [client arg-map]
  (p/administer-system client arg-map))

(defn list-databases
  "Lists all databases. arg-map requires no keys but can contain any of
the optional keys listed in the namespace doc.

Returns a channel of a collection of database names."
  [client arg-map]
  (p/list-databases client arg-map))

(defn connect
  "Connects to a database. Takes a client object and an arg-map with keys:

:db-name               database name

Returns a channel of a connection.
See namespace doc for error and timeout handling.

Returned connection supports ILookup for key-based access. Supported
keys are:

:db-name               database name"
  [client db-name]
  (p/connect client db-name))

(defn create-database
  "Creates a database specified by arg-map with key:

:db-name    The database name.

Returns a channel of true.
See namespace doc for error and timeout handling."
  [client arg-map]
  (p/create-database client arg-map))

(defn delete-database
  "Deletes a database specified by arg-map with keys:

 :db-name    The database name.

Returns a channel of true.
See namespace doc for error and timeout handling."
  [client arg-map]
  (p/delete-database client arg-map))

(defn db
  "Returns the current database value for a connection.

Supports ILookup interface for key-based access. Supported keys are:

:db-name               database name
:t                     basis t for the database
:as-of                 a point in time
:since                 a point in time
:history               true for history databases"
  [conn]
  (ares (p/recent-db conn)))

(defn sync
  "Used to coordinate with other clients.

Returns channel of a database value with basis :t >= t.
Does *not* make a remote call."
  [conn t]
  (p/sync conn t))

(defn- find-parent-conn
  [arg-map]
  (some p/-conn (:args arg-map)))

(defn q
  "Performs the query described by arg-map's :query and :args:

  :query  The query to perform: a map, list, or string (see below).
  :args   Data sources for the query, e.g. database values
          retrieved from a call to db, and/or rules.

  The query list form is [:find ?var1 ?var2 ...
                          :with ?var3 ...
                          :in $src1 $src2 ...
                          :where clause1 clause2 ...]

  :find  specifies the tuples to be returned
  :with  is optional, and names vars to be kept in the aggrgation set
         but not returned
  :in    is optional. Omitting ':in ...' is the same as specifying
         ':in $'
  :where limits the result returned

For a complete description of the query syntax, see
https://docs.datomic.com/cloud/query/query-data-reference.html.

Returns a channel of collections of tuples that closes when all results
are returned.

See namespace doc for chunk, offset/limit, timeout, and error handling."
  [arg-map]
  (if-let [conn (find-parent-conn arg-map)]
    (let [[nq as] (qs/parse-as (:query arg-map))]
      (if as
        (let [res (p/q conn (assoc arg-map :query nq))
              mapify (fn [chunk]
                       (if (:cognitect.anomalies/category chunk)
                         chunk
                         (mapv #(MapOnIndexed. as %) chunk)))]
          (async/pipe res (async/chan 1 (map mapify)))) 
        (p/q conn arg-map)))
    (doto (async/promise-chan) (async/put! {:cognitect.anomalies/category :cognitect.anomalies/incorrect
                                    :cognitect.anomalies/message "Query args must include a database"}))))


(defn qseq*
  "Doing the work of qseq that is common to sync/async"
  [arg-map]
  (if-let [conn (find-parent-conn arg-map)]
    (let [[nq as] (qs/parse-as (:query arg-map))]
      (if as
        (let [res (p/qseq conn (assoc arg-map :query nq))
              mapify (fn [chunk]
                       (if (:cognitect.anomalies/category chunk)
                         chunk
                         (update chunk :data (fn [data] (mapv #(MapOnIndexed. as %) data)))))]
          (async/pipe res (async/chan 1 (map mapify))))
        (p/qseq conn arg-map)))
    (doto (async/promise-chan) (async/put! {:cognitect.anomalies/category :cognitect.anomalies/incorrect
                                            :cognitect.anomalies/message "Query args must include a database"}))))

(defn qseq
  "Performs the query described by query and args (as per 'q'),
  returning a channel of collections of tuples that closes when all results
  are returned.  Item transformations such as
  'pull' are deferred until the seq is consumed. For queries with
  pull(s), this results in:
​
  * reduced memory use and the ability to execute larger queries
  * lower latency before the first results are returned"
  [arg-map]
  (let [res (qseq* arg-map)]
    (async/pipe res (async/chan 1 (map (fn [x]
                                         (if (:cognitect.anomalies/category x)
                                           x
                                           (:data x))))))))

(defn pull
  "Returns a hierarchical selection described by the keys in arg-map:

 :selector   the selector expression
 :eid        entity id

For a complete description of the selector syntax, see
https://docs.datomic.com/cloud/query/query-pull.html.

Returns a channel of a map."
  [db arg-map]
  (p/pull db arg-map))

(defn index-pull
  "Walks an index, pulling entities via :e if :avet or :v if :aevt,
using the selector.
​
    :index     :avet or :aevt
    :selector  a pull selector (see 'pull')
    :start     A vector in the same order as the index indicating
               the initial position. At least :a must be specified.
               Iteration is limited to datoms matching :a.
    :reverse   optional, when true iterate the index in reverse
               order
​
See namespace doc for timeout, offset/limit, and error handling.

Returns a channel of a map"
  [db arg-map]
  (p/index-pull db arg-map))

(defn tx-range
  "Retrieve transactions from the log as specified by arg-map:

 :start   Optional. The start time-point or nil to start from the
          beginning of the transaction log.
 :end     Optional. The end time-point, exclusive, or nil to
          run to the end of the transaction log.

Returns a channel of transactions that will close after all requested
transations are returned.

Transactions have keys:

 :t       the basis t of the transaction
 :data    a collection of the datom in the transaction

See datoms for a description of :data value.

See namespace doc for chunk, offset/limit, timeout, and error handling."
  [conn arg-map]
  (p/tx-range conn arg-map))

(defn transact
  "Submits a transaction specified by arg-map:

 :tx-data    a collection of list forms or map forms

For a complete specification of the tx-data format, see
https://docs.datomic.com/cloud/transactions/transaction-data-reference.html.

Returns a channel of a map with the following keys:

 :db-before  database value before the transaction
 :db-after   database value after the transaction
 :tx-data    collection of datoms produced by the transaction
 :tempids    a map from tempids to their resolved entity IDs.

See namespace doc for timeout and error handling."
  [conn arg-map]
  (p/transact conn arg-map))

(defn with-db
  "Returns a channel of a with-db value suitable for passing to 'with'."
  [conn]
  (p/with-db conn))

(defn as-of
  "Returns the value of the database as of some time-point.

See https://docs.datomic.com/cloud/time/filters.html."
  [db t]
  (p/as-of db t))

(defn datoms
  "Returns datoms from an index as specified by arg-map:

 :index       One of :eavt, :aevt, :avet, or :vaet.
 :components  Optional vector in the same order as the index
              containing one or more values to further narrow the
              result

Returns a channel of collections of datoms that will close after all
requested datoms are returned.

Datoms are associative and indexed:

Key     Index        Value
--------------------------
:e      0            entity id
:a      1            attribute id
:v      2            value
:tx     3            transaction id
:added  4            boolean add/retract

For a description of Datomic indexes, see
https://docs.datomic.com/cloud/query/raw-index-access.html.

See namespace doc for timeout, offset/limit, and error handling."
  [db arg-map]
  (p/datoms db arg-map))

(defn db-stats
  "Queries for database stats.
Returns a channel of a map including at least:

 :datoms      total count of datoms in the (history) database

See namespace doc for timeout and error handling."
  [db]
  (p/db-stats db))

(defn history
  "Returns a database value containing all assertions and
retractions across time. A history database can be passed to 'datoms',
'index-range', and 'q', but not to 'with' or 'pull'. Note
that queries against a history database will include retractions
as well as assertions. Retractions can be identified by the fifth
datom field ':added', which is true for asserts and false for
retracts.

See https://docs.datomic.com/cloud/time/filters.html."
  [db]
  (p/history db))

(defn index-range
  "Returns datoms from the AVET index as specified by arg-map:

 :attrid  An attribute entity identifier.
 :start   Optional. The start value, inclusive, of the requested
          range, defaulting to the beginning of the index.
 :end     Optional. The end value, exclusive, of the requested
          range, defaultin to the end of the index.

For a description of Datomic indexes, see
https://docs.datomic.com/cloud/query/raw-index-access.html.

Returns a channel of collections of datoms.

See namespace doc for chunk, offset/limit, timeout, and error handling."
  [db arg-map]
  (p/index-range db arg-map))

(defn since
  "Returns the value of the database since some time-point.

See https://docs.datomic.com/cloud/time/filters.html."
  [db t]
  (p/since db t))

(defn with
  "Applies tx-data to a database returned from 'with-db' or a
prior call to 'with'.  The result of calling 'with' is a
database value as-if the data was applied in a transaction, but
the durable database is unaffected.

Takes and returns data in the same format expected by transact.

See namespace doc for timeout and error handling."
  [db m]
  (p/with db m))





