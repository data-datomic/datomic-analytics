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

(ns datomic.query.support
  (:require [clojure.edn :as edn]))

(defn- incorrect!
  [msg]
  (throw (ex-info msg {:cognitect.anomalies/category :cognitect.anomalies/incorrect
                       :cognitect.anomalies/message msg})))

(defn listq->mapq
  "turns [:find ?a, ?b :in $src1 $src2 :where [$src1 ?a :likes ?b] ($src2 drummer ?b)] into
  {:find (?a ?b) :in ($src1 $src2) :where ([$src1 ?a :likes ?b] ($src2 drummer ?b))}"
  [lq]
  (->> lq
       (partition-by #{:find :keys :strs :syms :with :in :where :timeout})
       (partition 2)
       (reduce (fn [m [k v]]
                 (let [k (first k)]
                   (assoc m k v)))
               {})))

(defn disallow-find-variants!
  "Throws an anomay if query has a :find clause not supported in client"
  [query]
  (when (some #(or (vector? %) (= '. %)) (:find query))
    (incorrect! "Only find-rel elements are allowed in client :find")))

(defn query-map
  "Take a query map, string, or sequence and returns a normalized query map."
  [q]
  (let [q (if (string? q) (edn/read-string q) q)]
    (if (sequential? q)
      (listq->mapq q)
      q)))

(defn parse-as
  "takes a query (in any form, e.g. str/vec/map) and returns [q as]
  the returned query will be in map form, and 'as' will be nil or a vector of keys"
  [q]
  (let [{:keys [find keys strs syms] :as nq} (query-map q)]
    (if-let [asyms (or keys strs syms)]
      (let [as (cond->> asyms
                        keys (map keyword)
                        strs (map str)
                        :then vec)]
        (when-not (= (count (or keys strs syms)) (count find))
          (throw (incorrect! "Count of :keys/:strs/:syms must match count of :find")))
        [(dissoc nq :keys :syms :strs) as])
      [nq nil])))

(defn counted-seq
  "Creates a seq that is equal and equiv to base-seq, but whose head
implements Counted by returning ct."
  ([base-seq ct]
     (counted-seq base-seq ct nil))
  ([^java.util.List base-seq ct meta]
     (if (< ct 1)
       nil
       (proxy [clojure.lang.ASeq clojure.lang.Counted] [meta]
         ;; ASeq impl
         (first [] (first base-seq))
         (next [] (next base-seq))
         (withMeta [meta] (counted-seq base-seq ct meta))

         ;; Counted
         (count [] ct)

         ;; ASeq overrides
         (equiv [o] (= base-seq o))
         (equals [o] (.equals base-seq o))
         (hashCode [] (.hashCode base-seq))
         (hasheq [] (hash base-seq))
         (seq [] base-seq)
         (cons [o] (cons o base-seq))
         (more [] (rest base-seq))
         (containsAll [c] (.containsAll base-seq c))
         (toArray ([] (.toArray base-seq))
                  ([o] (.toArray base-seq o)))
         (contains [o] (.contains base-seq o))
         (iterator [] (.iterator base-seq))
         (subList [from to] (.subList base-seq from to))
         (indexOf [o] (.indexOf base-seq o))
         (lastIndexOf [o] (.lastIndexOf base-seq o))
         (listIterator ([] (.listIterator base-seq))
                       ([index] (.listIterator base-seq index)))
         (get [index] (nth base-seq index))))))
