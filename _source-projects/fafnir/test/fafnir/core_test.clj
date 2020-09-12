(ns fafnir.core-test
  (:require [clojure.test :refer :all]
            [datomic.api :refer [db q] :as d]
            [fafnir.core :refer :all]))

(defn run-plan [plan]
  (plan {}))

(def test-schema
  {:key/name #{:string :one}
   :key/age #{:long :one :indexed}
   :key/parent #{:ref :one}})

(defn make-db []
  (let [uri (str "datomic:mem://" (gensym "db_"))
        _ (d/create-database uri)
        conn (d/connect uri)]
    (-> (gen-plan
         [_ (assert-schema test-schema)]
         nil)
        (get-plan conn)
        commit)
    conn))

(deftest schema-test
  (is (let [conn (make-db)]
        (q '[:find ?e
             :where
             [?e :key/age ?v]]
           (db conn)))))

(deftest assert-entity-tests
  (testing "can insert entities with ids"
    (is (= ((assert-entity {:db/id 42 :foo/bar 11}) {})
           [42 {:valid-ids {42 42}
                :tempids {nil 42}
                :new-ents {{:foo/bar 11
                            :db/id 42} 42}}]))))

(deftest gen-plan-tests
  (testing "basic gen-plan usage"
    (is (= (-> (gen-plan
                [id (assert-entity {:db/id 42 :foo/bar 11})]
                id)
               run-plan)
           [42 {:valid-ids {42 42}
                :tempids {nil 42}
                :new-ents {{:foo/bar 11
                            :db/id 42} 42}}])))

  (testing "multiple asserts in gen-plan"
    (is (= (-> (gen-plan
                [id (assert-entity {:db/id 42 :foo/bar 11})
                 cid (assert-entity {:db/id 43 :parent/id id})]
                cid)
               run-plan)
           [43 {:valid-ids {42 42
                            43 43}
                :tempids {nil 43}
                :new-ents {{:foo/bar 11
                            :db/id 42} 42
                            {:parent/id 42
                             :db/id 43} 43}}])))

  (testing "nested gen-plans"
    (is (= (-> (gen-plan
                [id (assert-entity {:db/id 42 :foo/bar 11})
                 cid (gen-plan
                      [cid (assert-entity {:db/id 43 :parent/id id})]
                      cid)]
                cid)
               run-plan)
           [43 {:valid-ids {42 42
                            43 43}
                :tempids {nil 43}
                :new-ents {{:foo/bar 11
                            :db/id 42} 42
                            {:parent/id 42
                             :db/id 43} 43}}]))))

(deftest update-entity-tests
  (testing "basic update-entity usage"
    (is (= (-> (gen-plan
                [id (assert-entity {:db/id 42 :foo/bar 11})
                 cid (update-entity id :update/attr 44
                                       :update/attr2 43)]
                cid)
               run-plan)
           [42 {:valid-ids {42 42}
                :updates [[42 :update/attr 44]
                          [42 :update/attr2 43]]
                :tempids {nil 42}
                :new-ents {{:foo/bar 11
                            :db/id 42} 42}}]))))

(deftest commit-and-query
  (testing "commit works"
    (let [conn (make-db)]
      (is (= (do (-> (gen-plan
                      [id (assert-entity {:key/name "John" :key/age 42})]
                      id)
                     (get-plan conn)
                     commit)
                 (-> (q '[:find ?name ?age
                          :where
                          [?e :key/name ?name]
                          [?e :key/age ?age]]
                        (db conn))
                     first))
             ["John" 42]))))

  (testing "slightly more complex insert"
    (let [conn (make-db)]
      (is (= (do (-> (gen-plan
                      [id (assert-entity {:key/name "John" :key/age 42})
                       pid (assert-entity {:key/name "Bill" :key/age 72})
                       _ (update-entity id :key/parent pid)]
                      id)
                     (get-plan conn)
                     commit)
                 (-> (q '[:find ?child ?parent
                          :where
                          [?e :key/name ?child]
                          [?e :key/parent ?pid]
                          [?pid :key/name ?parent]]
                        (db conn))
                     first))
             ["John" "Bill"])))))


(def singleton-helper
  (gen-plan
   [id (singleton {:key/name "John"
                   :key/age 42})]
   id))

(defn count-johns [conn]
  (count (q '[:find ?e
              :where
              [?e :key/name "John"]]
            (db conn))))

(deftest singletons
  (testing "can assert singletons"
    (let [conn (make-db)]
      (is (= (do (-> singleton-helper
                     (get-plan conn)
                     commit)
                 (count-johns conn))
             1))))
  (testing "can assert singletons are unified for a given transaction"
    (let [conn (make-db)]
      (is (= (do (-> (gen-plan
                      [id1 singleton-helper
                       id2 singleton-helper]
                      (do (assert (= id1 id2))
                          nil))
                     (get-plan conn)
                     commit)
                 (count-johns conn))
             1))))
  (testing "can assert singletons are unified for a given transaction"
    (let [conn (make-db)]
      (is (= (do (-> (gen-plan
                      [id1 singleton-helper]
                      id1)
                     (get-plan conn)
                     commit)
                 (-> (gen-plan
                      [id1 singleton-helper]
                      id1)
                     (get-plan conn)
                     commit)
                 (count-johns conn))
             1)))))

(deftest assoc-in-tests
  (testing "assoc, get, update-in"
    (is (= (-> (gen-plan
                [_ (assoc-in-plan [:foo :bar] 42)
                 x (get-in-plan [:foo :bar])
                 _ (do (assert (= x 42))
                       no-op)
                 _ (update-in-plan [:foo :bar] + 10)
                 x (get-in-plan [:foo :bar])
                 _ (do (assert (= x 52))
                       no-op)]
                x)
               run-plan)
           [52 {:user {:foo {:bar 52}}}]))))


(deftest query-tests
  (testing "create tests"
    (let [conn (make-db)]
      (-> (gen-plan
           [_ (assert-entity {:key/name "John"
                              :key/age 42})]
           nil)
          (get-plan conn)
          commit)
      (-> (gen-plan
           [results (query '[:find ?age
                             :in $ ?name
                             :where
                             [?e :key/name ?name]
                             [?e :key/age ?age]]
                           "John")]
           (is (= results #{[42]})))
          (get-plan conn)))))
