(ns pseudoace.model2schema
  (:require
   [clojure.string :as str]
   [com.gfredericks.forty-two :as f2]
   [datomic.api :as d :refer [entity entity-db q tempid]]
   [pseudoace.model :refer [model-node]]
   [pseudoace.utils :as utils]))

(def ^:dynamic *schema-notes* false)

(defn datomize-name
  "Make `n` into a Datomic-friendly name by converting to lower case
  and replacing underscores with hyphens."
  [^String n]
  (if (Character/isDigit (first n))
    (when *schema-notes*
      (println "WARNING: name starts with a digit: " n)
      (println "         Assuming literal English translation, "
               "e.g 2 -> two")))
  (let [dn (-> (str/lower-case n)
               (str/replace #"^\d+" (comp f2/words read-string))
               (str/replace #"[?#]" "")
               (str/replace #"(_|\s)" "-"))]
    (if (= dn "id")  ; id is reserved for the object name.
      "xid"
      dn)))

(declare node->schema)
(declare attr->model)

(defn simple-tag? [node]
  (and (= (:type node) :tag)
       (empty? (:children node))))

(defn- flatten-children [node]
  (loop [children (:children node)
         path     []]
    (case (count children)
      0 path
      1 (recur (:children (first children)) (conj path (first children)))
      (utils/throw-exc
       "Cannot flatten multiple chidren at "
       children))))

(def modeltype-to-datomic
  {:text     :db.type/string
   :int      :db.type/long
   :float    :db.type/double
   :date     :db.type/instant
   :ref      :db.type/ref})

(defn- tuple-member-names
  "Take a sequence of model nodes representing attributes within a component, and
   return a list of suitable names."
  [items attr]
  (let [basenames (map (comp datomize-name (some-fn :alt-name :name)) items)
        dups (->> (frequencies basenames)
                  (filter (fn [[k count]]
                            (> count 1)))
                  (map first)
                  (set))]
    (when (seq dups)
      (when *schema-notes*
        (println "WARNING: using synthetic names for" attr basenames)))
    (map-indexed
     (fn [index bn]
       (if (dups bn)
         (str bn "-" (char (+ (int \a) index)))
         bn))
     basenames)))

;; TODO (dasmoth):
;; This should probably also be doing UNIQUE-ness testing (and
;; returning it as metadata?)

(defn- enum-keys
  "Create a set of enum keys in namespace `enum-ns` from `nodes`.

  Walks a seq of tags (`nodes`); including any tag children,
  and synthesize entities which can be used as enum keys."
  ([enum-ns nodes]
   (enum-keys enum-ns "" [] nodes))
  ([enum-ns datomic-prefix ace-prefix nodes]
   (into (array-map)
         (mapcat
          (fn [{:keys [type name alt-name children]}]
            (if (= type :tag)
              (let [lname (str datomic-prefix (or alt-name (datomize-name name)))
                    path  (conj ace-prefix name)]
                (cons
                 [(utils/vmap
                   :db/id     (tempid :db.part/user)
                   :db/ident  (keyword enum-ns lname)
                   :pace/tags (str/join " " path))
                  (let [non-tag-children (remove #(= (:type %) :tag) children)]
                    (case (count non-tag-children)
                      0    nil
                      1    (cons
                            (first non-tag-children)
                            (flatten-children non-tag-children))
                      (utils/throw-exc
                       "Multiple non-tag children at "
                       name)))]
                 (enum-keys enum-ns (str lname ":") path children)))))
          nodes))))

(defn- prefix?
  "Test whether `prefix` is a prefix of `whole`."
  [whole prefix]
  (and (<= (count prefix) (count whole))
       (= (vec (take (count prefix) whole)) (vec prefix))))

(defn tag->schema
  "Build schema entities for `node`.

  `sid`: a tempid for the class identifier attribute.
  `mns`: a string representing the namespace in which this tag is located.
  `tagpath`: a sequence of strings representing
             the tag-path through the AceDB model."
  [sid mns tagpath tpm node]
  (let [attribute (keyword mns (or (:alt-name node)
                                   (datomize-name (last tagpath))))
        fchild (first (:children node))]
    (cond
     ;; "plain tag" case
     (empty? (:children node))
     [(utils/vmap
       :db/id               (tempid :db.part/db)
       :db/ident            attribute
       :db/valueType        :db.type/boolean
       :db/cardinality      :db.cardinality/one
       :db.install/_attribute :db.part/db
       :pace/tags           (str/join " " tagpath))]

     ;; "simple enum" case -- the only ones we auto-detect.
     ;; Could this be merged with the other enum case?
     (every? simple-tag? (:children node))
     (let [vns (str (namespace attribute) "." (name attribute))]
       (conj
        (keys (enum-keys vns (:children node)))

        (utils/vmap
         :db/id              (tempid :db.part/db)
         :db/ident           attribute
         :db/valueType       :db.type/ref
         :db/cardinality     (if (:unique? node)
                               :db.cardinality/one
                               :db.cardinality/many)
         :db.install/_attribute :db.part/db
         :pace/tags       (str/join " " tagpath))))

     (or (and (= (count (:children node)) 1)
              (#{:int :float :text :ref :date :hash} (:type fchild)))
         (:enum node))      ;; "Simple" enums have already been caught at this point.

     (if (and (empty? (:children fchild))
              (not= (:type fchild) :hash)
              (not (:enum node))
              ;; Becomes complex if there's a hash at the other end of the XREF.
              (not (if-let [x (:xref fchild)]
                     (:use-ns (tpm [(datomize-name (:name fchild)) x])))))
       ;; "simple datum" case
       (when-not (:suppress-xref fchild)
         (let [cname       (:name fchild)
               type        (modeltype-to-datomic (:type fchild))
               schema [(utils/vmap
                        :db/id           (tempid :db.part/db)
                        :db/ident        attribute
                        :db/valueType    type
                        :db/cardinality  (if (:unique? node)
                                           :db.cardinality/one
                                           :db.cardinality/many)
                        :db.install/_attribute :db.part/db
                        :pace/tags       (str/join " " tagpath)
                        :pace/obj-ref    (if (= type :db.type/ref)
                                           {:db/id (tempid :db.part/db)
                                            :pace/identifies-class (.substring
                                                                    cname
                                                                    1)})
                        :db/index        (if (= type :db.type/string)
                                           (.startsWith cname "?"))
                        :pace/fill-default (or (:fill-default fchild) nil))]]
           (if-let [x (:xref fchild)]
             (let [{:keys [tags mode]} (tpm [(datomize-name cname) x])]
               (conj schema
                     {:db/id          (tempid :db.part/db)
                      :pace/identifies-class (.substring cname 1)
                      :pace/xref  (utils/vmap
                                   :db/id                (tempid :db.part/user)
                                   :pace.xref/tags       (or tags x)
                                   :pace.xref/view       true
                                   :pace.xref/import     (= mode "INXREF")
                                   :pace.xref/export     (= mode "INXREF")
                                   :pace.xref/attribute  {:db/id (tempid :db.part/db)
                                                          :db/ident attribute}
                                   :pace.xref/obj-ref    sid)}))
             schema)))

       ;; "compound datum" case
       (let [cns        (str (namespace attribute) "." (name attribute))
             enum       (:enum node)
             enum-keys* (if enum
                          (enum-keys
                           (str cns "." (if (string? enum) enum "value"))
                           (:children node)))
             fc         (take-while (complement :suppress-xref)
                         (if enum-keys*
                           (let [longest (->>
                                          (vals enum-keys*)
                                          (sort-by count)
                                          (reverse)
                                          (first))]
                             (doseq [v (vals enum-keys*)]
                               (if-not (prefix? longest v)
                                 (utils/throw-exc
                                  "Bad enum:"
                                  v
                                  " is not a prefix of "
                                  longest)))
                             longest)
                           (flatten-children node)))
             hashes     (filter #(= (:type %) :hash) fc)
             concretes  (filter #(not= (:type %) :hash) fc)]
         (cond
          (seq fc)
          (concat
            [(utils/vmap
                    :db/id           (tempid :db.part/db)
                    :db/ident        attribute
                    :db/valueType    :db.type/ref
                    :db/cardinality  (if (or (and (not enum)
                                                  (empty? concretes))
                                             (and (:unique? node)
                                                  (every?
                                                   :unique? (butlast
                                                             concretes))))
                                       :db.cardinality/one
                                       :db.cardinality/many)
                    :db/isComponent  true
                    :db.install/_attribute :db.part/db
                    :pace/use-ns     (doall
                                      (for [h hashes]
                                       (datomize-name (:name h))))
                    :pace/tags       (str/join " " tagpath))]

            (if enum
              (conj
               (keys enum-keys*)
               {:db/id           (tempid :db.part/db)
                :db/ident        (keyword cns (if (string? enum) enum "value"))
                :db/valueType    :db.type/ref
                :db/cardinality  :db.cardinality/one
                :db.install/_attribute :db.part/db
                :pace/tags       ""
                :pace/order      0}))

            (mapcat
             (fn [i c mname]
               (let [type     (modeltype-to-datomic (:type c))
                     cname    (:name c)
                     cattr    (keyword cns mname)
                     schema [(utils/vmap
                              :db/id           (tempid :db.part/db)
                              :db/ident        cattr
                              :db/valueType    type
                              :db/cardinality  :db.cardinality/one
                              :db.install/_attribute :db.part/db
                              :pace/tags       ""
                              :pace/order      i
                              :db/index      (if (= type :db.type/string)
                                               (.startsWith (:name c) "?"))
                              :pace/fill-default (or (:fill-default c) nil)
                              :pace/obj-ref    (if (= type :db.type/ref)
                                                 {:db/id (tempid :db.part/db)
                                                  :pace/identifies-class
                                                  (.substring cname 1)}))]]
                 (if-let [x (:xref c)]
                   (let [{:keys [tags use-ns mode]} (tpm [(datomize-name cname) x])]
                     (conj
                      schema
                      {:db/id (tempid :db.part/db)
                       :pace/identifies-class (.substring cname 1)
                       :pace/xref (utils/vmap
                                   :db/id                (tempid :db.part/user)
                                   :pace.xref/tags       (or tags x)
                                   :pace.xref/attribute  {:db/id (tempid :db.part/db)
                                                          :db/ident cattr}
                                   :pace.xref/obj-ref    sid
                                   :pace.xref/view       true
                                   :pace.xref/import     (= mode "INXREF")
                                   :pace.xref/export     (= mode "INXREF")
                                   :pace.xref/use-ns     use-ns)}))
                   schema)))
             ;; In enum case, order 0 is reserved for the enum.
             (iterate inc (if enum 1 0))
             concretes
             (tuple-member-names concretes attribute)))

          ;; potentially-complex enum without augmentation
          enum
          (let [vns (str (namespace attribute) "." (name attribute))]
            (conj
             (keys (enum-keys vns (:children node)))
             (utils/vmap
              :db/id              (tempid :db.part/db)
              :db/ident           attribute
              :db/valueType       :db.type/ref
              :db/cardinality     (if (:unique? node)
                                    :db.cardinality/one
                                    :db.cardinality/many)
              :db.install/_attribute :db.part/db
              :pace/tags (str/join " " tagpath)))))

         ))

   (and (> (count (:children node)) 2)
        (= (count (:children fchild)) 1)
        (every? #(= (:children fchild)
                    (:children %))
                (rest (:children node))))
   (do
     (when *schema-notes*
       (println "Candidate augmented enum at " mns ":" tagpath))
     (mapcat (partial node->schema sid mns tagpath tpm) (:children node)))

   :default
   (mapcat (partial node->schema sid mns tagpath tpm) (:children node)))))

(defn- node->schema [sid mns tagpath tpm node]
  (if (= (:type node) :tag)
    (tag->schema sid mns (conj tagpath (:name node)) tpm node)))

(defn model->schema
 ([model]
  (model->schema {} model))
 ([tpm {:keys [name alt-name] :as model}]
  (let [mns (or alt-name (datomize-name name))
        is-hash? (.startsWith name "#")
        pid (if (not is-hash?)
              (tempid :db.part/db))
        sid (tempid :db.part/db)]
    (utils/conj-if
     (mapcat (partial node->schema sid mns [] tpm) (:children model))
     (if pid
       {:db/id       pid
        :db/ident    (keyword "wb.part" mns)
        :pace/_prefer-part sid
        :db.install/_partition :db.part/db})
     (utils/vmap
      :db/id          sid
      :db/ident       (keyword mns "id")
      :db/valueType   :db.type/string
      :db/unique      :db.unique/identity
      :db/cardinality :db.cardinality/one
      :db.install/_attribute :db.part/db
      :pace/identifies-class (.substring name 1)
      :pace/is-hash is-hash?)))))

(defn xref-info-map
  "Build a map of inbound XREFs for `model`."
  ([model]
   (reduce (partial xref-info-map (datomize-name (:name model)) [])
           {}
           (:children model)))
  ([class tagpath tpm node]
   (cond
     (= (:type node) :tag)
     (reduce (partial xref-info-map class (conj tagpath (:name node)))
             tpm (:children node))

     (:xref node)
     (assoc tpm [class (last tagpath)]
            {:tags (str/join " " tagpath)
             :mode (:suppress-xref node)
             :use-ns (if (= (:suppress-xref node) "INXREF")
                       (when-let [[h :as children] (:children node)]
                         (cond
                           (> (count children) 1)
                           (println "WARNING: INXREF has multiple children at "
                                    (pr-str node))

                           (seq (:children h))
                           (println
                            "WARNING: INXREF can only"
                            "be followed by a single node at "
                            (pr-str node))

                           (not= (:type h) :hash)
                           (println
                            "WARNING: INXREF can only be followed"
                            "by a hash at "
                            (pr-str node))

                           :default
                           #{(datomize-name (:name h))})))})

     :default
     tpm)))

(defn models->schema
  "Convert a set of models to a schema, resolving tag-paths of XREFs."
  [models]
  (mapcat (partial
           model->schema
           (reduce merge (map xref-info-map models)))
          models))

;;
;; Model reconstruction from a schema
;;
(defn conj-in-tagpath [root tagpath nodes]
  (if (empty? tagpath)
    (if (seq nodes)
      (assoc
       (if (:unique? (meta nodes))
         (assoc root :unique? true)
         root)
       :children (if-let [c (:children root)]
                   (into c nodes)
                   (vec nodes)))
      root) ; Special case to allow easy insertion of tags.
    (let [children (vec (:children root))
          index    (utils/find-index
                    #(= (:name %) (first tagpath))
                    children)
          model-node (model-node :tag (first tagpath) false false nil nil)]
      (assoc root :children
             (if (nil? index)
               (conj
                children
                (conj-in-tagpath model-node (rest tagpath) nodes))
               (assoc
                children
                index
                (conj-in-tagpath
                 (nth children index)
                 (rest tagpath)
                 nodes)))))))

(defn pace-items-for-ns [db ns]
    (map
    #(entity db (first %))
    (q '[:find ?t
         :in $ ?ns
         :where [?t :db/ident ?ti]
         [(namespace ?ti) ?tins]
         [(= ?tins ?ns)]
         [?t :pace/tags _]]
       db ns)))

(defn attr->model* [ti]
  (case (:db/valueType ti)
    :db.type/long
    [(model-node :int "Int" false false nil nil)]

    :db.type/float
    [(model-node :float "Float" false false nil nil)]

    :db.type/double
    [(model-node :float "Float" false false nil nil)]

    :db.type/instant
    [(model-node :datetype "DateType" false false nil nil)]

    :db.type/string
    [(model-node :text (if (:db/index ti)
                         "?Text"
                         "Text")
                 false false nil nil)]

    :db.type/boolean
    nil        ; All the nodes we need will be created
               ; by conj-in-tagpath

    :db.type/ref
    (cond
     (:db/isComponent ti)
      (let [db     (entity-db ti)
            concs  (pace-items-for-ns
                    db
                    (str (namespace (:db/ident ti)) "." (name (:db/ident ti))))
            hashes (for [ns (:pace/use-ns ti)]
                     (entity db (keyword ns "id")))]
        [(reduce
          (fn [next conc]
            (assoc (first (attr->model conc))   ;; Could this ever return multiples?
              :children (if next [next])))

          (reduce
           (fn [next hash]
             (model-node
              :hash
              (str "#" (:pace/identifies-class hash))
              false
              false
              nil
              (if next [next])))
           nil
           hashes)
          concs)])

     (:pace/obj-ref ti)
     [(model-node
       :ref
       (str "?" (:pace/identifies-class
                 (entity (entity-db ti) (:pace/obj-ref ti))))
       false
       false
       nil
       nil)]

     :default
     (if-let [enums (seq
                     (pace-items-for-ns
                      (entity-db ti)
                      (str (namespace (:db/ident ti))
                           "."
                           (name (:db/ident ti)))))]
       (for [e enums]
         (model-node :tag
                     (:pace/tags e)
                     false
                     false
                     nil
                     nil))
       (model-node :ref
                   "?Funny"
                   false
                   false
                   nil
                   nil)))))

(defn attr->model [ti]
  (if-let [m (attr->model* ti)]
    (with-meta
      m
      {:unique? (= (:db/cardinality ti)
                   :db.cardinality/one)})))

(defn schema->model [db ident]
  (let [root  (entity db ident)
        props (pace-items-for-ns db (namespace (:db/ident root)))]
    (reduce
     (fn [model ti]
       (conj-in-tagpath model
                        (str/split (:pace/tags ti) #" ")
                        (attr->model ti)))
     (model-node :ref
                 ;; what about hashes?
                 (str "?" (:pace/identifies-class root)) 
                 false
                 false
                 nil
                 nil)
     props)))

(defn flatten-model* [prefix model]
  (let [np        (conj prefix model)
        children  (seq (:children model))]
    (if children
      (mapcat (partial flatten-model* np) children)
      [np])))

(defn flatten-model [model]
  (flatten-model* [] model))

(defn print-model [model]
  (loop [[line & lines] (flatten-model model)
         last-line      []
         old-tab-stops  [0]]
    (when line
      (recur lines
             line
             (loop [[node & nodes]     line
                    [llnode & llnodes] last-line
                    [tab & tabs]       old-tab-stops
                    cur                -2
                    our-tabs           []]
               (if node
                 (if (not= node llnode)
                   (let [node-ns (if (:unique? node)
                                   (str (:name node) " UNIQUE")
                                   (:name node))
                         pos     (max (or tab 0) (+ cur 2))]
                     (print
                      (str/join (repeat (- pos (max cur 0)) \space)))
                     (print node-ns)
                     (recur nodes
                            nil ; Once we've hit a unique node, stop
                                ; trying to dedup.
                            nil
                            (+ pos (count node-ns))
                            (conj our-tabs pos)))
                   (recur nodes llnodes tabs cur (conj our-tabs tab)))
                 (do
                   (println)
                   our-tabs)))))))  ; pass the final tab-stops on to the next line
