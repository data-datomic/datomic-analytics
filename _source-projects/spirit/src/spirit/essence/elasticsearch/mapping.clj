(ns spirit.essence.elasticsearch.mapping)

(def to-datomic
  {:string  :string
   :byte    :long
   :short   :long
   :integer :long
   :long    :long
   :float   :double
   :double  :double
   :boolean :boolean
   :date    :instant})

(defn replace-index [{:keys [index] :as ele}]
  (let [pred {false "not_analyzed"
              true  "analyz1ed"
              nil   "no"
              :no  "no"}
        out (-> ele
                (get :index :na)
                pred)]
    (cond-> ele
      out (assoc :index out))))

(declare mapping)

(defn mapping-properties
  [{:keys [ident elements cardinality]}]
  (let [ptype (case cardinality
                :many :nested
                :object)]
    (reduce (fn [out {:keys [ident type search] :as ele}]
              (if (not= type :ref)
                (->> type
                     (assoc search :type)
                     (replace-index)
                     (assoc out ident))
                (->> {:type ptype
                      :properties (mapping-properties ele)}
                     (assoc out ident))))
            {}
            elements)))

(defn mapping [{:keys [ident cardinality] :as top}]
  (let [ptype (case cardinality
                :many :nested
                :object)]
    {ident {:type ptype
            :properties (mapping-properties top)}}))
