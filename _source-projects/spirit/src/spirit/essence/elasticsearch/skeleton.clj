(ns spirit.essence.elasticsearch.skeleton)

(defn skeleton-fields [fields {:keys [exclude include]}]
  (let [pred (cond include
                   (fn [d] (-> d :ident include))

                   exclude
                   (fn [d] (-> d :ident exclude not))

                   :else
                   (constantly true))]
    (reduce (fn [out field]
              (if (pred field)
                (conj out field)
                out))
            []
            fields)))

(declare skeleton)

(defn skeleton-refs [fields ns current [embed essence]]
  (reduce (fn [out {:keys [ident] :as field}]
            (if-let [prop (ident current)]
              (let [[opts] (cond (= :all prop) [{}]
                                 (vector? prop) prop)
                    [attr] (-> essence ns ident)
                    nns     (-> attr :ref :ns)]
                (conj out (skeleton essence [nns [opts]] embed)))
              out))
          []
          fields))

(defn skeleton
  ([essence [curr [{:keys [refer] :as opts}]] embed]
   (let [[refs fields] (reduce-kv (fn [[refs others] k [attr]]
                                    (let [attr (assoc attr :ident k)]
                                      (if (-> attr :type (= :ref))
                                        [(conj refs attr) others]
                                        [refs (conj others attr)])))
                                  [[] []]
                                  (curr essence))
         elements (->> (skeleton-refs refs curr (curr embed) [embed essence])
                       (concat (skeleton-fields fields opts))
                       (mapv (fn [{:keys [ident] :as ele}]
                               (if-let [nident (if refer (refer ident))]
                                 (assoc ele :ident nident)
                                 ele))))]
     (-> {:ident curr :type :ref}
         (assoc :elements elements)))))
