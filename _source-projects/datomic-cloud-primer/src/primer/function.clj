(ns primer.function
  (:require [clojure.pprint :as p]
            [datomic.client.api :as d]
            [primer.mbrainz :as mb]))

(defn custom-fn
  [s]
  (or (clojure.string/starts-with? s "は")
      (clojure.string/starts-with? s "荒")))

(comment
 ; Javaメソッドの呼び出し
  (d/q '[:find ?name
         :where
         [_ :artist/name ?name]
         [(.contains ^String ?name "ス")]]
       (d/db mb/conn))
  ; Clojure関数の呼び出し
  (d/q '[:find ?name
         :where
         [_ :artist/name ?name]
         [(clojure.string/starts-with? ?name "ザ")]]
       (d/db mb/conn))
  ; カスタム関数は、サーバ側には存在しないので実行不可能
  ;CompilerException clojure.lang.ExceptionInfo: Server Error 
  ;{:datomic.client-spi/request-id "26a0d891-fdc2-400d-ba57-08c402725f93", :cognitect.anomalies/category 
  ; :cognitect.anomalies/fault, :cognitect.anomalies/message "Server Error", 
  ; :dbs [{:database-id "73a18ece-869b-48af-a6af-5b778ea1417e", :t 28, :next-t 29, :history false}]}, compiling:(:24:5)
  #_(d/q '[:find ?name
           :where
           [_ :artist/name ?name]
           [(primer.function/custom-fn ?name)]]
         (d/db mb/conn))
  ; Whereは標準では論理積だが、論理和を指定することも可能
  (d/q '[:find ?name
         :where
         [_ :artist/name ?name]
         (or [(clojure.string/starts-with? ?name "は")]
             [(clojure.string/starts-with? ?name "荒")])]
       (d/db mb/conn)))

(defn ja-artists-not-released-in
  "find フィールドとnot-joinの変数は一致していないければならない"
  [year]
  (d/q '[:find ?name ?year
         :in $ ?year
         :where
         [?a :artist/name ?name]
         [?a :artist/country :country/JP]
         (not-join [?a ?year]
                   [?r :release/artists ?a]
                   [?r :release/year ?year])]
       (d/db mb/conn) year))

(comment
  ; はっぴいえんどは1971に”風街ろまん"をリリースしている
  (p/pprint (ja-artists-not-released-in 1971))
  ; 1972年のリリースはなし
  (p/pprint (ja-artists-not-released-in 1972)))
