---

# Datomic Cloud Primer

- clj-nakano #5 2018/3/14
- 株式会社シグニファイア代表 中村研二 (github: k2n, twitter: @k2nakamura)

---

## 必要なもの

- AWSアカウント
- [Clojure CLI](https://clojure.org/guides/deps_and_cli)
- AWS CLI
- GitHubアカウント

---

## Datomic Cloudを起動する

- AWS MarketplaceからCloudFormationに必要なパラメタを指定して起動
- テストが終わったらEC2インスタンスを止めるのを忘れずに！
  - [秒単位課金](https://aws.amazon.com/jp/about-aws/whats-new/2017/10/announcing-amazon-ec2-per-second-billing/)で無駄がなくなってます
- Datomicのライセンスとt2.nano, t2.smallのEC2インスタンスで月額約$30

---

## Music Brainzサンプルデータをアップロード

- [Music Brainz](https://musicbrainz.org/)はオープンな音楽の百科事典
- [mbrainz-sample](https://github.com/Datomic/mbrainz-sample)はDatomic On-prem向けのサンプルデータ
  - 手順はDatomic Cloudに適用できない
  - 今のところ、Datomic Cloud向けimporter/exporterはバンドルされていない
-  [mbrainz-importer](https://github.com/Datomic/mbrainz-importer)
- `config/manifest.edn.sample`を`config/manifest.edn`にコピーして編集

```
{:client-cfg {:region "us-west-2"
              :server-type :cloud
              :system "signifier-dev"
              :query-group "signifier-dev"
              :endpoint "http://entry.signifier-dev.us-west-2.datomic.net:8182"
              :proxy-host "localhost"
              :proxy-port 8182}
 :db-name "mbrainz-subset"
 :basedir "subsets"
 :concurrency 3}
```

---

## Music Brainzサンプルデータをアップロード

```
[kenji@k2n-mbp13: ] clojure -m datomic.mbrainz.importer config/manifest.edn
Loading batch file for  :schema
Batches already completed:  0
.{:process {:forms 1}, :result {:txes 1, :datoms 304}}
...
Loading batch file for  :releases
Batches already completed:  9
..{:process {:forms 2}, :result {:txes 2, :datoms 103856}}
"Elapsed time: 5708.891189 msecs"
Loading batch file for  :releases-artists
Batches already completed:  11
..{:process {:forms 2}, :result {:txes 2, :datoms 11810}}
"Elapsed time: 2640.703704 msecs"
Loading batch file for  :media
Batches already completed:  13
...........{:process {:forms 11}, :result {:txes 11, :datoms 874556}}
"Elapsed time: 46358.909202 msecs"
"Elapsed time: 68590.385983 msecs"
```
- 約109万件のデータが登録されている

--- 

## Datomic Cloudに開発環境から接続する

- 環境変数を設定する

```
export DATOMIC_AWS_REGION=us-west-2 # MarketPlaceで選択したAWSリージョン
export DATOMIC_SYSTEM=my-system # Marketplaceで指定したシステム名
export DATOMIC_QUERY_GROUP=my-system #将来クエリグループが提供された場合に使用。今はシステム名
export DATOMIC_PROXY_PORT=8182 #Socks proxyのローカルポート番号
```

- 本リポジトリをクローン

```
$ git clone https://github.com/clj-nakano/datomic-cloud-primer.git
```


- REPLの起動

````
$ cd datomic-cloud-primer
$ clj
````

---

## デモ


---

## Datomic Cloudではできないこと

- カスタム関数の実行。JavaランタイムとClojure coreの関数のみ利用可能
- 変換関数、カスタムトランザクション関数もサポートされていない
- 複数DBを横断するクエリ

