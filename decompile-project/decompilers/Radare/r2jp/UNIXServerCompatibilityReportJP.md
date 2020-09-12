## radare2 for servers support in 2020 (r2jp community)

[English version](https://github.com/radareorg/r2jp/blob/master/UNIXServerCompatibilityReportEN.md)

今まで互換性テスト結果を加え、2020年に対応すると対応しない`radare2` for UNIXサーババーションを纏めたいと思います。

以前2019年12月(＠SECCON 2019)にpancakeさんと本内容の打ち合わせを行い、今年(2020年)はどの対応体制のコミットが出来るかと相談しました。

結論は以下の内容となります。 (R2JP の内部向けの内容となります)

---
**今現在対応しているバーションと対応していないバーションの結論**

＊）`unsupported`は2020年からは対応をしていない、`supported`は対応予定、`core` = r2パッケージ、プラグイン無し
```
FreeBSD 9 :  unsupported, see: https://twitter.com/malwaremustd1e/status/1211930394628509697
FreeBSD 10 : unsupported, see: https://twitter.com/malwaremustd1e/status/1211930394628509697
FreeBSD 11 : supported, core : OK , r2ghidra: NG, r2dec NG 
FreeBSD 12 : supported, core : OK , r2ghidra: NG, r2dec NG 
Debian Wheezy: unsupported, see: https://twitter.com/malwaremustd1e/status/1215954152137740288
Debian Jessie: supported, core : OK , r2ghidra: NG, r2dec OK 
Debian Stretch: supported, core : OK , r2ghidra: NG, r2dec OK 
Tsurugi Linux SECCON edition: supported, core : OK , r2ghidra: OK, r2dec OK
Debian Buster: supported, core: OK, r2ghidra: PENDING, r2dec PENDING 
Tsurugi Linux 2020年3月アップデート: (TBA、テスト中)
NetBSD と OpenBSD (TBA、テスト中)
```
---
**テストの詳細内容↓ (v4.3.0 ～ v4.4.0 base/core)**

- 上記のテスト用にサポートされているバージョンはすべてx64ビットCPU用で、 `x32`ビットCPUのサーバーは現在非コミットの状態です。
- `r2ghidra`はdebianのコンパイルに失敗し、 `isnan`、 `isinf` <  `ambiguous`のCPPライブラリのエラーが出ています。
- CPPライブラリ互換性のBSDプラットフォームで`r2ghidra`プラグインが失敗しました。
- BSDテストスキーム全体で、`r2dec`には「duktape」のビルドに関する問題が発生しています。
- FreeBSD`x32`は現在サポートされていません。

  報告しましたの内容を[ご参照してください]((https：//twitter.com/malwaremustd1e/status/1211930394628509697))。
- Debian Stretch `x32`のテストはもうコミットされていませんが、最新の結果は下記のとおりです↓

  Debian Stretch x86、 `core` : OK , `r2ghidra`: NG, `r2dec` OK。
- OpenBSDは未だテスト中です。情報を改めて報告します。
- NetBSDの最新バージョンも後でお知らせします。

---
**最新安定しているバージョンの情報（Linux/BSDサーバについて）**

(1)下記のバーション以降は`x64`ビットのみの対応となりますが、`x32`サーバの対応が不可能となります。
```
~/radare2-STABLE]$ r2 -v
radare2 4.2.0-git 23842 @ freebsd-x86-64 git.4.1.1-128-g3c788a496
commit: 3c788a49674d2e1c0e8ddbe6cce52ab4a9bcf818 build: 2020-01-16__14:06:39
```
＊）上記のバーションの`pdc`、`r2dec`と`r2ghidra`プラグインのは**「現在対応しているバーション」**のセクションに書いた内容のとおりです。

本STABLEバーションの[パッケージはこちらになります](https://github.com/radareorg/radare2/releases/tag/4.2.0)

(2) FreeBSD/OpenBSDの`x32`ビットについて

FreeBSD/OpenBSDの`x32`ビットサーバは下記の`radare2`のバーションをインストールしてください。
```
radare2 4.0.0-git 23215 @ linux-x86-32 git.3.9.0-20-gd112883
commit: d112883c5ad7309ca577373639281bb8e57d9988 build: 2019-09-20__21:47:04

radare2 4.0.0-git 23226 @ freebsd-x86-32 git.3.9.0-20-gd112883
commit: d112883c5ad7309ca577373639281bb8e57d9988 build: 2019-09-20__21:55:04

radare2 4.0.0-git 23449 @ linux-x86-64 git.3.9.0-20-gd112883
commit: d112883c5ad7309ca577373639281bb8e57d9988 build: 2019-09-20__21:38:58

radare2 4.0.0-git 22926 @ freebsd-x86-64 git.3.9.0-20-gd112883c5
commit: d112883c5ad7309ca577373639281bb8e57d9988 build: 2019-09-20__12:53:49
```
↑なお、本バーションのデコンパイラープラグインは`pdc`と`r2dec`しか対応をしておりません。

本バーションの[パッケージはこちらになります](https://github.com/radareorg/radare2/releases/tag/4.0.0)

---
**安定している組み込みOSの情報**

Tsurugi [SECCON 2019 Special Edition](https://blog.0day.jp/p/20191218.html)
```
radare2 4.1.0-git 24455 @ linux-x86-64 git.4.0.0-235-g982be50
commit: 982be504999364c966d339c4c29f20da80128e14 build: 2019-12-17__10:29:05
```
- (core : OK , `r2ghidra`: OK, `r2dec` OK, 実績: SECCON2019、HACKLU2019、MMD-0065-2020とMMD-0066-2020の解析)
- ISOでVMのインストールが可能なのでサーバにもそのVMの導入方法も出来ます。
- 他のTsurugi Linuxバーションについて、テストの結果を確認次第に情報を追加します。

---
**今までの`radare2`のテスト/バグフィックス/試験のステータスについて**

- [現在確認中 ⇒ `radare2_4.5.0-git`](https://github.com/radareorg/radare2/releases/tag/continuous)
- 4.4.0 (性能試験の結果が良くない(重い)、4.5.0のリリースでバグフィックス)
- 4.3.0 (ビルドのバグがあり、4.5.0のリリースでバグフィックス)

＊）参考として[r2リリース情報の一覧](https://github.com/radareorg/radare2/releases)

---
**OSディストリビューションのEOL情報**

2020年に`radare2` UNIXの対応は様々なOSディストリビューションの下記の情報を参考にして対応の計画を作りました。

- [FreeBSD supported versons](https://www.freebsd.org/releases/)
- [Linux Debian supported versions:](https://wiki.debian.org/LTS/Extended)
- [OpenBSD supported versions](https://marc.info/?l=openbsd-announce) | [or Web site](https://www.openbsd.org/)
- [NetBSD supported versions](https://www.netbsd.org/releases/formal.html)
- [Tsurugi Linux check versions](https://tsurugi-linux.org/documentation_tsurugi_linux_changelog.php#)

＊）上記の内容を定期的に更新を行いますので変更される可能性もあります。

---
unixfreaxjp@tsurugiseccon:~$ date
Sat May 30 06:04:32 JST 2020
r2jp
