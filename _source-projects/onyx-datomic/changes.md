#### 0.14.5.1
* Updated `onyx.datomic.api` NS discovery to pick correct Datomic driver against modern datomic-pro releases

#### 0.10.0.1
* Fix bug in read-log recovery, where start tx would jump forward by 1 on recovery.

#### 0.10.0.0
* `:checkpoint/force-reset?` and `:checkpoint/key` are both deprecated in favour of 0.10.0's [resume-points](http://www.onyxplatform.org/docs/user-guide/0.10.0-beta4/#resume-point) feature.

#### 0.9.7.0
* Defensively reduce chances of deadlock.

#### 0.9.0
* Switch written key in event map from onyx.core/written? to datomic/written?

#### 0.8.10
* Fix shutdown issue that could cause input tasks to not stop cleanly

#### 0.8.6.1
* IMPORTANT FIX: producer threads now feedback exceptions to read-batch rather than just getting stuck

#### 0.8.3.1
* written tx data is now included in the event map under :datomic/written

#### 0.7.2.0
* Add a read-log input plugin to read the datomic log.

#### 0.7.0.2
* commit-tx :datomic/partition is now optional.
* commit-tx will now allow transaction data in vector as well as map form
* improved validation of task-map data

#### 0.7.0.1
* New index-range catalog type
* read-datoms can now supply components to datoms via :datomic/datoms-components
