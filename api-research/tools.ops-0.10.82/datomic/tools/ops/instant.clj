;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

(ns datomic.tools.ops.instant
  (:import
    [java.util Date]))

(set! *warn-on-reflection* true)

(def ^:private ^ThreadLocal thread-local-utc-date-format
  ;; SimpleDateFormat is not thread-safe, so we use a ThreadLocal proxy for access.
  ;; http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4228335
  (proxy [ThreadLocal] []
    (initialValue []
      (doto (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss.SSS")
        ;; RFC3339 says to use -00:00 when the timezone is unknown (+00:00 implies a known GMT)
        (.setTimeZone (java.util.TimeZone/getTimeZone "GMT"))))))

(defn ts->datetime
  "timestamp (msec) to a datetime."
  [^long msec]
  (let [^java.text.DateFormat date-format (.get thread-local-utc-date-format)]
    (.format date-format (Date. msec))))