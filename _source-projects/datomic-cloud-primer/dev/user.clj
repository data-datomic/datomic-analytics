(ns user
  (:require [clojure.tools.nrepl.server :refer [start-server stop-server]]))

(def port (+ (rand-int 64510) 1024))
(defonce server (start-server :port port))
(spit ".nrepl-port" port)
