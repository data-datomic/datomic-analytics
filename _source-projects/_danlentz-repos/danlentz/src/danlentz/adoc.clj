(ns danlentz.adoc
  (:require 
            [clojure.java.io :as io]
            [clj-uuid.api :as uuid]
            [environ.core :refer [env]])
  (:use     [clj-tuples])
  (:import  java.util.UUID
            org.asciidoctor.Asciidoctor
            org.asciidoctor.DocumentHeader
            org.asciidoctor.DirectoryWalker))

