;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(defpackage #:weblocks-perec-asd
  (:use :cl :asdf))

(in-package :weblocks-perec-asd)

(defsystem weblocks-perec
    :name "weblocks-perec-demo"
    :version "0.1"
    :author "Dan Lentz"
    :licence "Public Domain"
    :description "weblocks-perec"
    :depends-on (:hu.dwim.perec.all)
    :components ((:file "store")))
