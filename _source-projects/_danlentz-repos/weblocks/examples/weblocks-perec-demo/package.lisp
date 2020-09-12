;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(defpackage :weblocks-perec-demo
  (:use :weblocks :weblocks-perec
    :hu.dwim.common
    :hu.dwim.computed-class
    :hu.dwim.def
    :hu.dwim.defclass-star
    :hu.dwim.logger
    :hu.dwim.perec
    :hu.dwim.rdbms
    :hu.dwim.stefil
    :hu.dwim.util
    :local-time
    :hu.dwim.meta-model
    :hu.dwim.model
    :cl-ppcre
    :metacopy-with-contextl)

  (:import-from :weblocks-perec
    #:? #:?* #+swank #:^ #+swank #:^* #:apo)
  
  (:shadow #:name 
    #:parent)
  
  (:shadowing-import-from :weblocks
    #:begin-transaction
    #:commit-transaction
    #:rollback-transaction)

  (:shadowing-import-from :hu.dwim.perec
    #:text #:defun/cc
    #:time
    #:form
    #:set))


