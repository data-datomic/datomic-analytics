;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :weblocks-perec-demo)


(defstore *perec-store* :perec
  :database-class-name    'postgresql/perec
  :transaction-class-name 'transaction
  :spec '(:database "perec" :user-name "perec" :password "perec"))



