;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :weblocks-perec-demo)


(def persistent-class person ()
  ((first-name
      :accessor person-first-name
      :initarg :first-name
      :type string)
    (last-name
      :accessor person-last-name
      :initarg :last-name
      :type string)
    (age
      :initform nil
      :accessor person-age
      :initarg :age
      :type (or null integer))
    (address
      :initform (with-transaction  (make-instance 'address))
      :accessor person-address
      :initarg :address)))

;; (ensure-exported* 'person)



(defview person-table-view (:type table :inherit-from '(:scaffold person))
  (address :type mixin :view 'address-table-view))


(defview person-data-view (:type data :inherit-from '(:scaffold person))
  (address :type mixin :view 'address-data-view))


(defview person-form-view (:type form :inherit-from '(:scaffold person))
  (address :type mixin :view 'address-form-view))

