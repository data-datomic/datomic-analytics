;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :weblocks-perec-demo)

(def persistent-class address ()
  ((street
     :initform ""
     :accessor address-street
     :initarg :street
     :type string)
    (city
      :initform ""
      :accessor address-city
      :initarg :city
      :type string)
    (state
      :initform ""
      :accessor address-state
      :type string 
      :initarg :state)))


(defview address-table-view (:type table :inherit-from '(:scaffold address)))

(defview address-data-view  (:type data :inherit-from '(:scaffold address)))

(defview address-form-view  (:type form :inherit-from '(:scaffold address)))
;  (state :present-as us-state :parse-as us-state))


