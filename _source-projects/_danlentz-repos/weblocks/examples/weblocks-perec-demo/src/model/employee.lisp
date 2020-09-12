;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :weblocks-perec-demo)


(def persistent-class employee (person)
  ((company
     :accessor employee-company
     :initarg :company
     :type company)
    (contract
      :accessor employee-contract
      :initarg :contract
      :type symbol)))



(defview employee-table-view (:type table :inherit-from 'person-table-view)
  (company :reader (compose #'company-name #'employee-company)
    :order-by '(company name)))


(defview employee-data-view (:type data :inherit-from 'person-data-view)
  (company :reader (compose #'company-name #'employee-company))
  contract)


(defview employee-form-view (:type form :inherit-from 'person-form-view)
  (company :present-as (dropdown :choices #'all-companies
				 :label-key #'company-name)
	   :parse-as (object-id :class-name 'company)
	   :reader (compose #'object-id #'employee-company)
	   :requiredp t)
  (contract :present-as (radio :choices '(:full-time :part-time :consultant :intern))
	    :parse-as keyword))



