;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :weblocks-perec-demo)



(def persistent-class company ()
  ((name :initform ""
      :accessor company-name
      :initarg :name
      :type  string)
    (industry
      :initform ""
      :accessor company-industry
      :initarg :industry
      :type string)
    (non-profit
      :initform nil
      :accessor company-non-profit-p
      :initarg :non-profit-p
      :type boolean)))

;; (ensure-exported* 'company)


(defun all-companies (&optional arg)
  "Accepts an argument (by dropdown) and returns available companies."
  (declare (ignore arg))
  (find-persistent-objects *perec-store* 'company
    :order-by (cons 'name :asc)))



(defview company-table-view (:type table  :inherit-from '(:scaffold company)))

(defview company-form-view (:type form :inherit-from '(:scaffold company)))



