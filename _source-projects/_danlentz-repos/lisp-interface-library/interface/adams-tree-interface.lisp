;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-


(in-package :interface)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adams Tree Interface 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-interface <adams-tree> (<emptyable-box>) ()
  (:singleton))

(defmethod empty ((<i> <adams-tree>))
  nil)

(defmethod empty-p ((<i> <adams-tree>) (thing null))
  t)

(defmethod empty-p ((<i> <adams-tree>) (thing box!))
  (null (box-value thing)))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shared Significant Values 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar |%unbound%| '|%unbound%|)

(defun unbound ()
  |%unbound%|)

(defun leaf (&optional (<i> <adams-tree>))
  (empty <i>))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <RB-TREE> Interface 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-interface <rb-tree> (<adams-tree> <heighted-binary-tree>) ()
  (:abstract))

(defclass rb-tree-node (heighted-binary-tree-node) ())

(defmethod node-class ((<i> <rb-tree>))
  'rb-tree-node)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <RB-TREE> Interface 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-interface <weighted-binary-tree> (<binary-tree>) ()
  (:abstract))

(defclass weighted-binary-tree-node (binary-tree-node)
  ((size
    :initarg :size
    :initform 0
    :type integer
    :reader node-size)))

(define-interface <wb-tree> (<adams-tree> <weighted-binary-tree>) ()
  (:abstract))

(defclass wb-tree-node (weighted-binary-tree-node) ())

(defmethod node-class ((<i> <wb-tree>))
  'wb-tree-node)
