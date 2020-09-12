;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-


(in-package :pure)


(import '(interface::node/create interface::node/find interface::node/join
           interface::node/cons-enum interface::node/k interface::node/v interface::node/l
           interface::node/r interface::node/h interface::node/kv interface::node/lr
           interface::node/kvlr interface::node/kvlrh interface::kv interface::lr
           interface::kvlr interface::kvlrh interface::node/concat3 interface::node
           interface::leaf interface::unbound interface::node/singleton))

;;; pure RB-TREE

(define-interface <rb-tree> (interface::<rb-tree> <binary-tree> <order-parameter>) ()
  (:parametric (&optional (order interface::<universal-order>))
    (make-interface :order order))
  (:singleton))


(defclass rb-tree-node (interface::rb-tree-node binary-tree-node)
  ())
