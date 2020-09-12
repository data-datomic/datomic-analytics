;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-


(in-package :interface)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric node/create (<i> k v l r)
  (:documentation "Join left and right subtrees at root k/v.  Assumes
  all keys in l < k < all keys in r."))

(defgeneric node/join (<i> k v l r)
  (:documentation "Join left and right subtrees at root k/v,
  performing a rotation operation to balance the resulting tree,
  if needed.  Assumes all keys in l < k < all keys in r, and the
  relative  balance of the left and right subtrees is such that no
  more than one rotation operation will be required to restore balance
  for the two provided subtrees, l and r"))

(defgeneric node/concat3 (<i> k v l r))


(defun node/singleton (<i> k &optional (v (unbound)))
  "create and return a newly allocated weight balanced
   tree containing a single association, that value V with key K."
  (node/create <i> k v (leaf) (leaf)))

(defun node/cons-enum (node &optional enum)
  "efficient mechanism to accomplish partial enumeration of
   tree-structure into a consp representation without incurring the
   overhead of operating over the entire tree.  Used internally for
   implementation of higher-level collection api routines"
  (cond
    ((empty-p <adams-tree> node) enum)
    (t
      (kvlr (k v l r) node
        (node/cons-enum l (list (cons k v) r enum))))))


(defun node/find (<i> k node)
  "find k (if exists) in only d comparisons (d is depth of tree)
   rather than the traditional compare/low compare/high which takes on
   avg (* 1.5 (- d 1))"
  (labels ((recur (this best)
             (cond
               ((empty-p <i> this)               (return-from recur best))
               ((order<  <i> k (node/k this))    (recur (node/l this) best))
               (t                                (recur (node/r this) this)))))
    (let ((best (recur node nil)))
      (when best
        (unless (order< <i> (node/k best) k)
          (return-from node/find best))))))


(defmethod lookup ((<i> <adams-tree>) node key)
  (node/find <i> key node))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Red-Black Tree Invariants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod node-balance ((node rb-tree-node))
  (- (node-height (right node))
     (node-height (left node))))

(defmethod check-invariant :before ((i <rb-tree>) (node rb-tree-node) &key)
  (assert (typep (node-height node) `(integer 1 ,most-positive-fixnum)) (node))
  (assert (<= (node-height node)
            (+ 2 (max (node-height (left node)) (node-height (right node))))) (node))
  (assert (member (node-balance node) '(-2 -1 0 1 2)) (node)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Red-Black Tree Node Allocation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod  node/create ((<i> <rb-tree>) k v l r)
  "create a tree node with left son l, key k, value v, and right son r.
   Must have all elements of l < v < all elements of r.
   l and r must be balanced and have a height difference =< 2"
  (make-instance (node-class <i>) :key k :value v :left l :right r
    :height (+ 1 (max (node-height l) (node-height r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fundamental API for Node Instance Access [node abstract model]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun node/k (node)
  "Public API to access the K constituent of node"
  (node-key node))

(defun node/v (node)
  "Public API to access the V constituent of node"
  (node-value node))

(defun node/l (node)
  "Public API to access the L constituent of node"
  (left node))

(defun node/r (node)
  "Public API to access the R constituent of node"
  (right node))

(defun node/h (node)
  "Public API to access the X constituent of node"
  (node/height node))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Node Constituent Accessors [relies only on abstract node model]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun node/kv (node)
  "return a list containing k and v constituent values of node"
  (list (node/k node) (node/v node)))

(defun node/lr (node)
  "return a list containing l and r constituent values of node"
  (list (node/l node) (node/r node)))

(defun node/kvlr (node)
  "return a list containing k, v, l, and r constituent values of node"
  (list (node/k node) (node/v node) (node/l node) (node/r node)))

(defun node/kvlrh (node)
  "return a list containing k, v, l, r, and x constituent values of node"
  (list (node/k node) (node/v node) (node/l node) (node/r node) (node/h node)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Destructuring Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro kv ((k v) node &body body)
  "destructure tree node: key, value"
   (let ((gtree (gensym (symbol-name :tree-))))
     `(let ((,gtree ,node))
        (let ((,k  (node/k ,gtree))
              (,v  (node/v ,gtree)))
          ,@body))))

(defmacro lr ((l r) node &body body)
  "destructure tree node: left, right"
   (let ((gtree (gensym (symbol-name :tree-))))
     `(let ((,gtree ,node))
        (let ((,l  (node/l ,gtree))
              (,r  (node/r ,gtree)))
          ,@body))))

(defmacro kvlr ((k v l r) node &body body)
  "destructure tree node: key, value, left, right"
  (let ((gtree (gensym (symbol-name :tree-))))
    `(let ((,gtree ,node))
       (let ((,k  (node/k ,gtree))
              (,v  (node/v ,gtree))
              (,l  (node/l ,gtree))
              (,r  (node/r ,gtree)))
         ,@body))))

(defmacro kvlrh ((k v l r h) node &body body)
  "destructure tree node: key, value, left, right, balance-param"
  (let ((gtree (gensym (symbol-name :tree-))))
    `(let ((,gtree ,node))
       (let ((,k  (node/k ,gtree))
              (,v  (node/v ,gtree))
              (,l  (node/l ,gtree))
              (,r  (node/r ,gtree))
              (,h  (node/h ,gtree)))
         ,@body))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod  node/join ((<i> <rb-tree>) k v l r)
  "Join left and right subtrees at root k/v, performing a rotation
  step to balance the resulting tree, if needed.  Assumes all keys in
  l < k < all keys in r, l and r balanced, and height difference <= 3"
  (let ((hl (node/height l))
         (hr (node/height r)))
    (cond
      ((> hl (+ 2 hr))    (kvlr (lk lv ll lr) l
                            (if (>= (node/height ll) (node/height lr))
                              (node/create <i> lk lv ll (node/create <i> k v lr r))
                              (kvlr (lrk lrv lrl lrr) lr
                                (node/create <i> lrk lrv
                                  (node/create <i> lr lv ll lrl)
                                  (node/create <i> k v lrr r))))))
      ((> hr (+ 2 hl))    (kvlr (rk rv rl rr) r
                            (if (>= (node/height rr) (node/height rl))
                              (node/create <i> rk rv (node/create <i> k v l rl) rr)
                              (kvlr (rlk rlv rll rlr) rl
                                (node/create <i> rlk rlv
                                  (node/create <i> k v l rll)
                                  (node/create <i> rk rv rlr rr))))))
        (t
          (node/create <i> k v l r)))))



(defmethod  node/concat3 ((<i> <rb-tree>) k v l r)
  (labels ((place-into (node k &optional (v (unbound)))
             (if (empty-p <i> node)
               (node/singleton <i> k v)
               (kvlr (key val l r) node
                 (cond
                   ((order< <i> k key) (node/join <i> key val (place-into l k v) r))
                   ((order< <i> key k) (node/join <i> key val l (place-into r k v)))
                   (t                  (node/create <i> key v l r)))))))
    (cond
      ((empty-p <i> l) (place-into r k v))
      ((empty-p <i> r) (place-into l k v))
      (t
        (kvlrh (lk lv ll lr lh) l
          (kvlrh (rk rv rl rr rh) r
            (cond
              ((> lh (+ 2 rh)) (node/join <i> lk lv ll (node/concat3 <i> k v lr r)))
              ((> rh (+ 2 lh)) (node/join <i> rk rv (node/concat3 <i> k v l rl) rr))
              (t               (node/create <i> k  v l r)))))))))



(defmethod node ((<i> <rb-tree>) &key key value left right)
  (node/concat3 <i> key value left right))





