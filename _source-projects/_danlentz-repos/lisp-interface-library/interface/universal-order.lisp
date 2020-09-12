;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

;;;;; Universal Ordering Relation

;;; Definately open to suggestions if there are any types for which a different ordering
;;; relation is preferred.  I looked at fset's ordering, but I don't think it's very
;;; sensible, as the order of any two objects cannot be relied upon and may change
;;; based on the relative order in which their compare functions are defined.  This
;;; tries to be more stable and revert to lexical based comparison when all else fails;
;;; I believe this will be consistent within the context of an individual lisp session.

(in-package :interface)

;;;
;;; this is just here to facilitate a learning and prototyping environment of course in
;;; practice there'd be reader conditionalization or some other mechanism
;;;

(let ((pkgs '(:closer-mop :local-time)))
  (unless (notany #'null (mapcar #'find-package pkgs))
    (ql:quickload pkgs)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prepare Reader Conditionals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (:load-toplevel :compile-toplevel :execute)
  (when (find-package :local-time)
    (pushnew :local-time *features*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro writing-readably (&rest forms)
  "Macro to wrap around some forms, causing their writing to be more suitable for
   lexical comparison."
  `(let ((*print-escape*  t)
          (*print-level*  nil)
          (*print-length* nil)
          (*print-array*  t)
          (*package*     (find-package :keyword)))     
     ,@forms))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object Comparison Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric slots-to-compare-using-class (class object)
  (:documentation ""))


(defmethod  slots-to-compare-using-class ((class standard-class) object)
  "comparible slots may be cutomized by class, with the default being all slots"
  (mapcar #'c2mop:slot-definition-name 
    (c2mop:class-slots (class-of object))))


(defgeneric slots-to-compare (object)
  (:documentation ""))


(defmethod  slots-to-compare ((object standard-object))
  "comparible slots of a standard-object are defined by specialization on it class"
  (slots-to-compare-using-class (class-of (class-of object)) object))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-interface <universal-order> (<order>) ()

  (:method> compare  ((a null) (b null))
    ;; ordinal comparison of null values is always equal
    0)

  (:method>  compare ((a t) (b t))
    (if (eq (type-of a) (type-of b))
      (compare  (sxhash a) (sxhash b))
      (writing-readably 
        (compare 
          (format nil "~S" (type-of a))
          (format nil "~S" (type-of b))))))

  (:method>  compare ((a number) (b number))
    (cond
      ((eql a b)  0)
      ((<   a b) -1)
      (t          1)))

  (:method>  compare ((a real) (b real))
    (cond
      ((eql a b)  0)
      ((<   a b) -1)
      (t          1)))

  (:method>  compare ((a character) (b character))
    (cond
      ((char= a b)  0)
      ((char< a b) -1)
      (t 1)))

  (:method>  compare ((a string) (b string))
    (cond
      ((string= a b)  0)
      ((string< a b) -1)
      (t 1)))

  (:method>  compare ((a hash-table) (b hash-table))
    (cond
      ((string=  (princ-to-string a) (princ-to-string b))  0)
      ((string<  (princ-to-string a) (princ-to-string b)) -1)
      (t 1)))

  (:method>   compare ((a symbol) (b symbol))
    (let ((pkgcmp (compare (symbol-package a) (symbol-package b))))
      (if (zerop pkgcmp)
        (compare (symbol-name a) (symbol-name b))
        pkgcmp)))
  
  (:method>  compare ((a pathname) (b pathname))
    (compare (namestring a) (namestring b)))


  (:method>  compare ((a package) (b package))
    (compare (package-name a) (package-name b)))

  #+local-time
  (:method>  compare ((a local-time:timestamp) (b local-time:timestamp))
    (cond
      ((local-time:timestamp= a b)  0)
      ((local-time:timestamp< a b) -1)
      (t 1)))

  (:method>  compare ((a standard-object) (b standard-object))
    ;; ordinal comparison of arbitrary standard-objects performed as follows:
    ;;  -- objects of different classes ordered by lexical comparison of class name
    ;;  -- objects of a class for which slots-to-compare returns null are ordered by lexical
    ;;      comparison of printed representation.  For standard print-unreadable-object output,
    ;;      this achieves equality on the objects being #'eq, otherwise returns a consistent
    ;;      but arbitrary ordinal comparison value for the lifetime of these specific instances.
    ;;      Customized print-unreadable-object representations also provides a simple means
    ;;      of adjustment to the resulting comparison.
    ;;  -- objects of identical class are compared based on the boundness and slot-value of
    ;;      the slots-names in list returned by slots-to-compare.  Slots unbound in both
    ;;      obects are considered equal. Unbound slots considered greater than bound slots of the
    ;;      same slot-name. Two bound slots-values with same slot-name are compared recursively
    ;;      with ord:compare.
    ;;  -- when all preceding steps complete without ordinal determination, the objects are
    ;;      considered equal
    (if (not (eq (class-of a) (class-of b)))
      (writing-readably 
        (compare 
          (format nil "~S" (class-name (class-of a)))
          (format nil "~S" (class-name (class-of b)))))
      (let ((slots (slots-to-compare a)))
        (when (null slots)
          (return-from compare
            (writing-readably 
              (compare (format nil "~S" a) (format nil "~S" b)))))
        (loop
          :for x :in slots
          :do (cond
                ((and (not (slot-boundp a x)) (not (slot-boundp b x)))  nil)            
                ((not (slot-boundp a x)) (return-from compare   1))
                ((not (slot-boundp b x)) (return-from compare  -1))
                (t
                  (let ((c (compare (slot-value a x) (slot-value b x))))
                    (unless (zerop c)
                      (return-from compare c))))))
        0)))
  
  (:method>  order< (a b)
    (minusp (compare  a b)))

  (:method>  order<= (a b)
    (not (plusp (compare a b))))

  (:method>  order> (a b)
    (plusp (compare  a b)))
  
  (:method>  order>= (a b)
    (not (minusp (compare  a b))))

  (:method>  == (a b)
    (zerop (compare  a b)))

  (:singleton))


(defun check-universal-ordinality (lesser greater)
  (assert (eql (compare <universal-order> lesser  greater) -1))
  (assert (eql (compare <universal-order> greater lesser)   1))
  (assert (eql (compare <universal-order> lesser  lesser)   0))
  (assert (eql (compare <universal-order> greater greater)  0))
  (assert (order< <universal-order> lesser greater))
  (assert (not (order< <universal-order> greater lesser)))
  (assert (order<= <universal-order> lesser greater))
  (assert (not (order<= <universal-order> greater lesser)))
  (assert (not (order> <universal-order> lesser greater)))
  (assert (order> <universal-order> greater lesser))
  (assert (not (order>= <universal-order> lesser greater)))
  (assert (order>= <universal-order> greater lesser))
  (assert (order>= <universal-order> lesser lesser))
  (assert (order>= <universal-order> greater greater))
  (assert (order<= <universal-order> lesser lesser))
  (assert (order>= <universal-order> lesser lesser))
  (assert (order<= <universal-order> greater greater))
  (assert (== <universal-order> greater greater))
  (assert (not (== <universal-order> lesser greater)))
  (assert (not (== <universal-order> greater lesser))))


(defclass foo ()
  ((a :initarg :a)
    (b :initarg :b)))

(defclass bar () ())

(defun check-universal-ordinality-for-various-types ()
  (progn 
    (check-universal-ordinality #\a  #\z)
    (check-universal-ordinality 1    2)
    (check-universal-ordinality 10   20)
    (check-universal-ordinality 10.0 20.0)
    (check-universal-ordinality 10   20.0)
    (check-universal-ordinality 10.0 20)
    (check-universal-ordinality 0  :x)
    (check-universal-ordinality 'x   :x)
    (check-universal-ordinality 10.0 'x)
    (check-universal-ordinality :x   10.0)
    (check-universal-ordinality "aardvark" "zebra")
    (check-universal-ordinality :aardvark :zebra)
    (check-universal-ordinality '#:aardvark '#:zebra)
    (check-universal-ordinality 'aardvark 'zebra)
    (check-universal-ordinality (make-instance 'standard-object) (make-instance 'standard-object))
    (check-universal-ordinality (make-instance 'foo :a 0 :b 0)   (make-instance 'foo :a 1 :b 1))
    (check-universal-ordinality (make-instance 'bar) (make-instance 'foo :a 0 :b 0))
    (check-universal-ordinality (local-time:now) (progn (sleep .001) (local-time:now)))
    (check-universal-ordinality (find-package :common-lisp) (find-package :keyword))
    (check-universal-ordinality #p"/tmp/aardvark" #p"/tmp/zebra")))
