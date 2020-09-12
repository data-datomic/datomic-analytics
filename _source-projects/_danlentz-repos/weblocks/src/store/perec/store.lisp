;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(defpackage :weblocks-perec
  (:use :cl-ppcre :weblocks
    :hu.dwim.common
    :hu.dwim.computed-class
    :hu.dwim.def
    :hu.dwim.defclass-star
    :hu.dwim.logger
    :hu.dwim.perec
    :hu.dwim.rdbms
    :hu.dwim.stefil
    :hu.dwim.util
;;    :hu.dwim.meta-model
;;    :hu.dwim.model
    :local-time
    :metacopy-with-contextl)

  (:shadow #:name #:log
    #:parent)
  
  (:shadowing-import-from :weblocks
    #:begin-transaction
    #:commit-transaction
    #:rollback-transaction)

  (:shadowing-import-from :hu.dwim.perec
    #:text #:defun/cc
    #:time #:oid
    #:form
    #:set))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import (let ((*package* (find-package :hu.dwim.perec)))
            (read-from-string
              "(*persistent-classes*
                *persistent-associations* *cache-slot-values*
                *mapped-type-precedence-list* *canonical-types* *compile-query-counter*
                *test-query-compiler* +unbound-slot-marker+ +not-cached-slot-marker+
                unbound-slot-marker-p
                canonical-type-for normalized-type-for mapped-type-for
                first-moment-for-partial-timestamp last-moment-for-partial-timestamp
                less-or-equal-p greater-or-equal-p
                validity-begin validity-end t-value unbound-slot-t
                find-slot collect-if concatenate-symbol set-type-p set-type-p*
                invalidate-cached-instance invalidate-all-cached-instances persistent
                clear-compiled-query-cache reset-compile-query-counter
                ensure-exported primary-table-slot-p data-table-slot-p
                primary-table-of primary-table-of data-tables-of
                direct-instances-identity-view-of direct-instances-data-view-of
                all-instances-identity-view-of all-instances-data-view-of
                prefetch-p cache-p compute-mapping reader-of writer-of
                compute-rdbms-types compute-reader compute-writer
                table-of columns-of reader-name-of writer-name-of
                lisp-value->rdbms-values rdbms-values->lisp-value
                depends-on-of depends-on-me-of primary-class-of effective-store-of
                compile-query
                defmapping
                )"))
    :weblocks-perec)
  (rename-package :hu.dwim.perec :hu.dwim.perec '(:perec))
  (rename-package :hu.dwim.rdbms :hu.dwim.rdbms '(:rdbms)))


(in-package :weblocks-perec)

(def logger store ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(def (function e) drop-all-local-tables ()
  (with-transaction
    (mapc (lambda (tab) (drop-table tab :cascade t))
      (collect-if (lambda (name) (starts-with-subseq "_" name))
        (list-tables)))))

(def (function e) ensure-exported* (thing)
  (etypecase thing
    (list              (mapc #'ensure-exported* thing))
    (symbol            (ensure-exported* (find-class thing)))
    (persistent-class  (ensure-exported thing))))


(def function call-with-ensured-transaction (store thunk)
  (if (rdbms:in-transaction-p)
    (funcall thunk)
    (with-transaction* (:database (etypecase store
                                    (database     store)
                                    (perec-store  (database-of store))))
      (funcall thunk))))

(def (macro e) ensure-transaction ((&optional store) &body body)
  (with-unique-names (gstore)
    `(let ((,gstore (or ,store *database*)))
       (call-with-ensured-transaction ,gstore (lambda () ,@body)))))

(def (function e) maybe-revive (thing &optional (store *database*))
  (flet ((do-it ()
           (typecase thing
             (list              (loop for o in thing collect (maybe-revive o)))
             (persistent-object (unless (instance-in-current-transaction-p thing)
                                  (revive-instance thing)))
             (t                 thing))))           
    (if (rdbms:in-transaction-p)
      (do-it)
      (ensure-transaction (store)
        (warn "Pointlessly Opening a transaction to briefly revive ~S" thing) 
        (do-it)))))

(def (function e) all-persistent-objects ()
  (ensure-transaction ()
    (select (o) (from o))))

(def (function e) purge-all-persistent-objects ()
  (with-transaction
    (mapc #'purge-instance (all-persistent-objects))))

(define-symbol-macro apo (all-persistent-objects))

(define-symbol-macro ? (prog1 * (describe *) (terpri)))

(define-symbol-macro ?* (ensure-transaction ()
                          (prog1 (maybe-revive *)
                            (describe *)
                            (terpri))))
#+swank
(def (function e) ^ (thing)
  (ensure-transaction ()
    (swank:inspect-in-emacs (maybe-revive thing))))

#+swank
(define-symbol-macro ^* (ensure-transaction ()
                          (swank:inspect-in-emacs (maybe-revive *))))

(def (macro e) a (&whole -whole- type &rest args)
  (declare (ignore type args))
  (check-type (rest -whole-) t)
  (with-unique-names (gtype gargs)
    `(let ((,gtype ',(cadr -whole-))
            (,gargs ',(cddr -whole-)))
       (print ,gtype)
       (print ,gargs)
       (ensure-transaction 
         (apply #'make-instance ,gtype ,gargs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Weblocks-Store API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def (special-variable e) *perec-store*)

(def (class* ea) perec-store ()
  ((database nil)))

(def method class-id-slot-name ((class persistent-class))
  'perec::oid)

(defmethod object-id :around ((obj persistent-object))
  (ensure-transaction ()
    (call-next-method)))

(def method object-id ((obj persistent-object))
  (oid-of obj))

(def method open-store ((store-type (eql :perec)) &rest args &key
                         (database-class-name    'postgresql/perec)
                         (transaction-class-name 'transaction)
                         (spec '(:database   "perec" :user-name "perec" :password  "perec"))
                         &allow-other-keys)
  (declare (ignore args))
  (store.info "opening :perec store")
  (setf *database* (make-instance database-class-name ;; 'hu.dwim.meta-model::postgresql/dwim
                     :muffle-warnings t
                     :generated-transaction-class-name transaction-class-name
                     ;; 'hu.dwim.meta-model::transaction-mixin/dwim
                     :connection-specification spec))
  (setf *compiled-query-cache* (make-compiled-query-cache))
  (setf *default-store* (make-instance 'perec-store :database *database*)))

(def function ensure-database ()
  (open-store :perec)
  (values *database* *default-store*))  

  
(def method close-store ((store perec-store))
  (store.info "closing :perec store ~A/~A" *perec-store* *database* )
  (setf *perec-store* nil)
  (when (eq *default-store* store)
    (setf *default-store* nil))
  (setf *database* nil))

(def method clean-store ((store perec-store))
  (store.warn "purging all object instances from :perec store ~A/~A"
    *perec-store* *database* )
  (with-transaction
    (purge-all-persistent-objects)))

(def method class-store ((class persistent-class))
  (if (subtypep (type-of *default-store*) 'perec-store)
    *default-store*
    (or *perec-store* (progn (mapstores (lambda (store)
                        (when (subtypep (type-of store) 'perec-store)
                          (return-from class-store store))))
                        (error "No perec-store available for instance of ~A" class)))))

(def method class-store ((classname (eql 'persistent-class)))
  (class-store (find-class classname)))


(def method class-visible-slots-impl ((class (eql (find-class 'persistent-object)))
                                       &key readablep writablep)
  (declare (ignore readablep writablep))
  (remove-if (lambda (dsd)
               (member (weblocks::slot-definition-name dsd)
                 '(perec::oid perec::persistent perec::transaction perec::transaction-event
                    perec::ensure-exported )))
    (call-next-method)))

(def method use-dynamic-transaction-p ((store perec-store))
  t)

(def method dynamic-transaction ((store perec-store) proc)
  (store.dribble "ensuring dynamic transaction on ~A/~A" store (database-of store))
  (call-with-ensured-transaction store proc))

(def method persist-object :around ((store perec-store) object &key)
  (ensure-transaction (store)
    (call-next-method)))

(def method persist-object ((store perec-store) (object persistent-object) &key)
  (store.info "Persisting ~S" object)
  (if (perec::instance-in-current-transaction-p object)
    (perec::commit)
    (if (perec::instance-in-transaction-p object)
      (perec::commit-transaction (database-of store) (perec::transaction-of object))
      (ensure-persistent (revive-instance object)))))

(def method delete-persistent-object :around ((store perec-store) (object persistent-object))
  (ensure-transaction (store)
    (call-next-method)))

(def method delete-persistent-object ((store perec-store) (object persistent-object))
  (store.dribble "deleting persistent instance ~A from ~A/~A" object store (database-of store))
  (purge-instance object))

(def method delete-persistent-object-by-id :around ((store perec-store) class-name object-id)
  (ensure-transaction (store)
    (call-next-method)))

(def method delete-persistent-object-by-id ((store perec-store) class-name object-id)
  (declare (ignore class-name))
  (store.dribble "deleting persistent instance-id ~D from ~A/~A"
    object-id store (database-of store))
  (purge-instance (load-instance object-id)))
  
(def method find-persistent-object-by-id :around ((store perec-store) class-name object-id)
  (ensure-transaction (store)
    (call-next-method)))

(def method find-persistent-object-by-id ((store perec-store) class-name object-id)
  (declare (ignore class-name))
  (store.dribble "finding persistent instance-id ~D from ~A/~A"
    object-id store (database-of store))
  (load-instance object-id))

(def function range-to-offset (range)
  (if range
    (min (car range) (cdr range))
    0))

(def function range-to-limit (range)
  (when range
    (abs (- (car range) (cdr range)))))

(def method find-persistent-objects ((store perec-store) class-name &key slot value 
                                      filter-fn range  (test 'equal)
                                      (order-by (cons 'perec::oid :asc)))
  (ensure-transaction (store)
    (let* ((test-slot-reader  (when (and slot value)
                                (first (slot-definition-readers 
                                         (find-slot-dsd (find-class class-name)
                                           slot)))))
            (order-slot-reader  (first (slot-definition-readers 
                                         (find-slot-dsd (find-class class-name)
                                           (car order-by)))))
            (order-direction    (if (eq (cdr order-by) :desc) :descending :ascending))
            (query (make-query `(select (o)
                                  (from   (o ,class-name))
                                  (where  (and
                                            ,(if filter-fn
                                               `(,filter-fn o) t)
                                            ,(if test-slot-reader
                                               `(,test ,value (,test-slot-reader o)) t)))
                                  (order-by ,order-direction (,order-slot-reader o))
                                  (offset   ,(range-to-offset range))
                                  (limit    ,(range-to-limit  range))))))
      (store.dribble "finding persistent objects with query: ~S" query)
      (execute-query query))))

;;;
;; (find-persistent-objects *default-store* 'prc::user :debug t)
;;
;; #<QUERY (NIL
;;          (SELECT (O)
;;            (FROM O)
;;            (WHERE (AND (TYPEP O 'HU.DWIM.PEREC.LOCAL::USER) T T))
;;            (ORDER-BY :ASCENDING (OID-OF O))
;;            (OFFSET 0)))>
;;
;; (find-persistent-objects *default-store* 'weblocks-perec-demo::employee :debug t
;;   :order-by (cons 'perec::oid :desc) :range (cons 1 3) :filter-fn 'identity
;;   :slot 'weblocks-perec-demo::age :value 7)
;;  
;; #<QUERY (NIL
;;          (SELECT (O)
;;            (FROM O)
;;            (WHERE
;;             (AND (TYPEP O 'WEBLOCKS-PEREC-DEMO::EMPLOYEE) (IDENTITY O)
;;                  (EQUAL 7 (WEBLOCKS-PEREC-DEMO::AGE-OF O))))
;;            (ORDER-BY :DESCENDING (OID-OF O))
;;            (OFFSET 1)
;;            (LIMIT 2)))>
;;;

(def method count-persistent-objects ((store perec-store) class-name &key filter-fn)
  (ensure-transaction (store)
    (let ((query (make-query `(select ((count (oid-of o)))
                                (from   (o ,class-name))
                                (where ,(if filter-fn `(,filter-fn o) t))))))
      (store.dribble "counting persistent objects with query: ~S" query)
      (first (execute-query query)))))

;;;
;; (count-persistent-objects *default-store* 'prc::user :debug t)
;;
;; #<QUERY (NIL
;;          (SELECT ((COUNT (OID-OF O)))
;;            (FROM O)
;;            (WHERE (AND (TYPEP O 'HU.DWIM.PEREC.LOCAL::USER) T))))>
;;
;;  (count-persistent-objects *default-store* 'weblocks-perec-demo::employee :debug t
;;    :filter-fn 'identity)
;;
;; #<QUERY (NIL
;;          (SELECT ((COUNT (OID-OF O)))
;;            (FROM O)
;;            (WHERE (AND (TYPEP O 'WEBLOCKS-PEREC-DEMO::EMPLOYEE) (IDENTITY O)))))>
;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persistent Standard-Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def persistent-class persistent-standard-object ()
  ((instance
     :initform nil
     :accessor instance-of
     :initarg :instance
     :type t)
    (classname
      :initform 'null
      :accessor classname-of
      :index t
      :type symbol)))

(def constructor persistent-standard-object ()
  (aprog1 (setf (classname-of -self-) (class-name (class-of (instance-of -self-))))
    (store.dribble "persisting standard-object of class ~A: ~A" it (instance-of -self-))))

(def method (setf instance-of) :after (value (-self- persistent-standard-object))
  (aprog1 (setf (classname-of -self-) (class-name (class-of (instance-of -self-))))
    (store.dribble "updating classname of standard-object  ~A to ~A" (instance-of -self-) it)))

;; (ensure-exported* 'persistent-standard-object)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persistent-Proxy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *proxies*      (make-hash-table))
(defvar *view-proxies* (make-hash-table))

(def class persistent-proxy ()
  ((proxy-oid :accessor proxy-oid :initarg :oid :initform nil)))


(def method object-id ((obj persistent-proxy))
  (proxy-oid obj))

(def function def-to-proxy-slot (def)
  `(,(weblocks::slot-definition-name def)
     ,@(mapcan #'(lambda (arg) `(:reader ,arg))  (weblocks::slot-definition-readers def))
     ,@(mapcan #'(lambda (arg) `(:writer ,arg))  (weblocks::slot-definition-writers def))
     ,@(mapcan #'(lambda (arg) `(:initarg ,arg)) (weblocks::slot-definition-initargs def))
     :initform ,(weblocks::slot-definition-initform def)
     :type ,(weblocks::slot-definition-type def)))
   
(def (function e) make-proxy-instance (class-name &rest args)
  "Creates an instance of the proxy."
  (store.info "Making proxy ~S" class-name)
  (apply #'make-instance (return-proxy-classname class-name) args))

(def generic base-class (thing))

;;(def method class-store ((classname (eql 'persistent-proxy)))
;;  (class-store 'persistent-class))

(def function return-proxy-classname (classname)
  (aprog1 (or (gethash classname *proxies*)
            (let* ((persistent-class (find-class classname))
                    (new-name (intern (format nil "~A-~A" classname (gensym)) *package*))
                    (visible-slot-defs (class-visible-slots-impl persistent-class))
                    (class-def `(defclass ,new-name (persistent-proxy)
                                  ((base-class
                                     :accessor base-class
                                     :allocation :class
                                     :initform ',classname)
                                    ,@(mapcar #'def-to-proxy-slot visible-slot-defs)))))
              (eval class-def)
              (setf (gethash classname *proxies* new-name) new-name)))
    (store.dribble "proxy-classname for ~S => ~S" classname it))) 

(def method shared-initialize :after ((class persistent-class) slot-names &rest args)
  "Ensure that the proxy list is always clean when the class is redefined"
  (declare (ignore args slot-names))
  (store.warn "clearing proxy for redefined persistent class ~S" (class-name class))
  (remhash (class-name class) *proxies*))

(def method weblocks:dataseq-data-form-class :around ((seq dataseq))
  "Catch persistent classes and return proxy classes that can be used to instantiate views"
  (let* ((classname (call-next-method)))
    (aprog1 (if (subtypep classname 'persistent-object)
              (return-proxy-classname classname)
              classname)
      (store.dribble "dataseq-data-form classname for ~A => ~A" classname it))))

(def method weblocks::class-from-view :around (view &optional class-name)
  "Part of the quickview / dataform method for creating anonymous instances"
  (aprog1 (if (and class-name (subtypep class-name 'persistent-object))
            (find-class (return-proxy-classname class-name))
            (call-next-method))
    (store.dribble "class from view ~A [~A] => ~A" view class-name it)))

(def method persist-object ((store perec-store) (object persistent-proxy) &key)
  "Catch when trying to persist a proxy and create a persistent-object"
  (store.info "Persisting proxy ~S" object)
  (with-transaction* (:database (database-of store))
    (aif (proxy-oid object)
      (let ((instance (find-persistent-object-by-id it)))
        (store.dribble "found matching persistent instance ~A" instance)
        (loop for slot in (mapcar #'slot-definition-name (class-slots (class-of object))) 
          when    (and (slot-boundp object slot) (not (member slot '(base-class proxy-oid))))
          do      (setf (slot-value instance slot) (slot-value object slot))
          finally (return instance)))
      (let ((instance (with-transaction* (:database (database-of store))
                        (make-instance (base-class object)))))
        (with-revived-instance instance
          (store.dribble "making new persistent instance ~A" instance)
          (loop for slot in (mapcar #'slot-definition-name (class-slots (class-of object)))
            when    (not (member slot '(base-class proxy-oid))) 
            do      (setf (slot-value instance slot) (slot-value object slot))
            finally (and (setf (proxy-oid object) (oid-of instance))
                      (return instance))))))))

;; (and (setf (proxy-oid object) (oid-of instance))

(def method object-class-name ((obj persistent-proxy))
  (base-class obj))


(def method weblocks:render-object-view-impl :around (o v w &key)
  (ensure-transaction ()
    (call-next-method (maybe-revive o) v w)))



