;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)

#+()
(eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op :cffi-grovel)
  (asdf:operate 'asdf:load-op :cffi-objects))
  
(defclass load-only-file (asdf:cl-source-file)
  ((last-loaded :accessor load-date :initform nil)))

(defmethod operation-done-p ((op compile-op) (component load-only-file))
  t)

(defmethod perform :around ((op compile-op) (component load-only-file))
  nil)

(defmethod operation-done-p ((op load-op) (component load-only-file))
  (and (load-date component)
       (>= (load-date component) (file-write-date (component-pathname component)))))

(defmethod perform ((op load-op) (component load-only-file))
  (prog1 (load (component-pathname component))
    (setf (load-date component)
          (file-write-date (component-pathname component)))))

(asdf:defsystem :cl-ctrie
  :serial t
  :description      "CL-CTrie: a lock-free, concurrent, key/value index supporting
                    both fast transient and efficient memory-mapped persistent storage
                    models."
  
  :author           "Dan Lentz <danlentz@gmail.com>"
  :license          "MIT"
  :version          "0.2.5"
  
  :long-description "This is a common-lisp implementation of the CTrie unordered map
                     data-structure described in the paper 'Concurrent Tries with
                     Efficient Non-Blocking Snapshots, (c) ACM 2-25-2012' by Prokopec,
                     Bronson, Bagwell, and Odersky.  A brief overview of general ctrie
                     concepts and existing implementations is available at
                     <http://en.wikipedia.org/wiki/Ctrie>, whose introduction is briefly
                     excerpted here as follows:

                            The Ctrie data structure is a non-blocking
                          concurrent hash array mapped trie based on
                          single-word compare-and-swap instructions in a
                          shared-memory system. It supports concurrent
                          LOOKUP, INSERT and REMOVE operations. Just like
                          the hash array mapped trie, it uses the entire
                          32-bit space for hash values thus having low
                          risk of hashcode collisions... Ctries have been
                          shown to be comparable in performance with
                          concurrent skip lists, concurrent hash tables
                          and similar data structures in terms of the
                          lookup operation...  However, they are far more
                          scalable than most concurrent hash tables where
                          the insertions are concerned. Most concurrent
                          hash tables are bad at conserving memory - when
                          the keys are removed from the hash table, the
                          underlying array is not [reduced in size]. Ctries
                          have the property that the allocated memory is
                          always a function of only the current number of
                          keys in the data-structure.  Ctries have logarithmic
                          complexity bounds of the basic operations...
                          with a low constant factor due to [large dispersal
                          ratio, (32^n arcs at level n)]. Ctries support a
                          lock-free, linearizable, constant-time SNAPSHOT (CLONE)
                          operation... This is a breakthrough in concurrent
                          data-structure design, since other existing concurrent
                          data-structures do not support [radable/writable]
                          snapshots. [This provides the means to support
                          features such as] lock-free, linearizable iterator,
                          size and clear operations. [This is superior to other]
                          existing concurrent data-structuresm [which require
                          the use of global locks [for exclusive, blocking
                          semantics for update access] permitting... [no
                          concurrent readers or writers] during any [update,
                          insert, remove or other modifications]. In particular,
                          Ctries have an O(1) ITERATOR creation operation, O(1)
                          CLEAR operation, O(1) DUPLICATE operation and an
                          amortized O(log n) operation for SIZE-RETRIEVAL.

                     Currently the lisp platform supported by cl-ctrie is SBCL version
                     1.0.55 or greater, although support could easily be entended to
                     other common-lisp implementations that offer support for atomic
                     compare-and-swap functionality, notably LispWorks 5.x/6.x, which is
                     also well instrumented with lock-free, atomic primitives, although
                     this is not necessarily a high priority for the initial development
                     cycle."
  
  ;;  :weakly-depends-on (:cldoc)
  :depends-on        (:closer-mop :contextl :alexandria :printv :cldoc :unicly :uuid
                       :com.informatimago.common-lisp.heap :cl-ppcre :osicat
                       :iterate :cl-irregsexp :hu.dwim.stefil :hu.dwim.serializer
                       :cl-store)
  
  :components ((:static-file  "cl-ctrie.asd")
                (:static-file "readme.md")
                (:file "common-readtable")
                (:file "common-macro")
             ;  (:file "common-condition")
                (:file "common-ord")
                (:file "common-io")
                (:file "common-pointer")
             ;  (:file "common-tty")
             ;  (:file "common-diff")
             ;  (:file "common-stream")
                (:file "common-ring")
                (:file "common-array")
                (:file "common-instance")
                (:file "common-vm")
                (:file "common-atomic")
                (:file "common-tls")
                (:file "mmap-package")
                (:file "mmap-utils")   
                (:file "mmap-struct")  
                (:file "mmap-mop")     
                (:file "mmap-mtagmap") 
                (:file "mmap-class")   
                (:file "mmap-types")   
                (:file "mmap-iterator")     
                (:file "mmap-box")
                (:file "mmap-storage")
                (:file "mmap-string") 
                (:file "mmap-mcons")        
                (:file "mmap-gc")                     
                (:file "ctrie-package")
                (:file "ctrie-special")
                (:file "ctrie-conditions")
                (:file "ctrie-cas")
                (:file "ctrie-util")
                (:file "ctrie-codec")
                (:file "ctrie-store")
                (:file "ctrie-layers")
                (:file "ctrie-protocol")
                (:file "ctrie")
             ;  (:file "ctrie-lambda")
                (:file "cvm-package")
                (:file "cvm-memory")
                (:file "cvm-host")
                (:file "cvm-ctrie")
                #+cldoc (:file "ctrie-doc")
                (:file "tree-package")
                (:file "tree-node")
                (:file "tree-common")
                (:file "tree-balanced")
                (:file "vstm")
                (:file "index-package")
                (:file "index-protocol")
                (:file "index-metaclass")
                (:file "index-object")
                (:file "index-root")
                (:load-only-file "ctrie-metaclass")
                (:file "collection-packages")
                (:load-only-file "collection-class")
                (:file "collection-set")
                ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod asdf:perform :after ((o asdf:load-op) (c (eql (asdf:find-system :cl-ctrie))))
  (let ((*package* (find-package :cl-ctrie)))
    (funcall (intern (symbol-name :generate-alternative-package) (find-package :cl-ctrie)))))

(defmethod asdf:perform :before ((o asdf:load-op) (c (eql (asdf:find-system :cl-ctrie))))
  (unless (find-package :_) (make-package :_)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DOCUMENTATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cldoc
(defmethod asdf:perform ((o asdf::doc-op) (c (eql (asdf:find-system :cl-ctrie))))
  (asdf:load-system :cl-ctrie)
  (funcall (intern (symbol-name :readme-quietly) (find-package :cl-ctrie))))

#+cldoc
(defmethod asdf:operation-done-p ((o asdf::doc-op) (c (eql (asdf:find-system :cl-ctrie))))
  nil)

#+cldoc
(defmethod asdf:operation-done-p ((o cldoc::print-op) (c (eql (asdf:find-system :cl-ctrie))))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass test-file (asdf:cl-source-file)
  ())

(defmethod perform ((op asdf:load-op) (c test-file))
  (let ((*load-verbose* nil)
         (*compile-verbose* nil))
    (call-next-method)))

(defmethod perform ((op asdf:compile-op) (c test-file))
  (let ((*load-verbose* nil)
         (*compile-verbose* nil))
    (call-next-method)))

(asdf:defsystem :cl-ctrie-test
  :serial t
  :depends-on (:cl-ctrie :hu.dwim.stefil :lparallel)
  :components ((:module :test :serial t
                 :components ((:file "suite")
                               (:test-file "common-ord")
                               (:test-file "common-io")
                               (:test-file "common-pointer")
                               (:test-file "mmap-class")
                               (:test-file "mmap-box")
                               (:test-file "mmap-symbol")
                               (:test-file "mmap-mptr")
                               (:test-file "mmap-tree")
                               (:test-file "mmap-gc")
                               (:test-file "ctrie-util")
                               (:test-file "ctrie-layers")
                               (:test-file "ctrie-primatives")
                               (:test-file "ctrie-smokecheck")
                               (:test-file "tree-node")
                               ))))


(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :cl-ctrie))))
  ;; (when (find-package :cl-ctrie-test)
  ;;   (funcall (intern (symbol-name :end-kernels) (find-package :cl-ctrie-test)))
  ;;   (delete-package :cl-ctrie-test))
  (asdf:load-system :cl-ctrie-test)
  (with-open-file (log-stream (asdf:system-relative-pathname c "test" :type "log")
                    :direction :output :if-does-not-exist :create :if-exists :supersede)
    (let ((*standard-output*  (make-broadcast-stream *standard-output* log-stream))
           (*trace-output*    (make-broadcast-stream *trace-output*    log-stream))
           (*error-output*    (make-broadcast-stream *error-output*    log-stream)))      
      (funcall (intern (symbol-name :run-all-tests) (find-package :cl-ctrie-test))))))


(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :cl-ctrie-test))))
  (asdf:test-system :cl-ctrie))

(defmethod asdf:operation-done-p ((o asdf:test-op) (c (eql (asdf:find-system :cl-ctrie))))
  nil)

(defmethod asdf:operation-done-p ((o asdf:test-op) (c (eql (asdf:find-system :cl-ctrie-test))))
  nil)



