;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

;;;;; Universal Resource Identifier, implementing both URL and URN Interfaces

(in-package :interface)

;;;
;;; this is just here to facilitate a learning and prototyping environment of course in
;;; practice there'd be reader conditionalization or some other mechanism
;;;

(let ((pkgs '(:unicly :puri)))
  (unless (notany #'null (mapcar #'find-package pkgs))
    (ql:quickload pkgs)))

;;;
;;; In production there'd be a bit more care given to the nuances of uri/urn semantics. 
;;; 

(defgeneric render-uri (uri)
  (:method ((uri t))        (princ-to-string uri))
  (:method ((uri string))   uri)
  (:method ((uri puri:uri)) (puri:render-uri uri nil))
  (:method ((uri unicly:unique-universal-identifier)) (unicly:uuid-as-urn-string nil uri)))


(define-interface <uri-namestring> (<order>)
  ()
  (:method> order< (x y)
    (string-lessp (render-uri x) (render-uri y)))
  (:method> order<= (x y)
    (string-not-greaterp (render-uri x) (render-uri y)))
  (:method> order> (x y)
    (string-greaterp (render-uri x) (render-uri y)))
  (:method> order>= (x y)
    (string-not-lessp (render-uri x) (render-uri y)))
  (:method> == (x y)
    (string-equal (render-uri x) (render-uri y)))
  (:method> == ((x puri:uri) (y puri:uri))
    (puri:uri= x y))
  (:method> == ((x unicly:unique-universal-identifier) (y unicly:unique-universal-identifier))
    (unicly:uuid-eql x y))
  (:method> == ((x puri:uri) (y unicly:unique-universal-identifier))
    (== y x))
  (:method> == ((x unicly:unique-universal-identifier) (y puri:uri))
    (== x (unicly:make-v5-uuid unicly:*uuid-namespace-url* (render-uri y))))
  (:method> == ((x puri:urn) (y unicly:unique-universal-identifier))
    (== (render-uri x) (render-uri y)))
  (:method> == ((x unicly:unique-universal-identifier) (y puri:urn))
    (== y x))
  (:method> compare (x y)
    (cond
      ((==     x y)  0)
      ((order< x y) -1)
      (t             1)))
  (:abstract))


(define-interface <uri> (<uri-namestring>)
  ()
  (:method> uri-p (thing)
    (or (puri:uri-p thing) (unicly:unique-universal-identifier-p thing)))
  (:singleton))


(define-interface <url> (<uri> <makeable>) ()
  (:singleton))


(defmethod make ((<i> <url>) &key from)
  (etypecase from
    (string   (puri:uri from))
    (puri:uri from)))


(define-interface <urn> (<uri> <makeable>) ()
  (:singleton))


(defmethod make ((<i> <urn>) &key identity (ns unicly:*UUID-NAMESPACE-URL*) (type 5))
  (ecase type
    (5 (if identity (unicly:make-v5-uuid ns (render-uri (puri:uri identity))) ns))
    (4 (unicly:make-v4-uuid))
    (0 (unicly:make-null-uuid))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; testing it out...  The following does indeed show the intended operation and
;;; output that I'm looking to achieve from the <URI> Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (make <url> :from "http://www.gnu.org/")
;; => #<PURI:URI http://www.gnu.org/>
;;
;; (make <url> :from (make <url> :from "http://www.gnu.org/"))
;; => #<PURI:URI http://www.gnu.org/>
;;
;; (compare <url> (make <url> :from "http://www.gnu.org/") (make <url> :from "http://www.gnu.org/"))
;; => 0
;;
;; (== <url> (make <url> :from "http://www.gnu.org/") (make <url> :from "http://www.gnu.org/"))
;; => T
;;
;; (make <urn>)
;; => 6ba7b811-9dad-11d1-80b4-00c04fd430c8
;;
;; (== <urn> (make <urn>) unicly:*uuid-namespace-url*)
;; => T
;;
;; (== <uri> (make <urn>) unicly:*uuid-namespace-url*)
;; => T
;;
;; (make <urn> :type 4)
;; => f8321ec8-6b2e-4bba-99be-5537a4e59f91
;;
;; (== <uri> (make <urn> :type 4) (make <urn> :type 4))
;; => NIL
;;
;; (make <urn> :type 0)
;; => 00000000-0000-0000-0000-000000000000
;;
;; (== <uri> (make <urn> :type 0) (make <urn> :type 0))
;; => T
;;
;; (make <urn> :identity "test")
;; => da5b8893-d6ca-5c1c-9a9c-91f40a2a3649
;;
;; (render-uri (make <urn> :identity "test"))
;; => "urn:uuid:da5b8893-d6ca-5c1c-9a9c-91f40a2a3649"
;;
;; (== <uri> (make <urn> :identity "test") (make <urn> :identity "test"))
;; => T
;;
;; (== <uri> (make <urn> :identity "test") (make <urn> :identity "test0"))
;; => NIL
;;
;; (compare <uri> (make <urn> :identity "test") (make <urn> :identity "test"))
;; => 0
;;
;; (compare <uri> (make <urn> :identity "test") (make <urn> :type 0))
;; => 1
;;
;; (compare <uri>  (make <urn> :type 0) (make <urn> :identity "test"))
;; => -1
;;
;; (compare <uri> (make <url> :from "http://x.com/") (make <urn> :type 0))
;; => -1
;;
;; (compare <uri>  (make <urn> :type 0) (make <url> :from "http://x.com/"))
;; => 1
;;
;; (== <uri> (make <url> :from "http://x.com/") (make <urn> :identity "http://x.com/"))
;; => T
;;
;; (== <uri> (make <url> :from "http://x.com/") (make <urn> :identity "http://x.com"))
;; => T
;;
;; (== <uri> (make <url> :from "http://x.com") (make <urn> :identity "http://x.com/"))
;; => T
;;
;; (== <uri> (make <url> :from "http://y.com/") (make <urn> :identity "http://x.com/"))
;; => NIL
;;




