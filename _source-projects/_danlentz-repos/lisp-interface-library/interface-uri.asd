;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(defsystem :interface-uri
  :description "Interface to URI/URN/UUID"
  :depends-on (:interface :puri :unicly)
  :components
  ((:module "interface"
     :components ((:module "uri"
                    :components ((:file "uri")))))))
