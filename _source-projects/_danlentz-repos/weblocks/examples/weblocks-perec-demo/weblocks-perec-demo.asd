;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(defpackage :weblocks-perec-demo-asd
  (:use :cl :asdf))

(in-package :weblocks-perec-demo-asd)


(defsystem weblocks-perec-demo
    :name "weblocks-perec-demo"
    :version "0.1"
    :author "Dan Lentz"
    :licence "Public Domain"
    :description "weblocks-perec-demo"
    :depends-on (:weblocks-perec)
    :components ((:file "package")
                 (:module conf
		  :components ((:file "stores"))
                  :depends-on ("package"))
                 (:file "weblocks-perec-demo"
		  :depends-on ("conf" "package"))
		 (:module src
		  :components ((:file "layout"
				      :depends-on (model))
			       (:file "snippets")			    
			       (:file "init-session"
				      :depends-on ("layout" "snippets"))
			       (:module model
					:components ((:file "company")
						     (:file "address")
						     (:file "person"
							    :depends-on ("address"))
						     (:file "employee"
							    :depends-on ("person" "company")))))
		  :depends-on ("weblocks-perec-demo" conf package))))

