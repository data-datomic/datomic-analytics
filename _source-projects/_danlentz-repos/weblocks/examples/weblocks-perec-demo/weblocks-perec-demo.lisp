;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :weblocks-perec-demo)

;; (def logger log () :appenders ((make-instance 'persistent-appender)))

(def logger demo ())

(def (function e) recreate-data ()
  (demo.warn "Creating initial model data items")
  (with-transaction
    (make-instance 'employee :last-name "Sandwich" :first-name "Reuben" :age 7
      :contract :consultant
      :address (make-instance 'address :state "NJ" :city "Pennsville"
                 :street "15 Meadow Rd")
      :company (make-instance 'company :name "DogTech" :industry "Relaxation"
                 :non-profit-p t))
    (make-instance 'employee :last-name "Style" :first-name "Gangham" :age 1
      :contract :intern
      :address (make-instance 'address :state "CA" :city "Far Out"
                 :street "Cred")
      :company (make-instance 'company :name "Phy" :industry "Pop Music"
                 :non-profit-p t))
    (demo.info "Model Data Created")))


(def (function e) start-weblocks-perec-demo (&rest args)
  "Starts the application by calling 'start-weblocks'
  with appropriate arguments."
  (apply #'start-weblocks args)
  (demo.info "Starting weblocks-perec-demo with args ~S" args)
;;  (when (getf args :clean)
;;    (clean-store *perec-store*))
  (unless (all-persistent-objects)
    (recreate-data)))
  

(def (function e) stop-weblocks-perec-demo ()
  "Stops the application by calling 'stop-weblocks'"
  (demo.info "Stopping weblocks-perec-demo")
  (stop-webapp 'weblocks-perec-demo)
  (stop-weblocks))


;; Define our application
(defwebapp weblocks-perec-demo :prefix ""
           :autostart t
           :public-files-cache-time 100000
	   :description "A web application based on Weblocks"
	   :dependencies '((:stylesheet "suggest")))


(trace
  weblocks:persist-object
  weblocks:persist-objects
  weblocks::update-object-view-from-request
  weblocks:dataseq-data-form-class
  weblocks::class-from-view
  weblocks:class-store
  weblocks:object-store
  weblocks:generate-scaffold-view
  weblocks:render-object-view
  weblocks:open-store
  weblocks:close-store
  weblocks:find-persistent-object-by-id
  weblocks:find-persistent-objects
  weblocks:dynamic-transaction
  ) 
