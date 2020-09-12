;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :weblocks-perec-demo)


(defun render-header (&rest args)
  "This function renders the page header."
  (declare (ignore args))
  (with-html (:div :class "header" (with-extra-tags))))


(defmethod render-page-body :after ((app weblocks-perec-demo) rendered-html)
  (with-html
    (:div :class "footer"
      (:p :id "contact-info" :style "color:white;"
        "Contact us with any questions or comments at the "
        (:a :href "http://groups.google.com/group/weblocks" "Weblocks Google Group"))
      (:br))))



;; (:p :id "system-info" "Running on " (str (concatenate 'string (server-type) " "
;; (server-version))) " (" (str (concatenate 'string (lisp-implementation-type) " "
;; (lisp-implementation-version))) ")")
;; (:img :src "/pub/images/footer/valid-xhtml11.png" :alt "This site has valid XHTML 1.1.")
;; (:img :src "/pub/images/footer/valid-css.png" :alt "This site has valid CSS.")
