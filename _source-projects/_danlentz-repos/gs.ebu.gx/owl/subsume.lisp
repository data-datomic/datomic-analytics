;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; Subsume Module
;;;
;;; This code is written by Seiji Koide.
;;;
;;; Copyright (c) 2009 Seiji Koide
;;;
;; History
;; -------
;; 2009.11.03    File created and subsumed-p part is moved here from leanOWL.

(cl:provide :subsume)

(eval-when (:execute :load-toplevel :compile-toplevel)
  )

(defpackage :gx
  (:use :common-lisp)
  (:export subsumed-p ))

(in-package :gx)

;;;
;;;; Equality 
;;;

;;;
;;;; Functional Property
;;;

;;; owl:FunctionalProperty does not belong to OWL universe, and an instance of 
;;; owl:FunctionalProperty may not belong to OWL universe.

(defConcept owl:FunctionalProperty (rdf:type rdfs:Class))

(defun functional-property? (name)
  "returns true if <name> is an owl functional property name"
  (declare (inline))
  (and (boundp name) (functional-property-p (symbol-value name))))

(defun functional-property-p (obj)
  "Is this <obj> an instance of owl:FunctionalProperty?"
  ;;this is the same as '(cl:typep <obj> owl:FunctionalProperty)'
  (and (excl::standard-instance-p obj)
       (let ((class (class-of obj)))
         (cond ((eq class (load-time-value 
                           (symbol-value 'owl:FunctionalProperty))))
               ((mop:class-finalized-p class)
                (and (member (load-time-value
                              (symbol-value 'owl:FunctionalProperty))
                             (mop:class-precedence-list class)
                             :test #'eq)
                     t))
               ((labels ((walk-partial-cpl (c)
                           (let ((supers (mop:class-direct-superclasses c)))
                             (when (member (load-time-value
                                            (symbol-value 'owl:FunctionalProperty))
                                           supers
                                           :test #'eq)
                               (return-from functional-property-p t))
                             (mapc #'walk-partial-cpl supers))))
                  (declare (dynamic-extent #'walk-partial-cpl))
                  (walk-partial-cpl class)
                  nil))))))

(defun %get-functional-property-sames (x)
  (when (slot-exists-p x 'funprop-inverse)
    (loop for (prop . subj) in (slot-value x 'funprop-inverse)
        as obj = (slot-value subj prop)
        unless (eql x obj)
        append (remove x (mklist obj)))))

;;;
;;;; Inverse Functional Property
;;;
;;; An instance of owl:InversefunctionalProperty may not be an instance of 
;;; owl:ObjectProperty. Then, the range value may not be owl:Thing and may be rdfs:Literal.

(defConcept owl:InverseFunctionalProperty (rdf:type rdfs:Class))

(defProperty owl:disjointWith)

(defun inverse-functional-property-p (obj)
  "Is this <obj> an instance of owl:InverseFunctionalProperty?"
  ;;this is the same as '(cl:typep <obj> owl:InverseFunctionalProperty)'
  (and (excl::standard-instance-p obj)
       (let ((class (class-of obj)))
         (cond ((eq class (load-time-value 
                          (symbol-value 'owl:InverseFunctionalProperty))))
               ((not (mop:class-finalized-p class))
                (labels ((walk-partial-cpl (c)
                           (let ((supers (mop:class-direct-superclasses c)))
                             (when (member
                                    (load-time-value 
                                     (symbol-value 'owl:InverseFunctionalProperty))
                                    supers
                                    :test #'eq)
                               (return-from inverse-functional-property-p t))
                             (mapc #'walk-partial-cpl supers))))
                  (declare (dynamic-extent #'walk-partial-cpl))
                  (walk-partial-cpl class)
                  nil))
               (t (and (member (load-time-value 
                                (symbol-value 'owl:InverseFunctionalProperty))
                               (mop:class-precedence-list class)
                               :test #'eq)
                       t))))))

(defun %get-inverse-functional-property-sames (x)
  (loop for inv-funprop in (collect-owl-role-name-if #'inverse-functional-property-p x)
      as inv-funprop-val = (slot-value x inv-funprop)
      when inv-funprop-val
      append (loop for (subj val) in (collect-all-extensions-of inv-funprop)
                 when (or (equal inv-funprop-val val)
                          (member inv-funprop-val (%same-as-of val)
                                  :test #'definitely-%owl-same-p))
                 collect subj)))

(defun functional-property-equal-p  (x y &key (test #'definitely-%owl-same-p))
  ;; rdfp1 ter Horst
  (not (not (intersection (slot-value x 'funprop-inverse) (slot-value y 'funprop-inverse)
                          :test #'(lambda (xx yy)
                                    (and (eq (car xx) (car yy))                  ; funprop
                                         (funcall test (cdr xx) (cdr yy))))))))  ; object

(defun inverse-functional-property-equal-p (x y &key (test #'definitely-%owl-same-p))
  ;; rdfp2 cf. ter Horst
  (not (not 
        (some #'(lambda (role)
                  (let ((fil1 (slot-value x role))
                        (fil2 (slot-value y role)))
                    (cond ((equal fil1 fil2) t)
                          ; if fillers are string and equal, then equal (OWL-Full specs)
                          ((null fil1) nil)
                          ((null fil2) nil)
                          ((and (consp fil1) (consp fil2)) (intersection fil1 fil2 :test test))
                          ((consp fil1) (member fil2 fil1 :test test))
                          ((consp fil2) (member fil1 fil2 :test test))
                          ((funcall test fil1 fil2)))))
              (intersection (collect-owl-role-name-if #'inverse-functional-property-p x)
                            (collect-owl-role-name-if #'inverse-functional-property-p y))))))

(defun definitely-%owl-same-p (x y &optional pairs)
  (declare (optimize (speed 3) (safety 0)))
  (cond ((and (name x) (name y) (equal (name x) (name y))))
        ;; occurence check
        ((and pairs
              (or (member (cons x y) pairs :test #'equal)
                  (member (cons y x) pairs :test #'equal)))
         nil)
        ((and (owl-thing-p x) (owl-thing-p y)
              (member x (same-as-of y)
                      :test #'(lambda (a b) (definitely-%owl-same-p a b (cons (cons x y) pairs))))))
        ; See rdfp6, rdfp7
        ((and (rsc-object-p x) (rsc-object-p y)
              (or
               ;; rdfp1 by ter Horst
               (functional-property-equal-p
                x y :test #'(lambda (a b) (definitely-%owl-same-p a b (cons (cons x y) pairs))))
               ;; rdfp2 by ter Horst
               (inverse-functional-property-equal-p
                x y :test #'(lambda (a b) (definitely-%owl-same-p a b (cons (cons x y) pairs)))))))
        ((and (owl-thing-p x) (owl-thing-p y)
              (member x (slot-value y 'different-from))) ; explicitly stated different
         nil)
        ((equal x y))  ; <--
        ))

;;;
;;;; Subsumption 
;;;

(excl:without-redefinition-warnings
(defun subsumed-p (c d)
  "tests whether <c> is subsumed by <d> in OWL semantics."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((equal c d) (values t t))
        ((eq d t) (values t t))
        ((eq c nil) (values t t))
        ((eq d nil) (values nil t))
        ((eq c t) (values nil t))
        ;; this is for fast computation as RDF semantics
        ((and (not (rdf-instance-p c))
              (not (rdf-instance-p d))
              (or (%clos-subtype-p c d)
                  (when (%clos-subtype-p d c) (return-from subsumed-p (values nil t)))))
         (values t t))
        ;; in OWL semantics
        ((definitely-%owl-same-p c d) (values t t))   ; See rdfp9.
        ((and (owl-oneof-p c) (owl-oneof-p d))
         ;; {vin:Dry vin:OffDry} < {vin:Dry vin:OffDry vin:Sweet}
         (oneof-subsumed-p c d))
        ((and (or (eq c owl:Class) (owl-class-p c))
              (or (eq d owl:Class) (owl-class-p d))
              ;; note that owl:Class is not in owl:Class extensions.
              (or (eq c owl:Nothing)
                  (eq d owl:Thing)
                  (when (or (eq d owl:Nothing) (eq c owl:Thing)) (return-from subsumed-p (values nil t)))
                  (some #'(lambda (cc)
                            (some #'(lambda (dd) (subsumed-p-without-equivalency cc dd))
                                  (equivalent-classes-of d)))
                        (equivalent-classes-of c))))
         (values t t))
        ;; as individuals
        ((and (rsc-object-p c) (rsc-object-p d)
              (transitive-property-subsumed-p c d))
         (values t t))
        ;; resolve for RDF universe
        ((and (symbolp c) (symbolp d))
         (cond ((and (datatype? c) (datatype? d))
                (%rdf-subtypep c d))
               ((and (object? c) (object? d))
                (subsumed-p (symbol-value c) (symbol-value d)))
               ((and (datatype? c) (object? d))
                (values (subtypep (find-class 'rdf:XMLLiteral) d) t))
               ((and (object? c) (datatype? d))
                (values nil t))
               (t (values nil nil))))
        ((and (symbolp d) (object? d))
         (subsumed-p c (symbol-value d)))
        ((and (symbolp c) (rsc-object-p c))
         (subsumed-p (symbol-value c) d))
        ((and (uri-p c) (uri-value c)
              (uri-p d) (uri-value d))
         (subsumed-p (uri-value c) (uri-value d)))
        ((and (uri-p c) (uri-value c)) (subsumed-p (uri-value c) d))
        ((and (uri-p d) (uri-value d)) (subsumed-p c (uri-value d)))
        ;; %owl-subtypep is critical to process the form '(and ...)' for C.
        ((multiple-value-bind (val1 val2) (%owl-subtypep c d)
           (when val2 (return-from subsumed-p (values val1 val2)))))
        ((and (not (rdf-instance-p c))
              (not (rdf-instance-p d))
              (cl:subtypep c d))
         (values t t))
        (t (values nil nil))))
)

(defun subsumed-p-without-equivalency (cc dd)
  "subsumtion test without class equivalency of <cc> and <dd> for CLOS objects."
  (cond ((equal cc dd) (values t t))
        ((eq dd t) (values t t))
        ((eq cc nil) (values t t))
        ((eq cc t) (values nil t))
        ((eq dd nil) (values nil t))
        ((eq dd owl:Thing) (values t t))
        ((eq cc owl:Nothing) (values t t))
        ((eq dd owl:Nothing) (values nil t))
        ((eq cc owl:Thing) (values nil t))
        ((%clos-subtype-p cc dd) (values t t)) ; CLOS or RDFS default reasoning
        ((%clos-subtype-p dd cc) (values nil t))
        ((and (union-of cc) ; vin:Fruit has no supers but has two subs.
              (every #'(lambda (csub) (subsumed-p-without-equivalency csub dd)) (union-of cc)))
         (values t t))
        ;; union branching
        ((and (union-of dd)
              (some #'(lambda (dsub)
                        (subsumed-p-without-equivalency cc dsub))
                    (union-of dd)))
         (values t t))
        ((and (intersection-of dd)
              (intersection-subsumed-p cc (intersection-of dd)))
         (values t t))
        ;; See rdfp12 by ter Horst
        ((%owl-equivalent-p-without-equivalents cc dd) (values t t))
        ;; rdfp1 by ter Horst
        ((functional-property-equal-p
          cc dd :test #'%owl-equivalent-p-without-equivalents)
         (values t t))
        ;; rdfp2 by ter Horst
        ((inverse-functional-property-equal-p
          cc dd :test #'%owl-equivalent-p-without-equivalents)
         (values t t))
        ;; complement and disjoint
        ((%owl-complement-p cc dd) (values nil t))
        ((and (slot-exists-p cc 'disjoint-classes)
              (slot-boundp cc 'disjoint-classes)
              (member dd (slot-value cc 'disjoint-classes)
                      :test #'owl-equivalent-p))
         (values nil t))
        ((and (slot-exists-p dd 'disjoint-classes)
              (slot-boundp dd 'disjoint-classes)
              (member cc (slot-value dd 'disjoint-classes)
                      :test #'owl-equivalent-p))
         (values nil t))
        (t (values nil nil))))

(defun unfold-intersection (conj)
  "returns unfolded concepts of <conj>, if <conj> is an intersection in OWL, 
   otherwise returns <conj> itself."
  (cond ((intersection-p conj)
         (mapcar #'unfold-intersection (slot-value conj 'owl:intersectionOf)))
        (t conj)))
          
(defun intersection-subsumed-p (c dintersections)
  (let ((unfolded
         (mappend #'(lambda (conj) (mklist (unfold-intersection conj)))
                  dintersections)))
    (%intersection-subsumed-p c (remove-duplicates unfolded))))
(defun %intersection-subsumed-p (c dintersections)
  (unless (mop:class-finalized-p c) (mop:finalize-inheritance c))
  (let ((cpl (clos:class-precedence-list c))
        (cslots (mop:class-slots c))
        (dclss (remove-if #'owl-restriction-p dintersections))
        (drestrs nil))
    (and (every #'(lambda (dcls) 
                    (some #'(lambda (ccls) (subsumed-p ccls dcls))
                          (remove-if #'owl-restriction-p cpl)))
                dclss)
         (or (null (setq drestrs (remove-if-not #'owl-restriction-p dintersections)))
             (let ((props (remove-duplicates
                           (mapcar #'(lambda (dr) (name (onproperty-of dr))) drestrs)))
                   (dcards (remove-if-not #'(lambda (x) (cl:typep x owl:cardinalityRestriction)) drestrs))
                   (dothrs (remove-if #'(lambda (x) (cl:typep x owl:cardinalityRestriction)) drestrs)))
               (loop for prop in props
                   always 
                     (flet ((onproperty-p (x) (eq prop (name (onproperty-of x)))))
                       (let ((dothr (remove-if-not #'onproperty-p dothrs))
                             (dcard (remove-if-not #'onproperty-p dcards))
                             (cslot (find prop cslots :key #'mop:slot-definition-name))
                             (var (new-variable "gx")))
                         ;(format t "~%cslots:~S~%prop:~S cslot:~S" cslots prop cslot)
                         ;(format t "~%dothr:~S dcard:~S" dothr dcard)
                         (assert (or (null dcard) (length=1 dcard)))
                         (setq dcard (car dcard))
                         (flet ((get-maxcard (x)
                                             (and x
                                                  (or (and (slot-boundp x 'owl:cardinality) (slot-value x 'owl:cardinality))
                                                      (and (slot-boundp x 'owl:maxCardinality) (slot-value x 'owl:maxCardinality)))))
                                (get-mincard (x)
                                             (and x
                                                  (or (and (slot-boundp x 'owl:cardinality) (slot-value x 'owl:cardinality))
                                                      (and (slot-boundp x 'owl:minCardinality) (slot-value x 'owl:minCardinality))))))
                           (let ((cmax (slot-definition-maxcardinality cslot))
                                 (cmin (slot-definition-mincardinality cslot))
                                 (dmax (get-maxcard dcard))
                                 (dmin (get-mincard dcard)))
                             (let ((cmodels (generate-models-for-susumee-from-slot var prop cmax cmin cslot nil)))
                               (format t "~%cmodels:~S" cmodels)
                               (when dmax
                                 (setq cmodels
                                       (loop for model in cmodels
                                           when (satisfy-max-cardinality dmax model)
                                           collect model)))
                               (when dmin
                                 (setq cmodels
                                       (loop for model in cmodels
                                           when (satisfy-min-cardinality dmin model)
                                           collect model)))
                               (let ((dmodels (create-models-for-subsumer-from-restrictions var prop dmax dmin dothr nil)))
                                 (format t "~%dmodels:~S" dmodels)
                                 (every #'(lambda (dmodel)
                                            (some #'(lambda (cmodel) (%satisfy-model cmodel dmax dmin dmodel)) cmodels))
                                        dmodels)))))))))))))
  
;;;
;;;; Models
;;;
;;; <models> := ( <model>* )
;;; <model>  := ( <bindings> <cbindings> . <cmax> <cmin> <atoms> )
;;; <atoms>  := ( <atom>* )
;;; <atom>   := ( <role> <predecessor> <successor> ) 
;;; <predecessor> and <successor> is a <var>.
;;; * <models> are disjunction of models.
;;; * <atoms> are conjunction of atomic formula in logic.
;;; * <atom> may be general logical form, but should be a binary form in this program.
;;; * <cbindings> provides concept (class) bindings for logical variable.
;;; * <bindings> is usual one.
;;; * <role> is a lisp symbol.
;;; * <class> is a resource object.
;;;

(defun generate-models-for-susumee-from-slot (var role cmax cmin slotd models)
  "<var> is a <role>-predecessor. "
  (let ((styp (mklist (mop:slot-definition-type slotd))))
    (format t "~%styp: ~S cmax: ~S cmin: ~S" styp cmax cmin)
    ;; styp includes all user defined restrictions and range constraints on this var and role.
    (let ((types nil)
          (alls nil)
          (exists nil)
          (fillers nil)
          (conjuncts (case (op styp)
                       (and (cdr styp))
                       ((or not) (error "Cant happen!"))
                       (otherwise styp))))
      (loop for conjunct in conjuncts
          do (cond ((cl:typep conjunct 'forall)
                    (pushnew (forall-filler conjunct) alls))
                   ((cl:typep conjunct 'exists)
                    (pushnew (exists-filler conjunct) exists))
                   ((cl:typep conjunct 'fills)
                    (pushnew (fills-filler conjunct) fillers))
                   (t (pushnew conjunct types)))
          finally (progn
                    (setq types (nreverse types))
                    (setq alls (nreverse alls))
                    (setq exists (nreverse exists))
                    (setq fillers (nreverse fillers))))
      (generate-subsumee-models var role cmax cmin types alls exists fillers models))))

(defun create-models-for-subsumer-from-restrictions (var role max min conjuncts models)
  "creates a model from universal <var> and restriction <conjuncts>.
   Note that all properties in <conjuncts> are is <role>."
  (let ((fills      (remove-if-not #'(lambda (r) (cl:typep r 'owl:hasValueRestriction)) conjuncts))
        (exists     (remove-if-not #'(lambda (r) (cl:typep r 'owl:someValuesFromRestriction)) conjuncts))
        (universals (remove-if-not #'(lambda (r) (cl:typep r 'owl:allValuesFromRestriction)) conjuncts))
        (types      (remove-if     #'(lambda (r) (cl:typep r 'owl:Restriction)) conjuncts)))
    (format t "~%fills:~S exists:~S universals:~S types:~S" fills exists universals types)
    (let ((fillers (mapcar #'(lambda (fill) (slot-value fill 'owl:hasValue)) fills))
          (exists  (mapcar #'(lambda (exst) (slot-value exst 'owl:someValuesFrom)) exists))
          (alls    (mapcar #'(lambda (all)  (slot-value all  'owl:allValuesFrom)) universals)))
      (generate-subsumer-models var role max types alls exists fillers models))))

(defun clash-p (bindings cbindings conjuncts)
  "tests conjuncts clashing or not."
  (loop for atoms on (subst-bindings bindings conjuncts) by #'cddr
      do (destructuring-bind (role pred suc) (first atoms)
           (format t "~%role:~S pred:~S suc:~S" role pred suc)
           (loop for (r p s) in (cdr atoms)
               when (and (or (format t "~%r:~S p:~S s:~S" r p s) t)
                         (eq r role)
                         (owl-same-p+ p pred bindings)
                         (cond ((and (skolem-p s) (skolem-p suc))
                                (multiple-value-bind (s-pred s-suc) (unskolemize s)
                                  (multiple-value-bind (suc-pred suc-suc) (unskolemize suc)
                                    (when (and (eq s-pred suc-pred) (eq (lookup s-suc bindings) (lookup suc-suc bindings)))
                                      (disjoint-p (class-of+ s-suc bindings cbindings)
                                                  (class-of+ suc-suc bindings cbindings))))))
                               ((skolem-p s)
                                (multiple-value-bind (s-pred s-suc) (unskolemize s)
                                  (format t "~%s-pred:~S s-suc:~S" s-pred s-suc)
                                  (when (and (eq s-pred pred) (eq (lookup s-suc bindings) s))
                                    (disjoint-p (class-of+ s-suc bindings cbindings)
                                                (class-of+ suc bindings cbindings)))))
                               ((skolem-p suc)
                                (multiple-value-bind (suc-pred suc-suc) (unskolemize suc)
                                  (when (and (eq suc-pred p) (eq (lookup suc-suc bindings) suc))
                                    (disjoint-p (class-of+ s bindings cbindings)
                                                (class-of+ suc-suc bindings cbindings)))))
                               (t (disjoint-p (class-of+ s bindings cbindings)
                                              (class-of+ suc bindings cbindings)))))
               do (return-from clash-p t)))))

(defun add-conjunct-existentially (role predecessor successor exist model models)
  "adds a new atom of (<role> <predecessor> <successor>:<exist>) into <model>, 
   and returns a satisfiable model. Note this function is applied for existential 
   quantification of successor."
  (destructuring-bind (bindings cbindings . atoms) model
    (when (subsumed-p exist (class-of+ successor bindings cbindings))
      (setq cbindings (extend-bindings successor exist cbindings)))
    (setq bindings
          (extend-bindings successor (cons (skolem-constant successor) predecessor)  ; skolemize successor
                           bindings))
    (setq atoms (cons `(,role ,predecessor ,successor) atoms))
    (unless (clash-p bindings cbindings atoms)
      (list (cons bindings (cons cbindings atoms))))))

(defun impose-conjunct-existentially (role predecessor successor type model models)
  "imposes a new atom of (<role> <predecessor> <successor>:<type>) onto each atom in <atoms>
   and returns a satisfiable model for <bindings>, <cbindings>, and <atoms>. 
   Note that the cardinality of atoms do not increased but bindings may be extended and  
   cbindings are extended. If the conjunction is clashed, this function returns nil."
  (destructuring-bind (bindings cbindings . atoms) model
    (when (subsumed-p type (class-of+ successor bindings cbindings))
      (setq cbindings (extend-bindings successor type cbindings)))
    (setq bindings
          (extend-bindings successor (cons (skolem-constant successor) predecessor)  ; skolemize successor
                           bindings))
    (loop for atom in atoms with kbindings = +no-bindings+
        do 
          (multiple-value-setq (bindings cbindings kbindings)
            (unify `(,role ,predecessor ,successor) atom bindings cbindings))
        when bindings  ; satisfiable model
        collect (cons bindings (cons cbindings atoms)))))

(defun generate-subsumee-models (var role cmax cmin types alls exists fillers models)
  "generates updated models for <var> and <role> onto <models> with constraint 
   <cmax>, <cmin>, <types>, <alls>, <exists>, and <fillers> of <role>-successor."
  ;; fillers -> bindings
  (when fillers
    (let ((newvar (new-variable "gx"))
          (filler (car fillers)))
      (setq fillers (cdr fillers))
      (cond ((null models)
             (setq models (list (cons (extend-bindings newvar filler +no-bindings+)
                                      (cons +no-bindings+
                                            (cons `(,role ,var ,newvar) nil))))))
            (t (setq models
                     (loop for (bindings cbindings . atoms) in models
                         collect (cons (extend-bindings newvar filler bindings)
                                       (cons cbindings
                                             (cons `(,role ,var ,newvar) atoms)))))))
      (assert (null fillers))))
  (format t "~%Models:~S" models)
  ;; exists -> cbindings
  (when exists
    (setq models
          (generate-models-for-existential var role exists models)))
  (format t "~%Models:~S" models)
  ;; alls -> mapped to all models
  (when alls
    (cond ((null models)
           (let ((newvar (new-variable "gx"))
                 (all (first alls)))
             (setq alls (cdr alls))
             (setq models
                   (list (cons +no-bindings+
                               (cons (extend-bindings newvar all +no-bindings+)
                                     (cons `(,role ,var ,newvar) nil)))))))
          (cmin
           ;; then add infinite number possibility for universal type
           (setq models
                 (loop for model in models
                     as newvar = (new-variable "gx")
                     append (destructuring-bind (bindings cbindings . atoms) model
                              (when (< (length atoms) cmin) ; number of atoms in model is less than cmin
                                ;; then add the possibility of universal as inifinite number of atoms
                                (list (cons bindings
                                            (cons (extend-bindings newvar (first alls) cbindings)
                                                  (cons `(,role ,var ,newvar) atoms))))))))))
    ;; then ensure type constraint for universal for every model.
    (loop for (bindings cbindings . atoms) in models
        as kbindings = +no-bindings+
        as newvar = (new-variable "gx")
        when (loop for atom in atoms
                 always (every #'(lambda (all)
                                   (multiple-value-setq (bindings cbindings kbindings)
                                     (unify `(,role ,var ,newvar) atom
                                            bindings (extend-bindings newvar all cbindings)))
                                   (not (eq bindings +fail+))) ; not clashed
                               alls))
        collect (cons bindings (cons cbindings atoms))))
  (when types
    (cond ((null models)
           (let ((newvar (new-variable "gx"))
                 (type (first types)))
             (setq types (cdr types))
             (setq models
                   (list (cons +no-bindings+
                               (cons (extend-bindings newvar type +no-bindings+)
                                     (cons `(,role ,var ,newvar) nil)))))))
          (t (error "Not Yet!"))))
  ;;
  (format t "~%Models:~S" models)
  (reduce-models cmax models))

(defun reduce-models (cmax models)
  (loop for (bindings cbindings . atoms) in models
      as kbindings = +no-bindings+
      append (cond ((and cmax (> (length atoms) cmax))
                    ;; then reduce the possibilities
                    (reduce-atoms cmax bindings cbindings atoms))
                   (t (list (cons bindings (cons cbindings atoms)))))))

(defun reduce-atoms (cmax bindings cbindings atoms)
  (format t "~%cmax:~S~%bindings:~S~%cbindings:~S~%atoms:~S" cmax bindings cbindings atoms)
  (when (<= (length atoms) cmax)
    (return-from reduce-atoms (list (cons bindings (cons cbindings atoms)))))
  (setq models
        (loop for (one . others) in (mapcar #'(lambda (atom) (cons atom (remove atom atoms))) atoms)
            append (loop for i from 0 to (1- (length others))
                       append (multiple-value-bind (newbindings newcbindings newkbidnings)
                                   (unify one (nth i others) bindings cbindings)
                                 (when newbindings
                                   (list (cons newbindings (cons newcbindings others))))))))
  (setq models
        (remove-duplicates models
                           :test #'(lambda (model1 model2)
                                     (destructuring-bind (bindings1 cbindings1 . atoms1) model1
                                       (destructuring-bind (bindings2 cbindings2 . atoms2) model2
                                         (and (subsetp atoms1 atoms2
                                                       :test #'(lambda (atom1 atom2)
                                                                 (and (eq (first atom1) (first atom2))
                                                                      (eq (second atom1) (second atom2))
                                                                      (unify (third atom1) (third atom2) bindings2 cbindings2))))))))))
  (format t "~%reduced:~S" models)
  (reduce-models cmax models))

(defun generate-subsumer-models (var role cmax types alls exists fillers models)
  "generates updated models for <var> and <role> onto <models> with constraint 
   max caridinality <cmax>, <types>, <alls>, <exists>, and <fillers> of successor 
   of <var> for <role>. If <var> is nil, then new-var is created inside."
  ;; fillers -> bindings
  (when fillers
    (let ((newvar (new-variable "gx"))
          (filler (car fillers)))
      (setq fillers (cdr fillers))
      (cond ((null models)
             (setq models (list (cons (extend-bindings newvar filler +no-bindings+)
                                      (cons +no-bindings+
                                            (cons `(,role ,var ,newvar) nil))))))
            (t (setq models
                     (loop for (bindings cbindings . atoms) in models
                         collect (cons (extend-bindings newvar filler bindings)
                                       (cons cbindings
                                             (cons `(,role ,var ,newvar) atoms)))))))
      (assert (null fillers))))
  (format t "~%Models:~S" models)
  ;; exists -> cbindings
  (when exists
    (setq models
          (generate-models-for-existential var role exists models)))
  (format t "~%Models:~S" models)
  ;; alls -> mapped to all models
  (when alls
    (unless models
      (let ((newvar (new-variable "gx")))
        (setq models
              (list (cons +no-bindings+
                          (cons (extend-bindings newvar (first alls) +no-bindings+)
                                (cons `(,role ,var ,newvar) nil)))))
        (setq alls (cdr alls))))
    (format t "~%biingo:~S" models)
    (loop for (bindings cbindings . atoms) in models
        as kbindings = +no-bindings+
        as newvar = (new-variable "gx")
        when (loop for atom in (remove-if-not #'(lambda (a) (and (eq (first a) role) (eq (second a) var))) atoms)
                 always (every #'(lambda (all)
                                   (multiple-value-setq (bindings cbindings kbindings)
                                     (unify `(,role ,var ,newvar) atom
                                            bindings (extend-bindings newvar all cbindings)))
                                   (not (eq bindings +fail+)))
                               alls))
        collect (cons bindings (cons cbindings atoms))))
  (format t "~%Models:~S" models)
  models)

(defun generate-models-for-existential (var role exists models)
  "<exists> are a list of types for (role var y1:type1), (role var y2:type2) ..."
  (unless models
    (let ((newvar (new-variable "gx"))
          (exist (first exists)))
      (setq exists (cdr exists))
      (setq models
            (list (cons (extend-bindings newvar (cons (skolem-constant newvar) var) +no-bindings+)
                        (cons (extend-bindings newvar exist +no-bindings+)
                              (cons `(,role ,var ,newvar) nil)))))))
  (loop for exist in exists
      as newvar = (new-variable "gx")
      do (setq models
               (loop for model in models
                   append
                     (add-conjunct-existentially role var newvar exist model models))))
  models)

(defun %satisfy-model (cmodel dmax dmin dmodel)
  (destructuring-bind (dmodel-bindings dmodel-cbindings . dmodel-atoms) dmodel
    (destructuring-bind (cmodel-bindings cmodel-cbindings . cmodel-atoms) cmodel
      (every #'(lambda (datom)
                 (every #'(lambda (catom)
                            (format t "~&C:~S D:~S" catom datom)
                            (and (equal (car catom) (car datom))
                                 (equal (second catom) (second datom))
                                 (subsumed-p-successors 
                                  (third catom) cmodel-bindings cmodel-cbindings
                                  (third datom) dmodel-bindings dmodel-cbindings))
                            )
                        cmodel-atoms))
             dmodel-atoms))))

(defun subsumed-p-successors (y1 bindings1 cbindings1 y2 bindings2 cbindings2)
  (setq y1 (or (lookup y1 bindings1) y1))
  (setq y2 (or (lookup y2 bindings2) y2))
  (cond ((variable? y1)
         (cond ((variable? y2)
                (error "Not Yet1!"))
               ((skolem-p y2)
                (multiple-value-bind (pred2 suc2) (unskolemize y2)
                  (subsumed-p-in-satisfy (class-of+ y1 bindings1 cbindings1)
                                         (class-of+ suc2 bindings2 cbindings2))))
               ((rsc-object-p y2)
                (error "Not Yet3!"))
               (t (error "Cant happen!"))))
        ((skolem-p y1)
         (cond ((variable? y2)
                (multiple-value-bind (pred1 suc1) (unskolemize y1)
                  (subsumed-p-in-satisfy (class-of+ suc1 bindings1 cbindings1)
                                         (class-of+ y2 bindings2 cbindings2))))
               ((skolem-p y2)
                (multiple-value-bind (pred1 suc1) (unskolemize y1)
                  (multiple-value-bind (pred2 suc2) (unskolemize y2)
                    (and (eq pred1 pred2)
                         (subsumed-p-in-satisfy (class-of+ suc1 bindings1 cbindings1)
                                                (class-of+ suc2 bindings2 cbindings2))))))
               ((rsc-object-p y2)
                (error "Not Yet6!"))
               (t (error "Cant happen!"))))
        ((rsc-object-p y1)              ; filler
         (cond ((variable? y2)
                (typep-in-satisfy y1 (class-of+ y2 bindings2 cbindings2)))
               ((skolem-p y2)
                (error "Not Yet6!"))
               ((rsc-object-p y2) (owl-same-p-in-satisfy y1 y2)) ; filler
               (t (error "Cant happen!"))))
        (t (error "Cant happen!"))))

(defun subsumed-p-in-satisfy (x y)
  (subsumed-p x y))

(defun owl-same-p-in-satisfy (x y)
  (owl-same-p x y))

(defun typep-in-satisfy (x y)
  (typep x y))

;;;
;;;; Transitive Property
;;;

(defun transitive-property-p (obj)
  "Is this <obj> an instance of owl:TransitiveProperty?"
  ;;this is same as '(cl:typep <obj> owl:TransitiveProperty)'
  (and (excl::standard-instance-p obj)
       (let ((class (class-of obj)))
         (cond ((eq class (find-class 'owl:TransitiveProperty)))
               ((mop:class-finalized-p class)
                (and (member (find-class 'owl:TransitiveProperty)
                                (mop:class-precedence-list class)
                                :test #'eq)
                     t))
               ((labels ((walk-partial-cpl (c)
                                           (let ((supers (mop:class-direct-superclasses c)))
                                             (when (member
                                                    (find-class 'owl:TransitiveProperty)
                                                    supers :test #'eq)
                                               (return-from transitive-property-p t))
                                             (mapc #'walk-partial-cpl supers))))
                  (declare (dynamic-extent #'walk-partial-cpl))
                  (walk-partial-cpl class)
                  nil))))))

(defun transitive-property-subsumed-p (c d)
  (let ((props
         (intersection (append (collect-owl-role-name-if
                                #'transitive-property-p c)
                               (when (and (slot-exists-p c 'inverse-transitive)
                                          (slot-boundp c 'inverse-transitive))
                                 (with-slots (inverse-transitive) c
                                   (cond ((atom (car inverse-transitive))
                                          (list (car inverse-transitive)))
                                         (t (remove-duplicates
                                             (mapcar #'car inverse-transitive)))))))
                       (append (collect-owl-role-name-if
                                #'transitive-property-p d)
                               (when (and (slot-exists-p d 'inverse-transitive)
                                          (slot-boundp d 'inverse-transitive))
                                 (with-slots (inverse-transitive) d
                                   (cond ((atom (car inverse-transitive))
                                          (list (car inverse-transitive)))
                                         (t (remove-duplicates
                                             (mapcar #'car
                                               (slot-value d 'inverse-transitive)))))))))))
    (cond ((some #'(lambda (prop)
                     (some #'(lambda (dd)
                               (some #'(lambda (cc)
                                         (transitive-subp prop cc dd))
                                     (same-as-of c)))
                           (same-as-of d)))
                 props)
           (values t t))
          (t (values nil nil)))))

;;;
;;;
;;;

(defun %owl-subtypep (type1 type2)
  "same prodedure as %rdf-subtypep but calls subsumed-p instead-of %rdf-subtypep or 
%owl-subtypep. Conjuction is specially treated as same way of subsumed-p."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((and (atom type1) (atom type2))
         (cond ((and (rdf-class-p type1) (rdf-class-p type2))
                (cond ((%clos-subtype-p type1 type2) (values t t))
                      (t (values nil nil))))
               ((or (rdf-instance-p type1) (rdf-instance-p type2))
                (values nil t))
               (t (values nil nil))))
        ((and (atom type1) (consp type2))
         (case (op type2)
           ((not and or)
            (setq type2 (->nnf type2))
            (ecase (op type2)
              (and (cond ((intersection-subsumed-p type1 (args type2))
                          (values t t))
                         (t (values nil nil))))
              (or (loop for t2 in (args type2)
                      with known = t ; C < (A v B)  <=>  (C < A) v (C < B)
                      do (multiple-value-bind (val1 val2) (subsumed-p type1 t2)
                           ;; if true then immediately return with true
                           (when val1 (return-from %owl-subtypep (values t t)))
                           ;; else accumulate known, all false known (nil t) results false known.
                           (setq known (and known val2)))
                      finally ; pass false known/unknown
                        (return (values nil known))))
              (not (let ((ty2 (arg1 type2)))
                     (cond ((owl-equivalent-p type1 ty2) (values nil t)) ; x and (not x), disjoint
                           ((owl-equivalent-p type1 rdfs:Resource) (values t t))
                           ;;    type1 is included in t2, disjoint
                           ((subsumed-p type1 ty2) (values nil t)) 
                           ;;    (not ty2) = (not type1) U (type1 - ty2)
                           ((subsumed-p ty2 type1) (values nil t)) 
                           ((disjoint-p type1 ty2) (values t t))
                           (t (values nil nil)))))))
           ((forall exists fills) (error "Cant happen!"))
           (otherwise (%owl-subtypep type1 (cons 'and type2)))))
        ((and (consp type1) (atom type2))
         (case (op type1)
           ((not and or)
            (setq type1 (->nnf type1))
            (case (op type1)
              (and (%intersection1-subsumed-p var (args type1) type2))
              (or ;; => (and (subsumed-p t11 type2) (subsumed-p t12 type2) ...)
               (cond ((every #'(lambda (csub) (subsumed-p csub type2)) (args type1))
                      (values t t))
                     (t (values nil nil))))
              (not (let ((ty1 (arg1 type1)))
                     (cond ((owl-equivalent-p ty1 type2) (values nil t)) ; (not x) and x, disjoint
                           ((owl-equivalent-p type2 rdfs:Resource) (values t t))
                           ;;       (not ty1) = (not type2) U (type2 - ty1)
                           ((subsumed-p ty1 type2) (values nil t))
                           ((subsumed-p type2 ty1) (values nil t))  ; disjoint
                           ((disjoint-p ty1 type2) (values t t))
                           (t (values nil nil)))))
              (otherwise ; turned out atom by nnf
               (assert (atom type1))
               (subsumed-p type1 type2))))
           ((forall exists fills) (error "Cant happen!"))
           (otherwise (%owl-subtypep (cons 'and type1) type2))))
        ((and (consp type1) (consp type2))
         (case (op type1)
           ((not and or) (setq type1 (->nnf type1)))
           ((forall exists fills) (error "Cant happen!"))
           (otherwise (setq type1 (->nnf (cons 'and type1)))))
         (case (op type2)
           ((not and or) (setq type2 (->nnf type2)))
           ((forall exists fills) (error "Cant happen!"))
           (otherwise (setq type2 (->nnf (cons 'and type2)))))
         (cond ((and (eq (op type1) 'not) (eq (op type2) 'not))
                (subsumed-p (arg1 type2) (arg1 type1)))      ; ~A < ~B <=> B < A
               ;; C < (A ^ B)  <=>  (C < A) ^ (C < B)
               ((and (eq (op type1) 'not) (eq (op type2) 'and))
                ;; (subsumed-p type1 (and t1 t2 ...))
                (loop for t2 in (args type2)
                    do (multiple-value-bind (val1 val2) (subsumed-p type1 t2)
                         (when (null val1) (return-from %owl-subtypep (values nil val2)))))
                (values t t))
               ;; C < (A v B)  <=>  (C < A) v (C < B)
               ((and (eq (op type1) 'not) (eq (op type2) 'or))
                ;; (subsumed-p type1 (or t1 t2 ...))
                (loop for t2 in (args type2) with known = t
                    do (multiple-value-bind (val1 val2) (subsumed-p type1 t2)
                         (when val1 (return-from %owl-subtypep (values t t)))
                         (setq known (and known val2)))
                    finally ; pass false known/unknown
                      (return (values nil known))))
               ;;   (A v B) < C  <=>  (A < C) ^ (B < C)
               ((and (eq (car type1) 'or) (eq (car type2) 'not))
                ;; (subsumed-p (or t1 t2 ...) type2)
                (loop for t1 in (cdr type1)
                    do (multiple-value-bind (val1 val2) (subsumed-p t1 type2)
                         (when (null val1) (return-from %owl-subtypep (values nil val2)))))
                (values t t))
               ;;      (A ^ B) < C  <=>  (A < C) v (B < C)
               ((and (eq (car type1) 'and) (eq (car type2) 'not))
                ;; (subsumed-p (and t1 t2 ...) type2)
                (loop for t1 in (cdr type1) with known = t
                    do (multiple-value-bind (val1 val2) (subsumed-p t1 type2)
                         (when val1 (return-from %owl-subtypep (values t t)))
                         (setq known (and known val2)))
                    finally
                      (return (values nil known))))
               ;;
               ((and (eq (op type1) 'or) (eq (op type2) 'and))
                ;; (A v B) < (C ^ D) <=>  (A < C) ^ (A < D)  ^  (B < C) ^ (B < D) 
                (loop for t1 in (args type1)
                    do (loop for t2 in (args type2)
                           do (multiple-value-bind (val1 val2) (subsumed-p t1 t2)
                                (when (null val1)
                                  (return-from %owl-subtypep (values nil val2))))))
                (values t t))
               ((and (eq (op type1) 'and) (eq (op type2) 'or))
                ;; (A ^ B) < (C v D) <=>  (A < C) v (A < D)  v  (B < C) v (B < D) 
                (loop for t1 in (args type1) with known = t
                    do (loop for t2 in (args type2)
                           do (multiple-value-bind (val1 val2) (subsumed-p t1 t2)
                                (when val1 (return-from %owl-subtypep (values t t)))
                                (setq known (and known val2))))
                    finally
                      (return (values nil known))))
               ((and (eq (op type1) 'and) (eq (op type2) 'and))
                (%intersection12-subsumed-p (args type1) (args type2)))
               ((and (eq (car type1) 'or) (eq (car type2) 'or))
                (loop for t1 in (cdr type1)
                    do (multiple-value-bind (val1 val2) (subsumed-p t1 type2)
                         (when (null val1) (return-from %owl-subtypep (values nil val2)))))
                (values t t))
               ((error "Cant happen!"))))))
