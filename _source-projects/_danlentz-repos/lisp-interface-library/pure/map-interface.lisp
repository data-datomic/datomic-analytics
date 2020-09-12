;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Functional mapping of keys to values

#+xcvb (module (:depends-on ("interface/map-interface" "pure/iterator-interface")))

(in-package :pure)

(define-interface <empty!able> (<emptyable>) ()
  (:generic> empty! (ignored) (:in 1)
    (:values empty) (:out 0)
    (:method> (ignored)
       (declare (ignorable ignored))
       (empty))
    (:documentation
     "This function is pretty useless to call, but allows for
      automatic generation of mutating interface wrappers.")))

(define-interface <map> (interface::<map> <empty!able> <fount> <sink>) ()
  (:abstract)
  (:generic> insert (map key value) (:in 1) (:values map) (:out 0)
   (:documentation "Add a key-value pair to a map,
replacing any previous association for this key,
return a new map."))
  (:generic> drop (map key) (:in 1) (:values map value foundp) (:out 0)
   (:documentation "Drop from a map the association corresponding to given key,
returning three values:
a new map without that association,
the value from the dropped association,
and a boolean that is true iff an association was found."))
  (:generic> decons (map) (:in 1) (:values emptyp map key value) (:out 1)
   (:documentation "Drop an association from a map,
returning four values:
1- a boolean indicating whether the map was already empty.
2- a new map
3- a key
4- a value.
Which association is dropped is the same as per first-key-value."))
  (:generic> join (map1 map2) (:in 1 2) (:values map1) (:out 0 nil)
   (:documentation "Join two maps, returning a new joined map.
Mappings from MAP1 override those from MAP2."))
  (:generic> divide (map) (:in 1) (:values map2 map) (:out 1 0)
   (:documentation "Divide a map in two,
returning two maps MAP1 and MAP2 that each have strictly
fewer associations than MAP unless MAP is of size zero or one,
at which point MAP2 is empty."))
  (:generic> join/list (list) #|(:in #|((1 list))|#) (:values map) (:out 0)|#
   (:documentation "Join a list of maps,
returning a new joined map where mappings from
earlier mappings override those from latter mappings."))
  (:generic> divide/list (map) #|(:in 1) (:values list) (:out t #|((0 list))|#)|#
   (:documentation "Divide a map in a list of several submaps and return that list,
such that merging those maps with join/list
will return a map similar to the original one,
that the returned list is empty iff the initial map is empty,
that the returned list is of length one iff the initial map is a singleton,
and that otherwise, each element of the list is non-empty."))
  (:generic> update-key (map key fun) (:in 1) (:values map) (:out 0)
   (:documentation "Update the association of a map for a given key and
return a new updated map
calling fun with the previous associated value and T if found, with NIL and NIL otherwise,
where fun will return two values,
the new value and a boolean,
the association being dropped if the boolean is NIL,
otherwise a new association being setup with the new value."))
  (:generic> map/2 (fun map1 map2) (:in 2 3) (:values map) (:out 0 nil)
   (:documentation "Join two maps, returning a joined map.
For each key K present in either MAP1 or MAP2,
the function FUN is called with arguments K V1 F1 V2 F2 where
V1 and F1 are the value and found flag for MAP1, and
V2 and F2 are the value and found flag for MAP2,
and FUN returns value V and found flag F,
that correspond the lookup for K in the result.")))

#|
Instead of divide and divide/list and in the spirit of fold-left and fold-right,
we could have a
(defgeneric monoid-fold (i map m-null m-singleton m-join m-join/list))
|#

;;; Mixins implementing simple cases for a lot of the above functions
(define-interface <map-decons-from-first-key-value-drop> (<map>) () (:abstract))
(define-interface <map-divide/list-from-divide> (<map>) () (:abstract))
(define-interface <map-empty-is-nil> (<map> <empty-is-nil>) () (:abstract))
(define-interface <map-join-from-fold-left-insert> (<map>) () (:abstract))
(define-interface <map-join/list-from-join> (<map>) () (:abstract))
(define-interface <map-map/2-from-fold-left-lookup-insert-drop> (<map>) () (:abstract))
(define-interface <map-size<=n-p-from-decons> (<map>) () (:abstract))
(define-interface <map-update-key-from-lookup-insert-drop> (<map>) () (:abstract))
(define-interface <map-divide/list-from-divide> (<map>) () (:abstract))
