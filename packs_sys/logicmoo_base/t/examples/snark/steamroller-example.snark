;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-user -*-
;;; File: steamroller-example.lisp
;;; The contents of this file are subject to the Mozilla Public License
;;; Version 1.1 (the "License"); you may not use this file except in
;;; compliance with the License. You may obtain a copy of the License at
;;; http://www.mozilla.org/MPL/
;;;
;;; Software distributed under the License is distributed on an "AS IS"
;;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
;;; License for the specific language governing rights and limitations
;;; under the License.
;;;
;;; The Original Code is SNARK.
;;; The Initial Developer of the Original Code is SRI International.
;;; Portions created by the Initial Developer are Copyright (C) 1981-2011.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark-user)

(defun steamroller-example0 ()
  (refute-snark-example-file
   "PUZ031+1"
   '((use-hyperresolution))))

(defun steamroller-example ()
  (initialize)
  (use-hyperresolution)
  
  (declare-sort 'animal :subsorts-incompatible t)
  (declare-sort 'plant :subsorts-incompatible t)
  (declare-subsort 'bird 'animal)
  (declare-subsort 'caterpillar 'animal)
  (declare-subsort 'fox 'animal)
  (declare-subsort 'snail 'animal)
  (declare-subsort 'wolf 'animal)
  (declare-subsort 'grain 'plant)
  
  (declare-relation 'e 2 :sort '((1 animal)))	;animal*true
  (declare-relation 'm 2 :sort '((t animal)))	;animal*animal
  
  (declare-variable '?a1 :sort 'animal)
  (declare-variable '?a2 :sort 'animal)

  (assertion (forall ((?s1 snail) (?b1 bird))		;KIF-style sort specification
                     (m ?s1 ?b1))			;all KIF variables begin with ?
             :name snails-are-smaller-than-birds)
  (assertion (forall ((b1 :sort bird) (f1 :sort fox))	;SNARK-preferred sort specification
                     (m b1 f1))
             :name birds-are-smaller-than-foxes)
  (assertion (forall ((f1 true :sort fox)
                      (w1 wolf :sort wolf))		;this works too
                     (m f1 w1))
             :name foxes-are-smaller-than-wolves)
  (assertion (forall ((w1 wolf) (f1 fox))
                     (not (e w1 f1)))
             :name wolves-dont-eat-foxes)
  (assertion (forall ((w1 :sort wolf) (g1 :sort grain))
                     (not (e w1 g1)))
             :name wolves-dont-eat-grain)
  (assertion (forall ((b1 :sort bird) (c1 :sort caterpillar))
                     (e b1 c1))
             :name birds-eat-caterpillars)
  (assertion (forall ((b1 :sort bird) (s1 :sort snail))
                     (not (e b1 s1)))
             :name birds-dont-eat-snails)
  (assertion (forall ((c1 :sort caterpillar))
                     (exists ((p1 :sort plant))
                             (e c1 p1)))
             :name caterpillars-eat-some-plants)
  (assertion (forall ((s1 :sort snail))
                     (exists ((p1 :sort plant))
                             (e s1 p1)))
             :name snails-eat-some-plants)
  (assertion (forall ((p1 :sort plant) (p2 :sort plant))
                     (implied-by (or (e ?a1 ?a2) (e ?a1 p1)) (and (m ?a2 ?a1) (e ?a2 p2)))))
  
  (prove '(and (e ?x.animal ?y.animal) (e ?y.animal ?z.grain))
         :answer '(values ?x.animal ?y.animal ?z.grain)))

;;; steamroller-example.lisp EOF
