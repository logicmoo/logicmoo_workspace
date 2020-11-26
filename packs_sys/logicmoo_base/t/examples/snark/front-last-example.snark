;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-user -*-
;;; File: front-last-example.lisp
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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2002.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark-user)

;;; Let L be a nonempty list.
;;; Synthesize a program to compute the FRONT and LAST
;;; of the list where LAST of a list is its last element
;;; and FRONT is the list of all elements except the last.
;;;
;;; The program specification is
;;;   (EXISTS (Y Z) (= L (APPEND Y (CONS Z NIL))))
;;; i.e., find Y and Z such that L can be formed by
;;; appending Y (the FRONT of L) and a single element list
;;; containing Z (the LAST of L).
;;;
;;; The appropriate inductive axiom is given explicitly in the axiom
;;; named induction.
;;;
;;; Necessary properties of APPEND, CONS, HEAD, and TAIL are given
;;; in the axioms named append-nil, append-cons, and cons-definition.
;;;
;;; A proof of the query is found and the program
;;; defined by the values found for variables Y and Z
;;; in the specification.
;;;
;;; Resolution and paramodulation (for equality) are the inference
;;; rules used.

(defun front-last-example ()
  ;; Waldinger program synthesis example 1989-12-14
  (initialize)
  (use-resolution)
  (use-paramodulation)
  (use-literal-ordering-with-resolution     'literal-ordering-p)
  (use-literal-ordering-with-paramodulation 'literal-ordering-p)
  (use-conditional-answer-creation)
  (declare-constant 'nil)
  (declare-constant 'l)    
  (declare-function 'head   1)
  (declare-function 'tail   1)
  (declare-function 'cons   2)
  (declare-function 'append 2)
  (declare-function 'front  1)
  (declare-function 'last   1)
  (declare-ordering-greaterp 'l 'nil)
  (declare-ordering-greaterp 'head 'l)
  (declare-ordering-greaterp 'tail 'l)
  (declare-ordering-greaterp 'cons 'head)
  (declare-ordering-greaterp 'cons 'tail)
  (declare-ordering-greaterp 'append 'cons)
;;(assert '(forall (x) (= x x)))
  (assert '(/= l nil)
	  :name 'l-nonempty)
  (assert '(implies (and (/= l nil) (/= (tail l) nil))
                    (= (tail l) (append (front (tail l)) (cons (last (tail l)) nil))))
	  :name 'induction)
  (assert '(forall (u) (= (append nil u) u))
	  :name 'append-nil)
  (assert '(forall (u v w) (= (append (cons u v) w) (cons u (append v w))))
	  :name 'append-cons)
  (assert '(forall (x) (implied-by (= x (cons (head x) (tail x))) (/= x nil)))
	  :name 'cons-definition)
  (prove '(= l (append ?y (cons ?z nil))) :answer '(values ?y ?z)))

;;; front-last-example.lisp EOF
