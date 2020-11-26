;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-user -*-
;;; File: ramsey-examples.lisp
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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2006.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark-user)

;;; see http://mathworld.wolfram.com/RamseyNumber.html
;;; for Ramsey Number definition and results
;;;
;;; r( 3, 3) =  6  done
;;; r( 3, 4) =  9  done
;;; r( 3, 5) = 14  done
;;; r( 3, 6) = 18
;;; r( 3, 7) = 23
;;; r( 3, 8) = 28
;;; r( 3, 9) = 36
;;; r( 3,10) in [40,43]
;;; r( 4, 4) = 18
;;; r( 4, 5) = 25
;;; r( 4, 6) in [35,41]
;;; r( 5, 5) in [43,49]
;;; r( 6, 6) in [102,165]

(defun ramsey-3-3 (n)
  ;; results: found to be satisfiable for n=5, unsatisfiable for n=6 (should be unsatisfiable iff n>=6)
  (let ((clause-set (make-dp-clause-set)))
    (no-clique-of-order-3 n clause-set)
    (no-independent-set-of-order-3 n clause-set)
    (dp-satisfiable-p clause-set :atom-choice-function #'choose-an-atom-of-a-shortest-clause)))

(defun ramsey-3-4 (n)
  ;; results: found to be satisfiable for n=8, unsatisfiable for n=9 (should be unsatisfiable iff n>=9)
  (let ((clause-set (make-dp-clause-set)))
    (no-clique-of-order-3 n clause-set)
    (no-independent-set-of-order-4 n clause-set)
    (dp-satisfiable-p clause-set :atom-choice-function #'choose-an-atom-of-a-shortest-clause)))

(defun ramsey-3-5 (n)
  ;; results: found to be satisfiable for n=13, unsatisfiable for n=14 (should be unsatisfiable iff n>=14)
  (let ((clause-set (make-dp-clause-set)))
    (no-clique-of-order-3 n clause-set)
    (no-independent-set-of-order-5 n clause-set)
    (dp-satisfiable-p clause-set :atom-choice-function #'choose-an-atom-of-a-shortest-clause)))

(defun ramsey-3-6 (n)
  ;; results: found to be satisfiable for n=17, unsatisfiable for n=?? (should be unsatisfiable iff n>=18)
  (let ((clause-set (make-dp-clause-set)))
    (no-clique-of-order-3 n clause-set)
    (no-independent-set-of-order-6 n clause-set)
    (dp-satisfiable-p clause-set :atom-choice-function #'choose-an-atom-of-a-shortest-clause)))

(defun ramsey-4-4 (n)
  ;; results: found to be satisfiable for n=17, unsatisfiable for n=?? (should be unsatisfiable iff n>=18)
  (let ((clause-set (make-dp-clause-set)))
    (no-clique-of-order-4 n clause-set)
    (no-independent-set-of-order-4 n clause-set)
    (dp-satisfiable-p clause-set :atom-choice-function #'choose-an-atom-of-a-shortest-clause)))

(defun ramsey-4-5 (n)
  ;; results: found to be satisfiable for n=23, unsatisfiable for n=?? (should be unsatisfiable iff n>=25)
  (let ((clause-set (make-dp-clause-set)))
    (no-clique-of-order-4 n clause-set)
    (no-independent-set-of-order-5 n clause-set)
    (dp-satisfiable-p clause-set :atom-choice-function #'choose-an-atom-of-a-shortest-clause-WITH-MOST-OCCURRENCES-RANDOMLY)))

(defun ramsey-4-6 (n)
  ;; results: found to be satisfiable for n=29, unsatisfiable for n=??
  (let ((clause-set (make-dp-clause-set)))
    (no-clique-of-order-4 n clause-set)
    (no-independent-set-of-order-6 n clause-set)
    (dp-satisfiable-p clause-set :atom-choice-function #'choose-an-atom-of-a-shortest-clause-WITH-MOST-OCCURRENCES-RANDOMLY)))

(defun no-clique-of-order-3 (nnodes clause-set)
  ;; in every 3 node subset, at least one pair is not connected
  (dp-insert-wff `(forall ((i :in (ints 1 ,nnodes))
                           (j :in (ints i ,nnodes) :except i)
                           (k :in (ints j ,nnodes) :except j))
                          (or (not (c i j)) (not (c i k)) (not (c j k))))
                 clause-set))

(defun no-clique-of-order-4 (nnodes clause-set)
  ;; in every 4 node subset, at least one pair is not connected
  (dp-insert-wff `(forall ((i :in (ints 1 ,nnodes))
                           (j :in (ints i ,nnodes) :except i)
                           (k :in (ints j ,nnodes) :except j)
                           (l :in (ints k ,nnodes) :except k))
                          (or (not (c i j)) (not (c i k)) (not (c i l)) (not (c j k)) (not (c j l)) (not (c k l))))
                 clause-set))

(defun no-clique-of-order-5 (nnodes clause-set)
  ;; in every 5 node subset, at least one pair is not connected
  (dp-insert-wff `(forall ((i :in (ints 1 ,nnodes))
                           (j :in (ints i ,nnodes) :except i)
                           (k :in (ints j ,nnodes) :except j)
                           (l :in (ints k ,nnodes) :except k)
                           (m :in (ints l ,nnodes) :except l))
                          (or (not (c i j)) (not (c i k)) (not (c i l)) (not (c i m))
                              (not (c j k)) (not (c j l)) (not (c j m))
                              (not (c k l)) (not (c k m))
                              (not (c l m))))
                 clause-set))

(defun no-clique-of-order-6 (nnodes clause-set)
  ;; in every 6 node subset, at least one pair is not connected
  (dp-insert-wff `(forall ((i :in (ints 1 ,nnodes))
                           (j :in (ints i ,nnodes) :except i)
                           (k :in (ints j ,nnodes) :except j)
                           (l :in (ints k ,nnodes) :except k)
                           (m :in (ints l ,nnodes) :except l)
                           (n :in (ints m ,nnodes) :except m))
                          (or (not (c i j)) (not (c i k)) (not (c i l)) (not (c i m)) (not (c i n))
                              (not (c j k)) (not (c j l)) (not (c j m)) (not (c j n))
                              (not (c k l)) (not (c k m)) (not (c k n))
                              (not (c l m)) (not (c l n))
                              (not (c m n))))
                 clause-set))

(defun no-independent-set-of-order-3 (nnodes clause-set)
  ;; in every 3 node subset, at least one pair is connected
  (dp-insert-wff `(forall ((i :in (ints 1 ,nnodes))
                           (j :in (ints i ,nnodes) :except i)
                           (k :in (ints j ,nnodes) :except j))
                          (or (c i j) (c i k) (c j k)))
                 clause-set))

(defun no-independent-set-of-order-4 (nnodes clause-set)
  ;; in every 4 node subset, at least one pair is connected
  (dp-insert-wff `(forall ((i :in (ints 1 ,nnodes))
                           (j :in (ints i ,nnodes) :except i)
                           (k :in (ints j ,nnodes) :except j)
                           (l :in (ints k ,nnodes) :except k))
                          (or (c i j) (c i k) (c i l) (c j k) (c j l) (c k l)))
                 clause-set))

(defun no-independent-set-of-order-5 (nnodes clause-set)
  ;; in every 5 node-subset, at least one pair is connected
  (dp-insert-wff `(forall ((i :in (ints 1 ,nnodes))
                           (j :in (ints i ,nnodes) :except i)
                           (k :in (ints j ,nnodes) :except j)
                           (l :in (ints k ,nnodes) :except k)
                           (m :in (ints l ,nnodes) :except l))
                          (or (c i j) (c i k) (c i l) (c i m)
                              (c j k) (c j l) (c j m)
                              (c k l) (c k m)
                              (c l m)))
                 clause-set))

(defun no-independent-set-of-order-6 (nnodes clause-set)
  ;; in every 6 node-subset, at least one pair is connected
  (dp-insert-wff `(forall ((i :in (ints 1 ,nnodes))
                           (j :in (ints i ,nnodes) :except i)
                           (k :in (ints j ,nnodes) :except j)
                           (l :in (ints k ,nnodes) :except k)
                           (m :in (ints l ,nnodes) :except l)
                           (n :in (ints m ,nnodes) :except m))
                          (or (c i j) (c i k) (c i l) (c i m) (c i n)
                              (c j k) (c j l) (c j m) (c j n)
                              (c k l) (c k m) (c k n)
                              (c l m) (c l n)
                              (c m n)))
                 clause-set))

(defun ramsey-test ()
  ;; there doesn't seem to be any difference in search space size between choose-an-atom-of-a-shortest-clause and choose-an-atom-of-a-shortest-clause-randomly
  ;; choose-an-atom-of-a-shortest-clause-with-most-occurrences-randomly seems to work much better for satisfiable instances
  (cl:assert (eval (print '(ramsey-3-3 5))))		;2 branches
  (cl:assert (not (eval (print '(ramsey-3-3 6)))))	;22 branches
  (cl:assert (eval (print '(ramsey-3-4 8))))		;4 branches
  (cl:assert (not (eval (print '(ramsey-3-4 9)))))	;10,251 branches
  (cl:assert (eval (print '(ramsey-3-5 13))))		;93,125 branches
;;(cl:assert (not (eval (print '(ramsey-3-5 14)))))	;1,078,238,816 branches
;;(cl:assert (eval (print '(ramsey-4-4 17))))		;56,181,666 branches
;;(cl:assert (not (eval (print '(ramsey-4-4 18)))))
  )

;;; ramsey-examples.lisp EOF
