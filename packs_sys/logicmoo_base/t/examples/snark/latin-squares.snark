;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-user -*-
;;; File: latin-squares.lisp
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

(defun latin-square-clauses (order &key clause-set (standard t) &allow-other-keys)
  (let ((n-1 (- order 1)))
    ;; row, column, and values are numbered in [0,...,order-1]
    (unless clause-set
      (setf clause-set (make-dp-clause-set)))
    (dp-insert-wff `(forall ((i :in (ints 0 ,n-1)) (j :in (ints 0 ,n-1))) (exists ((k :in (ints 0 ,n-1))) (p i j k))) clause-set)
    (dp-insert-wff `(forall ((i :in (ints 0 ,n-1)) (k :in (ints 0 ,n-1))) (exists ((j :in (ints 0 ,n-1))) (p i j k))) clause-set)
    (dp-insert-wff `(forall ((j :in (ints 0 ,n-1)) (k :in (ints 0 ,n-1))) (exists ((i :in (ints 0 ,n-1))) (p i j k))) clause-set)
    (dp-insert-wff `(forall ((i :in (ints 0 ,n-1))
                             (j :in (ints 0 ,n-1))
                             (k :in (ints 1 ,n-1))
                             (l :in (ints 0 (- k 1))))
                            (and
                             (or (not (p i j l)) (not (p i j k)))
                             (or (not (p i l j)) (not (p i k j)))
                             (or (not (p l i j)) (not (p k i j)))))
                   clause-set)
    (when standard
      ;; fix first row and column for standard form
      (dp-insert-wff `(forall ((j :in (ints 0 ,n-1))) (p 0 j j)) clause-set)
      (dp-insert-wff `(forall ((i :in (ints 0 ,n-1))) (p i 0 i)) clause-set))
    clause-set))

(defun model-to-latin-square (atoms &optional order)
  ;; convert list of p atoms to sequence of sequences representation of latin square
  (unless order
    (let ((n 0))				;find its order
      (dolist (atom atoms)
        (when (and (consp atom) (eq 'p (first atom)))
          (dolist (k (rest atom))
            (when (> k n)
              (setf n k)))))
      (setf order (+ n 1))))
  (let ((ls (make-array order)))
    (dotimes (i order)
      (setf (aref ls i) (make-array order :initial-element nil)))
    (dolist (atom atoms)
      (when (and (consp atom) (eq 'p (first atom)))
        (let ((i (second atom))
              (j (third atom))
              (k (fourth atom)))
          (cl:assert (null (aref (aref ls i) j)))
          (setf (aref (aref ls i) j) k))))
    ls))

(defun generate-latin-squares (order &rest options &key (apply nil) (time t) &allow-other-keys)
  (let (clause-set)
    (flet ((make-clause-set ()
             (setf clause-set (apply #'latin-square-clauses order options)))
           (generate ()
             (dp-satisfiable-p clause-set
                               :find-all-models -1
                               :model-test-function (and apply (lambda (model) (funcall apply (model-to-latin-square model order)) t))
                               :trace-choices nil)))
      (if time (time (make-clause-set)) (make-clause-set))
      (if time (time (generate)) (generate)))))

(defun print-latin-square (ls)
  (map nil (lambda (row) (format t "~%") (map nil (lambda (v) (format t "~3@A" v)) row)) ls)
  ls)

(defun latin-square-conjugate (ls conjugate)
  (let* ((order (length ls))
         (ls* (make-array order)))
    (dotimes (i order)
      (setf (elt ls* i) (make-array order :initial-element nil)))
    (dotimes (i order)
      (dotimes (j order)
        (let ((k (elt (elt ls i) j)))
          (ecase conjugate
            (132
             (setf (elt (elt ls* i) k) j))
            (213
             (setf (elt (elt ls* j) i) k))
            (231
             (setf (elt (elt ls* j) k) i))
            ((312 column)
             (setf (elt (elt ls* k) i) j))
            ((321 row)
             (setf (elt (elt ls* k) j) i))
            (123				;makes copy of ls
             (setf (elt (elt ls* i) j) k))))))
    ls*))

(defun latin-square-standard-form (ls)
  (let* ((order (length ls))
         (ls* (make-array order)))
    (dotimes (i order)
      (setf (elt ls* i) (make-array order :initial-element nil)))
    ;; renumber entries so first row is 0,...,order-1
    (let ((row0 (elt ls 0)))
      (dotimes (i order)
        (let ((rowi (elt ls i))
              (rowi* (elt ls* i)))
          (dotimes (j order)
            (setf (elt rowi* j) (position (elt rowi j) row0))))))
    ;; sort rows so that first column is 0,...,order-1
    (sort ls* #'< :key (lambda (x) (elt x 0)))))

;;; latin-squares.lisp EOF
