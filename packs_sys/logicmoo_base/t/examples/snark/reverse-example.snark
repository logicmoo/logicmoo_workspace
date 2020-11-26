;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-user -*-
;;; File: reverse-example.lisp
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

(defun reverse-example (&key (length 3) magic)
  (let ((l nil))
    (dotimes (i length)
      (push i l))
    (initialize)
    (declare-function '$$cons 2 :new-name 'cons)
    (declare-function '$$list :any :new-name 'list)
    (declare-function '$$list* :any :new-name 'list*)
    (cond
     (magic
      (use-hyperresolution t)
      (use-magic-transformation t))
     (t
      (use-resolution t)
      (assert-supported nil)
      (assert-sequential t)
      (print-rows-shortened t)))
    (assert '(reverse nil nil))
    (assert '(implied-by
	      (reverse (cons ?x ?l) ?l1)
	      (and
	       (reverse ?l ?l2)
	       (append ?l2 (cons ?x nil) ?l1))))
    (assert '(append nil ?l ?l))
    (assert '(implied-by
	      (append (cons ?x ?l1) ?l2 (cons ?x ?l3))
	      (append ?l1 ?l2 ?l3)))
    (prove `(reverse (list ,@l) ?l) :answer '(values ?l))))

;;; reverse-example.lisp EOF
