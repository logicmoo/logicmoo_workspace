;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-user -*-
;;; File: overbeek-test.lisp
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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2008.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark-user)

(defun overbeek-test (&key (verbose t))
  #+Symbolics (zl:print-herald)
  (let ((p1 (default-print-rows-when-given?))
        (p2 (default-print-rows-when-derived?))
        (p3 (default-print-rows-prettily?))
        (p4 (default-print-final-rows?))
        (p5 (default-print-options-when-starting?))
        (p6 (default-print-assertion-analysis-notes?))
        (p7 (default-print-term-memory-when-finished?))
        (p8 (default-print-agenda-when-finished?)))
    (unwind-protect
      (let ((total-seconds 0.0))
        (dolist (x '(
	             ;; (print-rows-when-given print-rows-when-derived print-wffs-when-done problem-name)
	             (t   t       nil overbeek1)
	             (t   t       nil overbeek1e)
	             (t   t       nil overbeek3e)
	             (t   t       nil overbeek6)
	             (t   t       nil overbeek2e)
	             (t   :signal nil overbeek2)
	             (t   t       nil overbeek4e)
	             (t   t       nil overbeek3)
	             (t   t       nil overbeek7e)
	             (t   :signal nil overbeek7)
	             (t   :signal nil overbeek4)
	             (t   :signal nil overbeek5e)
	             (t   :signal nil overbeek6e)
	             (t   :signal nil overbeek5)
                     (t   :signal nil overbeek6-1)
                     (t   :signal nil overbeek4-1)
;;                   (t   t       nil overbeek5-1)
;;                   (t   t       nil overbeek7-1)
;;                   (t   t       nil overbeek7e-1)
                     ;;overbeek8e
                     ;;overbeek9e
                     ;;overbeek10e
	             ))
          (dotimes (i 3) (terpri))
          (let ((#-symbolics *break-on-signals* #+symbolics conditions::*break-on-signals* nil)
	        (snark::critique-options t))
            (default-print-rows-when-given (and verbose (first x)))
            (default-print-rows-when-derived (and verbose (second x)))
            (default-print-row-wffs-prettily nil)
            (unless verbose
              (default-print-final-rows nil)
              (default-print-options-when-starting nil)
              (default-print-assertion-analysis-notes nil)
              (default-print-term-memory-when-finished nil)
              (default-print-agenda-when-finished nil))
            (funcall (print (fourth x))))
          (incf total-seconds snark-lisp::*total-seconds*)
          (when (third x)
            (terpri)
            (print-rows :ancestry t))
          (prin1 (fourth x))
          (terpri))
        (format t "~%OVERBEEK-TEST Total = ~D seconds" (round total-seconds)))
      (default-print-rows-when-given p1)
      (default-print-rows-when-derived p2)
      (default-print-row-wffs-prettily p3)
      (default-print-final-rows p4)
      (default-print-options-when-starting p5)
      (default-print-assertion-analysis-notes p6)
      (default-print-term-memory-when-finished p7)
      (default-print-agenda-when-finished p8)
      nil)))

(defun refute-snark-example-file (name options &key format)
  (refute-file
   (make-pathname :directory (append (pathname-directory cl-user::*snark-system-pathname*) (list "examples"))
                  :name name
                  :type (case format (:tptp "tptp") (otherwise "kif")))
   :options options
   :format format
   :ignore-errors nil
   :verbose t
   :output-file nil
   :package :snark-user))

(defun overbeek1 ()
  (refute-snark-example-file 
   "GRP001-1+rm_eq_rstfp"
   '(;;(agenda-ordering-function #'fifo)
     ;;(row-weight-limit 4)			;4 is minimum value for which proof can be found
     (declare-constant 'e :alias 'identity)
     (declare-constant 'a)
     (declare-constant 'b)
     (declare-constant 'c)
     (declare-function 'f 2 :alias 'multiply :ordering-status :left-to-right)
     (declare-function 'g 1 :alias 'inverse :kbo-weight 0)
     (declare-relation 'p 3 :alias 'product)
     (ordering-functions>constants t)
     (declare-ordering-greaterp 'g 'f 'c 'b 'a 'e)
     (use-hyperresolution t)
     (use-term-ordering :kbo))))

(defun overbeek2 ()
  (refute-snark-example-file
   "GRP002-1+rm_eq_rstfp"
   '(;;(ROW-WEIGHT-LIMIT 9)
     (declare-constant 'e :alias 'identity)
     (declare-constant 'a)
     (declare-constant 'b)
     (declare-constant 'c)
     (declare-constant 'd)
     (declare-constant 'h)
     (declare-constant 'j)
     (declare-constant 'k)
     (declare-function 'f 2 :alias 'multiply)
     (declare-function 'g 1 :alias 'inverse :kbo-weight '(1 2))
     (declare-relation 'p 3 :alias 'product)
     (ordering-functions>constants t)
     (declare-ordering-greaterp 'g 'f 'k 'j 'h 'd 'c 'b 'a 'e)
     (use-hyperresolution t)
     (use-term-ordering :kbo))))

(defun overbeek3 ()
  (refute-snark-example-file
   "RNG008-6+rm_eq_rstfp"
   '(;;(agenda-ordering-function #'fifo)
     ;;(row-weight-limit 8)			;8 is minimum value for which proof can be found
     (declare-constant 'zero :alias 'additive_identity)
     (declare-constant 'a)
     (declare-constant 'b)
     (declare-constant 'c)
     (declare-function 'j 2 :alias 'add :ordering-status :left-to-right)
     (declare-function 'f 2 :alias 'multiply :ordering-status :left-to-right)
     (declare-function 'g 1 :alias 'additive_inverse :kbo-weight 0)
     (declare-relation 's 3 :alias 'sum)
     (declare-relation 'p 3 :alias 'product)
     (ordering-functions>constants t)
     (declare-ordering-greaterp 'g 'f 'j 'c 'b 'a 'zero)
     (use-hyperresolution t)
     (use-term-ordering :kbo))))

(defun overbeek4 ()
  (refute-snark-example-file
   "LCL024-1+rm_eq_rstfp"
   '((declare-relation 'p 1 :alias 'is_a_theorem)
     (declare-function 'e 2 :alias 'equivalent)
     (use-hyperresolution t))))

(defun overbeek5 ()
  (refute-snark-example-file
   "LCL038-1+rm_eq_rstfp"
   '((declare-relation 'p 1 :alias 'is_a_theorem)
     (declare-function 'i 2 :alias 'implies)
     (use-hyperresolution t))))

(defun overbeek6 ()
  (refute-snark-example-file
   "LCL111-1"
   '((declare-relation 'p 1 :alias '|is_a_theorem|)
     (declare-function 'i 2 :alias '|implies|)
     (declare-function 'n 1 :alias '|not|)
     ;;(agenda-ordering-function #'fifo)	;very fast with fifo ordering
     (use-hyperresolution t)
     (level-pref-for-giving 1))
   :format :tptp))

(defun overbeek7 ()
  (refute-snark-example-file
   "LCL114-1+rm_eq_rstfp"
   '((declare-relation 'p 1 :alias 'is_a_theorem)
     (declare-function 'i 2 :alias 'implies)
     (declare-function 'n 1 :alias 'not)
     (use-hyperresolution t)
     (level-pref-for-giving 1))))

(defun overbeek4-1 ()
  (refute-snark-example-file
   "LCL024-1+rm_eq_rstfp"
   '((declare-relation 'p 1 :alias 'is_a_theorem)
     (declare-function 'e 2 :alias 'equivalent)
     (use-resolution t)
     (use-literal-ordering-with-resolution 'literal-ordering-a))))

(defun overbeek5-1 ()
  (refute-snark-example-file
   "LCL038-1+rm_eq_rstfp"
   '((declare-relation 'p 1 :alias 'is_a_theorem)
     (declare-function 'i 2 :alias 'implies)
     (use-resolution t)
     (use-literal-ordering-with-resolution 'literal-ordering-a))))

(defun overbeek6-1 ()
  (refute-snark-example-file
   "LCL111-1"
   '((declare-relation 'p 1 :alias '|is_a_theorem|)
     (declare-function 'i 2 :alias '|implies|)
     (declare-function 'n 1 :alias '|not|)
     (use-resolution t)
     (assert-context :current)
     (use-literal-ordering-with-resolution 'literal-ordering-a)
     (level-pref-for-giving 1))
   :format :tptp))

(defun overbeek7-1 ()
  (refute-snark-example-file
   "LCL114-1+rm_eq_rstfp"
   '((declare-relation 'p 1 :alias 'is_a_theorem)
     (declare-function 'i 2 :alias 'implies)
     (declare-function 'n 1 :alias 'not)
     (use-resolution t)
     (use-literal-ordering-with-resolution 'literal-ordering-a)
     (level-pref-for-giving 1))))

(defun overbeek1e ()
  (refute-snark-example-file
   "GRP002-3+rm_eq_rstfp"
   '((declare-constant 'e :alias 'identity)
     (declare-constant 'a)
     (declare-constant 'b)
     (declare-function 'f 2 :alias 'multiply :ordering-status :left-to-right)
     (declare-function 'g 1 :alias 'inverse :kbo-weight '(1 2))
     (declare-function 'h 2 :alias 'commutator :kbo-weight '(5 3 3) :ordering-status :left-to-right)
     (ordering-functions>constants t)
     (declare-ordering-greaterp 'h 'g 'f 'b 'a 'e)
     (use-paramodulation t)
     (use-term-ordering :kbo))))

(defun overbeek2e ()
  (refute-snark-example-file
   "ROB005-1+rm_eq_rstfp"
   '((declare-constant 'a)
     (declare-constant 'b)
     (declare-constant 'c)
     (declare-function 'o 2 :alias 'add)
     (declare-function 'n 1 :alias 'negate)
     (ordering-functions>constants t)
     (declare-ordering-greaterp 'n 'o 'a 'b 'c)
     (use-paramodulation t))))

(defun overbeek3e ()
  (refute-snark-example-file
   "BOO002-1+rm_eq_rstfp"
   '(;;(agenda-ordering-function #'fifo)
     ;;(row-weight-limit 15)			;15 is minimum value for which proof can be found
     (declare-function 'f 3 :alias 'multiply :ORDERING-STATUS :RIGHT-TO-LEFT)
     (declare-function 'g 1 :alias 'inverse)
     (declare-constant 'a)
     (declare-constant 'b)
     (declare-ordering-greaterp 'b 'a 'g 'f)
     (use-paramodulation t)
     (use-term-ordering :kbo))))

(defun overbeek4e ()
  (refute-snark-example-file
   "GRP014-1+rm_eq_rstfp"
   '((declare-constant 'a)
     (declare-constant 'b)
     (declare-constant 'c)
     (declare-function 'f 2 :alias 'multiply :ordering-status :left-to-right)
     (declare-function 'i 1 :alias 'inverse :kbo-weight 0)
     (ordering-functions>constants t)
     (declare-ordering-greaterp 'i 'f 'c 'b 'a)
     (use-paramodulation t)
     (use-term-ordering :kbo)			;KBO better than RPO 4/20/92
     ;;(use-function-creation t)		;constant-creation only, insert new symbols into KB ordering
     )))

(defun overbeek5e ()
  (refute-snark-example-file
   "LCL109-2+rm_eq_rstfp"
   '(;;(ROW-WEIGHT-LIMIT 21)			;21 works, think 19 will too
     (declare-function 'i 2 :alias 'implies #| :ordering-status :left-to-right |#)
     (declare-function 'n 1 :alias 'not)
     (declare-constant 'a)
     (declare-constant 'b)
     (declare-constant 't :alias 'true0)
     (ordering-functions>constants t)
     (declare-ordering-greaterp 'i 'n 'a 'b 't)
     (use-paramodulation t))))

(defun overbeek6e ()
  (refute-snark-example-file
   "COL049-1+rm_eq_rstfp"
   '(;;(row-weight-limit 21)			;don't know what value works (19 doesn't)
     (declare-function 'a 2 :alias 'apply :ordering-status :left-to-right)
     (declare-function 'f 1 :weight-code (list (constantly 1)))
     (declare-constant 'b)
     (declare-constant 'm)
     (declare-constant 'w)
     (ordering-functions>constants t)
     (declare-ordering-greaterp 'a 'f 'b 'w 'm)
     (use-paramodulation t))))

(defun overbeek7e ()
  (refute-snark-example-file
   "RNG009-5+rm_eq_rstfp"
   '((row-weight-before-simplification-limit 100)
     (row-weight-limit 50)
     (declare-constant 'zero :alias 'additive_identity)
     (declare-function '* 2 :alias 'multiply :ordering-status :left-to-right)
     (declare-function '- 1 :alias 'additive_inverse)
     (declare-function '+ 2 :alias 'add)
     (ordering-functions>constants t)
     (declare-ordering-greaterp '* '- '+ 'zero)
     (DECLARE-CANCELLATION-LAW '= '+ 'zero)
     (use-paramodulation t))))

(defun overbeek7e-1 ()
  (refute-snark-example-file
   "RNG009-5+rm_eq_rstfp"
   '((row-weight-before-simplification-limit 100)
     (row-weight-limit 50)
     (declare-constant 'zero :alias 'additive_identity)
     (declare-function '* 2 :alias 'multiply :ordering-status :left-to-right)
     (declare-function '- 1 :alias 'additive_inverse)
     (declare-function '+ 2 :alias 'add)
     (ordering-functions>constants t)
     (declare-ordering-greaterp '* '- '+ 'zero)
     (DECLARE-CANCELLATION-LAW '= '+ 'zero)
     (use-paramodulation t)
     (use-associative-unification t))))

(defun overbeek8e ()
  (refute-snark-example-file
   "COL003-1+rm_eq_rstfp"
   '((declare-function 'a 2 :alias 'apply :ordering-status :left-to-right)
     (declare-function 'f 1 :weight-code (list (constantly 1)))
     (declare-constant 'b)
     (declare-constant 'w)
     (ordering-functions>constants t)
     (declare-ordering-greaterp 'a 'f 'b 'w)
     (use-paramodulation t))))

(defun overbeek9e ()
  (refute-snark-example-file
   "RNG010-5+rm_eq_rstfp"
   '((use-paramodulation t))))

(defun overbeek10e ()
  (refute-snark-example-file
   "RNG011-5+rm_eq_rstfp"
   '((use-paramodulation t))))

;;; overbeek-test.lisp EOF
