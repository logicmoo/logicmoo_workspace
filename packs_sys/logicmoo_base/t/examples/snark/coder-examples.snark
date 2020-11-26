;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-user -*-
;;; File: coder-examples.lisp
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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2004.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark-user)

(defun coder-test ()
  (time (coder-overbeek6))
  (time (coder-ycl-rst))
  (time (coder-ycl-rst-together))
  (time (coder-veroff-5-2))
  (time (coder-veroff-4-1 :all-proofs t))
  (time (coder-ex7b))
  (time (coder-ex9 :max-syms 18 :max-vars 2))
  nil)

(defun coder-xcb-reflex (&rest options)
  ;; 10-step proof
  ;; 11-step proof by (coder-xcb-reflex :max-syms 35)
  ;; 13-step proof by (coder-xcb-reflex :max-syms 31)
  (apply
   'coder
   '((e ?x (e (e (e ?x ?y) (e ?z ?y)) ?z)))
   '(e a a)
   options))

(defun coder-overbeek6 (&rest options)
  ;; 5-step proof
  (apply
   'coder
   '("i(a,i(b,a))"				;Prolog style with declared variables
     "i(i(X,Y),i(i(Y,?z),i(X,?z)))"		;Prolog style with explicit variables (capitalized-or ?-prefix)
     (i (i (i a b) b) (i (i b a) a))		;Lisp style with declared variables
     (i (i (n ?x) (n ?y)) (i ?y ?x)))		;Lisp style with explicit variables
   "i(i(a,b),i(i(c,a),i(c,b)))"			;variable declarations don't apply to target
   :variables '(a b c)
   options))

(defun coder-overbeek4 (&rest options)
  ;; 10-step proof
  (apply
   'coder
   '((e ?x (e (e ?y (e ?z ?x)) (e ?z ?y))))
   '(e (e (e a (e b c)) c) (e b a))
   options))

(defun coder-ycl-rst (&rest options)
  ;; prove reflexivity (4-step proof),
  ;; symmetry (5-step proof),
  ;; and transitivity (6-step proof) from ycl
  ;; coder searches until all have been found
  (apply
   'coder
   '((e (e ?x ?y) (e (e ?z ?y) (e ?x ?z))))
   '(and
     (e a a)
     (e (e a b) (e b a))
     (e (e a b) (e (e b c) (e a c))))
   options))

(defun coder-ycl-rst-together (&rest options)
  ;; prove reflexivity, symmetry, and transitivity from ycl in a single derivation
  ;; 9-step proof
  (apply
   'coder
   '((e (e ?x ?y) (e (e ?z ?y) (e ?x ?z))))
   '(together
     (e a a)
     (e (e a b) (e b a))
     (e (e a b) (e (e b c) (e a c))))
   options))

(defun coder-veroff-5-2 (&rest options)
  ;; problem from
  ;; Robert Veroff, "Finding Shortest Proofs: An Application of Linked Inference Rules",
  ;; JAR 27,2 (August 2001), 123-129
  ;; 8-step proof
  (apply
   'coder
   '((i ?x (i ?y ?x))
     (i (i ?x (i ?y ?z)) (i (i ?x ?y) (i ?x ?z))))
   '(i (i a (i b c)) (i b (i a c)))
   options))

(defun coder-veroff-4-1 (&rest options)
  ;; converse (because there's a typo) of problem from
  ;; Robert Veroff, "Finding Shortest Proofs: An Application of Linked Inference Rules",
  ;; JAR 27,2 (August 2001), 123-129
  ;; 7 6-step proofs, just like Veroff reported
  (apply
   'coder
   '((i (i (i ?v1 ?v2) ?v3) (i (i ?v2 (i ?v3 ?v5)) (i ?v4 (i ?v2 ?v5)))))
   '(i (i v2 (i v3 v5)) (i (i (i v1 v2) v3) (i v4 (i v2 v5))))
   options))

(defun ii-schema ()
  '(i ?x (i ?y ?x)))

(defun id-schema ()
  '(i (i ?x (i ?y ?z)) (i (i ?x ?y) (i ?x ?z))))

(defun cr-schema1 ()
  '(i (i ?x (n ?y)) (i (i ?x ?y) (n ?x))))

(defun cr-schema2 ()
  '(i (i (n ?x) (n ?y)) (i (i (n ?x) ?y) ?x)))

(defun eq-schema1 ()
  '(i (e ?x ?y) (i ?x ?y)))

(defun eq-schema2 ()
  '(i (e ?x ?y) (i ?y ?x)))

(defun eq-schema3 ()
  '(i (i ?x ?y) (i (i ?y ?x) (e ?y ?x))))

(defun or-schema ()
  '(e (o ?x ?y) (i (n ?x) ?y)))

(defun and-schema ()
  '(e (a ?x ?y) (n (o (n ?x) (n ?y)))))

(defun alt-and-schema ()
  '(e (a ?x ?y) (n (i ?x (n ?y)))))

(defun coder-ex1 (&rest options)
  ;; from Genesereth chapter 4
  ;; 3-step proof
  (apply
   'coder
   (list (ii-schema)
         (id-schema)
         '(i p q)
         '(i q r))
   '(i p r)
   options))

(defun coder-ex2 (&rest options)
  ;; from Genesereth chapter 4 exercise
  ;; 6-step proof
  (apply
   'coder
   (list (ii-schema)
         (id-schema)
         (cr-schema1)
         (cr-schema2)
         '(i p q)
         '(i q r))
   '(i (i p (n r)) (n p))
   options))

(defun coder-ex3 (&rest options)
  ;; from Genesereth chapter 4 exercise
  ;; 5-step proof
  (apply
   'coder
   (list (ii-schema)
         (id-schema)
         (cr-schema1)
         (cr-schema2)
         '(n (n p)))
   'p
   options))

(defun coder-ex4 (&rest options)
  ;; 5-step proof
  (apply
   'coder
   (list (ii-schema)
         (id-schema)
         (cr-schema1)
         (cr-schema2)
         'p)
   '(n (n p))
   options))

(defun coder-ex5 (&rest options)
  ;; 4-step proof
  (apply
   'coder
   (list (ii-schema)
         (id-schema)
         (cr-schema1)
         (cr-schema2)
         (eq-schema1)
         (eq-schema2)
         (eq-schema3))
   '(e p p)
   options))

(defun coder-ex6 (&rest options)
  ;; 4-step proof
  (apply
   'coder
   (list (ii-schema)
         (id-schema)
         (cr-schema1)
         (cr-schema2)
         (eq-schema1)
         (eq-schema2)
         (eq-schema3)
         '(e p q))
   '(e q p)
   options))

(defun coder-ex6a (&rest options)
  ;; 5-step proof
  (apply
   'coder
   (list (ii-schema)
         (id-schema)
         (cr-schema1)
         (cr-schema2)
         (eq-schema1)
         (eq-schema2)
         (eq-schema3))
   '(i (e p q) (e q p))
   options))

(defun coder-ex6b (&rest options)
  ;; 7-step proof
  (apply
   'coder
   (list (ii-schema)
         (id-schema)
         (cr-schema1)
         (cr-schema2)
         (eq-schema1)
         (eq-schema2)
         (eq-schema3))
   '(e (e p q) (e q p))
   options))

(defun coder-ex7a ()
  ;; 5-step proof, 5-step proof, 2-step proof
  (coder (list (ii-schema)
               (id-schema)
               (eq-schema1)
               (eq-schema2)
               (eq-schema3)
               '(e p q)
               '(e q r))
         '(i p r)
         :must-use '(6 7))
  (coder (list (ii-schema)
               (id-schema)
               (eq-schema1)
               (eq-schema2)
               (eq-schema3)
               '(e p q)
               '(e q r))
         '(i r p)
         :must-use '(6 7))
  (coder (list (ii-schema)
               (id-schema)
               (eq-schema1)
               (eq-schema2)
               (eq-schema3)
               '(i p r)
               '(i r p))
         '(e p r)
         :must-use '(6 7)))

(defun coder-ex7b ()
  ;; 12-step proof
  (coder (list (ii-schema)
               (id-schema)
               (eq-schema1)
               (eq-schema2)
               (eq-schema3)
               '(e p q)
               '(e q r))
         '(together (e p r) (i p q) (i q r) (i p r) (i r q) (i q p) (i r p))
         :must-use t
         :max-syms 7))

(defun coder-ex8 (&rest options)
  ;; 3-step proof
  (apply
   'coder
   (list (ii-schema)
         (id-schema)
         (cr-schema1)
         (cr-schema2)
         (eq-schema1)
         (eq-schema2)
         (eq-schema3)
         (or-schema)
         'q)
   '(o p q)
   options))

(defun coder-ex9 (&rest options)
  ;; no 1,...,8-step proof
  ;; 9-step proof by (coder-ex9 :max-syms 18 :max-vars 2)
  (apply
   'coder
   (list (ii-schema)
         (id-schema)
         (cr-schema1)
         (cr-schema2)
         (eq-schema1)
         (eq-schema2)
         (eq-schema3)
         (or-schema)
         'p)
   '(o p q)
   options))

(defun coder-ex10 (&rest options)
  ;; no 1,...,8-step proof
  ;; 13-step proof by (coder-ex10 :max-syms 18 :max-vars 2 :must-use '(1 2 3 4 5 6 8 9 10 11))
  (apply
   'coder
   (list (ii-schema)
         (id-schema)
         (cr-schema1)
         (cr-schema2)
         (eq-schema1)
         (eq-schema2)
         (eq-schema3)
         (or-schema)
         (and-schema)
         'p
         'q)
   '(a p q)
   options))

(defun coder-ex11 (&rest options)
  ;; no 1,...,8-step proof
  ;; 9-step proof by (coder-ex11 :max-syms 16 :max-vars 2)
  (apply
   'coder
   (list (ii-schema)
         (id-schema)
         (cr-schema1)
         (cr-schema2)
         (eq-schema1)
         (eq-schema2)
         (eq-schema3)
         (alt-and-schema)
         'p
         'q)
   '(a p q)
   options))

;;; coder-examples.lisp EOF
