" (c) 1993,1994 Copyright (c) University of Washington
  Written by Tony Barrett.

  All rights reserved. Use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  All copies must
  include this copyright message.  This software is made available AS IS, and
  neither the authors nor the University of Washington make any warranty about
  the software or its performance.

  When you first acquire this software please send mail to
  bug-ucpop@cs.washington.edu; the same address should be used for problems."

(in-package :domains)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Blocks world domains
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (domain blocks-world-domain)
  (:requirements :strips :equality :conditional-effects)

  (:constants Table)

  (:predicates (on ?x ?y)
	       (clear ?x)
	       (block ?b)
	       )

  ;; Define step for placing one block on another.
  (:action puton
	     :parameters (?X ?Y ?Z)
	     :precondition (and (on ?X ?Z) (clear ?X) (clear ?Y)
				 (not (= ?Y ?Z)) (not (= ?X ?Z))
				 (not (= ?X ?Y)) (not (= ?X Table)))
	     :effect
	     (and (on ?X ?Y) (not (on ?X ?Z))
		   (when (not (= ?Z Table)) (clear ?Z))
		   (when (not (= ?Y Table)) (not (clear ?Y))))))

(define (problem sussman-anomaly)       ; graphplan 3 steps
    (:domain blocks-world-domain)
  (:objects A B C)
  (:init (block A) (block B) (block C) (block Table)
	 (on C A) (on A Table) (on B Table)
	 (clear C) (clear B) (clear Table))
  (:goal (and (on B C) (on A B)))
  (:length (:serial 3) (:parallel 3)))

;;;UCPOP(22): (bf-control 'sussman-anomaly)
;;;
;;;Initial  : ((BLOCK A) (BLOCK B) (BLOCK C) (BLOCK TABLE) (ON C A) (ON A TABLE)
;;;            (ON B TABLE) (CLEAR C) (CLEAR B) (CLEAR TABLE))
;;;
;;;Step 1  : (PUTON C TABLE A)      Created 2
;;;           0  -> (ON C A)
;;;           0  -> (CLEAR C)
;;;           0  -> (CLEAR TABLE)
;;;Step 2  : (PUTON B C TABLE)      Created 3
;;;           0  -> (ON B TABLE)
;;;           0  -> (CLEAR B)
;;;           0  -> (CLEAR C)
;;;Step 3  : (PUTON A B TABLE)      Created 1
;;;           0  -> (ON A TABLE)
;;;           2  -> (CLEAR A)
;;;           0  -> (CLEAR B)
;;;
;;;Goal    : (AND (ON B C) (ON A B))
;;;           3  -> (ON B C)
;;;           1  -> (ON A B)
;;;Complete!
;;;
;;;UCPOP (Init = 10 ; Goals = 3 ) => Win  (3 steps)     CPU 283
;;;     Nodes (V = 51  ; Q = 25  ; C = 82  )             Branch 1.4901961
;;;     Working Unifies: 481                             Bindings added: 202
;;;NIL

(define (problem tower-invert3)         ; graphplan 4 steps
    (:domain blocks-world-domain)
  (:objects A B C)
  (:init (block A) (block B) (block C) (block Table)
	 (on a b) (on b c) (on c table)
	 (clear a) (clear table))
  (:goal (and (on b c) (on c a)))
  (:length (:serial 4) (:parallel 4)))

(define (problem tower-invert4)         ; graphplan 6 steps, 8 actions
    (:domain blocks-world-domain)
  (:objects A B C D)
  (:init (block A) (block B) (block C) (block D) (block Table)
	 (on a b) (on b c) (on c d) (on d table)
	 (clear a) (clear table))
  (:goal (and (on b c) (on c d) (on d a)))
  (:length (:serial 6) (:parallel 6)))

;(defun random-bw-problem (n)
;  (def-problem (intern (format nil "RANDOM-BLOCKS-~a" n))
;    :domain 'blocks-world-domain
;    :init `(and ,@(random-bw-state n))
;    :goal `(and ,@(random-bw-state n))))

;(defun random-bw-state (n &aux (state nil) (ret '((clear table))))
;  (labels ((PERMUTE (list)
;	     (let ((l (copy-list list))
;		   (ret nil))
;	       (do ()
;		   ((null l) ret)
;		 (let ((i (random (length l))))
;		   (push (nth i l) ret)
;		   (setf l (delete (nth i l) l)))))))
;    (dolist (b (permute (subseq '(a b c d e f) 0 n)))
;      (let ((r (random (1+ (length state)))))
;	(if (zerop r) (push (list b) state)
;	  (push b (nth (1- r) state)))))
;    (dolist (tower state (permute ret))
;      (push `(clear ,(car tower)) ret)
;      (do ((b tower (cdr b)))
;	  ((null b))
;	(push `(on ,(car b) ,(if (cdr b) (cadr b) 'table)) ret)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  McDermott blocks world
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (domain mcd-blocksworld)
  (:requirements :adl :universal-preconditions :disjunctive-preconditions)
  (:constants Table)
  (:predicates (on ?x ?y)
	       (clear ?x)
	       (block ?b)
	       (above ?x ?y))
  (:action puton
	     :parameters (?x ?y ?d)
	     :precondition (and (not (= ?x ?y)) (not (= ?x table)) (not (= ?d ?y))
				 (on ?x ?d)
				 (or (= ?x Table)
				      (forall (?b) (imply (block ?b) (not (on ?b ?x)))))
				 (or (= ?y Table)
				      (forall (?b) (imply (block ?b) (not (on ?b ?y))))))
	     :effect
	     (and (on ?x ?y) (not (on ?x ?d))
		   (forall (?c)
			    (when (or (= ?y ?c) (above ?y ?c))
				   (above ?x ?c)))
		   (forall (?e)
			    (when (and (above ?x ?e) (not (= ?y ?e))
					 (not (above ?y ?e)))
				   (not (above ?x ?e)))))))

;;;UCPOP(41): (bf-control 'mcd-sussman-anomaly)
;;;
;;;Initial  : ((BLOCK A) (BLOCK B) (BLOCK C) (BLOCK TABLE) (ON C A) (ON B TABLE)
;;;            (ON A TABLE))
;;;
;;;Step 1  : (PUTON C TABLE A)      Created 2
;;;           0  -> (ON C A)
;;;           0  -> (NOT (ON TABLE C))
;;;           0  -> (NOT (ON C C))
;;;           0  -> (NOT (ON B C))
;;;           0  -> (NOT (ON A C))
;;;Step 2  : (PUTON B C TABLE)      Created 3
;;;           0  -> (ON B TABLE)
;;;           0  -> (NOT (ON TABLE B))
;;;           0  -> (NOT (ON C B))
;;;           0  -> (NOT (ON B B))
;;;           0  -> (NOT (ON A B))
;;;           0  -> (NOT (ON TABLE C))
;;;           0  -> (NOT (ON C C))
;;;           0  -> (NOT (ON B C))
;;;           0  -> (NOT (ON A C))
;;;Step 3  : (PUTON A B TABLE)      Created 1
;;;           0  -> (ON A TABLE)
;;;           0  -> (NOT (ON TABLE A))
;;;           2  -> (NOT (ON C A))
;;;           0  -> (NOT (ON B A))
;;;           0  -> (NOT (ON A A))
;;;           0  -> (NOT (ON TABLE B))
;;;           0  -> (NOT (ON C B))
;;;           0  -> (NOT (ON B B))
;;;           0  -> (NOT (ON A B))
;;;
;;;Goal    : (AND (ON B C) (ON A B))
;;;           3  -> (ON B C)
;;;           1  -> (ON A B)
;;;Complete!
;;;
;;;UCPOP (Init = 7  ; Goals = 3 ) => Win  (3 steps)     CPU 400
;;;     Nodes (V = 54  ; Q = 25  ; C = 101 )             Branch 1.462963
;;;     Working Unifies: 976                             Bindings added: 163
;;;NIL

(define (problem mcd-sussman-anomaly)
    (:domain mcd-blocksworld)
  (:objects A B C)
  (:init (block A) (block B) (block C) (block Table)
	 (on c a) (on b table) (on a table))
  (:goal (and (on b c) (on a b))))


(define (problem mcd-tower-invert)
    (:domain mcd-blocksworld)
  (:objects A B C D E)
  (:init (block A) (block B) (block C) (block D) (block E) (block Table)
	 (clear a) (on a b) (on b c) (on c d) (on d e)(on e table)
	 (clear table))
  (:goal (and (on b c) (on c d) (on d e) (on e a))))





(define (domain  mcd-blocksworld-axiom)
    (:requirements :adl :domain-axioms :quantified-preconditions)

  (:constants Table)
  (:predicates (on ?x ?y)
	       (clear ?x)
	       (block ?b)
	       (above ?x ?y))

  (:axiom 
	  :vars (?b ?x)
	  :context (or (= ?x Table)
		       (not (exists (?b ?x) (on ?b ?x))))
	  :implies (clear ?x))

  (:action puton
	     :parameters (?x ?y ?d)
	     :precondition (and (not (= ?x ?y)) (not (= ?x table)) (not (= ?d ?y))
				 (on ?x ?d) (clear ?x) (clear ?y))
	     :effect
	     (and (on ?x ?y) (not (on ?x ?d))
		  (forall (?c)
			  (when (or (= ?y ?c) (above ?y ?c))
				 (above ?x ?c)))
		  (forall (?e)
			   (when (and (above ?x ?e) (not (= ?y ?e))
					(not (above ?y ?e)))
				  (not (above ?x ?e)))))))

(define (problem mcd-sussman)
    (:domain mcd-blocksworld-axiom)
  (:objects A B C)
  (:init (block A) (block B) (block C) (block Table)
	 (on c a) (on b table) (on a table))
  (:goal (and (on b c) (on a b))))

(define (problem mcd-tower)
    (:domain mcd-blocksworld-axiom)
  (:objects A B C)
  (:init (block A) (block B) (block C) (block Table)
	 (clear a) (on a b) (on b c) (on c table)
		(clear table))
  (:goal (and (on b c) (on c a))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prodigy blocks world
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (domain prodigy-bw)
  (:requirements :strips :typing)
  (:predicates (on ?x ?y)
	       (on-table ?x)
	       (clear ?x)
	       (arm-empty)
	       (holding ?x)
	       )
  (:types )				; object, the default

  (:action pick-up
	     :parameters (?ob1)
	     :precondition (and (clear ?ob1) (on-table ?ob1) (arm-empty))
	     :effect
	     (and (not (on-table ?ob1))
		   (not (clear ?ob1))
		   (not (arm-empty))
		   (holding ?ob1)))
  (:action put-down
	     :parameters (?ob)
	     :precondition (holding ?ob)
	     :effect
	     (and (not (holding ?ob))
		   (clear ?ob)
		   (arm-empty)
		   (on-table ?ob)))
  (:action stack
	     :parameters (?sob ?sunderob)
	     :precondition (and (holding ?sob) (clear ?sunderob))
	     :effect
	     (and (not (holding ?sob))
		   (not (clear ?sunderob))
		   (clear ?sob)
		   (arm-empty)
		   (on ?sob ?sunderob)))
  (:action unstack
	     :parameters (?sob ?sunderob)
	     :precondition (and (on ?sob ?sunderob) (clear ?sob) (arm-empty))
	     :effect
	     (and (holding ?sob)
		   (clear ?sunderob)
		   (not (clear ?sob))
		   (not (arm-empty))
		   (not (on ?sob ?sunderob)))))

(define (problem simple)
    (:domain prodigy-bw)
  (:objects A B C)
  (:init (clear a) (arm-empty) (on a b) (on-table b))
  (:goal (and (on-table a) (clear b))))

(define (problem prodigy-sussman)       ; graphplan 6 steps
    (:domain prodigy-bw)
  (:objects A B C)
  (:init (on-table a) (on-table b) (on c a)
		(clear b) (clear c) (arm-empty))
  (:goal (and (on a b) (on b c)))
  (:length (:serial 6) (:parallel 6)))

;;; 
;;; 

(define (problem huge-fct)
    (:domain prodigy-bw)
  (:objects A B C D E F G H I J)
  (:init (on-table a) (on b a) (on c b) (Clear c) (on-table d)
	 (on e d) (on f e) (clear f) (On-table g) (on h g) (on i h)
	 (on j i) (clear j) (arm-empty))
  (:goal (and (on d a) (on g d) (on e b) (on h e) (on i c) (on f j)))
)

(define (problem prodigy-p22)           ; graphplan 12 steps
  ;; Relatively hard. Requires 12 steps and takes graphplan 20 seconds.
  ;; (unstack c d) (put-down c) (unstack d e) (put-down d)
  ;; (unstack e f) (put-down e) (unstack f g) (stack f a) (unstack c b)
  ;; (stack c d) (pick-up b) (stack b c)
    (:domain prodigy-bw)
  (:objects A B C D E F G)
  (:init (on-table A) (clear A)
	 (on-table B) (clear B)
	 (on-table G)
	 (on F G)
	 (on E F)
	 (on D E)
	 (on C D) (clear C)
	 (arm-empty))
  (:goal (and (on B C) (on-table A) (on F A) (on C D)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple strips blocks world from Smith and Peot

(define (domain simple-blocks)
  (:requirements :strips :equality)
  (:predicates (on ?x ?y)
	       (clear ?x))
  (:constants Table)

  (:action PutTable
	     :parameters (?x ?z)
	     :precondition (and (on ?x ?z) (clear ?x)
                                 (not (= ?x Table)) (not (= ?z Table)))
	     :effect (and (on ?x Table) (clear ?z) (not (on ?x ?z))))
  (:action Put
	     :parameters (?x ?y ?z)
	     :precondition (and (on ?x ?z) (clear ?x) (clear ?y)
                                  (not (= ?x Table)) (not (= ?y Table)) (not (= ?x ?y)))
	     :effect (and (on ?x ?y) (clear ?z)
                           (not (on ?x ?z)) (not (clear ?y)))))

(define (problem simple-block1)         ; sussman anomaly - graphplan 3 steps
    (:domain simple-blocks)
  (:objects A B C)
  (:goal (and (on a b) (on b c)))
  (:init (on a table)
	 (on c a) (clear c)
	 (on b table) (clear b))
  (:length (:serial 3) (:parallel 3)))

(define (problem simple-block2)         ; graphplan 4 steps, 6 actions
    (:domain simple-blocks)
  (:objects A B C D)
  (:goal (and (on c d) (on b c) (on a b)))
  (:init (on a table) (on b a)
	 (on c table) (on d c)
	 (clear d) (clear b))
  (:length (:serial 4) (:parallel 4)))

(define (problem simple-block3)         ; graphplan 4 steps, 6 actions
    (:domain simple-blocks)
  (:objects A B C D E)
  (:goal (and (on d e) (on c d) (on b c) (on a b)))
  (:init (on a table) (on b a)
	 (on c table) (on d c)
	 (on e table) (clear e)
	 (clear d) (clear b))
  (:length (:serial 4) (:parallel 4)))

(define (problem simple-block-stack)         ; graphplan 4 steps, 6 actions
    (:domain simple-blocks)
  (:objects A B C)
  (:goal (and (on a b) (on b c)))
  (:init (on a table) (on b table)
	 (on c table) (clear b) (clear a) (clear c))
  (:length (:serial 2) (:parallel 2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  AT&T blocks world
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ( domain att-bw )
    ( :requirements :strips :equality :disjunctive-preconditions)
  (:predicates (is-table ?t)
	       (block ?b)
	       (clear ?x)
	       (on ?x ?y))
  (:constants table)


    ( :action move
        :parameters (?obj ?source ?dest)   ; move obj from source to dest
        :precondition (AND (OR (is-table ?dest) (clear ?dest))
                           (block ?obj)
                           (clear ?obj)
                           (on ?obj ?source)
                           (not (= ?obj ?source)) (not (= ?obj ?dest))
			   (not (= ?source ?dest)) (not (= ?obj table)))
        :effect (AND (NOT (on ?obj ?source))
                     (clear ?source)
                     (NOT (clear ?dest))
                     (on ?obj ?dest))))

;;;
;;; bw_large_a
;;; Initial:  3/2/1   5/4	9/8/7/6         "3 on 2 on 1"
;;; Goal:     1/5     8/9/4	2/3/7/6
;;; Length: 6

( define ( problem att-bw-large-a ) 
    (:domain att-bw)
  (:objects A B C D E F G H I)
  (:init (block A) (block B) (block C) (block D) (block E)
	 (block F) (block G) (block H) (block I) (is-table table)
	 (on A table) (on D table) (on F table)
	 (on C B) (on B A) (on E D)
	 (on I H) (on H G) (on G F)
	 (clear C) (clear E) (clear I))
  (:goal (AND (on A E) (on E table)
               (on H I) (on I D) (on D table)
               (on B C) (on C G) (on G F) (on F table)
               (clear A) (clear H) (clear B))))

( define ( problem att-sussman ) 
    (:domain att-bw)
  (:objects A B C)
  (:init ( block a ) ( block b ) ( block c ) ( is-table table)
	 ( on a table)
	 ( on b table)
	 ( on c a )
	 ( clear b )
	 ( clear c ) )
  (:goal ( AND (on a b) (on b c) ))
  (:length (:serial 3) (:parallel 3))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  AT&T blocks world without disjunctive preconditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ( domain att-bw2 )
    ( :requirements :strips :equality)
  (:predicates (is-table ?t)
	       (block ?b)
	       (clear ?b)
	       (on ?x ?y))
  (:constants table)
	       
    ( :action move-to-table
        :parameters (?obj ?source ?dest)   ; move obj from source to dest
        :precondition (AND (is-table ?dest)
                           (block ?obj)
                           (clear ?obj)
                           (on ?obj ?source)
                           (not (= ?obj ?source)) (not (= ?obj ?dest))
			   (not (= ?source ?dest)) (not (= ?obj table)))
        :effect (AND (NOT (on ?obj ?source))
                     (clear ?source)
                     (on ?obj ?dest)))

    ( :action move-to-block
        :parameters (?obj ?source ?dest)   ; move obj from source to dest
        :precondition (AND (clear ?dest)
                           (block ?obj)
                           (block ?dest)
                           (clear ?obj)
                           (on ?obj ?source)
                           (not (= ?obj ?source)) (not (= ?obj ?dest))
			   (not (= ?source ?dest)) (not (= ?obj table)))
        :effect (AND (NOT (on ?obj ?source))
                     (NOT (clear ?dest))
                     (clear ?source)
                     (on ?obj ?dest))))

( define ( problem att-bw2-large-a ) 
    (:domain att-bw2)
  (:objects A B C D E F G H I)
  (:init (block A) (block B) (block C) (block D) (block E)
	 (block F) (block G) (block H) (block I) (is-table table)
	 (on A table) (on D table) (on F table)
	 (on C B) (on B A) (on E D)
	 (on I H) (on H G) (on G F)
	 (clear C) (clear E) (clear I))
    (:goal (AND (on A E) (on E table)
               (on H I) (on I D) (on D table)
               (on B C) (on C G) (on G F) (on F table)
               (clear A) (clear H) (clear B))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  SNLP blocks world with 2 operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (domain snlp-bw1)
    (:requirements :adl)
  
  (:predicates (clear ?x)
	       (on-table ?x)
	       (on ?x ?y))
  

    (:action stack
	       :parameters (?x ?y)
	       :precondition
	       (AND (clear ?x)
		    (clear ?y)
		    (on-table ?x))
	       :effect
	       (AND (on ?x ?y)
		    (NOT (on-table ?x))
		    (NOT (clear ?y))))
  (:action unstack
	     :parameters (?x ?y)
	     :precondition
	     (AND (clear ?x)
		  (on ?x ?y))
	     :effect
	     (AND (on-table ?x)
		  (NOT (on ?x ?y))
		  (clear ?y))))

(define (problem big-bw1)
    (:domain snlp-bw1)
  (:objects a b c d e f g)
  (:init (on c a) (on-table b) (on-table a)
	 (on e c) (on d b)
	 (on f d) (on g e)
	 (clear f)
	 (clear g))
  (:goal (AND (on a b)
	       (on b c)
	       (on c d)
	       (on d e)
	       (on e f)
	       (on f g)))
  (:length (:serial 11) (:parallel 7))
    )

(define (problem med-bw1)
    (:domain snlp-bw1)
  (:objects a b c d e)
  (:init (on c a) (on-table b) (on-table a)
	 (on e c) (on d b)
	 (clear d) (clear e))
  (:goal (AND (on a b)
	      (on b c)
	      (on c d)
	      (on d e)))
  (:length (:serial 7) (:parallel 5)))

(define (problem small-bw1)
    (:domain snlp-bw1)
  (:objects a b c d)
  (:init (on c a) (on-table b) (on-table a)
	 (on d b)
	 (clear d) (clear c))
  (:goal (AND (on a b)
	      (on b c)
	      (on c d)))
  (:length (:serial 5) (:parallel 4)))

(define (problem sussman1)
    (:domain snlp-bw1)
  (:objects a b c)
  (:init (on c a) (on-table b) (on-table a) (clear c) (clear b))
  (:goal (AND (on a b) (on b c)))
  (:length (:serial 3) (:parallel 3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  SNLP blocks world with 3 operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (domain snlp-bw2)
    (:requirements :adl)
  
  (:predicates (clear ?x)
	       (on-table ?x)
	       (on ?x ?y))

    (:action stack
	       :parameters (?x ?y)
	       :precondition
	       (AND (clear ?x)
		    (clear ?y)
		    (on-table ?x))
	       :effect
	       (AND (on ?x ?y)
		    (NOT (on-table ?x))
		    (NOT (clear ?y))))
  (:action unstack
	     :parameters (?x ?y)
	     :precondition
	     (AND (clear ?x)
		  (on ?x ?y))
	     :effect
	     (AND (on-table ?x)
		  (NOT (on ?x ?y))
		  (clear ?y)))
  (:action move
	     :parameters (?x ?y ?z)
	     :precondition
	     (AND (on ?x ?y)
		  (clear ?x)
		  (clear ?z))
	     :effect
	     (AND (on ?x ?z)
		  (clear ?y)
		  (NOT (clear ?z))
		  (NOT (on ?x ?y)))))

(define (problem big-bw2)
    (:domain snlp-bw2)
  (:objects a b c d e f g)
  (:init (on c a) (on-table b) (on-table a)
	 (on e c) (on d b)
	 (on f d) (on g e)
	 (clear f)
	 (clear g))
  (:goal (AND (on a b)
	      (on b c)
	      (on c d)
	      (on d e)
	      (on e f)
	      (on f g)))
  (:length (:serial 7) (:parallel 7))
    )

(define (problem med-bw2)
    (:domain snlp-bw2)
  (:objects a b c d e)
  (:init (on c a) (on-table b) (on-table a)
	 (on e c) (on d b)
	 (clear d) (clear e))
  (:goal (AND (on a b)
	      (on b c)
	      (on c d)
	      (on d e)))
  (:length (:serial 5) (:parallel 5)))

(define (problem small-bw2)
    (:domain snlp-bw2)
  (:objects a b c d)
  (:init (on c a) (on-table b) (on-table a)
	 (on d b)
	 (clear d) (clear c))
  (:goal (AND (on a b)
	      (on b c)
	      (on c d)
	      ))
  (:length (:serial 4) (:parallel 4)))

(define (problem sussman2)
    (:domain snlp-bw2)
  (:objects a b c)
  (:init (on c a) (on-table b) (on-table a) (clear c) (clear b))
  (:goal (AND (on a b) (on b c)))
  (:length (:serial 3) (:parallel 3)))
