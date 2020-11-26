; (c) 1993,1994 Copyright (c) University of Washington
;  Written by Tony Barrett.

;  All rights reserved. Use of this software is permitted for non-commercial
;  research purposes, and it may be copied only for that use.  All copies must
;  include this copyright message.  This software is made available AS IS, and
;  neither the authors nor the University of Washington make any warranty about
;  the software or its performance.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ferry domain

(define (domain ferry-strips)
  (:requirements :strips)

  (:constants ferry)
  (:predicates (at ?x ?y)
	       (place ?x)
	       (auto ?x)
	       (at-ferry ?x)
	       (empty-ferry)
	       (on ?x ?y)
	       (ferryless ?x))

  (:action board
	     :parameters (?x ?y)
	     :precondition (and (at ?x ?y)
				 (place ?y)
				 (auto ?x)
				 (at-ferry ?y)
				 (empty-ferry))
	     :effect (and (on ?x ferry)
			   (not (at ?x ?y))
			   (not (empty-ferry))))
  (:action sail
	     :parameters (?x ?y)
	     :precondition (and (at-ferry ?x)
				 (place ?x)
				 (place ?y)
				 (ferryless ?y))
	     :effect (and (at-ferry ?y)
			  (ferryless ?x)
			   (not (at-ferry ?x))))
  (:action debark
	     :parameters (?x ?y)
	     :precondition (and (on ?x ferry)
				 (auto ?x)
				 (place ?y)
				 (at-ferry ?y))
	     :effect (and (not (on ?x ferry))
			   (at ?x ?y)
			   (empty-ferry))))

(define (problem strips-ferry1) 
    (:init (place a) 
	   (place b) 
	   (auto c1) 
	   (auto c2)
	   (at c1 a)
	   (at c2 a)
	   (at-ferry a)
	   (ferryless b)
	   (empty-ferry))
  (:objects a b c1 c2)
  (:goal (and (at c1 b)
	      (at c2 b)))
  (:domain ferry-strips))

(define (problem strips-ferry2)
    (:init (place a)
	   (place b)
	   (auto c1)
	   (at c1 a)
	   (at-ferry a)
	   (ferryless b)
	   (empty-ferry))
  (:objects a b c1)
  (:goal (and (at c1 b)))
  (:domain ferry-strips))

; Avoids "ferryless" predicate by using equality.

(define (domain ferry)
  (:requirements :strips :equality)

  (:constants ferry)
  (:predicates (at ?x ?y)
	       (place ?x)
	       (auto ?x)
	       (at-ferry ?x)
	       (empty-ferry)
	       (on ?x ?y))

  (:action board
	     :parameters (?x ?y)
	     :precondition (and (at ?x ?y)
				 (place ?y)
				 (auto ?x)
				 (at-ferry ?y)
				 (empty-ferry))
	     :effect (and (on ?x ferry)
			   (not (at ?x ?y))
			   (not (empty-ferry))))
  (:action sail
	     :parameters (?x ?y)
	     :precondition (and (at-ferry ?x)
				 (place ?x)
				 (place ?y)
				 (not (= ?x ?y)))
	     :effect (and (at-ferry ?y)
			   (not (at-ferry ?x))))
  (:action debark
	     :parameters (?x ?y)
	     :precondition (and (on ?x ferry)
				 (auto ?x)
				 (place ?y)
				 (at-ferry ?y))
	     :effect (and (not (on ?x ferry))
			   (at ?x ?y)
			   (empty-ferry))))

(define (problem ferry1) 
    (:init (place a) 
	   (place b) 
	   (auto c1) 
	   (auto c2)
	   (at c1 a)
	   (at c2 a)
	   (at-ferry a)
	   (empty-ferry))
  (:objects a b c1 c2)
  (:goal (and (at c1 b)
	      (at c2 b)))
  (:length (:serial 7) (:parallel 7))
  (:domain ferry))

(define (problem ferry2)
    (:init (place a)
	   (place b)
	   (auto c1)
	   (at c1 a)
	   (at-ferry a)
	   (empty-ferry))
  (:objects a b c1)
  (:goal (and (at c1 b)))
  (:length (:serial 3) (:parallel 3))
  (:domain ferry))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; typed version of the ferry domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (domain ferry-typed)
  (:requirements :strips :equality :typing)

  (:types auto place ferry)
  (:constants the-ferry - ferry)
  (:predicates (at-ferry ?l - place)
	       (at ?x - auto
		   ?y - place)
	       (empty-ferry)
	       (on ?x - auto
		   ?f - ferry))

  (:action board
	     :parameters (?x - auto ?y - place)
	     :precondition (and (at ?x ?y)(at-ferry ?y)(empty-ferry))
	     :effect 
	     (and (on ?x the-ferry)
		   (not (at ?x ?y))
		   (not (empty-ferry))))
  (:action sail
	     :parameters (?x ?y - place)
	     :precondition (and (at-ferry ?x) (not (= ?x ?y)))
	     :effect (and (at-ferry ?y)
			   (not (at-ferry ?x))))
  (:action debark
	     :parameters (?x - auto ?y - place)
	     :precondition (and (on ?x the-ferry)(at-ferry ?y))
	     :effect (and (not (on ?x the-ferry))
			   (at ?x ?y)
			   (empty-ferry))))

(define (problem test-ferry)
    (:domain ferry-typed)
  (:objects a b - place
	    c1 c2 - auto)
  (:init (at c1 a)(at c2 a)(at-ferry a)
	 (empty-ferry))
  (:length (:serial 7 ) (:parallel 7)) 
  (:goal (and (at c1 b)(at c2 b))))

;;;UCPOP(25): (bf-control 'test-ferry)
;;;
;;;Initial  : ((PLACE A) (PLACE B) (AUTO C1) (AUTO C2) (AT C1 A) (AT C2 A)
;;;            (AT-FERRY A) (EMPTY-FERRY))
;;;
;;;Step 1  : (BOARD C2 A)           Created 3 
;;;           0  -> (AT C2 A)           
;;;           0  -> (AT-FERRY A)        
;;;           0  -> (EMPTY-FERRY)       
;;;           0  -> (AUTO C2)           
;;;           0  -> (PLACE A)           
;;;Step 2  : (SAIL A B)             Created 2 
;;;           0  -> (AT-FERRY A)        
;;;           0  -> (PLACE A)           
;;;           0  -> (PLACE B)           
;;;Step 3  : (DEBARK C2 B)          Created 1 
;;;           3  -> (ON C2 FERRY)       
;;;           2  -> (AT-FERRY B)        
;;;           0  -> (AUTO C2)           
;;;           0  -> (PLACE B)           
;;;Step 4  : (SAIL B A)             Created 6 
;;;           2  -> (AT-FERRY B)        
;;;           0  -> (PLACE B)           
;;;           0  -> (PLACE A)           
;;;Step 5  : (BOARD C1 A)           Created 7 
;;;           0  -> (AT C1 A)           
;;;           6  -> (AT-FERRY A)        
;;;           1  -> (EMPTY-FERRY)       
;;;           0  -> (AUTO C1)           
;;;           0  -> (PLACE A)           
;;;Step 6  : (SAIL A B)             Created 5 
;;;           6  -> (AT-FERRY A)        
;;;           0  -> (PLACE A)           
;;;           0  -> (PLACE B)           
;;;Step 7  : (DEBARK C1 B)          Created 4 
;;;           7  -> (ON C1 FERRY)       
;;;           5  -> (AT-FERRY B)        
;;;           0  -> (AUTO C1)           
;;;           0  -> (PLACE B)           
;;;
;;;Goal    : (AND (AT C1 B) (AT C2 B))
;;;           4  -> (AT C1 B)           
;;;           1  -> (AT C2 B)           
;;;Complete!
;;;
;;;UCPOP (Init = 8  ; Goals = 3 ) => Win  (7 steps)     CPU 2633     
;;;     Nodes (V = 488 ; Q = 153 ; C = 786 )             Branch 1.3135246 
;;;     Working Unifies: 2194                            Bindings added: 362  
;;;NIL





