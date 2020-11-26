; (c) 1993,1994 Copyright (c) University of Washington
;  Written by Tony Barrett.

;  All rights reserved. Use of this software is permitted for non-commercial
;  research purposes, and it may be copied only for that use.  All copies must
;  include this copyright message.  This software is made available AS IS, and
;  neither the authors nor the University of Washington make any warranty about
;  the software or its performance.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Monkey domain (from prodigy)

;;; dsw added location back despite cryptic comment below
(define (domain monkey-domain)	       ; Comment: adding location caused fail
  (:requirements :strips :equality)
 
  (:predicates (location ?x)
	       (on-floor)
	       (at ?m ?x)
	       (hasknife)
	       (onbox ?x)
	       (hasbananas)
	       (hasglass)
	       (haswater))
  (:constants monkey box knife bananas glass waterfountain)

  ;; movement and clinbing
  (:action GO-TO
	     :parameters (?x ?y)
	     :precondition (and (location ?x) (location ?y) 
                                 (not (= ?y ?x)) (on-floor) (at monkey ?y))
	     :effect (and (at monkey ?x) (not (at monkey ?y))))
  
  (:action CLIMB
	     :parameters (?x)
	     :precondition (and (location ?x) (at box ?x) (at monkey ?x))
	     :effect (and (onbox ?x) (not (on-floor))))
  
  (:action PUSH-BOX
	     :parameters (?x ?y)
	     :precondition (and (location ?x) (location ?y) 
                                 (not (= ?y ?x)) (at box ?y) (at monkey ?y) 
				 (on-floor))
	     :effect (and (at monkey ?x) (not (at monkey ?y))
			   (at box ?x)    (not (at box ?y))))

  ;; getting bananas
  (:action GET-KNIFE
	     :parameters (?y)
	     :precondition (and (location ?y) (at knife ?y) (at monkey ?y))
	     :effect (and (hasknife) (not (at knife ?y))))
  
  (:action GRAB-BANANAS
	     :parameters (?y)
	     :precondition (and (location ?y) (hasknife) 
                                 (at bananas ?y) (onbox ?y))
	     :effect (hasbananas))
  
  ;; getting water
  (:action PICKGLASS
	     :parameters (?y)
	     :precondition (and (location ?y) (at glass ?y) (at monkey ?y))
	     :effect (and (hasglass) (not (at glass ?y))))
  
  (:action GETWATER
	     :parameters (?y)
	     :precondition (and (location ?y) (hasglass)
				 (at waterfountain ?y)
				 (at monkey ?y)
				 (onbox ?y))
	     :effect (haswater)))
      
(define (problem monkey-test1)
    (:domain monkey-domain)
  (:objects p1 p2 p3 p4)
  (:init (location p1)
	 (location p2)(location p3)(location p4)
	 (at monkey p1)(on-floor)(at box p2)(at bananas p3)
	 (at knife p4))
  (:goal (AND (hasbananas)))
  (:length (:serial 6) (:parallel 6))
    )

(define (problem monkey-test2)
    (:domain monkey-domain)
  (:objects P1 P2 P3 P4 P6)
  (:init (location p1)
	 (location p2)(location p3)
	 (location p4)(location p6)
	 (at monkey p1)(on-floor)
	 (at box p2)
	 (at bananas p3)
	 (at knife p4)
	 (at waterfountain p3)(at glass p6))
  (:goal (and (hasbananas) (haswater)))
  (:length (:serial 9) (:parallel 9)))

(define (problem monkey-test3)
    (:domain monkey-domain)
  (:objects p1 p2 p3 p4 p5 p6)
  (:init (location p1) (location p2)
	 (location p3)(location p4)
	 (location p5) (location p6)
	 (at monkey p1)(on-floor)
	 (at box p2)
	 (at bananas p3)
	 (at knife p4)
	 (at waterfountain p5)(at glass p6))
  (:goal (and (hasbananas) (haswater)))
  (:length (:serial 10) (:parallel 10))
    )

;;;UCPOP(32): (bf-control 'monkey-test1)
;;;
;;;Initial  : ((LOCATION P1) (LOCATION P2) (LOCATION P3) (LOCATION P4)
;;;            (AT MONKEY P1) (ON-FLOOR) (AT BOX P2) (AT BANANAS P3)
;;;            (AT KNIFE P4))
;;;
;;;Step 1  : (GO-TO P4 P1)          Created 5 
;;;           0  -> (ON-FLOOR)          
;;;           0  -> (AT MONKEY P1)      
;;;Step 2  : (GET-KNIFE P4)         Created 6 
;;;           0  -> (AT KNIFE P4)       
;;;           5  -> (AT MONKEY P4)      
;;;Step 3  : (GO-TO P2 P4)          Created 4 
;;;           0  -> (ON-FLOOR)          
;;;           5  -> (AT MONKEY P4)      
;;;Step 4  : (PUSH-BOX P3 P2)       Created 3 
;;;           0  -> (AT BOX P2)         
;;;           4  -> (AT MONKEY P2)      
;;;           0  -> (ON-FLOOR)          
;;;Step 5  : (CLIMB P3)             Created 2 
;;;           3  -> (AT BOX P3)         
;;;           3  -> (AT MONKEY P3)      
;;;Step 6  : (GRAB-BANANAS P3)      Created 1 
;;;           6  -> (HASKNIFE)          
;;;           0  -> (AT BANANAS P3)     
;;;           2  -> (ONBOX P3)          
;;;
;;;Goal    : (HASBANANAS)
;;;           1  -> (HASBANANAS)        
;;;Complete!
;;;
;;;UCPOP (Init = 9  ; Goals = 1 ) => Win  (6 steps)     CPU 850      
;;;     Nodes (V = 66  ; Q = 26  ; C = 103 )             Branch 1.3939394 
;;;     Working Unifies: 875                             Bindings added: 101  
;;;NIL

