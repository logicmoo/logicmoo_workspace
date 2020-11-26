;;; -*- Package: DOMAINS; Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :domains)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple Vehicle domain where a person has to get in to a vehicle,
;;; drive it somewhere, get out, and return to some other location.
;;; Recursion is a problem in this domain because the roads and
;;; bridges go both directions.

(define (domain bulldozer)
  (:requirements :strips :equality)
  (:predicates (road ?from ?to)
	       (at ?thing ?place)
	       (mobile ?thing)
	       (bridge ?from ?to)
	       (person ?p)
	       (vehicle ?v)
	       (driving ?p ?v))
	       
  (:action Drive
	     :parameters (?thing ?from ?to)
	     :precondition (and (road ?from ?to)
				 (at ?thing ?from)
				 (mobile ?thing)
				 (not (= ?from ?to)))
	     :effect (and (at ?thing ?to) (not (at ?thing ?from))))
  (:action Cross
	     :parameters (?thing ?from ?to)
	     :precondition (and (bridge ?from ?to)
				 (at ?thing ?from)
				 (mobile ?thing)
				 (not (= ?from ?to)))
	     :effect (and (at ?thing ?to) (not (at ?thing ?from))))
  (:action Board
	     :parameters (?person ?place ?vehicle)
	     :precondition (and (at ?person ?place)
				 (person ?person)
				 (vehicle ?vehicle)
				 (at ?vehicle ?place)
				 (not (= ?person ?vehicle)))
	     :effect (and (driving ?person ?vehicle)
			   (mobile ?vehicle)
			   (not (at ?person ?place))
			   (not (mobile ?person))))  
  (:action Disembark
	     :parameters (?person ?place ?vehicle)
	     :precondition (and (person ?person)
				 (vehicle ?vehicle)
				 (driving ?person ?vehicle)
				 (at ?vehicle ?place)
				 (not (= ?person ?vehicle)))
	     :effect (and (at ?person ?place)
			   (mobile ?person)
			   (not (driving ?person ?vehicle))
			   (not (mobile ?vehicle))))
  )

(define (problem Big-bull1)
    (:domain bulldozer)
  (:objects a b c d e f g jack bulldozer)
  (:goal (and (at bulldozer g) (at jack a)))
  (:init (at jack a) (at bulldozer e)
	 (vehicle bulldozer)
	 (mobile jack)
	 (person jack)
	 (road a b) (road b a)
	 (road a e) (road e a)
	 (road e b) (road b e)
	 (road a c) (road c a)
	 (road c b) (road b c)
	 (bridge b d) (bridge d b)
	 (bridge c f) (bridge f c)
	 (road d f) (road f d)
	 (road f g) (road g f)
	 (road d g) (road g d)))

(define (problem big-bull2)
    (:domain bulldozer)
  (:objects a b c d e f g h i j k l m n
	    jack bulldozer)
  (:goal (AND (at bulldozer g)))
  (:init (at jack a) (at bulldozer e)
	 (vehicle bulldozer)
	 (mobile jack)
	 (person jack)
	 (road a b) (road b a)
	 (road a c) (road c a)
	 (road c d) (road d c)
	 (road d e) (road e d)
	 (road e j) (road j e)
	 (road d f) (road f d)
	 (road f j) (road j f)
	 (road f k) (road k f)
	 (road j h) (road h j)
	 (road h k) (road k h)
	 (bridge k l) (bridge l k)
	 (bridge k n) (bridge n k)
	 (road l m) (road m l)
	 (road m n) (road n m)
	 (road m g) (road g m)
	 (road n g) (road g n)))
		  
(define (problem Bulldozer-prob)
    (:domain bulldozer)
  (:objects a b c d e f g
	    jack bulldozer)
  (:goal (AND (at bulldozer g)))
  (:init (at jack a) (at bulldozer e)
	 (vehicle bulldozer)
	 (mobile jack)
	 (person jack)
	 (road a b) (road b a)
	 (road a e) (road e a)
	 (road e b) (road b e)
	 (road a c) (road c a)
	 (road c b) (road b c)
	 (bridge b d) (bridge d b)
	 (bridge c f) (bridge f c)
	 (road d f) (road f d)
	 (road f g) (road g f)
	 (road d g) (road g d))
  (:length (:serial 5 ) (:parallel 5))
  )

(define (problem One-way)
    (:domain bulldozer)
  (:objects a b d e g
	    jack bulldozer)
  (:goal (AND (at bulldozer g)))
  (:length (:serial 5 ) (:parallel 5))  
  (:init (at jack a) (at bulldozer e)
	 (vehicle bulldozer)
	 (mobile jack)
	 (person jack)
	 (road a e)
	 (road e b)
	 (bridge b d)
	 (road d g)))

(define (problem dumber-than-dirt)
    (:domain bulldozer)
  (:objects a g
	    jack bulldozer)
  (:goal (AND (at bulldozer g)))
  (:init (at jack a) (at bulldozer a)
	 (vehicle bulldozer)
	 (mobile jack)
	 (person jack)
	 (road a g))
  (:length (:serial 2 ) (:parallel 2)) 
  )

(define (problem Go-Jack)
    (:domain bulldozer)
  (:objects a b d e g jack)
  (:goal (AND (at Jack g)))
  (:length (:serial 4 ) (:parallel 4))
  (:init (at jack a) 
	 ;;(at bulldozer e)
	 ;; (vehicle bulldozer)
	 (mobile jack)
	 (person jack)
	 (road a e)
	 (road e b)
	 (bridge b d)
	 (road d g)))

(define (problem Get-back-Jack)
    (:domain bulldozer)
  (:objects a b c d e f g
	    jack bulldozer)
  (:goal (and (at bulldozer g) (at Jack a)))
  (:length (:serial 9 ) (:parallel 9))  
  (:init (at jack a) (at bulldozer e)
	 (vehicle bulldozer)
	 (mobile jack)
	 (person jack)
	 (road a b) (road b a)
	 (road a e) (road e a)
	 (road e b) (road b e)
	 (road a c) (road c a)
	 (road c b) (road b c)
	 (bridge b d) (bridge d b)
	 (bridge c f) (bridge f c)
	 (road d f) (road f d)
	 (road f g) (road g f)
	 (road d g) (road g d)))

(define (problem Jack-back)
    (:domain bulldozer)
  (:objects a g jack bulldozer)
  (:length (:serial 4) (:parallel 4)) 
  (:goal (and (at bulldozer g) (at Jack a)))
  (:init (at jack a) (at bulldozer a)
	 (vehicle bulldozer)
	 (mobile jack)
	 (person jack)
	 (road a g) (road g a)))

(define (problem Jack-back2)
    (:domain bulldozer)
  (:objects a e g jack bulldozer)
  (:length (:serial 6) (:parallel 6))
  (:goal (and (at bulldozer g) (at Jack a)))
  (:init (at jack a) (at bulldozer e)
	 (vehicle bulldozer)
	 (mobile jack)
	 (person jack)
	 (road a e) (road e a)
	 (road a g) (road g a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simpler domain - vehicles, but no boarding and disembarking

(define (domain road-operators)
  (:requirements :strips)
  (:predicates (at ?v ?l)
	       (road ?l1 ?l2)
	       (bridge ?l1 ?l2)
	       (place ?l)
	       (vehicle ?v))

  (:action drive
	     :parameters (?vehicle ?location1 ?location2)
	     :precondition (and (at ?vehicle ?location1) 
				 (road ?location1 ?location2))
	     :effect
	     (and (at ?vehicle ?location2)
		   (not (at ?vehicle ?location1))))
  (:action cross
	     :parameters (?vehicle ?location1 ?location2)
	     :precondition (and (at ?vehicle ?location1) 
				 (bridge ?location1 ?location2))
	     :effect
	     (and (at ?vehicle ?location2)
		   (not (at ?vehicle ?location1)))))

(define (problem road-test)
    (:domain road-operators)
  (:objects a d g car bulldozer)
  (:init (vehicle car)(vehicle bulldozer) 
	 (place a)(place d)(place g)
	 (at car a) (at bulldozer a)
	 (road d g) (road g d)
	 (bridge a d) (bridge d a))
  (:goal (and (at car g) (at bulldozer g))))

;;;UCPOP(23): (bf-control (road-test))
;;;
;;;Initial  : ((VEHICLE CAR) (VEHICLE BULLDOZER) (PLACE A) (PLACE D) (PLACE G)
;;;            (AT CAR A) (AT BULLDOZER A) (BRIDGE A D) (BRIDGE D A) (ROAD D G)
;;;            (ROAD G D))
;;;
;;;Step 1  : (CROSS CAR A D)       Created 4 
;;;           0  -> (AT CAR A)         
;;;           0  -> (BRIDGE A D)        
;;;Step 2  : (CROSS BULLDOZER A D)       Created 2 
;;;           0  -> (AT BULLDOZER A)         
;;;           0  -> (BRIDGE A D)        
;;;Step 3  : (DRIVE CAR D G)       Created 3 
;;;           4  -> (AT CAR D)         
;;;           0  -> (ROAD D G)          
;;;Step 4  : (DRIVE BULLDOZER D G)       Created 1 
;;;           2  -> (AT BULLDOZER D)         
;;;           0  -> (ROAD D G)          
;;;
;;;Goal    : (AND (AT CAR G) (AT BULLDOZER G))
;;;           3  -> (AT CAR G)         
;;;           1  -> (AT BULLDOZER G)         
;;;Complete!
;;;
;;;UCPOP (Init = 11 ; Goals = 3 ) => Win  (4 steps)     CPU 133      
;;;     Nodes (V = 20  ; Q = 7   ; C = 28  )             Branch 1.35      
;;;     Working Unifies: 177                             Bindings added: 43   
;;;NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simpler domain - no vehicles, just roads

(define (domain roads)
  (:requirements :strips :equality)
  (:predicates (road ?from ?to)
	       (at ?thing ?location))
  (:action Run
	     :parameters (?person ?from ?to)
	     :precondition (and (road ?from ?to)
				 (at ?person ?from)
				 (not (= ?from ?to)))
	     :effect (and (at ?person ?to) (not (at ?person ?from)))))

(define (problem Run-Jack)
    (:domain roads)
  (:objects Jack Microsoft Rockwell KI)
  (:length (:serial 2) (:parallel 2))  
  (:goal (AND (at Jack Microsoft)))
  (:init (at jack Rockwell)
	 (road Rockwell KI) (road KI Microsoft)))

(define (problem Run-Jack-Run)		;no solution, endless recursion
    (:domain roads)
  (:objects Jack KI Rockwell)
  (:length (:serial -1) (:parallel -1))
  (:goal (and (at Jack KI) (at Jack Rockwell)))
  (:init (at jack Rockwell)
	 (road Rockwell KI) (road KI Rockwell)))


