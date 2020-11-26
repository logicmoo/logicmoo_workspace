;;; these domains engineered from the att satplan encodings 
;;; which were derived from the graphplan domains
;;; which I think were derived from Veloso's prodigy domains
;;; - DSW 1/97

(in-package :domains)

(define (domain logistics-strips)
  (:requirements :strips :equality)
  (:predicates (obj ?o)
	       (truck ?t)
	       (at ?o ?l)
	       (in ?o ?t)
	       (airplane ?p)
	       (airport ?s)
	       (in-city ?s ?city)
	       (city ?c)
	       (location ?l))
  ;; Need separate loading operations for trucks and planes, but
  ;; single unloading operation will do.
  (:action load-truck
	     :parameters (?o ?truck ?loc)
	     :precondition (and (obj ?o)
				 (truck ?truck)
				 (at ?o ?loc)
				 (at ?truck ?loc))
	     :effect (and (not (at ?o ?loc))
			   (in ?o ?truck)))
  (:action load-plane
	     :parameters (?o ?p ?loc)
	     :precondition (and (obj ?o)
				 (airplane ?p)
				 (at ?o ?loc)
				 (at ?p ?loc))
	     :effect (and (not (at ?o ?loc))
			   (in ?o ?p)))
  (:action unload
	     :parameters (?o ?v ?loc)
	     :precondition (and (in ?o ?v)
				 (at ?v ?loc))
	     :effect (and (at ?o ?loc)
			   (not (in ?o ?v))))
  (:action fly
	     :parameters (?p ?s ?d)
	     :precondition (and (airplane ?p)
				 (airport ?s)
				 (airport ?d)
				 (at ?p ?s)
				 (not (= ?s ?d)))
	     :effect (and (at ?p ?d)
			   (not (at ?p ?s))))
  (:action drive
	     :parameters (?truck ?s ?d ?city)
	     :precondition (and (truck ?truck)
				 (at ?truck ?s)
				 (in-city ?s ?city)
				 (in-city ?d ?city)
				 (not (= ?s ?d)))
	     :effect (and (at ?truck ?d)
			   (not (at ?truck ?s)))))

;;; a three step plan works
(define (problem att-log0) 
  (:domain logistics-strips)
  (:objects package1 pgh-truck bos-truck airplane1
	    bos-po pgh-po bos-airport pgh-airport
	    pgh bos)
  (:init (OBJ package1)		; statis predicates
	 (TRUCK pgh-truck)
	 (TRUCK bos-truck)
	 (AIRPLANE airplane1)
	 (LOCATION bos-po)
	 (LOCATION pgh-po)
	 (LOCATION bos-airport)
	 (LOCATION pgh-airport)
	 (AIRPORT bos-airport)
	 (AIRPORT pgh-airport)
	 (CITY pgh)
	 (CITY bos)
	 (IN-CITY pgh-po pgh)
	 (IN-CITY pgh-airport pgh)
	 (IN-CITY bos-po bos)
	 (IN-CITY bos-airport bos)
	 (at package1 pgh-airport);; dynamic predicates
	 (at airplane1 pgh-airport)
	 (at bos-truck bos-po)
	 (at pgh-truck pgh-po))
  (:goal (and (at package1 bos-airport)))
  )


;;; this one takes six steps
(define (problem att-log1) (:domain logistics-strips)
  (:objects package1 pgh-truck bos-truck airplane1
	    bos-po pgh-po bos-airport pgh-airport
	    pgh bos)
  (:init (OBJ package1)			; statis predicates
	 (TRUCK pgh-truck)
	 (TRUCK bos-truck)
	 (AIRPLANE airplane1)
	 (LOCATION bos-po)
	 (LOCATION pgh-po)
	 (LOCATION bos-airport)
	 (LOCATION pgh-airport)
	 (AIRPORT bos-airport)
	 (AIRPORT pgh-airport)
	 (CITY pgh)
	 (CITY bos)
	 (IN-CITY pgh-po pgh)
	 (IN-CITY pgh-airport pgh)
	 (IN-CITY bos-po bos)
	 (IN-CITY bos-airport bos)
	 (at package1 pgh-po);; dynamic predicates
	 (at airplane1 pgh-airport)
	 (at bos-truck bos-po)
	 (at pgh-truck pgh-po))
  (:goal (and (at package1 bos-airport))))

;;; this one takes nine steps
(define (problem att-log2) (:domain logistics-strips)
  (:objects package1 pgh-truck bos-truck airplane1
	    bos-po pgh-po bos-airport pgh-airport
	    pgh bos)
  (:init (OBJ package1)		; statis predicates
	 (TRUCK pgh-truck)
	 (TRUCK bos-truck)
	 (AIRPLANE airplane1)
	 (LOCATION bos-po)
	 (LOCATION pgh-po)
	 (LOCATION bos-airport)
	 (LOCATION pgh-airport)
	 (AIRPORT bos-airport)
	 (AIRPORT pgh-airport)
	 (CITY pgh)
	 (CITY bos)
	 (IN-CITY pgh-po pgh)
	 (IN-CITY pgh-airport pgh)
	 (IN-CITY bos-po bos)
	 (IN-CITY bos-airport bos)
	 (at package1 pgh-po);; dynamic predicates
	 (at airplane1 pgh-airport)
	 (at bos-truck bos-airport)
	 (at pgh-truck pgh-po))
  (:goal (and (at package1 bos-po))))

;;; ten steps
(define (problem att-log3) 
  (:domain logistics-strips)
  (:objects package1 package2 package3 package4 
	    package5 package6 package7 package8
	    pgh-truck bos-truck la-truck airplane1 airplane2
	    bos-po pgh-po la-po bos-airport pgh-airport la-airport
	    pgh bos la)
  (:init (OBJ package1)		; statis predicates
	 (OBJ package2)
	 (OBJ package3)
	 (OBJ package4)
	 (OBJ package5)
	 (OBJ package6)
	 (OBJ package7)
	 (OBJ package8)
	 (TRUCK pgh-truck)
	 (TRUCK bos-truck)
	 (TRUCK la-truck)
	 (AIRPLANE airplane1)
	 (AIRPLANE airplane2)
	 (LOCATION bos-po)
	 (LOCATION la-po)
	 (LOCATION pgh-po)
	 (LOCATION bos-airport)
	 (LOCATION la-airport)
	 (LOCATION pgh-airport)
	 (AIRPORT bos-airport)
	 (AIRPORT pgh-airport)
	 (AIRPORT la-airport)
	 (CITY pgh)
	 (CITY bos)
	 (CITY la)
	 (IN-CITY pgh-po pgh)
	 (IN-CITY pgh-airport pgh)
	 (IN-CITY bos-po bos)
	 (IN-CITY bos-airport bos)
	 (IN-CITY la-po la)
	 (IN-CITY la-airport la)
	 (at package1 pgh-po);; dynamic predicates
	 (at package2 pgh-po)
	 (at package3 pgh-po)
	 (at package4 pgh-po)
	 (at package5 bos-po)
	 (at package6 bos-po)
	 (at package7 bos-po)
	 (at package8 la-po)
	 (at airplane1 pgh-airport)
	 (at airplane2 pgh-airport)
	 (at bos-truck bos-po)
	 (at pgh-truck pgh-po)
	 (at la-truck la-po))
  (:goal (and (at package1 bos-po))))

(define (problem att-log4) 
  (:domain logistics-strips)
  (:objects package1 package2 package3 package4 
	    package5 package6 package7 package8
	    pgh-truck bos-truck la-truck airplane1 airplane2
	    bos-po pgh-po la-po bos-airport pgh-airport la-airport
	    pgh bos la)
  (:init (OBJ package1)		; statis predicates
	 (OBJ package2)
	 (OBJ package3)
	 (OBJ package4)
	 (OBJ package5)
	 (OBJ package6)
	 (OBJ package7)
	 (OBJ package8)
	 (TRUCK pgh-truck)
	 (TRUCK bos-truck)
	 (TRUCK la-truck)
	 (AIRPLANE airplane1)
	 (AIRPLANE airplane2)
	 (LOCATION bos-po)
	 (LOCATION la-po)
	 (LOCATION pgh-po)
	 (LOCATION bos-airport)
	 (LOCATION la-airport)
	 (LOCATION pgh-airport)
	 (AIRPORT bos-airport)
	 (AIRPORT pgh-airport)
	 (AIRPORT la-airport)
	 (CITY pgh)
	 (CITY bos)
	 (CITY la)
	 (IN-CITY pgh-po pgh)
	 (IN-CITY pgh-airport pgh)
	 (IN-CITY bos-po bos)
	 (IN-CITY bos-airport bos)
	 (IN-CITY la-po la)
	 (IN-CITY la-airport la)
	 (at package1 pgh-po);; dynamic predicates
	 (at package2 pgh-po)
	 (at package3 pgh-po)
	 (at package4 pgh-po)
	 (at package5 bos-po)
	 (at package6 bos-po)
	 (at package7 bos-po)
	 (at package8 la-po)
	 (at airplane1 pgh-airport)
	 (at airplane2 pgh-airport)
	 (at bos-truck bos-po)
	 (at pgh-truck pgh-po)
	 (at la-truck la-po))
  (:goal (and (at package1 bos-po)
	      (at package2 bos-airport))))

(define (problem att-log-a) 
  (:domain logistics-strips)
  (:objects package1 package2 package3 package4 
	    package5 package6 package7 package8
	    pgh-truck bos-truck la-truck airplane1 airplane2
	    bos-po pgh-po la-po bos-airport pgh-airport la-airport
	    pgh bos la)
  (:init (OBJ package1)		; statis predicates
	 (OBJ package2)
	 (OBJ package3)
	 (OBJ package4)
	 (OBJ package5)
	 (OBJ package6)
	 (OBJ package7)
	 (OBJ package8)
	 (TRUCK pgh-truck)
	 (TRUCK bos-truck)
	 (TRUCK la-truck)
	 (AIRPLANE airplane1)
	 (AIRPLANE airplane2)
	 (LOCATION bos-po)
	 (LOCATION la-po)
	 (LOCATION pgh-po)
	 (LOCATION bos-airport)
	 (LOCATION la-airport)
	 (LOCATION pgh-airport)
	 (AIRPORT bos-airport)
	 (AIRPORT pgh-airport)
	 (AIRPORT la-airport)
	 (CITY pgh)
	 (CITY bos)
	 (CITY la)
	 (IN-CITY pgh-po pgh)
	 (IN-CITY pgh-airport pgh)
	 (IN-CITY bos-po bos)
	 (IN-CITY bos-airport bos)
	 (IN-CITY la-po la)
	 (IN-CITY la-airport la)
	 (at package1 pgh-po);; dynamic predicates
	 (at package2 pgh-po)
	 (at package3 pgh-po)
	 (at package4 pgh-po)
	 (at package5 bos-po)
	 (at package6 bos-po)
	 (at package7 bos-po)
	 (at package8 la-po)
	 (at airplane1 pgh-airport)
	 (at airplane2 pgh-airport)
	 (at bos-truck bos-po)
	 (at pgh-truck pgh-po)
	 (at la-truck la-po))
  (:goal (and (at package1 bos-po)
	      (at package2 bos-airport)
	      (at package3 la-po)
	      (at package4 la-airport)
	      (at package5 pgh-po)
	      (at package6 pgh-airport)
	      (at package7 pgh-po)
	      (at package8 pgh-po)))
  )


;; ATT-LOGISTICS treats trucks and airplanes
;;fundamentally differently (both loading and unloading).
;;
;; LOGISTICS-TYPED does the same, but it is typed.
;; 
;; LOGISTICS-AXIOM introduces the axiom that says
;; they are both vehicles, and treats them
;; uniformly.
;; 
;; LOGISTICS-ADL treats them uniformly as well. 
;; Whereas TYPED and AXIOM treat a thing as not AT
;; a location until it is unloaded, LOGISTICS uses
;; the conditional effect.

(define ( domain att-logistics) 
    (:requirements :strips :equality)
    
    (:predicates (obj ?o)
	       (truck ?t)
	       (at ?o ?l)
	       (in ?o ?t)
	       (airplane ?p)
	       (airport ?s)
	       (in-city ?s ?city)
	       (city ?c)
	       (location ?l))

    (:action load-truck
       :parameters (?o ?truck ?loc)
       :precondition (AND (obj ?o)
                          (truck ?truck)
			  (location ?loc)
                          (at ?o ?loc)
                          (at ?truck ?loc))
       :effect (AND (NOT (at ?o ?loc))
                    (in ?o ?truck)))
    (:action load-plane
       :parameters (?o ?p ?loc)
       :precondition (AND (obj ?o)
                          (airplane ?p)
			  (airport ?loc) 
                          (at ?o ?loc)
                          (at ?p ?loc))
       :effect (AND (NOT (at ?o ?loc))
                    (in ?o ?p)))
    (:action unload-truck
       :parameters (?o ?truck ?loc)
       :precondition (AND
		      (obj ?o)	  
		      (location ?loc)	  
		      (truck ?truck)      
		      (in ?o ?truck)
		      (at ?truck ?loc))
       :effect (AND (at ?o ?loc)
                    (NOT (in ?o ?truck))))
    (:action unload-plane
       :parameters (?o ?p ?loc)
       :precondition (AND
		      (obj ?o)	
		      (location ?loc)	
		      (airplane ?p)     
		      (in ?o ?p)
		      (at ?p ?loc))
       :effect (AND (at ?o ?loc)
                    (NOT (in ?o ?p))))
    (:action fly
       :parameters (?p ?s ?d)
       :precondition (AND (airplane ?p)
                          (airport ?s)
                          (airport ?d)
                          (at ?p ?s)
                          (not (= ?s ?d)))
       :effect (AND (at ?p ?d)
                    (NOT (at ?p ?s))))
    (:action drive
       :parameters (?truck ?s ?d ?city)
       :precondition (AND (truck ?truck)
			  (location ?s)	
			  (location ?d)	
			  (city ?city)  
                          (at ?truck ?s)
                          (in-city ?s ?city)
                          (in-city ?d ?city)
                          (not (= ?s ?d)))
       :effect (AND (at ?truck ?d)
                    (NOT (at ?truck ?s)))))

;;; a three step plan works
(define (problem att-logistics0) (:domain att-logistics)
  (:objects package1 pgh-truck bos-truck airplane1
	    bos-po pgh-po bos-airport pgh-airport
	    pgh bos)
  (:init (OBJ package1)		; statis predicates
	 (TRUCK pgh-truck)
	 (TRUCK bos-truck)
	 (AIRPLANE airplane1)
	 (LOCATION bos-po)
	 (LOCATION pgh-po)
	 (LOCATION bos-airport)
	 (LOCATION pgh-airport)
	 (AIRPORT bos-airport)
	 (AIRPORT pgh-airport)
	 (CITY pgh)
	 (CITY bos)
	 (IN-CITY pgh-po pgh)
	 (IN-CITY pgh-airport pgh)
	 (IN-CITY bos-po bos)
	 (IN-CITY bos-airport bos)
	 (at package1 pgh-airport)    ;; dynamic predicates
	 (at airplane1 pgh-airport)
	 (at bos-truck bos-po)
	 (at pgh-truck pgh-po))
  (:goal (AND (at package1 bos-airport)))
  (:length (:serial 3) (:parallel 3)))

;;; this one takes six steps
(define (problem att-logistics1) (:domain att-logistics)
  (:objects package1 pgh-truck bos-truck airplane1
	    bos-po pgh-po bos-airport pgh-airport
	    pgh bos)
  (:init  (OBJ package1)		; static predicates
          (TRUCK pgh-truck)
          (TRUCK bos-truck)
          (AIRPLANE airplane1)
          (LOCATION bos-po)
          (LOCATION pgh-po)
          (LOCATION bos-airport)
          (LOCATION pgh-airport)
          (AIRPORT bos-airport)
          (AIRPORT pgh-airport)
          (CITY pgh)
          (CITY bos)
          (IN-CITY pgh-po pgh)
          (IN-CITY pgh-airport pgh)
          (IN-CITY bos-po bos)
          (IN-CITY bos-airport bos)
          (at package1 pgh-po)    ;; dynamic predicates
          (at airplane1 pgh-airport)
          (at bos-truck bos-po)
          (at pgh-truck pgh-po))
  (:goal (AND (at package1 bos-airport)))
  (:length (:serial 6) (:parallel 6)))

;;; this one takes nine steps
(define (problem att-logistics2) (:domain att-logistics)
  (:objects package1 pgh-truck bos-truck airplane1
	    bos-po pgh-po bos-airport pgh-airport
	    pgh bos)
  (:init  (OBJ package1)		; statis predicates
          (TRUCK pgh-truck)
          (TRUCK bos-truck)
          (AIRPLANE airplane1)
          (LOCATION bos-po)
          (LOCATION pgh-po)
          (LOCATION bos-airport)
          (LOCATION pgh-airport)
          (AIRPORT bos-airport)
          (AIRPORT pgh-airport)
          (CITY pgh)
          (CITY bos)
          (IN-CITY pgh-po pgh)
          (IN-CITY pgh-airport pgh)
          (IN-CITY bos-po bos)
          (IN-CITY bos-airport bos)
          (at package1 pgh-po)    ;; dynamic predicates
          (at airplane1 pgh-airport)
          (at bos-truck bos-airport)
          (at pgh-truck pgh-po))
  (:goal (AND (at package1 bos-po)))
  (:length (:serial 9) (:parallel 9)))

(define (problem att-logistics2.5) (:domain att-logistics)
  (:objects package1 pgh-truck bos-truck la-truck airplane1 airplane2
	    bos-po pgh-po la-po bos-airport pgh-airport la-airport
	    pgh bos la)
  (:init  (OBJ package1)		; statis predicates
          (TRUCK pgh-truck)
          (TRUCK bos-truck)
          (TRUCK la-truck)
          (AIRPLANE airplane1)
          (AIRPLANE airplane2)
          (LOCATION bos-po)
          (LOCATION la-po)
          (LOCATION pgh-po)
          (LOCATION bos-airport)
          (LOCATION la-airport)
          (LOCATION pgh-airport)
          (AIRPORT bos-airport)
          (AIRPORT pgh-airport)
          (AIRPORT la-airport)
          (CITY pgh)
          (CITY bos)
          (CITY la)
          (IN-CITY pgh-po pgh)
          (IN-CITY pgh-airport pgh)
          (IN-CITY bos-po bos)
          (IN-CITY bos-airport bos)
          (IN-CITY la-po la)
          (IN-CITY la-airport la)
          (at package1 pgh-po)    ;; dynamic predicates
          (at airplane1 pgh-airport)
          (at airplane2 pgh-airport)
          (at bos-truck bos-po)
          (at pgh-truck pgh-po)
          (at la-truck la-po))
  (:goal (AND (at package1 bos-po)))
  (:length (:serial 10) (:parallel 9))
  )

;;; ten steps
(define (problem att-logistics3) (:domain att-logistics)
  (:objects package1 package2 package3 package4 
	    package5 package6 package7 package8
	    pgh-truck bos-truck la-truck airplane1 airplane2
	    bos-po pgh-po la-po bos-airport pgh-airport la-airport
	    pgh bos la)
  (:init  (OBJ package1)		; statis predicates
          (OBJ package2)
          (OBJ package3)
          (OBJ package4)
          (OBJ package5)
          (OBJ package6)
          (OBJ package7)
          (OBJ package8)
          (TRUCK pgh-truck)
          (TRUCK bos-truck)
          (TRUCK la-truck)
          (AIRPLANE airplane1)
          (AIRPLANE airplane2)
          (LOCATION bos-po)
          (LOCATION la-po)
          (LOCATION pgh-po)
          (LOCATION bos-airport)
          (LOCATION la-airport)
          (LOCATION pgh-airport)
          (AIRPORT bos-airport)
          (AIRPORT pgh-airport)
          (AIRPORT la-airport)
          (CITY pgh)
          (CITY bos)
          (CITY la)
          (IN-CITY pgh-po pgh)
          (IN-CITY pgh-airport pgh)
          (IN-CITY bos-po bos)
          (IN-CITY bos-airport bos)
          (IN-CITY la-po la)
          (IN-CITY la-airport la)
          (at package1 pgh-po)    ;; dynamic predicates
          (at package2 pgh-po)
          (at package3 pgh-po)
          (at package4 pgh-po)
          (at package5 bos-po)
          (at package6 bos-po)
          (at package7 bos-po)
          (at package8 la-po)
          (at airplane1 pgh-airport)
          (at airplane2 pgh-airport)
          (at bos-truck bos-po)
          (at pgh-truck pgh-po)
          (at la-truck la-po))
  (:goal (AND (at package1 bos-po)))
  (:length (:serial 10) (:parallel 9))
  )

(define (problem att-logistics-a) (:domain att-logistics)
  (:objects package1 package2 package3 package4 
	    package5 package6 package7 package8
	    pgh-truck bos-truck la-truck airplane1 airplane2
	    bos-po pgh-po la-po bos-airport pgh-airport la-airport
	    pgh bos la)
  (:init  (OBJ package1)		; statis predicates
          (OBJ package2)
          (OBJ package3)
          (OBJ package4)
          (OBJ package5)
          (OBJ package6)
          (OBJ package7)
          (OBJ package8)
          (TRUCK pgh-truck)
          (TRUCK bos-truck)
          (TRUCK la-truck)
          (AIRPLANE airplane1)
          (AIRPLANE airplane2)
          (LOCATION bos-po)
          (LOCATION la-po)
          (LOCATION pgh-po)
          (LOCATION bos-airport)
          (LOCATION la-airport)
          (LOCATION pgh-airport)
          (AIRPORT bos-airport)
          (AIRPORT pgh-airport)
          (AIRPORT la-airport)
          (CITY pgh)
          (CITY bos)
          (CITY la)
          (IN-CITY pgh-po pgh)
          (IN-CITY pgh-airport pgh)
          (IN-CITY bos-po bos)
          (IN-CITY bos-airport bos)
          (IN-CITY la-po la)
          (IN-CITY la-airport la)
          (at package1 pgh-po)    ;; dynamic predicates
          (at package2 pgh-po)
          (at package3 pgh-po)
          (at package4 pgh-po)
          (at package5 bos-po)
          (at package6 bos-po)
          (at package7 bos-po)
          (at package8 la-po)
          (at airplane1 pgh-airport)
          (at airplane2 pgh-airport)
          (at bos-truck bos-po)
          (at pgh-truck pgh-po)
          (at la-truck la-po))
  (:goal (AND (at package1 bos-po)
	      (at package2 bos-airport)
	      (at package3 la-po)
	      (at package4 la-airport)
	      (at package5 pgh-po)
	      (at package6 pgh-airport)
	      (at package7 pgh-po)
	      (at package8 pgh-po)))
  (:length (:serial 11) (:parallel 11)) 
  )



(define (DOMAIN LOGISTICS-TYPED)
  (:requirements :strips :typing)
  (:types physobj - object
	  obj truck airplane - physobj
	  location city - object)
  (:predicates (at ?x - physobj ?l - location)
	       (in ?x - obj ?t - (either truck airplane))
	       (loc-at ?l - location ?c - city))

  (:action load-truck 
	     :parameters (?obj - obj ?truck - truck ?loc - location) 
	     :precondition (and (at ?truck ?loc) (at ?obj ?loc)) 
	     :effect (and (in ?obj ?truck) (not (at ?obj ?loc))))
  (:action load-airplane 
	     :parameters (?obj - obj ?airplane - airplane ?loc - location) 
	     :precondition (and (at ?obj ?loc) (at ?airplane ?loc)) 
	     :effect (and (in ?obj ?airplane) (not (at ?obj ?loc))))
  (:action unload-truck 
	     :parameters (?obj - obj ?truck - truck ?loc - location) 
	     :precondition (and (at ?truck ?loc) (in ?obj ?truck)) 
	     :effect (and (at ?obj ?loc) (not (in ?obj ?truck))))
  (:action unload-airplane 
	     :parameters (?obj - obj ?airplane - airplane ?loc - location) 
	     :precondition (and (in ?obj ?airplane) (at ?airplane ?loc)) 
	     :effect (and (at ?obj ?loc) (not (in ?obj ?airplane))))
  (:action drive-truck 
	     :parameters (?truck - truck ?loc-from ?loc-to - location
			  ?city - city)
	     :precondition (and (at ?truck ?loc-from) 
				 (loc-at ?loc-from ?city) 
				 (loc-at ?loc-to ?city))
	     :effect (and (at ?truck ?loc-to) (not (at ?truck ?loc-from))))
  (:action fly-airplane 
	     :parameters (?airplane - airplane ?loc-from ?loc-to - location)
	     :precondition (and (at ?airplane ?loc-from)) 
	     :effect (and (at ?airplane ?loc-to) 
			   (not (at ?airplane ?loc-from)))))

(define (DOMAIN LOGISTICS-AXIOM)
  (:requirements :strips :domain-axioms :disjunctive-preconditions)

  (:predicates (obj ?obj)
	       (vehicle ?x)
	       (truck ?t)
	       (airplane ?a)
	       (location ?l)
	       (at ?x ?l)
	       (in ?x ?v)
	       (loc-at ?l ?city)
	       (city ?c))
  
  (:axiom 
          :vars (?vehicle)
	  :context (or (truck ?vehicle)
		       (airplane ?vehicle))
	  :implies (vehicle ?vehicle))

  (:action load 
	     :parameters (?obj ?airplane ?loc) 
	     :precondition (and (obj ?obj) (vehicle ?airplane) (location ?loc) 
				(at ?obj ?loc) (at ?airplane ?loc)) 
	     :effect (and (in ?obj ?airplane) (not (at ?obj ?loc))))
  (:action unload 
	     :parameters (?obj ?airplane ?loc) 
	     :precondition (and (obj ?obj) (vehicle ?airplane) (location ?loc)
				(in ?obj ?airplane) (at ?airplane ?loc)) 
	     :effect (and (at ?obj ?loc) (not (in ?obj ?airplane))))
  (:action go 
	     :parameters (?vehicle ?loc-from ?loc-to ?city)
	     :precondition (and (vehicle ?vehicle) (location ?loc-from) 
				(location ?loc-to) (city ?city)
				(at ?vehicle ?loc-from) 
				(loc-at ?loc-from ?city) 
				(loc-at ?loc-to ?city))
	     :effect (and (at ?vehicle ?loc-to) 
			   (not (at ?vehicle ?loc-from)))))

(define (DOMAIN LOGISTICS-ADL)
  (:requirements :adl :domain-axioms)
  (:types physobj - object
	  obj vehicle - physobj
	  truck airplane - vehicle
	  location city - object
	  airport - location)
  (:predicates (at ?x - physobj ?l - location)
	       (in ?x - obj ?t - vehicle)
	       (loc-at ?l - location ?c - city))

;;; Unnecessary, because of the type inheritance (trucks and airplanes 
;;; are vehicles).
;;;  (:axiom is-vehicle
;;;	  :context (:or (truck ?vehicle)
;;;			(airplane ?vehicle))
;;;	  :implies (vehicle ?vehicle))
  (:action load 
	     :parameters (?obj - obj ?airplane - vehicle ?loc - location) 
	     :precondition (and (at ?obj ?loc) (at ?airplane ?loc)) 
	     :effect (and (in ?obj ?airplane)))
  (:action unload 
	     :parameters (?obj - obj ?airplane - vehicle ?loc - location) 
	     :precondition (and (in ?obj ?airplane) (at ?airplane ?loc)) 
	     :effect (and (not (in ?obj ?airplane))))
  (:action go 
	   :parameters (?vehicle - vehicle ?loc-from ?loc-to - location
			?city - city)
	     :precondition (and (at ?vehicle ?loc-from) 
				 (loc-at ?loc-from ?city) 
				 (loc-at ?loc-to ?city))
	     :effect  (and (at ?vehicle ?loc-to) 
			    (not (at ?vehicle ?loc-from))		  
			    (forall (?x - obj)
				     (when (and (in ?x ?vehicle))
					    (and (not (at ?x ?loc-from)) 
						  (at ?x ?loc-to)))))))

(define (problem logistics1)
  (:domain logistics-adl)
  (:objects package1 package2 package3 package4 package5 package6 - obj
	    pgh-truck bos-truck la-truck - truck
	    airplane1 airplane2 - airplane
	    bos-po la-po pgh-po - location
	    bos-airport pgh-airport la-airport - airport
	    pgh bos la - city)

  (:init (at package1 pgh-po)
	 (at package2 pgh-po)
	 (at package3 pgh-po)
	 (at package4 pgh-po)
	 (at package5 pgh-po)
	 (at package6 la-po)
	 (at airplane1 pgh-airport)
	 (at airplane2 pgh-airport)
	 (at bos-truck bos-po)
	 (at pgh-truck pgh-po)
	 (at la-truck la-po)
	 (loc-at pgh-po pgh)
	 (loc-at pgh-airport pgh)
	 (loc-at bos-po bos)
	 (loc-at bos-airport bos)
	 (loc-at la-po la)
	 (loc-at la-airport la))
  (:goal (and (at package1 bos-po)
	      (at package2 la-po)
	      (at package3 la-po)
	      (at package4 la-airport)
	      (at package5 bos-po)
	      (at package6 pgh-po)
	      )))

(define (problem logistics2)
  (:domain logistics-adl)
  (:objects package1 package2 package3 package5 package7 - obj
	    pgh-truck bos-truck la-truck ny-truck - truck
	    airplane1 airplane2 - airplane
	    bos-po la-po pgh-po ny-po - location
	    bos-airport pgh-airport la-airport ny-airport - airport
	    pgh bos la ny - city)
  (:init (at package1 pgh-po)
	 (at package2 pgh-po)
	 (at package3 pgh-po)
	 (at package5 bos-po)
	 (at package7 ny-po)
	 (at airplane1 pgh-airport)
	 (at airplane2 pgh-airport)
	 (at bos-truck bos-po)
	 (at pgh-truck pgh-po)
	 (at la-truck la-po)
	 (at ny-truck ny-po)
	 (loc-at pgh-po pgh)
	 (loc-at pgh-airport pgh)
	 (loc-at bos-po bos)
	 (loc-at bos-airport bos)
	 (loc-at la-po la)
	 (loc-at la-airport la)
	 (loc-at ny-po ny)
	 (loc-at ny-airport ny))
  (:goal (and (at package1 bos-po)
	      (at package2 ny-po)
	      (at package3 la-po)
	      (at package5 pgh-po)
	      (at package7 pgh-po)))
  )

(define (problem logistics3)
  (:domain logistics-adl)
  (:objects package1 package2 package3 package4 
	    package5 package6 package7 - obj
	    pgh-truck bos-truck la-truck ny-truck - truck
	    airplane1 airplane2 - airplane
	    bos-po la-po pgh-po ny-po - location
	    bos-airport pgh-airport la-airport ny-airport - airport
	    pgh bos la ny - city)
  (:init (at package1 pgh-po)
	 (at package2 pgh-po)
	 (at package3 pgh-po)
	 (at package4 ny-po)
	 (at package5 bos-po)
	 (at package6 bos-po)
	 (at package7 ny-po)
	 (at airplane1 pgh-airport)
	 (at airplane2 pgh-airport)
	 (at bos-truck bos-po)
	 (at pgh-truck pgh-po)
	 (at la-truck la-po)
	 (at ny-truck ny-po)
	 (loc-at pgh-po pgh)
	 (loc-at pgh-airport pgh)
	 (loc-at bos-po bos)
	 (loc-at bos-airport bos)
	 (loc-at la-po la)
	 (loc-at la-airport la)
	 (loc-at ny-po ny)
	 (loc-at ny-airport ny))
  (:goal (and
	  (at package1 bos-po)
	  (at package2 ny-po)
	  (at package3 la-po)
	  (at package4 la-airport)
	  (at package5 pgh-po)
	  (at package6 ny-airport)
	  (at package7 pgh-po)))
    )






