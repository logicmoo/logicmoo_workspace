; Revised by DVM, 6/3/1998 so that all domains have consistent names.
; The only difference in the logic is the use of conditional effects
; in the adl version, so that an object is at a location before it is
; unloaded.

;;; these domains engineered from the att satplan encodings 
;;; which were derived from the graphplan domains
;;; which I think were derived from Veloso's prodigy domains
;;; - DSW 1/97

(define (domain logistics-strips)
  (:requirements :strips) 
  (:predicates 	(OBJ ?obj)
	       	(TRUCK ?truck)
               	(LOCATION ?loc)
		(AIRPLANE ?airplane)
                (CITY ?city)
                (AIRPORT ?airport)
		(at ?obj ?loc)
		(in ?obj1 ?obj2)
		(in-city ?obj ?city))
 
  ; (:types )		; default object

(:action LOAD-TRUCK
  :parameters
   (?obj
    ?truck
    ?loc)
  :precondition
   (and (OBJ ?obj) (TRUCK ?truck) (LOCATION ?loc)
   (at ?truck ?loc) (at ?obj ?loc))
  :effect
   (and (not (at ?obj ?loc)) (in ?obj ?truck)))

(:action LOAD-AIRPLANE
  :parameters
   (?obj
    ?airplane
    ?loc)
  :precondition
   (and (OBJ ?obj) (AIRPLANE ?airplane) (LOCATION ?loc)
   (at ?obj ?loc) (at ?airplane ?loc))
  :effect
   (and (not (at ?obj ?loc)) (in ?obj ?airplane)))

(:action UNLOAD-TRUCK
  :parameters
   (?obj
    ?truck
    ?loc)
  :precondition
   (and (OBJ ?obj) (TRUCK ?truck) (LOCATION ?loc)
        (at ?truck ?loc) (in ?obj ?truck))
  :effect
   (and (not (in ?obj ?truck)) (at ?obj ?loc)))

(:action UNLOAD-AIRPLANE
  :parameters
   (?obj
    ?airplane
    ?loc)
  :precondition
   (and (OBJ ?obj) (AIRPLANE ?airplane) (LOCATION ?loc)
        (in ?obj ?airplane) (at ?airplane ?loc))
  :effect
   (and (not (in ?obj ?airplane)) (at ?obj ?loc)))

(:action DRIVE-TRUCK
  :parameters
   (?truck
    ?loc-from
    ?loc-to
    ?city)
  :precondition
   (and (TRUCK ?truck) (LOCATION ?loc-from) (LOCATION ?loc-to) (CITY ?city)
   (at ?truck ?loc-from)
   (in-city ?loc-from ?city)
   (in-city ?loc-to ?city))
  :effect
   (and (not (at ?truck ?loc-from)) (at ?truck ?loc-to)))

(:action FLY-AIRPLANE
  :parameters
   (?airplane
    ?loc-from
    ?loc-to)
  :precondition
   (and (AIRPLANE ?airplane) (AIRPORT ?loc-from) (AIRPORT ?loc-to)
	(at ?airplane ?loc-from))
  :effect
   (and (not (at ?airplane ?loc-from)) (at ?airplane ?loc-to)))
)

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

  (:action load 
	     :parameters (?obj - obj ?airplane - vehicle ?loc - location) 
	     :precondition (and (at ?obj ?loc) (at ?airplane ?loc)) 
	     :effect (and (in ?obj ?airplane)))
  (:action unload 
	     :parameters (?obj - obj ?airplane - vehicle ?loc - location) 
	     :precondition (and (in ?obj ?airplane) (at ?airplane ?loc)) 
	     :effect (and (not (in ?obj ?airplane))))
  (:action drive-truck 
	   :parameters (?truck - truck ?loc-from ?loc-to - location
			?city - city)
	     :precondition (and (at ?truck ?loc-from) 
				 (loc-at ?loc-from ?city) 
				 (loc-at ?loc-to ?city))
	     :effect  (and (at ?truck ?loc-to) 
			    (not (at ?truck ?loc-from))		  
			    (forall (?x - obj)
				     (when (and (in ?x ?truck))
					    (and (not (at ?x ?loc-from)) 
						  (at ?x ?loc-to))))))
  (:action fly-airplane
   :parameters (?plane - airplane ?loc-from ?loc-to - airport)
   :precondition (and (at ?plane ?loc-from) )
   :effect  (and (at ?plane ?loc-to) 
		      (not (at ?plane ?loc-from))		  
			      (forall (?x - obj)
				       (when (and (in ?x ?plane))
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






;; original name logistics.easy
;; (:length (:parallel 9))
;; optimal
;;

(define (problem log01)
    (:domain logistics-strips)
    (:objects
        package1
        package2
        package3

        airplane1
        airplane2

        pgh
        bos
        la

        pgh-truck
        bos-truck
        la-truck

        pgh-po
        bos-po
        la-po

        pgh-central
        bos-central
        la-central

        pgh-airport
        bos-airport
        la-airport
    )
    (:init
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)

        (AIRPLANE airplane1)
        (AIRPLANE airplane2)

        (CITY pgh)
        (CITY bos)
        (CITY la)

        (TRUCK pgh-truck)
        (TRUCK bos-truck)
        (TRUCK la-truck)

        (LOCATION bos-po)
        (LOCATION la-po)
        (LOCATION pgh-po)

        (LOCATION bos-central)
        (LOCATION la-central)
        (LOCATION pgh-central)

        (AIRPORT bos-airport)
        (LOCATION bos-airport)
        (AIRPORT pgh-airport)
        (LOCATION pgh-airport)
        (AIRPORT la-airport)
        (LOCATION la-airport)

        (in-city pgh-po pgh)
        (in-city pgh-airport pgh)
        (in-city pgh-central pgh)

        (in-city bos-po bos)
        (in-city bos-airport bos)
        (in-city bos-central bos)

        (in-city la-po la)
        (in-city la-airport la)
        (in-city la-central la)

        (at package1 pgh-po)
        (at package2 pgh-po)
        (at package3 pgh-po)

        (at airplane1 pgh-airport)
        (at airplane2 pgh-airport)

        (at bos-truck bos-po)
        (at pgh-truck pgh-po)
        (at la-truck la-po)

    )
    (:goal (and
        (at package1 bos-po)
        (at package2 la-po)
        (at package3 bos-po)
    ))
)

;; original name logistics.fact4pp
;; (:length (:parallel 9))
;; optimal
;;

(define (problem log02)
    (:domain logistics-strips)
    (:objects
        package1
        package2
        package3
        package4

        airplane1
        airplane2

        pgh
        bos
        la

        pgh-truck
        bos-truck
        la-truck

        pgh-po
        bos-po
        la-po

        pgh-central
        bos-central
        la-central

        pgh-airport
        bos-airport
        la-airport
    )
    (:init
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)

        (AIRPLANE airplane1)
        (AIRPLANE airplane2)

        (CITY pgh)
        (CITY bos)
        (CITY la)

        (TRUCK pgh-truck)
        (TRUCK bos-truck)
        (TRUCK la-truck)

        (LOCATION bos-po)
        (LOCATION la-po)
        (LOCATION pgh-po)

        (LOCATION bos-central)
        (LOCATION la-central)
        (LOCATION pgh-central)

        (AIRPORT bos-airport)
        (LOCATION bos-airport)
        (AIRPORT pgh-airport)
        (LOCATION pgh-airport)
        (AIRPORT la-airport)
        (LOCATION la-airport)

        (in-city pgh-po pgh)
        (in-city pgh-airport pgh)
        (in-city pgh-central pgh)

        (in-city bos-po bos)
        (in-city bos-airport bos)
        (in-city bos-central bos)

        (in-city la-po la)
        (in-city la-airport la)
        (in-city la-central la)

        (at package1 pgh-po)
        (at package2 pgh-po)
        (at package3 pgh-po)
        (at package4 pgh-po)
       
        (at airplane1 pgh-airport)
        (at airplane2 pgh-airport)
       
        (at bos-truck bos-po)
        (at pgh-truck pgh-po)
        (at la-truck la-po)

    )
    (:goal (and
        (at package1 bos-po)
        (at package2 la-airport)
        (at package3 bos-po)
        (at package4 la-airport))
    ))
;; original name logistics.a
;; extended version of logistics_facts7h
;; (:length (:parallel 11))
;; optimal
;; #actions 54 #states 10^11
;;
;; note: by going to a non-typed representation
;;       of the problems, the instances become (somewhat)
;;       harder to solve.
;;       (larger propositional representation)
;;

(define (problem log03)
    (:domain logistics-strips)
    (:objects
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8

        airplane1
        airplane2

        pgh
        bos
        la

        pgh-truck
        bos-truck
        la-truck

        pgh-po
        bos-po
        la-po

        pgh-central
        bos-central
        la-central

        pgh-airport
        bos-airport
        la-airport
    )
    (:init
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)

        (AIRPLANE airplane1)
        (AIRPLANE airplane2)

        (CITY pgh)
        (CITY bos)
        (CITY la)

        (TRUCK pgh-truck)
        (TRUCK bos-truck)
        (TRUCK la-truck)

        (LOCATION bos-po)
        (LOCATION la-po)
        (LOCATION pgh-po)

        (LOCATION bos-central)
        (LOCATION la-central)
        (LOCATION pgh-central)

        (AIRPORT bos-airport)
        (LOCATION bos-airport)
        (AIRPORT pgh-airport)
        (LOCATION pgh-airport)
        (AIRPORT la-airport)
        (LOCATION la-airport)

        (in-city pgh-po pgh)
        (in-city pgh-airport pgh)
        (in-city pgh-central pgh)

        (in-city bos-po bos)
        (in-city bos-airport bos)
        (in-city bos-central bos)

        (in-city la-po la)
        (in-city la-airport la)
        (in-city la-central la)

        (at package1 pgh-po)
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
        (at la-truck la-po)
    )
    (:goal (and
        (at package1 bos-po)
        (at package2 bos-airport)
        (at package3 la-po)
        (at package4 la-airport)
        (at package5 pgh-po)
        (at package6 pgh-airport)
        (at package7 pgh-po)
        (at package8 pgh-po)
    ))
)
;; original name logistics.b
;; (:length (:parallel 13))
;; optimal
;; #actions 47 #states 10^8
;;

(define (problem log04)
    (:domain logistics-strips)
    (:objects
        package1
        package2
        package3
        package4
        package5
        package6
        package7

        airplane1
        airplane2

        pgh
        bos
        la
        ny

        pgh-truck
        bos-truck
        la-truck
        ny-truck

        pgh-po
        bos-po
        la-po
        ny-po

        pgh-central
        bos-central
        la-central
        ny-central

        pgh-airport
        bos-airport
        la-airport
        ny-airport
    )
    (:init
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)

        (AIRPLANE airplane1)
        (AIRPLANE airplane2)

        (CITY pgh)
        (CITY bos)
        (CITY la)
        (CITY ny)

        (TRUCK pgh-truck)
        (TRUCK bos-truck)
        (TRUCK la-truck)
        (TRUCK ny-truck)

        (LOCATION bos-po)
        (LOCATION la-po)
        (LOCATION pgh-po)
        (LOCATION ny-po)

        (LOCATION bos-central)
        (LOCATION la-central)
        (LOCATION pgh-central)
        (LOCATION ny-central)

        (AIRPORT bos-airport)
        (LOCATION bos-airport)
        (AIRPORT pgh-airport)
        (LOCATION pgh-airport)
        (AIRPORT la-airport)
        (LOCATION la-airport)
        (AIRPORT ny-airport)
        (LOCATION ny-airport)

        (in-city pgh-po pgh)
        (in-city pgh-airport pgh)
        (in-city pgh-central pgh)

        (in-city bos-po bos)
        (in-city bos-airport bos)
        (in-city bos-central bos)

        (in-city la-po la)
        (in-city la-airport la)
        (in-city la-central la)

        (in-city ny-po ny)
        (in-city ny-airport ny)
        (in-city ny-central ny)

        (at package1 pgh-po)
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
    )
    (:goal (and
        (at package1 bos-po)
        (at package2 ny-po)
        (at package3 la-po)
        (at package5 pgh-po)
        (at package7 pgh-po)
    ))
)
;; original name logistics.c
;; (:length (:parallel 13))
;; optimal
;; #actions 63 #states 10^10
;;

(define (problem log05)
    (:domain logistics-strips)
    (:objects
        package1
        package2
        package3
        package4
        package5
        package6
        package7

        airplane1
        airplane2

        pgh
        bos
        la
        ny

        pgh-truck
        bos-truck
        la-truck
        ny-truck

        pgh-po
        bos-po
        la-po
        ny-po

        pgh-central
        bos-central
        la-central
        ny-central

        pgh-airport
        bos-airport
        la-airport
        ny-airport
    )
    (:init
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)

        (AIRPLANE airplane1)
        (AIRPLANE airplane2)

        (CITY pgh)
        (CITY bos)
        (CITY la)
        (CITY ny)

        (TRUCK pgh-truck)
        (TRUCK bos-truck)
        (TRUCK la-truck)
        (TRUCK ny-truck)

        (LOCATION bos-po)
        (LOCATION la-po)
        (LOCATION pgh-po)
        (LOCATION ny-po)

        (LOCATION bos-central)
        (LOCATION la-central)
        (LOCATION pgh-central)
        (LOCATION ny-central)

        (AIRPORT bos-airport)
        (LOCATION bos-airport)
        (AIRPORT pgh-airport)
        (LOCATION pgh-airport)
        (AIRPORT la-airport)
        (LOCATION la-airport)
        (AIRPORT ny-airport)
        (LOCATION ny-airport)

        (in-city pgh-po pgh)
        (in-city pgh-airport pgh)
        (in-city pgh-central pgh)

        (in-city bos-po bos)
        (in-city bos-airport bos)
        (in-city bos-central bos)

        (in-city la-po la)
        (in-city la-airport la)
        (in-city la-central la)

        (in-city ny-po ny)
        (in-city ny-airport ny)
        (in-city ny-central ny)

        (at package1 pgh-po)
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

    )
    (:goal (and
        (at package1 bos-po)
        (at package2 ny-po)
        (at package3 la-po)
        (at package4 la-airport)
        (at package5 pgh-po)
        (at package6 ny-airport)
        (at package7 pgh-po)
    ))
)
;; original name logistics.d
;; (:length (:parallel 14))
;; optimal
;; #actions 74 #states 10^16
;; note small bug sf locations
;;      fixed in "log09" (slightly easier)
;;

(define (problem log06)
    (:domain logistics-strips)
    (:objects
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8
        package9

        airplane1
        airplane2

        pgh
        bos
        la
        ny
        sf

        pgh-truck
        bos-truck
        la-truck
        ny-truck
        sf-truck

        pgh-po
        bos-po
        la-po
        ny-po
        sf-po

        pgh-central
        bos-central
        la-central
        ny-central
        sf-central

        pgh-airport
        bos-airport
        la-airport
        ny-airport
        sf-airport
    )
    (:init
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)
        (OBJ package9)

        (AIRPLANE airplane1)
        (AIRPLANE airplane2)

        (CITY pgh)
        (CITY bos)
        (CITY la)
        (CITY ny)
        (CITY sf)

        (TRUCK pgh-truck)
        (TRUCK bos-truck)
        (TRUCK la-truck)
        (TRUCK ny-truck)
        (TRUCK sf-truck)

        (LOCATION bos-po)
        (LOCATION la-po)
        (LOCATION pgh-po)
        (LOCATION ny-po)
        (LOCATION sf-po)

        (LOCATION bos-central)
        (LOCATION la-central)
        (LOCATION pgh-central)
        (LOCATION ny-central)
        (LOCATION sf-central)

        (AIRPORT bos-airport)
        (LOCATION bos-airport)
        (AIRPORT pgh-airport)
        (LOCATION pgh-airport)
        (AIRPORT la-airport)
        (LOCATION la-airport)
        (AIRPORT ny-airport)
        (LOCATION ny-airport)
        (AIRPORT sf-airport)
        (LOCATION sf-airport)

        (in-city pgh-po pgh)
        (in-city pgh-airport pgh)
        (in-city pgh-central pgh)

        (in-city bos-po bos)
        (in-city bos-airport bos)
        (in-city bos-central bos)

        (in-city la-po la)
        (in-city la-airport la)
        (in-city la-central la)

        (in-city ny-po ny)
        (in-city ny-airport ny)
        (in-city ny-central ny)

        (in-city sf-po ny)
        (in-city sf-airport ny)
        (in-city sf-central ny)

        (at package1 pgh-po)
        (at package2 pgh-central)
        (at package3 pgh-central)
        (at package4 ny-po)
        (at package5 bos-po)
        (at package6 bos-po)
        (at package7 ny-po)
        (at package8 sf-airport)
        (at package9 sf-central)

        (at airplane1 pgh-airport)
        (at airplane2 pgh-airport)

        (at bos-truck bos-po)
        (at pgh-truck pgh-airport)
        (at la-truck la-po)
        (at ny-truck ny-central)
        (at sf-truck sf-airport)
    )
    (:goal (and
       (at package1 bos-po)
       (at package2 ny-po)
       (at package3 la-central)
       (at package4 la-airport)
       (at package5 pgh-po)
       (at package6 ny-central)
       (at package7 pgh-po)
       (at package8 ny-central)
       (at package9 sf-po)
    ))
)

;; original name logistics.d1a
;; (:length (:parallel 13))
;; optimal
;;

(define (problem log07)
    (:domain logistics-strips)
    (:objects
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8
        package9

        airplane1
        airplane2
        airplane3

        pgh
        bos
        la
        ny
        sf

        pgh-truck
        bos-truck
        la-truck
        ny-truck
        sf-truck

        pgh-po
        bos-po
        la-po
        ny-po
        sf-po

        pgh-central
        bos-central
        la-central
        ny-central
        sf-central

        pgh-airport
        bos-airport
        la-airport
        ny-airport
        sf-airport
    )
    (:init
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)
        (OBJ package9)

        (AIRPLANE airplane1)
        (AIRPLANE airplane2)
        (AIRPLANE airplane3)

        (CITY pgh)
        (CITY bos)
        (CITY la)
        (CITY ny)
        (CITY sf)

        (TRUCK pgh-truck)
        (TRUCK bos-truck)
        (TRUCK la-truck)
        (TRUCK ny-truck)
        (TRUCK sf-truck)

        (LOCATION bos-po)
        (LOCATION la-po)
        (LOCATION pgh-po)
        (LOCATION ny-po)
        (LOCATION sf-po)

        (LOCATION bos-central)
        (LOCATION la-central)
        (LOCATION pgh-central)
        (LOCATION ny-central)
        (LOCATION sf-central)

        (AIRPORT bos-airport)
        (LOCATION bos-airport)
        (AIRPORT pgh-airport)
        (LOCATION pgh-airport)
        (AIRPORT la-airport)
        (LOCATION la-airport)
        (AIRPORT ny-airport)
        (LOCATION ny-airport)
        (AIRPORT sf-airport)
        (LOCATION sf-airport)

        (in-city pgh-po pgh)
        (in-city pgh-airport pgh)
        (in-city pgh-central pgh)

        (in-city bos-po bos)
        (in-city bos-airport bos)
        (in-city bos-central bos)

        (in-city la-po la)
        (in-city la-airport la)
        (in-city la-central la)

        (in-city ny-po ny)
        (in-city ny-airport ny)
        (in-city ny-central ny)

        (in-city sf-po sf)
        (in-city sf-airport sf)
        (in-city sf-central sf)

        (at package1 pgh-po)
        (at package2 pgh-central)
        (at package3 pgh-central)
        (at package4 ny-po)
        (at package5 bos-po)
        (at package6 bos-po)
        (at package7 ny-po)
        (at package8 sf-airport)
        (at package9 sf-central)
       
        (at airplane1 pgh-airport)
        (at airplane2 pgh-airport)
        (at airplane3 pgh-airport)
       
        (at bos-truck bos-po)
        (at pgh-truck pgh-airport)
        (at la-truck la-po)
        (at ny-truck ny-central)
        (at sf-truck sf-airport)
    )
    (:goal (and
        (at package1 bos-po)
        (at package2 ny-po)
        (at package3 la-central)
        (at package4 la-airport)
        (at package5 pgh-po)
        (at package6 ny-central)
        (at package7 pgh-po)
        (at package8 ny-central)
        (at package9 sf-po)
    ))
)
;; original name logistics.d1b
;; (:length (:parallel 17))
;; optimal
;;

(define (problem log08)
    (:domain logistics-strips)
    (:objects
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8
        package9

        airplane1

        pgh
        bos
        la
        ny
        sf

        pgh-truck
        bos-truck
        la-truck
        ny-truck
        sf-truck

        pgh-po
        bos-po
        la-po
        ny-po
        sf-po

        pgh-central
        bos-central
        la-central
        ny-central
        sf-central

        pgh-airport
        bos-airport
        la-airport
        ny-airport
        sf-airport
    )
    (:init
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)
        (OBJ package9)

        (AIRPLANE airplane1)

        (CITY pgh)
        (CITY bos)
        (CITY la)
        (CITY ny)
        (CITY sf)

        (TRUCK pgh-truck)
        (TRUCK bos-truck)
        (TRUCK la-truck)
        (TRUCK ny-truck)
        (TRUCK sf-truck)

        (LOCATION bos-po)
        (LOCATION la-po)
        (LOCATION pgh-po)
        (LOCATION ny-po)
        (LOCATION sf-po)

        (LOCATION bos-central)
        (LOCATION la-central)
        (LOCATION pgh-central)
        (LOCATION ny-central)
        (LOCATION sf-central)

        (AIRPORT bos-airport)
        (LOCATION bos-airport)
        (AIRPORT pgh-airport)
        (LOCATION pgh-airport)
        (AIRPORT la-airport)
        (LOCATION la-airport)
        (AIRPORT ny-airport)
        (LOCATION ny-airport)
        (AIRPORT sf-airport)
        (LOCATION sf-airport)

        (in-city pgh-po pgh)
        (in-city pgh-airport pgh)
        (in-city pgh-central pgh)

        (in-city bos-po bos)
        (in-city bos-airport bos)
        (in-city bos-central bos)

        (in-city la-po la)
        (in-city la-airport la)
        (in-city la-central la)

        (in-city ny-po ny)
        (in-city ny-airport ny)
        (in-city ny-central ny)

        (in-city sf-po sf)
        (in-city sf-airport sf)
        (in-city sf-central sf)

        (at package1 pgh-po)
        (at package2 pgh-central)
        (at package3 pgh-central)
        (at package4 ny-po)
        (at package5 bos-po)
        (at package6 bos-po)
        (at package7 ny-po)
        (at package8 sf-airport)
        (at package9 sf-central)

        (at airplane1 pgh-airport)

        (at bos-truck bos-po)
        (at pgh-truck pgh-airport)
        (at la-truck la-po)
        (at ny-truck ny-central)
        (at sf-truck sf-airport)
    )
    (:goal (and
        (at package1 bos-po)
        (at package2 ny-po)
        (at package3 la-central)
        (at package4 la-airport)
        (at package5 pgh-po)
        (at package6 ny-central)
        (at package7 pgh-po)
        (at package8 ny-central)
        (at package9 sf-po)
    ))
)
;; original name logistics.d1
;; (:length (:parallel 14))
;; optimal
;;

(define (problem log09)
    (:domain logistics-strips)
    (:objects
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8
        package9

        airplane1
        airplane2

        pgh
        bos
        la
        ny
        sf

        pgh-truck
        bos-truck
        la-truck
        ny-truck
        sf-truck

        pgh-po
        bos-po
        la-po
        ny-po
        sf-po

        pgh-central
        bos-central
        la-central
        ny-central
        sf-central

        pgh-airport
        bos-airport
        la-airport
        ny-airport
        sf-airport
    )
    (:init
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)
        (OBJ package9)

        (AIRPLANE airplane1)
        (AIRPLANE airplane2)

        (CITY pgh)
        (CITY bos)
        (CITY la)
        (CITY ny)
        (CITY sf)

        (TRUCK pgh-truck)
        (TRUCK bos-truck)
        (TRUCK la-truck)
        (TRUCK ny-truck)
        (TRUCK sf-truck)

        (LOCATION bos-po)
        (LOCATION la-po)
        (LOCATION pgh-po)
        (LOCATION ny-po)
        (LOCATION sf-po)

        (LOCATION bos-central)
        (LOCATION la-central)
        (LOCATION pgh-central)
        (LOCATION ny-central)
        (LOCATION sf-central)

        (AIRPORT bos-airport)
        (LOCATION bos-airport)
        (AIRPORT pgh-airport)
        (LOCATION pgh-airport)
        (AIRPORT la-airport)
        (LOCATION la-airport)
        (AIRPORT ny-airport)
        (LOCATION ny-airport)
        (AIRPORT sf-airport)
        (LOCATION sf-airport)

        (in-city pgh-po pgh)
        (in-city pgh-airport pgh)
        (in-city pgh-central pgh)

        (in-city bos-po bos)
        (in-city bos-airport bos)
        (in-city bos-central bos)

        (in-city la-po la)
        (in-city la-airport la)
        (in-city la-central la)

        (in-city ny-po ny)
        (in-city ny-airport ny)
        (in-city ny-central ny)

        (in-city sf-po sf)
        (in-city sf-airport sf)
        (in-city sf-central sf)

        (at package1 pgh-po)
        (at package2 pgh-central)
        (at package3 pgh-central)
        (at package4 ny-po)
        (at package5 bos-po)
        (at package6 bos-po)
        (at package7 ny-po)
        (at package8 sf-airport)
        (at package9 sf-central)

        (at airplane1 pgh-airport)
        (at airplane2 pgh-airport)

        (at bos-truck bos-po)
        (at pgh-truck pgh-airport)
        (at la-truck la-po)
        (at ny-truck ny-central)
        (at sf-truck sf-airport)
    )
    (:goal (and
       (at package1 bos-po)
       (at package2 ny-po)
       (at package3 la-central)
       (at package4 la-airport)
       (at package5 pgh-po)
       (at package6 ny-central)
       (at package7 pgh-po)
       (at package8 ny-central)
       (at package9 sf-po)
    ))
)
;; a logistics problem instance
;; name: log07
;; #packages: 10        #cities: 6  #planes: 2
;; #locs_per_city: 2   #trucks_per_city: 1
;; #goals: 10           seed: 76525784

(define (problem log10)
    (:domain logistics-strips)
    (:objects 
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8
        package9
        package10
        plane1
        plane2
        truck1-1
        loc1-1
        loc1-2
        city1
        truck2-1
        loc2-1
        loc2-2
        city2
        truck3-1
        loc3-1
        loc3-2
        city3
        truck4-1
        loc4-1
        loc4-2
        city4
        truck5-1
        loc5-1
        loc5-2
        city5
        truck6-1
        loc6-1
        loc6-2
        city6
    )
    (:init 
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)
        (OBJ package9)
        (OBJ package10)
        (AIRPLANE plane1)
        (AIRPLANE plane2)
        (TRUCK truck1-1)
        (LOCATION loc1-1)
        (LOCATION loc1-2)
        (CITY city1)
        (AIRPORT loc1-1)
        (TRUCK truck2-1)
        (LOCATION loc2-1)
        (LOCATION loc2-2)
        (CITY city2)
        (AIRPORT loc2-1)
        (TRUCK truck3-1)
        (LOCATION loc3-1)
        (LOCATION loc3-2)
        (CITY city3)
        (AIRPORT loc3-1)
        (TRUCK truck4-1)
        (LOCATION loc4-1)
        (LOCATION loc4-2)
        (CITY city4)
        (AIRPORT loc4-1)
        (TRUCK truck5-1)
        (LOCATION loc5-1)
        (LOCATION loc5-2)
        (CITY city5)
        (AIRPORT loc5-1)
        (TRUCK truck6-1)
        (LOCATION loc6-1)
        (LOCATION loc6-2)
        (CITY city6)
        (AIRPORT loc6-1)
        (in-city loc1-1 city1)
        (in-city loc1-2 city1)
        (in-city loc2-1 city2)
        (in-city loc2-2 city2)
        (in-city loc3-1 city3)
        (in-city loc3-2 city3)
        (in-city loc4-1 city4)
        (in-city loc4-2 city4)
        (in-city loc5-1 city5)
        (in-city loc5-2 city5)
        (in-city loc6-1 city6)
        (in-city loc6-2 city6)
        (at plane1 loc4-1)
        (at plane2 loc4-1)
        (at truck1-1 loc1-1)
        (at truck2-1 loc2-2)
        (at truck3-1 loc3-1)
        (at truck4-1 loc4-2)
        (at truck5-1 loc5-2)
        (at truck6-1 loc6-1)
        (at package1 loc5-1)
        (at package2 loc4-1)
        (at package3 loc6-2)
        (at package4 loc4-1)
        (at package5 loc4-2)
        (at package6 loc3-1)
        (at package7 loc2-2)
        (at package8 loc4-1)
        (at package9 loc3-2)
        (at package10 loc6-1)
    )
    (:goal (and
        (at package1 loc2-1)
        (at package2 loc5-1)
        (at package3 loc4-1)
        (at package4 loc2-1)
        (at package5 loc4-2)
        (at package6 loc3-2)
        (at package7 loc4-1)
        (at package8 loc5-2)
        (at package9 loc6-2)
        (at package10 loc6-1)
    ))
)
;; a logistics problem instance
;; name: log08
;; #packages: 9        #cities: 5  #planes: 2
;; #locs_per_city: 2   #trucks_per_city: 1
;; #goals: 9           seed: 82752309

(define (problem log11)
    (:domain logistics-strips)
    (:objects 
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8
        package9
        plane1
        plane2
        truck1-1
        loc1-1
        loc1-2
        city1
        truck2-1
        loc2-1
        loc2-2
        city2
        truck3-1
        loc3-1
        loc3-2
        city3
        truck4-1
        loc4-1
        loc4-2
        city4
        truck5-1
        loc5-1
        loc5-2
        city5
    )
    (:init 
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)
        (OBJ package9)
        (AIRPLANE plane1)
        (AIRPLANE plane2)
        (TRUCK truck1-1)
        (LOCATION loc1-1)
        (LOCATION loc1-2)
        (CITY city1)
        (AIRPORT loc1-1)
        (TRUCK truck2-1)
        (LOCATION loc2-1)
        (LOCATION loc2-2)
        (CITY city2)
        (AIRPORT loc2-1)
        (TRUCK truck3-1)
        (LOCATION loc3-1)
        (LOCATION loc3-2)
        (CITY city3)
        (AIRPORT loc3-1)
        (TRUCK truck4-1)
        (LOCATION loc4-1)
        (LOCATION loc4-2)
        (CITY city4)
        (AIRPORT loc4-1)
        (TRUCK truck5-1)
        (LOCATION loc5-1)
        (LOCATION loc5-2)
        (CITY city5)
        (AIRPORT loc5-1)
        (in-city loc1-1 city1)
        (in-city loc1-2 city1)
        (in-city loc2-1 city2)
        (in-city loc2-2 city2)
        (in-city loc3-1 city3)
        (in-city loc3-2 city3)
        (in-city loc4-1 city4)
        (in-city loc4-2 city4)
        (in-city loc5-1 city5)
        (in-city loc5-2 city5)
        (at plane1 loc4-1)
        (at plane2 loc5-1)
        (at truck1-1 loc1-2)
        (at truck2-1 loc2-1)
        (at truck3-1 loc3-2)
        (at truck4-1 loc4-2)
        (at truck5-1 loc5-1)
        (at package1 loc1-2)
        (at package2 loc1-2)
        (at package3 loc5-2)
        (at package4 loc3-2)
        (at package5 loc1-2)
        (at package6 loc3-1)
        (at package7 loc5-1)
        (at package8 loc1-1)
        (at package9 loc5-2)
    )
    (:goal (and
        (at package1 loc1-2)
        (at package2 loc5-1)
        (at package3 loc1-1)
        (at package4 loc5-2)
        (at package5 loc2-1)
        (at package6 loc2-2)
        (at package7 loc3-1)
        (at package8 loc3-1)
        (at package9 loc3-2)
    ))
)
;; a logistics problem instance
;; name: log09
;; #packages: 9        #cities: 4  #planes: 3
;; #locs_per_city: 2   #trucks_per_city: 1
;; #goals: 9           seed: 21736079

(define (problem log12)
    (:domain logistics-strips)
    (:objects 
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8
        package9
        plane1
        plane2
        plane3
        truck1-1
        loc1-1
        loc1-2
        city1
        truck2-1
        loc2-1
        loc2-2
        city2
        truck3-1
        loc3-1
        loc3-2
        city3
        truck4-1
        loc4-1
        loc4-2
        city4
    )
    (:init 
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)
        (OBJ package9)
        (AIRPLANE plane1)
        (AIRPLANE plane2)
        (AIRPLANE plane3)
        (TRUCK truck1-1)
        (LOCATION loc1-1)
        (LOCATION loc1-2)
        (CITY city1)
        (AIRPORT loc1-1)
        (TRUCK truck2-1)
        (LOCATION loc2-1)
        (LOCATION loc2-2)
        (CITY city2)
        (AIRPORT loc2-1)
        (TRUCK truck3-1)
        (LOCATION loc3-1)
        (LOCATION loc3-2)
        (CITY city3)
        (AIRPORT loc3-1)
        (TRUCK truck4-1)
        (LOCATION loc4-1)
        (LOCATION loc4-2)
        (CITY city4)
        (AIRPORT loc4-1)
        (in-city loc1-1 city1)
        (in-city loc1-2 city1)
        (in-city loc2-1 city2)
        (in-city loc2-2 city2)
        (in-city loc3-1 city3)
        (in-city loc3-2 city3)
        (in-city loc4-1 city4)
        (in-city loc4-2 city4)
        (at plane1 loc1-1)
        (at plane2 loc1-1)
        (at plane3 loc1-1)
        (at truck1-1 loc1-1)
        (at truck2-1 loc2-2)
        (at truck3-1 loc3-1)
        (at truck4-1 loc4-1)
        (at package1 loc2-1)
        (at package2 loc4-1)
        (at package3 loc3-1)
        (at package4 loc3-1)
        (at package5 loc2-1)
        (at package6 loc2-2)
        (at package7 loc2-2)
        (at package8 loc2-1)
        (at package9 loc1-2)
    )
    (:goal (and
        (at package1 loc2-1)
        (at package2 loc1-1)
        (at package3 loc2-2)
        (at package4 loc1-2)
        (at package5 loc1-2)
        (at package6 loc4-1)
        (at package7 loc2-1)
        (at package8 loc3-1)
        (at package9 loc1-1)
    ))
)
;; a logistics problem instance
;; name: log10
;; #packages: 9        #cities: 6  #planes: 3
;; #locs_per_city: 2   #trucks_per_city: 1
;; #goals: 9           seed: 82947632

(define (problem log13)
    (:domain logistics-strips)
    (:objects 
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8
        package9
        plane1
        plane2
        plane3
        truck1-1
        loc1-1
        loc1-2
        city1
        truck2-1
        loc2-1
        loc2-2
        city2
        truck3-1
        loc3-1
        loc3-2
        city3
        truck4-1
        loc4-1
        loc4-2
        city4
        truck5-1
        loc5-1
        loc5-2
        city5
        truck6-1
        loc6-1
        loc6-2
        city6
    )
    (:init 
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)
        (OBJ package9)
        (AIRPLANE plane1)
        (AIRPLANE plane2)
        (AIRPLANE plane3)
        (TRUCK truck1-1)
        (LOCATION loc1-1)
        (LOCATION loc1-2)
        (CITY city1)
        (AIRPORT loc1-1)
        (TRUCK truck2-1)
        (LOCATION loc2-1)
        (LOCATION loc2-2)
        (CITY city2)
        (AIRPORT loc2-1)
        (TRUCK truck3-1)
        (LOCATION loc3-1)
        (LOCATION loc3-2)
        (CITY city3)
        (AIRPORT loc3-1)
        (TRUCK truck4-1)
        (LOCATION loc4-1)
        (LOCATION loc4-2)
        (CITY city4)
        (AIRPORT loc4-1)
        (TRUCK truck5-1)
        (LOCATION loc5-1)
        (LOCATION loc5-2)
        (CITY city5)
        (AIRPORT loc5-1)
        (TRUCK truck6-1)
        (LOCATION loc6-1)
        (LOCATION loc6-2)
        (CITY city6)
        (AIRPORT loc6-1)
        (in-city loc1-1 city1)
        (in-city loc1-2 city1)
        (in-city loc2-1 city2)
        (in-city loc2-2 city2)
        (in-city loc3-1 city3)
        (in-city loc3-2 city3)
        (in-city loc4-1 city4)
        (in-city loc4-2 city4)
        (in-city loc5-1 city5)
        (in-city loc5-2 city5)
        (in-city loc6-1 city6)
        (in-city loc6-2 city6)
        (at plane1 loc6-1)
        (at plane2 loc6-1)
        (at plane3 loc3-1)
        (at truck1-1 loc1-2)
        (at truck2-1 loc2-1)
        (at truck3-1 loc3-2)
        (at truck4-1 loc4-2)
        (at truck5-1 loc5-1)
        (at truck6-1 loc6-1)
        (at package1 loc3-2)
        (at package2 loc3-2)
        (at package3 loc4-2)
        (at package4 loc5-2)
        (at package5 loc2-1)
        (at package6 loc5-2)
        (at package7 loc4-2)
        (at package8 loc1-1)
        (at package9 loc2-2)
    )
    (:goal (and
        (at package1 loc5-2)
        (at package2 loc1-1)
        (at package3 loc5-2)
        (at package4 loc1-2)
        (at package5 loc3-2)
        (at package6 loc6-1)
        (at package7 loc6-2)
        (at package8 loc3-1)
        (at package9 loc6-2)
    ))
)
;; a logistics problem instance
;; name: log11
;; #packages: 11        #cities: 6  #planes: 3
;; #locs_per_city: 2   #trucks_per_city: 1
;; #goals: 11           seed: 121386628

(define (problem log14)
    (:domain logistics-strips)
    (:objects 
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8
        package9
        package10
        package11
        plane1
        plane2
        plane3
        truck1-1
        loc1-1
        loc1-2
        city1
        truck2-1
        loc2-1
        loc2-2
        city2
        truck3-1
        loc3-1
        loc3-2
        city3
        truck4-1
        loc4-1
        loc4-2
        city4
        truck5-1
        loc5-1
        loc5-2
        city5
        truck6-1
        loc6-1
        loc6-2
        city6
    )
    (:init 
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)
        (OBJ package9)
        (OBJ package10)
        (OBJ package11)
        (AIRPLANE plane1)
        (AIRPLANE plane2)
        (AIRPLANE plane3)
        (TRUCK truck1-1)
        (LOCATION loc1-1)
        (LOCATION loc1-2)
        (CITY city1)
        (AIRPORT loc1-1)
        (TRUCK truck2-1)
        (LOCATION loc2-1)
        (LOCATION loc2-2)
        (CITY city2)
        (AIRPORT loc2-1)
        (TRUCK truck3-1)
        (LOCATION loc3-1)
        (LOCATION loc3-2)
        (CITY city3)
        (AIRPORT loc3-1)
        (TRUCK truck4-1)
        (LOCATION loc4-1)
        (LOCATION loc4-2)
        (CITY city4)
        (AIRPORT loc4-1)
        (TRUCK truck5-1)
        (LOCATION loc5-1)
        (LOCATION loc5-2)
        (CITY city5)
        (AIRPORT loc5-1)
        (TRUCK truck6-1)
        (LOCATION loc6-1)
        (LOCATION loc6-2)
        (CITY city6)
        (AIRPORT loc6-1)
        (in-city loc1-1 city1)
        (in-city loc1-2 city1)
        (in-city loc2-1 city2)
        (in-city loc2-2 city2)
        (in-city loc3-1 city3)
        (in-city loc3-2 city3)
        (in-city loc4-1 city4)
        (in-city loc4-2 city4)
        (in-city loc5-1 city5)
        (in-city loc5-2 city5)
        (in-city loc6-1 city6)
        (in-city loc6-2 city6)
        (at plane1 loc4-1)
        (at plane2 loc2-1)
        (at plane3 loc5-1)
        (at truck1-1 loc1-2)
        (at truck2-1 loc2-1)
        (at truck3-1 loc3-2)
        (at truck4-1 loc4-2)
        (at truck5-1 loc5-1)
        (at truck6-1 loc6-1)
        (at package1 loc5-2)
        (at package2 loc5-2)
        (at package3 loc2-2)
        (at package4 loc3-2)
        (at package5 loc6-1)
        (at package6 loc3-2)
        (at package7 loc2-2)
        (at package8 loc3-1)
        (at package9 loc2-2)
        (at package10 loc3-2)
        (at package11 loc1-1)
    )
    (:goal (and
        (at package1 loc3-2)
        (at package2 loc3-2)
        (at package3 loc3-2)
        (at package4 loc6-1)
        (at package5 loc4-2)
        (at package6 loc1-1)
        (at package7 loc4-2)
        (at package8 loc2-2)
        (at package9 loc3-2)
        (at package10 loc3-1)
        (at package11 loc1-2)
    ))
)
;; a logistics problem instance
;; name: log12
;; #packages: 11        #cities: 6  #planes: 2
;; #locs_per_city: 2   #trucks_per_city: 1
;; #goals: 11           seed: 69437334

(define (problem log15)
    (:domain logistics-strips)
    (:objects 
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8
        package9
        package10
        package11
        plane1
        plane2
        truck1-1
        loc1-1
        loc1-2
        city1
        truck2-1
        loc2-1
        loc2-2
        city2
        truck3-1
        loc3-1
        loc3-2
        city3
        truck4-1
        loc4-1
        loc4-2
        city4
        truck5-1
        loc5-1
        loc5-2
        city5
        truck6-1
        loc6-1
        loc6-2
        city6
    )
    (:init 
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)
        (OBJ package9)
        (OBJ package10)
        (OBJ package11)
        (AIRPLANE plane1)
        (AIRPLANE plane2)
        (TRUCK truck1-1)
        (LOCATION loc1-1)
        (LOCATION loc1-2)
        (CITY city1)
        (AIRPORT loc1-1)
        (TRUCK truck2-1)
        (LOCATION loc2-1)
        (LOCATION loc2-2)
        (CITY city2)
        (AIRPORT loc2-1)
        (TRUCK truck3-1)
        (LOCATION loc3-1)
        (LOCATION loc3-2)
        (CITY city3)
        (AIRPORT loc3-1)
        (TRUCK truck4-1)
        (LOCATION loc4-1)
        (LOCATION loc4-2)
        (CITY city4)
        (AIRPORT loc4-1)
        (TRUCK truck5-1)
        (LOCATION loc5-1)
        (LOCATION loc5-2)
        (CITY city5)
        (AIRPORT loc5-1)
        (TRUCK truck6-1)
        (LOCATION loc6-1)
        (LOCATION loc6-2)
        (CITY city6)
        (AIRPORT loc6-1)
        (in-city loc1-1 city1)
        (in-city loc1-2 city1)
        (in-city loc2-1 city2)
        (in-city loc2-2 city2)
        (in-city loc3-1 city3)
        (in-city loc3-2 city3)
        (in-city loc4-1 city4)
        (in-city loc4-2 city4)
        (in-city loc5-1 city5)
        (in-city loc5-2 city5)
        (in-city loc6-1 city6)
        (in-city loc6-2 city6)
        (at plane1 loc3-1)
        (at plane2 loc4-1)
        (at truck1-1 loc1-2)
        (at truck2-1 loc2-2)
        (at truck3-1 loc3-1)
        (at truck4-1 loc4-1)
        (at truck5-1 loc5-2)
        (at truck6-1 loc6-1)
        (at package1 loc4-2)
        (at package2 loc1-2)
        (at package3 loc3-2)
        (at package4 loc3-1)
        (at package5 loc5-1)
        (at package6 loc2-2)
        (at package7 loc1-2)
        (at package8 loc6-1)
        (at package9 loc5-2)
        (at package10 loc3-2)
        (at package11 loc4-1)
    )
    (:goal (and
        (at package1 loc6-2)
        (at package2 loc4-1)
        (at package3 loc5-1)
        (at package4 loc5-1)
        (at package5 loc5-2)
        (at package6 loc6-2)
        (at package7 loc4-1)
        (at package8 loc3-2)
        (at package9 loc3-1)
        (at package10 loc1-1)
        (at package11 loc2-1)
    ))
)
;; a logistics problem instance
;; name: log13
;; #packages: 6        #cities: 6  #planes: 1
;; #locs_per_city: 2   #trucks_per_city: 1
;; #goals: 6           seed: 68345397

(define (problem log16)
    (:domain logistics-strips)
    (:objects 
        package1
        package2
        package3
        package4
        package5
        package6
        plane1
        truck1-1
        loc1-1
        loc1-2
        city1
        truck2-1
        loc2-1
        loc2-2
        city2
        truck3-1
        loc3-1
        loc3-2
        city3
        truck4-1
        loc4-1
        loc4-2
        city4
        truck5-1
        loc5-1
        loc5-2
        city5
        truck6-1
        loc6-1
        loc6-2
        city6
    )
    (:init 
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (AIRPLANE plane1)
        (TRUCK truck1-1)
        (LOCATION loc1-1)
        (LOCATION loc1-2)
        (CITY city1)
        (AIRPORT loc1-1)
        (TRUCK truck2-1)
        (LOCATION loc2-1)
        (LOCATION loc2-2)
        (CITY city2)
        (AIRPORT loc2-1)
        (TRUCK truck3-1)
        (LOCATION loc3-1)
        (LOCATION loc3-2)
        (CITY city3)
        (AIRPORT loc3-1)
        (TRUCK truck4-1)
        (LOCATION loc4-1)
        (LOCATION loc4-2)
        (CITY city4)
        (AIRPORT loc4-1)
        (TRUCK truck5-1)
        (LOCATION loc5-1)
        (LOCATION loc5-2)
        (CITY city5)
        (AIRPORT loc5-1)
        (TRUCK truck6-1)
        (LOCATION loc6-1)
        (LOCATION loc6-2)
        (CITY city6)
        (AIRPORT loc6-1)
        (in-city loc1-1 city1)
        (in-city loc1-2 city1)
        (in-city loc2-1 city2)
        (in-city loc2-2 city2)
        (in-city loc3-1 city3)
        (in-city loc3-2 city3)
        (in-city loc4-1 city4)
        (in-city loc4-2 city4)
        (in-city loc5-1 city5)
        (in-city loc5-2 city5)
        (in-city loc6-1 city6)
        (in-city loc6-2 city6)
        (at plane1 loc2-1)
        (at truck1-1 loc1-1)
        (at truck2-1 loc2-2)
        (at truck3-1 loc3-1)
        (at truck4-1 loc4-2)
        (at truck5-1 loc5-2)
        (at truck6-1 loc6-1)
        (at package1 loc2-2)
        (at package2 loc3-2)
        (at package3 loc4-2)
        (at package4 loc5-2)
        (at package5 loc6-2)
        (at package6 loc5-1)
    )
    (:goal (and
        (at package1 loc1-1)
        (at package2 loc4-1)
        (at package3 loc5-2)
        (at package4 loc4-2)
        (at package5 loc6-1)
        (at package6 loc6-1)
    ))
)
;; a logistics problem instance
;; name: log14
;; #packages: 6        #cities: 6  #planes: 1
;; #locs_per_city: 2   #trucks_per_city: 1
;; #goals: 6           seed: 68345397
;; modified by hand from log13.
;; placed packages in circle & one destination

(define (problem log17)
    (:domain logistics-strips)
    (:objects 
        package1
        package2
        package3
        package4
        package5
        package6
        plane1
        truck1-1
        loc1-1
        loc1-2
        city1
        truck2-1
        loc2-1
        loc2-2
        city2
        truck3-1
        loc3-1
        loc3-2
        city3
        truck4-1
        loc4-1
        loc4-2
        city4
        truck5-1
        loc5-1
        loc5-2
        city5
        truck6-1
        loc6-1
        loc6-2
        city6
    )
    (:init 
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (AIRPLANE plane1)
        (TRUCK truck1-1)
        (LOCATION loc1-1)
        (LOCATION loc1-2)
        (CITY city1)
        (AIRPORT loc1-1)
        (TRUCK truck2-1)
        (LOCATION loc2-1)
        (LOCATION loc2-2)
        (CITY city2)
        (AIRPORT loc2-1)
        (TRUCK truck3-1)
        (LOCATION loc3-1)
        (LOCATION loc3-2)
        (CITY city3)
        (AIRPORT loc3-1)
        (TRUCK truck4-1)
        (LOCATION loc4-1)
        (LOCATION loc4-2)
        (CITY city4)
        (AIRPORT loc4-1)
        (TRUCK truck5-1)
        (LOCATION loc5-1)
        (LOCATION loc5-2)
        (CITY city5)
        (AIRPORT loc5-1)
        (TRUCK truck6-1)
        (LOCATION loc6-1)
        (LOCATION loc6-2)
        (CITY city6)
        (AIRPORT loc6-1)
        (in-city loc1-1 city1)
        (in-city loc1-2 city1)
        (in-city loc2-1 city2)
        (in-city loc2-2 city2)
        (in-city loc3-1 city3)
        (in-city loc3-2 city3)
        (in-city loc4-1 city4)
        (in-city loc4-2 city4)
        (in-city loc5-1 city5)
        (in-city loc5-2 city5)
        (in-city loc6-1 city6)
        (in-city loc6-2 city6)
        (at plane1 loc2-1)
        (at truck1-1 loc1-1)
        (at truck2-1 loc2-2)
        (at truck3-1 loc3-1)
        (at truck4-1 loc4-2)
        (at truck5-1 loc5-2)
        (at truck6-1 loc6-1)
        (at package1 loc1-2)
        (at package2 loc2-2)
        (at package3 loc3-2)
        (at package4 loc4-2)
        (at package5 loc5-2)
        (at package6 loc6-2)
    )
    (:goal (and
        (at package1 loc6-2)
        (at package2 loc6-2)
        (at package3 loc6-2)
        (at package4 loc6-2)
        (at package5 loc6-2)
        (at package6 loc6-2)
    ))
)
;; a logistics problem instance
;; name: log15
;; #packages: 8        #cities: 6  #planes: 2
;; #locs_per_city: 3   #trucks_per_city: 2
;; #goals: 8           seed: 112000697

(define (problem log18)
    (:domain logistics-strips)
    (:objects 
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8
        plane1
        plane2
        truck1-1
        truck1-2
        loc1-1
        loc1-2
        loc1-3
        city1
        truck2-1
        truck2-2
        loc2-1
        loc2-2
        loc2-3
        city2
        truck3-1
        truck3-2
        loc3-1
        loc3-2
        loc3-3
        city3
        truck4-1
        truck4-2
        loc4-1
        loc4-2
        loc4-3
        city4
        truck5-1
        truck5-2
        loc5-1
        loc5-2
        loc5-3
        city5
        truck6-1
        truck6-2
        loc6-1
        loc6-2
        loc6-3
        city6
    )
    (:init 
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)
        (AIRPLANE plane1)
        (AIRPLANE plane2)
        (TRUCK truck1-1)
        (TRUCK truck1-2)
        (LOCATION loc1-1)
        (LOCATION loc1-2)
        (LOCATION loc1-3)
        (CITY city1)
        (AIRPORT loc1-1)
        (TRUCK truck2-1)
        (TRUCK truck2-2)
        (LOCATION loc2-1)
        (LOCATION loc2-2)
        (LOCATION loc2-3)
        (CITY city2)
        (AIRPORT loc2-1)
        (TRUCK truck3-1)
        (TRUCK truck3-2)
        (LOCATION loc3-1)
        (LOCATION loc3-2)
        (LOCATION loc3-3)
        (CITY city3)
        (AIRPORT loc3-1)
        (TRUCK truck4-1)
        (TRUCK truck4-2)
        (LOCATION loc4-1)
        (LOCATION loc4-2)
        (LOCATION loc4-3)
        (CITY city4)
        (AIRPORT loc4-1)
        (TRUCK truck5-1)
        (TRUCK truck5-2)
        (LOCATION loc5-1)
        (LOCATION loc5-2)
        (LOCATION loc5-3)
        (CITY city5)
        (AIRPORT loc5-1)
        (TRUCK truck6-1)
        (TRUCK truck6-2)
        (LOCATION loc6-1)
        (LOCATION loc6-2)
        (LOCATION loc6-3)
        (CITY city6)
        (AIRPORT loc6-1)
        (in-city loc1-1 city1)
        (in-city loc1-2 city1)
        (in-city loc1-3 city1)
        (in-city loc2-1 city2)
        (in-city loc2-2 city2)
        (in-city loc2-3 city2)
        (in-city loc3-1 city3)
        (in-city loc3-2 city3)
        (in-city loc3-3 city3)
        (in-city loc4-1 city4)
        (in-city loc4-2 city4)
        (in-city loc4-3 city4)
        (in-city loc5-1 city5)
        (in-city loc5-2 city5)
        (in-city loc5-3 city5)
        (in-city loc6-1 city6)
        (in-city loc6-2 city6)
        (in-city loc6-3 city6)
        (at plane1 loc2-1)
        (at plane2 loc3-1)
        (at truck1-1 loc1-1)
        (at truck1-2 loc1-1)
        (at truck2-1 loc2-1)
        (at truck2-2 loc2-1)
        (at truck3-1 loc3-3)
        (at truck3-2 loc3-2)
        (at truck4-1 loc4-2)
        (at truck4-2 loc4-3)
        (at truck5-1 loc5-2)
        (at truck5-2 loc5-1)
        (at truck6-1 loc6-3)
        (at truck6-2 loc6-2)
        (at package1 loc2-3)
        (at package2 loc4-2)
        (at package3 loc5-3)
        (at package4 loc1-2)
        (at package5 loc5-1)
        (at package6 loc4-2)
        (at package7 loc2-1)
        (at package8 loc1-2)
    )
    (:goal (and
        (at package1 loc5-3)
        (at package2 loc4-3)
        (at package3 loc5-3)
        (at package4 loc4-1)
        (at package5 loc1-2)
        (at package6 loc1-3)
        (at package7 loc2-3)
        (at package8 loc2-3)
    ))
)
;; a logistics problem instance
;; name: log16
;; #packages: 9        #cities: 6  #planes: 2
;; #locs_per_city: 2   #trucks_per_city: 1
;; #goals: 9           seed: 70842326

(define (problem log19)
    (:domain logistics-strips)
    (:objects 
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8
        package9
        plane1
        plane2
        truck1-1
        loc1-1
        loc1-2
        city1
        truck2-1
        loc2-1
        loc2-2
        city2
        truck3-1
        loc3-1
        loc3-2
        city3
        truck4-1
        loc4-1
        loc4-2
        city4
        truck5-1
        loc5-1
        loc5-2
        city5
        truck6-1
        loc6-1
        loc6-2
        city6
    )
    (:init 
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)
        (OBJ package9)
        (AIRPLANE plane1)
        (AIRPLANE plane2)
        (TRUCK truck1-1)
        (LOCATION loc1-1)
        (LOCATION loc1-2)
        (CITY city1)
        (AIRPORT loc1-1)
        (TRUCK truck2-1)
        (LOCATION loc2-1)
        (LOCATION loc2-2)
        (CITY city2)
        (AIRPORT loc2-1)
        (TRUCK truck3-1)
        (LOCATION loc3-1)
        (LOCATION loc3-2)
        (CITY city3)
        (AIRPORT loc3-1)
        (TRUCK truck4-1)
        (LOCATION loc4-1)
        (LOCATION loc4-2)
        (CITY city4)
        (AIRPORT loc4-1)
        (TRUCK truck5-1)
        (LOCATION loc5-1)
        (LOCATION loc5-2)
        (CITY city5)
        (AIRPORT loc5-1)
        (TRUCK truck6-1)
        (LOCATION loc6-1)
        (LOCATION loc6-2)
        (CITY city6)
        (AIRPORT loc6-1)
        (in-city loc1-1 city1)
        (in-city loc1-2 city1)
        (in-city loc2-1 city2)
        (in-city loc2-2 city2)
        (in-city loc3-1 city3)
        (in-city loc3-2 city3)
        (in-city loc4-1 city4)
        (in-city loc4-2 city4)
        (in-city loc5-1 city5)
        (in-city loc5-2 city5)
        (in-city loc6-1 city6)
        (in-city loc6-2 city6)
        (at plane1 loc5-1)
        (at plane2 loc4-1)
        (at truck1-1 loc1-2)
        (at truck2-1 loc2-2)
        (at truck3-1 loc3-1)
        (at truck4-1 loc4-1)
        (at truck5-1 loc5-2)
        (at truck6-1 loc6-1)
        (at package1 loc6-2)
        (at package2 loc5-2)
        (at package3 loc3-2)
        (at package4 loc3-1)
        (at package5 loc1-1)
        (at package6 loc6-2)
        (at package7 loc1-2)
        (at package8 loc4-1)
        (at package9 loc5-2)
    )
    (:goal (and
        (at package1 loc5-2)
        (at package2 loc4-1)
        (at package3 loc6-2)
        (at package4 loc2-1)
        (at package5 loc1-1)
        (at package6 loc5-1)
        (at package7 loc1-2)
        (at package8 loc2-2)
        (at package9 loc6-1)
    ))
)
;; a logistics problem instance
;; name: log17
;; #packages: 10        #cities: 7  #planes: 2
;; #locs_per_city: 2   #trucks_per_city: 1
;; #goals: 10           seed: 62880555
;; modified by hand to increase regularity
;;    force more sharing in solution; harder to solve

(define (problem log20)
    (:domain logistics-strips)
    (:objects 
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8
        package9
        package10
        plane1
        plane2
        truck1-1
        loc1-1
        loc1-2
        city1
        truck2-1
        loc2-1
        loc2-2
        city2
        truck3-1
        loc3-1
        loc3-2
        city3
        truck4-1
        loc4-1
        loc4-2
        city4
        truck5-1
        loc5-1
        loc5-2
        city5
        truck6-1
        loc6-1
        loc6-2
        city6
        truck7-1
        loc7-1
        loc7-2
        city7
    )
    (:init 
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)
        (OBJ package9)
        (OBJ package10)
        (AIRPLANE plane1)
        (AIRPLANE plane2)
        (TRUCK truck1-1)
        (LOCATION loc1-1)
        (LOCATION loc1-2)
        (CITY city1)
        (AIRPORT loc1-1)
        (TRUCK truck2-1)
        (LOCATION loc2-1)
        (LOCATION loc2-2)
        (CITY city2)
        (AIRPORT loc2-1)
        (TRUCK truck3-1)
        (LOCATION loc3-1)
        (LOCATION loc3-2)
        (CITY city3)
        (AIRPORT loc3-1)
        (TRUCK truck4-1)
        (LOCATION loc4-1)
        (LOCATION loc4-2)
        (CITY city4)
        (AIRPORT loc4-1)
        (TRUCK truck5-1)
        (LOCATION loc5-1)
        (LOCATION loc5-2)
        (CITY city5)
        (AIRPORT loc5-1)
        (TRUCK truck6-1)
        (LOCATION loc6-1)
        (LOCATION loc6-2)
        (CITY city6)
        (AIRPORT loc6-1)
        (TRUCK truck7-1)
        (LOCATION loc7-1)
        (LOCATION loc7-2)
        (CITY city7)
        (AIRPORT loc7-1)
        (in-city loc1-1 city1)
        (in-city loc1-2 city1)
        (in-city loc2-1 city2)
        (in-city loc2-2 city2)
        (in-city loc3-1 city3)
        (in-city loc3-2 city3)
        (in-city loc4-1 city4)
        (in-city loc4-2 city4)
        (in-city loc5-1 city5)
        (in-city loc5-2 city5)
        (in-city loc6-1 city6)
        (in-city loc6-2 city6)
        (in-city loc7-1 city7)
        (in-city loc7-2 city7)
        (at plane1 loc1-1)
        (at plane2 loc1-1)
        (at truck1-1 loc1-1)
        (at truck2-1 loc2-1)
        (at truck3-1 loc3-2)
        (at truck4-1 loc4-1)
        (at truck5-1 loc5-1)
        (at truck6-1 loc6-2)
        (at truck7-1 loc7-1)

        (at package1 loc1-2)
        (at package2 loc2-2)
        (at package3 loc3-2)
        (at package4 loc4-2)
        (at package5 loc5-2)
        (at package6 loc6-2)
        (at package7 loc7-2)
        (at package8 loc1-2)
        (at package9 loc2-2)
        (at package10 loc3-2)
    )
    (:goal (and
        (at package1 loc3-2)
        (at package2 loc4-2)
        (at package3 loc5-2)
        (at package4 loc6-2)
        (at package5 loc6-2)
        (at package6 loc5-2)
        (at package7 loc4-2)
        (at package8 loc5-2)
        (at package9 loc6-2)
        (at package10 loc7-2)
    ))
)

;; a logistics problem instance
;; name: log06
;; #packages: 10        #cities: 6  #planes: 3
;; #locs_per_city: 2   #trucks_per_city: 1
;; #goals: 10           seed: 12268313

(define (problem log21)
    (:domain logistics-strips)
    (:objects 
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8
        package9
        package10
        plane1
        plane2
        plane3
        truck1-1
        loc1-1
        loc1-2
        city1
        truck2-1
        loc2-1
        loc2-2
        city2
        truck3-1
        loc3-1
        loc3-2
        city3
        truck4-1
        loc4-1
        loc4-2
        city4
        truck5-1
        loc5-1
        loc5-2
        city5
        truck6-1
        loc6-1
        loc6-2
        city6
    )
    (:init 
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)
        (OBJ package9)
        (OBJ package10)
        (AIRPLANE plane1)
        (AIRPLANE plane2)
        (AIRPLANE plane3)
        (TRUCK truck1-1)
        (LOCATION loc1-1)
        (LOCATION loc1-2)
        (CITY city1)
        (AIRPORT loc1-1)
        (TRUCK truck2-1)
        (LOCATION loc2-1)
        (LOCATION loc2-2)
        (CITY city2)
        (AIRPORT loc2-1)
        (TRUCK truck3-1)
        (LOCATION loc3-1)
        (LOCATION loc3-2)
        (CITY city3)
        (AIRPORT loc3-1)
        (TRUCK truck4-1)
        (LOCATION loc4-1)
        (LOCATION loc4-2)
        (CITY city4)
        (AIRPORT loc4-1)
        (TRUCK truck5-1)
        (LOCATION loc5-1)
        (LOCATION loc5-2)
        (CITY city5)
        (AIRPORT loc5-1)
        (TRUCK truck6-1)
        (LOCATION loc6-1)
        (LOCATION loc6-2)
        (CITY city6)
        (AIRPORT loc6-1)
        (in-city loc1-1 city1)
        (in-city loc1-2 city1)
        (in-city loc2-1 city2)
        (in-city loc2-2 city2)
        (in-city loc3-1 city3)
        (in-city loc3-2 city3)
        (in-city loc4-1 city4)
        (in-city loc4-2 city4)
        (in-city loc5-1 city5)
        (in-city loc5-2 city5)
        (in-city loc6-1 city6)
        (in-city loc6-2 city6)
        (at plane1 loc6-1)
        (at plane2 loc5-1)
        (at plane3 loc4-1)
        (at truck1-1 loc1-1)
        (at truck2-1 loc2-2)
        (at truck3-1 loc3-2)
        (at truck4-1 loc4-1)
        (at truck5-1 loc5-2)
        (at truck6-1 loc6-2)
        (at package1 loc1-2)
        (at package2 loc6-2)
        (at package3 loc5-2)
        (at package4 loc6-2)
        (at package5 loc1-1)
        (at package6 loc5-1)
        (at package7 loc2-1)
        (at package8 loc1-2)
        (at package9 loc4-2)
        (at package10 loc4-1)
    )
    (:goal (and
        (at package1 loc6-1)
        (at package2 loc4-2)
        (at package3 loc3-1)
        (at package4 loc2-2)
        (at package5 loc3-1)
        (at package6 loc1-1)
        (at package7 loc4-2)
        (at package8 loc4-2)
        (at package9 loc3-2)
        (at package10 loc4-1)
    ))
)
;; a logistics problem instance
;; name: log19
;; #packages: 9        #cities: 5  #planes: 2
;; #locs_per_city: 3   #trucks_per_city: 1
;; #goals: 9           seed: 74368277

(define (problem log22)
    (:domain logistics-strips)
    (:objects 
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8
        package9
        plane1
        plane2
        truck1-1
        loc1-1
        loc1-2
        loc1-3
        city1
        truck2-1
        loc2-1
        loc2-2
        loc2-3
        city2
        truck3-1
        loc3-1
        loc3-2
        loc3-3
        city3
        truck4-1
        loc4-1
        loc4-2
        loc4-3
        city4
        truck5-1
        loc5-1
        loc5-2
        loc5-3
        city5
    )
    (:init 
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)
        (OBJ package9)
        (AIRPLANE plane1)
        (AIRPLANE plane2)
        (TRUCK truck1-1)
        (LOCATION loc1-1)
        (LOCATION loc1-2)
        (LOCATION loc1-3)
        (CITY city1)
        (AIRPORT loc1-1)
        (TRUCK truck2-1)
        (LOCATION loc2-1)
        (LOCATION loc2-2)
        (LOCATION loc2-3)
        (CITY city2)
        (AIRPORT loc2-1)
        (TRUCK truck3-1)
        (LOCATION loc3-1)
        (LOCATION loc3-2)
        (LOCATION loc3-3)
        (CITY city3)
        (AIRPORT loc3-1)
        (TRUCK truck4-1)
        (LOCATION loc4-1)
        (LOCATION loc4-2)
        (LOCATION loc4-3)
        (CITY city4)
        (AIRPORT loc4-1)
        (TRUCK truck5-1)
        (LOCATION loc5-1)
        (LOCATION loc5-2)
        (LOCATION loc5-3)
        (CITY city5)
        (AIRPORT loc5-1)
        (in-city loc1-1 city1)
        (in-city loc1-2 city1)
        (in-city loc1-3 city1)
        (in-city loc2-1 city2)
        (in-city loc2-2 city2)
        (in-city loc2-3 city2)
        (in-city loc3-1 city3)
        (in-city loc3-2 city3)
        (in-city loc3-3 city3)
        (in-city loc4-1 city4)
        (in-city loc4-2 city4)
        (in-city loc4-3 city4)
        (in-city loc5-1 city5)
        (in-city loc5-2 city5)
        (in-city loc5-3 city5)
        (at plane1 loc1-1)
        (at plane2 loc1-1)
        (at truck1-1 loc1-3)
        (at truck2-1 loc2-3)
        (at truck3-1 loc3-3)
        (at truck4-1 loc4-1)
        (at truck5-1 loc5-2)
        (at package1 loc5-3)
        (at package2 loc2-3)
        (at package3 loc4-1)
        (at package4 loc3-3)
        (at package5 loc4-3)
        (at package6 loc2-2)
        (at package7 loc1-2)
        (at package8 loc5-1)
        (at package9 loc5-1)
    )
    (:goal (and
        (at package1 loc4-3)
        (at package2 loc4-2)
        (at package3 loc2-3)
        (at package4 loc5-2)
        (at package5 loc1-2)
        (at package6 loc5-3)
        (at package7 loc3-3)
        (at package8 loc2-2)
        (at package9 loc1-1)
    ))
)
;; a logistics problem instance
;; name: log20
;; #packages: 9        #cities: 5  #planes: 2
;; #locs_per_city: 3   #trucks_per_city: 1
;; #goals: 9           seed: 79224903

(define (problem log23)
    (:domain logistics-strips)
    (:objects 
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8
        package9
        plane1
        plane2
        truck1-1
        loc1-1
        loc1-2
        loc1-3
        city1
        truck2-1
        loc2-1
        loc2-2
        loc2-3
        city2
        truck3-1
        loc3-1
        loc3-2
        loc3-3
        city3
        truck4-1
        loc4-1
        loc4-2
        loc4-3
        city4
        truck5-1
        loc5-1
        loc5-2
        loc5-3
        city5
    )
    (:init 
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)
        (OBJ package9)
        (AIRPLANE plane1)
        (AIRPLANE plane2)
        (TRUCK truck1-1)
        (LOCATION loc1-1)
        (LOCATION loc1-2)
        (LOCATION loc1-3)
        (CITY city1)
        (AIRPORT loc1-1)
        (TRUCK truck2-1)
        (LOCATION loc2-1)
        (LOCATION loc2-2)
        (LOCATION loc2-3)
        (CITY city2)
        (AIRPORT loc2-1)
        (TRUCK truck3-1)
        (LOCATION loc3-1)
        (LOCATION loc3-2)
        (LOCATION loc3-3)
        (CITY city3)
        (AIRPORT loc3-1)
        (TRUCK truck4-1)
        (LOCATION loc4-1)
        (LOCATION loc4-2)
        (LOCATION loc4-3)
        (CITY city4)
        (AIRPORT loc4-1)
        (TRUCK truck5-1)
        (LOCATION loc5-1)
        (LOCATION loc5-2)
        (LOCATION loc5-3)
        (CITY city5)
        (AIRPORT loc5-1)
        (in-city loc1-1 city1)
        (in-city loc1-2 city1)
        (in-city loc1-3 city1)
        (in-city loc2-1 city2)
        (in-city loc2-2 city2)
        (in-city loc2-3 city2)
        (in-city loc3-1 city3)
        (in-city loc3-2 city3)
        (in-city loc3-3 city3)
        (in-city loc4-1 city4)
        (in-city loc4-2 city4)
        (in-city loc4-3 city4)
        (in-city loc5-1 city5)
        (in-city loc5-2 city5)
        (in-city loc5-3 city5)
        (at plane1 loc3-1)
        (at plane2 loc1-1)
        (at truck1-1 loc1-1)
        (at truck2-1 loc2-1)
        (at truck3-1 loc3-3)
        (at truck4-1 loc4-3)
        (at truck5-1 loc5-3)
        (at package1 loc5-1)
        (at package2 loc2-3)
        (at package3 loc2-3)
        (at package4 loc4-2)
        (at package5 loc5-2)
        (at package6 loc5-3)
        (at package7 loc5-3)
        (at package8 loc2-1)
        (at package9 loc5-1)
    )
    (:goal (and
        (at package1 loc5-1)
        (at package2 loc1-2)
        (at package3 loc2-1)
        (at package4 loc5-1)
        (at package5 loc4-3)
        (at package6 loc2-2)
        (at package7 loc1-3)
        (at package8 loc4-2)
        (at package9 loc3-2)
    ))
)
;; a logistics problem instance
;; name: log21
;; #packages: 9        #cities: 5  #planes: 2
;; #locs_per_city: 3   #trucks_per_city: 1
;; #goals: 9           seed: 70675221

(define (problem log24)
    (:domain logistics-strips)
    (:objects 
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8
        package9
        plane1
        plane2
        truck1-1
        loc1-1
        loc1-2
        loc1-3
        city1
        truck2-1
        loc2-1
        loc2-2
        loc2-3
        city2
        truck3-1
        loc3-1
        loc3-2
        loc3-3
        city3
        truck4-1
        loc4-1
        loc4-2
        loc4-3
        city4
        truck5-1
        loc5-1
        loc5-2
        loc5-3
        city5
    )
    (:init 
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)
        (OBJ package9)
        (AIRPLANE plane1)
        (AIRPLANE plane2)
        (TRUCK truck1-1)
        (LOCATION loc1-1)
        (LOCATION loc1-2)
        (LOCATION loc1-3)
        (CITY city1)
        (AIRPORT loc1-1)
        (TRUCK truck2-1)
        (LOCATION loc2-1)
        (LOCATION loc2-2)
        (LOCATION loc2-3)
        (CITY city2)
        (AIRPORT loc2-1)
        (TRUCK truck3-1)
        (LOCATION loc3-1)
        (LOCATION loc3-2)
        (LOCATION loc3-3)
        (CITY city3)
        (AIRPORT loc3-1)
        (TRUCK truck4-1)
        (LOCATION loc4-1)
        (LOCATION loc4-2)
        (LOCATION loc4-3)
        (CITY city4)
        (AIRPORT loc4-1)
        (TRUCK truck5-1)
        (LOCATION loc5-1)
        (LOCATION loc5-2)
        (LOCATION loc5-3)
        (CITY city5)
        (AIRPORT loc5-1)
        (in-city loc1-1 city1)
        (in-city loc1-2 city1)
        (in-city loc1-3 city1)
        (in-city loc2-1 city2)
        (in-city loc2-2 city2)
        (in-city loc2-3 city2)
        (in-city loc3-1 city3)
        (in-city loc3-2 city3)
        (in-city loc3-3 city3)
        (in-city loc4-1 city4)
        (in-city loc4-2 city4)
        (in-city loc4-3 city4)
        (in-city loc5-1 city5)
        (in-city loc5-2 city5)
        (in-city loc5-3 city5)
        (at plane1 loc4-1)
        (at plane2 loc5-1)
        (at truck1-1 loc1-1)
        (at truck2-1 loc2-3)
        (at truck3-1 loc3-2)
        (at truck4-1 loc4-1)
        (at truck5-1 loc5-2)
        (at package1 loc4-1)
        (at package2 loc4-3)
        (at package3 loc3-2)
        (at package4 loc2-2)
        (at package5 loc5-3)
        (at package6 loc4-3)
        (at package7 loc3-2)
        (at package8 loc2-3)
        (at package9 loc3-1)
    )
    (:goal (and
        (at package1 loc3-2)
        (at package2 loc5-3)
        (at package3 loc5-2)
        (at package4 loc2-3)
        (at package5 loc4-1)
        (at package6 loc1-2)
        (at package7 loc4-2)
        (at package8 loc5-1)
        (at package9 loc1-2)
    ))
)
;; a logistics problem instance
;; name: log22
;; #packages: 9        #cities: 5  #planes: 2
;; #locs_per_city: 3   #trucks_per_city: 1
;; #goals: 9           seed: 76638317

(define (problem log25)
    (:domain logistics-strips)
    (:objects 
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8
        package9
        plane1
        plane2
        truck1-1
        loc1-1
        loc1-2
        loc1-3
        city1
        truck2-1
        loc2-1
        loc2-2
        loc2-3
        city2
        truck3-1
        loc3-1
        loc3-2
        loc3-3
        city3
        truck4-1
        loc4-1
        loc4-2
        loc4-3
        city4
        truck5-1
        loc5-1
        loc5-2
        loc5-3
        city5
    )
    (:init 
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)
        (OBJ package9)
        (AIRPLANE plane1)
        (AIRPLANE plane2)
        (TRUCK truck1-1)
        (LOCATION loc1-1)
        (LOCATION loc1-2)
        (LOCATION loc1-3)
        (CITY city1)
        (AIRPORT loc1-1)
        (TRUCK truck2-1)
        (LOCATION loc2-1)
        (LOCATION loc2-2)
        (LOCATION loc2-3)
        (CITY city2)
        (AIRPORT loc2-1)
        (TRUCK truck3-1)
        (LOCATION loc3-1)
        (LOCATION loc3-2)
        (LOCATION loc3-3)
        (CITY city3)
        (AIRPORT loc3-1)
        (TRUCK truck4-1)
        (LOCATION loc4-1)
        (LOCATION loc4-2)
        (LOCATION loc4-3)
        (CITY city4)
        (AIRPORT loc4-1)
        (TRUCK truck5-1)
        (LOCATION loc5-1)
        (LOCATION loc5-2)
        (LOCATION loc5-3)
        (CITY city5)
        (AIRPORT loc5-1)
        (in-city loc1-1 city1)
        (in-city loc1-2 city1)
        (in-city loc1-3 city1)
        (in-city loc2-1 city2)
        (in-city loc2-2 city2)
        (in-city loc2-3 city2)
        (in-city loc3-1 city3)
        (in-city loc3-2 city3)
        (in-city loc3-3 city3)
        (in-city loc4-1 city4)
        (in-city loc4-2 city4)
        (in-city loc4-3 city4)
        (in-city loc5-1 city5)
        (in-city loc5-2 city5)
        (in-city loc5-3 city5)
        (at plane1 loc4-1)
        (at plane2 loc5-1)
        (at truck1-1 loc1-1)
        (at truck2-1 loc2-2)
        (at truck3-1 loc3-1)
        (at truck4-1 loc4-1)
        (at truck5-1 loc5-2)
        (at package1 loc5-1)
        (at package2 loc5-3)
        (at package3 loc1-3)
        (at package4 loc1-1)
        (at package5 loc5-3)
        (at package6 loc5-3)
        (at package7 loc5-2)
        (at package8 loc5-2)
        (at package9 loc2-3)
    )
    (:goal (and
        (at package1 loc2-3)
        (at package2 loc3-3)
        (at package3 loc3-1)
        (at package4 loc2-3)
        (at package5 loc2-1)
        (at package6 loc4-3)
        (at package7 loc5-1)
        (at package8 loc5-2)
        (at package9 loc4-2)
    ))
)
;; a logistics problem instance
;; name: log23
;; #packages: 9        #cities: 5  #planes: 2
;; #locs_per_city: 3   #trucks_per_city: 1
;; #goals: 9           seed: 84823437

(define (problem log26)
    (:domain logistics-strips)
    (:objects 
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8
        package9
        plane1
        plane2
        truck1-1
        loc1-1
        loc1-2
        loc1-3
        city1
        truck2-1
        loc2-1
        loc2-2
        loc2-3
        city2
        truck3-1
        loc3-1
        loc3-2
        loc3-3
        city3
        truck4-1
        loc4-1
        loc4-2
        loc4-3
        city4
        truck5-1
        loc5-1
        loc5-2
        loc5-3
        city5
    )
    (:init 
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)
        (OBJ package9)
        (AIRPLANE plane1)
        (AIRPLANE plane2)
        (TRUCK truck1-1)
        (LOCATION loc1-1)
        (LOCATION loc1-2)
        (LOCATION loc1-3)
        (CITY city1)
        (AIRPORT loc1-1)
        (TRUCK truck2-1)
        (LOCATION loc2-1)
        (LOCATION loc2-2)
        (LOCATION loc2-3)
        (CITY city2)
        (AIRPORT loc2-1)
        (TRUCK truck3-1)
        (LOCATION loc3-1)
        (LOCATION loc3-2)
        (LOCATION loc3-3)
        (CITY city3)
        (AIRPORT loc3-1)
        (TRUCK truck4-1)
        (LOCATION loc4-1)
        (LOCATION loc4-2)
        (LOCATION loc4-3)
        (CITY city4)
        (AIRPORT loc4-1)
        (TRUCK truck5-1)
        (LOCATION loc5-1)
        (LOCATION loc5-2)
        (LOCATION loc5-3)
        (CITY city5)
        (AIRPORT loc5-1)
        (in-city loc1-1 city1)
        (in-city loc1-2 city1)
        (in-city loc1-3 city1)
        (in-city loc2-1 city2)
        (in-city loc2-2 city2)
        (in-city loc2-3 city2)
        (in-city loc3-1 city3)
        (in-city loc3-2 city3)
        (in-city loc3-3 city3)
        (in-city loc4-1 city4)
        (in-city loc4-2 city4)
        (in-city loc4-3 city4)
        (in-city loc5-1 city5)
        (in-city loc5-2 city5)
        (in-city loc5-3 city5)
        (at plane1 loc2-1)
        (at plane2 loc1-1)
        (at truck1-1 loc1-1)
        (at truck2-1 loc2-2)
        (at truck3-1 loc3-3)
        (at truck4-1 loc4-1)
        (at truck5-1 loc5-1)
        (at package1 loc5-2)
        (at package2 loc3-1)
        (at package3 loc4-3)
        (at package4 loc2-3)
        (at package5 loc3-1)
        (at package6 loc1-1)
        (at package7 loc4-3)
        (at package8 loc4-3)
        (at package9 loc4-3)
    )
    (:goal (and
        (at package1 loc3-2)
        (at package2 loc5-2)
        (at package3 loc4-3)
        (at package4 loc3-2)
        (at package5 loc3-1)
        (at package6 loc3-3)
        (at package7 loc3-2)
        (at package8 loc2-3)
        (at package9 loc4-3)
    ))
)
;; a logistics problem instance
;; name: log24
;; #packages: 9        #cities: 5  #planes: 2
;; #locs_per_city: 3   #trucks_per_city: 1
;; #goals: 9           seed: 93568004

(define (problem log27)
    (:domain logistics-strips)
    (:objects 
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8
        package9
        plane1
        plane2
        truck1-1
        loc1-1
        loc1-2
        loc1-3
        city1
        truck2-1
        loc2-1
        loc2-2
        loc2-3
        city2
        truck3-1
        loc3-1
        loc3-2
        loc3-3
        city3
        truck4-1
        loc4-1
        loc4-2
        loc4-3
        city4
        truck5-1
        loc5-1
        loc5-2
        loc5-3
        city5
    )
    (:init 
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)
        (OBJ package9)
        (AIRPLANE plane1)
        (AIRPLANE plane2)
        (TRUCK truck1-1)
        (LOCATION loc1-1)
        (LOCATION loc1-2)
        (LOCATION loc1-3)
        (CITY city1)
        (AIRPORT loc1-1)
        (TRUCK truck2-1)
        (LOCATION loc2-1)
        (LOCATION loc2-2)
        (LOCATION loc2-3)
        (CITY city2)
        (AIRPORT loc2-1)
        (TRUCK truck3-1)
        (LOCATION loc3-1)
        (LOCATION loc3-2)
        (LOCATION loc3-3)
        (CITY city3)
        (AIRPORT loc3-1)
        (TRUCK truck4-1)
        (LOCATION loc4-1)
        (LOCATION loc4-2)
        (LOCATION loc4-3)
        (CITY city4)
        (AIRPORT loc4-1)
        (TRUCK truck5-1)
        (LOCATION loc5-1)
        (LOCATION loc5-2)
        (LOCATION loc5-3)
        (CITY city5)
        (AIRPORT loc5-1)
        (in-city loc1-1 city1)
        (in-city loc1-2 city1)
        (in-city loc1-3 city1)
        (in-city loc2-1 city2)
        (in-city loc2-2 city2)
        (in-city loc2-3 city2)
        (in-city loc3-1 city3)
        (in-city loc3-2 city3)
        (in-city loc3-3 city3)
        (in-city loc4-1 city4)
        (in-city loc4-2 city4)
        (in-city loc4-3 city4)
        (in-city loc5-1 city5)
        (in-city loc5-2 city5)
        (in-city loc5-3 city5)
        (at plane1 loc2-1)
        (at plane2 loc2-1)
        (at truck1-1 loc1-1)
        (at truck2-1 loc2-3)
        (at truck3-1 loc3-3)
        (at truck4-1 loc4-3)
        (at truck5-1 loc5-3)
        (at package1 loc5-3)
        (at package2 loc2-1)
        (at package3 loc1-2)
        (at package4 loc3-3)
        (at package5 loc3-2)
        (at package6 loc4-1)
        (at package7 loc1-1)
        (at package8 loc5-2)
        (at package9 loc2-3)
    )
    (:goal (and
        (at package1 loc3-2)
        (at package2 loc5-3)
        (at package3 loc3-3)
        (at package4 loc2-2)
        (at package5 loc5-3)
        (at package6 loc2-3)
        (at package7 loc4-1)
        (at package8 loc3-2)
        (at package9 loc1-3)
    ))
)
;; a logistics problem instance
;; name: log25
;; #packages: 10        #cities: 6  #planes: 3
;; #locs_per_city: 3   #trucks_per_city: 1
;; #goals: 10           seed: 15940243

(define (problem log28)
    (:domain logistics-strips)
    (:objects 
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8
        package9
        package10
        plane1
        plane2
        plane3
        truck1-1
        loc1-1
        loc1-2
        loc1-3
        city1
        truck2-1
        loc2-1
        loc2-2
        loc2-3
        city2
        truck3-1
        loc3-1
        loc3-2
        loc3-3
        city3
        truck4-1
        loc4-1
        loc4-2
        loc4-3
        city4
        truck5-1
        loc5-1
        loc5-2
        loc5-3
        city5
        truck6-1
        loc6-1
        loc6-2
        loc6-3
        city6
    )
    (:init 
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)
        (OBJ package9)
        (OBJ package10)
        (AIRPLANE plane1)
        (AIRPLANE plane2)
        (AIRPLANE plane3)
        (TRUCK truck1-1)
        (LOCATION loc1-1)
        (LOCATION loc1-2)
        (LOCATION loc1-3)
        (CITY city1)
        (AIRPORT loc1-1)
        (TRUCK truck2-1)
        (LOCATION loc2-1)
        (LOCATION loc2-2)
        (LOCATION loc2-3)
        (CITY city2)
        (AIRPORT loc2-1)
        (TRUCK truck3-1)
        (LOCATION loc3-1)
        (LOCATION loc3-2)
        (LOCATION loc3-3)
        (CITY city3)
        (AIRPORT loc3-1)
        (TRUCK truck4-1)
        (LOCATION loc4-1)
        (LOCATION loc4-2)
        (LOCATION loc4-3)
        (CITY city4)
        (AIRPORT loc4-1)
        (TRUCK truck5-1)
        (LOCATION loc5-1)
        (LOCATION loc5-2)
        (LOCATION loc5-3)
        (CITY city5)
        (AIRPORT loc5-1)
        (TRUCK truck6-1)
        (LOCATION loc6-1)
        (LOCATION loc6-2)
        (LOCATION loc6-3)
        (CITY city6)
        (AIRPORT loc6-1)
        (in-city loc1-1 city1)
        (in-city loc1-2 city1)
        (in-city loc1-3 city1)
        (in-city loc2-1 city2)
        (in-city loc2-2 city2)
        (in-city loc2-3 city2)
        (in-city loc3-1 city3)
        (in-city loc3-2 city3)
        (in-city loc3-3 city3)
        (in-city loc4-1 city4)
        (in-city loc4-2 city4)
        (in-city loc4-3 city4)
        (in-city loc5-1 city5)
        (in-city loc5-2 city5)
        (in-city loc5-3 city5)
        (in-city loc6-1 city6)
        (in-city loc6-2 city6)
        (in-city loc6-3 city6)
        (at plane1 loc1-1)
        (at plane2 loc1-1)
        (at plane3 loc3-1)
        (at truck1-1 loc1-2)
        (at truck2-1 loc2-1)
        (at truck3-1 loc3-3)
        (at truck4-1 loc4-1)
        (at truck5-1 loc5-3)
        (at truck6-1 loc6-3)
        (at package1 loc4-3)
        (at package2 loc1-2)
        (at package3 loc3-1)
        (at package4 loc4-1)
        (at package5 loc4-1)
        (at package6 loc4-3)
        (at package7 loc2-3)
        (at package8 loc5-3)
        (at package9 loc6-2)
        (at package10 loc5-2)
    )
    (:goal (and
        (at package1 loc2-2)
        (at package2 loc5-2)
        (at package3 loc3-1)
        (at package4 loc4-2)
        (at package5 loc2-1)
        (at package6 loc5-2)
        (at package7 loc3-3)
        (at package8 loc1-2)
        (at package9 loc3-2)
        (at package10 loc4-3)
    ))
)
;; a logistics problem instance
;; name: log26
;; #packages: 10        #cities: 6  #planes: 3
;; #locs_per_city: 3   #trucks_per_city: 1
;; #goals: 10           seed: 3967928

(define (problem log29)
    (:domain logistics-strips)
    (:objects 
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8
        package9
        package10
        plane1
        plane2
        plane3
        truck1-1
        loc1-1
        loc1-2
        loc1-3
        city1
        truck2-1
        loc2-1
        loc2-2
        loc2-3
        city2
        truck3-1
        loc3-1
        loc3-2
        loc3-3
        city3
        truck4-1
        loc4-1
        loc4-2
        loc4-3
        city4
        truck5-1
        loc5-1
        loc5-2
        loc5-3
        city5
        truck6-1
        loc6-1
        loc6-2
        loc6-3
        city6
    )
    (:init 
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)
        (OBJ package9)
        (OBJ package10)
        (AIRPLANE plane1)
        (AIRPLANE plane2)
        (AIRPLANE plane3)
        (TRUCK truck1-1)
        (LOCATION loc1-1)
        (LOCATION loc1-2)
        (LOCATION loc1-3)
        (CITY city1)
        (AIRPORT loc1-1)
        (TRUCK truck2-1)
        (LOCATION loc2-1)
        (LOCATION loc2-2)
        (LOCATION loc2-3)
        (CITY city2)
        (AIRPORT loc2-1)
        (TRUCK truck3-1)
        (LOCATION loc3-1)
        (LOCATION loc3-2)
        (LOCATION loc3-3)
        (CITY city3)
        (AIRPORT loc3-1)
        (TRUCK truck4-1)
        (LOCATION loc4-1)
        (LOCATION loc4-2)
        (LOCATION loc4-3)
        (CITY city4)
        (AIRPORT loc4-1)
        (TRUCK truck5-1)
        (LOCATION loc5-1)
        (LOCATION loc5-2)
        (LOCATION loc5-3)
        (CITY city5)
        (AIRPORT loc5-1)
        (TRUCK truck6-1)
        (LOCATION loc6-1)
        (LOCATION loc6-2)
        (LOCATION loc6-3)
        (CITY city6)
        (AIRPORT loc6-1)
        (in-city loc1-1 city1)
        (in-city loc1-2 city1)
        (in-city loc1-3 city1)
        (in-city loc2-1 city2)
        (in-city loc2-2 city2)
        (in-city loc2-3 city2)
        (in-city loc3-1 city3)
        (in-city loc3-2 city3)
        (in-city loc3-3 city3)
        (in-city loc4-1 city4)
        (in-city loc4-2 city4)
        (in-city loc4-3 city4)
        (in-city loc5-1 city5)
        (in-city loc5-2 city5)
        (in-city loc5-3 city5)
        (in-city loc6-1 city6)
        (in-city loc6-2 city6)
        (in-city loc6-3 city6)
        (at plane1 loc4-1)
        (at plane2 loc4-1)
        (at plane3 loc5-1)
        (at truck1-1 loc1-1)
        (at truck2-1 loc2-3)
        (at truck3-1 loc3-3)
        (at truck4-1 loc4-1)
        (at truck5-1 loc5-1)
        (at truck6-1 loc6-3)
        (at package1 loc3-3)
        (at package2 loc1-3)
        (at package3 loc6-1)
        (at package4 loc3-2)
        (at package5 loc6-1)
        (at package6 loc3-2)
        (at package7 loc6-1)
        (at package8 loc1-1)
        (at package9 loc4-1)
        (at package10 loc3-1)
    )
    (:goal (and
        (at package1 loc3-3)
        (at package2 loc1-3)
        (at package3 loc5-2)
        (at package4 loc1-1)
        (at package5 loc6-2)
        (at package6 loc4-3)
        (at package7 loc5-2)
        (at package8 loc6-1)
        (at package9 loc2-3)
        (at package10 loc3-3)
    ))
)
;; a logistics problem instance
;; name: log27
;; #packages: 9        #cities: 5  #planes: 2
;; #locs_per_city: 3   #trucks_per_city: 1
;; #goals: 9           seed: 85419287

(define (problem log30)
    (:domain logistics-strips)
    (:objects 
        package1
        package2
        package3
        package4
        package5
        package6
        package7
        package8
        package9
        plane1
        plane2
        truck1-1
        loc1-1
        loc1-2
        loc1-3
        city1
        truck2-1
        loc2-1
        loc2-2
        loc2-3
        city2
        truck3-1
        loc3-1
        loc3-2
        loc3-3
        city3
        truck4-1
        loc4-1
        loc4-2
        loc4-3
        city4
        truck5-1
        loc5-1
        loc5-2
        loc5-3
        city5
    )
    (:init 
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)
        (OBJ package4)
        (OBJ package5)
        (OBJ package6)
        (OBJ package7)
        (OBJ package8)
        (OBJ package9)
        (AIRPLANE plane1)
        (AIRPLANE plane2)
        (TRUCK truck1-1)
        (LOCATION loc1-1)
        (LOCATION loc1-2)
        (LOCATION loc1-3)
        (CITY city1)
        (AIRPORT loc1-1)
        (TRUCK truck2-1)
        (LOCATION loc2-1)
        (LOCATION loc2-2)
        (LOCATION loc2-3)
        (CITY city2)
        (AIRPORT loc2-1)
        (TRUCK truck3-1)
        (LOCATION loc3-1)
        (LOCATION loc3-2)
        (LOCATION loc3-3)
        (CITY city3)
        (AIRPORT loc3-1)
        (TRUCK truck4-1)
        (LOCATION loc4-1)
        (LOCATION loc4-2)
        (LOCATION loc4-3)
        (CITY city4)
        (AIRPORT loc4-1)
        (TRUCK truck5-1)
        (LOCATION loc5-1)
        (LOCATION loc5-2)
        (LOCATION loc5-3)
        (CITY city5)
        (AIRPORT loc5-1)
        (in-city loc1-1 city1)
        (in-city loc1-2 city1)
        (in-city loc1-3 city1)
        (in-city loc2-1 city2)
        (in-city loc2-2 city2)
        (in-city loc2-3 city2)
        (in-city loc3-1 city3)
        (in-city loc3-2 city3)
        (in-city loc3-3 city3)
        (in-city loc4-1 city4)
        (in-city loc4-2 city4)
        (in-city loc4-3 city4)
        (in-city loc5-1 city5)
        (in-city loc5-2 city5)
        (in-city loc5-3 city5)
        (at plane1 loc1-1)
        (at plane2 loc3-1)
        (at truck1-1 loc1-1)
        (at truck2-1 loc2-1)
        (at truck3-1 loc3-1)
        (at truck4-1 loc4-1)
        (at truck5-1 loc5-1)
        (at package1 loc4-2)
        (at package2 loc5-3)
        (at package3 loc5-1)
        (at package4 loc5-2)
        (at package5 loc5-2)
        (at package6 loc4-3)
        (at package7 loc3-3)
        (at package8 loc2-3)
        (at package9 loc3-1)
    )
    (:goal (and
        (at package1 loc5-2)
        (at package2 loc2-2)
        (at package3 loc1-3)
        (at package4 loc1-2)
        (at package5 loc5-2)
        (at package6 loc4-1)
        (at package7 loc5-2)
        (at package8 loc2-3)
        (at package9 loc3-1)
    ))
)
