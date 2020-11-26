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