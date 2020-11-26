

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