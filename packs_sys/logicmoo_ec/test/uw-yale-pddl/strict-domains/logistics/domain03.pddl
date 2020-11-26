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
