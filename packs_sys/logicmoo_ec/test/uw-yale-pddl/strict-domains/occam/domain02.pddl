
(define (domain people)
  (:requirements :adl :existential-preconditions)
  (:predicates (office ?o)
	       (userid ?u)
	       (office-of ?u ?f ?l)
	       (userid-of ?u ?f ?l)
	       (first ?f)
	       (last ?l)
	       (phone ?ph)
	       (phone-at-loc ?ph ?o))
  (:action userid-room
	     :parameters (?userid ?office ?first ?last)
	     :precondition
	     (and
	      (office ?office)
	      )
	     :effect
	     (and
	      (userid ?userid)
	      (office-of ?office ?first ?last)
	      (userid-of ?userid ?first ?last)))
  (:action finger
	     :parameters (?first ?last ?userid ?office ?phone)
	     :precondition
	     (and
	      (userid ?userid)
	      )
	     :effect
	     (and
	      (first ?first)
	      (last ?last)
	      (office ?office)
	      (phone ?phone)
	      (userid-of ?userid ?first ?last)
	      (office-of ?office ?first ?last)
	      (phone-at-loc ?phone ?office)
	      ))
  )