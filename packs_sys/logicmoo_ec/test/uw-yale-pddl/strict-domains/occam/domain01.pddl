(define (domain movie)
  (:requirements :adl :existential-preconditions)
  (:predicates (movie ?m)
	       (review ?r)
	       (review-of ?m ?r)
	       (first ?f)
	       (last ?l)
	       (actor-in-movie ?f ?l ?m))

  (:action movie-net-reviews
	     :parameters (?m ?r)
	     :precondition
	     (and
	      )
	     :effect
	     (and
	      (movie ?m)
	      (review ?r)
	      (review-of ?m ?r)
	      ))
  (:action movie-now-actor
	     :parameters (?f ?l ?m)
	     :precondition
	     (and
	      (first ?f)
	      (last ?l))
	     :effect
	     (and
	      (movie ?m)
	      (actor-in-movie ?f ?l ?m)
              ))
  )





