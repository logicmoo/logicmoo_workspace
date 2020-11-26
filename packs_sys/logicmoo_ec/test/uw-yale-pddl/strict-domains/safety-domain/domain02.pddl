(define (domain safety-test2-domain)
    (:requirements :strips :safety-constraints :disjunctive-preconditions
		   :universal-preconditions)

  (:safety (forall (?d) (or (data.encrypted ?d) (data.secure ?d))))

  (:predicates (data.encrypted ?d)
	       (data.secure ?d)
	       (data.sent ?d)
	       (data ?d))

  (:action SEND-DATA
      :parameters (?d)
      :precondition (data ?d)
      :effect (and (data.sent ?d)
		    (not (data.secure ?d))))
      
  (:action ENCRYPT
      :parameters (?d)
      :precondition (data ?d)
      :effect (data.encrypted ?d))
  )