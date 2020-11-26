(define (domain  mcd-blocksworld-axiom)
    (:requirements :adl :domain-axioms :quantified-preconditions)

  (:constants Table)
  (:predicates (on ?x ?y)
	       (clear ?x)
	       (block ?b)
	       (above ?x ?y))

  (:axiom 
	  :vars (?b ?x)
	  :context (or (= ?x Table)
		       (not (exists (?b) (on ?b ?x))))
	  :implies (clear ?x))

  (:action puton
	     :parameters (?x ?y ?d)
	     :precondition (and (not (= ?x ?y)) (not (= ?x table)) (not (= ?d ?y))
				 (on ?x ?d) (clear ?x) (clear ?y))
	     :effect
	     (and (on ?x ?y) (not (on ?x ?d))
		  (forall (?c)
			  (when (or (= ?y ?c) (above ?y ?c))
				 (above ?x ?c)))
		  (forall (?e)
			   (when (and (above ?x ?e) (not (= ?y ?e))
					(not (above ?y ?e)))
				  (not (above ?x ?e)))))))