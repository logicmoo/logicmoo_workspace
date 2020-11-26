;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The same thing, without typing.

(define (domain fridge-domain-strips)
    (:requirements :adl)
  
  (:predicates (screwed ?s)
	       (holds ?s ?b)
	       (in-place ?b)
	       (part-of ?b ?f)
	       (fridge-on ?f)
	       (fridge ?f)
	       (covers ?b ?x)
	       (attached ?x)
	       (ok ?c)
	       (screw ?s)
	       (backplane ?b)
	       (compressor ?c))

  (:action unfasten
	     :parameters (?x ?y)
			  :precondition (AND 
					 (screw ?x) (backplane ?y)
					 (screwed ?X) (holds ?x ?y) )
	     :effect (NOT (screwed ?X)))
  (:action fasten
	     :parameters (?x ?y)
	     :precondition (AND 
			    (screw ?x) (backplane ?y)
			    (NOT (screwed ?X)) (holds ?x ?y))
	     :effect (screwed ?X))
  (:action remove-backplane
	     :parameters (?x ?f ?a ?b ?c ?d)
	     :precondition (AND 
			    (backplane ?x)
			    (not (= ?a ?b)) (not (= ?a ?c)) (not (= ?a ?d))
				 (not (= ?b ?c)) (not (= ?b ?d)) (not (= ?c ?d))
				 (in-place ?x) (part-of ?x ?f) (NOT (fridge-on ?f))
				 (holds ?a ?x)  (holds ?b ?x)  
				 (holds ?c ?x)  (holds ?d ?x)
				 (NOT (screwed ?a)) (NOT (screwed ?b)) 
				 (NOT (screwed ?c)) (NOT (screwed ?d)))
	     :effect (NOT (in-place ?X)))
  (:action attach-backplane
	     :parameters (?x ?f ?a ?b ?c ?d)
	     :precondition (AND 
			    (backplane ?x)
			    (not (= ?a ?b)) (not (= ?a ?c)) (not (= ?a ?d))
				 (not (= ?b ?c)) (not (= ?b ?d)) (not (= ?c ?d))
				 (NOT (in-place ?x))
				 (part-of ?x ?f) (NOT (fridge-on ?f))
				 (holds ?a ?x)  (holds ?b ?x) 
				 (holds ?c ?x)  (holds ?d ?x)
				 (NOT (screwed ?a)) (NOT (screwed ?b))
				 (NOT (screwed ?c)) (NOT (screwed ?d)))
	     :effect (in-place ?X))
  (:action start-fridge
	     :parameters (?f ?a ?b ?c ?d ?x)
	     :precondition (AND (not (= ?a ?b)) (not (= ?a ?c)) (not (= ?a ?d))
				 (not (= ?b ?c)) (not (= ?b ?d)) (not (= ?c ?d))
				 (backplane ?x) (in-place ?x) (part-of ?x ?f)
				 (holds ?a ?x)(holds ?b ?x)(holds ?c ?x)(holds ?d ?x)
				 (screwed ?a) (screwed ?b) (screwed ?c) (screwed ?d)
				 (NOT (fridge-on ?f)))
	     :effect (fridge-on ?f))
  (:action stop-fridge
	     :parameters (?f)
	     :precondition (fridge-on ?f)
	     :effect
	     (NOT (fridge-on ?f)))
  (:action change-compressor
	     :parameters (?x ?y ?a)
	     :precondition (AND (not (= ?x ?y)) (backplane ?a) (NOT (in-place ?a))
				 (covers ?a ?x)
				 (compressor ?x) (compressor ?y) 
				 (attached ?x) (NOT (attached ?y)))
	     :effect (AND (NOT (attached ?X)) (attached ?y)
			   (NOT (covers ?a ?x)) (covers ?a ?y))))