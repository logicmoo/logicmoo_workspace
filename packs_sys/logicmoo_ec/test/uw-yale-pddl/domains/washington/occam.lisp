;; Occam.lisp

"The code below contains two correct, clean, working examples of occam domains
coded for UCPOP4.0.  They should be much easier to understand than my previous
attempt.  Actor-movie-review does the simplest two-step
plan with a single ordering.  Query-for-first-names is the domain from the
AAAI-96 Kwok and Weld paper that shows how natural it is to deal with unbound
variables in the operators (?first and ?last in userid-room).

I now understand the sentence from AAAI-96, p.38:  'But OCCAM allows ...
representing information sources that generate information which translates
into partially specified sentences in the world model.'

First of all, this is not quite right, and it is confusing.
What you really mean to say is, 'OCCAM allows ... sites that contain
partially specified tuples, like <Rich Segal segal 429 ???-????>.'  
The two statements are the same only if sentences
in the world model are ground site predicates like 

    'Finger knows the tuple <Rich Segal segal 429 543-5129>.'

Clearly, this is not true in OCCAM or Information Manifold, both of which 
make a clean distinction between the world model and the site model.  OCCAM's
world model might look like:

(name-of Rich Segal <person94>)
(userid-of segal <person94>)
(office-of 429 <person94>)

So this claim really amounts to the claim that OCCAM can do projective
mappings onto useful variables, which is not much of a claim.  SIMS does
this trivially by mapping to a data model in which all predicates are unary
(types) or binary (roles) anyway.

Second of all, this 'deep' statement has virtually no impact
on the implementation of unbound variables in operators. It just means that
operators are allowed to mention (existential) hidden values that cannot be
used as outputs, like ?first and ?last in userid-room.
(the unbound variables in operators)

Unbound variables in queries are even simpler to implement, by the way:
they are just output variables (like free variables) that don't get output.
I make no distinction below, because I assume there is an implicit join and
project-onto-relevant-attributes at the end of each plan.

-- Marc"

(in-package :domains)

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

(define (problem actor-movie-review)
    (:domain movie)
    (:objects firstname lastname)
    (:init  (first firstname)
	    (last lastname)
   	    )
    (:goal
     (AND (exists (?m1)
		  (and (movie ?m1)
		       (exists (?r1)
			       (and (review ?r1)
				    (actor-in-movie firstname lastname ?m1)
				    (review-of ?m1 ?r1)
				    (movie ?m1)
				    (review ?r1)
				    )))))))


"UCPOP(36): (bf-control 'actor-movie-review)

Initial  : ((FIRST FIRST) (LAST LAST))

Step 1  : (MOVIE-NOW-ACTOR FIRST LAST ?M2)   Created 2 
           0  -> (LAST LAST)         
           0  -> (FIRST FIRST)       
Step 2  : (MOVIE-NET-REVIEWS ?M2 ?R1)   Created 1 

Goal    : (EXISTS (?M1GOAL) (MOVIE ?M1GOAL)
           (EXISTS (?R1GOAL) (REVIEW ?R1GOAL)
            (AND (ACTOR-IN-MOVIE FIRST LAST ?M1GOAL)
             (REVIEW-OF ?M1GOAL ?R1GOAL) (MOVIE ?M1GOAL) (REVIEW ?R1GOAL))))
           1  -> (REVIEW ?R1)        
           2  -> (MOVIE ?M2)         
           1  -> (REVIEW-OF ?M2 ?R1) 
           2  -> (ACTOR-IN-MOVIE FIRST LAST ?M2)
           1  -> (REVIEW ?R1)        
           1  -> (MOVIE ?M2)         
Facts:
Complete!

UCPOP Stats: Initial terms = 2 ;   Goals = 3 ;  Success (2 steps)
      Created 17 plans, but explored only 9
      CPU time:    0.0330 sec
      Branching factor:  1.778
      Working Unifies: 22  
      Bindings Added: 15  
#plan<S=3; O=0; U=0; F=0>
#Stats:<cpu time = 0.0330>"

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

(define (problem query-for-first-names)
    (:domain people)
    (:objects the-office)
    (:init (office the-office))
    (:goal
     (and (exists (?f)
		  (and (first ?f)
		       (exists (?l)
			       (and (last ?l)
				    (office-of the-office ?f ?l)
				    )))))))
#|
"UCPOP(38): (bf-control 'query-for-first-names)

Initial  : ((OFFICE OFFICE))

Step 1  : (USERID-ROOM ?USERID2 OFFICE ?FIRST2 ?LAST2)   Created 2 
           0  -> (OFFICE OFFICE)     
Step 2  : (FINGER ?FIRST2 ?LAST2 ?USERID2 ?OFFICE1 ?PHONE1)   Created 1 
           2  -> (USERID ?USERID2)   

Goal    : (EXISTS (?FGOAL) (FIRST ?FGOAL)
           (EXISTS (?LGOAL) (LAST ?LGOAL)
            (AND (OFFICE-OF OFFICE ?FGOAL ?LGOAL))))
           2  -> (OFFICE-OF OFFICE ?FIRST2 ?LAST2)
           1  -> (LAST ?LAST2)       
           1  -> (FIRST ?FIRST2)     
Facts:
Complete!

UCPOP Stats: Initial terms = 1 ;   Goals = 3 ;  Success (2 steps)
      Created 11 plans, but explored only 5
      CPU time:    0.0170 sec
      Branching factor:  2.000
      Working Unifies: 18  
      Bindings Added: 15  
#plan<S=3; O=0; U=0; F=0>
#Stats:<cpu time = 0.0170>
[1] UCPOP(39): :res
UCPOP(40): " |#
