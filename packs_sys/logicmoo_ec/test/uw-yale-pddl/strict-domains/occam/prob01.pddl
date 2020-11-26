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


;UCPOP(36): (bf-control 'actor-movie-review)
;
;Initial  : ((FIRST FIRST) (LAST LAST))
;
;Step 1  : (MOVIE-NOW-ACTOR FIRST LAST ?M2)   Created 2 
;           0  -> (LAST LAST)         
;           0  -> (FIRST FIRST)       
;Step 2  : (MOVIE-NET-REVIEWS ?M2 ?R1)   Created 1 
;
;Goal    : (EXISTS (?M1GOAL) (MOVIE ?M1GOAL)
;           (EXISTS (?R1GOAL) (REVIEW ?R1GOAL)
;            (AND (ACTOR-IN-MOVIE FIRST LAST ?M1GOAL)
;             (REVIEW-OF ?M1GOAL ?R1GOAL) (MOVIE ?M1GOAL) (REVIEW ?R1GOAL))))
;           1  -> (REVIEW ?R1)        
;           2  -> (MOVIE ?M2)         
;           1  -> (REVIEW-OF ?M2 ?R1) 
;           2  -> (ACTOR-IN-MOVIE FIRST LAST ?M2)
;           1  -> (REVIEW ?R1)        
;           1  -> (MOVIE ?M2)         
;Facts:
;Complete!
;
;UCPOP Stats: Initial terms = 2 ;   Goals = 3 ;  Success (2 steps)
;      Created 17 plans, but explored only 9
;      CPU time:    0.0330 sec
;      Branching factor:  1.778
;      Working Unifies: 22  
;      Bindings Added: 15  
;#plan<S=3; O=0; U=0; F=0>
;#Stats:<cpu time = 0.0330>
