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


;UCPOP(38): (bf-control 'query-for-first-names)

;Initial  : ((OFFICE OFFICE))
;
;Step 1  : (USERID-ROOM ?USERID2 OFFICE ?FIRST2 ?LAST2)   Created 2 
;           0  -> (OFFICE OFFICE)     
;Step 2  : (FINGER ?FIRST2 ?LAST2 ?USERID2 ?OFFICE1 ?PHONE1)   Created 1 
;           2  -> (USERID ?USERID2)   
;
;Goal    : (EXISTS (?FGOAL) (FIRST ?FGOAL)
;           (EXISTS (?LGOAL) (LAST ?LGOAL)
;            (AND (OFFICE-OF OFFICE ?FGOAL ?LGOAL))))
;           2  -> (OFFICE-OF OFFICE ?FIRST2 ?LAST2)
;           1  -> (LAST ?LAST2)       
;           1  -> (FIRST ?FIRST2)     
;Facts:
;Complete!

;UCPOP Stats: Initial terms = 1 ;   Goals = 3 ;  Success (2 steps)
;      Created 11 plans, but explored only 5
;      CPU time:    0.0170 sec
;      Branching factor:  2.000
;      Working Unifies: 18  
;      Bindings Added: 15  
;#plan<S=3; O=0; U=0; F=0>
;#Stats:<cpu time = 0.0170>
;[1] UCPOP(39): :res
;UCPOP(40): " 

