(define (problem monkey-test1)
    (:domain monkey-domain)
  (:objects p1 p2 p3 p4)
  (:init (location p1)
	 (location p2)(location p3)(location p4)
	 (at monkey p1)(on-floor)(at box p2)(at bananas p3)
	 (at knife p4))
  (:goal (AND (hasbananas)))
  (:length (:serial 6) (:parallel 6))
    )



;;;UCPOP(32): (bf-control 'monkey-test1)
;;;
;;;Initial  : ((LOCATION P1) (LOCATION P2) (LOCATION P3) (LOCATION P4)
;;;            (AT MONKEY P1) (ON-FLOOR) (AT BOX P2) (AT BANANAS P3)
;;;            (AT KNIFE P4))
;;;
;;;Step 1  : (GO-TO P4 P1)          Created 5 
;;;           0  -> (ON-FLOOR)          
;;;           0  -> (AT MONKEY P1)      
;;;Step 2  : (GET-KNIFE P4)         Created 6 
;;;           0  -> (AT KNIFE P4)       
;;;           5  -> (AT MONKEY P4)      
;;;Step 3  : (GO-TO P2 P4)          Created 4 
;;;           0  -> (ON-FLOOR)          
;;;           5  -> (AT MONKEY P4)      
;;;Step 4  : (PUSH-BOX P3 P2)       Created 3 
;;;           0  -> (AT BOX P2)         
;;;           4  -> (AT MONKEY P2)      
;;;           0  -> (ON-FLOOR)          
;;;Step 5  : (CLIMB P3)             Created 2 
;;;           3  -> (AT BOX P3)         
;;;           3  -> (AT MONKEY P3)      
;;;Step 6  : (GRAB-BANANAS P3)      Created 1 
;;;           6  -> (HASKNIFE)          
;;;           0  -> (AT BANANAS P3)     
;;;           2  -> (ONBOX P3)          
;;;
;;;Goal    : (HASBANANAS)
;;;           1  -> (HASBANANAS)        
;;;Complete!
;;;
;;;UCPOP (Init = 9  ; Goals = 1 ) => Win  (6 steps)     CPU 850      
;;;     Nodes (V = 66  ; Q = 26  ; C = 103 )             Branch 1.3939394 
;;;     Working Unifies: 875                             Bindings added: 101  
;;;NIL

