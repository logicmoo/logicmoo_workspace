(define (problem road-test)
    (:domain road-operators)
  (:objects a d g car bulldozer)
  (:init (vehicle car)(vehicle bulldozer) 
	 (place a)(place d)(place g)
	 (at car a) (at bulldozer a)
	 (road d g) (road g d)
	 (bridge a d) (bridge d a))
  (:goal (and (at car g) (at bulldozer g))))



;;;UCPOP(23): (bf-control (road-test))
;;;
;;;Initial  : ((VEHICLE CAR) (VEHICLE BULLDOZER) (PLACE A) (PLACE D) (PLACE G)
;;;            (AT CAR A) (AT BULLDOZER A) (BRIDGE A D) (BRIDGE D A) (ROAD D G)
;;;            (ROAD G D))
;;;
;;;Step 1  : (CROSS CAR A D)       Created 4 
;;;           0  -> (AT CAR A)         
;;;           0  -> (BRIDGE A D)        
;;;Step 2  : (CROSS BULLDOZER A D)       Created 2 
;;;           0  -> (AT BULLDOZER A)         
;;;           0  -> (BRIDGE A D)        
;;;Step 3  : (DRIVE CAR D G)       Created 3 
;;;           4  -> (AT CAR D)         
;;;           0  -> (ROAD D G)          
;;;Step 4  : (DRIVE BULLDOZER D G)       Created 1 
;;;           2  -> (AT BULLDOZER D)         
;;;           0  -> (ROAD D G)          
;;;
;;;Goal    : (AND (AT CAR G) (AT BULLDOZER G))
;;;           3  -> (AT CAR G)         
;;;           1  -> (AT BULLDOZER G)         
;;;Complete!
;;;
;;;UCPOP (Init = 11 ; Goals = 3 ) => Win  (4 steps)     CPU 133      
;;;     Nodes (V = 20  ; Q = 7   ; C = 28  )             Branch 1.35      
;;;     Working Unifies: 177                             Bindings added: 43   
;;;NIL

