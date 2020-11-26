(define (problem ho-demo)
    (:domain homeowner)
  (:init (object wall) (object plumbing) (holey-walls) (water on))
  (:goal (and (water on) (not (holey-walls)))))

;;;UCPOP(40): (bf-control 'homeowner)
;;;
;;;Initial  : ((OBJECT WALL) (OBJECT PLUMBING) (HOLEY-WALLS) (WATER ON))
;;;
;;;Step 1  : (TURN-FAUCET OFF)      Created 3 
;;;Step 2  : (FIX PLUMBING)         Created 2 
;;;           3  -> (WATER OFF)         
;;;           0  -> (OBJECT PLUMBING)   
;;;Step 3  : (TURN-FAUCET ON)       Created 4 
;;;Step 4  : (FIX WALL)             Created 1 
;;;           2  -> (GOOD-PLUMBING)     
;;;           0  -> (OBJECT WALL)       
;;;
;;;Goal    : (AND (WATER ON) (NOT (HOLEY-WALLS)))
;;;           4  -> (WATER ON)          
;;;           1  -> (NOT (HOLEY-WALLS)) 
;;;Facts:
;;;Complete!
;;;
;;;UCPOP Stats: Initial terms = 4 ;   Goals = 3 ;  Success (4 steps)
;;;      Created 64 plans, but explored only 42
;;;      CPU time:    0.1340 sec
;;;      Branching factor:  1.143
;;;      Working Unifies: 72  
;;;      Bindings Added: 25  
;;;#plan<S=5; O=0; U=0>
;;;#Stats:<cpu time = 0.1340>

