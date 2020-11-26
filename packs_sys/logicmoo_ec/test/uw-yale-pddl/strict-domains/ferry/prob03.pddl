(define (problem test-ferry)
    (:domain ferry-typed)
  (:objects a b - place
	    c1 c2 - auto)
  (:init (at c1 a)(at c2 a)(at-ferry a)
	 (empty-ferry))
  (:length (:serial 7 ) (:parallel 7)) 
  (:goal (and (at c1 b)(at c2 b))))



;;;UCPOP(25): (bf-control 'test-ferry)
;;;
;;;Initial  : ((PLACE A) (PLACE B) (AUTO C1) (AUTO C2) (AT C1 A) (AT C2 A)
;;;            (AT-FERRY A) (EMPTY-FERRY))
;;;
;;;Step 1  : (BOARD C2 A)           Created 3 
;;;           0  -> (AT C2 A)           
;;;           0  -> (AT-FERRY A)        
;;;           0  -> (EMPTY-FERRY)       
;;;           0  -> (AUTO C2)           
;;;           0  -> (PLACE A)           
;;;Step 2  : (SAIL A B)             Created 2 
;;;           0  -> (AT-FERRY A)        
;;;           0  -> (PLACE A)           
;;;           0  -> (PLACE B)           
;;;Step 3  : (DEBARK C2 B)          Created 1 
;;;           3  -> (ON C2 FERRY)       
;;;           2  -> (AT-FERRY B)        
;;;           0  -> (AUTO C2)           
;;;           0  -> (PLACE B)           
;;;Step 4  : (SAIL B A)             Created 6 
;;;           2  -> (AT-FERRY B)        
;;;           0  -> (PLACE B)           
;;;           0  -> (PLACE A)           
;;;Step 5  : (BOARD C1 A)           Created 7 
;;;           0  -> (AT C1 A)           
;;;           6  -> (AT-FERRY A)        
;;;           1  -> (EMPTY-FERRY)       
;;;           0  -> (AUTO C1)           
;;;           0  -> (PLACE A)           
;;;Step 6  : (SAIL A B)             Created 5 
;;;           6  -> (AT-FERRY A)        
;;;           0  -> (PLACE A)           
;;;           0  -> (PLACE B)           
;;;Step 7  : (DEBARK C1 B)          Created 4 
;;;           7  -> (ON C1 FERRY)       
;;;           5  -> (AT-FERRY B)        
;;;           0  -> (AUTO C1)           
;;;           0  -> (PLACE B)           
;;;
;;;Goal    : (AND (AT C1 B) (AT C2 B))
;;;           4  -> (AT C1 B)           
;;;           1  -> (AT C2 B)           
;;;Complete!
;;;
;;;UCPOP (Init = 8  ; Goals = 3 ) => Win  (7 steps)     CPU 2633     
;;;     Nodes (V = 488 ; Q = 153 ; C = 786 )             Branch 1.3135246 
;;;     Working Unifies: 2194                            Bindings added: 362  
;;;NIL
