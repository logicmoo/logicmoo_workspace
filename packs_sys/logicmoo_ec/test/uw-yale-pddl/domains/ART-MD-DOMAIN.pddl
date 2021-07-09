(DEFINE (DOMAIN ART-MD-DOMAIN) 
    (:requirements :adl)
  
  (:predicates (I-1) (I-2) (I-3) (I-4) (I-5) (I-6) (I-7) (I-8)
	       (G-1) (G-2) (G-3) (G-4) (G-5) (G-6) (G-7) (G-8))

  (:ACTION ART-MD--A-1  :PRECONDITION (AND (I-1)) :EFFECT (AND (G-1)))
  (:ACTION ART-MD--A-2  :PRECONDITION (AND (I-2)) :EFFECT (AND (G-2) (NOT (I-1))))
  (:ACTION ART-MD--A-3  :PRECONDITION (AND (I-3)) :EFFECT (AND (G-3) (NOT (I-1)) (NOT (I-2))))
  (:ACTION ART-MD--A-4  :PRECONDITION (AND (I-4)) :EFFECT (AND (G-4) (NOT (I-1)) (NOT (I-2)) 
							       (NOT (I-3))))
  (:ACTION ART-MD--A-5  :PRECONDITION (AND (I-5)) :EFFECT
	   (AND (G-5) (NOT (I-1)) (NOT (I-2)) (NOT (I-3)) (NOT (I-4))))
  (:ACTION ART-MD--A-6  :PRECONDITION (AND (I-6)) :EFFECT
	   (AND (G-6) (NOT (I-1)) (NOT (I-2)) (NOT (I-3)) (NOT (I-4)) (NOT (I-5))))
  (:ACTION ART-MD--A-7  :PRECONDITION (AND (I-7)) :EFFECT
	   (AND (G-7) (NOT (I-1)) (NOT (I-2)) (NOT (I-3)) (NOT (I-4)) (NOT (I-5)) (NOT (I-6))))
  (:ACTION ART-MD--A-8  :PRECONDITION (AND (I-8)) :EFFECT
	   (AND (G-8) (NOT (I-1)) (NOT (I-2)) (NOT (I-3)) (NOT (I-4)) (NOT (I-5)) (NOT (I-6)) 
		(NOT (I-7))))) 

(DEFINE (PROBLEM ART-MD-DOMAIN1) (:DOMAIN ART-MD-DOMAIN) 
	(:init (I-6) (I-7) (I-3) (I-4) (I-2) (I-8) (I-5) (I-1)) 
	(:GOAL (AND (G-2)))
	(:length (:serial 1) (:parallel 1)))

(DEFINE (PROBLEM ART-MD-DOMAIN2) 
        (:DOMAIN ART-MD-DOMAIN) 
	(:init (I-4) (I-8) (I-2) (I-5) (I-6) (I-1) (I-7) (I-3)) 
	(:GOAL (AND (G-7)))
	(:length (:serial 1) (:parallel 1))) 

(DEFINE (PROBLEM ART-MD-DOMAIN3) (:DOMAIN ART-MD-DOMAIN) 
	(:init (I-3) (I-4) (I-2) (I-6) (I-7) (I-5) (I-1) (I-8)) 
	(:GOAL (AND (G-7)))
	(:length (:serial 1) (:parallel 1)))

(DEFINE (PROBLEM ART-MD-DOMAIN4) (:DOMAIN ART-MD-DOMAIN) 
	(:init (I-5) (I-8) (I-6) (I-3) (I-2) (I-4) (I-1) (I-7)) 
	(:GOAL (AND (G-3)))
	(:length (:serial 1) (:parallel 1))) 

(DEFINE (PROBLEM ART-MD-DOMAIN5) (:DOMAIN ART-MD-DOMAIN) 
	(:init (I-1) (I-7) (I-2) (I-8) (I-3) (I-6) (I-4) (I-5)) 
	(:GOAL (AND (G-6)))
	(:length (:serial 1) (:parallel 1)))

(DEFINE (PROBLEM ART-MD-DOMAIN6) (:DOMAIN ART-MD-DOMAIN) 
	(:init (I-6) (I-8) (I-5) (I-7) (I-2) (I-4) (I-3) (I-1)) 
	(:GOAL (AND (G-5) (G-6)))
	(:length (:serial 2) (:parallel 2))) 

(DEFINE (PROBLEM ART-MD-DOMAIN7) (:DOMAIN ART-MD-DOMAIN) 
	(:init (I-6) (I-2) (I-1) (I-3) (I-4) (I-5) (I-8) (I-7)) 
	(:GOAL (AND (G-6) (G-2)))
	(:length (:serial 2) (:parallel 2))) 

(DEFINE (PROBLEM ART-MD-DOMAIN8) (:DOMAIN ART-MD-DOMAIN) 
	(:init (I-8) (I-7) (I-1) (I-5) (I-2) (I-3) (I-6) (I-4)) 
	(:GOAL (AND (G-6) (G-2)))
	(:length (:serial 2) (:parallel 2)))

(DEFINE (PROBLEM ART-MD-DOMAIN9) (:DOMAIN ART-MD-DOMAIN) 
	(:init (I-7) (I-8) (I-1) (I-4) (I-5) (I-6) (I-3) (I-2)) 
	(:GOAL (AND (G-6) (G-5)))
	(:length (:serial 2) (:parallel 2)))

(DEFINE (PROBLEM ART-MD-DOMAIN10) (:DOMAIN ART-MD-DOMAIN) 
	(:init (I-8) (I-2) (I-1) (I-4) (I-7) (I-3) (I-5) (I-6)) 
	(:GOAL (AND (G-8) (G-4)))
	(:length (:serial 2) (:parallel 2))) 

(DEFINE (PROBLEM ART-MD-DOMAIN11) (:DOMAIN ART-MD-DOMAIN) 
	(:init (I-6) (I-2) (I-8) (I-3) (I-4) (I-1) (I-5) (I-7)) 
	(:GOAL (AND (G-2) (G-3) (G-7)))
	(:length (:serial 3) (:parallel 3))) 

(DEFINE (PROBLEM ART-MD-DOMAIN12) (:DOMAIN ART-MD-DOMAIN) 
	(:init (I-1) (I-3) (I-8) (I-2) (I-4) (I-5) (I-7) (I-6)) 
	(:GOAL (AND (G-2) (G-4) (G-8)))
	(:length (:serial 3) (:parallel 3))) 

(DEFINE (PROBLEM ART-MD-DOMAIN13) (:DOMAIN ART-MD-DOMAIN) 
	(:init (I-7) (I-2) (I-1) (I-4) (I-3) (I-5) (I-6) (I-8)) 
	(:GOAL (AND (G-7) (G-1) (G-4)))
	(:length (:serial 3) (:parallel 3)))

(DEFINE (PROBLEM ART-MD-DOMAIN14) (:DOMAIN ART-MD-DOMAIN) 
	(:init (I-4) (I-8) (I-1) (I-6) (I-3) (I-5) (I-7) (I-2)) 
	(:GOAL (AND (G-7) (G-2) (G-8)))
	(:length (:serial 3) (:parallel 3)))

(DEFINE (PROBLEM ART-MD-DOMAIN15) (:DOMAIN ART-MD-DOMAIN) 
	(:init (I-2) (I-5) (I-4) (I-8) (I-1) (I-6) (I-7) (I-3)) 
	(:GOAL (AND (G-2) (G-6) (G-1)))
	(:length (:serial 3) (:parallel 3))) 

(DEFINE (PROBLEM ART-MD-DOMAIN16) (:DOMAIN ART-MD-DOMAIN) 
	(:init (I-2) (I-5) (I-4) (I-3) (I-8) (I-6) (I-1) (I-7)) 
	(:GOAL (AND (G-1) (G-4) (G-3) (G-7)))
	(:length (:serial 4) (:parallel 4)))

(DEFINE (PROBLEM ART-MD-DOMAIN17) (:DOMAIN ART-MD-DOMAIN) 
	(:init (I-1) (I-7) (I-3) (I-8) (I-2) (I-6) (I-5) (I-4)) 
	(:GOAL (AND (G-8) (G-7) (G-6) (G-2)))
	(:length (:serial 4) (:parallel 4))) 

(DEFINE (PROBLEM ART-MD-DOMAIN18) (:DOMAIN ART-MD-DOMAIN) 
	(:init (I-6) (I-2) (I-3) (I-5) (I-1) (I-4) (I-7) (I-8)) 
	(:GOAL (AND (G-1) (G-2) (G-3) (G-8)))
	(:length (:serial 4) (:parallel 4))) 

(DEFINE (PROBLEM ART-MD-DOMAIN19) (:DOMAIN ART-MD-DOMAIN) 
	(:init (I-5) (I-3) (I-1) (I-2) (I-8) (I-7) (I-6) (I-4)) 
	(:GOAL (AND (G-2) (G-4) (G-1) (G-6)))
	(:length (:serial 4) (:parallel 4))) 

(DEFINE (PROBLEM ART-MD-DOMAIN20) (:DOMAIN ART-MD-DOMAIN) 
	(:init (I-8) (I-2) (I-4) (I-1) (I-5) (I-7) (I-6) (I-3)) 
	(:GOAL (AND (G-1) (G-4) (G-6) (G-3)))
	(:length (:serial 4) (:parallel 4))) 


















