(DEFINE (DOMAIN ART-1D-DOMAIN) 
    (:requirements :adl)
  
  (:predicates (I-1) (I-2) (I-3) (I-4) (I-5) (I-6) (I-7) (I-8)
	       (G-1) (G-2) (G-3) (G-4) (G-5) (G-6) (G-7) (G-8))

  (:ACTION ART-1D--A-1 :PRECONDITION (AND (I-1)) :EFFECT (AND (G-1)))
  (:ACTION ART-1D--A-2 :PRECONDITION (AND (I-2)) :EFFECT (AND (G-2) (NOT (I-1))))
  (:ACTION ART-1D--A-3 :PRECONDITION (AND (I-3)) :EFFECT (AND (G-3) (NOT (I-2))))
  (:ACTION ART-1D--A-4 :PRECONDITION (AND (I-4)) :EFFECT (AND (G-4) (NOT (I-3))))
  (:ACTION ART-1D--A-5 :PRECONDITION (AND (I-5)) :EFFECT (AND (G-5) (NOT (I-4))))
  (:ACTION ART-1D--A-6 :PRECONDITION (AND (I-6)) :EFFECT (AND (G-6) (NOT (I-5))))
  (:ACTION ART-1D--A-7 :PRECONDITION (AND (I-7)) :EFFECT (AND (G-7) (NOT (I-6))))
  (:ACTION ART-1D--A-8 :PRECONDITION (AND (I-8)) :EFFECT (AND (G-8) (NOT (I-7))))) 

(DEFINE (PROBLEM ART-1D-DOMAIN1) (:DOMAIN ART-1D-DOMAIN )
	(:init (I-5) (I-3) (I-2) (I-8) (I-6) (I-7) (I-1) (I-4))
	(:length (:serial 1) (:parallel 1))
	(:GOAL (AND (G-6))))

(DEFINE (PROBLEM ART-1D-DOMAIN2) (:DOMAIN ART-1D-DOMAIN )
	(:init (I-4) (I-5) (I-2) (I-7) (I-6) (I-1) (I-3) (I-8))
	(:length (:serial 1) (:parallel 1))
	(:GOAL (AND (G-7)))) 

(DEFINE (PROBLEM ART-1D-DOMAIN3) (:DOMAIN ART-1D-DOMAIN )
	(:init (I-3) (I-6) (I-2) (I-7) (I-4) (I-5) (I-1) (I-8)) 
	(:length (:serial 1) (:parallel 1))
	(:GOAL (AND (G-3)))) 

(DEFINE (PROBLEM ART-1D-DOMAIN4) (:DOMAIN ART-1D-DOMAIN )
	(:init (I-1) (I-2) (I-7) (I-8) (I-6) (I-4) (I-3) (I-5))
	(:length (:serial 1) (:parallel 1))
	(:GOAL (AND (G-2)))) 

(DEFINE (PROBLEM ART-1D-DOMAIN5) (:DOMAIN ART-1D-DOMAIN )
	(:init (I-4) (I-6) (I-2) (I-5) (I-7) (I-3) (I-1) (I-8)) 
	(:GOAL (AND (G-7)))
	(:length (:serial 1) (:parallel 1))) 

(DEFINE (PROBLEM ART-1D-DOMAIN6) (:DOMAIN ART-1D-DOMAIN )
	(:init (I-3) (I-5) (I-7) (I-6) (I-8) (I-1) (I-2) (I-4)) 
	(:GOAL (AND (G-4) (G-5)))
	(:length (:serial 2) (:parallel 2)))

(DEFINE (PROBLEM ART-1D-DOMAIN7) (:DOMAIN ART-1D-DOMAIN )
	(:init (I-2) (I-3) (I-8) (I-4) (I-1) (I-6) (I-7) (I-5)) 
	(:GOAL (AND (G-6) (G-7)))
	(:length (:serial 2) (:parallel 2))) 

(DEFINE (PROBLEM ART-1D-DOMAIN8) (:DOMAIN ART-1D-DOMAIN )
	(:init (I-7) (I-2) (I-6) (I-1) (I-4) (I-8) (I-5) (I-3)) 
	(:GOAL (AND (G-7) (G-8)))
	(:length (:serial 2) (:parallel 2))) 

(DEFINE (PROBLEM ART-1D-DOMAIN9) (:DOMAIN ART-1D-DOMAIN )
	(:init (I-5) (I-8) (I-2) (I-4) (I-3) (I-1) (I-7) (I-6)) 
	(:GOAL (AND (G-6) (G-1)))
	(:length (:serial 2) (:parallel 1)))

(DEFINE (PROBLEM ART-1D-DOMAIN10) (:DOMAIN ART-1D-DOMAIN )
	(:init (I-2) (I-5) (I-1) (I-4) (I-6) (I-8) (I-3) (I-7)) 
	(:GOAL (AND (G-7) (G-6)))
	(:length (:serial 2) (:parallel 2))) 

(DEFINE (PROBLEM ART-1D-DOMAIN11) (:DOMAIN ART-1D-DOMAIN )
	(:init (I-7) (I-3) (I-8) (I-6) (I-5) (I-4) (I-2) (I-1)) 
	(:GOAL (AND (G-6) (G-8) (G-7)))
	(:length (:serial 3) (:parallel 3))) 

(DEFINE (PROBLEM ART-1D-DOMAIN12) (:DOMAIN ART-1D-DOMAIN )
	(:init (I-5) (I-3) (I-6) (I-7) (I-1) (I-4) (I-2) (I-8)) 
	(:GOAL (AND (G-4) (G-5) (G-6)))
	 (:length (:serial 3) (:parallel 3)))

(DEFINE (PROBLEM ART-1D-DOMAIN13) (:DOMAIN ART-1D-DOMAIN )
	(:init (I-5) (I-1) (I-4) (I-2) (I-8) (I-7) (I-6) (I-3)) 
	(:GOAL (AND (G-3) (G-2) (G-1)))
	 (:length (:serial 3) (:parallel 3)))

(DEFINE (PROBLEM ART-1D-DOMAIN14) (:DOMAIN ART-1D-DOMAIN )
	(:init (I-7) (I-1) (I-6) (I-8) (I-2) (I-4) (I-3) (I-5)) 
	(:GOAL (AND (G-3) (G-4) (G-8)))
	 (:length (:serial 3) (:parallel 2))) 

(DEFINE (PROBLEM ART-1D-DOMAIN15) (:DOMAIN ART-1D-DOMAIN )
	(:init (I-2) (I-1) (I-3) (I-5) (I-4) (I-6) (I-8) (I-7)) 
	(:GOAL (AND (G-8) (G-2) (G-5)))
	 (:length (:serial 3) (:parallel 1))) 

(DEFINE (PROBLEM ART-1D-DOMAIN16) (:DOMAIN ART-1D-DOMAIN )
	(:init (I-3) (I-1) (I-4) (I-6) (I-2) (I-5) (I-7) (I-8)) 
	(:GOAL (AND (G-6) (G-1) (G-3) (G-4)))
	 (:length (:serial 4) (:parallel 2))) 

(DEFINE (PROBLEM ART-1D-DOMAIN17) (:DOMAIN ART-1D-DOMAIN )
	(:init (I-1) (I-7) (I-3) (I-5) (I-2) (I-8) (I-4) (I-6)) 
	(:GOAL (AND (G-5) (G-3) (G-1) (G-6)))
	(:length (:serial 4) (:parallel 2))) 

(DEFINE (PROBLEM ART-1D-DOMAIN18) (:DOMAIN ART-1D-DOMAIN )
	(:init (I-7) (I-4) (I-1) (I-5) (I-6) (I-2) (I-8) (I-3)) 
	(:GOAL (AND (G-6) (G-1) (G-5) (G-8)))
	 (:length (:serial 4) (:parallel 2))) 

(DEFINE (PROBLEM ART-1D-DOMAIN19) (:DOMAIN ART-1D-DOMAIN )
	(:init (I-8) (I-3) (I-4) (I-1) (I-5) (I-7) (I-2) (I-6)) 
	(:GOAL (AND (G-1) (G-5) (G-2) (G-4)))
	(:length (:serial 4) (:parallel 2))) 

(DEFINE (PROBLEM ART-1D-DOMAIN20) (:DOMAIN ART-1D-DOMAIN )
	(:init (I-6) (I-2) (I-3) (I-4) (I-8) (I-5) (I-1) (I-7)) 
	(:GOAL (AND (G-6) (G-2) (G-1) (G-8)))
	(:length (:serial 4) (:parallel 2))) 

