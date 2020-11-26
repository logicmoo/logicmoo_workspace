(DEFINE (DOMAIN ART-1D-RD-DOMAIN)
    (:requirements :adl)

  (:predicates (I-1) (I-2) (I-3) (I-4) (I-5) (I-6) (I-7) (I-8)
	       (G-1) (G-2) (G-3) (G-4) (G-5) (G-6) (G-7) (G-8)
	       (HE) (HF))

  (:ACTION ART-1D-RD--A-1 
	   :PRECONDITION (AND (I-1) (HF)) 
	   :EFFECT (AND (G-1) (HE) (NOT (HF))))
  (:ACTION ART-1D-RD--A-2 
	   :PRECONDITION (AND (I-2) (HE)) 
	   :EFFECT (AND (G-2) (HF) (NOT (HE)) (NOT (I-1))))
  (:ACTION ART-1D-RD--A-3 
	   :PRECONDITION (AND (I-3) (HF)) 
	   :EFFECT (AND (G-3) (HE) (NOT (HF)) (NOT (I-2))))
  (:ACTION ART-1D-RD--A-4 
	   :PRECONDITION (AND (I-4) (HE)) 
	   :EFFECT (AND (G-4) (HF) (NOT (HE)) (NOT (I-3))))
  (:ACTION ART-1D-RD--A-5 
	   :PRECONDITION (AND (I-5) (HF)) 
	   :EFFECT (AND (G-5) (HE) (NOT (HF)) (NOT (I-4))))
  (:ACTION ART-1D-RD--A-6 
	   :PRECONDITION (AND (I-6) (HE)) 
	   :EFFECT (AND (G-6) (HF) (NOT (HE)) (NOT (I-5))))
  (:ACTION ART-1D-RD--A-7 
	   :PRECONDITION (AND (I-7) (HF)) 
	   :EFFECT (AND (G-7) (HE) (NOT (HF)) (NOT (I-6))))
  (:ACTION ART-1D-RD--A-8 
	   :PRECONDITION (AND (I-8) (HE)) 
	   :EFFECT (AND (G-8) (HF) (NOT (HE)) (NOT (I-7)))))

(DEFINE (PROBLEM ART-1D-RD-DOMAIN1) (:DOMAIN ART-1D-RD-DOMAIN) 
	(:init (HE) (I-7) (I-1) (I-2) (I-6) (HF) (I-5) (I-8) (I-4) (I-3)) 
	(:GOAL (AND (G-7)))
	(:length (:serial 1) (:parallel 1))) 

(DEFINE (PROBLEM ART-1D-RD-DOMAIN2) (:DOMAIN ART-1D-RD-DOMAIN) 
	(:init (I-7) (I-6) (I-5) (I-4) (HE) (I-2) (I-8) (I-3) (I-1) (HF)) 
        (:GOAL (AND (G-7)))
	 (:length (:serial 1) (:parallel 1)))

(DEFINE (PROBLEM ART-1D-RD-DOMAIN3) (:DOMAIN ART-1D-RD-DOMAIN) 
	(:init (I-4) (I-6) (HE) (I-7) (I-2) (I-5) (HF) (I-1) (I-8) (I-3)) 
	(:GOAL (AND (G-4)))
	(:length (:serial 1) (:parallel 1)))

(DEFINE (PROBLEM ART-1D-RD-DOMAIN4) (:DOMAIN ART-1D-RD-DOMAIN) 
	(:init (HE) (I-7) (I-6) (I-8) (I-3) (I-1) (HF) (I-2) (I-5) (I-4)) 
	(:GOAL (AND (G-3)))
	(:length (:serial 1) (:parallel 1)))

(DEFINE (PROBLEM ART-1D-RD-DOMAIN5) (:DOMAIN ART-1D-RD-DOMAIN) 
	(:init (I-1) (I-4) (I-3) (HF) (I-7) (I-6) (I-8) (I-5) (I-2) (HE)) 
	(:GOAL (AND (G-4)))
	(:length (:serial 1) (:parallel 1))) 

(DEFINE (PROBLEM ART-1D-RD-DOMAIN6) (:DOMAIN ART-1D-RD-DOMAIN) 
	(:init (I-2) (I-8) (I-6) (I-3) (I-4) (HF) (I-5) (I-1) (HE) (I-7)) 
	(:GOAL (AND (G-6) (G-3)))
	(:length (:serial 2) (:parallel 2))) 

(DEFINE (PROBLEM ART-1D-RD-DOMAIN7) (:DOMAIN ART-1D-RD-DOMAIN) 
	(:init (I-4) (I-5) (HF) (I-7) (HE) (I-8) (I-6) (I-2) (I-1) (I-3)) 
	(:GOAL (AND (G-7) (G-4)))
	(:length (:serial 2) (:parallel 2))) 

(DEFINE (PROBLEM ART-1D-RD-DOMAIN8) (:DOMAIN ART-1D-RD-DOMAIN) 
	(:init (I-8) (I-7) (I-2) (I-6) (I-5) (I-4) (HF) (I-1) (HE) (I-3)) 
	(:GOAL (AND (G-3) (G-2)))
	(:length (:serial 2) (:parallel 2)))

(DEFINE (PROBLEM ART-1D-RD-DOMAIN9) (:DOMAIN ART-1D-RD-DOMAIN)
	(:init (I-7) (I-2) (I-1) (I-3) (HE) (HF) (I-6) (I-4) (I-8) (I-5)) 
	(:GOAL (AND (G-4) (G-2)))
	(:length (:serial 3) (:parallel 3)))

(DEFINE (PROBLEM ART-1D-RD-DOMAIN10) (:DOMAIN ART-1D-RD-DOMAIN) 
	(:init (I-7) (I-2) (I-6) (HF) (I-3) (I-4) (HE) (I-1) (I-8) (I-5)) 
	(:GOAL (AND (G-5) (G-1)))
	(:length (:serial 3) (:parallel 3))) 

(DEFINE (PROBLEM ART-1D-RD-DOMAIN11) (:DOMAIN ART-1D-RD-DOMAIN) 
	(:init (I-6) (HE) (I-7) (I-5) (I-2) (HF) (I-4) (I-3) (I-1) (I-8)) 
	(:GOAL (AND (G-6) (G-3) (G-1)))
	(:length (:serial 3) (:parallel 3)))

(DEFINE (PROBLEM ART-1D-RD-DOMAIN12) (:DOMAIN ART-1D-RD-DOMAIN) 
	(:init (I-4) (I-8) (I-6) (I-5) (I-2) (I-7) (HE) (HF) (I-3) (I-1)) 
	(:GOAL (AND (G-5) (G-8) (G-6)))
	(:length (:serial 3) (:parallel 3))) 

(DEFINE (PROBLEM ART-1D-RD-DOMAIN13) (:DOMAIN ART-1D-RD-DOMAIN) 
	(:init (I-4) (I-5) (HF) (I-8) (I-2) (I-6) (HE) (I-7) (I-3) (I-1)) 
	(:GOAL (AND (G-3) (G-4) (G-1)))
	(:length (:serial 3) (:parallel 3))) 

(DEFINE (PROBLEM ART-1D-RD-DOMAIN14) (:DOMAIN ART-1D-RD-DOMAIN) 
	(:init (I-5) (I-6) (HE) (I-4) (I-8) (I-7) (I-2) (I-1) (I-3) (HF)) 
	(:GOAL (AND (G-4) (G-5) (G-2)))
	(:length (:serial 3) (:parallel 3))) 

(DEFINE (PROBLEM ART-1D-RD-DOMAIN15) (:DOMAIN ART-1D-RD-DOMAIN) 
	(:init (I-3) (I-2) (HE) (I-1) (I-8) (I-6) (I-5) (I-7) (I-4) (HF)) 
	(:GOAL (AND (G-4) (G-5) (G-8)))
	(:length (:serial 3) (:parallel 3))) 

(DEFINE (PROBLEM ART-1D-RD-DOMAIN16) (:DOMAIN ART-1D-RD-DOMAIN) 
	(:init (I-3) (I-2) (HE) (I-7) (I-4) (I-5) (I-1) (I-6) (I-8) (HF)) 
	(:GOAL (AND (G-8) (G-2) (G-3) (G-4)))
	(:length (:serial 5) (:parallel 5))) 

(DEFINE (PROBLEM ART-1D-RD-DOMAIN17) (:DOMAIN ART-1D-RD-DOMAIN) 
	(:init (HF) (I-5) (I-1) (I-3) (HE) (I-6) (I-4) (I-8) (I-2) (I-7)) 
	(:GOAL (AND (G-8) (G-4) (G-7) (G-3)))
	(:length (:serial 4) (:parallel 4)))

(DEFINE (PROBLEM ART-1D-RD-DOMAIN18) (:DOMAIN ART-1D-RD-DOMAIN) 
	(:init (I-6) (I-1) (HE) (I-7) (I-4) (I-8) (HF) (I-5) (I-3) (I-2)) 
	(:GOAL (AND (G-8) (G-3) (G-1) (G-5)))
	 (:length (:serial 5) (:parallel 5))) 

(DEFINE (PROBLEM ART-1D-RD-DOMAIN19) (:DOMAIN ART-1D-RD-DOMAIN) 
	(:init (I-7) (I-4) (I-3) (I-1) (I-8) (I-5) (I-2) (HE) (I-6) (HF)) 
	(:GOAL (AND (G-7) (G-6) (G-5) (G-4)))
	(:length (:serial 4) (:parallel 4))) 

(DEFINE (PROBLEM ART-1D-RD-DOMAIN20) (:DOMAIN ART-1D-RD-DOMAIN) 
	(:init (I-7) (HF) (I-1) (I-3) (I-5) (I-8) (I-2) (I-6) (I-4) (HE)) 
	(:GOAL (AND (G-7) (G-3) (G-1) (G-6)))
	(:length (:serial 5) (:parallel 5))) 

