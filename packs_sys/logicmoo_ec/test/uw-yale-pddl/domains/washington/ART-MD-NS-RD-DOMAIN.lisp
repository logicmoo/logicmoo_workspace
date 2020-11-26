
(IN-PACKAGE "DOMAINS") 
(DEFINE (DOMAIN ART-MD-NS-RD-DOMAIN)
    (:requirements :adl)
  
  (:predicates (P-1) (P-2) (P-3) (P-4) (P-5) (P-6) (P-7) (P-8)
	       (G-1) (G-2) (G-3) (G-4) (G-5) (G-6) (G-7) (G-8)
	       (HE) (HF))

  (:ACTION ART-MD-NS-RD--1A-1 :PRECONDITION (AND (P-1) (HF)) :EFFECT 
	   (AND (G-1) (HE) (NOT (HF))))
  (:ACTION ART-MD-NS-RD--2A-1 :PRECONDITION (AND (P-1) (HF)) :EFFECT 
	   (AND (G-1) (HE) (NOT (HF))))
  (:ACTION ART-MD-NS-RD--1A-2 :PRECONDITION (AND (P-2) (HE)) :EFFECT
	   (AND (G-2) (HF) (NOT (HE)) (NOT (P-1))))
  (:ACTION ART-MD-NS-RD--2A-2 :PRECONDITION (AND (P-2) (HE)) :EFFECT
	   (AND (G-2) (HF) (NOT (HE)) (NOT (P-1))))
  (:ACTION ART-MD-NS-RD--1A-3 :PRECONDITION (AND (P-3) (HF)) :EFFECT
	   (AND (G-3) (HE) (NOT (HF)) (NOT (P-1)) (NOT (P-2))))
  (:ACTION ART-MD-NS-RD--2A-3 :PRECONDITION (AND (P-3) (HF)) :EFFECT
	   (AND (G-3) (HE) (NOT (HF)) (NOT (P-1)) (NOT (P-2))))
  (:ACTION ART-MD-NS-RD--1A-4 :PRECONDITION (AND (P-4) (HE)) :EFFECT
	   (AND (G-4) (HF) (NOT (HE)) (NOT (P-1)) (NOT (P-2)) (NOT (P-3))))
  (:ACTION ART-MD-NS-RD--2A-4 :PRECONDITION (AND (P-4) (HE)) :EFFECT
	   (AND (G-4) (HF) (NOT (HE)) (NOT (P-1)) (NOT (P-2)) (NOT (P-3))))
  (:ACTION ART-MD-NS-RD--1A-5 :PRECONDITION (AND (P-5) (HF)) :EFFECT
	   (AND (G-5) (HE) (NOT (HF)) (NOT (P-1)) (NOT (P-2)) (NOT (P-3)) (NOT (P-4))))
  (:ACTION ART-MD-NS-RD--2A-5 :PRECONDITION (AND (P-5) (HF)) :EFFECT
	   (AND (G-5) (HE) (NOT (HF)) (NOT (P-1)) (NOT (P-2)) (NOT (P-3)) (NOT (P-4))))
  (:ACTION ART-MD-NS-RD--1A-6 :PRECONDITION (AND (P-6) (HE)) :EFFECT
	   (AND (G-6) (HF) (NOT (HE)) (NOT (P-1)) (NOT (P-2)) (NOT (P-3)) (NOT (P-4)) 
		(NOT (P-5))))
  (:ACTION ART-MD-NS-RD--2A-6 :PRECONDITION (AND (P-6) (HE)) :EFFECT
	   (AND (G-6) (HF) (NOT (HE)) (NOT (P-1)) (NOT (P-2)) (NOT (P-3)) (NOT (P-4)) 
		(NOT (P-5))))
  (:ACTION ART-MD-NS-RD--1A-7 :PRECONDITION (AND (P-7) (HF)) :EFFECT
	   (AND (G-7) (HE) (NOT (HF)) (NOT (P-1)) (NOT (P-2)) (NOT (P-3)) (NOT (P-4)) 
		(NOT (P-5)) (NOT (P-6))))
  (:ACTION ART-MD-NS-RD--2A-7 :PRECONDITION (AND (P-7) (HF)) :EFFECT
	   (AND (G-7) (HE) (NOT (HF)) (NOT (P-1)) (NOT (P-2)) (NOT (P-3)) (NOT (P-4)) 
		(NOT (P-5)) (NOT (P-6))))
  (:ACTION ART-MD-NS-RD--1A-8 :PRECONDITION (AND (P-8) (HE)) :EFFECT
	   (AND (G-8) (HF) (NOT (HE)) (NOT (P-1)) (NOT (P-2)) (NOT (P-3)) (NOT (P-4)) 
		(NOT (P-5)) (NOT (P-6)) (NOT (P-7))))
  (:ACTION ART-MD-NS-RD--2A-8 :PRECONDITION (AND (P-8) (HE)) :EFFECT
	   (AND (G-8) (HF) (NOT (HE)) (NOT (P-1)) (NOT (P-2)) (NOT (P-3)) (NOT (P-4)) 
		(NOT (P-5)) (NOT (P-6)) (NOT (P-7))))) 



(DEFINE (PROBLEM ART-MD-NS-RD-DOMAIN1) (:DOMAIN ART-MD-NS-RD-DOMAIN) 
	(:init (HF) (P-7) (P-8) (P-3) (P-4) (P-2) (P-6) (P-1) (P-5) (HE)) 
	(:GOAL (AND (G-1)))
	(:length (:serial 1) (:parallel 1)))

(DEFINE (PROBLEM ART-MD-NS-RD-DOMAIN2) (:DOMAIN ART-MD-NS-RD-DOMAIN) 
	(:init (P-4) (P-6) (P-3) (HE) (P-7) (P-8) (HF) (P-5) (P-2) (P-1)) 
	(:GOAL (AND (G-1)))
	(:length (:serial 1) (:parallel 1)))

(DEFINE (PROBLEM ART-MD-NS-RD-DOMAIN3) (:DOMAIN ART-MD-NS-RD-DOMAIN) 
	(:init (P-5) (HF) (P-6) (P-8) (P-1) (HE) (P-3) (P-2) (P-7) (P-4)) 
	(:GOAL (AND (G-7)))
	(:length (:serial 1) (:parallel 1)))

(DEFINE (PROBLEM ART-MD-NS-RD-DOMAIN4) (:DOMAIN ART-MD-NS-RD-DOMAIN) 
	(:init (P-1) (P-4) (P-3) (P-7) (HE) (P-5) (P-8) (HF) (P-6) (P-2)) 
	(:GOAL (AND (G-8)))
	(:length (:serial 1) (:parallel 1)))

(DEFINE (PROBLEM ART-MD-NS-RD-DOMAIN5) (:DOMAIN ART-MD-NS-RD-DOMAIN) 
	(:init (P-2) (P-4) (HE) (P-3) (P-8) (HF) (P-1) (P-7) (P-5) (P-6)) 
	(:GOAL (AND (G-4)))
	(:length (:serial 1) (:parallel 1))) 

(DEFINE (PROBLEM ART-MD-NS-RD-DOMAIN6) (:DOMAIN ART-MD-NS-RD-DOMAIN) 
	(:init (P-6) (P-1) (P-2) (HE) (HF) (P-3) (P-8) (P-5) (P-7) (P-4)) 
	(:GOAL (AND (G-2) (G-4)))
	(:length (:serial 3) (:parallel 3))) 

(DEFINE (PROBLEM ART-MD-NS-RD-DOMAIN7) (:DOMAIN ART-MD-NS-RD-DOMAIN) 
	(:init (P-5) (HE) (HF) (P-7) (P-6) (P-2) (P-1) (P-8) (P-3) (P-4)) 
	(:GOAL (AND (G-2) (G-6)))
	(:length (:serial 3) (:parallel 3)))

(DEFINE (PROBLEM ART-MD-NS-RD-DOMAIN8) (:DOMAIN ART-MD-NS-RD-DOMAIN) 
	(:init (P-7) (P-3) (HF) (P-6) (P-8) (P-2) (HE) (P-5) (P-1) (P-4)) 
	(:GOAL (AND (G-3) (G-6)))
	(:length (:serial 2) (:parallel 2)))

(DEFINE (PROBLEM ART-MD-NS-RD-DOMAIN9) (:DOMAIN ART-MD-NS-RD-DOMAIN) 
	(:init (P-6) (HF) (P-5) (P-2) (P-4) (P-8) (P-3) (HE) (P-1) (P-7)) 
	(:GOAL (AND (G-2) (G-1)))
	(:length (:serial 2) (:parallel 2)))

(DEFINE (PROBLEM ART-MD-NS-RD-DOMAIN10) (:DOMAIN ART-MD-NS-RD-DOMAIN) 
	(:init (P-3) (P-6) (P-5) (HF) (P-8) (P-1) (P-2) (P-7) (P-4) (HE)) 
	(:GOAL (AND (G-8) (G-3)))
	(:length (:serial 2) (:parallel 2)))

(DEFINE (PROBLEM ART-MD-NS-RD-DOMAIN11) (:DOMAIN ART-MD-NS-RD-DOMAIN)
	(:init (P-7) (P-5) (P-3) (P-4) (P-2) (HF) (P-8) (P-1) (P-6) (HE)) 
	(:GOAL (AND (G-3) (G-1) (G-2)))
	(:length (:serial 3) (:parallel 3))) 

(DEFINE (PROBLEM ART-MD-NS-RD-DOMAIN12) (:DOMAIN ART-MD-NS-RD-DOMAIN) 
	(:init (P-4) (P-6) (HE) (P-1) (P-5) (P-2) (P-7) (P-8) (HF) (P-3)) 
	(:GOAL (AND (G-2) (G-1) (G-8)))
	(:length (:serial 4) (:parallel 4))) 

(DEFINE (PROBLEM ART-MD-NS-RD-DOMAIN13) (:DOMAIN ART-MD-NS-RD-DOMAIN) 
	(:init (HF) (P-2) (P-8) (P-3) (P-1) (P-4) (HE) (P-7) (P-6) (P-5)) 
	(:GOAL (AND (G-2) (G-4) (G-1)))
	(:length (:serial 4) (:parallel 4)))

(DEFINE (PROBLEM ART-MD-NS-RD-DOMAIN14) (:DOMAIN ART-MD-NS-RD-DOMAIN) 
	(:init (P-2) (P-5) (HF) (P-1) (P-7) (HE) (P-3) (P-6) (P-4) (P-8)) 
	(:GOAL (AND (G-5) (G-6) (G-4)))
	(:length (:serial 3) (:parallel 3))) 

(DEFINE (PROBLEM ART-MD-NS-RD-DOMAIN15) (:DOMAIN ART-MD-NS-RD-DOMAIN) 
	(:init (HF) (P-5) (P-4) (P-3) (P-8) (P-1) (P-6) (P-2) (HE) (P-7)) 
	(:GOAL (AND (G-2) (G-1) (G-7)))
	(:length (:serial 3) (:parallel 3))) 

(DEFINE (PROBLEM ART-MD-NS-RD-DOMAIN16) (:DOMAIN ART-MD-NS-RD-DOMAIN) 
	(:init (P-6) (P-5) (HE) (P-2) (HF) (P-7) (P-1) (P-4) (P-3) (P-8)) 
	(:GOAL (AND (G-6) (G-8) (G-1) (G-7)))
	(:length (:serial 4) (:parallel 4)))

(DEFINE (PROBLEM ART-MD-NS-RD-DOMAIN17) (:DOMAIN ART-MD-NS-RD-DOMAIN) 
	(:init (P-1) (P-6) (P-8) (P-2) (HF) (P-4) (P-5) (HE) (P-7) (P-3)) 
	(:GOAL (AND (G-1) (G-8) (G-5) (G-6)))
	(:length (:serial 6) (:parallel 6))) 

(DEFINE (PROBLEM ART-MD-NS-RD-DOMAIN18) (:DOMAIN ART-MD-NS-RD-DOMAIN) 
	(:init (P-8) (P-6) (P-7) (P-4) (P-5) (P-1) (P-3) (HF) (P-2) (HE)) 
	(:GOAL (AND (G-7) (G-6) (G-4) (G-5)))
	(:length (:serial 4) (:parallel 4))) 

(DEFINE (PROBLEM ART-MD-NS-RD-DOMAIN19) (:DOMAIN ART-MD-NS-RD-DOMAIN) 
	(:init (HF) (P-7) (P-5) (P-2) (P-6) (P-1) (HE) (P-8) (P-3) (P-4)) 
	(:GOAL (AND (G-2) (G-5) (G-4) (G-3)))
	(:length (:serial 4) (:parallel 4))) 

(DEFINE (PROBLEM ART-MD-NS-RD-DOMAIN20) (:DOMAIN ART-MD-NS-RD-DOMAIN) 
	(:init (P-8) (P-2) (HF) (HE) (P-7) (P-3) (P-6) (P-4) (P-1) (P-5)) 
	(:GOAL (AND (G-2) (G-7) (G-4) (G-5)))
	(:length (:serial 6) (:parallel 6))) 




























