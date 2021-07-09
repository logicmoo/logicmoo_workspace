(define (domain ART-MD-NS-DOMAIN)
    (:requirements :adl)
  (:predicates (P-1) (P-2) (P-3) (P-4) (P-5) (P-6) (P-7) (P-8)
	       (G-1) (G-2) (G-3) (G-4) (G-5) (G-6) (G-7) (G-8))
  (:ACTION ART-MD-NS--1A-1 :PRECONDITION (AND (P-1)) :EFFECT (AND (G-1)))
  (:ACTION ART-MD-NS--2A-1 :PRECONDITION (AND (P-1)) :EFFECT (AND (G-1)))
  (:ACTION ART-MD-NS--1A-2 :PRECONDITION (AND (P-2)) :EFFECT (AND (G-2) (NOT (P-1))))
  (:ACTION ART-MD-NS--2A-2 :PRECONDITION (AND (P-2)) :EFFECT (AND (G-2) (NOT (P-1))))
  (:ACTION ART-MD-NS--1A-3 :PRECONDITION (AND (P-3)) :EFFECT (AND (G-3) (NOT (P-1)) 
								  (NOT (P-2))))
  (:ACTION ART-MD-NS--2A-3 :PRECONDITION (AND (P-3)) :EFFECT (AND (G-3) (NOT (P-1)) 
								  (NOT (P-2))))
  (:ACTION ART-MD-NS--1A-4 :PRECONDITION (AND (P-4)) :EFFECT
	   (AND (G-4) (NOT (P-1)) (NOT (P-2)) (NOT (P-3))))
  (:ACTION ART-MD-NS--2A-4 :PRECONDITION (AND (P-4)) :EFFECT
	   (AND (G-4) (NOT (P-1)) (NOT (P-2)) (NOT (P-3))))
  (:ACTION ART-MD-NS--1A-5 :PRECONDITION (AND (P-5)) :EFFECT
	   (AND (G-5) (NOT (P-1)) (NOT (P-2)) (NOT (P-3)) (NOT (P-4))))
  (:ACTION ART-MD-NS--2A-5 :PRECONDITION (AND (P-5)) :EFFECT
	   (AND (G-5) (NOT (P-1)) (NOT (P-2)) (NOT (P-3)) (NOT (P-4))))
  (:ACTION ART-MD-NS--1A-6 :PRECONDITION (AND (P-6)) :EFFECT
	   (AND (G-6) (NOT (P-1)) (NOT (P-2)) (NOT (P-3)) (NOT (P-4)) (NOT (P-5))))
  (:ACTION ART-MD-NS--2A-6 :PRECONDITION (AND (P-6)) :EFFECT
	   (AND (G-6) (NOT (P-1)) (NOT (P-2)) (NOT (P-3)) (NOT (P-4)) (NOT (P-5))))
  (:ACTION ART-MD-NS--1A-7 :PRECONDITION (AND (P-7)) :EFFECT
	   (AND (G-7) (NOT (P-1)) (NOT (P-2)) (NOT (P-3)) (NOT (P-4)) (NOT (P-5)) 
		(NOT (P-6))))
  (:ACTION ART-MD-NS--2A-7 :PRECONDITION (AND (P-7)) :EFFECT
	   (AND (G-7) (NOT (P-1)) (NOT (P-2)) (NOT (P-3)) (NOT (P-4)) (NOT (P-5)) 
		(NOT (P-6))))
  (:ACTION ART-MD-NS--1A-8 :PRECONDITION (AND (P-8)) :EFFECT
	   (AND (G-8) (NOT (P-1)) (NOT (P-2)) (NOT (P-3)) (NOT (P-4)) (NOT (P-5)) 
		(NOT (P-6)) (NOT (P-7))))
  (:ACTION ART-MD-NS--2A-8 :PRECONDITION (AND (P-8)) :EFFECT
	   (AND (G-8) (NOT (P-1)) (NOT (P-2)) (NOT (P-3)) (NOT (P-4)) (NOT (P-5)) 
		(NOT (P-6)) (NOT (P-7))))) 



(DEFINE (PROBLEM ART-MD-NS-DOMAIN1) (:DOMAIN ART-MD-NS-DOMAIN) 
	(:init (P-5) (P-4) (P-6) (P-7) (P-1) (P-8) (P-2) (P-3))
	(:GOAL (AND (G-1)))
	(:length (:serial 1) (:parallel 1)))
  
(DEFINE (PROBLEM ART-MD-NS-DOMAIN2) (:DOMAIN ART-MD-NS-DOMAIN) 
	(:init (P-2) (P-3) (P-1) (P-6) (P-4) (P-7) (P-5) (P-8))
	(:GOAL (AND (G-8)))
	(:length (:serial 1) (:parallel 1))) 

(DEFINE (PROBLEM ART-MD-NS-DOMAIN3) (:DOMAIN ART-MD-NS-DOMAIN) 
	(:init (P-4) (P-3) (P-8) (P-1) (P-6) (P-5) (P-7) (P-2))
	(:GOAL (AND (G-4)))
	(:length (:serial 1) (:parallel 1)))
 
(DEFINE (PROBLEM ART-MD-NS-DOMAIN4) (:DOMAIN ART-MD-NS-DOMAIN) 
	(:init (P-2) (P-8) (P-5) (P-1) (P-3) (P-7) (P-6) (P-4))
	(:GOAL (AND (G-8)))
	(:length (:serial 1) (:parallel 1))) 
 
(DEFINE (PROBLEM ART-MD-NS-DOMAIN5) (:DOMAIN ART-MD-NS-DOMAIN) 
	(:init (P-4) (P-6) (P-2) (P-3) (P-7) (P-8) (P-1) (P-5))
	(:GOAL (AND (G-7)))
	(:length (:serial 1) (:parallel 1))) 
 
(DEFINE (PROBLEM ART-MD-NS-DOMAIN6) (:DOMAIN ART-MD-NS-DOMAIN) 
	(:init (P-2) (P-8) (P-6) (P-1) (P-5) (P-7) (P-4) (P-3))
	(:GOAL (AND (G-2) (G-4)))
	(:length (:serial 2) (:parallel 2))) 
 
(DEFINE (PROBLEM ART-MD-NS-DOMAIN7) (:DOMAIN ART-MD-NS-DOMAIN) 
	(:init (P-8) (P-6) (P-3) (P-5) (P-7) (P-2) (P-1) (P-4))
	(:GOAL (AND (G-8) (G-4)))
	(:length (:serial 2) (:parallel 2))) 
 
(DEFINE (PROBLEM ART-MD-NS-DOMAIN8) (:DOMAIN ART-MD-NS-DOMAIN) 
	(:init (P-7) (P-4) (P-5) (P-2) (P-6) (P-8) (P-1) (P-3))
	(:GOAL (AND (G-3) (G-5)))
	(:length (:serial 2) (:parallel 2))) 

(DEFINE (PROBLEM ART-MD-NS-DOMAIN9) (:DOMAIN ART-MD-NS-DOMAIN) 
	(:init (P-2) (P-8) (P-1) (P-3) (P-5) (P-6) (P-7) (P-4)) 
	(:GOAL (AND (G-7) (G-2)))
	(:length (:serial 2) (:parallel 2)))

(DEFINE (PROBLEM ART-MD-NS-DOMAIN10) (:DOMAIN ART-MD-NS-DOMAIN) 
	(:init (P-3) (P-8) (P-5) (P-7) (P-1) (P-6) (P-2) (P-4))
	(:GOAL (AND (G-2) (G-3)))
	(:length (:serial 2) (:parallel 2)))

(DEFINE (PROBLEM ART-MD-NS-DOMAIN11) (:DOMAIN ART-MD-NS-DOMAIN) 
	(:init (P-1) (P-3) (P-8) (P-7) (P-2) (P-4) (P-5) (P-6))
	(:GOAL (AND (G-3) (G-2) (G-1)))
	(:length (:serial 3) (:parallel 3))) 
 
(DEFINE (PROBLEM ART-MD-NS-DOMAIN12) (:DOMAIN ART-MD-NS-DOMAIN) 
	(:init (P-1) (P-4) (P-8) (P-2) (P-3) (P-6) (P-7) (P-5))
	(:GOAL (AND (G-2) (G-4) (G-8)))
	(:length (:serial 3) (:parallel 3))) 
 
(DEFINE (PROBLEM ART-MD-NS-DOMAIN13) (:DOMAIN ART-MD-NS-DOMAIN) 
	(:init (P-6) (P-7) (P-3) (P-5) (P-2) (P-8) (P-1) (P-4))
	(:GOAL (AND (G-8) (G-2) (G-3)))
	(:length (:serial 3) (:parallel 3))) 
 
(DEFINE (PROBLEM ART-MD-NS-DOMAIN14) (:DOMAIN ART-MD-NS-DOMAIN) 
	(:init (P-8) (P-6) (P-5) (P-7) (P-1) (P-4) (P-3) (P-2))
	(:GOAL (AND (G-4) (G-6) (G-5)))
	(:length (:serial 3) (:parallel 3))) 
 
(DEFINE (PROBLEM ART-MD-NS-DOMAIN15) (:DOMAIN ART-MD-NS-DOMAIN) 
	(:init (P-6) (P-3) (P-8) (P-2) (P-5) (P-7) (P-4) (P-1))
	(:GOAL (AND (G-6) (G-2) (G-4)))
	(:length (:serial 3) (:parallel 3)))
 
(DEFINE (PROBLEM ART-MD-NS-DOMAIN16) (:DOMAIN ART-MD-NS-DOMAIN) 
	(:init (P-5) (P-3) (P-1) (P-6) (P-4) (P-8) (P-2) (P-7))
	(:GOAL (AND (G-7) (G-6) (G-4) (G-3)))
	(:length (:serial 4) (:parallel 4)))
 
(DEFINE (PROBLEM ART-MD-NS-DOMAIN17) (:DOMAIN ART-MD-NS-DOMAIN) 
	(:init (P-2) (P-4) (P-1) (P-8) (P-7) (P-5) (P-3) (P-6))
	(:GOAL (AND (G-2) (G-5) (G-6) (G-4)))
	(:length (:serial 4) (:parallel 4)))
 
(DEFINE (PROBLEM ART-MD-NS-DOMAIN18) (:DOMAIN ART-MD-NS-DOMAIN) 
	(:init (P-3) (P-2) (P-5) (P-6) (P-1) (P-7) (P-8) (P-4))
	(:GOAL (AND (G-2) (G-1) (G-6) (G-4)))
	(:length (:serial 4) (:parallel 4)))
 
(DEFINE (PROBLEM ART-MD-NS-DOMAIN19) (:DOMAIN ART-MD-NS-DOMAIN) 
	(:init (P-3) (P-2) (P-6) (P-1) (P-4) (P-7) (P-8) (P-5))
	(:GOAL (AND (G-1) (G-3) (G-7) (G-8)))
	(:length (:serial 4) (:parallel 4))) 
 
(DEFINE (PROBLEM ART-MD-NS-DOMAIN20) (:DOMAIN ART-MD-NS-DOMAIN) 
	(:init (P-8) (P-2) (P-4) (P-7) (P-6) (P-1) (P-5) (P-3))
	(:GOAL (AND (G-6) (G-1) (G-2) (G-3)))
	(:length (:serial 4) (:parallel 4))) 
 
