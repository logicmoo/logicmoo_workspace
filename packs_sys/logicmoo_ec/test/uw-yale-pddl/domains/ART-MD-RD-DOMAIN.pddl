(DEFINE (DOMAIN ART-MD-RD-DOMAIN)
    (:requirements :adl)

  (:predicates (I-1) (I-2) (I-3) (I-4) (I-5) (I-6) (I-7) (I-8)
	       (G-1) (G-2) (G-3) (G-4) (G-5) (G-6) (G-7) (G-8)
	       (HE) (HF))
  
  
  (:ACTION ART-MD-RD--A-1 :PRECONDITION (AND (I-1) (HF)) :EFFECT (AND (G-1) (HE) (NOT (HF))))
  (:ACTION ART-MD-RD--A-2 :PRECONDITION (AND (I-2) (HE)) :EFFECT (AND (G-2) (HF) (NOT (HE)) 
								      (NOT (I-1))))
  (:ACTION ART-MD-RD--A-3 :PRECONDITION (AND (I-3) (HF)) :EFFECT
	   (AND (G-3) (HE) (NOT (HF)) (NOT (I-1)) (NOT (I-2))))
  (:ACTION ART-MD-RD--A-4 :PRECONDITION (AND (I-4) (HE)) :EFFECT
	   (AND (G-4) (HF) (NOT (HE)) (NOT (I-1)) (NOT (I-2)) (NOT (I-3))))
  (:ACTION ART-MD-RD--A-5 :PRECONDITION (AND (I-5) (HF)) :EFFECT
	   (AND (G-5) (HE) (NOT (HF)) (NOT (I-1)) (NOT (I-2)) (NOT (I-3)) (NOT (I-4))))
  (:ACTION ART-MD-RD--A-6 :PRECONDITION (AND (I-6) (HE)) :EFFECT
	   (AND (G-6) (HF) (NOT (HE)) (NOT (I-1)) (NOT (I-2)) (NOT (I-3)) (NOT (I-4)) (NOT (I-5))))
  (:ACTION ART-MD-RD--A-7 :PRECONDITION (AND (I-7) (HF)) :EFFECT
	   (AND (G-7) (HE) (NOT (HF)) (NOT (I-1)) (NOT (I-2)) (NOT (I-3)) (NOT (I-4)) (NOT (I-5)) 
		(NOT (I-6))))
  (:ACTION ART-MD-RD--A-8 :PRECONDITION (AND (I-8) (HE)) :EFFECT
	   (AND (G-8) (HF) (NOT (HE)) (NOT (I-1)) (NOT (I-2)) (NOT (I-3)) (NOT (I-4)) (NOT (I-5)) 
		(NOT (I-6)) (NOT (I-7)))))

(DEFINE (PROBLEM ART-MD-RD-DOMAIN1) (:DOMAIN ART-MD-RD-DOMAIN) 
	(:init (I-8) (I-2) (HF) (I-3) (I-5) (I-7) (I-6) (I-1) (HE) (I-4)) 
	(:GOAL (AND (G-4)))
	(:length (:serial 1) (:parallel 1)))

(DEFINE (PROBLEM ART-MD-RD-DOMAIN2) (:DOMAIN ART-MD-RD-DOMAIN) 
	(:init (I-5) (I-6) (I-8) (I-2) (I-4) (I-1) (HE) (I-3) (I-7) (HF)) 
	(:GOAL (AND (G-6)))
	(:length (:serial 1) (:parallel 1)))

(DEFINE (PROBLEM ART-MD-RD-DOMAIN3) (:DOMAIN ART-MD-RD-DOMAIN) 
	(:init (I-7) (HE) (I-2) (I-4) (I-1) (I-6) (I-5) (I-3) (I-8) (HF)) 
	(:GOAL (AND (G-3)))
	(:length (:serial 1) (:parallel 1)) )

(DEFINE (PROBLEM ART-MD-RD-DOMAIN4) (:DOMAIN ART-MD-RD-DOMAIN) 
	(:init (HF) (I-4) (I-1) (I-7) (I-5) (I-6) (I-3) (I-8) (HE) (I-2)) 
	(:GOAL (AND (G-7)))
	(:length (:serial 1) (:parallel 1)))

(DEFINE (PROBLEM ART-MD-RD-DOMAIN5) (:DOMAIN ART-MD-RD-DOMAIN) 
	(:init (HE) (I-7) (I-3) (I-1) (I-8) (HF) (I-4) (I-5) (I-2) (I-6)) 
	(:GOAL (AND (G-1)))
	(:length (:serial 1) (:parallel 1)))

(DEFINE (PROBLEM ART-MD-RD-DOMAIN6) (:DOMAIN ART-MD-RD-DOMAIN) 
	(:init (I-6) (HE) (I-2) (I-7) (HF) (I-3) (I-5) (I-1) (I-8) (I-4)) 
	(:GOAL (AND (G-6) (G-8)))
	(:length (:serial 3) (:parallel 3)))

(DEFINE (PROBLEM ART-MD-RD-DOMAIN7) (:DOMAIN ART-MD-RD-DOMAIN) 
	(:init (I-8) (HF) (I-5) (I-7) (I-6) (I-4) (I-3) (I-1) (I-2) (HE)) 
	(:GOAL (AND (G-3) (G-1)))
	(:length (:serial 3) (:parallel 3)))

(DEFINE (PROBLEM ART-MD-RD-DOMAIN8) (:DOMAIN ART-MD-RD-DOMAIN) 
	(:init (I-7) (I-6) (I-8) (HE) (I-3) (HF) (I-2) (I-1) (I-5) (I-4)) 
	(:GOAL (AND (G-7) (G-8)))
	(:length (:serial 2) (:parallel 2)))

(DEFINE (PROBLEM ART-MD-RD-DOMAIN9) (:DOMAIN ART-MD-RD-DOMAIN) 
	(:init (I-5) (I-2) (I-8) (HF) (I-6) (I-4) (I-7) (HE) (I-3) (I-1)) 
	(:GOAL (AND (G-8) (G-2)))
	(:length (:serial 3) (:parallel 3)))

(DEFINE (PROBLEM ART-MD-RD-DOMAIN10) (:DOMAIN ART-MD-RD-DOMAIN) 
	(:init (I-3) (I-6) (I-8) (I-4) (I-1) (HF) (I-7) (I-2) (HE) (I-5)) 
	(:GOAL (AND (G-7) (G-4)))
	(:length (:serial 2) (:parallel 2)))

(DEFINE (PROBLEM ART-MD-RD-DOMAIN11) (:DOMAIN ART-MD-RD-DOMAIN) 
	(:init (I-1) (I-3) (I-7) (I-2) (I-5) (I-8) (I-6) (I-4) (HE) (HF)) 
	(:GOAL (AND (G-5) (G-8) (G-4)))
	(:length (:serial 3) (:parallel 3)))

(DEFINE (PROBLEM ART-MD-RD-DOMAIN12) (:DOMAIN ART-MD-RD-DOMAIN) 
	(:init (I-1) (I-5) (HE) (I-4) (I-2) (I-8) (I-7) (HF) (I-3) (I-6)) 
	(:GOAL (AND (G-8) (G-1) (G-3)))
	(:length (:serial 4) (:parallel 4)))

(DEFINE (PROBLEM ART-MD-RD-DOMAIN13) (:DOMAIN ART-MD-RD-DOMAIN) 
	(:init (I-7) (HF) (I-6) (I-8) (I-3) (I-5) (I-4) (HE) (I-1) (I-2)) 
	(:GOAL (AND (G-3) (G-5) (G-8)))
	(:length (:serial 4) (:parallel 4)))

(DEFINE (PROBLEM ART-MD-RD-DOMAIN14) (:DOMAIN ART-MD-RD-DOMAIN) 
	(:init (I-3) (HF) (I-1) (I-5) (I-4) (I-8) (I-6) (I-7) (I-2) (HE)) 
	(:GOAL (AND (G-2) (G-4) (G-7)))
	(:length (:serial 4) (:parallel 4)))

(DEFINE (PROBLEM ART-MD-RD-DOMAIN15) (:DOMAIN ART-MD-RD-DOMAIN) 
	(:init (I-4) (HE) (I-6) (I-1) (HF) (I-8) (I-3) (I-5) (I-2) (I-7)) 
	(:GOAL (AND (G-4) (G-8) (G-6)))
	(:length (:serial 5) (:parallel 5)))

(DEFINE (PROBLEM ART-MD-RD-DOMAIN16) (:DOMAIN ART-MD-RD-DOMAIN) 
	(:init (I-8) (I-6) (HE) (HF) (I-2) (I-1) (I-7) (I-3) (I-4) (I-5)) 
	(:GOAL (AND (G-3) (G-2) (G-6) (G-4)))
	(:length (:serial 5) (:parallel 5)))

(DEFINE (PROBLEM ART-MD-RD-DOMAIN17) (:DOMAIN ART-MD-RD-DOMAIN) 
	(:init (I-7) (I-5) (HE) (I-2) (I-8) (I-3) (I-6) (HF) (I-1) (I-4)) 
	(:GOAL (AND (G-5) (G-4) (G-3) (G-2)))
	(:length (:serial 4) (:parallel 4)))

(DEFINE (PROBLEM ART-MD-RD-DOMAIN18) (:DOMAIN ART-MD-RD-DOMAIN) 
	(:init (HF) (I-6) (HE) (I-4) (I-7) (I-1) (I-8) (I-2) (I-3) (I-5)) 
	(:GOAL (AND (G-1) (G-3) (G-8) (G-6)))
	(:length (:serial 6) (:parallel 6)))

(DEFINE (PROBLEM ART-MD-RD-DOMAIN19) (:DOMAIN ART-MD-RD-DOMAIN) 
	(:init (I-7) (I-2) (I-1) (I-8) (HF) (I-4) (I-3) (HE) (I-5) (I-6)) 
	(:GOAL (AND (G-2) (G-1) (G-4) (G-6)))
	(:length (:serial 6) (:parallel 6)))

(DEFINE (PROBLEM ART-MD-RD-DOMAIN20) (:DOMAIN ART-MD-RD-DOMAIN) 
	(:init (I-7) (I-4) (I-2) (I-1) (I-3) (HF) (I-6) (I-5) (I-8) (HE)) 
	(:GOAL (AND (G-7) (G-5) (G-1) (G-4)))
	(:length (:serial 5) (:parallel 5)))




