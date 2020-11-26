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


