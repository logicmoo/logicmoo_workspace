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
