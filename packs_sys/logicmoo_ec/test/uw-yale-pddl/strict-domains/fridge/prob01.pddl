(define (problem fixa)
    (:domain fridge-domain-typed)
  (:objects s1 s2 s3 s4 - screw
	    b1 - backplane
	    c1 c2 - compressor
	    f1)
  (:init (fridge f1)
	 (covers b1 c1) (part-of b1 f1)
	 (holds s1 b1)  (holds s2 b1)  (holds s3 b1)
	 (holds s4 b1)
	 (ok c1) (ok c2) (fridge-on f1)
	 (screwed s1) (screwed s2) (screwed s3) (screwed s4)
	 (in-place b1) (attached c1))
  (:goal (and (attached c2) (ok c2)))
  (:length (:serial 7 ) (:parallel 3)))