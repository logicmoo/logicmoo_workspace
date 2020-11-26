(define (problem rat-insulin-adl)
    (:domain molgen-adl)
  (:objects insulin-gene e-coli-exosome junk-exosome
	    e-coli junk antibiotic-1)
  (:init (molecule insulin-gene)
	 (molecule e-coli-exosome)
	 (molecule junk-exosome) (molecule linker)
	 (bacterium e-coli) (bacterium junk)
	 (antibiotic antibiotic-1)
	 (mRNA insulin-gene)
	 (cleavable e-coli-exosome)
	 (cleavable junk-exosome)
	 (accepts junk-exosome junk)
	 (accepts e-coli-exosome e-coli)
	 (resists antibiotic-1 e-coli-exosome))
  (:goal (AND (exists (?y)
		      (and (bacterium ?y) 
			   (exists (?x)
				   (and (molecule ?x)
					(contains insulin-gene ?x)
					(contains ?x ?y)
					(pure ?y))))))))




;;;UCPOP(30): (bf-control 'rat-insulin)
;;;
;;;Initial  : ((MOLECULE INSULIN-GENE) (MOLECULE E-COLI-EXOSOME)
;;;            (MOLECULE JUNK-EXOSOME) (MOLECULE LINKER) (BACTERIUM E-COLI)
;;;            (BACTERIUM JUNK) (ANTIBIOTIC ANTIBIOTIC-1) (MRNA INSULIN-GENE)
;;;            (CLEAVABLE E-COLI-EXOSOME) (CLEAVABLE JUNK-EXOSOME)
;;;            (ACCEPTS JUNK-EXOSOME JUNK) (ACCEPTS E-COLI-EXOSOME E-COLI)
;;;            (RESISTS ANTIBIOTIC-1 E-COLI-EXOSOME))
;;;
;;;Step 1  : (REVERSE-TRANSCRIBE INSULIN-GENE)   Created 10
;;;           0  -> (MRNA INSULIN-GENE)
;;;Step 2  : (SEPARATE INSULIN-GENE)   Created 9
;;;           10 -> (CONNECTED-CDNA-MRNA INSULIN-GENE)
;;;Step 3  : (POLYMERIZE INSULIN-GENE)   Created 8
;;;           9  -> (SINGLE-STRAND INSULIN-GENE)
;;;Step 4  : (DIGEST INSULIN-GENE)   Created 7
;;;           8  -> (HAIR-PIN INSULIN-GENE)
;;;Step 5  : (LIGATE LINKER INSULIN-GENE)   Created 6
;;;           7  -> (DOUBLE-STRAND INSULIN-GENE)
;;;Step 6  : (CLEAVE INSULIN-GENE)   Created 5
;;;           6  -> (CLEAVABLE INSULIN-GENE)
;;;Step 7  : (CLEAVE E-COLI-EXOSOME)   Created 4
;;;           0  -> (CLEAVABLE E-COLI-EXOSOME)
;;;Step 8  : (LIGATE INSULIN-GENE E-COLI-EXOSOME)   Created 3
;;;           5  -> (CLEAVED INSULIN-GENE)
;;;           4  -> (CLEAVED E-COLI-EXOSOME)
;;;Step 9  : (TRANSFORM E-COLI-EXOSOME E-COLI)   Created 2
;;;           3  -> (CLEAVABLE E-COLI-EXOSOME)
;;;           0  -> (ACCEPTS E-COLI-EXOSOME E-COLI)
;;;           0  -> (BACTERIUM E-COLI)
;;;Step 10 : (SCREEN E-COLI E-COLI-EXOSOME ANTIBIOTIC-1)   Created 1
;;;           0  -> (RESISTS ANTIBIOTIC-1 E-COLI-EXOSOME)
;;;           2  -> (CONTAINS E-COLI-EXOSOME E-COLI)
;;;           0  -> (BACTERIUM E-COLI)
;;;           0  -> (ANTIBIOTIC ANTIBIOTIC-1)
;;;
;;;Goal    : (EXISTS ((BACTERIUM ?YGOAL) (MOLECULE ?XGOAL))
;;;           (AND (CONTAINS INSULIN-GENE ?XGOAL) (CONTAINS ?XGOAL ?YGOAL)
;;;            (PURE ?YGOAL)))
;;;           3  -> (CONTAINS INSULIN-GENE E-COLI-EXOSOME)
;;;           2  -> (CONTAINS E-COLI-EXOSOME E-COLI)
;;;           1  -> (PURE E-COLI)
;;;           0  -> (BACTERIUM E-COLI)
;;;           0  -> (MOLECULE E-COLI-EXOSOME)
;;;Complete!
;;;
;;;UCPOP (Init = 13 ; Goals = 3 ) => Win  (10 steps)     CPU 6850
;;;     Nodes (V = 896 ; Q = 203 ; C = 1255)             Branch 1.2265625
;;;     Working Unifies: 3176                            Bindings added: 1600
;;;NIL

