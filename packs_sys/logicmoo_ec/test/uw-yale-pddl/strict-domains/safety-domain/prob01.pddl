(define (problem safety-test1)
    (:domain safety-test1-domain)
  (:objects paper.tex ~/tex fig.ps)
  (:init (file paper.tex) (in.directory paper.tex ~/tex)
	 (file fig.ps) (in.directory fig.ps ~/tex))
  (:goal (and (not (in.directory paper.tex ~/tex))
	      (not (in.directory fig.ps ~/tex)))))




;;; UCPOP(36): (bf-control 'safety-test1)
;;; 
;;; Initial  : ((FILE PAPER.TEX) (IN.DIRECTORY PAPER.TEX ~/TEX) (FILE FIG.PS)
;;;             (IN.DIRECTORY FIG.PS ~/TEX))
;;; 
;;; Step 1  : (BACKUP FIG.PS)        Created 4 
;;;            0  -> (FILE FIG.PS)       
;;; Step 2  : (BACKUP PAPER.TEX)     Created 2 
;;;            0  -> (FILE PAPER.TEX)    
;;; Step 3  : (RM FIG.PS ~/TEX)      Created 3 
;;;            0  -> (IN.DIRECTORY FIG.PS ~/TEX)
;;;            0  -> (FILE FIG.PS)       
;;;            4  -> (WRITTEN-TO-TAPE FIG.PS)
;;; Step 4  : (RM PAPER.TEX ~/TEX)   Created 1 
;;;            0  -> (IN.DIRECTORY PAPER.TEX ~/TEX)
;;;            0  -> (FILE PAPER.TEX)    
;;;            2  -> (WRITTEN-TO-TAPE PAPER.TEX)
;;; 
;;; Goal    : (AND (NOT (IN.DIRECTORY PAPER.TEX ~/TEX))
;;;            (NOT (IN.DIRECTORY FIG.PS ~/TEX)))
;;;            3  -> (NOT (IN.DIRECTORY FIG.PS ~/TEX))
;;;            1  -> (NOT (IN.DIRECTORY PAPER.TEX ~/TEX))
;;; Facts:
;;; Complete!
;;; 
;;; UCPOP Stats: Initial terms = 4 ;   Goals = 3 ;  Success (4 steps)
;;;       Created 13 plans, but explored only 12
;;;       CPU time:    0.0100 sec
;;;       Branching factor:  1.000
;;;       Working Unifies: 37  
;;;       Bindings Added: 6   
;;; #plan<S=5; O=0; U=0; F=0>
;;; #Stats:<cpu time = 0.0100>
