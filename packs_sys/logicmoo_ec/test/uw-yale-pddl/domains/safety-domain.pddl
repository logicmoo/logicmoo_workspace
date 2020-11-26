; (c) 1993,1994,1995 Copyright (c) University of Washington
;  Written by Ying Sun.

;  All rights reserved. Use of this software is permitted for non-commercial
;  research purposes, and it may be copied only for that use.  All copies must
;  include this copyright message.  This software is made available AS IS, and
;  neither the authors nor the University of Washington make any warranty about
;  the software or its performance.

;;;
;;; Safety constraints tests
;;;

(define (domain safety-test1-domain)
    (:requirements :strips :safety-constraints :disjunctive-preconditions
		   :universal-preconditions)

  (:safety (forall (?f) (or (file ?f) (written-to-tape ?f))))

  (:predicates (file ?f)
	       (written-to-tape ?f)
	       (in.directory ?f ?d))

  (:action RM
	     :parameters (?f ?d)
	     :precondition (and (file ?f)
				 (in.directory ?f ?d))
	     :effect (and (not (file ?f))
			   (not (in.directory ?f ?d))))
      
  (:action BACKUP
	     :parameters (?f)
	     :precondition (file ?f)
	     :effect (written-to-tape ?f))
  )


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


(define (domain safety-test2-domain)
    (:requirements :strips :safety-constraints :disjunctive-preconditions
		   :universal-preconditions)

  (:safety (forall (?d) (or (data.encrypted ?d) (data.secure ?d))))

  (:predicates (data.encrypted ?d)
	       (data.secure ?d)
	       (data.sent ?d)
	       (data ?d))

  (:action SEND-DATA
      :parameters (?d)
      :precondition (data ?d)
      :effect (and (data.sent ?d)
		    (not (data.secure ?d))))
      
  (:action ENCRYPT
      :parameters (?d)
      :precondition (data ?d)
      :effect (data.encrypted ?d))
  )


(define (problem safety-test2)
    (:domain safety-test2-domain)
  (:objects email-addr credit-no)
  (:init (data email-addr)
	 (data credit-no)
	 (not (data.secure email-addr)))
  (:goal (and (data.sent email-addr)
	      (data.sent credit-no))))


;;; UCPOP(37): (bf-control 'safety-test2)
;;; 
;;; Initial  : ((DATA EMAIL-ADDR) (DATA CREDIT-NO) (NOT (DATA.SECURE EMAIL-ADDR)))
;;; 
;;; Step 1  : (ENCRYPT CREDIT-NO)    Created 3 
;;;            0  -> (DATA CREDIT-NO)    
;;; Step 2  : (SEND-DATA CREDIT-NO)   Created 2 
;;;            0  -> (DATA CREDIT-NO)    
;;;            3  -> (DATA.ENCRYPTED CREDIT-NO)
;;; Step 3  : (SEND-DATA EMAIL-ADDR)   Created 1 
;;;            0  -> (DATA EMAIL-ADDR)   
;;; 
;;; Goal    : (AND (DATA.SENT EMAIL-ADDR) (DATA.SENT CREDIT-NO))
;;;            2  -> (DATA.SENT CREDIT-NO)
;;;            1  -> (DATA.SENT EMAIL-ADDR)
;;; Facts:
;;; Complete!
;;; 
;;; UCPOP Stats: Initial terms = 3 ;   Goals = 3 ;  Success (3 steps)
;;;       Created 9 plans, but explored only 8
;;;       CPU time:    0.0000 sec
;;;       Branching factor:  1.000
;;;       Working Unifies: 13  
;;;       Bindings Added: 3   
;;; #plan<S=4; O=0; U=0; F=0>
;;; #Stats:<cpu time = 0.0000>
