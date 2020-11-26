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
