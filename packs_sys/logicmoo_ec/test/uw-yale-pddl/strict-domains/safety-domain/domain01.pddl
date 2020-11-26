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





