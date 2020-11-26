;;NEW
;; (blast-instances #$BlastedThing)
(define blast-instances (c)
  (csetq *rt* (foc "RetainedThing"))
  (csetq *hit-list* (with-all-mts (all-instances c)))
  (cdolist (ivar *hit-list*)
    (punless  (with-all-mts (isa? ivar *rt*))
    (print ivar)
    
    (pwhen (fort-p ivar)      
      (pcond 
         ((cand (NART-P ivar) (askm `(#$resultQuotedIsa ,(car (NART-EL-FORMULA ivar)) #$TheTerm))))
         (t (cyc-kill ivar) (print (assertion-count))))))))

(define kill-empty-mts ()
 (cdolist (mt (all-instances #$Microtheory))
   ))
 

;; (blast-instances #$City)
;; (blast-instances #$Person)
;; (blast-instances #$Action)
;; (blast-instances #$Country)
;; (blast-instances #$MusicPerformanceAgent)
;; (blast-instances #$Event)
;; (blast-instances #$Organization)
