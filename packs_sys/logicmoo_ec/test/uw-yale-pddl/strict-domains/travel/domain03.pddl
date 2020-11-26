;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simpler domain - no vehicles, just roads

(define (domain roads)
  (:requirements :strips :equality)
  (:predicates (road ?from ?to)
	       (at ?thing ?location))
  (:action Run
	     :parameters (?person ?from ?to)
	     :precondition (and (road ?from ?to)
				 (at ?person ?from)
				 (not (= ?from ?to)))
	     :effect (and (at ?person ?to) (not (at ?person ?from)))))






