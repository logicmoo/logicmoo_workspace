(define (domain art)
    (:predicates (g) (p1) (p2) (p3) (p4) (p5) (p6)
		 (q1) (q2) (q3) (q4) (q5) (q6))

    (:action G0
	       :parameters ()
	       :precondition (q1)
	       :effect (g))
    (:action H0
	       :parameters ()
	       :precondition (p1)
	       :effect (g))
    (:action A1
	       :parameters ()
	       :precondition (p2)
	       :effect (p1))
    (:action B1
	       :parameters ()
	       :precondition (p2)
	       :effect (p1))
    (:action A2
	       :parameters ()
	       :precondition (p3)
	       :effect (p2))
    (:action B2
	       :parameters ()
	       :precondition (p3)
	       :effect (p2))
    (:action A3
	       :parameters ()
	       :precondition (p4)
	       :effect (p3))
    (:action B3
	       :parameters ()
	       :precondition (p4)
	       :effect (p3))
    (:action A4
	       :parameters ()
	       :precondition (p5)
	       :effect (p4))
    (:action B4
	       :parameters ()
	       :precondition (p5)
	       :effect (p4))
    (:action A5
	       :parameters ()
	       :precondition (p6)
	       :effect (p5))
    (:action B5
	       :parameters ()
	       :precondition (p6)
	       :effect (p5))
    (:action X1
	       :parameters ()
	       :precondition (q2)
	       :effect (q1))
    (:action Y1
	       :parameters ()
	       :precondition (q2)
	       :effect (q1))
    (:action X2
	       :parameters ()
	       :precondition (q3)
	       :effect (q2))
    (:action Y2
	       :parameters ()
	       :precondition (q3)
	       :effect (q2))
    (:action X3
	       :parameters ()
	       :precondition (q4)
	       :effect (q3))
    (:action Y3
	       :parameters ()
	       :precondition (q4)
	       :effect (q3))
    (:action X4
	       :parameters ()
	       :precondition (q5)
	       :effect (q4))
    (:action Y4
	       :parameters ()
	       :precondition (q5)
	       :effect (q4))
    (:action X5
	       :parameters ()
	       :precondition (q6)
	       :effect (q5))
    (:action Y5
	       :parameters ()
	       :precondition (q6)
	       :effect (q5)))

(define (problem art1a)
    (:domain art)
  (:init (q1))
  (:goal (AND (g)))
  (:length (:serial 1) (:parallel 1)))

(define (problem art1b)
    (:domain art)
  (:init  (p1))
  (:goal (AND (g)))
  (:length (:serial 1) (:parallel 1)))

(define (problem art2a)
    (:domain art)
  (:init (q2))
  (:goal (AND (g)))
  (:length (:serial 2) (:parallel 2)))

(define (problem art2b)
    (:domain art)
  (:init (p2))
  (:goal (AND (g)))
  (:length (:serial 2) (:parallel 2)))

(define (problem art3a)
    (:domain art)
  (:init (q3))
  (:goal (AND (g)))
  (:length (:serial 3) (:parallel 3)))

(define (problem art3b)
    (:domain art)
  (:init (p3))
  (:goal (AND (g)))
  (:length (:serial 3) (:parallel 3)))

(define (problem art4a)
    (:domain art)
  (:init (q4))
  (:goal (AND (g)))
  (:length (:serial 4) (:parallel 4)))

(define (problem art4b)
    (:domain art)
  (:init (p4))
  (:goal (AND (g)))
  (:length (:serial 4) (:parallel 4)))

(define (problem art5a)
    (:domain art)
  (:init (q5))
  (:goal (AND (g)))
  (:length (:serial 5) (:parallel 5)))

(define (problem art5b)
    (:domain art)
  (:init (p5))
  (:goal (AND (g)))
  (:length (:serial 5) (:parallel 5)))

(define (problem art6a)
    (:domain art)
  (:init (q6))
  (:goal (AND (g)))
  (:length (:serial 6) (:parallel 6)))

(define (problem art6b)
    (:domain art)
  (:init (p6))
  (:goal (AND (g)))
  (:length (:serial 6) (:parallel 6)))







