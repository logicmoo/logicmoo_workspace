
# Test Domains

Empirical testing found that some of planners advertised capabilities either were not implemented, crashed the planner, did not parse properly or did not return a plan at all. To test the planners, a few simple domains and sample problems are implemented that test a specific feature of the PDDL language. The domains and their problems are listed below. The results of the tests can be found in the table at the bottom of this page.


# Simple Strips Blocks World
The Blocks World described in STRIPS. Predicates are used for on, clear and block. The goal is to build a single tower with all three blocks.

````pddl
(define (domain strips-blocksworld)
  (:requirements :strips)

  (:predicates
    (on ?x ?y) (clear ?x) (block ?x))

  (:action move
    :parameters (?b ?x ?y)
    :precondition (and (block ?b) (clear ?b) (on ?b ?x) (block ?y) (clear ?y))
    :effect (and (not (on ?b ?x)) (clear ?x)
  	       (not (clear ?y)) (on ?b ?y)))

  (:action move-to-table
    :parameters (?b ?x)
    :precondition (and (block ?b) (on ?b ?x) (clear ?b))
    :effect (and (not (on ?b ?x)) (clear ?x) (on ?b table)))
)
````

````pddl
(define (problem strips-bw-1)
  (:domain strips-blocksworld)

  (:objects a b c table)

  (:init
     (on b table) (on a table) (on c a)
     (clear b) (clear c) (clear table)
     (block a) (block b) (block c))

  (:goal
     (and (on a b) (on b c) (on c table)))
)
````

# Blocks World in ADL
The same Blocks World, but implemented using ADL features. It uses typing to get rid of the unary block predicate and conditional effects to integrate both move actions of the STRIPS example into a single action.
````pddl
(define (domain adl-blocksworld)
  (:requirements :adl)
  (:types block)
  (:predicates (on ?x ?y) (clear ?x))

  (:action move
     :parameters (?b - block ?x ?y)
     :precondition (and
     		    (clear ?b) (on ?b ?x) (clear ?y))
     :effect (and (on ?b ?y)
     	     	  (not (on ?b ?x))
		  (clear ?x)
		  (when (not (= ?y table))
		  	(not (clear ?y))))
  )
)
````

````pddl
(define (problem adl-blocksworld-problem)
  (:domain adl-blocksworld)
  (:objects a b c - block table)

  (:init
     (on b table) (on a table) (on c a)
     (clear b) (clear c) (clear table)
  )

  (:goal
     (and (on a b) (on b c) (on c table)))
)
````

# Blocks World with ADL and Derived Predicates
This example uses derived predicates to infer the value of the clear predicate, instead of having to keep track of its value manually through the action as in the previous ADL example.

````pddl
(define (domain der-adl-blocksworld)
  (:requirements :adl :derived-predicates)
  (:types block)
  (:predicates (on ?x ?y))
  (:derived (clear ?x) (or (= ?x table)
 	                   (not (exists (?y - block) (on ?y ?x)))))

  (:action move
     :parameters (?b - block ?x ?y)
     :precondition (and (clear ?b) (on ?b ?x) (clear ?y))
     :effect (and (on ?b ?y)
     	     	  (not (on ?b ?x)))
  )
)
````

````pddl
(define (problem der-adl-blocksworld-problem)
  (:domain der-adl-blocksworld)
  (:objects a b c - block table)

  (:init
     (on b table) (on a table) (on c a)
  )

  (:goal
     (and (on a b) (on b c) (on c table)))
)
````

# Variable Binding in Preconditions
One particular oddity of the move action in the ADL example is that it requires three parameters. The two obvious parameters are the block to move, and its destination. Maybe unexpectedly, another parameter is required, and that is the block that is underneath the block that is to be moved. This is to unset the on predicate that describes the current situation of block b on block x before the move. Since this block x can be derived from the data, it would feel natural if it is not required as a parameter and instead could be queried in the precondition. This program tests whether this is supported.

````pddl
(define (domain precond-adl-blocksworld)
  (:requirements :adl)
  (:types block)
  (:predicates (on ?x ?y) (clear ?x))

  (:action move
; notice the use of only two parameters, the parameter
; ?x is bound in the precondition.
     :parameters (?b - block ?y)
     :precondition (exists (?x) (and
     		    (clear ?b) (on ?b ?x) (clear ?y)))
     :effect (and (on ?b ?y)
     	     	  (not (on ?b ?x))
		  (clear ?x)
		  (when (not (= ?y table))
		  	(not (clear ?y))))
  )
)
````

# Jug Pouring
The jug pouring domain describes the classic problem of having three jugs with a capacity of eight, five and three liters, respectively. At the start the largest jug is completely filled, and the goal is to divide the contents in two, ie. four liters in two jugs. This domain tests the numerical features of PDDL.

````pddl
(define (domain jug-pouring)
(:requirements :typing :fluents)
(:types jug)
(:functions
  (amount ?j - jug) - number
  (capacity ?j - jug) - number)
(:action pour
  :parameters (?jug1 ?jug2 - jug)
  :precondition (> (- (capacity ?jug2) (amount ?jug2)) 0)
  :effect (and
; if the available space in jug2 is larger or equal to the amount in
; jug1, we pour everything in and jug1 is empty.
                (when (>= (- (capacity ?jug2) (amount ?jug2)) (amount ?jug1))
                  (and
		     (increase (amount ?jug2) (amount ?jug1))
                     (assign (amount ?jug1) 0)
                  ))
; if jug2 can only be partially filled, we subtract the amount from jug1
; and add it to jug2 until jug2 is full.
      	     	(when (< (- (capacity ?jug2) (amount ?jug2)) (amount ?jug1))
                  (and
                     (decrease (amount ?jug1) (- (capacity ?jug2) (amount ?jug2)))
		     (assign (amount ?jug2) (capacity ?jug2))
		  ))
           ))
)
````

````pddl
(define (problem jug-pouring-problem)
  (:domain jug-pouring)

  (:objects jug3 jug5 jug8 - jug)

  (:init (= (amount jug3) 0)
         (= (capacity jug3) 3)
         (= (amount jug5) 0)
         (= (capacity jug5) 5)
         (= (amount jug8) 8)
         (= (capacity jug8) 8))

  (:goal (= (amount jug8) 4))
)

````
