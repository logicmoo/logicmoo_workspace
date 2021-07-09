
(define (domain fairytalecastle)

(:requirements :typing)

(:types
	special couch table chest seating open_container
	open_close_container treasury drawing_room container_object
	player room sword dragon worm frog golden black red brown
	yellow white silver green colour apple necklace crown key
	castle seat couchleg climbable edible takeable west_exit
	east_exit south_exit north_exit exit wall small ugly wooden
	generic_container weapon not_so_easy_to_kill easy_to_kill
	property object locked_unlocked open_closed 
)

(:action take_patient
	:parameters (?x - takeable ?y - generic_container)
	:precondition 
		  (and  
		        (accessible ?x) 
			(no_inventory_object ?x)
			(has_location ?x ?y) 
			)
	:effect 
           (and (inventory_object ?x)
		(not(no_inventory_object ?x)) 
		(not(has_location ?x ?y)))
		
)

(:action take_patient
	:parameters (?x - takeable ?y - top)
	:precondition 	
		     (and   (accessible ?x)
			(no_inventory_object ?x)
                        (has_detail ?y ?x))
	:effect	
           (and 
	       (inventory_object ?x)
		(not(no_inventory_object ?x)) 
                (not(has_detail ?y ?x)) 
		(no_has_detail ?y ?x) )
)

(:action drop_patient
	:parameters (?x - object ?y - generic_container)
	:precondition 
	     (and 
	      (inventory_object ?x)
                      (here ?y))
	:effect 
	   (and
		(not(inventory_object ?x))
		(no_inventory_object ?x)
                (has_location ?x ?y) )
)

(:action put_patient
	:parameters (?x - takeable ?y - generic_container)
	:precondition 
	       (and (inventory_object ?x) 
		(accessible ?x)
		(accessible ?y))
	:effect 
	 (and
		(has_location ?x ?y) 
		(not(inventory_object ?x)))
)			    
		
(:action throw_patient_target 
	 :parameters (?x - takeable ?y - generic_container)
	 :precondition 
		    (and   (inventory_object ?x)
		       (alive ?x)
		       (accessible ?x))
	 :effect (and
		 (not(inventory_object ?x))
		 (no_inventory_object ?x)
		 (has_location ?x ?y)
		 (not(alive ?x))
		 )
)

(:action kill_patient
	 :parameters (?x - easy_to_kill)
	 :precondition 
	 (and
		       (alive ?x)
		       (accessible ?x)
		       )
	 :effect 
	 (and
	  (not(alive ?x))
		 (not(accessible ?x))
		 (no_accessible ?x)
		 )
)

(:action kill_patient_instr
	 :parameters (?x - not_so_easy_to_kill ?y - weapon)
	 :precondition 
	 (and 
		       (alive ?x)
		       (accessible ?x)
		       (inventory_object ?y)
		       )
	 :effect
	 (and
	      (not(alive ?x))
		 (not(accessible))
		 (no_accessible ?x)
		 )
)

(:action open_patient
	 :parameters (?x - open_close_container)
	 :precondition 
		  (and
		       (closed ?x)
		       (unlocked ?x)
		       (accessible ?x)
		       )
	 :effect
	 (and
		(not(closed ?x))
		(open ?x)
		)
)

(:action close_patient
	 :parameters (?x - open_close_container)
	 :precondition
	    (and
			(open ?x)
			(accessible ?x)
			)
	 :effect
	  (and
		(closed ?x)
		(not(open ?x))
		)
)

(:action unlock_patient_instr
	 :parameters (?x - open_close_container ?y - key)
	 :precondition
		 (and
		       (locked ?x)
		       (accessible ?x)
		       (inventory_object ?y)
		       (fits_in ?y ?x)
		       )
	 :effect
	 (and
		(not(locked ?x))
		(unlocked ?x)
		)
)

(:action lock_patient_instr
	 :parameters (?x - open_close_container ?y - key)
	 :precondition
		 (and
		       (closed ?x)
		       (unlocked ?x)
		       (accessible ?x)
		       (inventory_object ?y)
		       (fits_in ?y ?x)
		       )
	 :effect
	 (and
		(locked ?x)
		(not(unlocked ?x))
		)
)

(:action eat_patient
	:parameters (?x - edible ?y - worm)
	:precondition
	(and (inventory_object ?x)
		      (no_has_detail ?x ?y)	
		      )
	:effect
	 (and
	  (nirvana ?x)
		(not(inventory_object ?x))
		(no_inventory_object ?x)
		(not(accessible ?x))
		(no_accessible ?x)
		)
)

(:action sitdown_patient
	 :parameters (?x - seating ?y - generic_container)
	 :precondition
	   (and
	        	(accessible ?x)
			(no_here ?x)
			(here ?y)
			)
	 :effect
	 (and
		(here ?x)
		(not(no_here ?x))
		(not(here ?y))
		(no_here ?y)
		(not(accessible ?y))
		(no_accessible ?y)
		)
)

(:action standup_patient
	 :parameters (?x - seating ?y - generic_container)
	 :precondition
	 (and
			(here ?x)
			(has_location ?x ?y)
			)
	 :effect
	   (and
		(here ?y)
		(no_here ?x)
		(not(here ?x))
		(not(no_here ?y))
		(accessible ?y)
		(not(no_accessible ?y))
		)
)

(:action access
	 :parameters (?x - generic_container ?z - top)
	 :precondition
	   (and
			(accessible ?x)
			(open ?x)
			(has_location ?z ?x)
			)
	 :effect 
	 (and
			(accessible ?z) 
			(not(no_accessible ?z))
			)
)

(:action unaccess_first
	 :parameters (?x - generic_container ?z - top)
	 :precondition
	 (and
			(accessible ?z)
			(closed ?x)
			(has_location ?z ?x)
			)
	 :effect 
	 (and
			(not(accessible ?z)) 
			(no_accessible ?z)
			)
)

(:action unaccess_recursion
	 :parameters (?x - generic_container ?z - top)
	 :precondition
	 (and
			(no_accessible ?x)
			(has_location ?z ?x)
			(accessible ?z)
			(no_here ?z)
			)
	 :effect 
	 (and
			(not(accessible ?z)) 
			(no_accessible ?z)
			)
)


)
 

