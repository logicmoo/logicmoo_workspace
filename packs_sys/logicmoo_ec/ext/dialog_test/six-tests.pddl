(define
  (domain Car-Inspection-659ec0ca-00c3-5d29-4028-2dca3c489a64)
(:requirements :strips :typing)
(:types )
(:constants )

(:predicates 
  (does-have-status_C4)
  (maybe-have-status_C4)
  (does-have-status_C3)
  (maybe-have-status_C3)
  (does-have user_initiative_message)
  (maybe-have user_initiative_message)
  (does-have-status_C2)
  (maybe-have-status_C2)
  (does-have pass_status_options)
  (maybe-have pass_status_options)
  (does-have-status_FLU)
  (maybe-have-status_FLU)
  (user_initiative)
  (does-have message)
  (maybe-have message)
  (GOAL)
  (STARTED)
  (can-do-disambiguation-start_conversation)
  (can-do-disambiguation-ask-for_C1)
  (can-do-disambiguation-ask-for_C2)
  (can-do-disambiguation-ask-for_C3)
  (can-do-disambiguation-ask-for_C4)
  (can-do-disambiguation-end_conversation)
  (can-do-disambiguation-state-message))
(:action dialogue-disambiguation-start_conversation
  :parameters ()
  :precondition
   (and
    (can-do-disambiguation-start_conversation)
    (does-have user_initiative_message)
    (not (maybe-have user_initiative_message))
    (user_initiative))
  :effect
    (labeled-oneof resolve-start_conversation
    (outcome start_conversation_what__
    (and
      (does-have user_initiative_message)
      (not (maybe-have user_initiative_message))
      (STARTED)
      (can-do-disambiguation-start_conversation)
      (can-do-disambiguation-ask-for_C1)
      (can-do-disambiguation-ask-for_C2)
      (can-do-disambiguation-ask-for_C3)
      (can-do-disambiguation-ask-for_C4)
      (can-do-disambiguation-end_conversation)
      (can-do-disambiguation-state-message)))
    (outcome start_conversation_initiative-switch__
     (and
      (not (user_initiative))
      (STARTED)
      (can-do-disambiguation-start_conversation)
      (can-do-disambiguation-ask-for_C1)
      (can-do-disambiguation-ask-for_C2)
      (can-do-disambiguation-ask-for_C3)
      (can-do-disambiguation-ask-for_C4)
      (can-do-disambiguation-end_conversation)
      (can-do-disambiguation-state-message)))
    (outcome start_conversation_C3__check-pass_status_options-eq-found
     (and
      (does-have pass_status_options)
      (not (maybe-have pass_status_options))
      (does-have user_initiative_message)
      (not (maybe-have user_initiative_message))
      (does-have-status_C3)
      (not (maybe-have-status_C3))
      (does-have pass_status_options)
      (not (maybe-have pass_status_options))
      (STARTED)
      (can-do-disambiguation-start_conversation)
      (can-do-disambiguation-ask-for_C1)
      (can-do-disambiguation-ask-for_C2)
      (can-do-disambiguation-ask-for_C3)
      (can-do-disambiguation-ask-for_C4)
      (can-do-disambiguation-end_conversation)
      (can-do-disambiguation-state-message)))
    (outcome start_conversation_C4__check-pass_status_options-eq-found
     (and
      (does-have pass_status_options)
      (not (maybe-have pass_status_options))
      (does-have user_initiative_message)
      (not (maybe-have user_initiative_message))
      (does-have-status_C4)
      (not (maybe-have-status_C4))
      (does-have pass_status_options)
      (not (maybe-have pass_status_options))
      (STARTED)
      (can-do-disambiguation-start_conversation)
      (can-do-disambiguation-ask-for_C1)
      (can-do-disambiguation-ask-for_C2)
      (can-do-disambiguation-ask-for_C3)
      (can-do-disambiguation-ask-for_C4)
      (can-do-disambiguation-end_conversation)
      (can-do-disambiguation-state-message)))
    (outcome start_conversation_C2__check-status_C2-eq-found
     (and
      (does-have-status_C2)
      (not (maybe-have-status_C2))
      (does-have user_initiative_message)
      (not (maybe-have user_initiative_message))
      (does-have-status_C2)
      (not (maybe-have-status_C2))
      (STARTED)
      (can-do-disambiguation-start_conversation)
      (can-do-disambiguation-ask-for_C1)
      (can-do-disambiguation-ask-for_C2)
      (can-do-disambiguation-ask-for_C3)
      (can-do-disambiguation-ask-for_C4)
      (can-do-disambiguation-end_conversation)
      (can-do-disambiguation-state-message)))
    (outcome start_conversation_FLU__check-status_FLU-eq-found
     (and
      (does-have-status_FLU)
      (not (maybe-have-status_FLU))
      (does-have user_initiative_message)
      (not (maybe-have user_initiative_message))
      (does-have-status_FLU)
      (not (maybe-have-status_FLU))
      (STARTED)
      (can-do-disambiguation-start_conversation)
      (can-do-disambiguation-ask-for_C1)
      (can-do-disambiguation-ask-for_C2)
      (can-do-disambiguation-ask-for_C3)
      (can-do-disambiguation-ask-for_C4)
      (can-do-disambiguation-end_conversation)
      (can-do-disambiguation-state-message)))
    (outcome start_conversation_fallback__
     (and
      (does-have user_initiative_message)
      (not (maybe-have user_initiative_message))
      (STARTED)
      (can-do-disambiguation-start_conversation)
      (not (can-do-disambiguation-ask-for_C1))
      (not (can-do-disambiguation-ask-for_C2))
      (not (can-do-disambiguation-ask-for_C3))
      (not (can-do-disambiguation-ask-for_C4))
      (not (can-do-disambiguation-end_conversation))
      (not (can-do-disambiguation-state-message))))))

(:action dialogue-disambiguation-ask-for_C1
  :parameters ()
  :precondition
   (and
    (can-do-disambiguation-ask-for_C1)
    (not (user_initiative))
    (not (does-have-status_FLU))
    (not (maybe-have-status_FLU))
    (STARTED))
  :effect
    (labeled-oneof resolve-ask-for_C1
    (outcome ask-for_C1_initiative-switch__
     (and
      (does-have user_initiative_message)
      (not (maybe-have user_initiative_message))
      (user_initiative)
      (can-do-disambiguation-start_conversation)
      (can-do-disambiguation-ask-for_C1)
      (can-do-disambiguation-ask-for_C2)
      (can-do-disambiguation-ask-for_C3)
      (can-do-disambiguation-ask-for_C4)
      (can-do-disambiguation-end_conversation)
      (can-do-disambiguation-state-message)))
    (outcome ask-for_C1_detected__check-status_FLU-eq-found
     (and
      (does-have-status_FLU)
      (not (maybe-have-status_FLU))
      (does-have message)
      (not (maybe-have message))
      (does-have-status_FLU)
      (not (maybe-have-status_FLU))
      (not (can-do-disambiguation-start_conversation))
      (not (can-do-disambiguation-ask-for_C1))
      (not (can-do-disambiguation-ask-for_C2))
      (not (can-do-disambiguation-ask-for_C3))
      (not (can-do-disambiguation-ask-for_C4))
      (not (can-do-disambiguation-end_conversation))
      (can-do-disambiguation-state-message)))
    (outcome ask-for_C1_help-local-options__
     (and
      (does-have message)
      (not (maybe-have message))
      (not (can-do-disambiguation-start_conversation))
      (not (can-do-disambiguation-ask-for_C1))
      (not (can-do-disambiguation-ask-for_C2))
      (not (can-do-disambiguation-ask-for_C3))
      (not (can-do-disambiguation-ask-for_C4))
      (not (can-do-disambiguation-end_conversation))
      (can-do-disambiguation-state-message)))))

(:action dialogue-disambiguation-ask-for_C2
  :parameters ()
  :precondition
   (and
    (can-do-disambiguation-ask-for_C2)
    (not (user_initiative))
    (not (does-have-status_C2))
    (not (maybe-have-status_C2))
    (STARTED))
  :effect
    (labeled-oneof resolve-ask-for_C2
    (outcome ask-for_C2_initiative-switch__
     (and
      (does-have user_initiative_message)
      (not (maybe-have user_initiative_message))
      (user_initiative)
      (can-do-disambiguation-start_conversation)
      (can-do-disambiguation-ask-for_C1)
      (can-do-disambiguation-ask-for_C2)
      (can-do-disambiguation-ask-for_C3)
      (can-do-disambiguation-ask-for_C4)
      (can-do-disambiguation-end_conversation)
      (can-do-disambiguation-state-message)))
    (outcome ask-for_C2_detected__check-status_C2-eq-found
     (and
      (does-have-status_C2)
      (not (maybe-have-status_C2))
      (does-have message)
      (not (maybe-have message))
      (does-have-status_C2)
      (not (maybe-have-status_C2))
      (not (can-do-disambiguation-start_conversation))
      (not (can-do-disambiguation-ask-for_C1))
      (not (can-do-disambiguation-ask-for_C2))
      (not (can-do-disambiguation-ask-for_C3))
      (not (can-do-disambiguation-ask-for_C4))
      (not (can-do-disambiguation-end_conversation))
      (can-do-disambiguation-state-message)))
    (outcome ask-for_C2_help-local-options__
     (and
      (does-have message)
      (not (maybe-have message))
      (not (can-do-disambiguation-start_conversation))
      (not (can-do-disambiguation-ask-for_C1))
      (not (can-do-disambiguation-ask-for_C2))
      (not (can-do-disambiguation-ask-for_C3))
      (not (can-do-disambiguation-ask-for_C4))
      (not (can-do-disambiguation-end_conversation))
      (can-do-disambiguation-state-message)))))

(:action dialogue-disambiguation-ask-for_C3
  :parameters ()
  :precondition
   (and
    (can-do-disambiguation-ask-for_C3)
    (not (user_initiative))
    (not (does-have-status_C3))
    (not (maybe-have-status_C3))
    (STARTED))
  :effect
    (labeled-oneof resolve-ask-for_C3
    (outcome ask-for_C3_initiative-switch__
     (and
      (user_initiative)
      (does-have user_initiative_message)
      (not (maybe-have user_initiative_message))
      (can-do-disambiguation-start_conversation)
      (can-do-disambiguation-ask-for_C1)
      (can-do-disambiguation-ask-for_C2)
      (can-do-disambiguation-ask-for_C3)
      (can-do-disambiguation-ask-for_C4)
      (can-do-disambiguation-end_conversation)
      (can-do-disambiguation-state-message)))
    (outcome ask-for_C3_detected__check-pass_status_options-eq-found
     (and
      (does-have pass_status_options)
      (not (maybe-have pass_status_options))
      (does-have message)
      (not (maybe-have message))
      (does-have-status_C3)
      (not (maybe-have-status_C3))
      (does-have pass_status_options)
      (not (maybe-have pass_status_options))
      (not (can-do-disambiguation-start_conversation))
      (not (can-do-disambiguation-ask-for_C1))
      (not (can-do-disambiguation-ask-for_C2))
      (not (can-do-disambiguation-ask-for_C3))
      (not (can-do-disambiguation-ask-for_C4))
      (not (can-do-disambiguation-end_conversation))
      (can-do-disambiguation-state-message)))
    (outcome ask-for_C3_help-local-options__
     (and
      (does-have message)
      (not (maybe-have message))
      (not (can-do-disambiguation-start_conversation))
      (not (can-do-disambiguation-ask-for_C1))
      (not (can-do-disambiguation-ask-for_C2))
      (not (can-do-disambiguation-ask-for_C3))
      (not (can-do-disambiguation-ask-for_C4))
      (not (can-do-disambiguation-end_conversation))
      (can-do-disambiguation-state-message)))))

(:action dialogue-disambiguation-ask-for_C4
  :parameters ()
  :precondition
   (and
    (can-do-disambiguation-ask-for_C4)
    (not (user_initiative))
    (not (does-have-status_C4))
    (not (maybe-have-status_C4))
    (STARTED))
  :effect
    (labeled-oneof resolve-ask-for_C4
    (outcome ask-for_C4_initiative-switch__
     (and
      (does-have user_initiative_message)
      (not (maybe-have user_initiative_message))
      (user_initiative)
      (can-do-disambiguation-start_conversation)
      (can-do-disambiguation-ask-for_C1)
      (can-do-disambiguation-ask-for_C2)
      (can-do-disambiguation-ask-for_C3)
      (can-do-disambiguation-ask-for_C4)
      (can-do-disambiguation-end_conversation)
      (can-do-disambiguation-state-message)))
    (outcome ask-for_C4_detected__check-pass_status_options-eq-found
     (and
      (does-have pass_status_options)
      (not (maybe-have pass_status_options))
      (does-have message)
      (not (maybe-have message))
      (does-have-status_C4)
      (not (maybe-have-status_C4))
      (does-have pass_status_options)
      (not (maybe-have pass_status_options))
      (not (can-do-disambiguation-start_conversation))
      (not (can-do-disambiguation-ask-for_C1))
      (not (can-do-disambiguation-ask-for_C2))
      (not (can-do-disambiguation-ask-for_C3))
      (not (can-do-disambiguation-ask-for_C4))
      (not (can-do-disambiguation-end_conversation))
      (can-do-disambiguation-state-message)))
    (outcome ask-for_C4_help-local-options__
     (and
      (does-have message)
      (not (maybe-have message))
      (not (can-do-disambiguation-start_conversation))
      (not (can-do-disambiguation-ask-for_C1))
      (not (can-do-disambiguation-ask-for_C2))
      (not (can-do-disambiguation-ask-for_C3))
      (not (can-do-disambiguation-ask-for_C4))
      (not (can-do-disambiguation-end_conversation))
      (can-do-disambiguation-state-message)))))

(:action dialogue-disambiguation-end_conversation
  :parameters ()
  :precondition
   (and
    (can-do-disambiguation-end_conversation)
    (does-have-status_FLU)
    (not (maybe-have-status_FLU))
    (does-have-status_C2)
    (not (maybe-have-status_C2))
    (does-have-status_C3)
    (not (maybe-have-status_C3))
    (does-have-status_C4)
    (not (maybe-have-status_C4))
    (STARTED))
  :effect
    (labeled-oneof resolve-end_conversation
    (outcome end_conversation-outcome-fallback__
     (and
      (GOAL)
      (can-do-disambiguation-start_conversation)
      (can-do-disambiguation-ask-for_C1)
      (can-do-disambiguation-ask-for_C2)
      (can-do-disambiguation-ask-for_C3)
      (can-do-disambiguation-ask-for_C4)
      (can-do-disambiguation-end_conversation)
      (can-do-disambiguation-state-message)))))

(:action dialogue-disambiguation-state-message
  :parameters ()
  :precondition
   (and
    (can-do-disambiguation-state-message)
    (does-have message)
    (not (maybe-have message))
    (STARTED))
  :effect
    (labeled-oneof resolve-state-message
    (outcome state-message-outcome-fallback__
     (and
      (not (does-have message))
      (not (maybe-have message))
      (can-do-disambiguation-start_conversation)
      (can-do-disambiguation-ask-for_C1)
      (can-do-disambiguation-ask-for_C2)
      (can-do-disambiguation-ask-for_C3)
      (can-do-disambiguation-ask-for_C4)
      (can-do-disambiguation-end_conversation)
      (can-do-disambiguation-state-message)))))

)
(define
  (problem Car-Inspection-659ec0ca-00c3-5d29-4028-2dca3c489a64_problem)
(:domain Car-Inspection-659ec0ca-00c3-5d29-4028-2dca3c489a64)
(:objects)
(:init
   (does-have user_initiative_message)
   (user_initiative)
   (can-do-disambiguation-start_conversation)
   (can-do-disambiguation-ask-for_C1)
   (can-do-disambiguation-ask-for_C2)
   (can-do-disambiguation-ask-for_C3)
   (can-do-disambiguation-ask-for_C4)
   (can-do-disambiguation-end_conversation)
   (can-do-disambiguation-state-message))
(:goal
  (and
  (GOAL))))

(define (domain test_domain)
(:requirements :strips)
(:predicates 
   (FOIL_ACT_EXEC_dialogue-disambiguation-ask-for_C2)
   (user_initiative)
   (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation)
   (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation_OUTCOME_start_conversation_initiative-switch__)
   (goal))

(:action dialogue-disambiguation-ask-for_C2 
	   :parameters () 
	   :precondition(and (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation_OUTCOME_start_conversation_initiative-switch__)
			  (not (user_initiative)) )
	   :effect(and (labeled-oneof resolve-ask-for_C2 (outcome ask-for_C2_detected__check-status_C2-eq-found(and (FOIL_ACT_EXEC_dialogue-disambiguation-ask-for_C2)))
				   (outcome ask-for_C2_initiative-switch__ (and (FOIL_ACT_EXEC_dialogue-disambiguation-ask-for_C2)
												   (user_initiative)))
				   (outcome ask-for_C2_help-local-options__ (and (FOIL_ACT_EXEC_dialogue-disambiguation-ask-for_C2))) ) ))

(:action dialogue-disambiguation-ask-for_C1 
	   :parameters () 
	   :precondition(and 
			  (not (user_initiative)) )
	   :effect(and (labeled-oneof resolve-ask-for_C1 (outcome ask-for_C1_help-local-options__ (and ))
				   (outcome ask-for_C1_initiative-switch__ (and (user_initiative)))
				   (outcome ask-for_C1_detected__check-status_FLU-eq-found(and )) ) ))

(:action dialogue-disambiguation-start_conversation 
	   :parameters () 
	   :precondition(and (user_initiative)
			  (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation))
	   :effect(and (labeled-oneof resolve-start_conversation (outcome start_conversation_fallback__ (and (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation)))
				   (outcome start_conversation_C2__check-status_C2-eq-found(and (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation)))
				   (outcome start_conversation_initiative-switch__ (and (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation)
											  (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation_OUTCOME_start_conversation_initiative-switch__)))
				   (outcome start_conversation_C4__check-pass_status_options-eq-found(and (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation)))
				   (outcome start_conversation_what__ (and (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation)))
				   (outcome start_conversation_FLU__check-status_FLU-eq-found(and (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation)))
				   (outcome start_conversation_C3__check-pass_status_options-eq-found(and (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation))) ) ))

(:action dialogue-disambiguation-ask-for_C4 
	   :parameters () 
	   :precondition(and 
			  (not (user_initiative)) )
	   :effect(and (labeled-oneof resolve-ask-for_C4 (outcome ask-for_C4_initiative-switch__ (and (user_initiative)))
				   (outcome ask-for_C4_help-local-options__ (and ))
				   (outcome ask-for_C4_detected__check-pass_status_options-eq-found(and )) ) ))

(:action dialogue-disambiguation-end_conversation 
	   :parameters () 
	   :precondition(and )
	   :effect(and (labeled-oneof resolve-end_conversation (outcome end_conversation-outcome-fallback__ (and (goal))) ) ))

(:action dialogue-disambiguation-ask-for_C3 
	   :parameters () 
	   :precondition(and 
			  (not (user_initiative)) )
	   :effect(and (labeled-oneof resolve-ask-for_C3 (outcome ask-for_C3_initiative-switch__ (and (user_initiative)))
				   (outcome ask-for_C3_detected__check-pass_status_options-eq-found(and ))
				   (outcome ask-for_C3_help-local-options__ (and )) ) ))
)
(define (domain test_domain)
(:requirements :strips)
(:predicates 
   (goal)
   (does-have-status_C4)
   (user_initiative))

(:action dialogue-disambiguation-ask-for_C1 
	   :parameters () 
	   :precondition(and 
			  (not (user_initiative)) )
	   :effect(and (labeled-oneof resolve-ask-for_C1 (outcome ask-for_C1_detected__check-status_FLU-eq-found(and ))
				   (outcome ask-for_C1_help-local-options__ (and ))
				   (outcome ask-for_C1_initiative-switch__ (and (user_initiative))) ) ))

(:action dialogue-disambiguation-end_conversation 
	   :parameters () 
	   :precondition(and (does-have-status_C4))
	   :effect(and (labeled-oneof resolve-end_conversation (outcome end_conversation-outcome-fallback__ (and (goal))) ) ))

(:action dialogue-disambiguation-ask-for_C3 
	   :parameters () 
	   :precondition(and 
			  (not (user_initiative)) )
	   :effect(and (labeled-oneof resolve-ask-for_C3 (outcome ask-for_C3_initiative-switch__ (and (user_initiative)))
				   (outcome ask-for_C3_help-local-options__ (and ))
				   (outcome ask-for_C3_detected__check-pass_status_options-eq-found(and )) ) ))

(:action dialogue-disambiguation-ask-for_C4 
	   :parameters () 
	   :precondition(and 
			  (not (user_initiative))
			  (not (does-have-status_C4)) )
	   :effect(and (labeled-oneof resolve-ask-for_C4 (outcome ask-for_C4_initiative-switch__ (and (user_initiative)))
				   (outcome ask-for_C4_help-local-options__ (and ))
				   (outcome ask-for_C4_detected__check-pass_status_options-eq-found(and (does-have-status_C4))) ) ))

(:action dialogue-disambiguation-ask-for_C2 
	   :parameters () 
	   :precondition(and 
			  (not (user_initiative)) )
	   :effect(and (labeled-oneof resolve-ask-for_C2 (outcome ask-for_C2_help-local-options__ (and ))
				   (outcome ask-for_C2_detected__check-status_C2-eq-found(and ))
				   (outcome ask-for_C2_initiative-switch__ (and (user_initiative))) ) ))
)


(define (problem test_problem)
(:domain test_domain)

(:objects )

(:init 
   (user_initiative))

(:goal(and 
	  (FOIL_ACT_EXEC_dialogue-disambiguation-ask-for_C2)
	  (goal))))


(define (problem test_problem)
(:domain test_domain)

(:objects )

(:init 
   (user_initiative))

(:goal(and 
	  (goal))))


