(define (domain test_domain)
    (:requirements :strips)
  (:predicates 
   (FOIL_ACT_EXEC_dialogue-disambiguation-ask-for_clutch-seal-tightness)
   (user_initiative)
   (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation)
   (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation_OUTCOME_start_conversation_initiative-switch__)
   (goal)
   )

  (:action dialogue-disambiguation-ask-for_clutch-seal-tightness 
	   :parameters () 
	   :precondition (and (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation_OUTCOME_start_conversation_initiative-switch__)
			      (not (user_initiative)) )
	   :effect (and (labeled-oneof resolve-ask-for_clutch-seal-tightness (outcome ask-for_clutch-seal-tightness_detected__check-clutch_seal_tightness_status-eq-found (and (FOIL_ACT_EXEC_dialogue-disambiguation-ask-for_clutch-seal-tightness)
																					       ))
				       (outcome ask-for_clutch-seal-tightness_initiative-switch__ (and (FOIL_ACT_EXEC_dialogue-disambiguation-ask-for_clutch-seal-tightness)
												       (user_initiative)
												       ))
				       (outcome ask-for_clutch-seal-tightness_help-local-options__ (and (FOIL_ACT_EXEC_dialogue-disambiguation-ask-for_clutch-seal-tightness)
													)) ) ))
  (:action dialogue-disambiguation-ask-for_oil-level 
	   :parameters () 
	   :precondition (and 
			  (not (user_initiative)) )
	   :effect (and (labeled-oneof resolve-ask-for_oil-level (outcome ask-for_oil-level_help-local-options__ (and 
														  ))
				       (outcome ask-for_oil-level_initiative-switch__ (and (user_initiative)
											   ))
				       (outcome ask-for_oil-level_detected__check-oil_status-eq-found (and 
												       )) ) ))
  (:action dialogue-disambiguation-start_conversation 
	   :parameters () 
	   :precondition (and (user_initiative)
			      (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation)
			      )
	   :effect (and (labeled-oneof resolve-start_conversation (outcome start_conversation_fallback__ (and (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation)
													      ))
				       (outcome start_conversation_clutch-seal-tightness__check-clutch_seal_tightness_status-eq-found (and (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation)
																	   ))
				       (outcome start_conversation_initiative-switch__ (and (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation)
											    (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation_OUTCOME_start_conversation_initiative-switch__)
											    ))
				       (outcome start_conversation_spark-plug__check-pass_status_options-eq-found (and (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation)
														       ))
				       (outcome start_conversation_what__ (and (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation)
									       ))
				       (outcome start_conversation_oil__check-oil_status-eq-found (and (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation)
												       ))
				       (outcome start_conversation_break-pad__check-pass_status_options-eq-found (and (FOIL_ACT_EXEC_dialogue-disambiguation-start_conversation)
														      )) ) ))
  (:action dialogue-disambiguation-ask-for_spark-plug 
	   :parameters () 
	   :precondition (and 
			  (not (user_initiative)) )
	   :effect (and (labeled-oneof resolve-ask-for_spark-plug (outcome ask-for_spark-plug_initiative-switch__ (and (user_initiative)
														       ))
				       (outcome ask-for_spark-plug_help-local-options__ (and 
											 ))
				       (outcome ask-for_spark-plug_detected__check-pass_status_options-eq-found (and 
														 )) ) ))
  (:action dialogue-disambiguation-end_conversation 
	   :parameters () 
	   :precondition (and 
			  )
	   :effect (and (labeled-oneof resolve-end_conversation (outcome end_conversation-outcome-fallback__ (and (goal)
														  )) ) ))
  (:action dialogue-disambiguation-ask-for_break-pad 
	   :parameters () 
	   :precondition (and 
			  (not (user_initiative)) )
	   :effect (and (labeled-oneof resolve-ask-for_break-pad (outcome ask-for_break-pad_initiative-switch__ (and (user_initiative)
														     ))
				       (outcome ask-for_break-pad_detected__check-pass_status_options-eq-found (and 
														))
				       (outcome ask-for_break-pad_help-local-options__ (and 
											)) ) ))

  )
