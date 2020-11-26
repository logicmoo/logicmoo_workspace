(define (domain test_domain)
    (:requirements :strips)
  (:predicates 
   (goal)
   (have_spark_plug_status)
   (user_initiative)
   )

  (:action dialogue-disambiguation-ask-for_oil-level 
	   :parameters () 
	   :precondition (and 
			  (not (user_initiative)) )
	   :effect (and (labeled-oneof resolve-ask-for_oil-level (outcome ask-for_oil-level_detected__check-oil_status-eq-found (and 
																 ))
				       (outcome ask-for_oil-level_help-local-options__ (and 
											))
				       (outcome ask-for_oil-level_initiative-switch__ (and (user_initiative)
											   )) ) ))
  (:action dialogue-disambiguation-end_conversation 
	   :parameters () 
	   :precondition (and (have_spark_plug_status)
			      )
	   :effect (and (labeled-oneof resolve-end_conversation (outcome end_conversation-outcome-fallback__ (and (goal)
														  )) ) ))
  (:action dialogue-disambiguation-ask-for_break-pad 
	   :parameters () 
	   :precondition (and 
			  (not (user_initiative)) )
	   :effect (and (labeled-oneof resolve-ask-for_break-pad (outcome ask-for_break-pad_initiative-switch__ (and (user_initiative)
														     ))
				       (outcome ask-for_break-pad_help-local-options__ (and 
											))
				       (outcome ask-for_break-pad_detected__check-pass_status_options-eq-found (and 
														)) ) ))
  (:action dialogue-disambiguation-ask-for_spark-plug 
	   :parameters () 
	   :precondition (and 
			  (not (user_initiative))
			  (not (have_spark_plug_status)) )
	   :effect (and (labeled-oneof resolve-ask-for_spark-plug (outcome ask-for_spark-plug_initiative-switch__ (and (user_initiative)
														       ))
				       (outcome ask-for_spark-plug_help-local-options__ (and 
											 ))
				       (outcome ask-for_spark-plug_detected__check-pass_status_options-eq-found (and (have_spark_plug_status)
														     )) ) ))
  (:action dialogue-disambiguation-ask-for_clutch-seal-tightness 
	   :parameters () 
	   :precondition (and 
			  (not (user_initiative)) )
	   :effect (and (labeled-oneof resolve-ask-for_clutch-seal-tightness (outcome ask-for_clutch-seal-tightness_help-local-options__ (and 
																	  ))
				       (outcome ask-for_clutch-seal-tightness_detected__check-clutch_seal_tightness_status-eq-found (and 
																     ))
				       (outcome ask-for_clutch-seal-tightness_initiative-switch__ (and (user_initiative)
												       )) ) ))

  )
