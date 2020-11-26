 
:- module( domain_vcrphone, [ relevant_to_task/3,
			 relevant_to_tasks/3,
			 relevant_answer/2,
			 all_answers/2,
			 plan/2,
			 reduce/3,
			 revisable/1,
			 sysaction/1,
			 dominates/2,
			 abstract/3,
			 task/1,
			 voice/1
			] ).

:- use_module( library(vcr_languages) ).
:- use_module( library(telephone_names) ).
:- use_module( library(lists), [ member/2, select/3, append/3 ] ).


/*----------------------------------------------------------------------
     Dialogue plans

     plan( ?Name, ?Plan )
----------------------------------------------------------------------*/

default(dummy).

plan( top,
      [ findout([ task(vcr_top),
		  task(tp_top),
		  task(main_menu) ]) ] ).

plan( main_menu,
      [ findout(main_menu),
	if_then_else(main_menu,
		     [ forget,
		       change_domain(menu) ],
		     [ forget(task(_)),
		       forget(not main_menu),
		       exec(top) ]
		    ) ] ).

% VCR

plan( vcr_top,
      [ findout([ task(vcr_change_play_status),
		  task(vcr_new_program_position),
		  task(vcr_timer_recording),
		  task(vcr_settings),
		  task(vcr_query) ]) ] ).

plan( vcr_change_play_status,
      [	findout([ task(vcr_play),
		  task(vcr_stop),
		  task(vcr_ff),
		  task(vcr_rew),
		  task(vcr_pause_still),
		  task(vcr_still_adv),
		  task(vcr_rec) ]) ] ).

plan( vcr_timer_recording,
      [ findout([ task(vcr_add_program),
		  task(vcr_delete_program) ]) ] ).

plan( vcr_add_program,
      [ dev_query(vcr,available_program_slot),
	if_then_else(dev_result(vcr, available_program_slot, no),
		     [ forget,
		       inform(no_available_program_slot),
		       exec(vcr_top) ],
		     exec(vcr_add_program1)) ] ).

plan( vcr_add_program1,
      [ findout(X1^program_position_to_store(X1)),
	findout(X2^date_to_store(X2)),
	findout(X3^start_time_to_store(X3)),
	findout(X4^stop_time_to_store(X4)),
	dev_query(vcr, available_program_slot),
	if_then_else(dev_result(vcr, available_program_slot,no),
		     [ forget,
		       inform(no_available_program_slot),
		       exec(vcr_top) ],
		     [ dev_do(vcr, 'AddProgram'),
		       if_then(program_position_to_store(Position),
			if_then(date_to_store(Date),
			 if_then(start_time_to_store(Start),
			  if_then(stop_time_to_store(Stop),
				  [ forget,
				    inform(program_added(Position,Date,Start,Stop)) ] )))),
		       exec(vcr_top) ]) ] ).

plan( vcr_delete_program,
      [ dev_get(vcr, programs),
	if_then_else(dev_val(vcr,programs,[]),
		     [ forget,
		       inform(no_program_to_delete) ],
		     if_then(dev_val(vcr, programs, Programs),
			     [ inform(programs(Programs)),
			       exec(vcr_delete_program1) ]) ) ] ).

plan( vcr_delete_program1,
      [ findout(X^program_number(X)),
	if_then(program_number(N),
		[ dev_query(vcr, program_exists(N)),
		  if_then_else(dev_result(vcr, program_exists(N),yes),
			       [ dev_do(vcr, 'DeleteProgram'),
				 forget,
				 inform(program_deleted) ],
			       [ forget,
				 inform(no_such_program) ]) ]),
	exec(vcr_top) ] ).

plan( vcr_play,
      [ dev_get( vcr, play_status ),
	if_then_else( dev_val( vcr, play_status, playing ),
		      [ forget,
			inform(already_playing),
			exec(vcr_top) ],
		      [ dev_do(vcr,  'Play' ),
			dev_set( vcr, play_status, playing ),
			forget,
			inform(now_playing),
			exec(vcr_top) ] ) ] ).

plan( vcr_stop,
      [ dev_get( vcr, play_status ),
	if_then_else( dev_val( vcr, play_status, stopped ),
		      [ forget,
			inform(already_stopped) ],
		      [ dev_do( vcr, 'Stop' ),
			dev_set( vcr, play_status, stopped ),
			forget,
			inform(now_stopped) ] ),
	exec(vcr_top) ] ).

plan( vcr_ff,
      [ dev_get( vcr, play_status ),
	if_then_else( dev_val( vcr, play_status, ff ),
		      [ forget,
			inform(already_ff) ],
		      [ dev_do( vcr, 'FF' ),
			dev_set( vcr, play_status, ff ),
			forget,
			inform(now_ff) ] ),
	exec(vcr_top) ] ).

plan( vcr_rew,
      [ dev_get( vcr, play_status ),
	if_then_else( dev_val( vcr, play_status, rewinding ),
		      [ forget,
			inform(already_rewinding) ],
		      [ dev_do( vcr, 'Rew' ),
			dev_set( vcr,play_status, rewinding ),
			forget,
			inform(now_rewinding) ] ),
	exec(vcr_top) ] ).

plan( vcr_pause_still,
      [ dev_get( vcr, play_status ),
	if_then_else( dev_val( vcr, val(play_status), paused ),
		      [ dev_do( vcr, 'Play' ),
			dev_set( vcr, play_status, playing ),
			forget,
			inform(now_playing),
			exec(vcr_top) ],
		      if_then_else( dev_val( vcr, play_status, stopped ),
				    [ forget,
				      inform(already_stopped) ],
				    [ dev_do( vcr, 'PauseStill' ),
				      dev_set( vcr, play_status, paused ),
				      forget,
				      inform(now_paused) ] ) ),
	exec(vcr_top) ] ).

plan( vcr_still_adv,
      [ dev_get( vcr, play_status ),
	if_then_else( dev_val( vcr, play_status, paused ),
		      [ dev_do( vcr, 'StillAdv' ),
			forget,
			inform(advanced),
			exec(vcr_top) ],
		      [ forget,
			inform(not_paused),
			exec(vcr_top) ] ) ] ).

plan( vcr_rec,
      [ dev_get( vcr, play_status ),
	if_then_else( dev_val( vcr, play_status, recording ),
		      [ forget,
			inform(already_recording) ],
		      [ dev_do( vcr, 'Record' ),
			dev_set( vcr, play_status, recording ),
			forget,
			inform(now_recording) ] ),
	exec(vcr_top) ] ).

plan( vcr_new_program_position,
      [ findout(X^new_program_position(X)),
	case([ ( new_program_position(increase), exec(vcr_increase_program_position) ),
	       ( new_program_position(decrease), exec(vcr_decrease_program_position) ),
	       [ dev_do( vcr, 'SetProgramPosition' ),
		 if_then( new_program_position(P),
			  [ dev_set( vcr, program_position, P ),
			    forget,
			    inform( ok_new_program_position(P) ),
			    exec(vcr_top) ] ) ] ]) ] ).

plan( vcr_increase_program_position,
      [ dev_do(vcr,  'IncreaseProgramPosition' ),
	get_dev( vcr, program_position ),
	if_then( vcr_result( val(program_position), P ),
		 [ forget,
		   inform( ok_new_program_position(P) ) ]),
	exec(vcr_top) ] ).

plan( vcr_decrease_program_position,
      [ dev_do(vcr,  'DecreaseProgramPosition' ),
	dev_get( vcr, program_position ),
	if_then( vcr_result( val(program_position), P ),
		 [ forget,
		   inform( ok_new_program_position(P) ) ] ),
	exec(vcr_top) ] ).

plan( vcr_settings,
      [ findout(task(vcr_set_clock)) ] ).

plan( vcr_set_clock,
      [ findout(X^new_clock(X)),
	dev_do(vcr, 'SetClock'),
	forget,
	inform(clock_was_set),
	exec(vcr_top) ] ).

plan( vcr_query,
      [ findout([ task(vcr_query_status),
		  task(vcr_query_channel) ]) ] ).

plan( vcr_query_status, % ?x.status(x)
      [ findout(X^status(X)),
	dev_get( vcr, play_status ),
	if_then(status(X),
		if_then_else(dev_val(vcr, play_status,X),
			     [ forget,
			       inform(confirm_status(X)) ],
			     if_then(dev_val(vcr, play_status,Y),
				     [ forget,
				       inform(status_is(Y)) ]))),
	exec(vcr_top) ] ).

/*
plan( vcr_query_status_wh,		% ?x.status(x)
      [ dev_get( vcr, play_status ),
	if_then(dev_val(vcr, play_status,X)
	       inform(status_is(X))),
	forget,
	exec(vcr_top) ] ).

plan( upnp_query_wh,		% ?x.status(x)
      [ findout( X^device(X)),
	findout( V^variable(V)),
	if_then( device(Dev) and variable(Var),
		 [ dev_get( Dev, Var ),
		   if_then_else(
				dev_val( Dev, Var ,Val  ), 
				inform( answer( Dev, Var ,Val  ) ),
				
	forget,
	exec(vcr_top) ] ).

*/

plan( vcr_query_channel,
      [ dev_get(vcr, program_position ),
	if_then( dev_val( vcr, program_position, P ),
		 [ forget,
		   inform( current_program_position(P) ) ] ),
	exec(vcr_top) ] ).
	






% Telephone

plan( tp_top,
      [ findout([ task(tp_base_station),
		  task(tp_handset) ]) ] ).

  % Base station

plan( tp_base_station,
      [ findout([ task(tp_answering_machine),
		  task(tp_base_station_settings) ]) ] ).

plan( tp_answering_machine,
      [ findout([ task(tp_answering_machine_switch_on),
		  task(tp_answering_machine_switch_off) ]) ] ).

plan( tp_answering_machine_switch_on,
      [ dev_get( phone, answering_machine_onoff ),
	if_then_else(dev_val(answering_machine_onoff, on),
		     [ forget,
		       inform(answering_machine_already_on) ],
		     [ dev_do(phone, 'AnsweringMachineOn'),
		       dev_set(phone, answering_machine_onoff,on),
		       forget,
		       inform(answering_machine_now_on) ]),
	exec(tp_top) ] ).

plan( tp_answering_machine_switch_off,
      [ dev_get(phone, answering_machine_onoff),
	if_then_else(dev_val(answering_machine_onoff,off),
		     [ forget,
		       inform(answering_machine_already_off) ],
		     [ dev_do(phone, 'AnsweringMachineOff'),
		       dev_set(phone, answering_machine_onoff,off),
		       forget,
		       inform(answering_machine_now_off) ]),
	exec(tp_top) ] ).

plan( tp_base_station_settings,
      [ findout([ task(tp_base_station_settings_autoanswer),
		  task(tp_base_station_settings_basic) ]) ] ).

plan( tp_base_station_settings_autoanswer,
      [ findout([ task(tp_base_station_settings_autoanswer_on),
		  task(tp_base_station_settings_autoanswer_off) ]) ] ).

plan( tp_base_station_settings_autoanswer_on,
      [ dev_get( phone, autoanswer_onoff),
	if_then_else(dev_val( phone, autoanswer_onoff,on),
		     [ forget,
		       inform(autoanswer_already_on) ],
		     [ dev_do(phone, 'AutoAnswerOn'),
		       dev_set( vcr, autoanswer_onoff,on),
		       forget,
		       inform(autoanswer_now_on) ]),
	exec(tp_top) ] ).

plan( tp_base_station_settings_autoanswer_off,
      [ dev_get( phone, autoanswer_onoff),
	if_then_else(dev_val( phone, autoanswer_onoff,off),
		     [ forget,
		       inform(autoanswer_already_off) ],
		     [ dev_do(phone, 'AutoAnswerOff'),
		       dev_set( vcr, autoanswer_onoff,off),
		       forget,
		       inform(autoanswer_now_off) ]),
	exec(tp_top) ] ).

plan( tp_base_station_settings_basic,
      [ findout([ task(tp_base_station_settings_basic_date),
		  task(tp_base_station_settings_basic_language) ]) ] ).

plan( tp_base_station_settings_basic_language,
      [ findout(X^base_station_language(X)),
	dev_do(phone, 'SetBaseStationLanguage'),
	if_then(base_station_language(L),
		[ forget,
		  inform(base_station_language_was_set(L)),
		  exec(tp_top) ]) ] ).


plan( tp_base_station_settings_basic_date,
      [ forget,
	inform(date_cannot_be_set),
	exec(tp_top) ] ).


% Handset

plan( tp_handset,
      [ findout([ task(tp_phonebook),
		  task(tp_handset_language),
		  task(tp_handset_earpiece_volume),
		  task(tp_handset_warnings) ]) ] ).

plan( tp_handset_language,
      [ findout(X^handset_language(X)),
	dev_do(phone, 'SetHandsetLanguage'),
	if_then(handset_language(L),
		[ forget,
		  inform(handset_language_was_set(L)),
		  exec(tp_top) ]) ] ).

plan( tp_handset_warnings,
      [ findout([ task(tp_handset_warnings_volume),
		  task(tp_handset_warnings_signals) ]) ] ).

plan( tp_handset_warnings_signals,
      [ findout(X1^signal_type(X1)),
	findout(X2^tone_or_melody(X2)),
	dev_do(phone, 'SetRingerToneOrMelody'),
	if_then(signal_type(ST),
		if_then(tone_or_melody(TM),
			[ forget,
			  inform(signal_was_set(ST,TM)) ])),
	exec(tp_top) ] ).

plan( tp_handset_warnings_volume,
      [ findout(X^ring_volume(X)),
	case([ ( ring_volume(increase), exec(tp_handset_warnings_volume_increase) ),
	       ( ring_volume(decrease), exec(tp_handset_warnings_volume_decrease) ),
	       [ dev_do(phone, 'SetHandsetRingVolume'),
		 forget,
		 inform(ring_volume_was_set),
		 exec(tp_top) ] ]) ] ).

plan( tp_handset_warnings_volume_increase,
      [ dev_do(phone,  'IncreaseHandsetRingVolume' ),
	dev_get( phone, ring_volume ),
	if_then( dev_val( phone, ring_volume, V ),
		 [ forget,
		   inform( new_ring_volume(V) ) ] ),
	exec(tp_top) ] ).

plan( tp_handset_warnings_volume_decrease,
      [ dev_do(phone,  'DecreaseHandsetRingVolume' ),
	dev_get( phone, ring_volume ),
	if_then( dev_val( phone, ring_volume, V ),
		 [ forget,
		   inform( new_ring_volume(V) ) ] ),
	exec(tp_top) ] ).

plan( tp_handset_earpiece_volume,
      [ findout(X^earpiece_volume(X)),
	case([ ( earpiece_volume(increase), exec(tp_handset_earpiece_volume_increase) ),
	       ( earpiece_volume(decrease), exec(tp_handset_earpiece_volume_decrease) ),
	       [ dev_do(phone, 'SetEarpieceVolume'),
		 forget,
		 inform(earpiece_volume_was_set),
		 exec(tp_top) ] ]) ] ).

plan( tp_handset_earpiece_volume_increase,
      [ dev_do(phone,  'IncreaseEarpieceVolume' ),
	dev_get( phone, earpiece_volume ),
	if_then( dev_val( phone, earpiece_volume, V ),
		 [ forget,
		   inform( new_earpiece_volume(V) ) ] ),
	exec(tp_top) ] ).

plan( tp_handset_earpiece_volume_decrease,
      [ dev_do(phone,  'DecreaseEarpieceVolume' ),
	dev_get( phone, earpiece_volume ),
	if_then( dev_val( phone, earpiece_volume, V ),
		 [ forget,
		   inform( new_earpiece_volume(V) ) ] ),
	exec(tp_top) ] ).

  % Phonebook

plan( tp_phonebook,
      [ findout([ task(tp_phonebook_new_entry),
		  task(tp_phonebook_search_entry),
		  task(tp_phonebook_delete_entry) ]) ] ).

plan( tp_phonebook_new_entry,
      [ findout( X1^phonebook_name_to_add(X1) ),
	if_then( phonebook_name_to_add(Name),
		 [ dev_query( phone, phonebook_entry(Name) ),
		   if_then_else( dev_val( phone, phonebook_entry(Name), found(_) ),
				 [ forget,
				   inform(phonebook_entry_exists(Name)) ],
				 [ findout( X2^phonebook_number_to_add(X2) ),
				   dev_do(phone,  'AddPhonebookEntry' ),
				   forget,
				   inform(phonebook_entry_added(Name)) ] ) ] ),
	exec(tp_top) ] ).

plan( tp_phonebook_search_entry,
      [ findout( X1^phonebook_name_to_find(X1) ),
	if_then( phonebook_name_to_find(Name),
		 [ dev_query( phone, phonebook_entry(Name) ),
		   if_then_else( dev_result( phone, phonebook_entry(Name), found(Number) ),
				 [ findout( call(Number) ),
				   if_then_else( call(Number),
						 [ dev_do(phone,  'CallByPhonebookName' ),
						   forget,
						   inform( now_calling(Name) ) ],
						 forget ) ],
				 [ forget,
				   inform( phonebook_entry_not_found ) ] ) ] ),
	exec(tp_top) ] ).

plan( tp_phonebook_delete_entry,
      [ findout( X1^phonebook_entry_to_delete(X1) ),
	if_then( phonebook_entry_to_delete(Name),
		 [ dev_query( phone, phonebook_entry(Name) ),
		   if_then_else( dev_result( phone, phonebook_entry(Name), found(_) ),
				 [ dev_do(phone,  'DeletePhonebookEntry' ),
				   if_then( phonebook_entry_to_delete(Name),
					    [ forget,
					      inform(phonebook_entry_deleted(Name)) ] ) ],
				 [ forget,
				   inform( phonebook_entry_not_found ) ] )
		 ] ),
	exec(tp_top) ] ).


/*--------------------------------------------------------------
     Conceptual knowledge
----------------------------------------------------------------------*/

task( main_menu ).

settings_task(vcr_settings).
settings_task(tp_base_station_settings).

onoff_task(tp_answering_machine_switch_on,on).
onoff_task(tp_answering_machine_switch_off,off).
onoff_task(tp_base_station_settings_autoanswer_on,on).
onoff_task(tp_base_station_settings_autoanswer_off,off).

language_task(tp_base_station_settings_basic_language).
language_task(tp_handset_language).

volume_task(tp_handset_volume).
volume_task(tp_handset_warnings_volume).
volume_task(tp_handset_earpiece_volume).

status( playing ).
status( stopped ).
status( paused ).
status( ff ).
status( rewinding ).
status( recording ).

language( L ) :-
	vcr_language( L ).

signal_type( internal ).
signal_type( external ).
signal_type( message ).
signal_type( search_signal ).

tone_or_melody( low ).
tone_or_melody( medium ).
tone_or_melody( high ).
tone_or_melody( mixed ).
tone_or_melody( jumpy ).
tone_or_melody( eine_kleine_nachtmusik ).
tone_or_melody( toccata ).
tone_or_melody( elise ).
tone_or_melody( samba ).
tone_or_melody( blues_rythm ).

% Voice

voice( 8 ).

/*----------------------------------------------------------------------
     relevant_answer( -Question, +Answer )
     -- Returns (if it succeeds) a Question to which
        the Answer is relevant 
----------------------------------------------------------------------*/

relevant_answer( X^task(X), task(X) ):-
	plan( X, _ ).
relevant_answer( X^task(X), X ):-
	plan( X, _ ).

relevant_answer( X^program_position_to_store(X), program_position_to_store(X)) :-
	user:val(lexicon,L),
	L:program_position( X ).
relevant_answer( X^program_position_to_store(X), program_position(X)) :-
	user:val(lexicon,L),
	L:program_position( X ).
relevant_answer( X^program_position_to_store(X), number(X)) :-
	user:val(lexicon,L),
	L:program_position( X ).
relevant_answer( X^program_position_to_store(X), X) :-
	user:val(lexicon,L),
	L:program_position( X ).

relevant_answer( X^new_program_position(X), new_program_position(X)) :-
	user:val(lexicon,L),
	L:program_position( X ).
relevant_answer( X^new_program_position(X), program_position(X)) :-
	user:val(lexicon,L),
	L:program_position( X ).
relevant_answer( X^new_program_position(X), number(X)) :-
	user:val(lexicon,L),
	L:program_position( X ).
relevant_answer( X^new_program_position(X), X) :-
	user:val(lexicon,L),
	L:program_position( X ).
relevant_answer( X^new_program_position(X), incdec(X)).

relevant_answer( X^date_to_store(X), date_to_store(X)) :-
	user:val(lexicon,L),
	L:date(X).
relevant_answer( X^date_to_store(X), number(X)) :-
	user:val(lexicon,L),
	L:date(X).
relevant_answer( X^date_to_store(X), X) :-
	user:val(lexicon,L),
	L:date(X).

relevant_answer( X^start_time_to_store(X), start_time_to_store(X)) :-
	user:val(lexicon,L),
	L:time(X).
relevant_answer( X^start_time_to_store(X), number(X)) :-
	user:val(lexicon,L),
	L:time(X).
relevant_answer( X^start_time_to_store(X), X) :-
	user:val(lexicon,L),
	L:time(X).

relevant_answer( X^stop_time_to_store(X), stop_time_to_store(X)) :-
	user:val(lexicon,L),
	L:time(X).
relevant_answer( X^stop_time_to_store(X), number(X)) :-
	user:val(lexicon,L),
	L:time(X).
relevant_answer( X^stop_time_to_store(X), X) :-
	user:val(lexicon,L),
	L:time(X).

relevant_answer( X^program_number(X), program_number(X)) :-
	user:val(lexicon,L),
	L:program_number( X ).
relevant_answer( X^program_number(X), number(X)) :-
	user:val(lexicon,L),
	L:program_number( X ).
relevant_answer( X^program_number(X), X) :-
	user:val(lexicon,L),
	L:program_number( X ).

relevant_answer( X^new_clock(X), new_clock(X)) :-
	user:val(lexicon,L),
	L:time(X).
relevant_answer( X^new_clock(X), number(X)) :-
	user:val(lexicon,L),
	L:time(X).
relevant_answer( X^new_clock(X), X) :-
	user:val(lexicon,L),
	L:time(X).

relevant_answer( X^status(X), status(X)) :-
	status(X).
relevant_answer( X^status(X), X) :-
	status(X).

relevant_answer( X^base_station_language(X), base_station_language(X)) :-
	language(X).
relevant_answer( X^base_station_language(X), language(X)) :-
	language(X).
relevant_answer( X^base_station_language(X), X) :-
	language(X).

relevant_answer( X^handset_language(X), handset_language(X)) :-
	language(X).
relevant_answer( X^handset_language(X), language(X)) :-
	language(X).
relevant_answer( X^handset_language(X), X) :-
	language(X).

relevant_answer( X^ring_volume(X), ring_volume(X)) :-
	user:val(lexicon,L),
	L:ring_volume(X).
relevant_answer( X^ring_volume(X), number(X)) :-
	user:val(lexicon,L),
	L:ring_volume(X).
relevant_answer( X^ring_volume(X), X) :-
	user:val(lexicon,L),
	L:ring_volume(X).
relevant_answer( X^ring_volume(X), incdec(X) ).

relevant_answer( X^earpiece_volume(X), earpiece_volume(X)) :-
	user:val(lexicon,L),
	L:earpiece_volume(X).
relevant_answer( X^earpiece_volume(X), number(X)) :-
	user:val(lexicon,L),
	L:earpiece_volume(X).
relevant_answer( X^earpiece_volume(X), X) :-
	user:val(lexicon,L),
	L:earpiece_volume(X).
relevant_answer( X^earpiece_volume(X), incdec(X)).

relevant_answer( X^signal_type(X), signal_type(X)) :-
	signal_type(X).
relevant_answer( X^signal_type(X), X) :-
	signal_type(X).

relevant_answer( X^tone_or_melody(X), tone_or_melody(X)) :-
	tone_or_melody(X).
relevant_answer( X^tone_or_melody(X), X) :-
	tone_or_melody(X).

relevant_answer( X^phonebook_name_to_add(X), phonebook_name_to_add(X)) :-
	lex_name(X).
relevant_answer( X^phonebook_name_to_add(X), name(X)).
relevant_answer( X^phonebook_name_to_add(X), X) :-
	lex_name(X).

relevant_answer( X^phonebook_number_to_add(X), phonebook_number_to_add(X)) :-
	numberAtom(X).
relevant_answer( X^phonebook_number_to_add(X), number(X)).
relevant_answer( X^phonebook_number_to_add(X), X) :-
	atom(X),
	numberAtom(X).

numberAtom(A):-
	atom_chars(A,C),
	number_chars(N,C),
	number(N).


relevant_answer( X^phonebook_name_to_find(X), phonebook_name_to_find(X)) :-
	lex_name(X).
relevant_answer( X^phonebook_name_to_find(X), name(X)).
relevant_answer( X^phonebook_name_to_find(X), X) :-
	lex_name(X).

relevant_answer( X^phonebook_entry_to_delete(X), phonebook_entry_to_delete(X)) :-
	lex_name(X).
relevant_answer( X^phonebook_entry_to_delete(X), name(X)).
relevant_answer( X^phonebook_entry_to_delete(X), X) :-
	lex_name(X).

%relevant_answer( Tasks, call) :-
%	member( task(tp_phonebook_search_entry), Tasks ).
	      
/***** GENERAL GODIS STUFF ******/


%%% Yes/no question P?

% yes
relevant_answer( Q, yes) :-
	ynq(Q).
% no
relevant_answer( Q, no) :-
	ynq(Q).
% P
relevant_answer( Q, P ) :-
	ynq(Q),
	P = Q.
% P
relevant_answer( Q, not(P) ) :-
	ynq(Q),
	P = Q.

% Alt-questions, full answer
relevant_answer( AltList, Alt ):-
	member( Alt, AltList ). 

relevant_answer( TaskList, task(T) ):-
	member( task(T1), TaskList ),
	dominates( T1, T ).

% Alt-questions, specific

relevant_answer( AltList, task_type(settings) ) :-
	member( task(X), AltList ),
	settings_task(X).

relevant_answer( AltList, onoff(OnOff) ) :-
	member( task(X), AltList ),
	onoff_task(X,OnOff).

relevant_answer( AltList, task_type(volume) ) :-
	member( task(X), AltList ),
	volume_task(X).

relevant_answer( AltList, task_type(language) ) :-
	member( task(X), AltList ),
	language_task(X).

%%% Definition of yes/no questions

ynq( YNQ ):-
	\+ YNQ = _^_,		% whq
	\+ YNQ = [_|_].		% altq


/*----------------------------------------------------------------------
     relevant_to_task( +Move, -Task, -Plan )
     -- Returns (if it succeeds) a Plan to which Move is relevant
----------------------------------------------------------------------*/

relevant_to_task( Move, vcr_add_program, Plan):-
	relevant_to_task1( Move, vcr_add_program1, Plan ).
relevant_to_task( Move, Task, Plan ):-
	relevant_to_task1( Move, Task, Plan ),
	\+ Task = vcr_add_program1.

relevant_to_task1( Move, Task, Plan ):-
	plan( Task, Plan ),
	Move = answer( A ),
	member( findout( Q ), Plan ),
	relevant_answer( Q, A ).

% gives all elliptical answers
all_answers( Q, As ):-
	setof( A, relevant_elliptical_answer( Q, A ), As ).

relevant_elliptical_answer( Q, A ):-
	relevant_answer( Q, A ),
	atom( A ).

implied_question( FindOuts, Question ) :-
	FindOuts = [ _ | _ ],
	member( F, FindOuts ),
	implied_question( F, Question ).

implied_question( _^Q, Q ).
implied_question( Q, Q ).

/*----------------------------------------------------------------------
     relevant_to_tasks( +Move, -Tasks )
     -- Returns (if it succeeds) a list of tasks Tasks to which Move is
     relevant; all elements in Task have the form task(T)
----------------------------------------------------------------------*/

relevant_to_task( Move, Task ):-
	plan( Task, Plan ),
	Move = answer( A ),
	member( findout( Q ), Plan ),
	relevant_answer( Q, A ).

relevant_to_tasks( Move, Tasks ):-
	setof( task(Task), relevant_to_task( Move, Task ), Tasks ).


/*----------------------------------------------------------------------
   dominates( T1, T2 )
   -- Task T1 dominates T2 in the menu hierarchy
----------------------------------------------------------------------*/

dominates0( T1, T2 ):-
	plan( T1, [ findout( Ts ) | _ ] ),
	member( task( T2 ), Ts ).

dominates0( T1, T2 ):-
	plan( T1, [ findout( T ) | _ ] ),
	task( T2 ) =  T.


dominates( T1, T2 ):-
	dominates0( T1, T2 ).

dominates( T1, T3 ):-
	dominates0( T1, T2 ),
	dominates( T2, T3 ).
	



/*----------------------------------------------------------------------
   reduce( +Q, +A, -P )

   -- reduces a quesition and an answer to a proposition

   ** this should perhaps be in the definition of the datatypes "question"
   and "answer", as an operation which takes a question and an answer
   and yields a question
----------------------------------------------------------------------*/


/*
** Y/N-questions
*/

reduce( Q, yes, P ):-
	ynq( Q ),
	P = Q. 

reduce( Q, no, P ):-
	ynq( Q ),
	P = not( Q ).

reduce( Q, P, P ):-
	ynq( Q ),
	P = Q.

reduce( Q, not(P), not(P) ):-
	ynq( Q ),
	P = Q.

/*
** Alt-questions
*/

% findout([ A1, A2, ..., An ]) is answered by A1 or A2 or .... or An

reduce(AltList, A, P):-
	member(A, AltList),!,
	P = A.

% findout([Pred(_),...]) is answered by Pred(_)

reduce(AltList, A, P):-
	AltList = [ A0 | _ ],
	A0 =.. [ Pred | _ ],
	A =.. [ Pred | _ ],!,
	P = A.

% specific alternatives

reduce( AltList, task_type(settings), task(X) ) :-
	member( task(X), AltList ),
	settings_task(X).

reduce( AltList, task_type(volume), task(X) ) :-
	member( task(X), AltList ),
	volume_task(X).

reduce( AltList, task_type(language), task(X) ) :-
	member( task(X), AltList ),
	language_task(X).

reduce( AltList, onoff(OnOff), task(X) ) :-
	member( task(X), AltList ),
	onoff_task(X,OnOff).

%reduce( AltList, call, task(X) ) :-
%	X = tp_phonebook_search_entry,
%	member( task(X), AltList ).

/*
** WH-questions
*/

% X^Q is answered by an atom (ellipsis)

reduce(X^P, X, P):-
	atom(X).

% X^Q is answered by full proposition

% specific questions

reduce( X^program_position_to_store(X), number(X), program_position_to_store(X) ).
reduce( X^program_position_to_store(X), program_position(X), program_position_to_store(X) ).
reduce( X^new_program_position(X), number(X), new_program_position(X) ).
reduce( X^new_program_position(X), program_position(X), new_program_position(X) ).
reduce( X^new_program_position(X), incdec(X), new_program_position(X) ).
reduce( X^date_to_store(X), number(X), date_to_store(X) ).
reduce( X^start_time_to_store(X), number(X), start_time_to_store(X) ).
reduce( X^stop_time_to_store(X), number(X), stop_time_to_store(X) ).
reduce( X^program_number(X), number(X), program_number(X) ).
reduce( X^new_clock(X), number(X), new_clock(X) ).
reduce( X^base_station_language(X), language(X), base_station_language(X) ).
reduce( X^handset_language(X), language(X), handset_language(X) ).
reduce( X^ring_volume(X), number(X), ring_volume(X) ).
reduce( X^ring_volume(X), incdec(X), ring_volume(X) ).
reduce( X^earpiece_volume(X), number(X), earpiece_volume(X) ).
reduce( X^earpiece_volume(X), incdec(X), earpiece_volume(X) ).
reduce( X^phonebook_name_to_add(X), name(X), phonebook_name_to_add(X) ).
reduce( X^phonebook_name_to_find(X), name(X), phonebook_name_to_find(X) ).
%reduce( X^phonebook_name_to_find(X), name_to_call(X), phonebook_name_to_find(X) ).
reduce( X^phonebook_entry_to_delete(X), name(X), phonebook_entry_to_delete(X) ).
reduce( X^phonebook_number_to_add(X), number(X), phonebook_number_to_add(X) ).
reduce( call(X), call, call(X) ).

% question of form X^P(Y,X)

reduce( X^Q, A, P ):-
	Q =.. [Y,_,X],
	A =.. [Y,_,X],
	P = Q,
	!.

% question of form X^P(X)

reduce( X^Q, A, P ):-
	Q =.. [Y,X],
	A =.. [Y,X],
	P = Q,
	!.

% abstract(+A,+P,-Q)
%
% Q is a question s.t. reduce(Q,A,P) holds

% abstract( A, P, _^P ).
abstract( A, P, X^Q ):-
	A =.. [Y,_],
	P =.. [Y,_],
	Q =.. [Y,X],
%	P = Q,
	!.

sysaction( dummy ).
