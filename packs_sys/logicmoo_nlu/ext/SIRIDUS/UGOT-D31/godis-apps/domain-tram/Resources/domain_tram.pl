 
/*************************************************************************

         name: domain_tram.pl 
  description: Tram domain file
 
*************************************************************************/

:- module( domain_tram, [ resource_of_type/1,
			 plan/2,
			 issue/2,
			 sort_restr/1,
			 isa/2,
			 postcond/2,
			 depends/2,
			 incompatible/2
			  ] ).

resource_of_type(domain).


:- discontiguous output_form/2, input_form/2, plan/2, postcond/2, incompatible/2.
:- use_module( library(lists), [ member/2, select/3, append/3 ] ).

:- ensure_loaded( library( semsort_tram ) ).

:- use_module( library(vcr_languages) ).
:- use_module( library(telephone_names) ).


/*----------------------------------------------------------------------
     Dialogue plans

     plan( ?ActionOrIssue, ?Plan )
----------------------------------------------------------------------*/

incompatible(P,P):-singleton(P).
singleton(start_time_to_store).
singleton(stop_time_to_store).

default_question(dummy).
incompatible(dummy,dummy).
% menus

plan( top,
      [ forget_all,
	raise( X^action(X) ),
	findout( set( [ action(tram_times),
			action(set_mode) ] ) ) ] ).
postcond( top, none ).

plan( up, [] ).

plan( tram_times, 
      [ raise( X^action-trtimes(X) ),
        findout(X^dep_stop(X)),
	findout(X^arr_stop(X)),
	raise( X^informtime(X))
      ] ).
postcond( tram_times, none ).

plan( set_mode,
      [ raise( X^action-setmode(X) ),
	findout( set( [ action(re_speech),
			action(re_graph),
			action(re_speech_and_graph),
			action(predef_mode) ] ) )
      ] ).
postcond( set_mode, none ).

plan( re_speech,
      [ raise( X^speech_val(X) ),
	findout( set( [ speech_val(no),
			speech_val(min),
			%speech_val(interm),
			speech_val(max),
			speech_val(ground),
			speech_val(compl),
			speech_val(indet) ] ) ),
	set_speech_value,
	raise( X^action2(X) )
      ] ).
postcond( re_speech, done(re_speech) ).

plan( re_graph,
      [ raise( X^graph_val(X) ),
	findout( set( [ graph_val(no),
			graph_val(min),
			%graph_val(interm),
			graph_val(max),
			graph_val(ground),
			graph_val(compl),
			graph_val(indet) ] ) ),
	set_graph_value,
	raise( X^action2(X) )
      ] ).
postcond( re_graph, done(re_graph) ).

plan( re_speech_and_graph,
      [ raise( X^speech_val(X) ),
	findout( set( [ speech_val(no),
			speech_val(min),
			%speech_val(interm),
			speech_val(max),
			speech_val(ground),
			speech_val(compl),
			speech_val(indet) ] ) ),
	set_speech_value,
	raise( X^graph_val(X) ),
	findout( set( [ graph_val(no),
			graph_val(min),
			%graph_val(interm),
			graph_val(max),
			graph_val(ground),
			graph_val(compl),
			graph_val(indet) ] ) ),
	set_graph_value,
	raise( X^action2(X) )
      ] ).
postcond( re_speech_and_graph, done(re_speech) ).
postcond( re_speech_and_graph, done(re_graph) ).


plan( predef_mode,
      [ raise( X^predefmode(X)),
	findout( set( [ action(mode_only_speech1),
			action(mode_only_speech2),
			action(mode_only_graph),
			action(mode_both_speech_and_graph) ] ) )
      ] ).
postcond( predef_mode, none ).

plan( mode_only_speech1,
      [ mode_only_speech1 ] ).
postcond( mode_only_speech1, done(mode_only_speech1) ).

plan( mode_only_speech2,
      [ mode_only_speech2 ] ).
postcond( mode_only_speech2, done(mode_only_speech2) ).

plan( mode_only_graph,
      [ mode_only_graph ] ).
postcond( mode_only_graph, done(mode_only_graph) ).

plan( mode_both_speech_and_graph,
      [ mode_both_speech_and_graph ] ).
postcond( mode_both_speech_and_graph, done(mode_both_speech_and_graph) ).


      
plan( vcr_change_play_status,
      [	findout( set( [ action(vcr_play),
			action(vcr_stop),
			action(vcr_ff),
			action(vcr_rew),
			action(vcr_pause_still),
%			action(vcr_still_adv),
			action(vcr_rec) ]) ) ] ).
postcond( vcr_change_play_status, done(vcr_play) ).
postcond( vcr_change_play_status, done(vcr_stop) ).
postcond( vcr_change_play_status, done(vcr_ff) ).
postcond( vcr_change_play_status, done(vcr_rew) ).
postcond( vcr_change_play_status, done(vcr_pause_still) ).
postcond( vcr_change_play_status, done(vcr_rec) ).


plan( vcr_play,
      [ dev_do(vcr,  'Play' ),
	dev_set( vcr, play_status, playing ) ] ).
postcond( vcr_play, done( 'Play' ) ).

plan( vcr_stop,
      [ dev_do( vcr, 'Stop' ),
	dev_set( vcr, play_status, stopped ) ] ).
postcond( vcr_stop, done( 'Stop' ) ).

plan( vcr_ff,
      [ dev_do( vcr, 'FF' ),
	dev_set( vcr, play_status, ff ) ] ).
postcond( vcr_ff, done( 'FF' ) ).

plan( vcr_rew,
      [ dev_do( vcr, 'Rew' ),
	dev_set( vcr,play_status, rewinding ) ] ).
postcond( vcr_rew, done( 'Rew' ) ).


plan( vcr_pause_still,
      [ dev_do( vcr, 'PauseStill' ),
	dev_set( vcr, play_status, paused ) ] ).
postcond( vcr_pause_still, done( 'PauseStill' ) ).

/*
plan( vcr_pause_still,
      [ dev_get( vcr, play_status ),
	if_then_else( play_status(paused) ),
		      [ dev_do( vcr, 'Play' ),
			dev_set( vcr, play_status, playing ) ],
		      if_then_else(  play_status(stopped) ),
				    [  ],
				    [ dev_do( vcr, 'PauseStill' ),
				      dev_set( vcr, play_status, paused ) ] ] ).
*/

/*
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
*/

plan( vcr_rec,
      [ dev_do( vcr, 'Record' ),
	dev_set( vcr, play_status, recording ) %???
      ] ).
postcond( vcr_rec, done( 'Record' ) ).

plan( vcr_new_channel,
       [ findout(X^new_channel(X)),
	 dev_do( vcr, 'SetChannel' ) ] ).
postcond( vcr_new_channel, done( 'SetChannel' ) ).

plan( vcr_timer_recording,
      [ findout( set( [ action(vcr_add_program),
			action(vcr_delete_program) ]) ) ] ).
postcond( vcr_timer_recording, done( vcr_add_program ) ).
postcond( vcr_timer_recording, done( vcr_delete_program ) ).

/*
plan( vcr_add_program,
      [ dev_query(vcr,available_program_slot),
	if_then_else(dev_result(vcr, available_program_slot, no),
		     inform(no_available_program_slot),
		     exec(vcr_add_program1)) ] ).
*/

% precondition: some program is free
plan( vcr_add_program,
      [ findout(X1^channel_to_store(X1)),
	findout(X2^date_to_store(X2)),
	findout(X3^start_time_to_store(X3)),
	findout(X4^stop_time_to_store(X4)),
	dev_do(vcr, 'AddProgram') ] ).
postcond( vcr_add_program, done( 'AddProgram' ) ).



/*
	if_then(program_position_to_store(Position),
		if_then(date_to_store(Date),
			if_then(start_time_to_store(Start),
				if_then(stop_time_to_store(Stop),
					confirm(program_added(Position,Date,Start,Stop)) ] ))))) ] ).
*/
/*
plan( vcr_delete_program,
      [ dev_get(vcr, programs),
	if_then_else(dev_val(vcr,programs,[])),
		     [ forget,
		       inform(no_program_to_delete) ],
		     if_then(dev_val(vcr, programs, Programs),
			     [ inform(programs(Programs)),
			       exec(vcr_delete_program1) ]) ] ).
*/

% precondition: there is some program
/*plan( vcr_delete_program,
      [ findout(X^program_to_delete(X)),
	if_then( program_to_delete(N),
		[ dev_query(vcr, program_exists(N) ),
		  %if_then_else(dev_result(vcr, program_exists(N), yes),
		  if_then_else( program_exists(N),
			       dev_do(vcr, 'DeleteProgram'),
%			       icm:acc*neg:action(vcr_delete_program),
			       report( 'DeleteProgram', failed(no_such_program) )
			       %disconfirm( vcr_delete_program, no_such_program )
			      ) ] ) ] ).
*/
plan( vcr_delete_program,
      [ findout(X^program_to_delete(X)),
	dev_do(vcr, 'DeleteProgram') ] ).
postcond( vcr_delete_program, done( 'DeleteProgram' ) ).
postcond( vcr_delete_program, status( 'DeleteProgram', failed(_) ) ).


plan( vcr_settings,
       [ findout(action(vcr_set_clock)) ] ).
postcond( vcr_settings, done( 'SetClock' ) ).
% to avoid looping caused by single choice in settings menu
postcond( vcr_settings, (not action( vcr_set_clock )) ).


plan( vcr_set_clock,
      [ findout(X^new_clock(X)),
	dev_do(vcr, 'SetClock') ] ).
postcond( vcr_set_clock, done( 'SetClock' ) ).



% DIRECT COMMANDS

plan( vcr_increase_channel,
       [ dev_do(vcr,  'IncreaseChannel' )] ).

plan( vcr_decrease_channel,
      [ dev_do(vcr,  'DecreaseChannel' ) ] ).



% plans

% what's the vcr doing?

plan( X^play_status(X),
       [ dev_get( vcr, play_status ) ] ).
     

% is the vcr playing / paused / stopped / ...
%plan( play_status( Status ),
%       [ dev_get( vcr, play_status ) ] ).

%plan( dev_val( vcr, Parameter, Value ),
%        [ dev_get( vcr, Parameter ) ] ).

plan( play_status( Status ),
       [ dev_query( vcr, play_status(Status) ) ] ).


% what channel is on?

plan( X^current_channel(X),		
     [ dev_get(vcr, current_channel ) ] ).

	


plan( change_language,
      [ raise(X^language(X)),
	findout(set([language(english), language(svenska)])),
	change_language ] ).
postcond( change_language, done(change_language) ).


% SL021125
plan( change_domain,
      [ raise(X^domain(X)),
	findout(set([domain(vcr), domain(telephone)])),
	change_domain ] ).
postcond( change_domain, done(change_domain) ).


depends( X^channel_to_store(X), Y^current_channel(Y) ).

/*--------------------------------------------------------------
     Conceptual knowledge
----------------------------------------------------------------------*/

% sortal restrictions; determine relevance and resolvement

sort_restr( language( X ) ) :- sem_sort( X, language ).
sort_restr( domain( X ) ) :- sem_sort( X, domain ).% SL021125

%Tram
sort_restr( dep_stop( X ) ) :- sem_sort( X, stop ).
sort_restr( arr_stop( X ) ) :- sem_sort( X, stop ).
sort_restr( speech_val( X ) ) :- sem_sort( X, info_val ).
sort_restr( graph_val( X ) ) :- sem_sort( X, info_val ).
sort_restr( info_val( X ) ) :- sem_sort( X, info_val ).

sort_restr( new_channel( X ) ) :- sem_sort( X, number ).
sort_restr( current_channel( X ) ) :- sem_sort( X, number ).

sort_restr( channel_to_store( X ) ) :- sem_sort( X, number ).
sort_restr( start_time_to_store( X ) ) :- sem_sort( X, time ).
sort_restr( stop_time_to_store( X ) ) :- sem_sort( X, time ).
sort_restr( date_to_store( X ) ) :- sem_sort( X, date ).

sort_restr( new_clock( X ) ) :- sem_sort( X, time ).

sort_restr( play_status( X ) ) :- sem_sort( X, play_status ).

sort_restr( program_to_delete( X ) ) :- sem_sort( X, number ).

sort_restr( number( X ) ) :- sem_sort( X, number ).
sort_restr( channel( X ) ) :- sem_sort( X, channel ).
sort_restr( date( X ) ) :- sem_sort( X, date ).
sort_restr( time( X ) ) :- sem_sort( X, time ).
sort_restr( program( X ) ) :- sem_sort( X, program ).

% negation

sort_restr( not P ) :- sort_restr( P ).

% action

sort_restr( action( X ) ) :- sem_sort( X, action ).
sort_restr( action( respond(Q) ) ) :- sort_restr( issue(Q) ).


% issue

sort_restr( issue(Q) ):- plan( Q, _ ),
	\+ sort_restr( action( Q ) ).

sort_restr( issue(Q) ):-
	plan( _, Plan ),
	member( findout(Q), Plan ),
	\+ sort_restr( action( Q ) ).

% metaissue

sort_restr( und(_DP*P) ):- sort_restr(P).

% sloppy, but allows "no" as answer to clarification alt-q
% could be replaced by general rule saying that not(set([p1, ..., ])) is sortally correct,
% menaing the same as (not P1 and not p2 and...)

sort_restr( not und(_DP*set(_))).

% parameter validity; determines acceptance

valid_parameter( language( X ) ) :- sem_sort( X, language ).
valid_parameter( domain( X ) ) :- sem_sort( X, domain ). %SL021125

%Tram
valid_parameter( dep_stop( X ) ) :- sem_sort( X, stop ).
valid_parameter( arr_stop( X ) ) :- sem_sort( X, stop ).
valid_parameter( speech_val( X ) ) :- sem_sort( X, info_val ).
valid_parameter( graph_val( X ) ) :- sem_sort( X, info_val ).
valid_parameter( info_val( X ) ) :- sem_sort( X, info_val ).

valid_parameter( new_channel(N) ):- sem_sort( N, channel ).
valid_parameter( channel_to_store(N) ):- sem_sort( N, channel ).
valid_parameter( current_channel(N) ):- sem_sort( N, channel ).


valid_parameter( start_time_to_store(N) ):- sem_sort( N, time ).
valid_parameter( stop_time_to_store(N) ):- sem_sort( N, time ).

valid_parameter( date_to_store( X ) ) :- sem_sort( X, date ).

valid_parameter( new_clock( X ) ) :- sem_sort( X, time ).

valid_parameter( play_status( X ) ) :- sem_sort( X, play_status ).

%valid_parameter( program_to_delete( X ) ) :- sem_sort( X, program ).



	
