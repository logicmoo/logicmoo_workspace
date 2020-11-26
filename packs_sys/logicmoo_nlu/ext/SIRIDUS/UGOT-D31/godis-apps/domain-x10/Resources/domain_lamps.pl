/*************************************************************************

         name: domain_lamps.pl 
	 date: 2004-11-23
       author: Andreas Wallentin
 
*************************************************************************/

:- module( domain_lamps, [ plan/2,
			   issue/2,
			   sort_restr/1,
			   isa/2,
			   postcond/2,
			   depends/2
			 ] ).

:- discontiguous plan/2, postcond/2.
:- use_module( library(lists), [member/2] ).
:- ensure_loaded( [semsort_lamps] ).


resource_of_type(domain).


default_question(dummy).

plan( top,
      [ forget_all,
	raise(X^action(X)),
	findout( set([ action(lamps_turn_on),
		       action(lamps_turn_off),
		       action(add_lamp),
		       action(remove_lamp)
		     ])
	       )
      ]).
postcond( top, none ).

plan( lamps_restart,
      [ forget_all ] ).
postcond( lamps_restart, none ).


plan( lamps_turn_on,
      [ findout(L^lamp_to_turn_on(L)),
	if_then(lamp_to_turn_on(Lamp),
		[ dev_query(lamps,lamp_on_exists(Answer)),
	%%% chk if there exists such a lamp
		  if_then_else( lamp_on_exists(Answer),
				[dev_query(lamps,current_lamp_off(Off)),
			%%% it must be off to be turned on
				 if_then_else( current_lamp_off(Off),
					       [dev_do(lamps,'TurnOn')],
					       [dev_query(lamps,current_lamp(Lamp)),
						report('TurnOn',failed(Lamp))]
					     )
				],
				[dev_query(lamps,current_lamp(Lamp)),
				 report('Exist',failed(Lamp))]
			      )
		]) %%end if_then
      ] ).
postcond( lamps_turn_on, done('TurnOn') ).
postcond( lamps_turn_on, status('TurnOn',failed(_)) ).
postcond( lamps_turn_on, status('Exist',failed(_)) ).

plan( lamps_turn_off,
      [ findout(L^lamp_to_turn_off(L)),
	if_then(lamp_to_turn_off(Lamp),
		[ dev_query(lamps,lamp_off_exists(Answer)),
	%%% chk if there exists such a lamp
		  if_then_else( lamp_off_exists(Answer),
				[dev_query(lamps,current_lamp_off(Off)),
			%%% it must be off to be turned on
				 if_then_else( current_lamp_off(Off),
					       [dev_query(lamps,current_lamp(Lamp)),
						report('TurnOff',failed(Lamp))],
					       [dev_do(lamps,'TurnOff')]
					     )
				],
				[dev_query(lamps,current_lamp(Lamp)),
				 report('Exist',failed(Lamp))]
			      )
		]) %%end if_then
      ] ).
postcond( lamps_turn_off, done('TurnOff') ).
postcond( lamps_turn_off, status('TurnOff',failed(_)) ).
postcond( lamps_turn_off, status('Exist',failed(_)) ).

%%%  kolla så att det finns lampor
%%% ex: dev_query(any_lamps(Svar))
plan( all_lamps_on,
      [ dev_query(lamps,any_lamps),
	if_then_else( any_lamps,
		      [dev_do(lamps,'AllOn')],
		      [report('AnyLamps',failed)]
		    )
      ] ).
postcond( all_lamps_on, done('AllOn') ).
postcond( all_lamps_on, status('AnyLamps',failed) ).

plan( all_lamps_off,
      [ dev_query(lamps,any_lamps),
	if_then_else( any_lamps,
		      [dev_do(lamps,'AllOff')],
		      [report('AnyLamps',failed)]
		    )
      ] ).
postcond( all_lamps_off, done('AllOff') ).
postcond( all_lamps_off, status('AnyLamps',failed) ).


plan( add_lamp,
      [findout(L^lamp_to_add(L)),
       if_then(lamp_to_add(Lamp),
	       [ dev_query(lamps,lamp_add_exists(Answer)),
       %%% if lamp sort exists, do not plug in another
		 if_then_else( lamp_add_exists(Answer),
			       [dev_query(lamps,current_lamp(Lamp)),
				report('Single',failed(Lamp))],
			       [dev_do(lamps,'AddLamp')]
			     )
	       ])
      ] ).
postcond( add_lamp, done('AddLamp') ).
postcond( add_lamp, status('Single',failed(_)) ).


plan( remove_lamp,
      [findout(L^lamp_to_remove(L)),
       if_then(lamp_to_remove(Lamp),
	       [ dev_query(lamps,lamp_remove_exists(Answer)),
       %%% if lamp sort exists, do not plug in another
		 if_then_else( lamp_remove_exists(Answer),
			       [dev_do(lamps,'RemoveLamp')],
			       [dev_query(lamps,current_lamp(Lamp)),
				report('Single',failed(Lamp))]
			     )
	       ])
      ] ).      
postcond( remove_lamp, done('RemoveLamp') ).
postcond( remove_lamp, status('Exist',failed(_)) ).


%%% questions
%%% checking status and so forth
plan( Lamps^available_lamps(Lamps),
      [ dev_query(lamps,available_lamps(Lamps)) ] ).

plan( Lamps^lamps_on(Lamps),
      [ dev_query(lamps,lamps_on(Lamps)) ] ).

plan( Lamps^lamps_off(Lamps),
      [ dev_query(lamps,lamps_off(Lamps)) ] ).

plan( Lamp^is_lit(Lamp),
      [ dev_query(lamps,is_lit(Lamp)) ] ).
      
plan( Lamp^is_off(Lamp),
      [ dev_query(lamps,is_off(Lamp)) ] ).


depends(dummy,dummy).


/*--------------------------------------------------------------
     Conceptual knowledge
----------------------------------------------------------------------*/

% sortal restrictions; determine relevance and resolvement
% all POSSIBLE propositions, see also valid_parameter/1

sort_restr( domain( X ) ) :-          sem_sort( X, domain ).% SL021125
sort_restr( not P ) :-                sort_restr( P ).
sort_restr( action( X ) ) :-          sem_sort( X, action ).
sort_restr( action( respond(Q) ) ):-  sort_restr( issue(Q) ).

sort_restr( lamp_to_turn_on(X) ):-    is_lamp( X ).
sort_restr( lamp_to_turn_off(X) ):-   is_lamp( X ).
sort_restr( lamp_to_add(X) ):-        is_lamp( X ).
sort_restr( lamp_to_remove(X) ):-     is_lamp( X ).
sort_restr( lamp(X) ):-               is_lamp( X ).

%%% for questions
sort_restr( available_lamps(X) ):-    atomic(X).
sort_restr( lamps_on(X) ):-           atomic(X).
sort_restr( lamps_off(X) ):-          atomic(X).
sort_restr( is_lit(X) ):-             atomic(X).
sort_restr( is_off(X) ):-             atomic(X).

%% "ren" databassökning
% issues
sort_restr( issue(Q) ):-
	plan( Q, _ ),
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


%%% parameter validity; determines acceptance
%%% checks all ACTUAL props in DB

valid_parameter( domain( X ) ) :-           sem_sort( X, domain ).
valid_parameter( action(X) ) :-             sem_sort( X, action ).
valid_parameter( not P ) :-                 valid_parameter( P ).
valid_parameter( action( respond(Q) ) ) :-  valid_parameter( issue(Q) ).

valid_parameter( lamp_to_turn_on(X) ):-     sem_sort( X, lamp ).
valid_parameter( lamp_to_turn_off(X) ):-    sem_sort( X, lamp ).
valid_parameter( lamp_to_add(X) ):-         sem_sort( X, lamp ).
valid_parameter( lamp_to_remove(X) ):-      sem_sort( X, lamp ).
valid_parameter( lamp(X) ):-                sem_sort( X, lamp ).
