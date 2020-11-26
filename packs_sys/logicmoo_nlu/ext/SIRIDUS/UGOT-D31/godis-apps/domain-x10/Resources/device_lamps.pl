/*************************************************************************

         name: device_lamps.pl 
	 date: 2004-11-23
       author: Andreas Wallentin
 
*************************************************************************/

:- module( device_lamps, [ dev_set/2,
			   dev_get/2,
			   dev_do/2,
			   dev_query/3,
			   valid_parameter/1,
			   exists/1
			 ] ).

:- use_module( library(lists), [select/3,
				member/2,
				append/3] ).

%:- ensure_loaded( library(oaag) ).
:- ensure_loaded(trindikit(tkit_oaa)).
:- ensure_loaded( lamps ).

resource_of_type(upnp_dev).

:- multifile synset/2.

%%% also see default_value/2

:- dynamic variable_value/2,
	existing_lamps/1.


environment_mode(simulation).

action( 'TurnOn',      [] ).
action( 'TurnOff',     [] ).
action( 'AddLamp',     [] ).
action( 'RemoveLamp',  [] ).
action( 'AllOn',       [] ).
action( 'AllOff',      [] ).


%%% existing_lamps( LampList ) where LampList == lampVariant(Status)
existing_lamps( [] ).
%existing_lamps( [desklamp(off),
%		 floorlamp(off)] ).

%%% 
%%% See further down for help predicates
%%% 



%default_value( current_lamp, taklampa ).

query( current_lamp(_Lamp),       [] ).%%% who am i?
query( current_lamp_off(_Status), [] ).%%% is current lamp off?
query( lamp_on_exists(_Lamp),     [lamp_to_turn_on] ).%%% does current lamp exist?
query( lamp_off_exists(_Lamp),    [lamp_to_turn_off] ).%%% does current lamp exist?
query( lamp_add_exists(_Lamp),    [lamp_to_add] ).
query( lamp_remove_exists(_Lamp), [lamp_to_remove] ).

query( available_lamps(_L),       [] ).%%% available lamps
query( any_lamps,                 [] ).
query( lamps_on(_L),              [] ).
query( lamps_off(_L),             [] ).
query( is_lit(_L),                [] ).
query( is_off(_L),                [] ).


%init_lamps:-
%	environment_mode(simulation),
%	user:flag( visualize_devices, yes ),
%	!,%trace,
%	ensure_loaded(library(visualization)),
%	gfx_set_initial_values(device_lamps,play).


init_lamps.


dev_query( Query, Commitments, Answer):-
	query(Query,Vars),
	set_command_variables(Vars,Commitments,_Values),
	perform_query(Query,Answer).


%%% is Lamp lit/on
perform_query( is_lit(Lamp), Answer ):-
	(
	  exists(Lamp),
	  swe2eng(Lamp,EngLamp),
	  chk_status(EngLamp,EngLamp-Status),
	  (
	    Status = on,
	    atom_concat('Ja, ',Lamp,A1),
	    atom_concat(A1,'n är tänd',Atom)
	  ;
	    atom_concat('Nej, ',Lamp,A1),
	    atom_concat(A1,'n är inte tänd',Atom)
	  )
	;
	  atom_concat('Det finns ingen ',Lamp,A1),
	  atom_concat(A1,' inkopplad',Atom)
	),
	Answer = is_lit(Atom).

%%% is Lamp turned off
perform_query( is_off(Lamp), Answer ):-
	(
	  exists(Lamp),
	  swe2eng(Lamp,EngLamp),
	  chk_status(EngLamp,EngLamp-Status),
	  (
	    Status = off,
	    atom_concat('Ja, ',Lamp,A1),
	    atom_concat(A1,'n är släckt',Atom)
	  ;
	    atom_concat('Nej, ',Lamp,A1),
	    atom_concat(A1,'n är inte släckt',Atom)
	  )
	;
	  atom_concat('Det finns ingen ',Lamp,A1),
	  atom_concat(A1,' inkopplad',Atom)
	),
	Answer = is_off(Atom).


%%% are there any lamps plugged in
perform_query( any_lamps, Answer ):-
	existing_lamps(Lamps),
	(
	  Lamps = [],
	  Answer = not(any_lamps)
	;
	  Answer = any_lamps
	).


%%% which lamps are plugged in
perform_query( available_lamps(LampsAtom), available_lamps(LampsAtom) ):-
	existing_lamps(List),
	lamplist2atom(List,LampsAtom).

%%% is Lamp plugged in
%%% so far different queries depending on situation
perform_query( lamp_on_exists(Lamp), Answer ):-
	(
	  dev_get(lamp_to_turn_on,Lamp),
	  exists(Lamp),
	  try(retractall(variable_value(current_lamp,_))),
	  dev_set(current_lamp,Lamp),
	  Answer = lamp_on_exists(Lamp)
	;
	  Answer = not(lamp_on_exists(Lamp))
	).
perform_query( lamp_off_exists(Lamp), Answer ):-
	(
	  dev_get(lamp_to_turn_off,Lamp),
	  exists(Lamp),
	  try(retractall(variable_value(current_lamp,_))),
	  dev_set(current_lamp,Lamp),
	  Answer = lamp_off_exists(Lamp)
	;
	  Answer = not(lamp_off_exists(Lamp))
	).
perform_query( lamp_add_exists(Lamp), Answer ):-
	(
	  dev_get(lamp_to_add,Lamp),
	  exists(Lamp),
	  try(retractall(variable_value(current_lamp,_))),
	  dev_set(current_lamp,Lamp),
	  Answer = lamp_add_exists(Lamp)
	;
	  Answer = not(lamp_add_exists(Lamp))
	).
perform_query( lamp_remove_exists(Lamp), Answer ):-
	(
	  dev_get(lamp_to_remove,Lamp),
	  exists(Lamp),
	  try(retractall(variable_value(current_lamp,_))),
	  dev_set(current_lamp,Lamp),
	  Answer = lamp_remove_exists(Lamp)
	;
	  Answer = not(lamp_remove_exists(Lamp))
	).


%%% "help query"
%%% used to get hold of the lamp currently being handled
perform_query( current_lamp(Lamp), current_lamp(Lamp) ):-
	dev_get(current_lamp,Lamp).

%%% is the current lamp on/off
perform_query( current_lamp_off(Status), Answer ):-
	dev_get(current_lamp,SweLamp),
	swe2eng(SweLamp,EngLamp),
	chk_status(EngLamp,_-Status),
	(
	  Status = off,
	  Answer = current_lamp_off(Status)
	;
	  Answer = not(current_lamp_off(Status))
	).

%%% which lamps are on
perform_query( lamps_on(Lamps), lamps_on(Lamps) ):-
	(
	  setof(L,Not^chk_status(L,Not-on),List),
	  lamplist2atom2(List,Lamps)
	;
	  Lamps = 'Inga lampor är tända'
	).

%%% which lamps are off
perform_query( lamps_off(Lamps), lamps_off(Lamps) ):-
	(
	  setof(L,Not^chk_status(L,Not-off),List),
	  lamplist2atom2(List,Lamps)
	;
	  Lamps = 'Inga lampor är släckta'
	).


	
dev_set(ID,Value1) :-
	environment_mode(simulation),
	interpret_pragmatically(ID,Value1,Value),
	try(retract(variable_value(ID,_))),
	assert(variable_value(ID,Value)),
	( user:flag(visualize_devices,yes) ->
	    gfx_set_node_value(player,ID,Value)
	;
	    true
	).
	

dev_get(ID,Value) :-
	environment_mode(simulation),
	( variable_value(ID,CurrentValue) ->
	    Value = CurrentValue
	;
	    default_value(ID,Value)
	).


dev_do(Command,Commitments) :-
	action(Command,Vars),
	set_command_variables(Vars,Commitments,Values),
	( environment_mode(simulation) ->
	    output_upnp(Command,Values) ;
	    true ),
	perform_command(Command).


set_command_variables([],_,[]).
set_command_variables([Var|Vars],Commitments,[Val|Vals]) :-
	Com =.. [ Var, Val ],
	member(Com,Commitments),
	dev_set(Var,Val),
	set_command_variables(Vars,Commitments,Vals).



%%% turning on all lamps
perform_command( 'AllOn' ):-
	existing_lamps(Lamps),
	all_on(Lamps).

%%% turning off all lamps
perform_command( 'AllOff' ):-
	existing_lamps(Lamps),
	all_off(Lamps).

%%% turning on the Lamp currently handled
perform_command( 'TurnOn' ):-
	dev_get(lamp_to_turn_on,Lamp),
	swe2eng(Lamp,Concept),
	chg_status(Concept,on),
	oaag:solve(devmExecuteAction(on,Concept,100,_AA),[],_A).

%%% turning off the Lamp currently handled
perform_command( 'TurnOff' ):-
	dev_get(lamp_to_turn_off,Lamp),
	swe2eng(Lamp,Concept),
	chg_status(Concept,off),
	oaag:solve(devmExecuteAction(off,Concept,100,_G),[],_A).

%%% plugging in the currently handled lamp
%%% status is Off as default
perform_command( 'AddLamp' ):-
	dev_get(lamp_to_add,Lamp),
	swe2eng(Lamp,Concept),
	add_lamp(Concept),
	lamp([Lamp],Address),
	oaag:solve(devmCatchDevice(Concept,Lamp,0,Address),[],_Ans).

%%% removes the current Lamp
perform_command( 'RemoveLamp' ):-
	dev_get(lamp_to_remove,Lamp),
	swe2eng(Lamp,Concept),
	remove_lamp(Concept),
	lamp([Lamp],_Address),
	oaag:solve(devmRemove(Concept),[],_Ans).


output_upnp(Command,Parameters) :-
 	Term =.. [ Command | Parameters ],
 	format('\n[UPnP] ~w\n\n',[Term]).

try(G) :-
	( G -> true ; true ).


interpret_pragmatically(_,V,V).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Below are help predicates used to handle lamps
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

exists( SweLampSort ):-
	swe2eng2(SweLampSort,_).


swe2eng2( SweLamp, Concept ):-
	synset(Words,Concept),
	member(SweLamp,Words),
	Lamp =.. [Concept,_Status],
	existing_lamps(List),
	member(Lamp,List).
	
swe2eng( SweLamp, Concept ):-
	synset(Words,Concept),
	member(SweLamp,Words).


%%% all_on(Lamps) == existing_lamps(Lamps)
all_on( [] ).
all_on( [Lamp|Rest] ):-
	Lamp =.. [LampSort,_Status],
	chg_status(LampSort,on),
	oaag:solve(devmExecuteAction(on,LampSort,100,_AA),[],_Ans),
	all_on(Rest).

all_off( [] ).
all_off( [Lamp|Rest] ):-
	Lamp =.. [LampSort,_Status],
	chg_status(LampSort,off),
	oaag:solve(devmExecuteAction(off,LampSort,0,_AA),[],_Ans),
	all_off(Rest).


chk_status( Lamp, Lamp-Status ):-
	existing_lamps( List ),
	select(F,List,_),
	F =.. [Lamp,Status].

chg_status( LampSort, NewStatus ):-
	existing_lamps( LampList ),
	Lamp =.. [LampSort,_Status],
	select(Lamp,LampList,NewList),
	retractall(existing_lamps(_)),
	NewLamp =.. [LampSort,NewStatus],
	append(NewList,[NewLamp],NewestList),
	assert(existing_lamps(NewestList)).

remove_lamp( LampSort ):-
	existing_lamps(LampList),
	Lamp =.. [LampSort,_Status],
	select(Lamp,LampList,NewList),
	retract(existing_lamps(_)),
	assert(existing_lamps(NewList)).

%%% default status will be off
add_lamp( LampSort ):-
	add_lamp( LampSort, off ).
add_lamp( LampSort, Status ):-
	existing_lamps(LampList),
	Lamp =.. [LampSort,Status],
	retract(existing_lamps(_)),
	append(LampList,[Lamp],NewList),
	assert(existing_lamps(NewList)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% taking list, returning atoms

%%% lamplist2atom( ?ListOfLampsWithStatus, ?LampSortsAtom)
lamplist2atom(List,Atom):-
	lamplist2atom(List,[],Atom),
	!.
lamplist2atom([],_,'').
lamplist2atom([Last],_,Atom):-
	Last =.. [L,_],
	%%% to swedish
	synset([SweL],L),
	atom_concat('',SweL,Atom).
lamplist2atom([First|Rest],Temp,Atom):-
	lamplist2atom(Rest,Temp,A2),
	First =.. [F,_],
	%%% to swedish
	synset([SweF],F),
	atom_concat(SweF,',',Atom2),
	atom_concat(Atom2,A2,Atom).

%%% lamplist2atom2( ?ListOfLampSorts, ?LampSortsAtom)
lamplist2atom2(List,Atom):-
	lamplist2atom2(List,[],Atom),
	!.
lamplist2atom2([],_,'').
lamplist2atom2([Last],_,Atom):-
       %%% to swedish
	synset([SweLast],Last),
	atom_concat('',SweLast,Atom).
lamplist2atom2([First|Rest],Temp,Atom):-
	lamplist2atom2(Rest,Temp,A2),
	%%% to swedish
	synset([SweFirst],First),
	atom_concat(SweFirst,',',Atom2),
	atom_concat(Atom2,A2,Atom).


synset( [bordslampa],           tablelamp ).
synset( [bordlampa],            tablelamp ).
synset( [taklampa],             ceilinglamp ).
synset( [skrivbordslampa],      desklamp ).
synset( [sänglampa],            bedlamp ).
synset( [hörnlampa],            cornerlamp ).
synset( [sofflampa],            couchlamp ).
synset( [golvlampa],            floorlamp ).


:- init_lamps.
