
:- module(output_manager,
	[abstract_action_to_action/2]
    ).

%======================================================================

:- use_module(library(lists)).

:- use_module('$REGULUS/PrologLib/utilities').

%======================================================================

% OUTPUT MANAGEMENT: ABSTRACT

%ACTION TO CONCRETE ACTION

abstract_action_to_action(say(AbstractAction), [OutputString,Message]) :-
	perform_output_generation(AbstractAction, OutputString,Message),
	!.

perform_output_generation(AbstractAction,OutputString,Message) :-
	generation_grammar(AbstractAction, OutputWords,[]),
	join_with_spaces(OutputWords, OutputAtom),
	atom_chars(OutputAtom, OutputString),
	message_for_dtsend(AbstractAction, Message),
	!.
perform_output_generation(_AbsAct, _OutputString, _Message) :-
	format('~N~nError in output manager.~n', []),
	fail.



%TTS RESPONSES TO COMMANDS ========================================================
	
%generation_grammar(no) --> ['no'].
generation_grammar(unable_to_interpret) --> ['sorry that doesn\'t make sense'].



%RESPONSES TO FIND COMMANDS

generation_grammar(obj(Object,found)) --> 
	['We found'],
	generation_grammar(Object).
/*
generation_grammar([found,Object]) --> 
	['We found'],
	generation_grammar(Object).

generation_grammar([lose,Object]) -->
	generation_grammar(Object),
	['was lost'].
*/


%RESPONSES TO INTERACTION WITH FOUND OBJECT COMMANDS

generation_grammar([rotate, Object]) -->
	['We are turning toward'],
	generation_grammar(Object).

	
generation_grammar([surround, Object]) -->
	['We are surrounding'],
	generation_grammar(Object).
	
generation_grammar([circle, Object]) -->
	['We are surrounding'],
	generation_grammar(Object).
	
generation_grammar([guard, Object]) -->
	['We are guarding'],
	generation_grammar(Object).
	
generation_grammar([rescue, Object]) -->
	['We are rescuing'],
	generation_grammar(Object).
	
generation_grammar([stalk, Object]) -->
	['We are stalking'],
	generation_grammar(Object).



generation_grammar([reset]) -->
	['We are going home'].

	

%RESPONSES TO MAKING SHAPES

generation_grammar([make,Shape]) -->
	['We are making'],
	generation_grammar(Shape).



%RESPONSES TO MOTION COMMANDS

generation_grammar([rotate,Direction]) -->
	['We turned'],
	generation_grammar(Direction).

generation_grammar([move, Direction]) -->
	['Moving'],
	generation_grammar(Direction).
	
generation_grammar([move, Cardinal]) -->
	['Moving'],
	generation_grammar(Cardinal).	


generation_grammar([move_out]) -->
	['Spreading out'].

generation_grammar([move_in]) -->
	['Coming together'].

generation_grammar([stop]) -->
	['stop'].

%RESPONSES
generation_grammar([fire,Weapon]) -->
	['firing'],
  generation_grammar(Weapon).





/*
generation_grammar([attack_formation_delta]) -->
	['all your base are belong to us'].
*/

generation_grammar(ambiguous) --> 
  ['sorry, that\'s ambiguous'].








generation_grammar(friend) --> ['a friend'].
generation_grammar(human) --> ['a human'].
generation_grammar(victim) --> ['a victim'].


generation_grammar(dev(lost)) --> ['lost'].
generation_grammar(dev(found)) --> ['found'].

generation_grammar(triangle) -->['a triangle'].
generation_grammar(delta) -->['attack formation delta'].

generation_grammar(iso_triangle) -->['an isosceles triangle'].
generation_grammar(equi_triangle) -->['an equilateral triangle'].
generation_grammar(circle) -->['a circle'].
generation_grammar(right_triangle) -->['a right triangle'].
generation_grammar(trick) --> ['a trick'].
generation_grammar(delta) --> ['attack formation delta'].
generation_grammar(laser) --> ['lasers'].

generation_grammar(south) --> ['south'].
generation_grammar(north) --> ['north']. 
generation_grammar(east) --> ['east'].
generation_grammar(west) --> ['west'].
%generation_grammar(northeast) --> ['northeast'].
%generation_grammar(northwest) --> ['northwest'].
%generation_grammar(southeast) --> ['southeast'].
%generation_grammar(southwest) --> ['southwest'].

%generation_grammar(forward) --> ['forward'].
%generation_grammar(backward) --> ['backward'].

generation_grammar(left) --> ['left'].
generation_grammar(right) --> ['right'].
generation_grammar(around) --> ['around'].



%ROBOT COMMANDS ASSOCIATED WITH RESPONSES =========================================================

%ROBOT COMMANDS FOR FINDING OBJECTS

message_for_dtsend([find,human],'$CL3_TRACK,POORVICTIM,*,').
message_for_dtsend([find,friend],'$CL3_TRACK,POORVICTIM,*,').
message_for_dtsend([find,victim],'$CL3_TRACK,POORVICTIM,*,').

%ROBOTS COMMANDS FOR INTERACTING WITH FOUND OBJECTS

message_for_dtsend([rotate,human],'$CL3_TRACK,POORVICTIM,*,').
message_for_dtsend([rotate,friend],'$CL3_TRACK,POORVICTIM,*,').
message_for_dtsend([rotate,victim],'$CL3_TRACK,POORVICTIM,*,').


message_for_dtsend([reset],'$CL3_BETA,A,60,*,:$CL3_THETA,A,0,*,:$CL3_Y,A,0,*,:$CL3_X,A,0,*,:$CL3_Q,A,30,*,:$CL3_P,A,30,*,').

message_for_dtsend([fire_lasers], '$CL3_SOUND,61,*,').

message_for_dtsend([move_out], '$CL3_P,P,.25,*,:$CL3_Q,P,.25,*,').
message_for_dtsend([move_in], '$CL3_P,P,-.25,*,:$CL3_Q,P,-.25,*,').

message_for_dtsend([move, north],'$CL3_Y,R,1,*,').
message_for_dtsend([move, south],'$CL3_Y,R,-1,*,').
message_for_dtsend([move, west],'CL3_X,R,-1,*,').
message_for_dtsend([move, east],'$CL3_X,R,1,*,'). 

message_for_dtsend([face, north],'$CL3_THETA,A,0,*,').
message_for_dtsend([face, south],'$CL3_THETA,A,180,*,').
message_for_dtsend([face, west],'$CL3_THETA,A,-90,*,').
message_for_dtsend([face, east],'$CL3_THETA,A,90,*,').
message_for_dtsend([face, northeast],'$CL3_THETA,A,45,*,').
message_for_dtsend([face, southeast],'$CL3_THETA,A,135,*,').
message_for_dtsend([face, northwest],'$CL3_THETA,A,-45,*,').
message_for_dtsend([face, southwest],'$CL3_THETA,A,-135,*,'). 
message_for_dtsend([face, left],'$CL3_THETA,A,-90,*,').
message_for_dtsend([face, right],'$CL3_THETA,A,90,*,').
message_for_dtsend([face, around],'$CL3_THETA,A,180,*,').

message_for_dtsend([move, forward], '$CL3_DIRECTION,20,*,').
message_for_dtsend([move, backward], '$CL3_DIRECTION,-20,*,').

message_for_dtsend([make, triangle],'$CL3_BETA,A,60,*,:$CL3_THETA,A,0,*,:$CL3_Y,R,0,*,:$CL3_X,R,0,*,:$CL3_Q,A,30,*,:$CL3_P,A,30,*'). 
message_for_dtsend([make, right_triangle],'$CL3_BETA,A,90,*,:$CL3_THETA,A,0,*,:$CL3_Y,R,0,*,:$CL3_X,R,0,*,:$CL3_Q,A,30,*,:$CL3_P,A,30,*').


message_for_dtsend([stop],'$CL3_STOP,*,').


message_for_dtsend([make, trick], '$CL3_P,A,5.0,*,:$CL3_Q,A,7.0,*,:$CL3_BETA,A,30.0,*').

%message_for_dtsend([attack_formation_delta], '$CL3_BETA,A,120,*,:$CL3_THETA,A,0,*,:$CL3_Y,R,0,*,:$CL3_X,R,0,*,:$CL3_Q,A,30,*,:$CL3_P,A,30,*,:$CL3_DIRECTION,2,*,').
