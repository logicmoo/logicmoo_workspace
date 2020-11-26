
:- module(output_manager,
	[abstract_action_to_action/2]
    ).

%======================================================================

:- use_module(library(lists)).

:- use_module('$REGULUS/PrologLib/utilities').

:- ensure_loaded('$REGULUS/Examples/EnglishRobots/Prolog/generation_grammar.pl').
:- ensure_loaded('$REGULUS/Examples/GermanRobots/Prolog/generation_grammar.pl').

%======================================================================

% OUTPUT MANAGEMENT: ABSTRACT

%ACTION TO CONCRETE ACTION

abstract_action_to_action(say(Language, AbstractAction),
			  say_string_and_send_message_to_robot(OutputString, Message)) :-
	perform_output_generation(Language, AbstractAction, OutputString, Message),
	!.

perform_output_generation(Language, AbstractAction,OutputString,Message) :-
	(   generation_grammar(Language, AbstractAction, OutputWords, []) ->
	    join_with_spaces(OutputWords, OutputAtom)
	;
	    otherwise ->
	    format_to_atom('Unable to realize ~w in generation grammar', [AbstractAction], OutputAtom)
	),
	
	atom_codes(OutputAtom, OutputString),
	(   message_for_dtsend(AbstractAction, Message) ->
	    true
	;
	    Message = '$CL3_NULL'
	),
	!.
perform_output_generation(_Language, _AbsAct, _OutputString, _Message) :-
	format('~N~nError in output manager.~n', []),
	fail.


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

message_for_dtsend([fire,laser], '$CL3_SOUND,61,*,').

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
