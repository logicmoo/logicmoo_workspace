
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(input_manager,
	[lf_to_dialogue_move/2]
    ).

:- ensure_loaded('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%======================================================================
 
% INPUT MANAGEMENT: LF TO DIALOGUE MOVE
 
lf_to_dialogue_move(LF, Move) :-
	lf_patterns:extract_feature_values(LF, FeaturesVals),
	post_process_move(FeaturesVals, Move),
	!.
lf_to_dialogue_move(_LF, _DialogueMove) :-
	format('~N~nLF to dialogue move failed~n', []),
	fail.
 
%======================================================================

post_process_move(MoveIn, MoveOut) :-
	move_query_objects_and_utterance_types_out_of_aggregates(MoveIn, MoveNext),
	safe_remove_duplicates_preserving_order(MoveNext, MoveOut),
	!.
post_process_move(MoveIn, MoveIn) :-
	!.
post_process_move(MoveIn, MoveOut) :-
	format('~N*** Error: bad call: ~w~n', [post_process_move(MoveIn, MoveOut)]),
	fail.

% Hack to try and fix results of some bad attachments.

move_query_objects_and_utterance_types_out_of_aggregates([], []).
move_query_objects_and_utterance_types_out_of_aggregates([F | R], Result) :-
	move_query_objects_and_utterance_types_out_of_aggregate(F, F1, MovedItems),
	move_query_objects_and_utterance_types_out_of_aggregates(R, R1),
	append([F1 | MovedItems], R1, Result).

move_query_objects_and_utterance_types_out_of_aggregate(aggregate(Type, Body),
							aggregate(Type, Body1),
							MovedItems) :-
	move_query_objects_and_utterance_types_out_of_aggregate1(Body, Body1, MovedItems),
	!.
move_query_objects_and_utterance_types_out_of_aggregate(Other, Other, []).

move_query_objects_and_utterance_types_out_of_aggregate1([], [], []).
move_query_objects_and_utterance_types_out_of_aggregate1([F | R], R1, [F | MovedR]) :-
	(   F = (query_object = _)
	;   F = (utterance_type = _)
	),
	!,
	move_query_objects_and_utterance_types_out_of_aggregate1(R, R1, MovedR).
move_query_objects_and_utterance_types_out_of_aggregate1([F | R], [F | R1], MovedR) :-
	!,
	move_query_objects_and_utterance_types_out_of_aggregate1(R, R1, MovedR).










