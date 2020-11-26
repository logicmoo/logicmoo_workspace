
:- module(nbest_features,
	  [feature_weight/2,
	   feature_value_for_record/3,
	   extract_feat_val_from_record/3
	  ]).

:- use_module('$REGULUS/PrologLib/utilities').
:- use_module(library(lists)).

:- use_module('$REGULUS/Examples/Calendar/Prolog/dialogue_utils').
:- use_module('$REGULUS/Examples/Calendar/Prolog/calendar_utils').

%---------------------------------------------------------------
 
% Place in the N-best list - lower number is better
feature_weight(rank, -1).

% Strongly penalise hypotheses that produce no dialogue move
feature_weight(no_dialogue_move, -50).

% Penalise queries with no contentful constraints
feature_weight(underconstrained_query, -10).

% Disprefer combination of indefinite mention of meeting + available meeting referent
feature_weight(indefinite_meeting_and_meeting_referent, -2).

% FEATURES CURRENTLY WITH ZERO WEIGHTS FOR SVM TRAINING

% Prefer combination of definite mention of meeting + available meeting referent
feature_weight(definite_meeting_and_meeting_referent, 0).

% Penalise inconsistent tense specifications
feature_weight(inconsistent_tense, 0).

% Meeting referent available (only likely to be useful when combined with something else)
feature_weight(meeting_referent_available, 0).

%---------------------------------------------------------------

feature_value_for_record(rank, Record, Score) :-
	member(rank=Rank, Record),
	Score = Rank,
	!.
feature_value_for_record(no_dialogue_move, Record, Score) :-
	(   member(dialogue_move=_Move, Record) ->
	    Score = 0
	;
	    Score = 1
	),
	!.
feature_value_for_record(underconstrained_query, Record, Score) :-
	member(dialogue_move=Move, Record),
	(   underconstrained_query(Move) ->
	    Score = 1
	;
	    Score = 0
	),
	!.
feature_value_for_record(inconsistent_tense, Record, Score) :-
	member(resolved_dialogue_move=Move, Record),
	(   inconsistent_tense(Move) ->
	    Score = 1
	;
	    Score = 0
	),
	!.
feature_value_for_record(definite_meeting_and_meeting_referent, Record, Score) :-
	member(in_state=InState, Record),
	member(dialogue_move=Move, Record),
	(   (   meeting_referent_available(InState),
		definite_reference_to_meeting(Move)
	    ) ->
	    Score = 1
	;
	    Score = 0
	),
	!.
feature_value_for_record(indefinite_meeting_and_meeting_referent, Record, Score) :-
	member(in_state=InState, Record),
	member(parse=LF, Record),
	(   (   meeting_referent_available(InState),
		indefinite_reference_to_meeting(LF)
	    ) ->
	    Score = 1
	;
	    Score = 0
	),
	!.
feature_value_for_record(meeting_referent_available, Record, Score) :-
	member(in_state=InState, Record),
	(   meeting_referent_available(InState) ->
	    Score = 1
	;
	    Score = 0
	),
	!.
feature_value_for_record(_OtherFeat, _Record, 0).

%---------------------------------------------------------------

underconstrained_query(Move) :-
	member(utterance_type=ynq, Move),
	\+ adequately_constrained_ynq(Move).
underconstrained_query(Move) :-
	member(utterance_type=whq, Move),
	\+ adequately_constrained_whq(Move).

adequately_constrained_ynq(Move) :-
	member(PatternElement, Move),
	\+ non_constraining_pattern_element(PatternElement),
	\+ PatternElement = (meeting = _),
	\+ PatternElement = (referent_from_context = _),
	\+ PatternElement = aggregate(_, _),
	!.

adequately_constrained_whq(Move) :-
	member(query_object=Type, Move),
	Type \== meeting,
	!.
adequately_constrained_whq(Move) :-
	member(PatternElement, Move),
	\+ non_constraining_pattern_element(PatternElement),
	\+ PatternElement = (meeting = _),
	\+ PatternElement = (referent_from_context = _),
	!.

%---------------------------------------------------------------

definite_reference_to_meeting(Move) :-
	member(referent_from_context=meeting, Move).

%---------------------------------------------------------------

inconsistent_tense(Move) :-
	member(tense_information=TenseInterval, Move),
	member(in_interval=Interval, Move),
	\+ intervals_intersect(Interval, TenseInterval).

inconsistent_tense(Move) :-
	member(tense_information=TenseInterval, Move),
	member(meeting=Id, Move),
	atomic(Id),
	interval_for_meeting_id(Id, Interval),
	\+ intervals_intersect(Interval, TenseInterval).

%---------------------------------------------------------------

/*
  term(a,meeting,[[on_date,date(unspecified,7,9)]])
*/

indefinite_reference_to_meeting(term(a, meeting, _)) :-
	!.
indefinite_reference_to_meeting(T) :-
	compound(T),
	functor(T, _F, N),
	indefinite_reference_to_meeting_args(N, T).

indefinite_reference_to_meeting_args(I, T) :-
	I >= 1,
	arg(I, T, Arg),
	indefinite_reference_to_meeting(Arg),
	!.
indefinite_reference_to_meeting_args(I, T) :-
	I >= 2,
	I1 is I - 1,
	!,
	indefinite_reference_to_meeting_args(I1, T).

%---------------------------------------------------------------

meeting_referent_available(State) :-
	member(referents=References, State),
	member(record(meeting, _), References).

%---------------------------------------------------------------

/*

THINGS FOR EXTRACTING FEATURES FOR N-BEST TRAINING

- Features extracted from resolved LF
  - Triples?
- Features extracted from resolved dialogue move
  - Anything left unresolved?
- Initial hand-coded features
  - ellipsis + trivial resolution

*/

% Semantically correct or not

extract_feat_val_from_record(Record, semantically_correct, Val) :-
	member(sem_recognition=Val, Record).

% Hand-coded features (including rank)

extract_feat_val_from_record(Record, Feat, Val) :-
	feature_value_for_record(Feat, Record, Val).

% What was recognised [list of words]

extract_feat_val_from_record(Record, recognised_words, Words) :-
	member(recognised=Words, Record).

% Confidence score 

extract_feat_val_from_record(Record, confidence, Confidence) :-
	member(confidence=Confidence, Record).

% Actual response

extract_feat_val_from_record(Record, response, Response) :-
	member(action=tts(Response), Record).

% Features extracted from abstract action
%  Categorise as one of following
%    No response
%    Error response
%    No
%    Yes
%    Reference list with N elements
%    Other

extract_feat_val_from_record(Record, response_type, no_response) :-
	\+ member(abstract_action_and_out_state=[_AbstractAction, _OutState], Record).

extract_feat_val_from_record(Record, response_type, ActionSummary) :-
	member(abstract_action_and_out_state=[AbstractAction, _OutState], Record),
	summarise_abstract_action(AbstractAction, ActionSummary).

% Features from in-state
%  - Previous LF present?
%  - Referent of each type available?

extract_feat_val_from_record(Record, lf_context_available, Val) :-
	(   ( member(in_state=InState, Record), member(lf=_, InState) ) ->
	    Val = yes
	;
	    Val = no
	).
extract_feat_val_from_record(Record, referent_available, ReferentType) :-
	member(in_state=InState, Record),
	member(referents=Referents, InState),
	member(Referent, Referents),
	referent_type(Referent, ReferentType).

% Features extracted from dialogue move
%   Features from feature/value list
extract_feat_val_from_record(Record, dialogue_move, MoveSummary) :-
	member(dialogue_move=Move, Record),
	summarise_dialogue_move(Move, MoveSummary).

% Features extracted from resolution
%   Trivial/non-trivial

extract_feat_val_from_record(Record, resolution, Val) :-
	member(resolution=Resolution, Record),
	(   Resolution = [trivial] ->
	    Val = trivial
	;
	    Val = non_trivial
	).

% Features extracted from parse
%  - Triples
%  - ellipsis/non-ellipsis

extract_feat_val_from_record(Record, elliptical_utterance, Val) :-
	member(parse=LF, Record),
	(   term_contains_functor(LF, elliptical/0) ->
	    Val = yes
	;
	    Val = no
	).

%---------------------------------------------------------------

summarise_abstract_action(AbstractAction, ActionSummary) :-
	(   ( AbstractAction = say(SayAction), summarise_say_action(SayAction, ActionSummary) ) ->
	    true
	;
	    ActionSummary = other
	).

summarise_say_action(yes, say_yes) :-
	!.
summarise_say_action(no, say_no) :-
	!.
summarise_say_action(referent_list([]), say_nothing_found) :-
	!.
summarise_say_action(referent_list(List), say_list_of_n_referents(N)) :-
	length(List, N),
	N > 0,
	!.
summarise_say_action(error_list(_), say_clarification_question) :-
	!.


referent_type(record(ReferentType, _), ReferentType).
referent_type(attribute(_, _, ReferentType), ReferentType).

summarise_dialogue_move([], []).
summarise_dialogue_move([F | R], [F1 | R1]) :-
	summarise_dialogue_move_element(F, F1),
	!,
	summarise_dialogue_move(R, R1).

summarise_dialogue_move_element(query_object=Object, Object) :-
	!.
summarise_dialogue_move_element(Key=_Val, Key).
