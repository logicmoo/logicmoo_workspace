% resolve.pl

% Resolution processing for translation

:- module(resolve,
	[null_discourse_context/1,
	 get_preceding_source_discourse/2,
	 set_preceding_source_discourse/3,
	 get_preceding_resolved_interlingua/2,
	 set_preceding_resolved_interlingua/3,
	 get_preceding_target_utterance/2,
	 set_preceding_target_utterance/3,

	 ellipsis_processing_is_activated/0,
	 switch_on_answer_resolution/0,
	 switch_off_answer_resolution/0,
	 
	 perform_resolution/4,

	 remove_phrase_utterance_marking/2]
    ).

%----------------------------------------------------------------------

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(library(ordsets)).

%----------------------------------------------------------------------

null_discourse_context(Context) :-
	empty_assoc_generic(Context).

get_preceding_source_discourse(SourceDiscourse, Context) :-
	nonvar(Context),
	get_assoc_generic(preceding_source_discourse, Context, SourceDiscourse),
	!.
get_preceding_source_discourse(SourceDiscourse, _Context) :-
	SourceDiscourse = [].

set_preceding_source_discourse(NewSourceDiscourse, ContextIn, ContextOut) :-
	nonvar(ContextIn),
	put_assoc_generic(preceding_source_discourse, ContextIn, NewSourceDiscourse, ContextOut).

get_preceding_resolved_interlingua(Interlingua, Context) :-
	nonvar(Context),
	get_assoc_generic(preceding_resolved_interlingua, Context, Interlingua),
	!.
get_preceding_resolved_interlingua(Interlingua, _Context) :-
	Interlingua = [].

set_preceding_resolved_interlingua(NewInterlingua, ContextIn, ContextOut) :-
	nonvar(ContextIn),
	put_assoc_generic(preceding_resolved_interlingua, ContextIn, NewInterlingua, ContextOut).

get_preceding_target_utterance(Utt, Context) :-
	nonvar(Context),
	get_assoc_generic(preceding_target_utterance, Context, Utt),
	!.
get_preceding_target_utterance(Utt, _Context) :-
	Utt = '*no_preceding_utterance*'.

set_preceding_target_utterance(NewUtt, ContextIn, ContextOut) :-
	nonvar(ContextIn),
	put_assoc_generic(preceding_target_utterance, ContextIn, NewUtt, ContextOut).

%----------------------------------------------------------------------

ellipsis_processing_is_activated :-
	current_predicate(user:ellipsis_class_example/2).

%----------------------------------------------------------------------

:- dynamic answer_resolution_on/0.

switch_on_answer_resolution :-
	answer_resolution_on,
	!.
switch_on_answer_resolution :-
	assertz(answer_resolution_on).

switch_off_answer_resolution :-
	retractall(answer_resolution_on).

%----------------------------------------------------------------------

/*
- Resolution processing
  - CurrentSourceDiscourse x PrecedingResolvedSourceDiscourse -> ResolvedSourceDiscourse
    - Predicate: perform_resolution(CurrentSourceDiscourse, PrecedingResolvedSourceDiscourse, ResolvedSourceDiscourse)
  - So far, only yields non-trivial result if CurrentSourceDiscourse is a phrase
    - otherwise, CurrentSourceDiscourse = ResolvedSourceDiscourse
  - Remove [utterance_type, phrase]
  - Sort
  - Full match against some ellipsis_class_example, giving Id
  - Partial match of some other ellipsis_class_example with same Id against PrecedingResolvedSourceDiscourse
  - Replace matched material in PrecedingResolvedSourceDiscourse with CurrentSourceDiscourse
  - Match if first elt in pair is same, i.e. [Key, X] matches [Key, Y]
*/

perform_resolution(CurrentSourceDiscourse, PrecedingResolvedSourceDiscourse, ResolvedSourceDiscourse, ResolutionProcessing) :-
	var(PrecedingResolvedSourceDiscourse),
	format('~N*** WARNING: preceding resolved source discourse representation in call to perform_resolution/3 was uninstantiated.~n', []),
	ResolvedSourceDiscourse = CurrentSourceDiscourse,
	ResolutionProcessing = trivial,
	!.
perform_resolution(CurrentSourceDiscourse, PrecedingResolvedSourceDiscourse0, ResolvedSourceDiscourse, ResolutionProcessing) :-
	nonvar(PrecedingResolvedSourceDiscourse0),
	(   PrecedingResolvedSourceDiscourse0 = external(PrecedingResolvedSourceDiscourse) ->
	    Mode = answer ;

	    % For debugging
	    answer_resolution_on ->
	    PrecedingResolvedSourceDiscourse0 = PrecedingResolvedSourceDiscourse,
	    Mode = answer ;
	    
	    PrecedingResolvedSourceDiscourse0 = PrecedingResolvedSourceDiscourse,
	    Mode = default
	),
	(   whq_representation(PrecedingResolvedSourceDiscourse) ->
	    Wh = wh ;
	    
	    Wh = non_wh
	),
	all_resolutions(CurrentSourceDiscourse, PrecedingResolvedSourceDiscourse, Wh, Pairs),
	keysort(Pairs, SortedPairs),
	SortedPairs = [_NegLengthReplaced-[ResolvedSourceDiscourse0, ResolutionProcessing] | _Rest],
	(   ( Mode = answer, Wh = wh ) ->
	    change_whq_to_dcl(ResolvedSourceDiscourse0, ResolvedSourceDiscourse1),
	    change_you_to_i(ResolvedSourceDiscourse1, ResolvedSourceDiscourse) ;

	    ( Mode = answer, Wh \== wh ) ->
	    change_ynq_to_dcl(ResolvedSourceDiscourse0, ResolvedSourceDiscourse1),
	    change_you_to_i(ResolvedSourceDiscourse1, ResolvedSourceDiscourse) ;
	    
	    ( Mode \== answer, Wh = wh ) ->
	    change_whq_to_ynq(ResolvedSourceDiscourse0, ResolvedSourceDiscourse) ;
	    
	    ResolvedSourceDiscourse0 = ResolvedSourceDiscourse
	),
	!.
perform_resolution(CurrentSourceDiscourse, _PrecedingResolvedSourceDiscourse, ResolvedSourceDiscourse, ResolutionProcessing) :-
	ResolvedSourceDiscourse = CurrentSourceDiscourse,
	ResolutionProcessing = trivial,
	!.
perform_resolution(CurrentSourceDiscourse, PrecedingResolvedSourceDiscourse, ResolvedSourceDiscourse, ResolutionProcessing) :-
	format('~N*** ERROR: bad call: ~w~n', [perform_resolution(CurrentSourceDiscourse, PrecedingResolvedSourceDiscourse, ResolvedSourceDiscourse, ResolutionProcessing)]),
	fail.

all_resolutions(CurrentSourceDiscourse, PrecedingResolvedSourceDiscourse, Wh, Pairs) :-
	findall(LengthReplaced-[ResolvedSourceDiscourse, ResolutionProcessing],
		perform_resolution1(CurrentSourceDiscourse, PrecedingResolvedSourceDiscourse, Wh,
				    ResolvedSourceDiscourse, ResolutionProcessing, LengthReplaced),
		Pairs),
	!.

perform_resolution1(CurrentSourceDiscourse, PrecedingResolvedSourceDiscourse, Wh,
		    ResolvedSourceDiscourse,
		    ellipsis_substitution(Id, (Matched2 --> Matched1) ), ResolutionScore) :-
	ellipsis_processing_is_activated,
	remove_phrase_utterance_marking_and_uninstantiate_null_roles(CurrentSourceDiscourse, CurrentSourceDiscourse1),
	make_transfer_representation_canonical(CurrentSourceDiscourse1, CurrentSourceDiscourse2),
	make_transfer_representation_canonical(PrecedingResolvedSourceDiscourse, PrecedingResolvedSourceDiscourse1),

	full_match_against_ellipsis_class_example(CurrentSourceDiscourse2, Id, Matched1),

	get_ellipsis_class_and_context(Wh, Id, EllipsisClassMember2, EllipsisClassContext),
	
	partial_match(EllipsisClassMember2, PrecedingResolvedSourceDiscourse1, Matched2, NonMatchedPrecedingResolvedSourceDiscourse),
	%partial_match(EllipsisClassContext, NonMatchedPrecedingResolvedSourceDiscourse, _Matched3, _NonMatched2),
	partial_match(EllipsisClassContext, PrecedingResolvedSourceDiscourse1, _Matched3, _NonMatched2),

	align_roles_if_possible(CurrentSourceDiscourse2, Matched2),

	resolution_score(EllipsisClassMember2, EllipsisClassContext, Matched2, Matched1, ResolutionScore),
	
	append(NonMatchedPrecedingResolvedSourceDiscourse, CurrentSourceDiscourse2, ResolvedSourceDiscourse0),
	(   is_list(ResolvedSourceDiscourse0) ->
	    sort(ResolvedSourceDiscourse0, ResolvedSourceDiscourse)
	;
	    otherwise ->
	    format2error('~N*** Error in resolution: non-list resolved representation: ~w~n', [ResolvedSourceDiscourse0]),
	    fail
	).
perform_resolution1(CurrentSourceDiscourse, PrecedingResolvedSourceDiscourse, Wh,
		    ResolvedSourceDiscourse, 
		    ellipsis_substitution(Id, FromTo), LengthReplaced) :-
	remove_clause_from_representation(PrecedingResolvedSourceDiscourse, PrecedingClause, PrecedingResolvedSourceDiscourseWithoutClause),
	(   PrecedingClause = (Role=PrecedingClauseBody) ->
	    perform_resolution1(CurrentSourceDiscourse, PrecedingClauseBody, Wh,
				ResolvedClause, 
				ellipsis_substitution(Id, FromTo), LengthReplaced),
	    ResolvedSourceDiscourse = [Role=[clause, ResolvedClause] | PrecedingResolvedSourceDiscourseWithoutClause]
	;
	    otherwise ->
	    perform_resolution1(CurrentSourceDiscourse, PrecedingClause, Wh,
				ResolvedClause, 
				ellipsis_substitution(Id, FromTo), LengthReplaced),
	    ResolvedSourceDiscourse = [[clause, ResolvedClause] | PrecedingResolvedSourceDiscourseWithoutClause]
	).
% Add content of elliptical phrase to content of preceding source discourse.
% Since this matches no material, and we prioritise matching as much as possible,
% resolutions performed using this method will always be dispreferred compared
% to resolutions performed using substitution.
perform_resolution1(CurrentSourceDiscourse, PrecedingResolvedSourceDiscourse, _Wh,
		    ResolvedSourceDiscourse,
		    ellipsis_addition(CurrentSourceDiscourse1), NegLengthReplaced) :-
	ellipsis_processing_is_activated,
	remove_phrase_utterance_marking_and_uninstantiate_null_roles(CurrentSourceDiscourse, CurrentSourceDiscourse1),
	       
	NegLengthReplaced = 0,
	
	append(PrecedingResolvedSourceDiscourse, CurrentSourceDiscourse1, ResolvedSourceDiscourse0),
	sort(ResolvedSourceDiscourse0, ResolvedSourceDiscourse).

full_match_against_ellipsis_class_example(SourceDiscourse, Id, Matched) :-
	user:ellipsis_class_example(EllipsisClassMember, Id),
	full_match(EllipsisClassMember, SourceDiscourse, Matched).

get_ellipsis_class_and_context(wh, Id, EllipsisClassMember, EllipsisClassContext) :-
	user:ellipsis_class_example(EllipsisClassMember, wh-Id),
	EllipsisClassContext = [].
get_ellipsis_class_and_context(wh, Id, EllipsisClassMember, EllipsisClassContext) :-
	user:ellipsis_class_example([EllipsisClassMember, EllipsisClassContext], wh_with_context-Id).
get_ellipsis_class_and_context(non_wh, Id, EllipsisClassMember, EllipsisClassContext) :-
	user:ellipsis_class_example(EllipsisClassMember, Id),
	EllipsisClassContext = [].

%----------------------------------------------------------------------

full_match(List1, List2, Matched) :-
	partial_match(List1, List2, Matched, []).

partial_match([], Rest, [], Rest) :-
	!.
partial_match([F | R], [F1 | R1], [F1 | MatchedRest], Rest) :-
	element_match(F, F1),
	partial_match(R, R1, MatchedRest, Rest).
partial_match(List1, [F1 | R1], Matched, [F1 | Rest]) :-
	partial_match(List1, R1, Matched, Rest).

% Ellipsis records were previously stored in ground form.
% Now they are stored in partially uninstantiated form.
%element_match([Type, _Val1], [Type, _Val2]).
element_match([Type, Val], [Type, Val]).
element_match(_Role1=[Type, Val], _Role2=[Type, Val]).

%----------------------------------------------------------------------
% If all the roles in the matched phrase are Role, and Role is neither a var or null, 
% set all the uninstantiated roles in the elliptical phrase to Role.

align_roles_if_possible(Elliptical, Matched) :-
	all_roles_same(Matched, Role),
	Role \== null,
	instantiate_uninstantiated_roles(Elliptical, Role),
	!.
align_roles_if_possible(_Elliptical, _Matched).

all_roles_same([], _Role).
all_roles_same([Role1=_ | Rest], Role) :-
	nonvar(Role1),
	Role = Role1,
	!,
	all_roles_same(Rest, Role).

instantiate_uninstantiated_roles([], _Role).
instantiate_uninstantiated_roles([Role=_ | R], Role) :-
	instantiate_uninstantiated_roles(R, Role).
instantiate_uninstantiated_roles([_F | R], Role) :-
	instantiate_uninstantiated_roles(R, Role).

%----------------------------------------------------------------------

resolution_score(EllipsisClassMember2, EllipsisClassContext, Matched, ReplacedBy, ResolutionScore) :-	
	length(EllipsisClassMember2, MainLengthReplaced),
	length(EllipsisClassContext, ContextLength),
	       
	resolution_preference_score(Matched, PreferenceScore),
	clause_non_clause_score(Matched, ReplacedBy, ClauseNonClauseScore),
	
	ResolutionScore is -1 * ( MainLengthReplaced + ContextLength + PreferenceScore + ClauseNonClauseScore ),
	!.

% Disprefer resolutions that introduce or eliminate clauses
clause_non_clause_score(Matched, ReplacedBy, ClauseNonClauseScore) :-
	(   ( representation_contains_clause(Matched), representation_contains_clause(ReplacedBy) ) ->
	    ClauseNonClauseScore = 0
	;
	    ( \+representation_contains_clause(Matched), \+representation_contains_clause(ReplacedBy) ) ->
	    ClauseNonClauseScore = 0
	;
	    otherwise ->
	    ClauseNonClauseScore = -1
	).

representation_contains_clause(List) :-
	member([clause, _], List),
	!.
representation_contains_clause(List) :-
	member(_Role=[clause, _], List),
	!.

resolution_preference_score(Matched, PreferenceScore) :-
	current_predicate(user:resolution_preference/2),
	findall(Score,
		resolution_preference_result(Matched, Score),
		Scores),
	safe_sum_list(Scores, PreferenceScore),
	!.
resolution_preference_score(_Matched, PreferenceScore) :-
	PreferenceScore = 0.

resolution_preference_result(Matched, Score) :-
	user:resolution_preference(PreferencePattern, Score),
	member(PreferencePattern, Matched).

%----------------------------------------------------------------------

remove_phrase_utterance_marking_and_uninstantiate_null_roles(SourceDiscourseIn, SourceDiscourseOut) :-
	remove_phrase_utterance_marking(SourceDiscourseIn, SourceDiscourseNext),
	uninstantiate_null_roles(SourceDiscourseNext, SourceDiscourseOut).

remove_phrase_utterance_marking(SourceDiscourse, SourceDiscourse1) :-
	length(SourceDiscourse, OriginalLength),
	findall(Length-ShortenedSourceDiscourse,
		(   remove_phrase_utterance_marking1(SourceDiscourse, ShortenedSourceDiscourse),
		    length(ShortenedSourceDiscourse, Length)
		),
		Pairs),
	keysort(Pairs, SortedPairs),
	SortedPairs = [NewLength-SourceDiscourse1 | _Rest],
	NewLength < OriginalLength,
	!.

% In fact, remove possible interjection as well
remove_phrase_utterance_marking1(CurrentSourceDiscourse, CurrentSourceDiscourse2) :-
	get_utterance_marking(phrase, PhraseUtteranceMarking),
	list_to_ord_set(CurrentSourceDiscourse, CurrentSourceDiscourseOS),
	list_to_ord_set(PhraseUtteranceMarking, PhraseUtteranceMarkingOS),
	ord_subset(PhraseUtteranceMarkingOS, CurrentSourceDiscourseOS),
	ord_subtract(CurrentSourceDiscourseOS, PhraseUtteranceMarkingOS, CurrentSourceDiscourse1),
	remove_interjections(CurrentSourceDiscourse1, CurrentSourceDiscourse2),
	!.

%----------------------------------------------------------------------

remove_interjections(In, Out) :-
	get_utterance_marking(interjection_tag, InterjectionTag),
	remove_interjections1(In, InterjectionTag, Out),
	!.
remove_interjections(In, Out) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [remove_interjections(In, Out)]),
	fail.

remove_interjections1([], _Tag, []).
remove_interjections1([F | R], Tag, R1) :-
	(   safe_subsumes_chk([Tag, _], F)
	;
	    safe_subsumes_chk(_Role=[Tag, _], F)
	),
	!,
	remove_interjections1(R, Tag, R1).
remove_interjections1([F | R], Tag, [F | R1]) :-
	!,
	remove_interjections1(R, Tag, R1).

%----------------------------------------------------------------------

remove_clause_from_representation([Role=[clause, Clause] | Rest], Role=Clause, Rest) :-
	!.
remove_clause_from_representation([[clause, Clause] | Rest], Clause, Rest) :-
	!.
remove_clause_from_representation([F | R], Clause, [F | R1]) :-
	remove_clause_from_representation(R, Clause, R1).

%----------------------------------------------------------------------

uninstantiate_null_roles([], []).
uninstantiate_null_roles([F | R], [F1 | R1]) :-
	uninstantiate_null_role(F, F1),
	!,
	uninstantiate_null_roles(R, R1).

uninstantiate_null_role(null=Item, _AnonymousRole=Item) :-
	!.
uninstantiate_null_role(Other, Other).

%----------------------------------------------------------------------

change_whq_to_ynq(ResolvedSourceDiscourse, ResolvedSourceDiscourse1) :-
	change_utterance_type(ResolvedSourceDiscourse, whq, ynq, ResolvedSourceDiscourse1).

change_whq_to_dcl(ResolvedSourceDiscourse, ResolvedSourceDiscourse1) :-
	change_utterance_type(ResolvedSourceDiscourse, whq, dcl, ResolvedSourceDiscourse1).

change_ynq_to_dcl(ResolvedSourceDiscourse, ResolvedSourceDiscourse1) :-
	change_utterance_type(ResolvedSourceDiscourse, ynq, dcl, ResolvedSourceDiscourse1).

change_utterance_type(ResolvedSourceDiscourse, Type, Type1, ResolvedSourceDiscourse1) :-
	get_utterance_marking(Type, TypeUtteranceMarking),
	get_utterance_marking(Type1, Type1UtteranceMarking),
	
	list_to_ord_set(ResolvedSourceDiscourse, ResolvedSourceDiscourseOS),
	list_to_ord_set(TypeUtteranceMarking, TypeUtteranceMarkingOS),
	ord_subtract(ResolvedSourceDiscourseOS, TypeUtteranceMarkingOS, ResolvedSourceDiscourseOSMinusType),

	ord_union(Type1UtteranceMarking, ResolvedSourceDiscourseOSMinusType, ResolvedSourceDiscourse1),
	!.
change_utterance_type(ResolvedSourceDiscourse, Type, Type1, ResolvedSourceDiscourse1) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [change_utterance_type(ResolvedSourceDiscourse, Type, Type1, ResolvedSourceDiscourse1)]),
	fail.

%----------------------------------------------------------------------

change_you_to_i(Representation, Representation1) :-
	replace_element_recursively(Representation, you, i, Representation1).

replace_element_recursively(Representation, Type, Type1, Representation1) :-
	get_utterance_marking(Type, [Marking]),
	get_utterance_marking(Type1, [Marking1]),
	replace_element_recursively1(Representation, Marking, Marking1, Representation1).
replace_element_recursively(Representation, Type, Type1, Representation1) :-
	format('~N*** Error: bad call: ~w~n',
	       [replace_element_recursively(Representation, Type, Type1, Representation1)]),
	fail.

replace_element_recursively1([], _Marking, _Marking1, []).
replace_element_recursively1([F | R], Marking, Marking1, [F1 | R1]) :-
	replace_element_recursively2(F, Marking, Marking1, F1),
	!,
	replace_element_recursively1(R, Marking, Marking1, R1).

replace_element_recursively2(Role=Content, _=Content, _=Content1, Role=Content1) :-
	!.
replace_element_recursively2(Marking, Marking, Marking1, Marking1) :-
	!.
replace_element_recursively2(Role=[clause, Clause], Marking, Marking1, Role=[clause, Clause1]) :-
	replace_element_recursively1(Clause, Marking, Marking1, Clause1),
	!.
replace_element_recursively2([clause, Clause], Marking, Marking1, [clause, Clause1]) :-
	replace_element_recursively1(Clause, Marking, Marking1, Clause1),
	!.
replace_element_recursively2(Other, _Marking, _Marking1, Other).

%----------------------------------------------------------------------

whq_representation(Representation) :-
	get_utterance_marking(whq, WHQUtteranceMarking),
	list_to_ord_set(Representation, RepresentationOS),
	list_to_ord_set(WHQUtteranceMarking, WHQUtteranceMarkingOS),
	ord_subset(WHQUtteranceMarkingOS, RepresentationOS),
	!.

get_utterance_marking(Type, Value) :-
	utterance_marking_config_file_tag_and_default_value(Type, Tag, Default),
	(   user:regulus_config(Tag, Value0) ->
	    substitute_in_term(Value0, '*uninstantiated*', _NewVar, Value) ;

	    Value = Default
	),
	
	!.

% utterance_marking_config_file_tag_and_default_value(Type, Tag, Default)

utterance_marking_config_file_tag_and_default_value(phrase, phrase_utterance_marking, [[utterance_type, phrase]]).
utterance_marking_config_file_tag_and_default_value(whq, whq_utterance_marking, [[utterance_type, whq]]).
utterance_marking_config_file_tag_and_default_value(ynq, ynq_utterance_marking, [[utterance_type, ynq]]).
utterance_marking_config_file_tag_and_default_value(dcl, dcl_utterance_marking, [[utterance_type, dcl]]).
utterance_marking_config_file_tag_and_default_value(you, second_person_pro_element, [[pronoun, you]]).
utterance_marking_config_file_tag_and_default_value(i, first_person_pro_element, [[pronoun, i]]).
% The interjection_tag is just an atom...
utterance_marking_config_file_tag_and_default_value(interjection_tag, interjection_tag, interjection).

