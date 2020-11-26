
:- ensure_loaded('$REGULUS/PrologLib/compatibility').


:- module(strcat_semantics,
	  [convert_rules_to_strcat_semantics_if_necessary/2,
	   normal_sem_to_strcat_sem/2,
	   unpack_strcat_semantics_if_necessary/2,
	   unpack_strcat_semantics_from_mrcp/2
	  ]
	 ).

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/Prolog/regulus_eval').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
'SICSTUS3/4'( ( :- use_module(library(charsio)) ), ( :- use_module('$REGULUS/PrologLib/compatibility_charsio') ) ).

%------------------------------------------------------------------------------------

convert_rules_to_strcat_semantics_if_necessary(RulesIn, RulesOut) :-
	(   using_strcat_semantics ->
	    convert_rules_to_strcat_semantics(RulesIn, RulesOut)
	;
	    otherwise ->
	    RulesIn = RulesOut
	),
	!.
convert_rules_to_strcat_semantics_if_necessary(_RulesIn, _RulesOut) :-
	format2error('~NError: unable to convert rules to strcat semantics~n', []),
	fail.

%------------------------------------------------------------------------------------

unpack_strcat_semantics_if_necessary(StrcatVal, Val) :-
	using_strcat_semantics,
	!,
	unpack_strcat_semantics(StrcatVal, Val).
unpack_strcat_semantics_if_necessary(Val, Val) :-
	!.

unpack_strcat_semantics([], []) :-
	!.
unpack_strcat_semantics([(Key=StrcatVal) | R], [(Key=Val) | R1]) :-
	unpack_strcat_semantics(StrcatVal, Val),
	!,
	unpack_strcat_semantics(R, R1).
unpack_strcat_semantics(Atom, Sem) :-
	atom_codes(Atom, Str),
	append(Str, ".", StrWithPeriod),
	read_from_chars(StrWithPeriod, RawSem),
	regulus_eval_text(RawSem, Sem).

%------------------------------------------------------------------------------------

/*
'{value:concat([], concat([[imp, concat([[tense, imperative], [subj, [[spec, pro], [head, you]]]], concat(concat(concat([[verb_type, trans]], [[verb, grab]]), [[obj, concat(concat([[spec, the_sing]], [[head, gun]]), [])]]), []))]], []))}'
*/

unpack_strcat_semantics_from_mrcp(RawSemAtom, StrcatSem) :-
	atom_codes(RawSemAtom, RawSemStr),
	parse_mrcp_strcat_string(RawSemStr, MRCPSem),
	unpack_strcat_semantics(MRCPSem, [_Key=StrcatSem]),
	!.
unpack_strcat_semantics_from_mrcp(RawSemAtom, StrcatSem) :-
	format2error('~NError: bad call: ~w~n',
		     [unpack_strcat_semantics_from_mrcp(RawSemAtom, StrcatSem)]),
	fail.

%------------------------------------------------------------------------------------

parse_mrcp_strcat_string(RawSemStr, MRCPSem) :-
	mrcp_strcat_string(MRCPSem, RawSemStr, []).

mrcp_strcat_string(MRCPSem) -->
	"{",
	mrcp_strcat_string_body(MRCPSem),
	"}".
	
mrcp_strcat_string_body([F | R]) -->
	mrcp_strcat_string_body_element(F),
	!,
	mrcp_strcat_string_body_rest(R).
mrcp_strcat_string_body([]) --> [].

mrcp_strcat_string_body_rest([]) --> [].
mrcp_strcat_string_body_rest(R) -->
	",",
	mrcp_strcat_string_body(R).

mrcp_strcat_string_body_element(Key=Val) -->
	mrcp_atom(Key),
	":",
	mrcp_atom(Val).

mrcp_atom(Atom) -->
	mrcp_atom_string(Str),
	{atom_codes(Atom, Str)}.

mrcp_atom_string([F | R]) -->
	[F],
	{mrcp_atom_char(F)},
	!,
	mrcp_atom_string(R).
mrcp_atom_string([]) --> [].

mrcp_atom_char(Char) :-
	\+ reserved_char_for_mrcp_atoms(Char).

reserved_char_for_mrcp_atoms(0'{).
reserved_char_for_mrcp_atoms(0'}).
reserved_char_for_mrcp_atoms(0':).

%------------------------------------------------------------------------------------


convert_rules_to_strcat_semantics([], []).
convert_rules_to_strcat_semantics([F | R], [F1 | R1]) :-
	convert_rule_to_strcat_semantics(F, F1),
	!,
	convert_rules_to_strcat_semantics(R, R1).

convert_rule_to_strcat_semantics(rule((CatName:FeatVals --> RHS), LineInfo),
				 rule((CatName:FeatVals1 --> RHS), LineInfo)) :-
	on_exception(
		     Exception, 
		     convert_feats_to_strcat_semantics(FeatVals, FeatVals1),
		     ( inform_about_regulus_exception(Exception, LineInfo), fail )
		    ),
	!.

convert_feats_to_strcat_semantics([], []).
% Unfortunately, converting gsem to strcat form causes problems in merge_globals.
convert_feats_to_strcat_semantics([SemFeat=Sem | R], [SemFeat=StrcatSem | R1]) :-
	%member(SemFeat, [sem, gsem]),
	member(SemFeat, [sem]),
	normal_sem_to_strcat_sem(Sem, StrcatSem),
	!,
	convert_feats_to_strcat_semantics(R, R1).
convert_feats_to_strcat_semantics([F | R], [F | R1]) :-
	!,
	convert_feats_to_strcat_semantics(R, R1).

%------------------------------------------------------------------------------------

% If it contains a strcat, assume we've got a specialised grammar we've already converted
normal_sem_to_strcat_sem(Sem, StrcatSem) :-
	contains_strcat(Sem),
	!,
	Sem = StrcatSem.
normal_sem_to_strcat_sem(Sem, StrcatSem) :-
	normal_sem_to_strcat_sem1(Sem, ArgList-ArgListOut, (CharsIn-CharsIn)-(CharsOut-[])),
	(   CharsOut = [] ->
	    ArgListOut = []
	;
	    otherwise ->
	    atom_codes(FinalArg, CharsOut),
	    ArgListOut = [FinalArg]
	),
	(   ArgList = [] ->
	    StrcatSem = []
	;
	    ArgList = [SingleArg] ->
	    StrcatSem = SingleArg
	;
	    otherwise ->
	    StrcatSem =.. [strcat | ArgList]
	),
	!.
normal_sem_to_strcat_sem(Sem, StrcatSem) :-
	format('~N*** Error: bad call: ~w~n', [normal_sem_to_strcat_sem(Sem, StrcatSem)]),
	fail.

normal_sem_to_strcat_sem1(Atom, ArgsIn-ArgsIn, (CharsInH-CharsInT)-(CharsInH-CharsOutT)) :-
	atomic(Atom),
	with_output_to_chars(format(S, '~w', [Atom]),
			     S, CharsInT, CharsOutT),
	!.
normal_sem_to_strcat_sem1(Var, ArgsIn-ArgsOut, (CharsInH-CharsInT)-(CharsOut-CharsOut)) :-
	var(Var),
	CharsInT = [],
	(   CharsInH = [] ->
	    ArgsIn = [Var | ArgsOut]
	;
	    otherwise ->
	    atom_codes(LastArg, CharsInH),
	    ArgsIn = [LastArg, Var | ArgsOut]
	),
	!.
normal_sem_to_strcat_sem1(KeyvalList, ArgsIn-ArgsOut, (CharsInH-CharsInT)-(CharsOut-CharsOut)) :-
	is_keyval_list(KeyvalList),
	CharsInT = [],
	normal_sem_to_strcat_sem_on_keyval_list(KeyvalList, KeyvalList1),
	(   CharsInH = [] ->
	    ArgsIn = [KeyvalList1 | ArgsOut]
	;
	    otherwise ->
	    atom_codes(LastArg, CharsInH),
	    ArgsIn = [LastArg, KeyvalList1 | ArgsOut]
	),
	!.
normal_sem_to_strcat_sem1(List, ArgsIn-ArgsOut, (CharsInH-CharsInT)-(CharsOutH-CharsOutT)) :-
	is_list(List),
	List \= [],
	with_output_to_chars(format(S1, '[', []), 
			     S1, CharsInT, CharsInNext),
	normal_sem_to_strcat_sem1_list(List, ArgsIn-ArgsOut, (CharsInH-CharsInNext)-(CharsOutH-CharsNextT)),
	with_output_to_chars(format(S2, ']', []), 
			     S2, CharsNextT, CharsOutT),
	!.
normal_sem_to_strcat_sem1(GSLFunctionTerm, ArgsIn-ArgsOut, (CharsInH-CharsInT)-(CharsOutH-CharsOutT)) :-
	compound(GSLFunctionTerm),
	functor(GSLFunctionTerm, F, N),
	(  gsl_function(F/N) ; F = concat ; F = strcat ),
	GSLFunctionTerm =.. [F | GSLFunctionTermArgs],
	with_output_to_chars(format(S1, '~w(', [F]), 
			     S1, CharsInT, CharsInNext),
	normal_sem_to_strcat_sem1_list(GSLFunctionTermArgs, ArgsIn-ArgsOut, (CharsInH-CharsInNext)-(CharsOutH-CharsNextT)),
	with_output_to_chars(format(S2, ')', []), 
			     S2, CharsNextT, CharsOutT),
	!.

normal_sem_to_strcat_sem1_list([FinalArg], ArgsIn-ArgsOut, CharsIn-CharsOut) :-
	normal_sem_to_strcat_sem1(FinalArg, ArgsIn-ArgsOut, CharsIn-CharsOut),
	!.
normal_sem_to_strcat_sem1_list([F | R], ArgsIn-ArgsOut, CharsIn-CharsOut) :-
	normal_sem_to_strcat_sem1(F, ArgsIn-ArgsNext1, CharsIn-(CharsNextH-CharsNext1T)),
	with_output_to_chars(format(S1, ', ', []), 
			     S1, CharsNext1T, CharsNext2T),
	normal_sem_to_strcat_sem1_list(R, ArgsNext1-ArgsOut, (CharsNextH-CharsNext2T)-CharsOut).

normal_sem_to_strcat_sem_on_keyval_list([], []).
normal_sem_to_strcat_sem_on_keyval_list([(F = Val) | R], [(F = Val1) | R1]) :-
	normal_sem_to_strcat_sem(Val, Val1),
	!,
	normal_sem_to_strcat_sem_on_keyval_list(R, R1).

%------------------------------------------------------------------------------------

is_keyval_list([]).
is_keyval_list([(F = _Val) | R]) :-
	atom(F),
	!,
	is_keyval_list(R).

contains_strcat(Sem) :-
	term_contains_functor(Sem, strcat/_).
