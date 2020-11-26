
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(parse_xml_rec_result,
	[parse_xml_rec_result/2,
	 parse_xml_rec_result/3,
	 test_parse_xml_rec_result/1
	]).

%----------------------------------------------------------------------

:- use_module('$REGULUS/Prolog/strcat_semantics').
:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(xml)).

%----------------------------------------------------------------------

test_parse_xml_rec_result(discard_sem) :-
	File = '$REGULUS/Examples/Calendar/doc/4-best.txt',
	absolute_file_name(File, AbsFile),
	read_file_to_string(AbsFile, String),
	atom_codes(Atom, String),
	format('~N~n~w~n~n', [Atom]),
	parse_xml_rec_result(String, Result),
	prettyprintq(Result).
test_parse_xml_rec_result(discard_sem_n9_1) :-
	File = '$DEME/doc/rec_result_1.txt',
	absolute_file_name(File, AbsFile),
	read_file_to_string(AbsFile, String),
	atom_codes(Atom, String),
	format('~N~n~w~n~n', [Atom]),
	parse_xml_rec_result(String, Result),
	prettyprintq(Result).
test_parse_xml_rec_result(discard_sem_n9_2) :-
	File = '$DEME/doc/rec_result_2.txt',
	absolute_file_name(File, AbsFile),
	read_file_to_string(AbsFile, String),
	atom_codes(Atom, String),
	format('~N~n~w~n~n', [Atom]),
	parse_xml_rec_result(String, Result),
	prettyprintq(Result).
test_parse_xml_rec_result(discard_sem_n9_2a) :-
	File = '$DEME/doc/rec_result_2a.txt',
	absolute_file_name(File, AbsFile),
	read_file_to_string(AbsFile, String),
	atom_codes(Atom, String),
	format('~N~n~w~n~n', [Atom]),
	parse_xml_rec_result(String, Result),
	prettyprintq(Result).
test_parse_xml_rec_result(n9_with_sem_1) :-
	File = '$DEME/doc/n9_with_sem_1.txt',
	absolute_file_name(File, AbsFile),
	read_file_to_string(AbsFile, String),
	atom_codes(Atom, String),
	format('~N~n~w~n~n', [Atom]),
	parse_xml_rec_result(String, keep_sem, Result),
	prettyprintq(Result).
test_parse_xml_rec_result(keep_sem) :-
	File = '$MED_SLT2/Runtime/Prolog/n_best_xml_example.txt',
	absolute_file_name(File, AbsFile),
	read_file_to_string(AbsFile, String),
	atom_codes(Atom, String),
	format('~N~n~w~n~n', [Atom]),
	parse_xml_rec_result(String, keep_sem, Result),
	prettyprintq(Result).
test_parse_xml_rec_result(slm) :-
	File = '$MED_SLT2/Runtime/Prolog/slm_xml_example.txt',
	absolute_file_name(File, AbsFile),
	read_file_to_string(AbsFile, String),
	atom_codes(Atom, String),
	format('~N~n~w~n~n', [Atom]),
	parse_xml_rec_result(String, Result),
	prettyprintq(Result).

%----------------------------------------------------------------------

parse_xml_rec_result(String, Result) :-
	parse_xml_rec_result(String, discard_sem, Result).

%----------------------------------------------------------------------

parse_xml_rec_result(String, SemTreatment, HypsList) :-
	xml_parse_or_complain(String, XMLTerm),
	replace_strings_with_atoms_in_xml(XMLTerm, XMLTerm1),
	xml_term_to_hyps_list(XMLTerm1, SemTreatment, HypsList),
	!.
parse_xml_rec_result(String, SemTreatment, Result) :-
	format2error('~N*** Error: bad call: ~w~n', [parse_xml_rec_result(String, SemTreatment, Result)]),
	fail.

xml_parse_or_complain(String, XMLTerm) :-
	xml_parse(String, XMLTerm),
	!.
xml_parse_or_complain(String, _XMLTerm) :-
	format2error('~N*** Error: unable to parse XML string to Prolog: ~s~n', [String]),
	fail.
	       
xml_term_to_hyps_list(XMLTerm, SemTreatment, HypsList) :-
	get_interpretation_list_from_result(XMLTerm, InterpretationList),
	interpretation_list_to_hyps_list(InterpretationList, SemTreatment, HypsList).

% Format has changed a couple of times. Try to handle all variants.
get_interpretation_list_from_result(XMLTerm, InterpretationList) :-
	XMLTerm = xml(_VersionInfo, 
		      [element(result,
			       _GrammarInfo,
			       InterpretationList)]),
	!.
get_interpretation_list_from_result(XMLTerm, InterpretationList) :-
	XMLTerm = xml([], 
		      [_VersionInfo, 
		       element(result, 
			       _GrammarInfo, 
			       InterpretationList)]),
	!.
get_interpretation_list_from_result(XMLTerm, _InterpretationList) :-
	format2error('~N*** Error: unable to find interpretation list in Prolog XML result:~n', []),
	prettyprint(XMLTerm),
	fail.

interpretation_list_to_hyps_list([], _SemTreatment, []).
interpretation_list_to_hyps_list([F | R], SemTreatment, [F1 | R1]) :-
	interpretation_to_hyp(F, SemTreatment, F1),
	!,
	interpretation_list_to_hyps_list(R, SemTreatment, R1).

/*
element(interpretation, [confidence=59], 
                      [element(instance, [], 
                               [element(value, [confidence=0], 
                                        ['\n                ((whq (apply (lambda x ((tense present) (existential there_is) (subj (x (duration ((spec next) (time_period week))))))) (\n            '])]),
                       element(input, 
                               [(mode)=speech, confidence=65, 
                                'timestamp-start'='2007-07-26T11:21:06.749', 
                                'timestamp-end'='2007-07-26T11:21:06.921'],
                               ['\n            what meetings are there next week\n            ',
                                element(extensions, [], 
                                        ['\n                ', 
                                         element('word-confidence',[],[' 58 64 20 77 67 70 ']),
                                         '\n            ']),
                                '\n        ']),
                       element(extensions, [], 
                               [element(probability,[],[' -3069 ']), 
                                element('nl-probability',[],[' 0 ']), 
                                element('necessary-word-confidence',[],[' 0 '])])]),
*/

% Format for dialogue server (Nuance 8.5)
interpretation_to_hyp(Interpretation, discard_sem, Hyp) :-
	Interpretation = element(interpretation, [confidence=Confidence], 
				 [_RecResultElement,
				  element(input, 
					  _ModeInfo,
					  [RawRecStringAtom | _Rest]) |
				  _OtherElements]),
	split_atom_into_words(RawRecStringAtom, Words),
	join_with_spaces(Words, RecStringAtom),
	Hyp = nbest_hyp(Confidence, RecStringAtom).
% Format for dialogue server (Nuance 9, discard semantics)
interpretation_to_hyp(Interpretation, discard_sem, Hyp) :-
	Interpretation = element(interpretation, [grammar=_Grammar, confidence=Confidence], 
				 [element(input, 
					  _ModeInfo,
					  [RawRecStringAtom0 | _Rest]) |
				 _OtherElements]),
	replace_double_quotes_with_single(RawRecStringAtom0, RawRecStringAtom),
	split_atom_into_words(RawRecStringAtom, Words),
	join_with_spaces(Words, RecStringAtom),
	Hyp = nbest_hyp(Confidence, RecStringAtom).
/*
[element(interpretation, 
        [(grammar = 'session:testgrammar@paideiacomputing.com'), (confidence=0.81)],
        [element(input,[(mode)=speech],['grab the gun']), 
         element(instance, [], 
                 [element('SWI_literal', [], 
                          ['grab the gun']),
                  element(value, [confidence=81], 
                          ['concat([], concat([[imp, concat([[tense, imperative], [subj, [[spec,pro], [head, you]]]], concat(concat(concat([[verb_type, trans]], [[verb, grab]]), [[obj, concat(concat([[spec, the_sing]], [[head, gun]]), [])]]), []))]], []))']),
                  element('SWI_grammarName', [], 
                          ['session:testgrammar@paideiacomputing.com']),
                  element('SWI_meaning', [], 
                          ['{value:concat([], concat([[imp, concat([[tense, imperative], [subj, [[spec, pro], [head, you]]]], concat(concat(concat([[verb_type, trans]], [[verb, grab]]), [[obj, concat(concat([[spec, the_sing]], [[head, gun]]), [])]]), []))]], []))}'])])]),
*/
% Format for dialogue server (Nuance 9, strcat semantics)
interpretation_to_hyp(Interpretation, keep_sem_strcat, Hyp) :-
	Interpretation = element(interpretation, [grammar=_Grammar, confidence=Confidence], 
				 [element(input, 
					  _ModeInfo,
					  [RawRecStringAtom0 | _Rest]) |
				 OtherElements]),
	using_strcat_semantics,
	replace_double_quotes_with_single(RawRecStringAtom0, RawRecStringAtom),
	split_atom_into_words(RawRecStringAtom, Words),
	join_with_spaces(Words, RecStringAtom),
	member(element(instance, _, InstanceElements), OtherElements),
	member(element('SWI_meaning', _, [RawSem]), InstanceElements),
	unpack_strcat_semantics_from_mrcp(RawSem, LF),
	Hyp = nbest_hyp(Confidence, string_and_lf(RecStringAtom, LF)).

% Format for translation server
interpretation_to_hyp(Interpretation, keep_sem, Hyp) :-
	Interpretation = element(interpretation, [confidence=Confidence], 
				 [element(instance, _, 
					  [element(value, _MoreConfInfo, [LispFormattedSemAtom])]),
				  element(input, 
					  _ModeInfo,
					  [RawRecStringAtom | _Rest]) |
				  _OtherElements]),
	split_atom_into_words(RawRecStringAtom, Words),
	join_with_spaces(Words, RecStringAtom),
	parse_lisp_formatted_sem_value(LispFormattedSemAtom, LF),
	Hyp = hyp(RecStringAtom, Confidence, LF).
	
%----------------------------------------------------------------------

replace_strings_with_atoms_in_xml(Atom, Atom) :-
	atomic(Atom),
	!.
replace_strings_with_atoms_in_xml(pcdata(String), Atom) :-
	is_prolog_string(String),
	(   safe_number_codes(Atom, String)
	;
	    atom_codes(Atom, String)
	),
	!.
replace_strings_with_atoms_in_xml(String, Atom) :-
	is_prolog_string(String),
	(   safe_number_codes(Atom, String)
	;
	    atom_codes(Atom, String)
	),
	!.
replace_strings_with_atoms_in_xml(Term, Term1) :-
	functor(Term, F, N),
	functor(Term1, F, N),
	replace_strings_with_atoms_in_xml_args(N, Term, Term1).

replace_strings_with_atoms_in_xml_args(I, _Term, _Term1) :-
	I =< 0,
	!.
replace_strings_with_atoms_in_xml_args(I, Term, Term1) :-
	I > 0,
	arg(I, Term, Arg),
	arg(I, Term1, Arg1),
	replace_strings_with_atoms_in_xml(Arg, Arg1),
	I1 is I - 1,
	!,
	replace_strings_with_atoms_in_xml_args(I1, Term, Term1).

%----------------------------------------------------------------------

parse_lisp_formatted_sem_value(LispFormattedSemAtom, LF) :-
	atom_codes(LispFormattedSemAtom, LispFormattedSemString),
	parse_s_expression(LispFormattedSemString, LF),
	!.
parse_lisp_formatted_sem_value(LispFormattedSemAtom, LF) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [parse_lisp_formatted_sem_value(LispFormattedSemAtom, LF)]),
	fail.

parse_s_expression(LispFormattedString, LF) :-
	s_expression(LF, LispFormattedString, []),
	!.
parse_s_expression(LispFormattedString, _LF) :-
	format2error('~N*** Error: unable to treat "~s" as S-expression~n',
		     [LispFormattedString]),
	fail.

s_expression(S) -->
	optional_whitespaces,
	!,
	s_expression1(S),
	optional_whitespaces.

s_expression1(List) -->
	"(",
	!,
	s_expression_list(List),
	")".
s_expression1(Word) -->
	word(Word).

word(Word) -->
	non_special_char_sequence(Chars),
	{Chars \== []},
	{atom_codes(Word, Chars)},
	!.

s_expression_list([F | R]) -->
	s_expression(F),
	!,
	s_expression_list(R).
s_expression_list([]) -->
	optional_whitespaces,
	!.

optional_whitespaces -->
	[F],
	{whitespace_char(F)},
	!,
	optional_whitespaces.
optional_whitespaces -->
	[].

non_special_char_sequence([F | R]) -->
	[F],
	{non_special_char(F)},
	!,
	non_special_char_sequence(R).
non_special_char_sequence([]) -->
	[].

non_special_char(X) :-
	\+ special_char(X).

special_char(0'() :-
	!.
special_char(0')) :-
	!.
special_char(X) :-
	whitespace_char(X).

replace_double_quotes_with_single(Atom, Atom1) :-
	atom_codes(Atom, Str),
	replace_double_quotes_with_single_str(Str, Str1),
	atom_codes(Atom1, Str1).

replace_double_quotes_with_single_str([], []).
replace_double_quotes_with_single_str([F | R], [F1 | R1]) :-
	(   F = 0'" ->
	    F1 = 0'\'
	;
	    F1 = F
	),
	!,
	replace_double_quotes_with_single_str(R, R1).

