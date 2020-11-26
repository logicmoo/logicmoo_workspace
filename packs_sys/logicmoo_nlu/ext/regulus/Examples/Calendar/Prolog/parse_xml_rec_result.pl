
:- module(parse_xml_rec_result,
	[parse_xml_rec_result/2,
	 test/0
	]).

%----------------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(xml)).

%----------------------------------------------------------------------

test :-
	File = '$REGULUS/Examples/Calendar/doc/4-best.txt',
	absolute_file_name(File, AbsFile),
	read_file_to_string(AbsFile, String),
	atom_chars(Atom, String),
	format('~N~n~w~n~n', [Atom]),
	parse_xml_rec_result(String, Result),
	prettyprintq(Result).

%----------------------------------------------------------------------

parse_xml_rec_result(String, HypsList) :-
	xml_parse(String, XMLTerm),
	replace_strings_with_atoms_in_xml(XMLTerm, XMLTerm1),
	xml_term_to_hyps_list(XMLTerm1, HypsList),
	!.
parse_xml_rec_result(String, Result) :-
	format('~N*** Error: bad call: ~w~n', [parse_xml_rec_result(String, Result)]),
	fail.
	       
xml_term_to_hyps_list(XMLTerm, HypsList) :-
	XMLTerm = xml(_VersionInfo, 
		      [element(result,
			       _GrammarInfo,
			       InterpretationList)]),
	interpretation_list_to_hyps_list(InterpretationList, HypsList).

interpretation_list_to_hyps_list([], []).
interpretation_list_to_hyps_list([F | R], [F1 | R1]) :-
	interpretation_to_hyp(F, F1),
	!,
	interpretation_list_to_hyps_list(R, R1).

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
		      
interpretation_to_hyp(Interpretation, Hyp) :-
	Interpretation = element(interpretation, [confidence=Confidence], 
				 [_RecResultElement,
				  element(input, 
					  _ModeInfo,
					  [RawRecStringAtom | _Rest]),
				  _ExtensionsElement]),
	split_atom_into_words(RawRecStringAtom, Words),
	join_with_spaces(Words, RecStringAtom),
	Hyp = nbest_hyp(Confidence, RecStringAtom).
	
%----------------------------------------------------------------------

replace_strings_with_atoms_in_xml(Atom, Atom) :-
	atomic(Atom),
	!.
replace_strings_with_atoms_in_xml(pcdata(String), Atom) :-
	is_prolog_string(String),
	(   number_chars(Atom, String)
	;
	    atom_chars(Atom, String)
	),
	!.
replace_strings_with_atoms_in_xml(String, Atom) :-
	is_prolog_string(String),
	(   number_chars(Atom, String)
	;
	    atom_chars(Atom, String)
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
