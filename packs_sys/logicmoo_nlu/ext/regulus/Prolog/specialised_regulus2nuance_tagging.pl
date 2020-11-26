
% specialised_regulus2nuance_tagging.pl

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(specialised_regulus2nuance_tagging,
	  [specialised_regulus2nuance_tagging/3,
	   specialised_regulus2nuance_tagging/4,
	   specialised_regulus2nuance_tagging/5]
	 ).

:- use_module('$REGULUS/Prolog/regulus_declarations').
:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(library(ordsets)).

%---------------------------------------------------------------

/*

specialised_regulus2nuance_tagging(+RegulusGrammarFile, +SpecFile, +TaggingGrammarFile, +Debug, +TopGrammar)

- RegulusGrammarFile is a "no_binarise" specialised Regulus grammar file

- SpecFile is a file of items of the form

    tagging_class(<ClassId>, <Examples>)

    where

       <Grammar> is an atom that can be used as a Nuance grammar name
       Examples is a list of at least two lexical items, either atoms or comma-lists
       
- TaggingGrammarFile is an output Nuance tagging grammar file
  (we also create an equivalence classes file with a related name)

- Debug is one of {debug, nodebug}

- TopGrammar is the name of the top-level generated grammar

TaggingGrammarFile is created from RegulusGrammarFile and SpecFile by constructing
one tagging grammar for each tagging_class declaration in SpecFile. The tagging
grammar for tagging_class(<Grammar>, <Examples>) is constructed as follows:

a) Go through RegulusGrammarFile finding the lexicon entries matching <Examples>

b) Construct the anti-unification of the LHS categories in all these
lexicon entries, to create a category Pattern.

c) Find all Words such that there is a lexicon entry matching Pattern --> Words

d) Grammar is

<Grammar>
[
Words_1
Words_2
...
Words_n
]

The top-level grammar is

.MAIN
[
<Grammar_1>
<Grammar_2>
...
<Grammar_n>
]

We also write an "equivalences file" (this is primarily useful for the MedSLT help system).
Each grammar is one line. For example, the grammar

Increasing__tagging_sore_throat
[
   increasing
   decreasing
   unchanging
]

is written as

# Increasing__tagging_sore_throat
\bincreasing\b|\bdecrasing\b|\bunchanging\b

*/

specialised_regulus2nuance_tagging(RegulusGrammarFile, SpecFile, TaggingGrammarFile) :-
	specialised_regulus2nuance_tagging(RegulusGrammarFile, SpecFile, TaggingGrammarFile, no_debug, '.MAIN').

specialised_regulus2nuance_tagging(RegulusGrammarFile, SpecFile, TaggingGrammarFile, Debug) :-
	specialised_regulus2nuance_tagging(RegulusGrammarFile, SpecFile, TaggingGrammarFile, Debug, '.MAIN').

specialised_regulus2nuance_tagging(RegulusGrammarFile, SpecFile, TaggingGrammarFile, Debug, TopGrammar) :-
	read_spec_file(SpecFile, RawSpecs),
	read_regulus_grammar_file(RegulusGrammarFile, RegulusGrammar, LexEntries),
	compile_spec(RawSpecs, RegulusGrammar, Specs),
	extract_tagging_grammars(Specs, RegulusGrammar, TaggingGrammar),
	warn_about_lex_entries_not_in_classes(TaggingGrammar, LexEntries, Debug),
	add_suffixes_to_tagging_grammar(TaggingGrammar, TopGrammar, TaggingGrammarWithSuffixes),
	write_tagging_grammar(TaggingGrammarWithSuffixes, TopGrammar, TaggingGrammarFile),
	write_equivalences(TaggingGrammar, TaggingGrammarFile),
	!.
specialised_regulus2nuance_tagging(RegulusGrammarFile, SpecFile, TaggingGrammarFile, Debug, TopGrammar) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [specialised_regulus2nuance_tagging(RegulusGrammarFile, SpecFile, TaggingGrammarFile, Debug, TopGrammar)]),
	fail.

%---------------------------------------------------------------

read_spec_file(SpecFile, RawSpecs) :-
	absolute_file_name(SpecFile, AbsSpecFile),
	prolog_file_to_list(AbsSpecFile, RawSpecs),
	length(RawSpecs, N),
	format('~N--- Read grammar spec file (~d items) ~w~n', [N, AbsSpecFile]),
	check_specs(RawSpecs, ok-Status),
	Status = ok,
	!.
read_spec_file(SpecFile, RawSpecs) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [read_spec_file(SpecFile, RawSpecs)]),
	fail.

check_specs([], Status-Status).
check_specs([F | R], StatusIn-StatusOut) :-
	check_spec(F, StatusIn-StatusNext),
	!,
	check_specs(R, StatusNext-StatusOut).

check_spec(Spec, StatusIn-StatusIn) :-
	Spec = tagging_class(ClassId, Examples),
	is_ok_nuance_grammar_name(ClassId),
	is_list_of_atoms_or_comma_atom_lists(Examples),
	!.
check_spec(Spec, _StatusIn-no_ok) :-
	format2error('~N*** Error: bad term in spec file: ~w~n', [Spec]),
	!.

is_ok_nuance_grammar_name(ClassId) :-
	atom(ClassId),
	atom_codes(ClassId, ClassIdChars),
	ClassIdChars = [F | _R],
	0'A =< F, F =< 0'Z,
	!.

is_list_of_atoms_or_comma_atom_lists([]).
is_list_of_atoms_or_comma_atom_lists([F | R]) :-
	is_atom_or_comma_atom_list(F),
	!,
	is_list_of_atoms_or_comma_atom_lists(R).

%---------------------------------------------------------------

read_regulus_grammar_file(RegulusGrammarFile, RegulusGrammar, LexEntries) :-
	absolute_file_name(RegulusGrammarFile, AbsRegulusGrammarFile),
	prolog_file_to_list(AbsRegulusGrammarFile, RegulusGrammar0),
	strip_frequency_labels_in_list(RegulusGrammar0, RegulusGrammar),
	length(RegulusGrammar, N),
	count_lexical_items(RegulusGrammar, NLexical, LexEntries),
	format('~N--- Read grammar file (~d items, ~d lexical items) ~w~n', [N, NLexical, AbsRegulusGrammarFile]),
	!.
read_regulus_grammar_file(RegulusGrammarFile, RegulusGrammar) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [read_regulus_grammar_file(RegulusGrammarFile, RegulusGrammar)]),
	fail.

strip_frequency_labels_in_list([], []).
strip_frequency_labels_in_list([F | R], [F1 | R1]) :-
	strip_frequency_label(F, F1),
	!,
	strip_frequency_labels_in_list(R, R1).

strip_frequency_label(frequency_labelled_item(_N, Rule), Rule) :-
	!.
strip_frequency_label(Other, Other).

count_lexical_items(RegulusGrammar, NLexical, LexicalItems) :-
	findall(Words,
		(   member(Item, RegulusGrammar),
		    is_lexical_regulus_rule(Item, Words)
		),
		LexicalItems),
	length(LexicalItems, NLexical).

is_lexical_regulus_rule((_LHS --> RHS), RHS) :-
        is_atom_or_comma_atom_list(RHS).
	
%---------------------------------------------------------------

compile_spec([], _RegulusGrammar, []).
compile_spec([tagging_class(ClassId, Examples) | R], RegulusGrammar, [F1 | R1]) :-
	compile_spec_item(tagging_class(ClassId, Examples), RegulusGrammar, F1),
	!,
	compile_spec(R, RegulusGrammar, R1).
compile_spec([tagging_class(ClassId, Examples) | R], RegulusGrammar, R1) :-
	format('~N*** Warning: unable to compile class "~w", examples "~w",~n', [ClassId, Examples]),
	!,
	compile_spec(R, RegulusGrammar, R1).
compile_spec([F | _], _RegulusGrammar, _) :-
	format2error('~N*** Error: bad item ~w in spec file~n', [F]),
	fail.

compile_spec_item(tagging_class(ClassId, Examples),
		  RegulusGrammar,
		  tagging_pattern(ClassId, Pattern)) :-
	get_cats_for_examples(Examples, RegulusGrammar, Cats),
	term_subsumer_on_list(Cats, Pattern),
	!.

get_cats_for_examples([], _RegulusGrammar, []).
get_cats_for_examples([F | R], RegulusGrammar, [F1 | R1]) :-
	get_cat_for_example(F, RegulusGrammar, F1),
	!,
	get_cats_for_examples(R, RegulusGrammar, R1).

/*

% Single example of rule of form "n-->hantavirus,infections"
% [ lexicon ] 

n:[sem=[[secondary_symptom,hantavirus_infections]],agr=1/\plur\/(2/\plur\/(3/\plur)),conj=n,n_of_mod_type=none,n_post_mod_type=none,n_pre_mod_type=none,sem_n_type=symptom,takes_about_pp=n,takes_attrib_pp=n,takes_cost_pp=n,takes_date_pp=n,takes_det_type=def\/(quant\/null),takes_duration_pp=n,takes_frequency_pp=n,takes_from_pp=n,takes_loc_pp=n,takes_passive_by_pp=none,takes_time_pp=n,takes_to_pp=n,takes_with_pp=n] --> hantavirus, infections.

*/

get_cat_for_example(Words, RegulusGrammar, Cat) :-
	member( (Cat --> Words), RegulusGrammar),
	!.
get_cat_for_example(Words, _RegulusGrammar, _Cat) :-
	format('~N*** Warning: unable to find example of "~w" in Regulus grammar',
	      [Words]),
	fail.

term_subsumer_on_list([X, Y], Subsumer) :-
	!,
	term_subsumer(X, Y, Subsumer).
term_subsumer_on_list([X], X) :-
	!.
term_subsumer_on_list([X, Y | R], Subsumer) :-
	term_subsumer(X, Y, Subsumer1),
	!,
	term_subsumer_on_list([Subsumer1 | R], Subsumer).

%---------------------------------------------------------------

extract_tagging_grammars([], _RegulusGrammar, []).
extract_tagging_grammars([F | R], RegulusGrammar, [F1 | R1]) :-
	extract_tagging_grammar(F, RegulusGrammar, F1),
	!,
	extract_tagging_grammars(R, RegulusGrammar, R1).

extract_tagging_grammar(tagging_pattern(ClassId, Pattern),
			RegulusGrammar,
			tagging_grammar(ClassId, Examples)) :-
	extract_tagging_grammar1(RegulusGrammar, Pattern, Examples),
	(   Examples = [] ->
	    format2error('~N*** Error: no examples found for: ~w~n',
			 [tagging_pattern(ClassId, Pattern)]),
	    fail
	;
	    true
	),
	!.
extract_tagging_grammar(F, _RegulusGrammar, _F1) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [extract_tagging_grammar(F, '...', '...')]),
	fail.

extract_tagging_grammar1([], _Pattern, []).
extract_tagging_grammar1([(LHS --> Words) | R], Pattern, [Words | R1]) :-
	safe_subsumes_chk(Pattern, LHS),
	is_atom_or_comma_atom_list(Words),
	!,
	extract_tagging_grammar1(R, Pattern, R1).
extract_tagging_grammar1([_F | R], Pattern, R1) :-
	!,
	extract_tagging_grammar1(R, Pattern, R1).

%---------------------------------------------------------------

warn_about_lex_entries_not_in_classes(TaggingGrammar, LexicalItemsFromRegulusGrammar, debug) :-
	statistics_for_tagging_grammar(TaggingGrammar, _NTags, _NLexicalItems, LexicalItemsFromTaggingGrammar),
	list_to_ord_set(LexicalItemsFromRegulusGrammar, LexicalItemsFromRegulusGrammarOS),
	list_to_ord_set(LexicalItemsFromTaggingGrammar, LexicalItemsFromTaggingGrammarOS),
	ord_subtract(LexicalItemsFromRegulusGrammarOS, LexicalItemsFromTaggingGrammarOS, DifferenceOS),
	format('~N~n--- Following items not assigned to any class:~n', []),
	format('~w~n~n', [DifferenceOS]),
	!.
warn_about_lex_entries_not_in_classes(_TaggingGrammar, _LexicalItemsFromRegulusGrammar, _Other) :-
	!.

%---------------------------------------------------------------

add_suffixes_to_tagging_grammar(TaggingGrammar, TopGrammar, TaggingGrammarWithSuffixes) :-
	top_grammar_to_suffix(TopGrammar, Suffix),
	add_suffixes_to_tagging_grammar1(TaggingGrammar, Suffix, TaggingGrammarWithSuffixes),
	!.
add_suffixes_to_tagging_grammar(TaggingGrammar, TopGrammar, TaggingGrammarWithSuffixes) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [add_suffixes_to_tagging_grammar(TaggingGrammar, TopGrammar, TaggingGrammarWithSuffixes)]),
	fail.

top_grammar_to_suffix(TopGrammar, Suffix) :-
	atom_codes(TopGrammar, TopGrammarChars),
	strip_leading_period(TopGrammarChars, TopGrammarChars1),
	strip_leading_MAIN_if_possible(TopGrammarChars1, SuffixChars),
	atom_codes(Suffix, SuffixChars),
	!.
top_grammar_to_suffix(TopGrammar, Suffix) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [top_grammar_to_suffix(TopGrammar, Suffix)]),
	fail.

strip_leading_period([0'. | Rest], Rest).

strip_leading_MAIN_if_possible(Chars, Rest) :-
	append("MAIN", Rest, Chars),
	!.
strip_leading_MAIN_if_possible(Chars, Chars).

add_suffixes_to_tagging_grammar1([], _Suffix, []).
add_suffixes_to_tagging_grammar1([F | R], Suffix, [F1 | R1]) :-
	add_suffixes_to_single_tagging_grammar(F, Suffix, F1),
	!,
	add_suffixes_to_tagging_grammar1(R, Suffix, R1).

add_suffixes_to_single_tagging_grammar(tagging_grammar(GrammarName, RHSList),
				       Suffix,
				       tagging_grammar(GrammarNameWithSuffix, RHSList)) :-
	format_to_atom('~w_~w', [GrammarName, Suffix], GrammarNameWithSuffix),
	!.

%---------------------------------------------------------------

write_tagging_grammar(TaggingGrammar, TopGrammar, TaggingGrammarFile) :-
	absolute_file_name(TaggingGrammarFile, AbsTaggingGrammarFile),
	
	open(AbsTaggingGrammarFile, write, S),
	write_tagging_grammar1(TaggingGrammar, TopGrammar, S),
	close(S),
	
	statistics_for_tagging_grammar(TaggingGrammar, NTags, NLexicalItems, _LexicalItems),
	format('~N--- Written tagging grammar file (~d tags, ~d lexical items) ~w~n',
	       [NTags, NLexicalItems, AbsTaggingGrammarFile]),
	!.
write_tagging_grammar(TaggingGrammar, TopGrammar, TaggingGrammarFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [write_tagging_grammar(TaggingGrammar, TopGrammar, TaggingGrammarFile)]),
	fail.

write_tagging_grammar1(TaggingGrammar, TopGrammar, S) :-
	top_level_tagging_grammar(TaggingGrammar, TopGrammar, TopLevelGrammar),
	write_single_tagging_grammar(TopLevelGrammar, S),
	write_tagging_grammar_list(TaggingGrammar, S).

top_level_tagging_grammar(TaggingGrammar, TopGrammar, TopLevelGrammar) :-
	findall(GrammarName,
		member(tagging_grammar(GrammarName, _RHSList), TaggingGrammar),
		GrammarNames),
	TopLevelGrammar = tagging_grammar(TopGrammar, GrammarNames),
	!.
top_level_tagging_grammar(TaggingGrammar, TopGrammar, TopLevelGrammar) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [top_level_tagging_grammar(TaggingGrammar, TopGrammar, TopLevelGrammar)]),
	fail.

write_tagging_grammar_list([], _S).
write_tagging_grammar_list([F | R], S) :-
	write_single_tagging_grammar(F, S),
	!,
	write_tagging_grammar_list(R, S).

write_single_tagging_grammar(tagging_grammar(LHS, RHSList), S) :-
	format(S, '~N~n~w~n', [LHS]),
	format(S, '~N[~n', []),
	write_single_tagging_grammar_rhs_list(RHSList, S),
	format(S, '~N]~n', []),
	!.
write_single_tagging_grammar(G, S) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [write_single_tagging_grammar(G, S)]),
	fail.

write_single_tagging_grammar_rhs_list([], _S).
write_single_tagging_grammar_rhs_list([F | R], S) :-
	write_single_tagging_grammar_rhs(F, S),
	!,
	write_single_tagging_grammar_rhs_list(R, S).

write_single_tagging_grammar_rhs(Atom, S) :-
	atom(Atom),
	format(S, '~N   ~w~n', [Atom]),
	!.
write_single_tagging_grammar_rhs(CommaList, S) :-
	comma_list_to_list(CommaList, List),
	join_with_spaces(List, ListAtom),
	format(S, '~N   ( ~w )~n', [ListAtom]),
	!.
write_single_tagging_grammar_rhs(F, S) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [write_single_tagging_grammar_rhs(F, S)]),
	fail.

%---------------------------------------------------------------

write_equivalences(TaggingGrammar, TaggingGrammarFile) :-
	make_equivalences_file_name_from_tagging_grammar(TaggingGrammarFile, EquivFile),
	
	open(EquivFile, write, S),
	write_equivalences1(TaggingGrammar, S),
	close(S),

	length(TaggingGrammar, N),
	format('--- Written equivalences file (~d entries): ~w~n~n', [N, EquivFile]),
	!.
write_equivalences(TaggingGrammar, TaggingGrammarFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [write_equivalences(TaggingGrammar, TaggingGrammarFile)]),
	fail.

make_equivalences_file_name_from_tagging_grammar(TaggingGrammarFile, EquivFile) :-
	absolute_file_name(TaggingGrammarFile, AbsTaggingGrammarFile),
	my_split_off_extension_from_pathname(AbsTaggingGrammarFile, MainTaggingName, Extension),
	format_to_atom('~w_equivs.~w', [MainTaggingName, Extension], EquivFile),
	!.
make_equivalences_file_name_from_tagging_grammar(TaggingGrammarFile, EquivFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [make_equivalences_file_name_from_tagging_grammar(TaggingGrammarFile, EquivFile)]),
	fail.

write_equivalences1([], _S).
write_equivalences1([F | R], S) :-
	write_equivalence(F, S),
	!,
	write_equivalences1(R, S).

write_equivalence(tagging_grammar(Grammar, RHSList), S) :-
	format(S, '~N# ~w~n', [Grammar]),
	write_equivalence1(RHSList, S).

write_equivalence1([], _S) :-
	!.
write_equivalence1([Last], S) :-
	write_equivalence_item(Last, S),
	!.
write_equivalence1([F | R], S) :-
	write_equivalence_item(F, S),
	format(S, '|', []),
	!,
	write_equivalence1(R, S).

write_equivalence_item(Item, S) :-
	format(S, '\\b', []),
	write_equivalence_item1(Item, S),
	format(S, '\\b', []),
	!.

write_equivalence_item1(Item, S) :-
	atomic(Item),
	format(S, '~w', [Item]),
	!.
write_equivalence_item1((F, R), S) :-
	write_equivalence_item1(F, S),
	format(S, ' ', []),
	write_equivalence_item1(R, S),
	!.

%---------------------------------------------------------------

statistics_for_tagging_grammar(TaggingGrammar, NTags, NLexicalItems, LexicalItems) :-
	length(TaggingGrammar, NTags),
	findall(LexicalItem,
		(   member(tagging_grammar(_LHS, RHSList), TaggingGrammar),
		    member(LexicalItem, RHSList)
		),
		LexicalItems),
	length(LexicalItems, NLexicalItems),
	!.
statistics_for_tagging_grammar(TaggingGrammar, NTags, NLexicalItems) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [statistics_for_tagging_grammar(TaggingGrammar, NTags, NLexicalItems)]),
	fail.

%---------------------------------------------------------------

my_split_off_extension_from_pathname(Pathname, PathnameWithoutExtension, Extension) :-
	atom_codes(Pathname, PathnameChars),
	portion_after_last_period(PathnameChars, ExtensionChars),
	append(PathnameWithoutExtensionChars, [0'. | ExtensionChars], PathnameChars),
	atom_codes(PathnameWithoutExtension, PathnameWithoutExtensionChars),
	atom_codes(Extension, ExtensionChars),
	!.
my_split_off_extension_from_pathname(Pathname, Pathname, '') :-
	!.
my_split_off_extension_from_pathname(Pathname, PathnameWithoutExtension, Extension) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [directory_and_file_for_pathname(Pathname, PathnameWithoutExtension, Extension)]),
	fail.

portion_after_last_period([0'. | R], End) :-
	\+ member(0'., R),
	!,	
	R = End.
portion_after_last_period([_F | R], End) :-
	portion_after_last_period(R, End).	

%------------------------------------------------------------------------------------


is_atom_or_comma_atom_list(V) :-
	var(V),
	!,
	fail.
is_atom_or_comma_atom_list(Atom) :-
	atom(Atom),
	!.
is_atom_or_comma_atom_list((P, Q)) :-
	atom(P),
	!,
	is_atom_or_comma_atom_list(Q).
