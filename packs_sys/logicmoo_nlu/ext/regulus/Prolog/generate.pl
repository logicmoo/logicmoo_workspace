% generate.pl

% Top-level file for generation

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(generate,
	  [compile_orthography_file_or_files/2,
	   compile_original_script_orthography_file_or_files/2,
	   compile_recognition_orthography_file_or_files/2,
	   
	   compile_collocation_file/2,
	   compile_collocation_file_reversed/2,
	   compile_original_script_collocation_file/2,
	   compile_original_script_collocation_file_reversed/2,

	   improve_collocations/2,
	   improve_original_script_collocations/2,
	   improve_interlingua_collocations/2,
	   improve_interlingua_collocations/3,

	   interlingua_collocation_rules_module/2,
	   
	   fix_orthography/2,
	   fix_orthography/3,
	   fix_original_script_orthography/2,
	   fix_recognition_orthography/2,
	   fix_recognition_orthography_on_atom/2,
	   fix_orthography_simple_on_string/3,

	   special_orthography_module/2,
	   fix_special_orthography_on_atom/3,
	   
	   generate_all_surface_word_tuples/2,
	   generate_all_surface_word_tuples/3,
	   generate_surface_words/3,
	   generate_surface_words/4,

	   generator_sem_constants_missing_for_sem/2,

	   generate_original_script_result/3,
	   generate_gloss_result/3,

	   recogniser_generate_original_script_result/3,
	   recogniser_generate_gloss_result/3,
	   
	   extract_word_lists_from_tuples/2,
	   extract_tagged_words_from_tuples/2,

	   remove_null_items_in_generated_words/2]).

%----------------------------------------------------------------------

:- use_module('$REGULUS/Prolog/regulus_read').
:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/Prolog/analysis_constraints').

:- use_module(library(lists)).
:- use_module(library(ordsets)).
%:- use_module(library(assoc)).
:- use_module(library(terms)).
'SICSTUS3/4'( ( :- use_module(library(charsio)) ), ( :- use_module('$REGULUS/PrologLib/compatibility_charsio') ) ).
:- use_module(library(timeout)).

:- use_module('$REGULUS/PrologLib/utilities').

%----------------------------------------------------------------------

% Maximum time allowed for generation, in seconds

generation_time_limit(TimeLimit) :-
	current_predicate(user:regulus_config/2),
	user:regulus_config(generation_time_limit, TimeLimit0),
	(   number(TimeLimit0) ->
	    TimeLimit = TimeLimit0 ;
	    format2error('~N*** Error: generation time limit = ~w, should be a number. Using default value.~n', [TimeLimit0]),
	    fail
	),
	!.
generation_time_limit(20).

%----------------------------------------------------------------------

compile_orthography_file_or_files(InFileOrFiles, OutFile) :-
	compile_orthography_file_or_files(InFileOrFiles, normal, OutFile).

compile_original_script_orthography_file_or_files(InFileOrFiles, OutFile) :-
	compile_orthography_file_or_files(InFileOrFiles, original_script, OutFile).

compile_recognition_orthography_file_or_files(InFileOrFiles, OutFile) :-
	compile_orthography_file_or_files(InFileOrFiles, recognition, OutFile).

compile_orthography_file_or_files(InFileOrFiles, NormalOrOriginalScript, OutFile) :-
	absolute_file_name(OutFile, AbsOutFile),
	
	read_orthography_file_or_files(InFileOrFiles, RawOrthographyRules, RawOrthographyDeclarations),

	compile_orthography_rules(RawOrthographyDeclarations, RawOrthographyRules, NormalOrOriginalScript, CompiledTransferRules),
	length(CompiledTransferRules, NRules),

	list_to_prolog_file(CompiledTransferRules, AbsOutFile),
	format('~N -- Compiled orthography rule file (~d items) written to: ~w~n~n', [NRules, AbsOutFile]),
	!.
compile_orthography_file_or_files(InFile, _NormalOrOriginalScript, _OutFile) :-
	format2error('~N~n*** Error: unable to compile orthography rule file or files ~w~n', [InFile]),
	fail.

%----------------------------------------------------------------------

% compile_orthography_rules(+RawOrthographyDeclarations, +RawOrthographyRules, +NormalOrOriginalScript, +CompiledOrthographyRules)

compile_orthography_rules(RawOrthographyDeclarations, RawOrthographyRules, NormalOrOriginalScript, CompiledOrthographyRules) :-
	compile_orthography_declarations(RawOrthographyDeclarations, NormalOrOriginalScript,
					 []-OrthographyDeclarations, []-LetterClasses),
	compile_orthography_rewrite_rules(RawOrthographyRules, LetterClasses, NormalOrOriginalScript, OrthographyRules),
	append(OrthographyDeclarations, OrthographyRules, CompiledOrthographyRules),
	!.

compile_orthography_declarations([], _NormalOrOriginalScript, In-In, ClassesIn-ClassesIn).
compile_orthography_declarations([F | R], NormalOrOriginalScript, In-Out, ClassesIn-ClassesOut) :-
	compile_orthography_declaration(F, NormalOrOriginalScript, In-Next, ClassesIn-ClassesNext),
	!,
	compile_orthography_declarations(R, NormalOrOriginalScript, Next-Out, ClassesNext-ClassesOut).

compile_orthography_declaration(declaration(Declaration, _LineInfo), NormalOrOriginalScript, In-Out, ClassesIn-ClassesOut) :-
	compile_orthography_declaration(Declaration, NormalOrOriginalScript, In-Out, ClassesIn-ClassesOut),
	!.
compile_orthography_declaration(letter_class(ClassId, Letters), NormalOrOriginalScript, In-Out, ClassesIn-ClassesOut) :-
	ClassesOut = [ClassId | ClassesIn],
	compile_orthography_declaration1(Letters, ClassId, NormalOrOriginalScript, In-Out),
	!.
compile_orthography_declaration(Decl, NormalOrOriginalScript, Rules, Classes) :-
	format2error('~N~n*** Error: bad call: ~w~n', [compile_orthography_declaration(Decl, NormalOrOriginalScript, Rules, Classes)]),
	fail.

compile_orthography_declaration1([], _ClassId, _NormalOrOriginalScript, In-In).
compile_orthography_declaration1([F | R], ClassId, NormalOrOriginalScript, In-Out) :-
	(   NormalOrOriginalScript = original_script ->
	    Rule = original_script_letter_class(F, ClassId)
	;
	    NormalOrOriginalScript = recognition ->
	    Rule = recognition_letter_class(F, ClassId)
	;
	    otherwise ->
	    Rule = letter_class(F, ClassId)
	),
	!,
	compile_orthography_declaration1(R, ClassId, NormalOrOriginalScript, [Rule | In]-Out).

compile_orthography_rewrite_rules([], _LetterClasses, _NormalOrOriginalScript, []).
compile_orthography_rewrite_rules([F | R], LetterClasses, NormalOrOriginalScript, [F1 | R1]) :-
	compile_orthography_rewrite_rule(F, LetterClasses, NormalOrOriginalScript, F1),
	!,
	compile_orthography_rewrite_rules(R, LetterClasses, NormalOrOriginalScript, R1).

compile_orthography_rewrite_rule(rule(Rule, _LineInfo), LetterClasses, NormalOrOriginalScript, CompiledRule) :-
	compile_orthography_rewrite_rule(Rule, LetterClasses, NormalOrOriginalScript, CompiledRule),
	!.
compile_orthography_rewrite_rule(orthography_rewrite(LHS, RHS),
				 LetterClasses,
				 NormalOrOriginalScript,
				 Rule) :-
	preprocess_orthography_pattern(LHS, LetterClasses, LHS1, []-Substs1),
	preprocess_orthography_pattern(RHS, LetterClasses, RHS1, Substs1-Substs),
	check_for_inconsistent_orthography_rule_variables(LHS1, RHS1, Substs),
	make_orthography_rule_body(Substs, NormalOrOriginalScript, Body),
	(   NormalOrOriginalScript = original_script ->
	    Rule = ( original_script_orthography_rewrite(LHS1, RHS1) :- Body )
	;
	    NormalOrOriginalScript = recognition ->
	    Rule = ( recognition_orthography_rewrite(LHS1, RHS1) :- Body )
	;
	    otherwise ->
	    Rule = ( orthography_rewrite(LHS1, RHS1) :- Body )
	),
	!.
compile_orthography_rewrite_rule(F, LetterClasses, NormalOrOriginalScript, F1) :-
	format2error('~N~n*** Error: bad call: ~w~n',
		     [compile_orthography_rewrite_rule(F, LetterClasses, NormalOrOriginalScript, F1)]),
	fail.

preprocess_orthography_pattern([], _LetterClasses, [], Substs-Substs) :-
	!.
preprocess_orthography_pattern([Letter, NumberChar | R], LetterClasses, [Var | R1], SubstsIn-SubstsOut) :-
	safe_number_codes(Number, [NumberChar]),
	!,
	atom_codes(ClassId, [Letter]),
	(   member(ClassId, LetterClasses) ->
	    true
	;
	    format2error('~N*** Error: "~w" is not defined as a letter class~n', [ClassId]),
	    fail
	),
	find_or_create_orthography_subst(ClassId, Number, Var, SubstsIn-SubstsNext),
	!,
	preprocess_orthography_pattern(R, LetterClasses, R1, SubstsNext-SubstsOut).
preprocess_orthography_pattern([F | R], LetterClasses, [F | R1], SubstsIn-SubstsOut) :-
	preprocess_orthography_pattern(R, LetterClasses, R1, SubstsIn-SubstsOut).

check_for_inconsistent_orthography_rule_variables(_LHS, _RHS, _Substs).

find_or_create_orthography_subst(ClassId, Number, Var, SubstsIn-SubstsIn) :-
	member(subst(ClassId, Number, Var), SubstsIn),
	!.
find_or_create_orthography_subst(ClassId, Number, Var, SubstsIn-SubstsOut) :-
	NewSubst = subst(ClassId, Number, Var),
	SubstsOut = [NewSubst | SubstsIn],
	!.
find_or_create_orthography_subst(ClassId, Number, Var, Substs) :-
	format2error('~N~n*** Error: bad call: ~w~n',
		     [find_or_create_orthography_subst(ClassId, Number, Var, Substs)]),
	fail.

make_orthography_rule_body([], _NormalOrOriginalScript, true) :-
	!.
make_orthography_rule_body([Subst], NormalOrOriginalScript, Cond) :-
	orthography_subst_to_cond(Subst, NormalOrOriginalScript, Cond),
	!.
make_orthography_rule_body([F | R], NormalOrOriginalScript, (CondF, CondR)) :-
	orthography_subst_to_cond(F, NormalOrOriginalScript, CondF),
	!,
	make_orthography_rule_body(R, NormalOrOriginalScript, CondR).

orthography_subst_to_cond(subst(ClassId, _Number, Var), NormalOrOriginalScript, Cond) :-
	(   NormalOrOriginalScript = original_script ->
	    Cond = original_script_letter_class(Var, ClassId)
	;
	    NormalOrOriginalScript = recognition ->
	    Cond = recognition_letter_class(Var, ClassId)
	;
	    otherwise ->
	    Cond = letter_class(Var, ClassId) 	    
	),
	!.
orthography_subst_to_cond(Subst, NormalOrOriginalScript, Cond) :-
	format2error('~N~n*** Error: bad call: ~w~n',
		     [orthography_subst_to_cond(Subst, NormalOrOriginalScript, Cond)]),
	fail.

%----------------------------------------------------------------------

compile_collocation_file(InFile, OutFile) :-
	compile_collocation_file(InFile, OutFile, normal).

compile_collocation_file_reversed(InFile, OutFile) :-
	compile_collocation_file(InFile, OutFile, reversed).

compile_collocation_file(InFile, OutFile, Direction) :-
	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(OutFile, AbsOutFile),
	
	format('~N~n--- Compiling collocation rule file: ~w~n', [AbsInFile]),
	%prolog_file_to_list(AbsInFile, RawRules),
	read_collocation_file_or_files(AbsInFile, ExpandedRules, _Declarations),

	compile_collocation_rules(ExpandedRules, CompiledRules, Direction),
	length(CompiledRules, NRules),

	list_to_prolog_file(CompiledRules, AbsOutFile),
	format('~N~n--- Compiled collocation rule file (~d items) written to: ~w~n~n', [NRules, AbsOutFile]),
	!.
compile_collocation_file(InFile, _OutFile, _Direction) :-
	format2error('~N~n*** Error: unable to compile collocation rule file ~w~n', [InFile]),
	fail.
 
%----------------------------------------------------------------------

compile_original_script_collocation_file(InFile, OutFile) :-
	compile_original_script_collocation_file(InFile, OutFile, normal).

compile_original_script_collocation_file_reversed(InFile, OutFile) :-
	compile_original_script_collocation_file(InFile, OutFile, reversed).

compile_original_script_collocation_file(InFile, OutFile, Direction) :-
	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(OutFile, AbsOutFile),
	
	format('~N~n--- Compiling original_script_collocation rule file: ~w~n', [AbsInFile]),
	%prolog_file_to_list(AbsInFile, RawRules),
	read_collocation_file_or_files(AbsInFile, ExpandedRules),

	compile_original_script_collocation_rules(ExpandedRules, CompiledRules, Direction),
	length(CompiledRules, NRules),

	list_to_prolog_file(CompiledRules, AbsOutFile),
	format('~N~n--- Compiled original_script_collocation rule file (~d items) written to: ~w~n~n', [NRules, AbsOutFile]),
	!.
compile_original_script_collocation_file(InFile, _OutFile, _Direction) :-
	format2error('~N~n*** Error: unable to compile original_script_collocation rule file ~w~n', [InFile]),
	fail.

%----------------------------------------------------------------------

compile_collocation_rules([], [], _Direction).
compile_collocation_rules([F | R], [F1 | R1], Direction) :-
	compile_collocation(F, F1, Direction),
	!,
	compile_collocation_rules(R, R1, Direction).

compile_collocation(Item, Item1, Direction) :-
	valid_collocation_item(Item),
	compile_collocation1(Item, Item1, Direction),
	!.
compile_collocation(Item, _, _Direction) :-
	format2error('~N*** Error: unable to compile collocation item ~w~n', [Item]),
	fail.

valid_collocation_item(better_collocation(In, Out)) :-
	is_prolog_string(In),
	is_prolog_string(Out),
	!.

compile_collocation1(better_collocation(Out, In), Result, reversed) :-
	compile_collocation1(better_collocation(In, Out), Result, normal).
compile_collocation1(better_collocation(In, Out), Result, normal) :-
	string_to_diff_list(In, In1, InLength),
	string_to_diff_list(Out, Out1, _OutLength),
	Result = better_collocation_internal(In1, Out1, InLength).

%----------------------------------------------------------------------

compile_original_script_collocation_rules([], [], _Direction).
compile_original_script_collocation_rules([F | R], [F1 | R1], Direction) :-
	compile_original_script_collocation(F, F1, Direction),
	!,
	compile_original_script_collocation_rules(R, R1, Direction).

compile_original_script_collocation(Item, Item1, Direction) :-
	valid_original_script_collocation_item(Item),
	compile_original_script_collocation1(Item, Item1, Direction),
	!.
compile_original_script_collocation(Item, _, _Direction) :-
	format2error('~N*** Error: unable to compile original_script_collocation item ~w~n', [Item]),
	fail.

valid_original_script_collocation_item(better_original_script_collocation(In, Out)) :-
	is_prolog_string(In),
	is_prolog_string(Out),
	!.

compile_original_script_collocation1(better_original_script_collocation(Out, In), Result, reversed) :-
	compile_original_script_collocation1(better_original_script_collocation(In, Out), Result, normal).
compile_original_script_collocation1(better_original_script_collocation(In, Out), Result, normal) :-
	string_to_diff_list(In, In1, InLength),
	string_to_diff_list(Out, Out1, _OutLength),
	Result = better_original_script_collocation_internal(In1, Out1, InLength).

%----------------------------------------------------------------------
	
string_to_diff_list(String, DiffListIn-DiffListOut, Length) :-
	split_string_into_words(String, Words),
	length(Words, Length),
	list_into_diff_list(Words, DiffListIn, DiffListOut).

list_into_diff_list([], Out, Out).
list_into_diff_list([F | R], [F | R1], Out) :-
	list_into_diff_list(R, R1, Out).

%----------------------------------------------------------------------

fix_orthography(WordsIn, WordsOut) :-
	fix_orthography(WordsIn, user, WordsOut).

fix_orthography(WordsIn, WordsOut, Id) :-
	special_orthography_module(Id, Module),
	current_predicate(Module:orthography_rewrite/2),
	fix_orthography1(WordsIn, normal(Module), WordsOut),
	!.
fix_orthography(WordsIn, WordsIn, _Id).

fix_original_script_orthography(WordsIn, WordsOut) :-
	current_predicate(user:original_script_orthography_rewrite/2),
	fix_orthography1(WordsIn, original_script, WordsOut),
	!.
fix_original_script_orthography(WordsIn, WordsIn).

fix_recognition_orthography(WordsIn, WordsOut) :-
	current_predicate(user:recognition_orthography_rewrite/2),
	fix_orthography1(WordsIn, recognition, WordsOut),
	!.
fix_recognition_orthography(WordsIn, WordsIn).

fix_recognition_orthography_on_atom(AtomIn, AtomOut) :-
	fix_recognition_orthography([AtomIn], WordsOut),
	join_with_spaces(WordsOut, AtomOut).

special_orthography_module(user, user) :-
	!.
special_orthography_module(Id, Module) :-
	format_to_atom('special_orthography_~w', [Id], Module),
	!.

fix_special_orthography_on_atom(AtomIn, Id, AtomOut) :-
	special_orthography_module(Id, Module),
	current_predicate(Module:orthography_rewrite/2),
	fix_orthography1([AtomIn], normal(Module), WordsOut),
	join_with_spaces(WordsOut, AtomOut),
	!.
fix_special_orthography_on_atom(Atom, _Id, Atom).

%----------------------------------------------------------------------

fix_orthography1(WordsIn, NormalOrOriginalScript, WordsOut) :-
	join_with_spaces(WordsIn, AtomIn),
	atom_codes(AtomIn, CharsIn0),
	fix_orthography_on_string(CharsIn0, NormalOrOriginalScript, CharsOut),
	atom_codes(AtomOut, CharsOut),
	split_atom_into_words(AtomOut, WordsOut),
	!.

fix_orthography_on_string(CharsIn0, NormalOrOriginalScript, CharsOut) :-
	add_spaces_at_beginning_and_end(CharsIn0, CharsIn),
	fix_orthography_on_string1(CharsIn, NormalOrOriginalScript, CharsOut0),
	remove_spaces_at_beginning_and_end(CharsOut0, CharsOut),
	!.

fix_orthography_simple_on_string(CharsIn0, Module, CharsOut) :-
	current_predicate(Module:orthography_rewrite/2),
	add_spaces_at_beginning_and_end(CharsIn0, CharsIn),
	fix_orthography_simple_on_string1(CharsIn, Module, CharsOut0),
	remove_spaces_at_beginning_and_end(CharsOut0, CharsOut),
	!.
fix_orthography_simple_on_string(WordsIn, _Module, WordsIn).

fix_orthography_on_string1([], _NormalOrOriginalScript, []).
fix_orthography_on_string1(In, NormalOrOriginalScript, Out) :-
	(   NormalOrOriginalScript = normal(Module) ->
	    Module:orthography_rewrite(PrefixIn, PrefixOut)
	;
	    NormalOrOriginalScript = recognition ->
	    user:recognition_orthography_rewrite(PrefixIn, PrefixOut)
	;
	    otherwise ->
	    user:original_script_orthography_rewrite(PrefixIn, PrefixOut)
	),
	append(PrefixIn, Rest, In),
	append(PrefixOut, Rest, Next),
	!,
	fix_orthography_on_string1(Next, NormalOrOriginalScript, Out).
fix_orthography_on_string1([F | R], NormalOrOriginalScript, [F | R1]) :-
	fix_orthography_on_string1(R, NormalOrOriginalScript, R1).

fix_orthography_simple_on_string1([], _Module, []).
fix_orthography_simple_on_string1(In, Module, Out) :-
	Module:orthography_rewrite(PrefixIn, PrefixOut),
	append(PrefixIn, Rest, In),
	!,
	fix_orthography_simple_on_string1(Rest, Module, Rest1),
	append(PrefixOut, Rest1, Out).
fix_orthography_simple_on_string1([F | R], Module, [F | R1]) :-
	!,
	fix_orthography_simple_on_string1(R, Module, R1).

add_spaces_at_beginning_and_end(CharsIn, CharsOut) :-
	append_list([[0' ], CharsIn, [0' ]], CharsOut),
	!.
add_spaces_at_beginning_and_end(CharsIn, CharsOut) :-
	format2error('~N~n*** Error: bad call: ~w~n',
		     [add_spaces_at_beginning_and_end(CharsIn, CharsOut)]),
	fail.

remove_spaces_at_beginning_and_end(CharsIn, CharsOut) :-
	remove_spaces_at_beginning(CharsIn, CharsNext),
	remove_spaces_at_end(CharsNext, CharsOut),
	!.
remove_spaces_at_beginning_and_end(CharsIn, CharsOut) :-
	format2error('~N~n*** Error: bad call: ~w~n',
		     [remove_spaces_at_beginning_and_end(CharsIn, CharsOut)]),
	fail.

remove_spaces_at_beginning([0' | R], R1) :-
	!,
	remove_spaces_at_beginning(R, R1).
remove_spaces_at_beginning(Other, Other).

remove_spaces_at_end(String, "") :-
	all_spaces(String),
	!.
remove_spaces_at_end([F | R], [F | R1]) :-
	!,
	remove_spaces_at_end(R, R1).

all_spaces([]).
all_spaces([0' | R]) :-
	!,
	all_spaces(R).

%----------------------------------------------------------------------

improve_collocations(WordsIn, WordsOut) :-
	improve_collocations(WordsIn, user, WordsOut).

improve_interlingua_collocations(WordsIn, WordsOut) :-
	improve_interlingua_collocations(WordsIn, check_interlingua, WordsOut).

improve_interlingua_collocations(WordsIn, Tag, WordsOut) :-
	interlingua_collocation_rules_module(Tag, Module),
	improve_collocations(WordsIn, Module, WordsOut).

%----------------------------------------------------------------------

improve_collocations(WordsIn, Module, WordsOut) :-
	current_predicate(Module:better_collocation_internal/3),
	join_with_spaces(WordsIn, Atom),
	split_atom_into_words(Atom, WordsIn1),
	improve_collocations1(WordsIn1, Module, WordsOut),
	!.
improve_collocations(WordsIn, _Module, WordsIn).

improve_collocations1([], _Module, []).
improve_collocations1(WordsIn, Module, BestWordsOut) :-
	findall(MinusN-[WordsInRest, WordsOut, WordsOutRest],
		(   Module:better_collocation_internal(WordsIn-WordsInRest, WordsOut-WordsOutRest, N),
		    MinusN is -1 * N
		),
		Pairs),
	keysort(Pairs, SortedPairs),
	SortedPairs = [_BestMinusN-[BestWordsInRest, BestWordsOut, BestWordsOutRest] | _Tail],
	!,
	improve_collocations1(BestWordsInRest, Module, BestWordsOutRest).
improve_collocations1([F | R], Module, [F | R1]) :-
	improve_collocations1(R, Module, R1).

%----------------------------------------------------------------------

improve_original_script_collocations(WordsIn, WordsOut) :-
	current_predicate(user:better_original_script_collocation_internal/3),
	join_with_spaces(WordsIn, Atom),
	split_atom_into_words(Atom, WordsIn1),
	improve_original_script_collocations1(WordsIn1, WordsOut),
	!.
improve_original_script_collocations(WordsIn, WordsIn).

improve_original_script_collocations1([], []).
improve_original_script_collocations1(WordsIn, BestWordsOut) :-
	findall(MinusN-[WordsInRest, WordsOut, WordsOutRest],
		(   user:better_original_script_collocation_internal(WordsIn-WordsInRest, WordsOut-WordsOutRest, N),
		    MinusN is -1 * N
		),
		Pairs),
	keysort(Pairs, SortedPairs),
	SortedPairs = [_BestMinusN-[BestWordsInRest, BestWordsOut, BestWordsOutRest] | _Tail],
	!,
	improve_original_script_collocations1(BestWordsInRest, BestWordsOutRest).
improve_original_script_collocations1([F | R], [F | R1]) :-
	improve_original_script_collocations1(R, R1).

%----------------------------------------------------------------------

:- dynamic interlingua_collocation_rules_module_internal/2.

interlingua_collocation_rules_module(Tag, Module) :-
	interlingua_collocation_rules_module_internal(Tag, Module),
	!.
interlingua_collocation_rules_module(Tag, Module) :-
	format_to_atom('interlingua_~w', [Tag], Module),
	asserta(interlingua_collocation_rules_module_internal(Tag, Module)),
	!.

%----------------------------------------------------------------------

generator_sem_constants_missing_for_sem(Sem, Missing) :-
	current_predicate(generator:generation_sem_constant/1),
	get_sem_constants_from_sem(Sem, Constants0-[]),
	sort(Constants0, Constants),
	findall(Const,
		(   member(Const, Constants),
		    \+ generator:generation_sem_constant(Const),
		    \+ harmless_missing_constant(Const)
		),
		Missing0),
	sort(Missing0, Missing),
	!.
generator_sem_constants_missing_for_sem(_Sem, Missing) :-
	Missing = [],
	!.
generator_sem_constants_missing_for_sem(Sem, Missing) :-
	format2error('~N~n*** Error: bad call: ~w~n',
		     [generator_sem_constants_missing_for_sem(Sem, Missing)]),
	fail.

harmless_missing_constant(null).

%----------------------------------------------------------------------

generate_surface_words(Representation, PreferredGeneratedWords, OtherGeneratedWords) :-
	generate_surface_words(Representation, PreferredGeneratedWords, OtherGeneratedWords, _TaggedWords).

generate_surface_words(Representation, PreferredGeneratedWords, OtherGeneratedWords, AllTaggedWords) :-
	generate_surface_words(Representation, PreferredGeneratedWords, OtherGeneratedWords, AllTaggedWords, user).

generate_surface_words(Representation, PreferredGeneratedWords, OtherGeneratedWords, AllTaggedWords, Module) :-
	generate_all_surface_word_tuples(Representation, AllTuples, Module),
	AllTuples = [PreferredTuple | OtherTuples],
	member(words=PreferredGeneratedWords, PreferredTuple),
	extract_word_lists_from_tuples(OtherTuples, OtherGeneratedWords),
	extract_tagged_words_from_tuples(AllTuples, AllTaggedWords).

%----------------------------------------------------------------------

/*

Tuples are of the form

[words = <ListOfWords>,
 tagged_words = <ListOfTaggedWords>,
 tree = <Tree>
 preference_score = <Score>,
 preference_info = <PreferenceInfo>]

PreferenceInfo is a list of items of the form <PreferenceItem>-<Score>

*/

generate_all_surface_word_tuples(Representation, Tuples) :-
	generate_all_surface_word_tuples(Representation, Tuples, user).

generate_all_surface_word_tuples(Representation, Tuples, Module) :-
	timeouts_unavailable,
	!,
	generate_all_surface_word_tuples1(Representation, Tuples, Module).
generate_all_surface_word_tuples(Representation, Tuples, Module) :-
	generation_time_limit(TimeLimit),
	TimeLimitInMilliseconds is integer( 1000 * TimeLimit ),
	
	time_out(generate_all_surface_word_tuples1(Representation, Tuples0, Module),
		 TimeLimitInMilliseconds,
		 Result),

	(   Result = time_out ->
	    format2error('~N~n*** Error: time out (~d seconds) when trying to generate from ~w~n',
			 [TimeLimit, Representation]),
	    Tuples = [[words = [generation_timed_out],
		       tagged_words = [generation_timed_out/null]]] ;

	    otherwise ->
	    Tuples = Tuples0
	).

generate_all_surface_word_tuples1(Representation, Tuples) :-
	generate_all_surface_word_tuples1(Representation, Tuples, user).

generate_all_surface_word_tuples1(Representation, Tuples, Module) :-
	findall([words=Words, recognition_orthography_words=RecognitionOrthographyWords, tagged_words=TaggedWords,
		 tree=Tree,
		 instantiated_representation=Representation],
		(   generate_surface_words2(Representation, Words0, RecognitionOrthographyWords, TaggedWords0, Tree, Module),
		    remove_null_items_in_generated_words(Words0, Words),
		    remove_null_items_in_generated_words(TaggedWords0, TaggedWords)
		),
		Tuples0),
	safe_remove_duplicates(Tuples0, Tuples1),
	order_tuples_using_generation_preferences(Tuples1, Tuples, Module),
	!.

remove_null_items_in_generated_words([], []) :-
	!.
remove_null_items_in_generated_words([F | R], R1) :-
	null_item_in_generated_words(F),
	!,
	remove_null_items_in_generated_words(R, R1).
remove_null_items_in_generated_words([F | R], [F | R1]) :-
	!,
	remove_null_items_in_generated_words(R, R1).
remove_null_items_in_generated_words(X, Y) :-
	format2error('~N~n*** Error: bad call: ~w~n',
		     [remove_null_items_in_generated_words(X, Y)]),
	fail.

null_item_in_generated_words(X/_) :-
	null_item_in_generated_words(X).
null_item_in_generated_words(null).
null_item_in_generated_words('*null*').

generate_surface_words2(TargetRepresentation, TargetWords, RecognitionOrthographyWords, TaggedWords, Tree, Module) :-
	generator_module_for_translation_module(Module, Module1),
	Module1:generate(TargetRepresentation, Tree, TargetWords0),
	parse_tree_to_tagged_word_list(Tree, TaggedWords),
	improve_collocations(TargetWords0, TargetWords1),
	fix_orthography(TargetWords1, TargetWords, Module),
	fix_recognition_orthography(TargetWords0, RecognitionOrthographyWords).

generator_module_for_translation_module(user, generator) :-
	!.
generator_module_for_translation_module(Module, Module).

%----------------------------------------------------------------------

generate_original_script_result(Representation, PreferredGeneratedTree, OriginalScript) :-
	current_predicate(generator_original_script:generate/3),
	remove_lexical_and_line_info_in_parse_tree(PreferredGeneratedTree, PreferredGeneratedTreeSkeleton),
	generator_original_script:generate(Representation, PreferredGeneratedTreeSkeleton, OriginalScriptWords0),
	improve_original_script_collocations(OriginalScriptWords0, OriginalScriptWords1),
	fix_original_script_orthography(OriginalScriptWords1, OriginalScriptWords),
	join_with_spaces(OriginalScriptWords, OriginalScript0),
	remove_spaces_from_original_script_result_if_necessary(OriginalScript0, OriginalScript),
	!.
generate_original_script_result(_Representation, _PreferredGeneratedTree, OriginalScript) :-
	current_predicate(generator_original_script:generate/3),
	OriginalScript = 'WARNING: unable to generate in original script',
	!.
 
%----------------------------------------------------------------------

recogniser_generate_original_script_result(Representation, PreferredGeneratedTree, OriginalScript) :-
	current_predicate(recogniser_generator_original_script:generate/3),
	%remove_lexical_and_line_info_in_parse_tree(PreferredGeneratedTree, PreferredGeneratedTreeSkeleton),
	remove_lexical_and_file_info_in_parse_tree(PreferredGeneratedTree, PreferredGeneratedTreeSkeleton),
	recogniser_generator_original_script:generate(Representation, PreferredGeneratedTreeSkeleton, OriginalScriptWords0),
	remove_null_items_in_generated_words(OriginalScriptWords0, OriginalScriptWords1),
	improve_original_script_collocations(OriginalScriptWords1, OriginalScriptWords2),
	fix_original_script_orthography(OriginalScriptWords2, OriginalScriptWords),
	join_with_spaces(OriginalScriptWords, OriginalScript0),
	remove_spaces_from_original_script_result_if_necessary(OriginalScript0, OriginalScript),
	!.

%----------------------------------------------------------------------

generate_gloss_result(Representation, PreferredGeneratedTree, Gloss) :-
	current_predicate(generator_gloss:generate/3),
	remove_lexical_and_line_info_in_parse_tree(PreferredGeneratedTree, PreferredGeneratedTreeSkeleton),
	generator_gloss:generate(Representation, PreferredGeneratedTreeSkeleton, GlossWords0),
	remove_null_items_in_generated_words(GlossWords0, GlossWords),
	join_with_spaces(GlossWords, Gloss),
	!.
generate_gloss_result(_Representation, _PreferredGeneratedTree, Gloss) :-
	current_predicate(generator_gloss:generate/3),
	Gloss = 'WARNING: unable to generate gloss',
	!.

%----------------------------------------------------------------------

recogniser_generate_gloss_result(Representation, PreferredGeneratedTree, Gloss) :-
	current_predicate(recogniser_generator_gloss:generate/3),
	%remove_lexical_and_line_info_in_parse_tree(PreferredGeneratedTree, PreferredGeneratedTreeSkeleton),
	remove_lexical_and_file_info_in_parse_tree(PreferredGeneratedTree, PreferredGeneratedTreeSkeleton),
	recogniser_generator_gloss:generate(Representation, PreferredGeneratedTreeSkeleton, GlossWords),
	join_with_spaces(GlossWords, Gloss),
	!.

%----------------------------------------------------------------------

remove_spaces_from_original_script_result_if_necessary(OriginalScript0, OriginalScript) :-
	current_predicate(user:regulus_config/2),
	user:regulus_config(no_spaces_in_original_script, yes),
	remove_spaces_in_atom(OriginalScript0, OriginalScript),
	!.
remove_spaces_from_original_script_result_if_necessary(OriginalScript0, OriginalScript) :-
	OriginalScript0 = OriginalScript.

remove_spaces_in_atom(Atom0, Atom) :-
	atom_codes(Atom0, Chars0),
	remove_spaces_in_chars(Chars0, Chars),
	atom_codes(Atom, Chars).

remove_spaces_in_chars([], []).
remove_spaces_in_chars([0'  | R], R1) :-
	!,
	remove_spaces_in_chars(R, R1).
remove_spaces_in_chars([F | R], [F | R1]) :-
	!,
	remove_spaces_in_chars(R, R1).

%----------------------------------------------------------------------

order_tuples_using_generation_preferences(TuplesIn, TuplesOut) :-
	order_tuples_using_generation_preferences(TuplesIn, TuplesOut, user).

order_tuples_using_generation_preferences(TuplesIn, TuplesOut, Module) :-
	(   current_predicate(Module:generation_preference/2)
	;
	    frequency_preference_information_available_in_term(TuplesIn)
	),
	score_tuples_with_generation_preferences(TuplesIn, ScoredTuplesIn, Module),
	keysort(ScoredTuplesIn, SortedScoredTuplesIn),
	reverse(SortedScoredTuplesIn, ReversedSortedScoredTuplesIn),
	unkey_list(ReversedSortedScoredTuplesIn, TuplesOut),
	!.
order_tuples_using_generation_preferences(TuplesIn, TuplesIn, _Module) :-
	!.
order_tuples_using_generation_preferences(TuplesIn, TuplesOut, Module) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [order_tuples_using_generation_preferences(TuplesIn, TuplesOut, Module)]),
	fail.

score_tuples_with_generation_preferences([], [], _Module).
score_tuples_with_generation_preferences([F | R], [Score-F1 | R1], Module) :-
	generation_preference_score_for_tuples(F, Score, Trace, Module),
	append(F, [preference_score=Score, preference_info=Trace], F1),
	!,
	score_tuples_with_generation_preferences(R, R1, Module).

generation_preference_score_for_tuples(Tuple, Score, Trace, Module) :-
	(   ( member(words=Words, Tuple), current_predicate(Module:generation_preference/2) ) ->
	    generation_preference_score_for_word_list(Words, WordsScore, WordsTrace, Module)
	;
	    
	    otherwise ->
	    WordsScore = 0,
	    WordsTrace = []
	),
	(   ( member(tagged_words=TaggedWords, Tuple), current_predicate(Module:generation_preference/2) ) ->
	    generation_preference_score_for_word_list(TaggedWords, TaggedWordsScore, TaggedWordsTrace, Module)
	;

	    otherwise ->
	    TaggedWordsScore = 0,
	    TaggedWordsTrace = []
	),
	(   ( member(tree=Tree, Tuple), frequency_preference_information_available_in_term(Tree) ) ->
	    rule_frequency_preference_score(Tree, TreeScore),
	    TreeTrace = [rule_frequencies-TreeScore];

	    otherwise ->
	    TreeScore = 0,
	    TreeTrace = []
	),
	% TreeScore is just to give a default ordering
	Score is WordsScore + TaggedWordsScore + 0.01 * TreeScore,
	append_list([WordsTrace, TaggedWordsTrace, TreeTrace], Trace),
	!.

generation_preference_score_for_word_list(Words, Score, SubListScores, Module) :-
	findall(SubList-SubListScore,
		generation_preference_score_for_sublist_of_word_list(Words, SubList, SubListScore, Module),
		SubListScores),
	total_for_sublist_scores(SubListScores, Score),
	!.
generation_preference_score_for_word_list(Words, Score, Module) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [generation_preference_score_for_word_list(Words, Score, Module)]),
	fail.

generation_preference_score_for_sublist_of_word_list(Words, SubList, SubListScore, Module) :-
	append(['*start*' | Words], ['*end*'], WordsWithStartAndEndTokens),
	sublist_of_list(SubList, WordsWithStartAndEndTokens),
	Module:generation_preference(SubList, SubListScore).

sublist_of_list(SubList, List) :-
	append(_Prefix, SubListAndRest, List),
	append(SubList, _Suffix, SubListAndRest).

total_for_sublist_scores(SubListScores, Score) :-
	total_for_sublist_scores1(SubListScores, 0-Score),
	!.
total_for_sublist_scores(SubListScores, Score) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [total_for_sublist_scores(SubListScores, Score)]),
	fail.

total_for_sublist_scores1([], In-In) :-
	!.
total_for_sublist_scores1([_List-Score | R], In-Out) :-
	Next is In + Score,
	!,
	total_for_sublist_scores1(R, Next-Out).

extract_word_lists_from_tuples([], []).
extract_word_lists_from_tuples([Tuple | R], [Words | R1]) :-
	member(words=Words, Tuple),
	!,
	extract_word_lists_from_tuples(R, R1).

extract_tagged_words_from_tuples([], []).
extract_tagged_words_from_tuples([F | R], [F1 | R1]) :-
	extract_tagged_words_from_tuple(F, F1),
	!,
	extract_tagged_words_from_tuples(R, R1).
 
extract_tagged_words_from_tuple(Tuple, TaggedWithStartAndEndTokens) :-
	member(tagged_words=Tagged, Tuple),
	append(['*start*' | Tagged], ['*end*'], TaggedWithStartAndEndTokens),
	!.
 




