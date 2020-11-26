% regulus_read.pl

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(regulus_read,
	  [read_regulus_file_or_files/2,
	   read_regulus_file/2,
	   read_regulus_files/2,

	   read_regulus_file_or_files/3,
	   read_regulus_file/3,
	   read_regulus_files/3,

	   read_transfer_file_or_files/3,

	   read_orthography_file_or_files/3,

	   read_collocation_file_or_files/2,
	   read_collocation_file_or_files/3,

	   read_generic_regulus_related_file_or_files/2,
	   read_generic_regulus_related_file_or_files/3,
	   read_and_compile_generic_regulus_related_file/2,

	   read_lf_pattern_file_or_files/2,
	   read_lf_pattern_file_or_files/3,

	   read_lf_rewrite_file_or_files/2,
	   read_lf_rewrite_file_or_files/3,

	   read_corpus_file/2,
	   read_corpus_file_printing_statistics/2,

	   include_closure_for_file_or_list/3,

	   strip_wrappers_from_rules/2,

	   all_sem_features/2,
	   add_lexical_feature_defaults/2,
	   expand_abbreviations_in_rules/2]
      ).

'LOAD_DYNAMIC_LEXICON_SUPPORT_IF_AVAILABLE'.
 
:- use_module('$REGULUS/Prolog/regulus_declarations').
:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/utilities').
       
:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(library(ordsets)).
:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).

%---------------------------------------------------------------

read_regulus_file_or_files(InFileOrFiles, ReadRules) :-
	read_regulus_file_or_files(InFileOrFiles, ReadRules, _Decls).

read_regulus_file(InFile, ReadRules) :-
	read_regulus_file(InFile, ReadRules, _Decls).

read_regulus_files(InFile, ReadRules) :-
	read_regulus_files(InFile, ReadRules, _Decls).

read_regulus_file_or_files(InFileOrFiles, ReadRules, Decls) :-
	(   is_list(InFileOrFiles) ->
	    read_regulus_files(InFileOrFiles, ReadRules, Decls) ;
	    read_regulus_files([InFileOrFiles], ReadRules, Decls)
	).	    

read_regulus_file(InFile, ReadRules, Decls) :-
	read_regulus_files([InFile], ReadRules, Decls).

read_regulus_files(Files, ReadRules3, ReadDeclarations) :-
	retract_all_regulus_preds,
	read_regulus_files1(Files, UnexpandedReadRules, ReadDeclarations, grammar),
	length(ReadDeclarations, NDecls),
	internalise_regulus_declarations(ReadDeclarations, 0-NIgnoredDecls),
	save_grammar_macros,
	expand_macros_in_rule_list(UnexpandedReadRules, ReadRules),
	process_dynamic_lex_entries_in_rules(ReadRules, ReadRules1),
	remove_labels_and_ignored_rules(ReadRules1, ReadRules2, 0-NIgnoredRules),
	canonicalise_rules(ReadRules2, ReadRules3),
	warn_about_unused_features(ReadRules3),
	warn_about_inconsistent_feature_spaces(ReadRules3),
	count_regulus_rules(ReadRules3, 0-NNonLexical, 0-NLexical),
	note_and_count_vocabulary_items(ReadRules3, NVocabulary),
	format('~N~n -- Read file(s): ~d declarations, ~d non-lexical rules, ~d lexical rules, ~d distinct vocabulary items.~n', [NDecls, NNonLexical, NLexical, NVocabulary]),
	format('~N -- ~d declarations and ~d rules ignored.~n~n', [NIgnoredDecls, NIgnoredRules]).
 
%---------------------------------------------------------------

read_transfer_file_or_files(InFileOrFiles, ReadRules, Decls) :-
	(   is_list(InFileOrFiles) ->
	    read_transfer_files(InFileOrFiles, ReadRules, Decls) ;
	    read_transfer_files([InFileOrFiles], ReadRules, Decls)
	).	    

read_transfer_files(Files, ReadRules1, ReadDeclarations) :-
	retract_regulus_preds_for_transfer_files,
	read_regulus_files1(Files, UnexpandedReadRules, ReadDeclarations, transfer),
	length(ReadDeclarations, NDecls),
	internalise_regulus_declarations_for_transfer(ReadDeclarations, 0-NIgnoredDecls),
	expand_macros_in_rule_list(UnexpandedReadRules, ReadRules),
	remove_labels_and_ignored_rules(ReadRules, ReadRules1, 0-NIgnoredRules),
	count_transfer_entries(ReadRules1, 0-NRules, 0-NLexical),
	format('~N~n -- Read file(s): ~d declarations, ~d transfer rules, ~d transfer lexicon entries.~n',
	       [NDecls, NRules, NLexical]),
	format('~N -- ~d declarations and ~d rules ignored.~n~n', [NIgnoredDecls, NIgnoredRules]).

%---------------------------------------------------------------

read_orthography_file_or_files(InFileOrFiles, ReadRules, Decls) :-
	(   is_list(InFileOrFiles) ->
	    read_orthography_files(InFileOrFiles, ReadRules, Decls) ;
	    read_orthography_files([InFileOrFiles], ReadRules, Decls)
	).	    

read_orthography_files(Files, ReadRules, ReadDeclarations) :-
	read_regulus_files1(Files, ReadRules, ReadDeclarations, orthography),
	length(ReadRules, NRules),
	length(ReadDeclarations, NDecls),
	format('~N~n -- Read file(s): ~d orthography declarations, ~d orthography rules.~n',
	       [NDecls, NRules]).

%---------------------------------------------------------------

read_collocation_file_or_files(InFileOrFiles, Rules) :-
	read_collocation_file_or_files(InFileOrFiles, Rules, _Decls).

read_collocation_file_or_files(InFileOrFiles, ReadRules, Decls) :-
	(   is_list(InFileOrFiles) ->
	    read_collocation_files(InFileOrFiles, ReadRules, Decls) ;
	    read_collocation_files([InFileOrFiles], ReadRules, Decls)
	).	    

read_collocation_files(Files, Rules, ReadDeclarations) :-
	read_regulus_files1(Files, ReadRules, ReadDeclarations, collocation),
	length(ReadRules, NRules),
	length(ReadDeclarations, NDecls),
	internalise_collocation_decls(ReadDeclarations),
	expand_collocation_macros_in_rules(ReadRules, Rules),
	%print_expanded_collocation_rules(Rules),
	format('~N~n -- Read file(s): ~d collocation declarations, ~d collocation rules.~n',
	       [NDecls, NRules]).

print_expanded_collocation_rules([]).
print_expanded_collocation_rules([F | R]) :-
	print_expanded_collocation_rule(F),
	!,
	print_expanded_collocation_rules(R).

print_expanded_collocation_rule(Rule) :-
	compound(Rule),
	Rule =.. [F, LHS, RHS],
	is_prolog_string(LHS),
	is_prolog_string(RHS),
	format('~N~q("~s", "~s").~n', [F, LHS, RHS]),
	!.
print_expanded_collocation_rule(Rule) :-
	format('~N~q.~n', [Rule]),
	!.

%---------------------------------------------------------------

read_generic_regulus_related_file_or_files(InFileOrFiles, ReadRules) :-
	(   is_list(InFileOrFiles) ->
	    read_generic_regulus_related_files(InFileOrFiles, ReadRules) ;
	    read_generic_regulus_related_files([InFileOrFiles], ReadRules)
	).

read_generic_regulus_related_files(Files, ReadRules1) :-
	retract_regulus_preds_for_generic_files,
	read_regulus_files1(Files, UnexpandedReadRules, ReadDeclarations, generic),
	length(ReadDeclarations, NDecls),
	internalise_generic_regulus_declarations(ReadDeclarations),
	expand_macros_in_rule_list(UnexpandedReadRules, ReadRules),
	remove_labels_and_ignored_rules(ReadRules, ReadRules1, 0-_NIgnoredRules),
	length(ReadRules1, NRules),
	format('~N~n -- Read file(s): ~d macros, ~d entries.~n', [NDecls, NRules]).

read_and_compile_generic_regulus_related_file(InFile, Package) :-
	read_generic_regulus_related_file_or_files(InFile, ReadRules),
	strip_wrappers_from_rules(ReadRules, ReadRules1),
	compiled_version_of_file(InFile, CompiledFile),
	list_to_prolog_file(ReadRules1, CompiledFile),
	safe_compile_with_redefine_warnings_off(Package, CompiledFile),
	!.
read_and_compile_generic_regulus_related_file(InFile, Package) :-
	format('~N*** Error: bad call: ~w~n', [read_and_compile_generic_regulus_related_file(InFile, Package)]),
	fail.

strip_wrappers_from_rules([], []).
strip_wrappers_from_rules([F | R], [F1 | R1]) :-
	strip_wrapper_from_rule(F, F1),
	!,
	strip_wrappers_from_rules(R, R1).

strip_wrapper_from_rule(rule(Element, _LineInfo), Element) :-
	!.
strip_wrapper_from_rule(rule(_Label, Element, _LineInfo), Element) :-
	!.
strip_wrapper_from_rule(F, F1) :-
	format('~N*** Error: bad call: ~w~n', [strip_wrapper_from_rule(F, F1)]),
	fail.

compiled_version_of_file(File, CompiledFile) :-
	safe_absolute_file_name(File, AbsFile),
	directory_and_file_for_pathname(AbsFile, Dir, BaseFile),
	format_to_atom('~w/compiled_~w', [Dir, BaseFile], CompiledFile),
	!.
compiled_version_of_file(File, CompiledFile) :-
	format('~N*** Error: bad call: ~w~n', [compiled_version_of_file(File, CompiledFile)]),
	fail.
 
%---------------------------------------------------------------


read_lf_pattern_file_or_files(InFileOrFiles, ReadRules) :-
	(   is_list(InFileOrFiles) ->
	    read_lf_pattern_files(InFileOrFiles, ReadRules) ;
	    read_lf_pattern_file_or_files([InFileOrFiles], ReadRules)
	).

read_lf_pattern_files(Files, ReadRules) :-
	retract_regulus_preds_for_lf_pattern_files,
	read_regulus_files1(Files, UnexpandedReadRules, ReadDeclarations, lf_pattern),
	length(ReadDeclarations, NDecls),
	internalise_regulus_declarations_for_lf_pattern(ReadDeclarations, 0-_NIgnoredDecls),
	expand_macros_in_rule_list(UnexpandedReadRules, ReadRules),
	length(ReadRules, NRules),
	format('~N~n -- Read file(s): ~d declarations, ~d LF pattern rules.~n',
	       [NDecls, NRules]).
 
%---------------------------------------------------------------

read_lf_rewrite_file_or_files(InFileOrFiles, ReadRules) :-
	(   is_list(InFileOrFiles) ->
	    read_lf_rewrite_files(InFileOrFiles, ReadRules) ;
	    read_lf_rewrite_file_or_files([InFileOrFiles], ReadRules)
	).

read_lf_rewrite_files(Files, ReadRules) :-
	retract_regulus_preds_for_lf_rewrite_files,
	read_regulus_files1(Files, UnexpandedReadRules, ReadDeclarations, lf_rewrite),
	length(ReadDeclarations, NDecls),
	internalise_regulus_declarations_for_lf_rewrite(ReadDeclarations, 0-_NIgnoredDecls),
	expand_macros_in_rule_list(UnexpandedReadRules, ReadRules),
	length(ReadRules, NRules),
	format('~N~n -- Read file(s): ~d declarations, ~d LF pattern rules.~n',
	       [NDecls, NRules]).

%---------------------------------------------------------------

% read_regulus_files1(+Files, -ReadRules, -ReadDeclarations, +FileType)

read_regulus_files1([], [], [], _FileType).
read_regulus_files1([F | R], ReadRules, ReadDeclarations, FileType) :-
	read_top_level_regulus_file(F, ReadRules-ReadRulesNext, ReadDeclarations-ReadDeclarationsNext, FileType),
	!,
	read_regulus_files1(R, ReadRulesNext, ReadDeclarationsNext, FileType).

read_top_level_regulus_file(InFile, RulesIn-RulesOut, DeclarationsIn-DeclarationsOut, FileType) :-
	(   FileType = grammar ->
	    
	    absolute_regulus_file_for_reading(InFile, AbsoluteFile),
	    format('~NReading Regulus file ~w~n', [AbsoluteFile]) ;

	    FileType = transfer ->
	    absolute_file_name(InFile, AbsoluteFile),
	    format('~NReading transfer file ~w~n', [AbsoluteFile]) ;

	    FileType = orthography ->
	    absolute_file_name(InFile, AbsoluteFile),
	    format('~NReading orthography rule file ~w~n', [AbsoluteFile]) ;

	    FileType = collocation ->
	    absolute_file_name(InFile, AbsoluteFile),
	    format('~NReading collocation rule file ~w~n', [AbsoluteFile]) ;

	    FileType = lf_pattern ->
	    absolute_file_name(InFile, AbsoluteFile),
	    format('~NReading LF pattern rule file ~w~n', [AbsoluteFile]) ;

	    FileType = lf_rewrite ->
	    absolute_file_name(InFile, AbsoluteFile),
	    format('~NReading LF rewrite rule file ~w~n', [AbsoluteFile]) ;

	    FileType = generic ->
	    absolute_file_name(InFile, AbsoluteFile),
	    format('~NReading generic rule file ~w~n', [AbsoluteFile]) ;

	    otherwise ->
	    format('~N*** Unknown Regulus file-type in read_top_level_regulus_file/4: ~w~n', [FileType]),
	    fail 
	),	
	open(AbsoluteFile, read, S),
	%line_count(S, FirstLineNumber),
	%format('~N--- Started reading (line number = ~d) ~w~n', [FirstLineNumber, AbsoluteFile]),
	read_regulus_stream(S, RulesIn-RulesOut, DeclarationsIn-DeclarationsOut, 0-_LastItemNumber, 0, AbsoluteFile,
			    FileType),
	close(S).
 
read_regulus_stream(S, InRules-OutRules, InDecls-OutDecls, ItemNumber0-OutItemNumber, LastLine0, File, FileType) :-
	ItemNumber is ItemNumber0 + 1,
	LastLine is LastLine0 + 1,
	%LastLine is LastLine0 + 2,
	%read(S, Term),
	safe_read(S, Term),
	line_count(S, CurrentLine),
	read_regulus_stream1(Term, S, InRules-OutRules, InDecls-OutDecls, ItemNumber-OutItemNumber, LastLine-CurrentLine, File,
			     FileType).

read_regulus_stream1(end_of_file, _S, InRules-InRules, InDecls-InDecls, ItemNumber-ItemNumber, _LastL-_CurrentL, _File,
		     _FileType) :- 
	!.
read_regulus_stream1(Term, S, InRules-OutRules, InDecls-OutDecls, InItemNumber-OutItemNumber, _LastLine-CurrentLine, File,
		     FileType) :-
	Term = include(IncludeFile),
	!,
	read_regulus_include_file(IncludeFile, File, InRules-NextRules, InDecls-NextDecls, InItemNumber-NextItemNumber,
				  FileType),
	read_regulus_stream(S, NextRules-OutRules, NextDecls-OutDecls, NextItemNumber-OutItemNumber, CurrentLine, File,
			    FileType).
read_regulus_stream1(labelled_item(Label, Term), S,
		     InRules-OutRules, InDecls-OutDecls, ItemNumber-OutItemNumber, LastLine-CurrentLine, File, FileType) :-
	member(FileType, [grammar, transfer]),
	LineInfo = line_info(ItemNumber, LastLine-CurrentLine, File),
	read_regulus_stream2(Term, Label, LineInfo, InRules-NextRules, InDecls-NextDecls, FileType),
	!,
	read_regulus_stream(S, NextRules-OutRules, NextDecls-OutDecls, ItemNumber-OutItemNumber, CurrentLine, File,
			    FileType).
read_regulus_stream1(frequency_labelled_item(Label, Term), S, 
		     InRules-OutRules, InDecls-OutDecls, ItemNumber-OutItemNumber, LastLine-CurrentLine, File, FileType) :-
	member(FileType, [grammar, transfer]),
	%LineInfo = line_info(ItemNumber, LastLine-CurrentLine, File),
	LineInfo = line_info(frequency(Label), LastLine-CurrentLine, File),
	read_regulus_stream2(Term, Label, LineInfo, InRules-NextRules, InDecls-NextDecls, FileType),
	!,
	read_regulus_stream(S, NextRules-OutRules, NextDecls-OutDecls, ItemNumber-OutItemNumber, CurrentLine, File,
			    FileType). 
read_regulus_stream1(Term, S,
		     InRules-OutRules, InDecls-OutDecls, ItemNumber-OutItemNumber, LastLine-CurrentLine, File, FileType) :-
	LineInfo = line_info(ItemNumber, LastLine-CurrentLine, File),
	read_regulus_stream2(Term, '*no_label*', LineInfo, InRules-NextRules, InDecls-NextDecls, FileType),
	!,
	read_regulus_stream(S, NextRules-OutRules, NextDecls-OutDecls, ItemNumber-OutItemNumber, CurrentLine, File,
			    FileType).
read_regulus_stream1(Term, S, InRules-OutRules, InDecls-OutDecls, ItemNumber-OutItemNumber, LastLine-CurrentLine, File,
		     FileType) :-
	!,
	format2error('~N*** Error: bad term ~w in ~w file between lines ~d and ~d~n', [Term, FileType, LastLine, CurrentLine]),
	read_regulus_stream(S, InRules-OutRules, InDecls-OutDecls, ItemNumber-OutItemNumber, CurrentLine, File, FileType).

%---------------------------------------------------------------

read_regulus_stream2(Term, Label, LineInfo, InRules-OutRules, InDecls-OutDecls, grammar) :-
	is_regulus_declaration(Term),
	!,
	InDecls = [declaration(Label, Term, LineInfo) | OutDecls],
	InRules = OutRules.
read_regulus_stream2(Term, Label, LineInfo, InRules-OutRules, InDecls-OutDecls, grammar) :- 
	Term = @MacroInvocation,
	!,
	InRules = [rule(Label, @MacroInvocation, LineInfo) | OutRules],
	InDecls = OutDecls.
read_regulus_stream2(Term, Label, LineInfo, InRules-OutRules, InDecls-OutDecls, grammar) :- 
	Term = dynamic_lexicon( @MacroInvocation ),
	!,
	InRules = [rule(Label, dynamic_lexicon( @MacroInvocation ), LineInfo) | OutRules],
	InDecls = OutDecls.
read_regulus_stream2(Term, Label, LineInfo, InRules-OutRules, InDecls-OutDecls, grammar) :- 
	is_regulus_rule(Term),
	!,
	InRules = [rule(Label, Term, LineInfo) | OutRules],
	InDecls = OutDecls.

read_regulus_stream2(Term, Label, LineInfo, InRules-OutRules, InDecls-OutDecls, transfer) :-
	is_transfer_declaration(Term),
	!,
	InDecls = [declaration(Label, Term, LineInfo) | OutDecls],
	InRules = OutRules.
read_regulus_stream2(Term, Label, LineInfo, InRules-OutRules, InDecls-OutDecls, transfer) :- 
	Term = @MacroInvocation,
	!,
	InRules = [rule(Label, @MacroInvocation, LineInfo) | OutRules],
	InDecls = OutDecls.
read_regulus_stream2(Term, Label, LineInfo, InRules-OutRules, InDecls-OutDecls, transfer) :- 
	is_transfer_entry(Term),
	!,
	InRules = [rule(Label, Term, LineInfo) | OutRules],
	InDecls = OutDecls.

read_regulus_stream2(Term, _Label, LineInfo, InRules-OutRules, InDecls-OutDecls, orthography) :-
	is_orthography_declaration(Term),
	!,
	InDecls = [declaration(Term, LineInfo) | OutDecls],
	InRules = OutRules.
read_regulus_stream2(Term, _Label, LineInfo, InRules-OutRules, InDecls-OutDecls, orthography) :- 
	is_orthography_entry(Term),
	!,
	InRules = [rule(Term, LineInfo) | OutRules],
	InDecls = OutDecls.

read_regulus_stream2(Term, _Label, LineInfo, InRules-OutRules, InDecls-OutDecls, collocation) :-
	is_collocation_declaration(Term),
	!,
	InDecls = [declaration(Term, LineInfo) | OutDecls],
	InRules = OutRules.
read_regulus_stream2(Term, _Label, LineInfo, InRules-OutRules, InDecls-OutDecls, collocation) :- 
	is_collocation_entry(Term),
	!,
	InRules = [rule(Term, LineInfo) | OutRules],
	InDecls = OutDecls.

read_regulus_stream2(Term, _Label, LineInfo, InRules-OutRules, InDecls-OutDecls, lf_pattern) :-
	is_lf_pattern_declaration(Term),
	!,
	InDecls = [declaration(no_label, Term, LineInfo) | OutDecls],
	InRules = OutRules.
read_regulus_stream2(Term, _Label, LineInfo, InRules-OutRules, InDecls-OutDecls, lf_pattern) :- 
	is_lf_pattern_entry(Term),
	!,
	InRules = [rule(no_label, Term, LineInfo) | OutRules],
	InDecls = OutDecls.
read_regulus_stream2(Term, _Label, LineInfo, InRules-OutRules, InDecls-OutDecls, lf_pattern) :- 
	Term = @MacroInvocation,
	!,
	InRules = [rule(no_label, @MacroInvocation, LineInfo) | OutRules],
	InDecls = OutDecls.

read_regulus_stream2(Term, _Label, LineInfo, InRules-OutRules, InDecls-OutDecls, lf_rewrite) :-
	is_lf_rewrite_declaration(Term),
	!,
	InDecls = [declaration(no_label, Term, LineInfo) | OutDecls],
	InRules = OutRules.
read_regulus_stream2(Term, _Label, LineInfo, InRules-OutRules, InDecls-OutDecls, lf_rewrite) :- 
	is_lf_rewrite_entry(Term),
	!,
	InRules = [rule(no_label, Term, LineInfo) | OutRules],
	InDecls = OutDecls.
read_regulus_stream2(Term, Label, LineInfo, InRules-OutRules, InDecls-OutDecls, lf_rewrite) :- 
	Term = @MacroInvocation,
	!,
	InRules = [rule(Label, @MacroInvocation, LineInfo) | OutRules],
	InDecls = OutDecls.

read_regulus_stream2(Term, _Label, LineInfo, InRules-OutRules, InDecls-OutDecls, generic) :-
	is_generic_declaration(Term),
	!,
	InDecls = [declaration(no_label, Term, LineInfo) | OutDecls],
	InRules = OutRules.
read_regulus_stream2(Term, _Label, LineInfo, InRules-OutRules, InDecls-OutDecls, generic) :- 
	is_generic_entry(Term),
	!,
	InRules = [rule(no_label, Term, LineInfo) | OutRules],
	InDecls = OutDecls.

%---------------------------------------------------------------

read_regulus_include_file(IncludeFile, CurrentFile, InRules-NextRules, InDecls-NextDecls, InItemNumber-NextItemNumber,
			  FileType) :-
	(   FileType = grammar ->
	    absolute_regulus_file_for_reading_relative_to_current(IncludeFile, CurrentFile, AbsoluteIncludeFile) ;
	    absolute_file_for_reading_relative_to_current(IncludeFile, CurrentFile, AbsoluteIncludeFile)
	),
	read_regulus_include_file1(AbsoluteIncludeFile, InRules-NextRules, InDecls-NextDecls, InItemNumber-NextItemNumber,
				   FileType).

read_regulus_include_file1(AbsoluteIncludeFile, InRules-InRules, InDecls-InDecls, InItemNumber-InItemNumber, _FileType) :-
	previously_loaded_include_file(AbsoluteIncludeFile),
	!.	
read_regulus_include_file1(AbsoluteIncludeFile, InRules-NextRules, InDecls-NextDecls, InItemNumber-NextItemNumber,
			   FileType) :-
	format('~N  [Including file ~w]~n', [AbsoluteIncludeFile]),
	% Do the assert before we start reading rather than after in case we get a recursive include.
	asserta(previously_loaded_include_file(AbsoluteIncludeFile)),
	open(AbsoluteIncludeFile, read, S),
	read_regulus_stream(S, InRules-NextRules, InDecls-NextDecls, InItemNumber-NextItemNumber, 0, AbsoluteIncludeFile,
			    FileType),
	close(S).

%---------------------------------------------------------------

count_regulus_rules([], NNonLex-NNonLex, NLex-NLex).
count_regulus_rules([F|R], NNonLexIn-NNonLexOut, NLexIn-NLexOut) :-
	is_non_lexical_regulus_rule(F),
	NNonLexNext is NNonLexIn + 1,
	!,
	count_regulus_rules(R, NNonLexNext-NNonLexOut, NLexIn-NLexOut).
count_regulus_rules([F|R], NNonLexIn-NNonLexOut, NLexIn-NLexOut) :-
	is_lexical_regulus_rule(F),
	NLexNext is NLexIn + 1,
	!,
	count_regulus_rules(R, NNonLexIn-NNonLexOut, NLexNext-NLexOut).
count_regulus_rules([F|_R], _, _) :-
	regulus_error('~NInternal error: cannot identify ~w as either lexical or non-lexical rule~n', [F]).
 
%---------------------------------------------------------------

count_transfer_entries([], NNonLex-NNonLex, NLex-NLex).
count_transfer_entries([F|R], NNonLexIn-NNonLexOut, NLexIn-NLexOut) :-
	is_non_lexical_transfer_entry(F),
	NNonLexNext is NNonLexIn + 1,
	!,
	count_transfer_entries(R, NNonLexNext-NNonLexOut, NLexIn-NLexOut).
count_transfer_entries([F|R], NNonLexIn-NNonLexOut, NLexIn-NLexOut) :-
	is_lexical_transfer_entry(F),
	NLexNext is NLexIn + 1,
	!,
	count_transfer_entries(R, NNonLexIn-NNonLexOut, NLexNext-NLexOut).
count_transfer_entries([F|_R], _, _) :-
	regulus_error('~NInternal error: cannot identify ~w as either lexical or non-lexical transfer rule~n', [F]).

%---------------------------------------------------------------

remove_labels_and_ignored_rules([], [], IgnoredIn-IgnoredIn) :-
	!.
remove_labels_and_ignored_rules([rule(Label, Term, _LineInfo) | R], R1, IgnoredIn-IgnoredOut) :-
	(   ignore_regulus_item(Label)
	;
	    dummy_rule_item(Term)
	),
	IgnoredNext is IgnoredIn + 1,
	!,
	remove_labels_and_ignored_rules(R, R1, IgnoredNext-IgnoredOut).
remove_labels_and_ignored_rules([F | R], [F1 | R1], IgnoredIn-IgnoredOut) :-
	remove_label_from_rule(F, F1),
	!,
	remove_labels_and_ignored_rules(R, R1, IgnoredIn-IgnoredOut).

remove_label_from_rule(rule(_Label, Term, LineInfo), rule(Term, LineInfo)) :-
	!.
remove_label_from_rule(Rule, Rule1) :-
	regulus_error('~NInternal error: bad call: ~w~n', [remove_label_from_rule(Rule, Rule1)]).

dummy_rule_item(Term) :-
	functor_indicating_dummy_rule(F/N),
	term_contains_functor(Term, F/N),
	!.

functor_indicating_dummy_rule('xxx'/0).

%---------------------------------------------------------------

note_and_count_vocabulary_items(Rules, NVocabulary) :-
	findall(Item, vocabulary_item_in_rule_list(Rules, Item), Items0),
	safe_remove_duplicates(Items0, Items),
	length(Items, NVocabulary),
	note_vocabulary_items(Items).

note_vocabulary_items([]).
note_vocabulary_items([F | R]) :-
	assertz(vocabulary_item(F)),
	!,
	note_vocabulary_items(R).

vocabulary_item_in_rule_list([rule((_LHS --> RHS), LineInfo) | _R], Item) :-
	vocabulary_item_in_rule_body(RHS, Item),
	warn_if_vocabulary_item_contains_spaces(Item, LineInfo).
vocabulary_item_in_rule_list([_F | R], Item) :-
	vocabulary_item_in_rule_list(R, Item).

vocabulary_item_in_rule_body(Body, Item) :-
	nonvar(Body),
	vocabulary_item_in_rule_body1(Body, Item).

vocabulary_item_in_rule_body1((P, Q), Item) :-
	!,
	(   vocabulary_item_in_rule_body(P, Item) ;
	    vocabulary_item_in_rule_body(Q, Item)
	).
vocabulary_item_in_rule_body1((P ; Q), Item) :-
	!,
	(   vocabulary_item_in_rule_body(P, Item) ;
	    vocabulary_item_in_rule_body(Q, Item)
	).
vocabulary_item_in_rule_body1(?P, Item) :-
	!,
	vocabulary_item_in_rule_body(P, Item).
vocabulary_item_in_rule_body1(Atom, Atom) :-
	Atom \== [],
	atomic(Atom),
	!.	
vocabulary_item_in_rule_body1(Body+_Comment, Atom) :-
	vocabulary_item_in_rule_body1(Body, Atom),
	!.	

warn_if_vocabulary_item_contains_spaces(_Atom, _LineInfo) :-
	allow_spaces_in_vocabulary_items,
	!.
warn_if_vocabulary_item_contains_spaces(Atom, LineInfo) :-
	atom(Atom),
	atom_codes(Atom, Chars),
	member(0' , Chars),
	inform_about_regulus_exception(regulus_exception('Vocabulary item contains spaces: "~w". You should probably split this up into individual words.', [Atom]),
				       LineInfo),
	!.
warn_if_vocabulary_item_contains_spaces(_Atom, _LineInfo).

allow_spaces_in_vocabulary_items :-
	current_predicate(user:regulus_config/2),
	user:regulus_config(allow_spaces_in_vocabulary_items, yes).

%---------------------------------------------------------------

% We want to internalise the declarations in the right order,
% irrespective of how they are ordered in the source file,
% so that we can produce appropriate error messages if necessary.

internalise_regulus_declarations(Declarations, IgnoredIn-IgnoredOut) :-
	internalise_declarations1(Declarations, ignore_item, IgnoredIn-IgnoredNext0),

	internalise_declarations1(Declarations, macro, IgnoredNext0-IgnoredNext1),
	internalise_declarations1(Declarations, default_macro, IgnoredNext1-IgnoredNext2),

	internalise_declarations1(Declarations, external_grammar, IgnoredNext2-IgnoredNext3),

	internalise_declarations1(Declarations, feature_value_space, IgnoredNext3-IgnoredNext4),

	mark_category_valued_feature_spaces,

	combine_feature_value_space_declarations,

	internalise_declarations1(Declarations, specialises, IgnoredNext4-IgnoredNext5),
	internalise_declarations1(Declarations, ignore_specialises, IgnoredNext5-IgnoredNext6),
	internalise_declarations1(Declarations, feature_value_space_substitution, IgnoredNext6-IgnoredNext7),

	combine_specialises_declarations,
	revise_feature_value_spaces,

	internalise_declarations1(Declarations, feature, IgnoredNext7-IgnoredNext8),
	internalise_declarations1(Declarations, lexical_feature_default, IgnoredNext8-IgnoredNext9),
	internalise_declarations1(Declarations, ignore_feature, IgnoredNext9-IgnoredNext10),

	internalise_declarations1(Declarations, feature_instantiation_schedule, IgnoredNext10-IgnoredNext11),
	check_feature_instantiation_schedule,

	internalise_declarations1(Declarations, category, IgnoredNext11-IgnoredNext12),

	add_feature_declarations_for_category_valued_features,
	revise_category_declarations_for_category_valued_features,
	revise_feature_instantiation_schedule_for_category_valued_features,

	internalise_declarations1(Declarations, top_level_category, IgnoredNext12-IgnoredOut),

	give_up_if_no_or_bad_top_level_category.

%---------------------------------------------------------------

% If we're loading a transfer file, we only have ignore_items and macros to internalise.

internalise_regulus_declarations_for_transfer(Declarations, IgnoredIn-IgnoredOut) :-
	internalise_declarations1(Declarations, ignore_item, IgnoredIn-IgnoredNext0),
	internalise_declarations1(Declarations, macro, IgnoredNext0-IgnoredOut).	

%---------------------------------------------------------------

% If we're loading an LF pattern file, we only have macros to internalise.

internalise_regulus_declarations_for_lf_pattern(Declarations, IgnoredIn-IgnoredOut) :-
	internalise_declarations1(Declarations, macro, IgnoredIn-IgnoredOut).	

%---------------------------------------------------------------

% If we're loading an LF rewrite file, we only have macros to internalise.

internalise_regulus_declarations_for_lf_rewrite(Declarations, IgnoredIn-IgnoredOut) :-
	internalise_declarations1(Declarations, macro, IgnoredIn-IgnoredOut).	

%---------------------------------------------------------------

% Only macros in generic Regulus-related file

internalise_generic_regulus_declarations(Declarations) :-
	internalise_declarations1(Declarations, macro, 0-_NIgnored).	

%---------------------------------------------------------------

give_up_if_no_or_bad_top_level_category :-
	top_level_category(TopCat),
	category_internal(TopCat, _Feats),
	!.
give_up_if_no_or_bad_top_level_category :-
	top_level_category(TopCat),
	regulus_error('~NTop-level category ~w not defined as a category.~n', [TopCat]).
give_up_if_no_or_bad_top_level_category :-
	regulus_error('~NNo top-level category defined.~n', []).

%---------------------------------------------------------------

internalise_declarations1([], _Functor, IgnoredIn-IgnoredIn).
internalise_declarations1([F | R], Functor, IgnoredIn-IgnoredOut) :-
	(   F = declaration(Label, Decl, LineInfo) ->
	    true ;
	    
	    F = declaration(Decl, LineInfo) ->
	    Label = '*no_label*' ;
	    
	    format(
	    '~N *** Internal error: bad call: ~w~n', 
	    [internalise_declarations1([F | R], Functor)]),
	    fail
	),
	(   functor(Decl, Functor, _N) ->
	    internalise_declaration(Label, Decl, LineInfo, IgnoredIn-IgnoredNext) ;
	    IgnoredIn = IgnoredNext
	),
	!,
	internalise_declarations1(R, Functor, IgnoredNext-IgnoredOut).	

internalise_declaration(Label, _Decl, _LineInfo, IgnoredIn-IgnoredOut) :-
	ignore_regulus_item(Label),
	IgnoredOut is IgnoredIn + 1,
	!.
internalise_declaration(_Label, Decl, LineInfo, IgnoredIn-IgnoredIn) :-
	on_exception(
	Exception, 
	internalise_declaration1(Decl),
	inform_about_regulus_exception(Exception, LineInfo)
    ).

internalise_declaration1(Decl) :-
	internalise_declaration2(Decl, Decl1),
	(   Decl1 = null ->
	    true ;
	    assertz(Decl1)
	),
	!.

internalise_declaration2(ignore_item(Label), ignore_regulus_item(Label)) :-
	ignore_item_decl_is_consistent(Label),
	!.
internalise_declaration2(macro(LHS, RHS), macro(LHS, RHS)) :-
	macro_decl_is_consistent(LHS, RHS),
	!.
internalise_declaration2(default_macro(LHS, RHS), default_macro(LHS, RHS)) :-
	macro_decl_is_consistent(LHS, RHS),
	!.
internalise_declaration2(external_grammar(GrammarName, Body), Result) :-
	external_grammar_decl_is_consistent(GrammarName, Body),
	(   external_grammar(GrammarName, Body) ->
	    Result = null ;
	    Result = external_grammar(GrammarName, Body)
	),
	!.
internalise_declaration2(feature_value_space(Name, SpaceIn), feature_value_space0(Name, OrderedSpace)) :-
	expand_macros_in_term(SpaceIn, Space),
	feature_value_space_decl_is_consistent(Name, Space),
	list_to_ord_set(Space, OrderedSpace),
	!.
internalise_declaration2(specialises(Val1, Val2, SpaceId), specialises(Val1, Val2, SpaceId)) :-
	specialises_decl_is_consistent(Val1, Val2, SpaceId),
	!.
internalise_declaration2(ignore_specialises(Val1, Val2, SpaceId), ignore_specialises(Val1, Val2, SpaceId)) :-
	ignore_specialises_decl_is_consistent(Val1, Val2, SpaceId),
	!.
internalise_declaration2(feature_value_space_substitution(Val1, Val2, SpaceId), feature_value_space_substitution(Val1, Val2, SpaceId)) :-
	feature_value_space_substitution_decl_is_consistent(Val1, Val2, SpaceId),
	!.
internalise_declaration2(feature(Feat, Space), feature(Feat, Space)) :-
	feature_decl_is_consistent(Feat, Space),
	!.
internalise_declaration2(ignore_feature(Feat), ignore_feature(Feat)) :-
	ignore_feature_decl_is_consistent(Feat),
	!.
internalise_declaration2(lexical_feature_default(Feat, Default), lexical_feature_default(Feat, Default)) :-
	lexical_feature_default_decl_is_consistent(Feat, Default),
	!.
internalise_declaration2(feature_instantiation_schedule(Schedule), feature_instantiation_schedule(Schedule)) :-
	feature_instantiation_schedule_decl_is_consistent(Schedule),
	!.
internalise_declaration2(category(CatName, FeatsIn), category_internal(CatName, FeatsOut)) :-
	expand_macros_in_term(FeatsIn, Feats1),
	flatten_list(Feats1, Feats2),
	category_decl_is_consistent(CatName, Feats2),
	remove_ignored_features_from_list(Feats2, Feats3),
	list_to_ord_set(Feats3, FeatsOut),
	!.
internalise_declaration2(top_level_category(Cat), top_level_category(Cat)) :-
	top_level_category_decl_is_consistent(Cat),
	!.
internalise_declaration2( Other, _) :-
	regulus_error('~NUnable to internalise regulus declaration ~q~n', [Other]).

%---------------------------------------------------------------


mark_category_valued_feature_spaces :-
	findall([Id, NonCatVals, CatVals], find_feature_valued_feature_space(Id, NonCatVals, CatVals), Triples),
	mark_category_valued_feature_spaces1(Triples).

mark_category_valued_feature_spaces1([]).
mark_category_valued_feature_spaces1([F | R]) :-
	mark_category_valued_feature_space1(F),
	mark_category_valued_feature_spaces1(R).

mark_category_valued_feature_space1([Id, NonCatVals, CatVals]) :-
	append(NonCatVals, CatVals, AllVals),
	retract(feature_value_space0(Id, _Space)),
	assertz(feature_value_space0(Id, [AllVals])),
	assertz(category_valued_feature_space(Id, NonCatVals, CatVals)).

find_feature_valued_feature_space(Id, NonCatVals, CatVals) :-
	feature_value_space0(Id, Space),
	Space = [Vals],
	findall(Cat, member(syn_term(Cat), Vals), CatVals),
	findall(NonCatVal, ( member(NonCatVal, Vals), atom(NonCatVal) ), NonCatVals),
	CatVals \== [].

%---------------------------------------------------------------

add_feature_declarations_for_category_valued_features :-
	findall(
	[Feat, CatVals, SpaceId], 
	( feature(Feat, SpaceId), 
	  \+ignore_feature(Feat), 
	  category_valued_feature_space(SpaceId, _NonCatVals, CatVals) 
      ),
	Triples),
	!,
	add_feature_declarations_for_category_valued_features1(Triples).

add_feature_declarations_for_category_valued_features1([]).
add_feature_declarations_for_category_valued_features1([[Feat, CatVals, SpaceId] | R]) :-
	add_feature_declarations_for_category_valued_feature(Feat, CatVals, SpaceId),
	add_feature_declarations_for_category_valued_features1(R).	

add_feature_declarations_for_category_valued_feature(_Feat, [], _SpaceId).
add_feature_declarations_for_category_valued_feature(Feat, [F | R], SpaceId) :-
	add_feature_declarations_for_category_valued_feature1(Feat, F, SpaceId),
	add_feature_declarations_for_category_valued_feature(Feat, R, SpaceId).

add_feature_declarations_for_category_valued_feature1(Feat, Cat, SpaceId) :-
	(   category_internal(Cat, SubFeats) ->
	    add_feature_declarations_for_category_valued_feature2(Feat, Cat, SubFeats) ;
	    regulus_error('~NUndeclared category ~w listed as possible value in value-space ~w', [Cat, SpaceId])
	).

add_feature_declarations_for_category_valued_feature2(_Feat, _Cat, []).
add_feature_declarations_for_category_valued_feature2(Feat, Cat, [F | R]) :-
	add_feature_declarations_for_category_valued_feature3(Feat, Cat, F),
	add_feature_declarations_for_category_valued_feature2(Feat, Cat, R).

add_feature_declarations_for_category_valued_feature3(Feat, Cat, SubFeat) :-
	\+ feature(SubFeat, _Space),
	!,
	regulus_error('~NCategory ~w declared as possible value of feature ~w contains undeclared feature ~w', [Cat, Feat, SubFeat]).
add_feature_declarations_for_category_valued_feature3(Feat, Cat, SubFeat) :-
	feature(SubFeat, SpaceId),
	category_valued_feature_space(SpaceId, _, _),
	!,
	regulus_error('~NCategory ~w declared as possible value of feature ~w contains feature-valued feature ~w', [Cat, Feat, SubFeat]).
add_feature_declarations_for_category_valued_feature3(Feat, Cat, SubFeat) :-
	feature(SubFeat, SpaceId),
	join_with_underscore([Feat, Cat, SubFeat], NewFeat),
	assertz(subfeature(Feat, Cat, SubFeat, NewFeat)),
	assertz(feature(NewFeat, SpaceId)).

%---------------------------------------------------------------

revise_category_declarations_for_category_valued_features :-
	findall(
	Cat, 
	( category_internal(Cat, Feats), 
	  member(Feat, Feats),
	  feature(Feat, Space),
	  category_valued_feature_space(Space, _, _)
      ),
	Cats),
	revise_category_declarations_for_category_valued_features1(Cats).

revise_category_declarations_for_category_valued_features1([]).
revise_category_declarations_for_category_valued_features1([F | R]) :-
	revise_category_declaration_for_category_valued_features(F),
	revise_category_declarations_for_category_valued_features1(R).

revise_category_declaration_for_category_valued_features(Cat) :-
	category_internal(Cat, NormalFeats),
	findall(SubFeat, ( member(Feat, NormalFeats), subfeature(Feat, _, _, SubFeat) ), SubFeats),
	append(NormalFeats, SubFeats, NewFeats0),
	sort(NewFeats0, NewFeats),
	retract(category_internal(Cat, NormalFeats)),
	assertz(category_internal(Cat, NewFeats)).

%---------------------------------------------------------------

revise_feature_instantiation_schedule_for_category_valued_features :-
	feature_instantiation_schedule(Schedule),
	revise_feature_instantiation_schedule_for_category_valued_features(Schedule, Schedule1),
	retractall(feature_instantiation_schedule(_)),
	assert(feature_instantiation_schedule(Schedule1)).

revise_feature_instantiation_schedule_for_category_valued_features([], []).
revise_feature_instantiation_schedule_for_category_valued_features([F | R], [F1 | R1]) :-
	revise_feature_instantiation_schedule_for_category_valued_features1(F, F1),
	revise_feature_instantiation_schedule_for_category_valued_features(R, R1).

revise_feature_instantiation_schedule_for_category_valued_features1([], []).
revise_feature_instantiation_schedule_for_category_valued_features1([Feat | R], Result) :-
	findall(SubFeat, subfeature(Feat, _, _, SubFeat), SubFeats),
	revise_feature_instantiation_schedule_for_category_valued_features1(R, R1),
	(   SubFeats \== [] ->
	    append([Feat | SubFeats], R1, Result) ;
	    Result = [Feat | R1]
	).	

%---------------------------------------------------------------

combine_feature_value_space_declarations :-
	all_feature_value_space_ids(Ids),
	combine_feature_value_space_declarations1(Ids).

all_feature_value_space_ids(Ids) :-
	setof(Id, Val^feature_value_space0(Id, Val), Ids),
	!.
all_feature_value_space_ids([]) :-
	format('~NREGULUS WARNING:~nNo feature_value_space declarations provided.~n', []).

combine_feature_value_space_declarations1([]).
combine_feature_value_space_declarations1([F|R]) :-
	combine_feature_value_space_declarations2(F),
	!,
	combine_feature_value_space_declarations1(R).

combine_feature_value_space_declarations2(Id) :-
	findall(Space, feature_value_space0(Id, Space), Spaces),
	(   combine_feature_value_space_declarations3(Spaces, CombinedSpace) ->
	    assertz(feature_value_space(Id, CombinedSpace)) ;
	    regulus_error('~NUnable to combine feature_value_space declarations for ~w~n', [Id])
	).

combine_feature_value_space_declarations3([Space], Space) :-
	!.
combine_feature_value_space_declarations3([F|R], CombinedSpace) :-
	combine_feature_value_space_declarations3(R, CombinedSpaceR),
	combine_feature_value_space_declarations4(F, CombinedSpaceR, CombinedSpace).

combine_feature_value_space_declarations4(Space1, Space2, CombinedSpace) :-
	is_list(Space1),
	is_list(Space2),
	length(Space1, Len),
	length(Space2, Len),
	!,
	combine_feature_value_space_declarations5(Space1, Space2, CombinedSpace).

combine_feature_value_space_declarations5([], [], []).
combine_feature_value_space_declarations5([F1|R1], [F2|R2], [F3|R3]) :-
	ord_union(F1, F2, F3List),
	list_to_ord_set(F3List, F3),
	!,
	combine_feature_value_space_declarations5(R1, R2, R3).

%---------------------------------------------------------------

combine_specialises_declarations :-
	findall(SpaceId, uncancelled_specialises(_Val1, _Val2, SpaceId), ConstrainedSpaceIds),
	list_to_ord_set(ConstrainedSpaceIds, ConstrainedSpaceIdsOS),
	combine_specialises_declarations1(ConstrainedSpaceIdsOS).

combine_specialises_declarations1([]).
combine_specialises_declarations1([F | R]) :-
	combine_specialises_declarations2(F),
	combine_specialises_declarations1(R).

combine_specialises_declarations2(SpaceId) :-
	findall(Super, uncancelled_specialises(_Sub, Super, SpaceId), Supers),
	list_to_ord_set(Supers, SupersOS),
	combine_specialises_declarations3(SupersOS, SpaceId).

combine_specialises_declarations3([], _SpaceId).
combine_specialises_declarations3([F | R], SpaceId) :-
	combine_specialises_declarations4(F, SpaceId),
	combine_specialises_declarations3(R, SpaceId).

combine_specialises_declarations4(Super, SpaceId) :-
	findall(Sub, transitively_specialises_feature_value(Super, SpaceId, Sub), Subs),
	list_to_ord_set(Subs, SubsOS),
	atom_list_to_disjunction(SubsOS, DisjunctionOfSubs),
	assertz(feature_value_space_substitution(Super, DisjunctionOfSubs, SpaceId)).

transitively_specialises_feature_value(Super, SpaceId, Sub) :-
	transitively_specialises_feature_value1(Super, SpaceId, Sub, []).

transitively_specialises_feature_value1(Super, SpaceId, Sub, Previous) :-
	uncancelled_specialises(NextSub, Super, SpaceId),
	error_if_circular_chain_of_specialisations(SpaceId, NextSub, Previous),
	(   \+ uncancelled_specialises(_LowerSub, NextSub, SpaceId) ->
	    Sub = NextSub ;
	    transitively_specialises_feature_value1(NextSub, SpaceId, Sub, [NextSub | Previous])
	).

error_if_circular_chain_of_specialisations(SpaceId, Sub, Previous) :-
	member(Sub, Previous),
	!,
	reverse([Sub | Previous], ReversedTrace),
	regulus_error('~NCircular chain of specialisations in ~w: ~w~n', [SpaceId, ReversedTrace]).
error_if_circular_chain_of_specialisations(_SpaceId, _Sub, _Previous).

atom_list_to_disjunction([Atom], Atom) :-
	atomic(Atom),
	!.
atom_list_to_disjunction([F | R], (F\/DisjR)) :-
	atom_list_to_disjunction(R, DisjR).

uncancelled_specialises(Val1, Val2, SpaceId) :-
	specialises(Val1, Val2, SpaceId),
	\+ ignore_specialises(Val1, Val2, SpaceId).

%---------------------------------------------------------------

revise_feature_value_spaces :-
	findall(feature_value_space(Id, Space), feature_value_space(Id, Space), Decls),
	retractall(feature_value_space(_,_)),
	revise_feature_value_spaces1(Decls).

revise_feature_value_spaces1([]).
revise_feature_value_spaces1([F | R]) :-
	revise_feature_value_space(F),
	!,
	revise_feature_value_spaces1(R).

revise_feature_value_space(feature_value_space(Id, Space)) :-
	remove_substituted_vals_from_feature_value_space(Space, Id, Space1),
	assertz(feature_value_space(Id, Space1)).

remove_substituted_vals_from_feature_value_space([], _Id, []).
remove_substituted_vals_from_feature_value_space([F | R], Id, [F1 | R1]) :-
	remove_substituted_vals_from_feature_value_space1(F, Id, F1),
	!,
	remove_substituted_vals_from_feature_value_space(R, Id, R1).

remove_substituted_vals_from_feature_value_space1([], _Id, []).
remove_substituted_vals_from_feature_value_space1([F | R], Id, Out) :-
	(   feature_value_space_substitution(F, _SubstitutedValue, Id) ->
	    Out = R1 ;
	    Out = [F | R1]
	),
	!,
	remove_substituted_vals_from_feature_value_space1(R, Id, R1).

%---------------------------------------------------------------

check_feature_instantiation_schedule :-
	findall(Schedule, feature_instantiation_schedule(Schedule), Schedules),
	length(Schedules, NSchedules),
	check_feature_instantiation_schedule1(NSchedules).

% If we have exactly one feature_instantiation_schedule declaration, fine.
check_feature_instantiation_schedule1(1) :-
	!.
% If we have none, make up a default declaration which says that we instantiate
% all the features in one go.
check_feature_instantiation_schedule1(0) :-
	!,
	findall(Feat, feature(Feat, _), Feats),
	asserta(feature_instantiation_schedule([Feats])).
% Otherwise signal an error.
check_feature_instantiation_schedule1(_Other) :-
	regulus_error('~NMore than one feature_instantiation_schedule declaration~n', []).

%---------------------------------------------------------------

ignore_item_decl_is_consistent(Label) :-
	(   atom(Label) ->
	    true ;
	    regulus_error('~NArgument in ignore_item must be an atom~n', [])
	).

%---------------------------------------------------------------

macro_decl_is_consistent(LHS, _RHS) :-
	(   var(LHS) ->
	    regulus_error('~NLHS in macro declaration may not be a variable~n', []) ;
	    true
	).

%---------------------------------------------------------------

external_grammar_decl_is_consistent(GrammarName, Body) :-
	(   term_contains_functor([GrammarName, Body], '@'/1) ->
	    regulus_error('~NMacros not yet permitted in external grammar declaration~n', []) ;
	    true
	),
	(   atom(GrammarName) ->
	    true ;
	    regulus_error('~NFirst arg in external_grammar declaration not an atom~n', [])
	),
	(   atom(Body) ->
	    true ;
	    regulus_error('~NSecond arg in external_grammar declaration not an atom~n', [])
	).	

feature_value_space_decl_is_consistent(Name, Space) :-
	(   term_contains_functor([Name, Space], '@'/1) ->
	    regulus_error('~NMacros not yet permitted in feature value space declaration~n', []) ;
	    true
	),
	(   atom(Name) ->
	    true ;
	    regulus_error('~NFirst arg in feature_value_space declaration not an atom~n', [])
	),
	(   is_list_of_lists_of_cat_vals(Space) ->
	    true
	;
	    is_list_of_cat_vals(Space) ->
	    regulus_error('~NSecond arg in feature_value_space declaration should be a LIST OF LISTS of category values. You maybe want "~w" instead of "~w"?~n', [[Space], Space])
	;
	    otherwise ->
	    regulus_error('~NSecond arg in feature_value_space declaration not a list of lists of category values~n', [])
	).

specialises_decl_is_consistent(Val1, Val2, SpaceId) :-
	(   term_contains_functor([Val1, Val2, SpaceId], '@'/1) ->
	    regulus_error('~NMacros not yet permitted specialises declaration~n', []) ;
	    true
	),
	(   atom(SpaceId) ->
	    true ;
	    regulus_error('~NThird arg in "specialises" declaration not an atom~n', [])
	),
	(   feature_value_space(SpaceId, Space) ->
	    true ;
	    regulus_error('~NThird arg in "specialises" declaration not declared as feature value space~n', [])
	),
	feature_value_space_inheritance_rel_is_consistent(Val1, Val2, Space).

ignore_specialises_decl_is_consistent(Val1, Val2, SpaceId) :-
	(   term_contains_functor([Val1, Val2, SpaceId], '@'/1) ->
	    regulus_error('~NMacros not yet permitted in ignore_specialises declaration~n', []) ;
	    true
	),
	(   atom(SpaceId) ->
	    true ;
	    regulus_error('~NThird arg in "ignore_specialises" declaration not an atom~n', [])
	),
	(   feature_value_space(SpaceId, Space) ->
	    true ;
	    regulus_error('~NThird arg in "ignore_specialises" declaration not declared as feature value space~n', [])
	),
	ignore_feature_value_space_inheritance_rel_is_consistent(Val1, Val2, Space).

feature_value_space_substitution_decl_is_consistent(Val1, Val2, SpaceId) :-
	(   term_contains_functor([Val1, Val2, SpaceId], '@'/1) ->
	    regulus_error('~NMacros not yet permitted in feature_value_space_substitution declaration~n', []) ;
	    true
	),
	(   atom(SpaceId) ->
	    true ;
	    regulus_error('~NThird arg in "feature_value_space_substitution" declaration not an atom~n', [])
	),
	(   feature_value_space(SpaceId, Space) ->
	    true ;
	    regulus_error('~NThird arg in "feature_value_space_substitution" declaration not declared as feature value space~n', [])
	),
	feature_value_space_substitution_rel_is_consistent(Val1, Val2, Space).

feature_value_space_inheritance_rel_is_consistent(Val1, Val2, Space) :-
	(   term_contains_functor([Val1, Val2, Space], '@'/1) ->
	    regulus_error('~NMacros not yet permitted in feature_value_space_inheritance_rel declaration~n', []) ;
	    true
	),
	member(SubSpace, Space),
	member(Val1, SubSpace),
	member(Val2, SubSpace),
	!.
feature_value_space_inheritance_rel_is_consistent(_Val1, _Val2, _Space) :-
	regulus_error('~N"specialises" declaration must be of form specialises(Val1, Val2, Space),\nwith Val1 and Val2 members of the same component of the feature value space Space~n', []).

ignore_feature_value_space_inheritance_rel_is_consistent(Val1, Val2, Space) :-
	(   term_contains_functor([Val1, Val2, Space], '@'/1) ->
	    regulus_error('~NMacros not yet permitted in ignore_feature_value_space_inheritance_rel declaration~n', []) ;
	    true
	),
	member(SubSpace, Space),
	member(Val1, SubSpace),
	member(Val2, SubSpace),
	!.
ignore_feature_value_space_inheritance_rel_is_consistent(Val1, Val2, Space) :-
	format('~NWARNING: not all values defined in declaration: ~w~n~n', [ignore_specialises(Val1, Val2, Space)]).

feature_value_space_substitution_rel_is_consistent(Val1, Val2, Space) :-
	(   term_contains_functor([Val1, Val2, Space], '@'/1) ->
	    regulus_error('~NMacros not yet permitted in feature_value_space_substitution_rel declaration~n', []) ;
	    true
	),
	member(SubSpace, Space),
	member(Val1, SubSpace),
	member(Val2, SubSpace),
	!.
feature_value_space_substitution_rel_is_consistent(Val1, Val2, Space) :-
	format('~NWARNING: not all values defined in declaration: ~w~n~n', [feature_value_space_substitution(Val1, Val2, Space)]).

feature_decl_is_consistent(Feat, Space) :-
	(   term_contains_functor([Feat, Space], '@'/1) ->
	    regulus_error('~NMacros not yet permitted in feature declaration~n', []) ;
	    true
	),
	(   atom(Feat) ->
	    true ;
	    regulus_error('~NFirst arg in feature declaration not an atom~n', [])
	),
	(   atom(Space) ->
	    true ;
	    regulus_error('~NSecond arg in feature declaration not an atom~n', [])
	),
	(   feature_value_space(Space, _) ->
	    true ;
	    regulus_error('~NSecond arg in feature declaration not declared as feature value space~n', [])
	).

ignore_feature_decl_is_consistent(Feat) :-
	(   term_contains_functor([Feat], '@'/1) ->
	    regulus_error('~NMacros not yet permitted in ignore_feature declaration~n', []) ;
	    true
	),
	(   atom(Feat) ->
	    true ;
	    regulus_error('~NArg in ignore_feature declaration not an atom~n', [])
	),	
	(   (  feature(Feat, _) ; Feat = sem ; Feat = gsem ) ->
	    true ;
	    regulus_error('~NArg in ignore_feature declaration not declared as feature~n', [])
	).

lexical_feature_default_decl_is_consistent(Feat, Default) :-
	(   term_contains_functor([Feat, Default], '@'/1) ->
	    regulus_error('~NMacros not yet permitted in lexical_feature_default declaration~n', []) ;
	    true
	),
	(   atom(Feat) ->
	    true ;
	    regulus_error('~NFirst arg in lexical_feature_default declaration not an atom~n', [])
	),	
	(   feature(Feat, _SpaceId) ->
	    true ;
	    regulus_error('~NFirst arg in lexical_feature_default declaration not declared as feature~n', [])
	),
	(   is_valid_feat_val(Feat, Default) ->
	    true;
	    regulus_error('~NSecond arg in lexical_feature_default declaration not a possible value for "~w"~n', [Feat])
	).	

feature_instantiation_schedule_decl_is_consistent(Schedule) :-
	(   term_contains_functor(Schedule, '@'/1) ->
	    regulus_error('~NMacros not yet permitted in feature_instantiation_schedule declaration~n', []) ;
	    true
	),
	(   is_list_of_atom_lists(Schedule) ->
	    true ;
	    regulus_error('~NArg in feature_instantiation_schedule declaration not a list of lists of atoms~n', [])
	),

	findall(NonFeat, non_feat_in_schedule(NonFeat, Schedule), NonFeats),
	findall(OmittedFeat, omitted_feat_in_schedule(OmittedFeat, Schedule), OmittedFeats),

	(   NonFeats = [] ->
	    true ;
	    regulus_error('~NFollowing atoms in feature_instantiation_schedule declaration not declared as features: ~w~n', [NonFeats])
	),

	(   OmittedFeats = [] ->
	    true ;
	    regulus_error('~NFollowing features not listed in feature_instantiation_schedule declaration: ~w~n', [OmittedFeats])
	).

non_feat_in_schedule(Feat, Schedule) :-
	member(Stage, Schedule), 
	member(Feat, Stage), 
	\+ feature(Feat, _).

omitted_feat_in_schedule(Feat, Schedule) :-
	feature(Feat, _),
	\+ ( ( member(Stage, Schedule), member(Feat, Stage) ) ).

category_decl_is_consistent(CatName, Feats) :-
	(   atom(CatName) ->
	    true ;
	    regulus_error('~NFirst arg in category declaration not an atom~n', [])
	),	
	(   is_list(Feats) ->
	    true ;
	    regulus_error('~NSecond arg in category declaration not a list~n', [])
	),	
	(   ( member(sem, Feats), member(gsem, Feats) ) ->
	    regulus_error('~NCannot have both sem and gsem as features~n', []) ;
	    true
	),
	(   ( undeclared_features_in_list(Feats, UndeclaredFeats), UndeclaredFeats \== [] ) ->
	    regulus_error('~NUndeclared features in category declaration: ~w~n', [UndeclaredFeats]) ;
	    true
	).

top_level_category_decl_is_consistent(Cat) :-
	(   term_contains_functor(Cat, '@'/1) ->
	    regulus_error('~NMacros not yet permitted in top_level_category declaration~n', []) ;
	    true
	),
	(   atom(Cat) ->
	    true ;
	    regulus_error('~NFirst arg in top_level_category declaration not an atom~n', [])
	),	
	(   category_internal(Cat, _) ->
	    true ;
	    regulus_error('~NFirst arg in top_level_category declaration not declared as category~n', [])
	).

% undeclared_features_in_list(+Feats, -UndeclaredFeats)

undeclared_features_in_list([], []).
undeclared_features_in_list([F | R], [F | R1]) :-
	\+ member(F, [sem, gsem]), 
	\+ feature(F, _),
	\+ ignore_feature(F),
	!,
	undeclared_features_in_list(R, R1).
undeclared_features_in_list([_F | R], R1) :-
	!,
	undeclared_features_in_list(R, R1).

%---------------------------------------------------------------

absolute_file_for_reading_relative_to_current(File, CurrentFile, AbsoluteFile) :-
	on_exception(
	_Exception, 
	absolute_file_for_reading_relative_to_current1(File, CurrentFile, AbsoluteFile),
	regulus_error('~NUnable to interpret ~w (included in ~w) as the name of a readable file.~n', [File, CurrentFile])
    ),
	!.
absolute_file_for_reading_relative_to_current(File, CurrentFile, _AbsoluteFile) :-
	regulus_error('~NUnable to interpret ~w (included in ~w) as the name of a readable file.~n', [File, CurrentFile]).

absolute_file_for_reading_relative_to_current1(File, CurrentFile, AbsoluteFile) :-
	directory_and_file_for_pathname(CurrentFile, CurrentDirectory, _),
	safe_working_directory(LastDirectory, CurrentDirectory),
	absolute_file_name(File, AbsoluteFile),
	safe_working_directory(_, LastDirectory).

%---------------------------------------------------------------

absolute_regulus_file_for_reading_relative_to_current(File, CurrentFile, AbsoluteFile) :-
	on_exception(
	_Exception, 
	absolute_regulus_file_for_reading_relative_to_current1(File, CurrentFile, AbsoluteFile),
	regulus_error('~NUnable to interpret ~w (included in ~w) as the name of a readable file with .regulus extension~n', [File, CurrentFile])
    ),
	!.
absolute_regulus_file_for_reading_relative_to_current(File, CurrentFile, _AbsoluteFile) :-
	regulus_error('~NUnable to interpret ~w (included in ~w) as the name of a readable file with .regulus extension~n', [File, CurrentFile]).

absolute_regulus_file_for_reading_relative_to_current1(File, CurrentFile, AbsoluteFile) :-
	directory_and_file_for_pathname(CurrentFile, CurrentDirectory, _),
	safe_working_directory(LastDirectory, CurrentDirectory),
	absolute_regulus_file_for_reading(File, AbsoluteFile),
	safe_working_directory(_, LastDirectory).

%---------------------------------------------------------------

absolute_regulus_file_for_reading(File, _AbsoluteFile) :-
	var(File),
	regulus_error('~NVariable used as file name~n', []).
absolute_regulus_file_for_reading(File, AbsoluteFile) :-
	add_regulus_extension_if_necessary(File, FileWithExtension),
	absolute_file_name(FileWithExtension, AbsoluteFile),
	safe_file_exists(AbsoluteFile),
	!.
absolute_regulus_file_for_reading(File, _AbsoluteFile) :-
	regulus_error('~NUnable to interpret ~w as the name of a readable file with .regulus extension~n', [File]).
 
add_regulus_extension_if_necessary(File, FileWithExtension) :-
	atomic(File),
	atom_codes(File, Chars),
	append(_Body, ".regulus", Chars),
	!,
	File = FileWithExtension.
add_regulus_extension_if_necessary(File, FileWithExtension) :-
	atomic(File),
	!,
	atom_codes(File, Chars),
	append(Chars, ".regulus", FullChars),
	atom_codes(FileWithExtension, FullChars).
add_regulus_extension_if_necessary(File, FileWithExtension) :-
	compound(File),
	functor(File, F, 1),
	functor(FileWithExtension, F, 1),
	arg(1, File, Arg),
	arg(1, FileWithExtension, ArgWithExtension),
	add_regulus_extension_if_necessary(Arg, ArgWithExtension).	

%---------------------------------------------------------------

read_corpus_file_printing_statistics(File, List) :-
	safe_absolute_file_name(File, AbsFile),
	read_corpus_file(AbsFile, List),
	length(List, N),
	format('~N--- Read file (~d records) ~w~n', [N, AbsFile]),
	!.

read_corpus_file(File, List) :-
	safe_absolute_file_name(File, AbsFile),
	prolog_file_to_list(AbsFile, List0),
	expand_multiple_entries_in_corpus_file(List0, List),
	!.

expand_multiple_entries_in_corpus_file([], []).
expand_multiple_entries_in_corpus_file([F | R], [F | RestList]) :-
	compound(F),
	functor(F, sent, _),
	!,
	expand_multiple_entries_in_corpus_file(R, RestList).
expand_multiple_entries_in_corpus_file([F | R], [F1 | RestList]) :-
	compound(F),
	F =.. [multiple_sent, Multiplicity | Args],
	number(Multiplicity),
	Multiplicity > 0,
	F1 =.. [sent | Args],
	Multiplicity1 is Multiplicity - 1,
	F2 =.. [multiple_sent, Multiplicity1 | Args],
	!,
	expand_multiple_entries_in_corpus_file([F2 | R], RestList).
expand_multiple_entries_in_corpus_file([F | R], RestList) :-
	compound(F),
	F =.. [multiple_sent, Multiplicity | _Args],
	number(Multiplicity),
	Multiplicity =< 0,
	!,
	expand_multiple_entries_in_corpus_file(R, RestList).
expand_multiple_entries_in_corpus_file([F | R], RestList) :-
	format('~N*** Warning: bad entry in corpus file: ~w~n', [F]),
	!,
	expand_multiple_entries_in_corpus_file(R, RestList).

%------------------------------------------------------------------------------------

include_closure_for_file_or_list(FileOrList, Closure, Extension) :-
	include_closure_for_file_or_list(FileOrList, '*top*', Closure, Extension).

include_closure_for_file_or_list(FileOrList, CurrentFile, Closure, Extension) :-
	findall(Member,
		member_of_include_closure_for_file_or_list(FileOrList, CurrentFile, Extension, Member),
		Members),
	sort(Members, Closure).
 
member_of_include_closure_for_file_or_list(List, CurrentFile, Extension, Member) :-
	is_list(List),
	member(File, List),
	member_of_include_closure_for_file_or_list(File, CurrentFile, Extension, Member).
member_of_include_closure_for_file_or_list(File, CurrentFile, Extension, Member) :-
	\+ is_list(File),
	(   CurrentFile = '*top*' ->
	    safe_absolute_file_name(File, AbsFile0)
	;
	    otherwise ->
	    absolute_file_for_reading_relative_to_current(File, CurrentFile, AbsFile0)
	),
	(   pathname_has_extension(AbsFile0, Extension) ->
	    AbsFile = AbsFile0 ;
	    format_to_atom('~w.~w', [AbsFile0, Extension], AbsFile)
	),
	safe_file_exists(AbsFile),
	(   Member = AbsFile ;
	    member_of_include_closure_for_file(AbsFile, Extension, Member)
	).

member_of_include_closure_for_file(File, Extension, Member) :-
	prolog_file_to_list(File, List),
	member(include(IncludedFile), List),
	member_of_include_closure_for_file_or_list(IncludedFile, File, Extension, Member).
 
%---------------------------------------------------------------

all_sem_features(Rules, SemFeats) :-
	findall(SemFeat, sem_feature_in_rule_in_list(SemFeat, Rules), SemFeats0),
	sort(SemFeats0, SemFeats),
	!.
all_sem_features(_Rules, SemFeats) :-
	SemFeats = [].

sem_feature_in_rule_in_list(SemFeat, Rules) :-
	member(rule(Rule, _LineInfo), Rules),
	sem_feature_in_rule(SemFeat, Rule).

sem_feature_in_rule(SemFeat, (H --> B)) :-
	!,
	(   sem_feature_in_rule(SemFeat, H) ;
	    sem_feature_in_rule(SemFeat, B)
	).
sem_feature_in_rule(SemFeat, (P, Q)) :-
	!,
	(   sem_feature_in_rule(SemFeat, P) ;
	    sem_feature_in_rule(SemFeat, Q)
	).
sem_feature_in_rule(SemFeat, (P ; Q)) :-
	!,
	(   sem_feature_in_rule(SemFeat, P) ;
	    sem_feature_in_rule(SemFeat, Q)
	).
sem_feature_in_rule(SemFeat, _Cat:FeaturesWithVals) :-
	!,
	(   member((sem=SemVal), FeaturesWithVals) ;
	    member((gsem=SemVal), FeaturesWithVals)
	),
	sem_feature_in_semval(SemFeat, SemVal).

sem_feature_in_semval(SemFeat, SemVal) :-
	is_list(SemVal),
	member((F=V), SemVal),
	(   SemFeat = F ;
	    sem_feature_in_semval(SemFeat, V)
	).	

%---------------------------------------------------------------

% add_lexical_feature_defaults(RulesIn, RulesOut) 

add_lexical_feature_defaults([], []).
add_lexical_feature_defaults([F | R], [F1 | R1]) :-
	(   is_lexical_regulus_rule(F) ->
	    add_lexical_feature_defaults_to_rule(F, F1) ;
	    F1 = F
	),
	!,
	add_lexical_feature_defaults(R, R1).

add_lexical_feature_defaults_to_rule(rule((CatName:FeatVals --> RHS), LineInfo), rule((CatName:FeatVals1 --> RHS1), LineInfo)) :-
	on_exception(
		     Exception, 
		     (   add_lexical_feature_defaults_to_feats(CatName, FeatVals, FeatVals1),
			 remove_annotations_from_lex_items(RHS, RHS1)
		     ),
		     (   inform_about_regulus_exception(Exception, LineInfo),
			 fail
		     )
		    ),
	!.
%add_lexical_feature_defaults_to_rule(X, Y) :-
%	regulus_error('Call failed: ~w', [add_lexical_feature_defaults_to_rule(X, Y)]).

add_lexical_feature_defaults_to_feats(CatName, _FeatVals, _FeatVals1) :-
	\+ category_internal(CatName, _FeatsOS),
	regulus_error('Undeclared category: ~w', [CatName]).
add_lexical_feature_defaults_to_feats(CatName, FeatVals, FeatVals1) :-
	category_internal(CatName, FeatsOS),
	feats_in_featvals(FeatVals, NamedFeats),
	list_to_ord_set(NamedFeats, NamedFeatsOS),
	ord_subtract(FeatsOS, NamedFeatsOS, UnnamedFeats),
	default_feat_vals_for_feat_list(UnnamedFeats, DefaultFeatVals),
	append(FeatVals, DefaultFeatVals, FeatVals1),
	!.
add_lexical_feature_defaults_to_feats(_CatName, _FeatVals, _FeatVals1) :-
	regulus_error('Unable to add lexical feature defaults.', []).

feats_in_featvals(FeatVals, Feats) :-
	feats_in_featvals1(FeatVals, Feats0),
	safe_remove_duplicates(Feats0, Feats).

feats_in_featvals1([], []).
feats_in_featvals1([(Feat=_Val) | RestFeatVals], [Feat | RestFeats]) :-
	feats_in_featvals1(RestFeatVals, RestFeats).

default_feat_vals_for_feat_list([], []).
default_feat_vals_for_feat_list([Feat | RestFeats], [(Feat = Default) | RestFeatVals]) :-
	lexical_feature_default(Feat, Default),
	!,
	default_feat_vals_for_feat_list(RestFeats, RestFeatVals).
default_feat_vals_for_feat_list([_Feat | RestFeats], RestFeatVals) :-
	!,
	default_feat_vals_for_feat_list(RestFeats, RestFeatVals).

%---------------------------------------------------------------

remove_annotations_from_lex_items(X, X1) :-
	remove_annotations_from_lex_items1(X, X1),
	!.
remove_annotations_from_lex_items(_X, _X1) :-
	regulus_error('Bad call to remove_annotations_from_lex_items/2', []).

remove_annotations_from_lex_items1(V, _) :-
	var(V),
	!,
	fail.
remove_annotations_from_lex_items1((F, R), (F1, R1)) :-
	!,
	remove_annotations_from_lex_items1(F, F1),
	remove_annotations_from_lex_items1(R, R1).
remove_annotations_from_lex_items1((F ; R), (F1 ; R1)) :-
	!,
	remove_annotations_from_lex_items1(F, F1),
	remove_annotations_from_lex_items1(R, R1).
remove_annotations_from_lex_items1((?(F)), (?(F1))) :-
	!,
	remove_annotations_from_lex_items1(F, F1).
remove_annotations_from_lex_items1(X+_Annotation, X) :-
	!.
remove_annotations_from_lex_items1(Atom, Atom) :-
	atomic(Atom),
	!.

%---------------------------------------------------------------

expand_abbreviations_in_rules([], []) :-
	!.
expand_abbreviations_in_rules([F | R], [F1 | R1]) :-
	(   is_lexical_regulus_rule(F) ->
	    %F1 = F
	    expand_abbreviations_in_rule(F, F1) 
	;
	    expand_abbreviations_in_rule(F, F1) 
	),
	!,
	expand_abbreviations_in_rules(R, R1).
expand_abbreviations_in_rules(_RulesIn, _RulesOut) :-
	regulus_error('Bad call to expand_abbreviations_in_rules/2', []).

expand_abbreviations_in_rule(rule((CatName:FeatVals --> RHS), LineInfo),
			     rule((CatName:FeatVals1 --> RHS), LineInfo)) :-
	on_exception(
		     Exception, 
		     expand_abbreviations_in_feats(CatName, FeatVals, FeatVals1),
		     ( inform_about_regulus_exception(Exception, LineInfo), fail )
		    ),
	!.
expand_abbreviations_in_rule(rule(_Rule, LineInfo), _) :-
	inform_about_regulus_exception(regulus_exception('Bad call to expand_abbreviations_in_rule/2', []), LineInfo),
	fail.

expand_abbreviations_in_feats(CatName, _FeatVals, _FeatVals1) :-
	\+ category_internal(CatName, _FeatsOS),
	regulus_error('Undeclared category: ~w', [CatName]).
expand_abbreviations_in_feats(_CatName, FeatVals, FeatVals1) :-
	expand_abbreviations_in_feats1(FeatVals, FeatVals1).

expand_abbreviations_in_feats1([], []).
expand_abbreviations_in_feats1([F | R], [F1 | R1]) :-
	expand_abbreviations_in_featval_pair(F, F1),
	!,
	expand_abbreviations_in_feats1(R, R1).

expand_abbreviations_in_featval_pair(SemFeat=Sem, SemFeat=Sem1) :-
	member(SemFeat, [sem, gsem]),
	expand_multiple_concats_in_semval(Sem, Sem1),
	!.
expand_abbreviations_in_featval_pair(Other, Other).
 
expand_multiple_concats_in_semval(Atom, Atom) :-
	atomic(Atom),
	!.
expand_multiple_concats_in_semval(Var, Var) :-
	var(Var),
	!.
expand_multiple_concats_in_semval(Concat, Concat1) :-
	compound(Concat),
	( functor(Concat, concat, N) ; functor(Concat, strcat, N) ),
	N > 2,
	expand_multiple_concat(Concat, Concat1),
	!.
expand_multiple_concats_in_semval(Term, Term1) :-
	functor(Term, F, N),
	functor(Term1, F, N),
	expand_multiple_concats_in_semval_args(N, Term, Term1).

expand_multiple_concats_in_semval_args(I, _Term, _Term1) :-
	I =< 0,
	!.
expand_multiple_concats_in_semval_args(I, Term, Term1) :-
	I > 0,
	arg(I, Term, Arg),
	arg(I, Term1, Arg1),
	expand_multiple_concats_in_semval(Arg, Arg1),
	I1 is I - 1,
	!,
	expand_multiple_concats_in_semval_args(I1, Term, Term1).
 
expand_multiple_concat(concat(X, Y), concat(X, Y)) :-
	!.
expand_multiple_concat(ConcatIn, ConcatOut) :-
	ConcatIn =.. [concat, FirstArg | RestArgs],
	ConcatNext =.. [concat | RestArgs],
	ConcatOut = concat(FirstArg1, ConcatNext1),
	!,
	expand_multiple_concats_in_semval(FirstArg, FirstArg1),
	expand_multiple_concat(ConcatNext, ConcatNext1).
expand_multiple_concat(strcat(X, Y), strcat(X, Y)) :-
	!.
expand_multiple_concat(StrcatIn, StrcatOut) :-
	StrcatIn =.. [strcat, FirstArg | RestArgs],
	StrcatNext =.. [strcat | RestArgs],
	StrcatOut = strcat(FirstArg1, StrcatNext1),
	!,
	expand_multiple_concats_in_semval(FirstArg, FirstArg1),
	expand_multiple_concat(StrcatNext, StrcatNext1).

%---------------------------------------------------------------

expand_macros_in_rule_list(RulesIn, RulesOut) :-
	findall(RuleOut, expand_macros_in_rule_list_member(RulesIn, RuleOut), RulesOut).

expand_macros_in_rule_list_member(RulesIn, RuleOut) :-
	member(RuleIn, RulesIn),
	expand_macros_in_rule_with_line_info(RuleIn, RuleOut).
 
expand_macros_in_rule_with_line_info(rule(Label, RuleBodyIn, LineInfo), rule(Label, RuleBodyOut, LineInfo)) :-
	on_exception(
	Exception, 
	expand_macros_in_rule_body(RuleBodyIn, RuleBodyOut),
	(
	  inform_about_regulus_exception(Exception, LineInfo),
	  regulus_error('Fatal compilation error', [])
	)
    ).

expand_macros_in_rule_body(dynamic_lexicon(LexIn), Result) :-
	expand_dynamic_lex_entry(dynamic_lexicon(LexIn), Result).
expand_macros_in_rule_body(RuleBodyIn, RuleBodyOut) :-
	\+ safe_subsumes_chk(dynamic_lexicon(_), RuleBodyIn),
	expand_macros_in_term(RuleBodyIn, RuleBodyNext),
	%% addition by peter ljunglöf, 4 jan 08, 11 jan 08
	apply_ground_gsl_functions(RuleBodyNext, RuleBodyNextNext),
	%% end addition
	flatten_and_check_after_macro_expansion(RuleBodyNextNext, RuleBodyOut).

expand_macros_in_term(In, Out) :-
	expand_macros_in_term(In, Out, []).

expand_macros_in_term(V, V, _Previous) :-
	var(V),
	!.
expand_macros_in_term(T, T, _Previous) :-
	\+ term_contains_macro_calls(T),
	!.
expand_macros_in_term((LHS=@MacroInvocation), TOut, Previous) :-
	!,
	expand_macros_in_term((LHS = @MacroInvocation), TOut, Previous).
expand_macros_in_term(@TIn, TOut, Previous) :-
	!,
	expand_macros_in_term(TIn, TInWithExpandedArgs, Previous),
	check_ok_macro_invocation(TInWithExpandedArgs, Previous, MacroOrDefaultMacro),
	macro_or_default_macro(MacroOrDefaultMacro, TInWithExpandedArgs, TNext),
	expand_macros_in_term(TNext, TOut, [TInWithExpandedArgs | Previous]).
expand_macros_in_term(TIn, TOut, Previous) :-
	functor(TIn, F, N),
	functor(TOut, F, N),
	expand_macros_in_term_args(N, TIn, TOut, Previous).

expand_macros_in_term_args(0, _TIn, _TOut, _Previous).
expand_macros_in_term_args(I, TIn, TOut, Previous) :-
	I > 0,
	arg(I, TIn, ArgIn),
	arg(I, TOut, ArgOut),
	expand_macros_in_term(ArgIn, ArgOut, Previous),
	I1 is I - 1,
	expand_macros_in_term_args(I1, TIn, TOut, Previous).	

macro_or_default_macro(macro, In, Out) :-
	macro(In, Out).
macro_or_default_macro(default_macro, In, Out) :-
	default_macro(In, Out).

term_contains_macro_calls(T) :-
	(   term_contains_functor(T, '@'/1)
	;
	    term_contains_functor(T, '=@'/2)
	),
	!.

%---------------------------------------------------------------
%% addition by peter ljunglöf, 4 jan 08
%% this will concatenate all ground occurrences of strcat(..) anywhere in a rule
%% making it possible to define macros such as this:
%%   macro(regularNoun(House,sing), House).
%%   macro(regularNoun(House,plur), strcat(House,s)).

%% modification by peter ljunglöf, 11 jan 08
%% added the GSL functions neg/1, add/2, sub/2, mul/2, div/2
%% also changed the name from concatenate_strings_in_term/2

apply_ground_gsl_functions(X, X) :- var(X), !.
apply_ground_gsl_functions(strcat(A,B), ABStr) :-
	!, apply_ground_gsl_functions(A, AStr),
	apply_ground_gsl_functions(B, BStr),
	( atom(AStr), atom(BStr) -> atom_concat(AStr, BStr, ABStr)
	; compound(AStr), AStr = (AH, AT) ->
	    apply_ground_gsl_functions(strcat(AT, BStr), ABStrT), ABStr = (AH, ABStrT)
	; compound(BStr), BStr = (BH, BT) ->
	    apply_ground_gsl_functions(strcat(AStr, BH), ABStrH), ABStr = (ABStrH, BT)
	; otherwise -> ABStr = strcat(AStr, BStr)
	).
apply_ground_gsl_functions(neg(A), ANeg) :-
	!, apply_ground_gsl_functions(A, ANum),
	( number(ANum) -> ANeg is -ANum
	; ANeg = neg(ANum)
	).
apply_ground_gsl_functions(add(A,B), ABSum) :-
	!, apply_ground_gsl_functions(A, ANum),
	apply_ground_gsl_functions(B, BNum),
	( number(ANum), number(BNum) -> ABSum is ANum+BNum
	; ABSum = add(ANum,BNum)
	).
apply_ground_gsl_functions(sub(A,B), ABDiff) :-
	!, apply_ground_gsl_functions(A, ANum),
	apply_ground_gsl_functions(B, BNum),
	( number(ANum), number(BNum) -> ABDiff is ANum-BNum
	; ABDiff = sub(ANum,BNum)
	).
apply_ground_gsl_functions(mul(A,B), ABProduct) :-
	!, apply_ground_gsl_functions(A, ANum),
	apply_ground_gsl_functions(B, BNum),
	( number(ANum), number(BNum) -> ABProduct is ANum*BNum
	; ABProduct = mul(ANum,BNum)
	).
apply_ground_gsl_functions(div(A,B), ABQuotient) :-
	!, apply_ground_gsl_functions(A, ANum),
	apply_ground_gsl_functions(B, BNum),
	( number(ANum), number(BNum) -> ABQuotient is ANum/BNum
	; ABQuotient = add(ANum,BNum)
	).
apply_ground_gsl_functions(TIn, TOut) :-
	functor(TIn, F, N),
	functor(TOut, F, N),
	apply_ground_gsl_functions_args(N, TIn, TOut).

apply_ground_gsl_functions_args(0, _TIn, _TOut) :- !.
apply_ground_gsl_functions_args(I, TIn, TOut) :-
	arg(I, TIn, ArgIn),
	arg(I, TOut, ArgOut),
	apply_ground_gsl_functions(ArgIn, ArgOut),
	I1 is I-1,
	apply_ground_gsl_functions_args(I1, TIn, TOut).

%% end addition, 4 jan 08
%---------------------------------------------------------------

flatten_and_check_after_macro_expansion(RuleIn, _RuleOut) :-
	var(RuleIn),
	!,
	regulus_error('~NResult of macro-expansion ~w not a rule~n', [RuleIn]).
flatten_and_check_after_macro_expansion(RuleIn, RuleOut) :-
	RuleIn = ( _LHS --> _RHS ),
	!,
	flatten_featval_lists_in_rule_body(RuleIn, RuleOut),
	check_ok_rule_after_macro_expansion(RuleOut).
flatten_and_check_after_macro_expansion(RuleIn, RuleIn) :-
	(   RuleIn = transfer_lexicon(_LHS, _RHS) ;
	    RuleIn = reverse_transfer_lexicon(_LHS, _RHS) ;
	    RuleIn = bidirectional_transfer_lexicon(_LHS, _RHS)
	),
	!,
	check_ok_transfer_lexicon_entry_after_macro_expansion(RuleIn).
flatten_and_check_after_macro_expansion(RuleIn, RuleOut) :-
	(   RuleIn = transfer_rule(LHS, RHS) ;
	    RuleIn = reverse_transfer_rule(LHS, RHS) ;
	    RuleIn = bidirectional_transfer_rule(LHS, RHS)
	),
	RuleIn =.. [RuleType, LHS, RHS],
	!,
	flatten_transfer_representation(LHS, LHS1),
	flatten_transfer_representation(RHS, RHS1),
	RuleOut =.. [RuleType, LHS1, RHS1],
	check_ok_transfer_rule_after_macro_expansion(RuleOut).
flatten_and_check_after_macro_expansion(RuleIn, RuleOut) :-
	RuleIn = ( transfer_rule(LHS, RHS) :- ContextConditions ), 
	!,
	flatten_transfer_representation(LHS, LHS1),
	flatten_transfer_representation(RHS, RHS1),
	RuleOut = ( transfer_rule(LHS1, RHS1) :- ContextConditions ), 
	check_ok_transfer_rule_after_macro_expansion(RuleOut).
flatten_and_check_after_macro_expansion(RuleIn, RuleOut) :-
	RuleIn = ( reverse_transfer_rule(LHS, RHS) :- ContextConditions ), 
	!,
	flatten_transfer_representation(LHS, LHS1),
	flatten_transfer_representation(RHS, RHS1),
	RuleOut = ( reverse_transfer_rule(LHS1, RHS1) :- ContextConditions ), 
	check_ok_transfer_rule_after_macro_expansion(RuleOut).
flatten_and_check_after_macro_expansion(RuleIn, RuleOut) :-
	RuleIn = ( bidirectional_transfer_rule(LHS, RHS) :- ContextConditions ), 
	!,
	flatten_transfer_representation(LHS, LHS1),
	flatten_transfer_representation(RHS, RHS1),
	RuleOut = ( bidirectional_transfer_rule(LHS1, RHS1) :- ContextConditions ), 
	check_ok_transfer_rule_after_macro_expansion(RuleOut).
flatten_and_check_after_macro_expansion(RuleIn, RuleIn) :-
	(   RuleIn = role_transfer_rule(LHS, RHS) ;
	    RuleIn = reverse_role_transfer_rule(LHS, RHS) ;
	    RuleIn = bidirectional_role_transfer_rule(LHS, RHS) ;
	    RuleIn = ( role_transfer_rule(LHS, RHS) :- _ContextConditions ) ;
	    RuleIn = ( reverse_role_transfer_rule(LHS, RHS) :- _ContextConditions ) ;
	    RuleIn = ( bidirectional_role_transfer_rule(LHS, RHS) :- _ContextConditions ) ;
	    RuleIn = ( role_list_transfer_rule(_Roles) :- _ContextConditions ) 
	),
	check_ok_role_transfer_rule_after_macro_expansion(RuleIn).

flatten_and_check_after_macro_expansion(RuleIn, RuleOut) :-
	is_lf_pattern_entry(RuleIn),
	!,
	RuleIn = RuleOut.
flatten_and_check_after_macro_expansion(RuleIn, RuleOut) :-
	is_lf_rewrite_entry(RuleIn),
	!,
	RuleIn = RuleOut.
flatten_and_check_after_macro_expansion(RuleIn, RuleIn).


%---------------------------------------------------------------

flatten_list(V, V) :-
	var(V),
	!.
flatten_list(A, A) :-
	atomic(A),
	!.
flatten_list([F | R], Result) :-
	is_list(F),
	!,
	flatten(F, F1),
	flatten(R, R1),
	append(F1, R1, Result).
flatten_list([F | R], [F | R1]) :-
	!,
	flatten(R, R1),
flatten_list(Other, Other).

%---------------------------------------------------------------

flatten_featval_lists_in_rule_body(In, Out) :-
	flatten_featval_lists_in_rule_body1(In, Out),
	!.
flatten_featval_lists_in_rule_body(In, Out) :-
	regulus_error('~NBad call: ~w~n', [flatten_featval_lists_in_rule_body(In, Out)]).

flatten_featval_lists_in_rule_body1(V, V) :-
	var(V),
	!.
flatten_featval_lists_in_rule_body1(A, A) :-
	atomic(A),
	!.
flatten_featval_lists_in_rule_body1(Cat:FeatVals, Cat:FeatVals1) :-
	!,
	flatten_featval_list(FeatVals, FeatVals1).
flatten_featval_lists_in_rule_body1(T, T1) :-
	functor(T, F, N),
	functor(T1, F, N),
	flatten_featval_lists_in_rule_body1_args(N, T, T1).

flatten_featval_lists_in_rule_body1_args(0, _T, _T1).
flatten_featval_lists_in_rule_body1_args(I, T, T1) :-
	I > 0,
	arg(I, T, Arg),
	arg(I, T1, Arg1),
	flatten_featval_lists_in_rule_body1(Arg, Arg1),
	I1 is I - 1,
	!,
	flatten_featval_lists_in_rule_body1_args(I1, T, T1).

flatten_featval_list([], []) :-
	!.
flatten_featval_list([F | R], [F | R1]) :-
	nonvar(F),
	F = (Feat = _Val),
	atomic(Feat),
	flatten_featval_list(R, R1),
	!.
flatten_featval_list([F | R], Result) :-
	is_list(F),
	flatten_featval_list(F, F1),
	flatten_featval_list(R, R1),
	append(F1, R1, Result),
	!.
flatten_featval_list(Other, Out) :-
	regulus_error('~NBad call: ~w~n', [flatten_featval_list(Other, Out)]).

%---------------------------------------------------------------

check_ok_rule_after_macro_expansion(Rule) :-
	is_macro_expanded_regulus_rule(Rule),
	!.
check_ok_rule_after_macro_expansion(Rule) :-
	regulus_error('~NResult of macro-expansion ~w not a rule~n', [Rule]).

check_ok_transfer_lexicon_entry_after_macro_expansion(Rule) :-
	is_lexical_transfer_entry(Rule),
	!.
check_ok_transfer_lexicon_entry_after_macro_expansion(Rule) :-
	regulus_error('~NResult of macro-expansion ~w not a transfer lexicon entry~n', [Rule]).

check_ok_transfer_rule_after_macro_expansion(Rule) :-
	is_non_lexical_transfer_entry(Rule),
	!.
check_ok_transfer_rule_after_macro_expansion(Rule) :-
	regulus_error('~NResult of macro-expansion ~w not a transfer rule~n', [Rule]).

check_ok_role_transfer_rule_after_macro_expansion(Rule) :-
	is_non_lexical_transfer_entry(Rule),
	!.
check_ok_role_transfer_rule_after_macro_expansion(Rule) :-
	regulus_error('~NResult of macro-expansion ~w not a transfer rule~n', [Rule]).

%---------------------------------------------------------------

is_macro_expanded_regulus_rule(Rule) :-
	nonvar(Rule),
	Rule = (LHS --> RHS),
	nonvar(LHS),
	(   current_predicate(user:regulus_config/2), user:regulus_config(prolog_semantics, yes)
	;
	    nonvar(RHS)
	),
	is_macro_expanded_regulus_rule_head(LHS),
	is_macro_expanded_regulus_rule_body(RHS),
	!.

is_macro_expanded_regulus_rule_head(Head) :-
	is_macro_expanded_cat(Head),
	!.

is_macro_expanded_regulus_rule_body(V) :-
	var(V),
	!,
	current_predicate(user:regulus_config/2),
	user:regulus_config(prolog_semantics, yes).
is_macro_expanded_regulus_rule_body((P, Q)) :-
	is_macro_expanded_regulus_rule_body(P),
	is_macro_expanded_regulus_rule_body(Q),
	!.
is_macro_expanded_regulus_rule_body((P ; Q)) :-
	is_macro_expanded_regulus_rule_body(P),
	is_macro_expanded_regulus_rule_body(Q),
	!.
is_macro_expanded_regulus_rule_body((?(P))) :-
	is_macro_expanded_regulus_rule_body(P),
	!.
is_macro_expanded_regulus_rule_body(Cat) :-
	is_macro_expanded_cat(Cat),
	!.
is_macro_expanded_regulus_rule_body(Body+Comment) :-
	atom(Comment),
	is_macro_expanded_regulus_rule_body(Body),
	!.
is_macro_expanded_regulus_rule_body(Atom) :-
	atom(Atom),
	!.

is_macro_expanded_cat(Cat) :-
	nonvar(Cat),
	Cat = CatName:FeatVals,
	atom(CatName),
	is_list(FeatVals),
	!.

%---------------------------------------------------------------

% If there is neither a macro nor a default macro definition that matches, signal an error.
check_ok_macro_invocation(LHS, _Previous, _MacroOrDefaultMacro) :-
	\+ macro(LHS, _RHS),
	\+ default_macro(LHS, _RHS),
	!,
	regulus_error('~NNo expansion defined for macro invocation ~w~n', [LHS]).
% If we have a cyclical macro expansion, signal an error.
check_ok_macro_invocation(LHS, Previous, _MacroOrDefaultMacro) :-
	(   compound(LHS) ;
	    atom(LHS)
	),
	id_member(LHS, Previous),
	!,
	regulus_error('~NCycle in macro invocation: ~w~n', [LHS]).
% If there is no macro definition that matches, then use a default macro.
% (We know there is one by now, since we got past the first clause).
check_ok_macro_invocation(LHS, _Previous, MacroOrDefaultMacro) :-
	\+ macro(LHS, _RHS),
	MacroOrDefaultMacro = default_macro,
	!.
% If we got this far, there is a macro that works, so use that.
check_ok_macro_invocation(_LHS, _Previous, MacroOrDefaultMacro) :-
	MacroOrDefaultMacro = macro,
	!.

%---------------------------------------------------------------

:- dynamic cat_used/1.
:- dynamic feature_used/2.

warn_about_unused_features(Rules) :-
	init_warn_about_unused_features,
	mark_used_features(Rules),
	warn_about_unused_features1,
	!.
warn_about_unused_features(_Rules) :-
	regulus_error('~NCall to warn_about_unused_features/1 failed.~n', []).

init_warn_about_unused_features :-
	retractall(cat_used(_)),
	retractall(feature_used(_, _)).

mark_used_features(Var) :-
	var(Var),
	!.
mark_used_features(Atom) :-
	atomic(Atom),
	!.
mark_used_features(Cat:FeatVals) :-
	!,
	(   is_list(FeatVals) ->
	    mark_used_cat(Cat),
	    mark_used_features_for_cat(Cat, FeatVals)
	;
	    otherwise ->
	    true
	).
mark_used_features(Term) :-
	compound(Term),
	functor(Term, _F, N),
	mark_used_features_args(N, Term),
	!.
mark_used_features(Other) :-
	regulus_error('~NBad call: ~w~n', [mark_used_features(Other)]).

mark_used_features_args(0, _Term).
mark_used_features_args(I, Term) :-
	I > 0,
	arg(I, Term, Arg),
	mark_used_features(Arg),
	I1 is I - 1,
	!,
	mark_used_features_args(I1, Term).

mark_used_features_for_cat(_Cat, []) :-
	!.
mark_used_features_for_cat(Cat, [F | R]) :-
	mark_used_feature_for_cat_and_featval(Cat, F),
	!,
	mark_used_features_for_cat(Cat, R).

mark_used_cat(Cat) :-
	(   cat_used(Cat) ->
	    true
	;
	    otherwise ->
	    assertz(cat_used(Cat))
	),
	!.
mark_used_cat(Cat) :-
	regulus_error('~NBad call: ~w~n', [mark_used_cat(Cat)]).

mark_used_feature_for_cat_and_featval(Cat, Feat=_Val) :-
	(   feature_used(Cat, Feat) ->
	    true
	;
	    otherwise ->
	    assertz(feature_used(Cat, Feat))
	),
	!.
mark_used_feature_for_cat_and_featval(Cat, F) :-
	regulus_error('~NBad call: ~w~n', [mark_used_feature_for_cat_and_featval(Cat, F)]).

warn_about_unused_features1 :-
	unused_cat_feat_pair(Cat, Feat),
	format('~N*** Warning: feature \'~w\' in category \'~w\' is never used~n', [Feat, Cat]),
	fail.
warn_about_unused_features1.

unused_cat_feat_pair(Cat, Feat) :-
	category_internal(Cat, Feats),
	member(Feat, Feats),
	\+ feature_used(Cat, Feat),
	cat_used(Cat).

%---------------------------------------------------------------

canonicalise_rules([], []).
canonicalise_rules([F | R], [F1 | R1]) :-
	canonicalise_rule0(F, F1),
	!,
	canonicalise_rules(R, R1).

canonicalise_rule0(rule(Rule, LineInfo),
		   rule(Rule1, LineInfo)) :-
	on_exception(Exception,
		     canonicalise_rule(Rule, Rule1),
		     (   nl,
			 inform_about_regulus_exception(Exception, LineInfo),
			 nl, nl,
			 fail
		     )
		    ),
	!.
canonicalise_rule0(F, F).

canonicalise_rule((Head --> Body), (Head --> Body1)) :-
	canonicalise_rule_body(Body, Body1).

canonicalise_rule_body((P, Q), Result) :-
	canonicalise_rule_body(P, P1),
	canonicalise_rule_body(Q, Q1),
	comma_list_to_list(P1, P1List),
	comma_list_to_list(Q1, Q1List),
	append(P1List, Q1List, ResultList),
	list_to_comma_list(ResultList, Result),
	!.
canonicalise_rule_body((P ; Q), (P1 ; Q1)) :-
	canonicalise_rule_body(P, P1),
	canonicalise_rule_body(Q, Q1),
	!.
canonicalise_rule_body(?(P), ?(P1)) :-
	canonicalise_rule_body(P, P1),
	!.
canonicalise_rule_body(Atom, LowerCaseAtom) :-
	atom(Atom),
	convert_lexical_items_to_lower_case,
	lowercase_atom(Atom, LowerCaseAtom).
canonicalise_rule_body(Other, Other).

% Do this in a way that allows an earlier setting to override a later one.
convert_lexical_items_to_lower_case :-
	current_predicate(user:regulus_config/2),
	user:regulus_config(convert_lexical_items_to_lower_case, Value),
	!,
	Value = yes.

%---------------------------------------------------------------

warn_about_inconsistent_feature_spaces([]).
warn_about_inconsistent_feature_spaces([F | R]) :-
	warn_about_inconsistent_feature_spaces_in_rule(F),
	!,
	warn_about_inconsistent_feature_spaces(R).

warn_about_inconsistent_feature_spaces_in_rule(rule(_Label, Rule, LineInfo)) :-
	!,
	warn_about_inconsistent_feature_spaces_in_rule(rule(Rule, LineInfo)).
warn_about_inconsistent_feature_spaces_in_rule(rule(Rule, LineInfo)) :-
	!,
	copy_term(Rule, Rule1),
	on_exception(Exception, 
		     instantiate_features_to_check_consistency(Rule1),
		     (   nl,
			 inform_about_regulus_exception(Exception, LineInfo),
			 nl, nl,
			 fail
		     )
		    ).
warn_about_inconsistent_feature_spaces_in_rule(_Other).

instantiate_features_to_check_consistency((Head --> Body)) :-
	instantiate_features_to_check_consistency(Head),
	instantiate_features_to_check_consistency(Body),
	!.
instantiate_features_to_check_consistency((P, Q)) :-
	instantiate_features_to_check_consistency(P),
	instantiate_features_to_check_consistency(Q),
	!.
instantiate_features_to_check_consistency((P ; Q)) :-
	instantiate_features_to_check_consistency(P),
	instantiate_features_to_check_consistency(Q),
	!.
instantiate_features_to_check_consistency(?(P)) :-
	instantiate_features_to_check_consistency(P),
	!.
instantiate_features_to_check_consistency(Cat:Feats) :-
	instantiate_features_to_check_consistency1(Feats, Cat),
	!.
instantiate_features_to_check_consistency(_Other).

instantiate_features_to_check_consistency1([], _Cat).
instantiate_features_to_check_consistency1([Feat=Val | R], Cat) :-
	instantiate_feature_to_check_consistency1(Feat, Val, Cat),
	!,
	instantiate_features_to_check_consistency1(R, Cat).

instantiate_feature_to_check_consistency1(Feat, Val, Cat) :-
	member(Feat, [sem, gsem]),
	!,
	instantiate_sem_structure_to_check_consistency(Feat, Val, Cat).
instantiate_feature_to_check_consistency1(Feat, Val, Cat) :-
	(   feature(Feat, Space) ->
	    true
	;
	    true ->
	    regulus_error('Unknown feature ~w', [Feat])
	),
	(   var(Val) ->
	    Val = cat_feat_space(Cat, Feat, Space)
	;
	    Val = cat_feat_space(_OtherCat, _OtherFeat, Space) ->
	    true
	;
	    Val = cat_feat_space(OtherCat, OtherFeat, OtherSpace) ->
	    signal_inconsistent_feature_exception(cat_feat_space(OtherCat, OtherFeat, OtherSpace), cat_feat_space(Cat, Feat, Space))
	;
	    otherwise ->
	    true
	).

instantiate_sem_structure_to_check_consistency(Feat, Val, Cat) :-
	(   var(Val) ->
	    Val = cat_feat_space(Cat, Feat, semantic_value)
	;
	    Val = cat_feat_space(_OtherCat, _OtherFeat, semantic_value) ->
	    true
	;
	    Val = cat_feat_space(OtherCat, OtherFeat, OtherSpace) ->
	    signal_inconsistent_feature_exception(cat_feat_space(OtherCat, OtherFeat, OtherSpace), cat_feat_space(Cat, Feat, semantic_value))
	;
	    compound(Val) ->
	    functor(Val, _F, N),
	    instantiate_sem_structure_to_check_consistency_args(N, Feat, Val, Cat)
	;
	    otherwise ->
	    true
	).

instantiate_sem_structure_to_check_consistency_args(I, _Feat, _Val, _Cat) :-
	I < 1,
	!.
instantiate_sem_structure_to_check_consistency_args(I, Feat, Val, Cat) :-
	arg(I, Val, Arg),
	instantiate_sem_structure_to_check_consistency(Feat, Arg, Cat),
	I1 is I - 1,
	!,
	instantiate_sem_structure_to_check_consistency_args(I1, Feat, Val, Cat).
	    
signal_inconsistent_feature_exception(FeatUse1, FeatUse2) :-	
	feature_use_description(FeatUse1, FeatUseDescription1),
	feature_use_description(FeatUse2, FeatUseDescription2),
	Format = 'Inconsistent feature spaces: ~w and ~w',
	Args = [FeatUseDescription1, FeatUseDescription2],
	regulus_error(Format, Args).

feature_use_description(cat_feat_space(Cat, Feat, Space), FeatUseDescription) :-
	format_to_atom('~w:[... ~w=<value in ~w> ...]', [Cat, Feat, Space], FeatUseDescription),
	!.
feature_use_description(FeatUse1, FeatUseDescription1) :-
	Format = 'Bad call: ~w',
	Args = [feature_use_description(FeatUse1, FeatUseDescription1)],
	regulus_error(Format, Args).

%---------------------------------------------------------------

is_list_of_lists_of_cat_vals([]).
is_list_of_lists_of_cat_vals([F | R]) :-
	is_list_of_cat_vals(F),
	!,
	is_list_of_lists_of_cat_vals(R).

is_list_of_cat_vals([]).
is_list_of_cat_vals([F | R]) :-
	possible_cat_val(F), 
	!,
	is_list_of_cat_vals(R).

possible_cat_val(Atom) :-
	atomic(Atom).
possible_cat_val(CatSpec) :-
	nonvar(CatSpec),
	CatSpec = syn_term(Cat),
	atom(Cat).

%---------------------------------------------------------------

internalise_collocation_decls(ReadDeclarations) :-
	retract_regulus_preds_for_collocation_rules,
	internalise_collocation_decls1(ReadDeclarations),
	!.
internalise_collocation_decls(_ReadDeclarations) :-
	Format = 'Bad call: ~w',
	Args = ['internalise_collocation_decls(...)'],
	regulus_error(Format, Args).

internalise_collocation_decls1([]).
internalise_collocation_decls1([F | R]) :-
	internalise_collocation_decl(F),
	!,
	internalise_collocation_decls1(R).

internalise_collocation_decl(declaration(_Label, collocation_macro(LHS, RHS), _LineInfo)) :-
	assertz(collocation_macro_internal(LHS, RHS)),
	!.
internalise_collocation_decl(declaration(collocation_macro(LHS, RHS), _LineInfo)) :-
	assertz(collocation_macro_internal(LHS, RHS)),
	!.
internalise_collocation_decl(Other) :-
	Format = 'Bad call: ~w',
	Args = [internalise_collocation_decl(Other)],
	regulus_error(Format, Args).

%---------------------------------------------------------------

expand_collocation_macros_in_rules(ReadRules, ExpandedRules) :-
	findall(ExpandedRule,
		(   member(Rule, ReadRules),
		    expand_collocation_macros_in_rule(Rule, ExpandedRule)
		),
		ExpandedRules),
	!.
expand_collocation_macros_in_rules(_ReadRules, _ExpandedRules) :-
	Format = 'Unable to expand collocation rules',
	Args = [],
	regulus_error(Format, Args).
	
expand_collocation_macros_in_rule(Rule, ExpandedRule) :-
	get_line_info_lhs_and_rhs_from_collocation_rule(Rule, LineInfo, F, LHS, RHS),
	expand_collocation_macros_in_rule1(F, LHS, RHS, LineInfo, ExpandedRule).

get_line_info_lhs_and_rhs_from_collocation_rule(Rule, LineInfo, F, LHS, RHS) :-
	(  Rule = rule(Body, LineInfo) ; Rule = rule(_Label, Body, LineInfo)  ),
	compound(Body),
	Body =.. [F, LHS, RHS],
	!.

expand_collocation_macros_in_rule1(F, LHS, RHS, LineInfo, ExpandedRule) :-
	on_exception(
	Exception, 
	expand_collocation_macros_in_rule2(F, LHS, RHS, ExpandedRule),
	inform_about_regulus_exception(Exception, LineInfo)
    ).

/*
better_collocation("SAY: YOU HAVE {@number(N)} SISTER",
		   "multimedia:prompt-ask-how-many-sisters-you-have {@number(N)}").
*/

expand_collocation_macros_in_rule2(F, LHS, RHS, ExpandedRule) :-
	tokenize_collocation_rule_lhs_or_rhs(LHS, LHS1, []-LHSVars),
	tokenize_collocation_rule_lhs_or_rhs(RHS, RHS1, LHSVars-_Vars),
	substitute_from_collocation_macros(LHS1, LHS2),
	substitute_from_collocation_macros(RHS1, RHS2),
	append_list(LHS2, LHS3),
	append_list(RHS2, RHS3),
	ExpandedRule =.. [F, LHS3, RHS3].

tokenize_collocation_rule_lhs_or_rhs(In, Out, VarsIn-VarsOut) :-
	collocation_rule_lhs_or_rhs(Out, VarsIn-VarsOut, In, []),
	!.
tokenize_collocation_rule_lhs_or_rhs(In, _Out, _Substitutions) :-
	regulus_error('~NIll-formed string "~s" in collocation rule~n', [In]).

collocation_rule_lhs_or_rhs([F | R], VarsIn-VarsOut) -->
	collocation_rule_lhs_or_rhs_component(F, VarsIn-VarsNext),
	!,
	collocation_rule_lhs_or_rhs(R, VarsNext-VarsOut).
collocation_rule_lhs_or_rhs([], VarsIn-VarsIn) -->
	[].

collocation_rule_lhs_or_rhs_component(macro_call(Body), VarsIn-VarsOut) -->
	"{@",
	non_curly_bracket_string(MacroStr),
	{ MacroStr \== [] },
	"}",
	{ add_or_use_var_pairs(MacroStr, Body, VarsIn-VarsOut) },
	!.
collocation_rule_lhs_or_rhs_component(str(Str), VarsIn-VarsIn) -->
	non_curly_bracket_string(Str),
	{ Str \== [] },
	!.

non_curly_bracket_string([F | R]) -->
	non_curly_bracket_char(F),
	!,
	non_curly_bracket_string(R).
non_curly_bracket_string([]) -->
	[].

non_curly_bracket_char(Char) -->
	[Char],
	{ \+ is_curly_bracket_char(Char) },
	!.

is_curly_bracket_char(0'{).
is_curly_bracket_char(0'}).

add_or_use_var_pairs(MacroStr, Body, VarsIn-VarsOut) :-
	append(MacroStr, ".", MacroStrWithPeriod),
	safe_read_from_chars(MacroStrWithPeriod, Body, [variable_names(Vars)]),
	add_or_use_var_pairs1(Vars, VarsIn-VarsOut).

add_or_use_var_pairs1([], VarsIn-VarsIn).
add_or_use_var_pairs1([F | R], VarsIn-VarsOut) :-
	(   member(F, VarsIn) ->
	    VarsNext = VarsIn
	;
	    otherwise ->
	    VarsNext = [F | VarsIn]
	),
	!,
	add_or_use_var_pairs1(R, VarsNext-VarsOut).

substitute_from_collocation_macros([], []).
substitute_from_collocation_macros([F | R], [F1 | R1]) :-
	substitute_from_collocation_macros_single(F, F1),
	substitute_from_collocation_macros(R, R1).

substitute_from_collocation_macros_single(str(Str), Str) :-
	!.
substitute_from_collocation_macros_single(macro_call(Body), Value) :-
	expand_collocation_macro_call(Body, PossibleValues),
	member(Value, PossibleValues).

expand_collocation_macro_call(Body, Expanded) :-
	findall(SomeExpanded,
		collocation_macro_internal(Body, SomeExpanded),
		AllExpanded),
	(   AllExpanded = [] ->
	    regulus_error('~NUnable to expand collocation macro call "~w" in collocation rule~n', [Body])
	;
	    otherwise ->
	    member(Expanded, AllExpanded),
	    % Need to instantiate Body
	    collocation_macro_internal(Body, Expanded)
	).

