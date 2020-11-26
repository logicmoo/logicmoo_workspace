
:- module(compile_lf_patterns,
	  [compile_lf_patterns/3]
    ).

:- use_module('$REGULUS/Prolog/regulus_read').
:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/utilities').
:- use_module(library(lists)).

compile_lf_patterns(InFile, ModuleList, TmpFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(TmpFile, AbsTmpFile),
	(   compile_lf_patterns1(AbsInFile, ModuleList, AbsTmpFile) ->
	    safe_compile_with_redefine_and_single_var_warnings_off(input_manager, AbsTmpFile) ;
	    format2error('~N*** Error: unable to compile ~w~n', [AbsInFile]),
	    fail
	).

compile_lf_patterns1(AbsInFile, ModuleList, AbsTmpFile) :-
	nl,
	read_pattern_file(AbsInFile, List),
	length(List, InN),
	format('~N -- Read file (~d patterns) ~w~n', [InN, AbsInFile]),
	
	compile_lf_patterns2(List, List1),

	length(List1, OutN),
	write_out_pattern_file(List1, ModuleList, AbsTmpFile),
	format('~N -- Written file (~d patterns) ~w~n', [OutN, AbsTmpFile]),
	nl,
	!.

%----------------------------------------------------------------------------

read_pattern_file(InFile, List) :-
	read_lf_pattern_file_or_files(InFile, List).

%----------------------------------------------------------------------------

compile_lf_patterns2([], []).
compile_lf_patterns2([F | R], [F1 | R1]) :-
	compile_lf_pattern(F, F1),
	!,
	compile_lf_patterns2(R, R1).

compile_lf_pattern(InRecord, OutRule) :-
	InRecord = rule(_Label, InRule, LineInfo),
	(   compile_lf_pattern1(InRule, OutRule) ->
	    true ;
	    complain_about_bad_rule(LineInfo),
	    fail
	).
compile_lf_pattern(InRecord, _OutRule) :-
	format2error('~N*** Error: bad record in file: ~w~n', [InRecord]),
	fail.

complain_about_bad_rule(LineInfo) :-
	format2error('~N*** Error compiling LF pattern ', []),
	inform_about_line_info(LineInfo).

compile_lf_pattern1(InRule, OutRule) :-
	(   InRule = ( Head :- Body) ->
	    true ;
	    ( Head, Body ) = ( InRule, true )
	),
	compile_lf_pattern_head(Head, SubPattern, CurrentLF, FullLF, Head1),
	compile_lf_pattern_body(Body, SubPattern, CurrentLF, FullLF, Body1),
	OutRule = ( Head1 :- Body1 ),
	!.

compile_lf_pattern_head(Head, SubPattern, CurrentLF, FullLF, Head1) :-
	Head = lf_pattern(CurrentLF, Feat=Val),
	Head1 = compiled_lf_pattern(CurrentLF, FullLF, Feat=Val),
	SubPattern = '*no_sub_pattern*',
	!.
compile_lf_pattern_head(Head, SubPattern, CurrentLF, FullLF, Head1) :-
	Head = lf_pattern(CurrentLF, SubPattern, Feat=Val),
	Head1 = compiled_lf_pattern(CurrentLF, FullLF, Feat=Val),
	!.

compile_lf_pattern_head(Head, SubPattern, CurrentLF, FullLF, Head1) :-
	Head = lf_boundary(CurrentLF, X^BoundaryLF),
	Head1 = compiled_lf_boundary(CurrentLF, FullLF, X^BoundaryLF),
	SubPattern = '*no_sub_pattern*',
	!.
compile_lf_pattern_head(Head, SubPattern, CurrentLF, FullLF, Head1) :-
	Head = lf_boundary(CurrentLF, SubPattern, X^BoundaryLF),
	Head1 = compiled_lf_boundary(CurrentLF, FullLF, X^BoundaryLF),
	!.

compile_lf_pattern_body(Body, SubPattern, _CurrentLF, _FullLF, Body1) :-
	SubPattern = '*no_sub_pattern*',
	Body1 = Body,
	!.
compile_lf_pattern_body(Body, SubPattern, CurrentLF, FullLF, Body1) :-
	Body1 = ( match_sub_pattern(SubPattern, CurrentLF, FullLF), Body ),
	!.

%----------------------------------------------------------------------------

write_out_pattern_file(List, ModuleList, OutFile) :-
	open(OutFile, write, OutS),
	write_pattern_intro(OutS, ModuleList),
	write_out_pattern_list(List, OutS),
	close(OutS).

write_pattern_intro(S, ModuleList) :-
	write_module_declaration(S),
	write_ensure_loaded_declaration(S),
	write_use_modules_declarations(ModuleList, S),
	!.
write_pattern_intro(S, ModuleList) :-
	format2error('~N*** Error: bad call: ~w~n', [write_pattern_intro(S, ModuleList)]),
	fail.

write_module_declaration(S) :-
	format(S, ':- module(lf_patterns, [extract_feature_values/2]).~n~n', []).

write_ensure_loaded_declaration(S) :-
	format(S, ':- ensure_loaded(~q).~n~n', ['$REGULUS/Prolog/lf_patterns_runtime.pl']).

write_use_modules_declarations([], S) :-
	format(S, '~N~n%-----------------------------------------------~n~n', []).
write_use_modules_declarations([F | R], S) :-
	write_use_module_declaration(F, S),
	!,
	write_use_modules_declarations(R, S).

write_use_module_declaration(Module, S) :-
	format(S, ':- use_module(~q).~n', [Module]).

write_out_pattern_list([], _S).
write_out_pattern_list([F | R], S) :-
	portray_clause(S, F), nl(S),
	!,
	write_out_pattern_list(R, S).

