
:- module(compile_lf_rewrite,
	  [compile_lf_rewrite_rules/2]
    ).

:- use_module('$REGULUS/Prolog/regulus_read').
:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/utilities').
:- use_module(library(lists)).

compile_lf_rewrite_rules(InFile, TmpFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(TmpFile, AbsTmpFile),
	(   compile_lf_rewrite_rules1(AbsInFile, AbsTmpFile) ->
	    safe_compile_with_redefine_and_single_var_warnings_off(user, AbsTmpFile)
	;
	    format2error('~N*** Error: unable to compile ~w~n', [AbsInFile]),
	    fail
	).

compile_lf_rewrite_rules1(AbsInFile, AbsTmpFile) :-
	nl,
	read_lf_rewrite_file_or_files(AbsInFile, List),
	length(List, InN),
	format('~N -- Read file (~d rewrite rules) ~w~n', [InN, AbsInFile]),
	
	compile_lf_rewrite_rule_list(List, List1),

	length(List1, OutN),
	write_out_lf_rewrite_file(List1, AbsTmpFile),
	format('~N -- Written file (~d rewrite rules) ~w~n', [OutN, AbsTmpFile]),
	nl,
	!.

%----------------------------------------------------------------------------

compile_lf_rewrite_rule_list([], []).
compile_lf_rewrite_rule_list([F | R], Result) :-
	compile_lf_rewrite_rule(F, F1),
	append(F1, R1, Result),
	!,
	compile_lf_rewrite_rule_list(R, R1).

compile_lf_rewrite_rule(InRecord, OutRules) :-
	InRecord = rule(_Label, InRule, LineInfo),
	findall(OutRule,
		compile_lf_rewrite_rule1(InRule, OutRule),
		OutRules),
	(   OutRules = [] ->
	    complain_about_bad_rule(LineInfo),
	    fail
	;
	    otherwise ->
	    true
	).
compile_lf_rewrite_rule(InRecord, _OutRules) :-
	format2error('~N*** Error: bad record in file: ~w~n', [InRecord]),
	fail.

complain_about_bad_rule(LineInfo) :-
	format2error('~N*** Error compiling lf_rewrite rule', []),
	inform_about_line_info(LineInfo).

compile_lf_rewrite_rule1(InRule, OutRule) :-
	(   InRule = ( Head :- Body) ->
	    comma_list_to_list(Body, BodyList)
	;
	    Head = InRule,
	    BodyList = []
	),
	compile_lf_rewrite_rule2(Head, BodyList, From, To, Conds),
	OutRule = lf_rewrite(From, To, Conds).

compile_lf_rewrite_rule2(Head, BodyList, From, To, Conds) :-
	Head = lf_rewrite(From0, To0),
	comma_list_to_list(From0, From0List),
	From0List = [From0ListF | From0ListR],
	(   From = From0ListF,
	    CondsList0 = From0ListR,
	    To = To0
	;
	    member(From, From0ListR),
	    delete(From0List, From, CondsList0),
	    To = true
	),
	append(CondsList0, BodyList, CondsList),
	(   CondsList = [] ->
	    Conds = true
	;
	    list_to_comma_list(CondsList, Conds)
	).	    

%----------------------------------------------------------------------------

write_out_lf_rewrite_file(List, OutFile) :-
	open(OutFile, write, OutS),
	write_out_lf_rewrite_list(List, OutS),
	close(OutS).

write_out_lf_rewrite_list([], _S).
write_out_lf_rewrite_list([F | R], S) :-
	portray_clause(S, F), nl(S),
	!,
	write_out_lf_rewrite_list(R, S).

