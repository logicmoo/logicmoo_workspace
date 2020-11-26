% ebl_compile_operationality.pl

/*

- Top level pred: compile_operationality_file/2
- Recursive descent, adding "Goal" argument
- Entries:
  - change_rule_and_context(OldContext, NewContext)  -> operational_goal(Goal, OldContext, NewContext)
  - change_contextOldContext, NewContext)            -> change_rule_and_context(Goal, OldContext, NewContext)
- Structures:
  - and
  - or
  - not
- Primitives:
  - cat(Cat)          -> goal_for_cat(Goal, Cat)
  - gap               -> gap_goal_for_cat(Goal)
  - dominates(Cat)    -> goal_dominates_cat(Goal, Cat)
  - lexical           -> lexical_goal(Goal)

*/

%---------------------------------------------------------------

:- module(ebl_compile_operationality,
	  [compile_operationality_file/2]
	 ).

%---------------------------------------------------------------

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').
:- use_module(library(lists)).

%---------------------------------------------------------------

compile_operationality_file(InFile, OutFile) :-
	prolog_file_or_files_to_list_including_line_info(InFile, InList),
	compile_operationality(InList, MainList),
	standard_intro_material(Intro),
	append(Intro, MainList, OutList),
	length(OutList, N),
	safe_absolute_file_name(OutFile, AbsOutFile),
	list_to_prolog_file(OutList, AbsOutFile),
	format('~N--- Written compiled operationality file (~d items) ~w~n', [N, AbsOutFile]),
	!.
compile_operationality_file(InFile, _OutFile) :-
	format2error('~N*** Error: unable to compile operationality file(s) ~w~n', [InFile]),
	fail.

%---------------------------------------------------------------

compile_operationality([], []).
compile_operationality([F | R], [F1 | R1]) :-
	compile_operationality_item(F, F1),
	!,
	compile_operationality(R, R1).

compile_operationality_item(term(Term, _LineInfo), CompiledTerm) :-
	nonvar(Term),
	compile_operationality_rule(Term, CompiledTerm),
	numbervars(CompiledTerm, 0, _),
	!.
compile_operationality_item(term(_Term, LineInfo), _CompiledTerm) :-
	LineInfo = line_info(_InItemNumber, FromLine-ToLine, File),
	format('~N*** Bad operationality rule between lines ~d and ~d in file ~w~n', [FromLine, ToLine, File]),
	fail.

%---------------------------------------------------------------

compile_operationality_rule((LHS :- RHS), (LHS1 :- RHS1)) :-
        !,
	compile_operationality_rule_head(LHS, LHS1, Goal),
	compile_operationality_rule_body(RHS, RHS1, Goal).
compile_operationality_rule(_Term, _CompiledTerm) :-
	format2error('~N*** Error: operationality rule must be of form Head :- Body~n', []),
	fail.

%---------------------------------------------------------------

compile_operationality_rule_head(LHS, LHS1, Goal) :-
	LHS = change_rule_and_context(OldContext, NewContext),
	LHS1 = operational_goal(Goal, OldContext, NewContext),
	!.
compile_operationality_rule_head(LHS, LHS1, Goal) :-
	LHS = change_context(OldContext, NewContext),
	%LHS1 = change_rule_and_context(Goal, OldContext, NewContext),
	LHS1 = change_context_goal(Goal, OldContext, NewContext),
	!.
compile_operationality_rule_head(_LHS, _LHS1, _Goal) :-
	format2error('~N*** Error: LHS of operationality rule must be of form\n"change_rule_and_context(OldContext, NewContext)" or "change_context(OldContext, NewContext)"~n', []),
	fail.

compile_operationality_rule_body((P, Q), (P1, Q1), Goal) :-
	!,
	compile_operationality_rule_body(P, P1, Goal),
	compile_operationality_rule_body(Q, Q1, Goal).
compile_operationality_rule_body((P ; Q), (P1 ; Q1), Goal) :-
	!,
	compile_operationality_rule_body(P, P1, Goal),
	compile_operationality_rule_body(Q, Q1, Goal).
compile_operationality_rule_body((\+ P), (\+ P1), Goal) :-
	!,
	compile_operationality_rule_body(P, P1, Goal).
compile_operationality_rule_body(cat(Cat), goal_for_cat(Goal, Cat), Goal) :-
	!.
compile_operationality_rule_body(gap, gap_goal(Goal), Goal) :-
	!.
compile_operationality_rule_body(dominates(Cat), goal_dominates_cat(Goal, Cat), Goal) :-
	!.
compile_operationality_rule_body(dominates_but_not_through(Cat, NotThrough), goal_dominates_cat_but_not_through(Goal, Cat, NotThrough), Goal) :-
	!.
compile_operationality_rule_body(immediately_dominates(Cat), goal_immediately_dominates_cat(Goal, Cat), Goal) :-
	!.
compile_operationality_rule_body(dominates_lex(Lex), goal_dominates_lex(Goal, Lex), Goal) :-
	!.
compile_operationality_rule_body(immediately_dominates_lex(Lex), goal_immediately_dominates_lex(Goal, Lex), Goal) :-
	!.
compile_operationality_rule_body(lexical, lexical_goal(Goal), Goal) :-
	!.
compile_operationality_rule_body(RHS, _RHS1, _Goal) :-
	format2error('~N*** Error: unknown structure ~w on RHS of operationality rule~n', [RHS]),
	fail.

%---------------------------------------------------------------

standard_intro_material(List) :-
	List = [
		( :- module(tmp_ebl_operational,
			    [operational_goal/3,
			     change_context/3
			    ]
			   )
		),
		
		( :- use_module(library(lists)) ),

		( :- use_module('$REGULUS/PrologLib/utilities') ),

		( :- use_module('$REGULUS/Prolog/ebl_operational') )

	       ].



		