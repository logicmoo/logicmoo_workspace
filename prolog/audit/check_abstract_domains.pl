:- module(check_abstract_domains, [list_abstract_domains/0,
				   list_abstract_domains/1]).

% TODO: Update it to be integrated in audit_common.pl

:- use_module(library(location_utils)).
:- use_module(library(audit/abstract_domain)).

:- multifile
	prolog:message//1,
	prolog:message_location//1.

current_abstract_domain(product(fail, sideff)).

:- multifile abstract_domain_product:eval_product/4.

abstract_domain_product:eval_product(fail, sideff, (product(fail, A2), _),
				     product(fail, A2)).

prolog:message(acheck(abstract_domains)) -->
    ['Check Abstract Domains'].
prolog:message(acheck(progress(Module))) -->
    ['Scanning ~w'-[Module]].
prolog:message(acheck(abstract_domain(product(fail, sideff),
				      From, Calls, Args))) -->
    prolog:message_location(From),
    [' ~w ~w always fails (~w)'-Args],
    ( {\+ empty(Calls)} ->
      [nl,'  at '],
      {current_prolog_flag(verbose,Verbose)},
      call_locations(Calls, Verbose)
    ;
      []
    ).

prolog:message(acheck(partial_eval(From, Calls, Args))) -->
    prolog:message_location(From),
    [' partial evaluation of [~w] throws the exception ~w'-Args, nl],
    {current_prolog_flag(verbose,Verbose)},
    call_locations(Calls, Verbose).

empty([]).
empty([A|B]) :-
    empty(A),
    empty(B).

call_locations([], _) --> [].
call_locations([Call|Calls], Verbose) -->
    call_locations(Call, Verbose),
    print_space(Verbose),
    call_locations(Calls, Verbose).
call_locations(stack(PI, From), Verbose) -->
    print_call(Verbose, PI, From).

print_space(silent) --> [' '].
print_space(verbose) --> [nl].

print_call(silent, PI, _) --> !, ['~w'-PI].
print_call(_,      PI, From) -->
    prolog:message_location(From),
    [': ~w'-[PI]].

list_abstract_domains :-
    print_message(information, acheck(abstract_domains)),
    list_abstract_domains(_).

% Top-down

%not_df_predicate_property(multifile).
%not_df_predicate_property(meta_predicate).
%not_df_predicate_property(dynamic).
%not_df_predicate_property(foreign).
%not_df_predicate_property(built_in).
%not_df_predicate_property(imported_from(_)).
%not_df_predicate_property(volatile).
:- meta_predicate analyzable_predicate(:).
analyzable_predicate(P) :-
    predicate_property(P, interpreted),
    \+ predicate_property(P, built_in),
    \+ predicate_property(P, multifile),
    \+ predicate_property(P, meta_predicate(_)),
    \+ predicate_property(P, dynamic).

% check_abstract_domain(MGoal-From) :-
% 	MGoal = M:Goal,
% 	Goal \= fail,
% 	functor(Goal, F, A),
% 	write('here we are'(MGoal-From)),nl,
% 	check_abstract_domains_goal(M, F, A, Goal, From).

% check_abstract_domains(Ref) :-
% 	check_code(Ref, check_abstract_domain).

list_abstract_domains(PI) :-
    current_abstract_domain(Domain),
    list_abstract_domains(Domain, PI).

list_abstract_domains(Domain, M:F/A) :-
    current_module(M),
    print_message(information, acheck(progress(M))),
    M \= user,
    module_property(M, class(user)),
    PI = M:F/A,
    findall(Trace, check_abstract_domain(Domain, PI, M, F, A, Trace), TracesL),
    append(TracesL, TracesU),
    sort(TracesU, Traces),
    report_problems(Traces, M, Domain),
    fail.
list_abstract_domains(_, _).

:- meta_predicate check_abstract_domain(*,:,*,*,*,*).

check_abstract_domain(Domain, PI, M, F, A, Trace) :-
    current_predicate(PI),
    F/A \= fail/0,
    %% print_message(information, acheck(progress(PI))),
    functor(Goal, F, A),
    MGoal = M:Goal,
    predicate_property(MGoal, exported),
    analyzable_predicate(M:Goal),
    predicate_from(MGoal, From),
    abstract_execute_entry(Goal, From, PI, Domain, [], _Calls,
			   [], Trace, _Result).

% check_abstract_domain_goal(Domain, PI, Goal, From) :-
% 	findall(Trace,
% 		abstract_execute_entry(Goal, From, PI, Domain,
% 		[], _Calls, [], Trace, _Result), TracesL),

report_problems(TraceCalls, M, Domain) :-
    group_pairs_by_key(TraceCalls, GTraceCalls),
    select_problematic_cases(GTraceCalls, M, ATraceCalls),
    maplist(report_problem(Domain), ATraceCalls).

report_problem(Domain, trace(PI, From, Result)-Calls) :-
    once(problematic_case(Result, Msg)),
    print_message(warning, acheck(abstract_domain(Domain, From, Calls,
						  [predicate, PI, Msg]))).

:- meta_predicate abstract_execute_entry(*,*,:,*,*,*,*,*,*).

abstract_execute_entry(true, From, PI, Domain, Calls0, Calls,
		       Trace0, Trace, Result) :- !,
    abstract_execute_goal(true, From, PI, Domain, Calls0, Calls,
			  Trace0, Trace, Result).
abstract_execute_entry(fail, From, PI, Domain, Calls0, Calls,
		       Trace0, Trace, Result) :- !,
    abstract_execute_goal(fail, From, PI, Domain, Calls0, Calls,
			  Trace0, Trace, Result).
abstract_execute_entry(Goal, From, PI, Domain, Calls0, Calls,
		       Trace0, [trace(PI, From, Result)-Calls|Trace], Result) :-
    abstract_execute_goal(Goal, From, PI, Domain, Calls0, Calls,
			  Trace0, Trace, Result).

problematic_case(product(fail, free), 'without doing anything').
problematic_case(product(fail, soft), 'only doing soft things').

select_problematic_cases([], _, []) :- !.
select_problematic_cases(GTraceCalls0, M, ATraceCalls) :-
    ( select(trace(PI, _, _)-_, GTraceCalls0, GTraceCalls1),
      PI \= M:_ ->
      remove_all(trace(PI), GTraceCalls1, GTraceCalls2),
      select_problematic_cases(GTraceCalls2, M, ATraceCalls)
    ; select(trace(PI, _, Result)-_, GTraceCalls0, GTraceCalls1),
      \+ problematic_case(Result, _) ->
      remove_all(trace(PI, _), GTraceCalls1, GTraceCalls2),
      select_problematic_cases(GTraceCalls2, M, ATraceCalls)
    ; GTraceCalls0 = ATraceCalls
    ).

remove_all(trace(PI), TraceCalls0, TraceCalls) :-
    select(trace(PI, _, _)-_, TraceCalls0, TraceCalls1),
    !,
    remove_all(trace(PI), TraceCalls1, TraceCalls).
remove_all(trace(PI, From), TraceCalls0, TraceCalls) :-
    select(trace(PI, From, _)-_, TraceCalls0, TraceCalls1),
    !,
    remove_all(trace(PI, From), TraceCalls1, TraceCalls).
remove_all(_, TraceCalls, TraceCalls).

% Abstract interpretation of the program in a given domain:
% Result = top  -> I don't know due to lost of precision (abstraction step)
% Result = bot  -> I don't know due to lack of information (not used)

% Limitations: Loops are not analyzed

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(domains(abstract_domain_fail)).
:- use_module(domains(abstract_domain_sideff)).
:- use_module(domains(abstract_domain_product)). % this always at the end

closed_list(Var) :- var(Var), !, fail.
closed_list([]).
closed_list([_|L]) :- closed_list(L).

partial_evaluate(A = A,   _).
partial_evaluate(A =.. B, _) :-
    ( nonvar(A) ->
      A =.. B
    ; nonvar(B), B = [F|L], atom(F), closed_list(L) ->
      A =.. B
    ).
partial_evaluate(functor(T, F, A), _) :-
    ( nonvar(T) ->
      functor(T, F, A)
    ; atom(F), integer(A), A >=0 ->
      functor(T, F, A)
    ).
partial_evaluate((A is B), _) :-
    ground(B) -> A is B.

:- meta_predicate abstract_execute_goal(*,*,:,*,*,*,*,*,*).

abstract_execute_goal(Goal, From, Module:_, Domain, Calls, Calls,
		      Trace, Trace, Result) :-
    trusted_result(Domain, Goal, Module, Result),
    catch(ignore(partial_evaluate(Goal, Module)), % Only to improve precision
	  error(What, _Where),
	  print_message(warning, acheck(partial_eval(From, Calls,
						     [Goal, error(What, _)])))
	 ),
    !.
abstract_execute_goal(Goal, _From, Module:_, Domain, Calls, Calls,
		      Trace, Trace, Result) :-
    \+ analyzable_predicate(Module:Goal),
    bot(Domain, Result),
    %% format(user_error, 'WARNING: Lost of precision because call to ~w~n',
    %%        [Module:Goal]),
	!.
abstract_execute_goal(_Goal, _From, PI, Domain, Calls, Calls,
		      Trace, Trace, Result) :-
    % over-aproximation: don't analyze loops (at least for now).
    \+ \+ member(stack(PI, __From), Calls),
    bot(Domain, Result),
    % format(user_error, 'WARNING: Lost of precision because loop ~w~n',
    %        [Module:Head]),
    !.
abstract_execute_goal(Goal, From, M0:F/A, Domain, Calls0, Calls, Trace0, Trace,
		      Result) :-
    ( predicate_property(M0:Goal, imported_from(MI)) ->
      M = MI
    ; M = M0
    ),
    PI = M:F/A,
    Calls1 = [stack(PI, From)|Calls0],
    ( clause(M:Goal, Body, ClauseRef) *->
      ignore(clause_info(ClauseRef, _File, TermPos, _NameOffset)),
      ( Body = true ->
	abstract_execute_goal(true, TermPos, PI, Domain, Calls1, Calls,
			      Trace0, Trace, Result)
      ;
	( TermPos = term_position(_, _, _, _, [_, BodyPos]) -> true
	; TermPos = BodyPos
	),
	abstract_execute_body(Body, ClauseRef, BodyPos, M, Domain,
			      Calls1, Calls, Trace0, Trace, Result)
      )
    ; abstract_execute_goal(fail, From, PI, Domain, Calls1, Calls,
			    Trace0, Trace, Result)
    ).

% TODO: Handle findall/3 and other meta predicates in a more general way -- EMM
% collapse_results_findall(fail, _, true).
% collapse_results_findall(sideff, Results, Result) :-
% 	( member(bot,  Results) -> Result = bot
% 	; member(top,  Results) -> Result = top
% 	; member(hard, Results) -> Result = hard
% 	; member(soft, Results) -> Result = soft
% 	; member(free, Results) -> Result = free
% 	; Result = top
% 	).
collapse_results_findall(product(fail, sideff), Results, Result) :-
    ( member(product(_, bot),  Results) -> Result = product(true, bot)
    ; member(product(_, top),  Results) -> Result = product(true, top)
    ; member(product(_, hard), Results) -> Result = product(true, hard)
    ; member(product(_, soft), Results) -> Result = product(true, soft)
    ; member(product(_, free), Results) -> Result = product(true, free)
    ; Result = product(true, top)
    ).

abstract_execute_body(Var, _, _, _, Domain, Calls, Calls, Trace, Trace, Result) :-
    var(Var),
    !,
    bot(Domain, Result).
abstract_execute_body(M:Goal, ClauseRef, term_position(_, _, _, _, [_, Pos]),
		      _M0, Domain, Calls0, Calls, Trace0, Trace, Result) :- !,
    abstract_execute_body(Goal, ClauseRef, Pos, M, Domain, Calls0, Calls,
			  Trace0, Trace, Result).
abstract_execute_body((A->B), ClauseRef, term_position(_, _, _, _, [PA, PB]),
		      M, Domain, Calls0, Calls, Trace0, Trace, Result) :- !,
    abstract_execute_body(A, ClauseRef, PA, M, Domain, Calls0, CallsA,
			  Trace0, Trace1, ResultA), % Cut here ???
    ( bot(Domain, Bot),
      eval(Domain, (ResultA->Bot), ResultA) ->
      Calls = CallsA, Result = ResultA, Trace = Trace1
    ; abstract_execute_body(B, ClauseRef, PB, M, Domain, Calls0, Calls,
			    Trace1, Trace, ResultB),
      eval(Domain, (ResultA->ResultB), Result)
    ).
abstract_execute_body(findall(_, A, _), ClauseRef, term_position(_, _, _, _, [_, PA, _]),
		      M, Domain, Calls, Calls, Trace0, Trace, Result) :- !,
    findall(ResultA-Trace1, abstract_execute_body(A, ClauseRef, PA, M, Domain, Calls, _,
						  [], Trace1, ResultA), ResultsTraces),
    pairs_keys_values(ResultsTraces, Results, Traces),
    append(Traces, Trace1),
    append(Trace1, Trace0, Trace),
    collapse_results_findall(Domain, Results, Result).
abstract_execute_body((A*->B), ClauseRef, term_position(_, _, _, _, [PA, PB]),
		      M, Domain, Calls0, Calls, Trace0, Trace, Result) :- !,
    abstract_execute_body(A, ClauseRef, PA, M, Domain, Calls0, CallsA,
			  Trace0, Trace1, ResultA), % Soft cut here ???
    ( bot(Domain, Bot),
      eval(Domain, (ResultA*->Bot), ResultA) ->
      Calls = CallsA, Result = ResultA, Trace = Trace1
    ; abstract_execute_body(B, ClauseRef, PB, M, Domain, Calls0, Calls,
			    Trace1, Trace, ResultB),
      eval(Domain, (ResultA->ResultB), Result)
    ).
abstract_execute_body((\+ A), ClauseRef, term_position(_, _, _, _, [PA]),
		      M, Domain, Calls0, Calls, Trace0, Trace, Result) :- !,
    abstract_execute_body(A, ClauseRef, PA, M, Domain, Calls0, Calls,
			  Trace0, Trace, ResultA),
    eval(Domain, (\+ ResultA), Result).
abstract_execute_body((A, B), ClauseRef, term_position(_, _, _, _, [PA, PB]),
		      M, Domain, Calls0, Calls, Trace0, Trace, Result) :- !,
    abstract_execute_body(A, ClauseRef, PA, M, Domain, Calls0, CallsA,
			  Trace0, Trace1, ResultA),
    ( bot(Domain, Bot),
      eval(Domain, (ResultA, Bot), ResultA) ->
      Calls = CallsA, Result = ResultA, Trace = Trace1
    ; abstract_execute_body(B, ClauseRef, PB, M, Domain, Calls0, Calls,
			    Trace1, Trace, ResultB),
      eval(Domain, (ResultA, ResultB), Result)
    ).
abstract_execute_body((A;B), ClauseRef, term_position(_, _, _, _, [PA, PB]),
		      M, Domain, Calls0, Calls, Trace0, Trace, Result) :- !,
    ( abstract_execute_body(A, ClauseRef, PA, M, Domain, Calls0, Calls,
			    Trace0, Trace, Result)
    ; abstract_execute_body(B, ClauseRef, PB, M, Domain, Calls0, Calls,
			    Trace0, Trace, Result)
    ).
abstract_execute_body(Goal, ClauseRef, TermPos, M, Domain, Calls0, Calls,
		      Trace0, Trace, Result) :-
    functor(Goal, F, A),
    PI = M:F/A,
    ( nonvar(TermPos) ->
      arg(1, TermPos, CharCount),
      From = clause_char_count(ClauseRef, CharCount)
    ;
      From = clause(ClauseRef)
    ),
    abstract_execute_entry(Goal, From, PI, Domain, Calls0, Calls,
			   Trace0, Trace, Result).
