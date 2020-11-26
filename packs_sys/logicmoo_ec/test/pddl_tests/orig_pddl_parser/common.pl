:-use_module(library(timeout)).
:-use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file contain common predicates that are used in planners

% takes two params from input
% first is a domain file and second problem file
% and run planner for it.
command_line:-
		prolog_flag(argv, [D,P]),
		solve_files(D, P),
		halt.
		
% Reads files and set timelimit for planner
solve_files(DomainFile, ProblemFile):-
		parseDomain(DomainFile, DD, _),
		parseProblem(ProblemFile, PP, _),
		term_to_ord_term(DD, D),
		term_to_ord_term(PP, P),
		reset_statistic,
		!,
		time_out(solve(D, P, S), 500000, _Result), % time limit for a planner
		show_statistic(P, S),
		!.


		
%solve(+Domain, +Problem, -Solution).
% Set domain and problem on blackboard
solve(D, P, Solution):-
		get_init(P, I),		bb_put(initState, I),
		get_goal(P, G),		bb_put(goalState, G),
		get_metric(P, M),		bb_put(metric, M),
		get_actions(D, A),	bb_put(actions, A),
		get_objects(P, O),	bb_put(objects, O),
		make_init_state(IS),
		search(IS, G, Solution).


%term_to_ord_term(+Term, -OrdTerm)
% Go throught the term and look for sets, return the same term
% with all sets become ordered.
term_to_ord_term([], []).
term_to_ord_term(A, A):-atomic(A), !.
term_to_ord_term([H|T], R):-
                term_to_ord_term(H, OH),
                term_to_ord_term(T, OT),
                 ord_add_element(OT, OH, R), !.
%               write(OH), write(OT), write('   '), write(R), nl.
term_to_ord_term(T, OT):-
                T =.. [F,P], !,
                term_to_ord_term(P, OP),
                OT =..[F,OP].
term_to_ord_term(T, OT):-
                T =.. [F,P|Ps],
                NT=.. [F|Ps],
                term_to_ord_term(P, OP),
                term_to_ord_term(NT, ONT),
                ONT =.. [_|OPs],
                OT =.. [F,OP|OPs], !.



% mysubset(+Subset, +Set)
% It is similar to subset/2. Subset can include free variables that are 
% grounded with atoms of Set.
mysubset([], _).
mysubset([X|R], S):- member(X, S), mysubset(R, S).



% Colenction shortcuts functions.
% get(+Structure, -Parameter)
get_actions(     	domain(_, _, _, _, _, _, _, A), A).		
get_problem_name(	problem(N, _, _, _, _, _, _, _, _), N).
get_init(		problem(_, _, _, _, I, _, _, _, _), I).
get_goal(		problem(_, _, _, _, _, G, _, _, _), G).
get_metric(		problem(_, _, _, _, _, _, _, M, _), M).
get_objects(		problem(_, _, _, O, _, _, _, _, _), O).
get_precondition(	action(_, _, P, _, _, _), P).
get_positiv_effect(	action(_, _, _, PE, _, _), PE).
get_negativ_effect(	action(_, _, _, _, NE, _), NE).
get_assign_effect(	action(_, _, _, _, _, AE), AE).
get_parameters(		action(_, P, _, _, _, _), P).
get_action_def(		action(Name, Params, _, _, _, _), F):-
		untype(Params, UP),
		F =.. [Name|UP].


% get_action(-Action, -ActionDef)
get_action(A):-
		get_action(A, _). 
get_action(A, ActionDef):-
		bb_get(actions, As),
		member(Afree, As),
		copy_term_spec(Afree, A),
%		A =.. [_, Name, Params|_],
		get_action_def(A, ActionDef).


get_goal(G):-bb_get(goalState, G).
get_init(I):-bb_get(initState, I).

%untype(LitOfParams, UntyperList).
untype([], []).
untype([H|T], [U|Us]):- compound(H), H =.. [_T, [U]], !, untype(T, Us).
untype([H|T], [H|Us]):- untype(T, Us).

%setInit(+Init, -State)
setInit([], []).
setInit([set(F, V)|Ls], S):-
	F =.. A,
	concat_atom(A, '-', CA),
	bb_put(CA, V),
%	write(CA),write(' '), write(V),  nl,
	setInit(Ls, S), !.
setInit([A|Ls], [A|Ss]):-
	setInit(Ls, Ss).

% concat_atom(+List, +Delimiter, -ConcatenateAtom)
concat_atom([E1, E2], D, O):-
		atom_concat(E1, D, Temp),
		atom_concat(Temp, E2, O).
concat_atom([H|T], D, O):-
		concat_atom(T, D, Ts),
		atom_concat(H, D, Temp),
		atom_concat(Temp, Ts, O).


% Special version of copy_term. variable x represented as ?(x)
% All occurs of ?(x) are replaced with real prolog variables.
% Modified version of code published by Bartak: http://kti.mff.cuni.cz/~bartak/prolog/data_struct.html
copy_term_spec(A,B):-			cp(A,[],B,_).

cp(A,Vars,A,Vars):-		atomic(A), A\= ?(_).
cp(?(V),Vars,NV,NVars):-	atomic(V), register_var(V,Vars,NV,NVars).
cp(V,Vars,NV,NVars):-		var(V),register_var(V,Vars,NV,NVars).

cp(Term,Vars,NTerm,NVars):-
		compound(Term),
		Term \= ?(_),
		Term=..[F|Args],    % decompose term
		cp_args(Args,Vars,NArgs,NVars),
		NTerm=..[F|NArgs].  % construct copy term
cp_args([H|T],Vars,[NH|NT],NVars):-	cp(H,Vars,NH,SVars),
cp_args(T,SVars,NT,NVars).
cp_args([],Vars,[],Vars).

% During copying one has to remeber copies of variables which can be used further during copying.
% Therefore the register of variable copies is maintained.
register_var(V,[X/H|T],N,[X/H|NT]):-
		V\==X,         % different variables
		register_var(V,T,N,NT).
register_var(V,[X/H|T],H,[X/H|T]):-
		V==X.          % same variables
register_var(V,[],N,[V/N]).



%minOfList(+List, -MaxiamlItem)
%Find minimum value of the list
minOfList([X|Xs], Min):-
	minOfList(Xs, X, Min).
minOfList([], Min, Min).
minOfList([X|Xs], Min0, Min):-
	( X @< Min0 -> Min1 = X ; Min1 = Min0 ),
	minOfList(Xs, Min1, Min).



reset_statistic:-
		bb_put(stat_nodes, 0),
		statistics(runtime, [T,_]),
		bb_put(startTime, T).

show_statistic:-
		bb_get(stat_nodes, N),
		bb_get(startTime, T0),
		statistics(runtime, [T1,_]),
		statistics(memory, [M, _]),
		T is T1-T0,
		format('~3d sec      ~d nodes        ~d bytes~n', [T, N, M]).

%show_statistic(+Problem, +Solution).
show_statistic(P, S):-
		ground(S),
		get_problem_name(P, Name),
		bb_get(stat_nodes, N),
		bb_get(startTime, T0),
		statistics(runtime, [T1,_]),
		statistics(memory, [M, _]),
		T is T1-T0,
		length(S, L),
		format('~a ~3d ~d ~d ~d', [Name,T, N, M, L]),
		solution_to_lisp(S),
		nl, !.
show_statistic(_, _).

solution_to_lisp([]).
solution_to_lisp([H|T]):-
		H =.. [F|P],
		write(' ('),
		write(F),
		write_list(P),
		write(')'),
		solution_to_lisp(T).
	
write_list([]).
write_list([H|T]):-
		write(' '), write(H),
		write_list(T).


stat_node:-
		bb_get(stat_nodes, N),
		NN is N+1,
		bb_update(stat_nodes, _, NN).



space(0):-!.
space(I):-
	write('  '),
	NI is I-1,
	space(NI).
	
writel([]):-nl.
writel([H|T]):-
		write(H),nl,
		writel(T).

w(X):-
    var(X),
    domain(X, D, F),!,
    write(X=D-F).
w(X):-
    var(X),!,
    write(X).

w(X):-
    atomic(X),!,
    write(X).
w([H|T]):-
    write('['), !,
    w_list([H|T]),
    write(']').
w(X):-
    compound(X),!,
    X=..[F|L],
    write(F),write('('),
    w_params(L),
    write(')').
w_params([H]):-
    w(H).
w_params([H,H2|T]):-
    w(H),write(','),
    w_params([H2|T]).
w_list([H]):-
    w(H), !.
w_list([H|T]):-
    w(H),
    write(','),
    w_list(T).

%state_record(State, PreviousState, Action, Deep, StateRecord)
state_record(S, PS, A, D, [S, PS, A, D]).

%solution(+StateRecord, +Visited, -ListOfActions)
solution(SR, V, L):-
		solution(SR, V, [], L).
solution(SR, _, L, L):-
		state_record(_, nil, nil, _, SR), !.
solution(SR, V, R, L):-
		state_record(_, PS, AD, _, SR),
		state_record(PS, _, _, _, Previous),
		member(Previous, V),
		solution(Previous, V, [AD|R], L).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handling mutexes

 make_mutex(M):-
		bagof(R1, forbiden_pair(R1), MA),
		bagof(R2, forbiden_pair(MA, R2), MB),
%		writel(MA),nl,
%		writel(MB),nl,
		union(MA, MB, M0),
%		list_to_set(M0_, M0),
%		write('Cistim:'),nl,	
		clear_mutex1(M0, M1),
		clear_mutex2(M1, M2),
		clear_duplicates(M2, M).
		%write('Ocistene:'),nl,writel(M),nl, length(M, L), write('Pocet: '), write(L),nl.

clear_duplicates([], []).
clear_duplicates([H|T], R):-
    member(M, T),
    identical_but_for_variables(H, M),
    !,
    clear_duplicates(T, R).
clear_duplicates([H|T], [H|R]):-
    clear_duplicates(T, R).

forbiden_pair(R):-
		get_action(A),
		get_positiv_effect(A, PE),
		get_negativ_effect(A, NE),
		member(P, PE),
		member(Q, NE),
		copy_term_spec(P-Q, R).
forbiden_pair(MA, NR):-
		member(P-Q, MA),
		get_action(A),
		get_precondition(A, Precond),
		get_positiv_effect(A, PE),
		member(R, Precond),
		member(P, PE),
		copy_term_spec(R-Q, NR).

clear_mutex1([], []):-!.
clear_mutex1([PP-QQ|T], M):-
		(P-Q = PP-QQ ; P-Q = QQ-PP),
		get_init(I),
		select(P, I, R),
		member(Q, R),
%		write('Rule1: '), write(PP-QQ),nl,
		clear_mutex1(T, M), !.
clear_mutex1([P-Q|R], [P-Q|M]):-
		clear_mutex1(R, M).

clear_mutex2(M0, M):-
		(select(P-Q, M0, R) ; select(Q-P, M0, R)),
		get_action(A, _Def), get_precondition(A, Precond), get_positiv_effect(A, PE), get_negativ_effect(A, NE),
		select(P, PE, RPE),
		\+ member(Q, NE),
		(
			member(Q, RPE)%, write('prva cast')
			;
			all_not_in(Precond, P, Q, M0)%, write('druha cast')
		),
%		write('Rule2: '), write(P-Q-_Def),nl,

		clear_mutex2(R, M), !.
clear_mutex2(M0, M0).

all_not_in([], _, _, _).
all_not_in([P|T], P, Q, M):-
	all_not_in(T, P, Q, M).
all_not_in([R|T], P, Q, M):-
		\+ (member(R-Q, M) ; member(Q-R, M)),
		%write(precon-R),nl,
		all_not_in(T, P, Q, M).



%check_mutex(+State).
check_mutex(S):-
		bb_get(mutex, M),
		pairfrom(S, P, Q, _),
		(member(P-Q, M) ; member(Q-P, M)),
%		write('Mutex pair.'), write(P-Q), nl,
		!, fail.
check_mutex(_).


identical_but_for_variables(X, Y) :-
		\+ \+ (
			copy_term(X, Z),
			numbervars(Z, 0, N),
			numbervars(Y, 0, N),
			Z = Y
		).