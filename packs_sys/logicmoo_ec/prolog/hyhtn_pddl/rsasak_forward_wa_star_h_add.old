:-module(rsasak_forward_wa_star_h_add,[solve_files/2]).
use_ocl :-fail.
pddl_structs:- fail.
 
% [Required] Load the Logicmoo Library Utils
:- use_module(library(logicmoo_common)).
:- use_module(library(logicmoo_planner)).
:- ensure_loaded(library(multimodal_dcg)).
:- ensure_loaded(library(statistics)).
:- expects_dialect(sicstus).
:-use_module(library(timeout)).
:-use_module(library(lists)).

%:- set_prolog_flag(gc,true).
:- op(100,xfy,'=>').
%:- debug.

:- style_check(-singleton).


:-use_module(rsasak_pddl_parser).

:- dynamic(use_local_pddl/0).
use_local_pddl:-throw(uses_local_pddl).

:- if((false,(gethostname(c3po);gethostname(titan)))).

:- initialization(user:use_module(library(swi/pce_profile))).
:- initialization( profiler(_,walltime) ).

:- endif.

% :- use_module(library(clpfd)).
:- use_module(library(dif)).
:-export(user:my_pfc_add/1).
user:my_pfc_add(A):-if_defined(pfc_add(A),assert_if_new(A)).


% :- qcompile_libraries.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILENAME:  common.pl 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file contain common predicates that are used in planners

% :- dynamic(domain/3).
% :- dynamic(pairfrom/4).


:- if(\+current_predicate(init_locl_planner_interface0/4)).
% :- with_no_mpred_expansions(ensure_loaded(library(logicmoo_hyhtn))).
:- endif.
% :- set_prolog_flag(gc,true).

:- initialization( profiler(_,cputime) ).
:- initialization(user:use_module(library(swi/pce_profile))).



:- ensure_loaded(library(logicmoo/util_structs)).
% :- ensure_loaded(library(sexpr_reader)).
%:- ensure_loaded(library(se)).

:- decl_struct(domain(domain_name, requires, types, constants, predicates, functions, constraints, actions, dict(extraprops))).
:- decl_struct(problem(problem_name, domain_name, requires, objects, init, goal, constraints, metric, length, dict(extraprops))).

:- decl_struct(action5(parameters=unk,sorted(preconditions),sorted(positiv_effect),sorted(negativ_effect),dict(extraprops))).

:- decl_argtypes(action(parameters=unk,sorted(preconditions),sorted(positiv_effect),sorted(negativ_effect),
    assign_effect,list(parameter_types),string(domain_name),list(varnames),
     dict(extraprops))).
:- decl_struct(action(string(action_name),list(parameter_types),sorted(preconditions),sorted(positiv_effect),sorted(negativ_effect),sorted(assign_effect),
        callable(parameters),callable(constraints),dict(extraprops))).


:- system:use_module(library(timeout)).
:- use_module(library(lists)).
:- use_module(library(listing)).



%   pairfrom(?Set, ?Element1, ?Element2, ?Residue)
%   is true when Set is a list, Element1 occurs in list, Element2
%   occurs in list after Element1, and Residue is everything in Set
%   bar the two Elements.  The point of this thing is to select_20_faster
%   pairs of elements from a set without selecting the same pair
%   twice in different orders.
%   This can be used to select_20_faster two elements from a given Set or to
%   insert two elements into a given Residue, but if neither Set
%   nor Residue is proper you're in trouble.  You could diagonalise
%   by doing Set = [_,_|L], same_length(L, Residue), and then
%   calling pairfrom.  We could do that here, but for the two uses
%   that are intended it is not necessary.

pairfrom([Element1|Set], Element1, Element2, Residue) :-
	select_20_faster(Element2, Set, Residue).
pairfrom([Head|Tail], Element1, Element2, [Head|Rest]) :-
	pairfrom(Tail, Element1, Element2, Rest).



%   select_20_faster(?Element, ?Set, ?Residue)
%   is true when Set is a list, Element occurs in Set, and Residue is
%   everything in Set except Element (things stay in the same order).

select_20_faster(X, [X|R],     R        ).
select_20_faster(X, [A,X|R],   [A|R]    ).
select_20_faster(X, [A,B,X|R], [A,B|R]  ).
select_20_faster(X, [A,B,C|L], [A,B,C|R]) :-
	select_20_faster(X, L, R).

/*  The original code was
	select_20_faster(X, [X|R], R).
	select_20_faster(X, [H|T], [H|R]) :- select_20_faster(X, T, R).
    This has been unrolled to save 10-20% of the time.
    It would be nice if select_20_faster/3 were in library(basics), but we're
    stuck with it now.  Ah, hindsight.
*/

% solve_files(+DomainFile, +ProblemFile)
%
%   Reads files and set timelimit for planner
%
solve_files(DomainFile, ProblemFile):- 
 forall(must(must_filematch(DomainFile,DomainFile0)),
   forall(must(must_filematch(ProblemFile,ProblemFile0)),
     (time(show_call(solve_files_0(DomainFile0, ProblemFile0)))))),!,
     nop(time(show_call(solve_files_w_ocl(DomainFile0, ProblemFile0)))).

% time(show_call(solve_files_w_ocl(DomainFile0, ProblemFile0))),
		
% Reads files and set timelimit for planner
solve_files_0(DomainFile, ProblemFile):- \+ pddl_structs, !,
       update_changed_files,
       directory_file_path(_,File,ProblemFile),
       wdmsg(solve_files(DomainFile, ProblemFile)),
       solve_files(DomainFile, ProblemFile, File),!.



solve_files_0(DomainFile, ProblemFile):- use_ocl,
   must_det_l(( 
      format('~q.~n',[solve_files(DomainFile, ProblemFile)]))),
   parseDomain(DomainFile, DD),
    parseProblem(ProblemFile, PP),
     solve_files_ddpp(DD, PP).


solve_files_0(DomainFile, ProblemFile):- pddl_structs,
   must_det_l(( 
  format('~q.~n',[solve_files(DomainFile, ProblemFile)]),
      parseDomain(DomainFile, D),
      prop_get(domain_name,D,DName),
      save_type_named(domain,DName,D),
      parseProblem(ProblemFile, P),
      prop_get(problem_name,P,PName),
      save_type_named(problem,PName,P),
      compile_domain(D,Dc),
      compile_problem(P,Pc),
      reset_statistic)),
    !,
    record_time(try_solve(PName,Dc,Pc,S),SolverTime),
    flag(time_used,X,X + SolverTime),
    show_statistic(P, S),
    !.


solve_files(DomainFile, ProblemFile, File):- slow_on(File),!,wdmsg(slow_on(DomainFile, ProblemFile)).
solve_files(DomainFile, ProblemFile, File):-
		parseDomain(DomainFile, DD, _),
		parseProblem(ProblemFile, PP, _),
		term_to_ord_term(DD, D),
		term_to_ord_term(PP, P),
		reset_statistic,		
		!,
		time_out(solve(D, P, S), 500000, _Result), % time limit for a planner
		show_statistic(P, S),
		!.
		
solve_files_ddpp(DD, PP):-
   must_det_l(( 
    term_to_ord_term(DD, D),prop_get(domain_name,D,DName),save_type_named(domain,DName,D),
    term_to_ord_term(PP, P),prop_get(problem_name,P,PName),save_type_named(problem,PName,P),    
    reset_statistic)),
    !,
    record_time(try_solve(PName, D,P,S),SolverTime),
    flag(time_used,X,X + SolverTime),
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
		


%% mysubset(+Subset, +Set)
%
% It is similar to subset/2. Subset can include free variables that are 
% grounded with atoms of Set.
%
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
%
setInit([], []).
setInit([set(F, V)|Ls], S):-
	F =.. A,
	concat_atom_iio(A, '-', CA),
	bb_put(CA, V),
%	write(CA),write(' '), write(V),  nl,
	setInit(Ls, S), !.
setInit([A|Ls], [A|Ss]):-
	setInit(Ls, Ss).


% concat_atom_iio(+List, +Delimiter, -ConcatenateAtom)
%

concat_atom_iio([E1, E2], D, O):-
		atom_concat(E1, D, Temp),
		atom_concat(Temp, E2, O).
concat_atom_iio([H|T], D, O):-
    concat_atom_iio(T, D, PTs),
		atom_concat(H, D, Temp),
    atom_concat(Temp, PTs, O).



% copy_term_spec(+Term, -Term)
%
% Special version of copy_term. variable x represented as ?(x)
% All occurs of ?(x) are replaced with real prolog variables.
% Modified version of code published by Bartak: http://kti.mff.cuni.cz/~bartak/prolog/data_struct.html
%
copy_term_spec(A,B):-			cp(A,[],B,_).

cp( VAR,Vars,VAR,Vars):- var(VAR),!.
cp(A,Vars,A,Vars):-		atomic(A), A\= ?(_).
cp(?(V),Vars,NV,NVars):-	atomic(V), register_var_pddl(V,Vars,NV,NVars).
cp(V,Vars,NV,NVars):-		var(V),register_var_pddl(V,Vars,NV,NVars).

cp(Term,Vars,NTerm,NVars):-
		compound(Term),
		Term \= ?(_),
		Term=..[F|Args],    % decompose term
		cp_args(Args,Vars,NArgs,NVars),
		NTerm=..[F|NArgs].  % construct copy term
cp_args([H|T],Vars,[NH|NT],NVars):-	cp(H,Vars,NH,SVars),
cp_args(T,SVars,NT,NVars).
cp_args([],Vars,[],Vars).

% register_var_pddl(?, ?, ?)
%
% During copying one has to remeber copies of variables which can be used further during copying.
% Therefore the register of variable copies is maintained.
register_var_pddl(V,[X/H|T],N,[X/H|NT]):-
		V\==X,         % different variables
		register_var_pddl(V,T,N,NT).
register_var_pddl(V,[X/H|T],H,[X/H|T]):-
		V==X.          % same variables
register_var_pddl(V,[],N,[V/N]).



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


%% show_statistic(+Problem, +Solution).
%
show_statistic(P, S):-
		ground(S),
		get_problem_name(P, Name),
		bb_get(stat_nodes, N),
		bb_get(startTime, T0),
		statistics(runtime, [T1,_]),
		statistics(memory, [M, _]),
		T is T1-T0,
        (is_list(S)-> length(S, L) ; L = -1),
		format('~a ~3d ~d ~d ~d', [Name,T, N, M, L]),
		solution_to_lisp(S),
		nl, !.
show_statistic(_, _).

solution_to_lisp([]).
solution_to_lisp([H|T]):-
		H =.. [F|P],
		write(' ('),
		write(F),
		write_list_sas(P),
		write(')'),
		solution_to_lisp(T).
	
write_list_sas([]).
write_list_sas([H|T]):-
		write(' '), write(H),
		write_list_sas(T).


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
    domain(X, D, F),
    !,
    write(X=D-F).   
    
w(X):-
    attvar(X),
    get_attrs(X,Attrs),!,
    write(=(X,Attrs)).
    
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
%
state_record(S, PS, A, D, [S, PS, A, D]).


%solution(+StateRecord, +Visited, -ListOfActions)
%
solution(SR, V, L):-
		solution(SR, V, [], L).
solution(SR, _, L, L):-
		state_record(_, nil, nil, _, SR), !.
solution(SR, V, R, L):-
		state_record(_, PS, AD, _, SR),
		state_record(PS, _, _, _, Previous),
		member(Previous, V),
		solution(Previous, V, [AD|R], L).


%is_goal(State)
%h(State, Value) 
%repeating(+State, +AnotherState)
:- expects_dialect(sicstus).
:-use_module(library(ordsets)).
:-use_module(library(heaps)).


% search(+InitState, +GoalState, -Solution)
%
search(I, _, Solution):-
		a_star(I, Solution, _).
		
		
% a_star(+InitState, -Actions, -Cost).
%
a_star(S, A, C):-
		state_record(S, nil, nil, 0, SR),
		list_to_heap([0-SR], PQ),
		a_star(PQ, [], A, C).


%a_star(+Queue, +Visited, -Solution, -Cost)
a_star(PQ, _, 'NO SOLUTION', _):-
		empty_heap(PQ),!.
a_star(PQ, V, Solution, C):-
		get_from_heap(PQ, C, SR, _),
		state_record(S, _, _, _, SR),
		is_goal(S),
%		write('FOUND SOLUTION'),nl,
%		state_record(S, _, _, D, SR), write(C-D), write('   '),write(S),nl,
%		writel(V),nl,halt,
		solution(SR, V, Solution).

a_star(PQ, V, Solution, C):-
		get_from_heap(PQ, _K, SR, RPQ),
		ord_add_element(V, SR, NV),
		(bagof(K-NS, next_node(SR, PQ, NV, K, NS), NextNodes) ; NextNodes=[]),
%		state_record(S, _, _, D, SR), write(_K-D), write('   '),write(S),length(NextNodes, L), write(L),nl,
%		write(NextNodes),nl,
		add_list_to_heap(RPQ, NextNodes, NPQ),
	
		stat_node,
		a_star(NPQ, NV, Solution, C).

%next_node(+StateRecord, +Queue, +Visited, -EstimateDeep, -NewStateRecord)
next_node(SR, Q, V, E, NewSR):-
		state_record(S, _, _, D, SR),
		step(S, A, NewS),
		state_record(NewS, _, _, _, Temp),
		\+ my_ord_member(NewS, V),
		heap_to_list(Q, PQL),
		\+ member(Temp, PQL),
		h(S, H),
		E is 5*H+D,
		ND is D+1,
		state_record(NewS, S, A, ND, NewSR).

%add_list_to_heap(+OldHeap, List, NewHeap)
%
add_list_to_heap(OH, [], OH).
add_list_to_heap(OH, [K-D|T], NH):-
		add_to_heap(OH, K, D, H),
		add_list_to_heap(H, T, NH).

my_ord_member(S, [SR|_]):-
		state_record(S2, _, _, _,SR),
		repeating(S, S2),
		!.
my_ord_member(S, [_|T]):-
		my_ord_member(S, T).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file must implements following predicates:
%
%% step(Mt,+State, -ActionDef, -NewState)
%   Return descendant of State and ActionDefinition that was used.
%
% is_goal(State) - is true when State is a goal state.  
%
% repeating(Goal1, Goal2):-  Goal1 is the same as Goal2.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :- expects_dialect(sicstus).
:-use_module(library(ordsets)).

make_init_state(I):-
		get_init(I),
		get_goal(G),
		bb_put(fictiveGoal, G).


make_solution(S, S).
		
%% step(Mt,+State, -ActionDef, -NewState)
%
%   Return descendant of State and ActionDefinition that was used.
%
step(State, ActionDef, NewState):-
		get_action(A, ActionDef),
		get_precondition(A, P),    mysubset(P, State),	% choose suitable action
		get_negativ_effect(A, NE), ord_subtract(State, NE, State2),	
		get_positiv_effect(A, PE), ord_union(State2, PE, NewState).


is_goal(S):-
		get_goal(G),
		ord_subset(G, S).

repeating(S1, S2):-
		S1 =  S2.
% :-use_module(library(sets)).
		
% Estimated distance to achive Goal.

%h(+State, -EstimatedValue)
%
h(S, E):-h_add(S, E).
% h(S, E):-h_addb(S, E).
% h(S, E):-h_diff(S, E).
% h(S, E):-h_0(S, E).

h_0(_, 0).

h_diff(S, E):-
		bb_get(fictiveGoal, G),
		ord_subtract(G, S, I),
		length(I, E).
	
h_add(S, E):-
		bb_get(fictiveGoal, G),
		relax(S, G, E).
%    write(G-S-E),nl.

relax(_, [], 0):-!.
relax(S, G, E):-
    subtract(G, S, Delta),
    setof(P, relax_step(S, P), RS),
    ord_union([S|RS], NS),
    relax(NS, Delta, NE),
    length(Delta, LD),
    E is LD+NE.

relax_step(State, PE):-
		get_action(A),	get_precondition(A, P),
		mysubset(P, State),
		get_positiv_effect(A, PE).



%init_heuristics(+InitState).
init_heuristics(_):-!.
init_heuristics_addb(InitState):-
    relax_addb(InitState, InitState, 0, Ps),
    bb_put(predicatesPrices, Ps).

relax_addb(_, [], _D, []):-!.
relax_addb(S, Delta, D, Ps):-
    mark_by(Delta, D, Marked),    
		setof(P, relax_step(S, P), PE),
		ord_union([S|PE], NS),
		ord_subtract(NS, S, NewDelta),
    ND is D+1,
    relax_addb(NS, NewDelta, ND, NewPs),
    ord_union(NewPs, Marked, Ps).

 mark_by([], _, []).
 mark_by([H|T], D, [H-D|NT]):-
    mark_by(T, D, NT).




%
%   run planner
%   takes two params from command line arguments
%   first is a domain file 
%   second problem file
	

command_line_sas:-
    prolog_flag(argv, [D,P]),!,
    solve_files(D, P),
    halt.

command_line_sas:- test_blocks, test_all.







/*

load_file(F):- must(read_file(F, L, Filename)),load_file_rest(Filename,L).
load_file_rest(_,[]):-!.
load_file_rest(F,L):- first_n_elements(L,10,ES),
   (
   (append(_,['define','(','domain',Named|_],ES),must_det_l((domainBNF(O, L, R1),prop_set(filename,O,F)))) ->  save_type_named(domain,Named,O);
   (append(_,['(','problem',Named|_],ES),must_det_l((problem(O, L, R1),prop_set(filename,O,F)))) ->  save_type_named(problem,Named,O);
    must((ensure_struct(sexpr_file,O),prop_set(filename,O,F),sterm(SO, L, R1),prop_set(sterm_value,O,SO)))),
   load_file_rest(F,R1).

load_domain(string(DP)):-!,load_file(string(DP)).
*/

load_domain(DP):- update_changed_files, \+ atom(DP),forall((filematch(DP,FOUND),exists_file(FOUND)),load_domain(FOUND)),!.
load_domain(DP):- \+ exists_file(DP),!, forall(must_filematch(DP,MATCH),((exists_file(MATCH),load_domain(MATCH)))).
load_domain(DP):-
   format('~q.~n',[load_domain(DP)]),
  directory_file_path(D,_,DP),directory_files(D,RList),   
   forall(member(T,RList),ignore((directory_file_path(D,T,TP),exists_file(TP),load_file(TP)))).






% :- solve_files(pddl('benchmarks/mystery/domain.pddl'),pddl('benchmarks/mystery/prob01.pddl')).
:-thread_local(t_l:loading_files/0).
:-thread_local(t_l:hyhtn_solve/1).
% t_l:other_planner(hyhtn_solve).



:- flag(time_used,_,0).
:- flag(time_used_other,_,0).

probfreecell:- solve_filespddl('benchmarks/freecell/domain.pddl', '../test/pddl_tests/benchmarks/freecell/probfreecell-9-5.pddl').
% :- debug,(must(test_blocks)).

:- fixup_exports.

end_of_file.

/*

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
*/