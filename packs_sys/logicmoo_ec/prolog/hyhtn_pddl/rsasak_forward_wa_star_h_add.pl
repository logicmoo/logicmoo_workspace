:-module(rsasak_forward_wa_star_h_add,[solve_files/2]).
use_ocl :-fail.
pddl_structs.

% [Required] Load the Logicmoo Library Utils
:- use_module(library(logicmoo_common)).
:- use_module(library(logicmoo_planner)).
% :- ensure_loaded(library(logicmoo/multimodal_dcg)).
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

% :- initialization(user:use_module(library(swi/pce_profile))).
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

%:- dynamic(is_saved_type/3).



:- if(\+current_predicate(init_locl_planner_interface0/4)).
% :- with_no_mpred_expansions(ensure_loaded(library(logicmoo_hyhtn))).
:- endif.
% :- set_prolog_flag(gc,true).

%:- initialization( profiler(_,cputime) ).
%:- initialization(user:use_module(library(swi/pce_profile))).



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


% solve(PN,+Domain, +Problem, -Solution).
%
%   Set domain and problem on blackboard
%
:-thread_local(t_l:other_planner/1).
solve(PN,D,P,S):- t_l:other_planner(C),!,on_x_rtrace(call(C,PN,D,P,S)),!.

solve(_,D, P, Solution):-
  must_det_l((
    bb_put(currentProblem, P),
    bb_put(currentDomain, D),
    prop_get(init,P, UCI),
    prop_get(goal,P, UCG),
    copy_term_for_assert((UCI,UCG),(I,G)),
    prop_get(domain_name,D,Mt),    
    must(prop_get(domain_name,P,Mt)),
    % abolish(actn,2),

    prop_get(actions,D, A),
    must_maplist(save_varnames_in_action,A,CA),
    bb_put(actions, CA), must_maplist(save_action(Mt), CA),

    bb_put(goalState, G),        
    bb_put(fictiveGoal, G))),!,    
    search(da(Mt,A,A5),I, G, Solution).

solve(_PN,D,P,Solution):-
  must_det_l((
    bb_put(currentProblem, P), 
    bb_put(currentDomain, D),    
    prop_get(init,P, I),
    prop_get(goal,P, G),
    copy_term_for_assert((I,G),(Ic,Gc)),
    prop_get(domain_name,D,Mt),
    must(prop_get(domain_name,P,Mt)),
    prop_get(actions,D, A),
    prop_get(actions5,D, A5),
    bb_put(goalState, G), 
    bb_put(fictiveGoal, G))),    
    search(da(Mt,A,A5),Ic, Gc, Solution).
    



%% mysubset(+Subset, +Set)
%
%   It is similar to subset/2. Subset can include free variables that are 
%   grounded with atoms of Set.
%
mysubset([], _).
mysubset([X|R], S):- member(X, S), mysubset(R, S).



% Collection of shortcuts

/*
,
  nop( prop_get(constraints(A,C))),
  nop((  prop_get(varnames(A,Vars)))),
  nop((checkConstraints(C),record_var_names(Vars))).


get_action_copy(Mt,A):- actn(Mt,A).

get_constrained_action(Mt,action(S, PTs, Precon, Pos, Neg, Assign, UT )):-
  get_action_copy(Mt,action(S, PTs, Precon, Pos, Neg, Assign, UT , C, Vars)),
  nop((call(C),record_var_names(Vars))).


get_action_bb(_Mt,A):- bb_get(actions, As),copy_term(As, Ass),!, member(A, Ass).

get_constrained_action_bb(Mt,action(S, PTs, Precon, Pos, Neg, Assign, UT )):-
  get_action_bb(Mt,action(S, PTs, Precon, Pos, Neg, Assign, UT , C, Vars)),
  nop((call(C),record_var_names(Vars))).

*/
% DMILES

get_constrained_action(da(Mt,AS,AS5),action(S, PTs, Precon, Pos, Neg, Assign, UT )):- !, actn(Mt,action(S, PTs, Precon, Pos, Neg, Assign, UT, _,_)).
get_constrained_action(da(Mt,AS,AS5),action(S, PTs, Precon, Pos, Neg, Assign, UT )):-
  copy_term(AS,ASC),
  member(action(S, PTs, Precon, Pos, Neg, Assign, UT , C, Vars),ASC),
  nop((call(C),record_var_names(Vars))).


get_constrained_action_bb(_Mt,action(S, PTs, Precon, Pos, Neg, Assign, UT )):- bb_get(actions, As),As \= [],!, copy_term(As, Ass),!, member(action(S, PTs, Precon, Pos, Neg, Assign, UT, _,_), Ass).
get_constrained_action_bb(da(Mt,AS,AS5),action(S, PTs, Precon, Pos, Neg, Assign, UT )):-
  copy_term(AS,ASC),
  member(action(S, PTs, Precon, Pos, Neg, Assign, UT , C, Vars),ASC),
  nop((call(C),record_var_names(Vars))).




all_dif(_):-!.
all_dif([A,B]):-!,dif(A,B),!.
all_dif([A,B,C|_]):-!,dif(A,B),dif(A,C),dif(B,C),!.
all_dif(_).

tk(_,_):-!.
tk(top,_):-!.
tk(K,X):- var(X),!,when(nonvar(X),tk(K,X)).
tk(K,X):- clause(error:has_type(K, X),B),!,show_call(B).
% tk(K,X):- dmsg(tk(K,X)).
tk(_,_).


% setInit(+Init, -State)
%
setInit([], []).
setInit([set(F, V)|Ls], S):-
    F =.. A,
    concat_atom_iio(A, '-', CA),
    bb_put(CA, V),
%    write(CA),write(' '), write(V),  nl,
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

/*
replc_vars(Term,NVars):-
 ignore(var(NVars) ->
   (NVarsM=mutable([]),replc_vars2(Term,NVarsM),arg(1,NVarsM,NVars)); replc_vars2(Term,NVars)).

replc_vars2(Term,NVars):-      
     compound(Term),
     arg(N,Term,V),
     once((var(V)->true;
     once((svar_fixvarname(V,Name)->  
        must_det_l((            
            arg(1,NVars,Vars),
            register_var_pddl(Name=NV,Vars,MVars),
            nb_setarg(N,Term,NV),
            nb_setarg(1,NVars,MVars)));
        replc_vars2(V,NVars))))),
     fail.
*/


replc_structure_vars1(B,AO):-replc_structure_vars2(B,A),if_changed(B,A,AO).

replc_structure_vars2(B,AO):-replc_structure_vars3(B,AO,_).

replc_structure_vars3(B,AO,Vars):- 
       (prop_get(varnames,B,Before)->true;Before=[]),
       replc_structure_vars4(B,Before,AO,Vars).
replc_structure_vars4(B,Before,AO,Vars):-
       cp(B,Before,A,After),!,       
       (Before\==After -> (numbervars(A:After),prop_set(varnames,A,After),unnumbervars(A:After,AO:Vars)) ;
         (A=AO)).

replc_structure_vars(A,AA):- copy_term(A,AC),
  must_det_l((     
      replc_structure_vars1(A,AA),
      (AA\=@=AC-> wdmsg(changed_replc_structure_var1s(A,AA));true))).

% ?- replc_structure_vars(v(a(?b,?c),mutable([]))). 


% copy_term_spec(+Term, -Term)
%
%   Special version of copy_term. variable x represented as ?(x)
%   All occurs of ?(x) are replaced with real prolog variables.
%   Modified version of code published by Bartak: http://kti.mff.cuni.cz/~bartak/prolog/data_struct.html
%
copy_term_spec(A,B):-			cp(A,[],B,_).

cp( VAR,Vars,VAR,Vars):- var(VAR),!.
cp( VAR,Vars,NV,NVars):- /*logicmoo_i_sexp_reader:*/svar(VAR,_),!,must((svar_fixvarname(VAR,Name),atom(Name))),!, must(register_var_pddl(Name=NV,Vars,NVars)).
cp([],Vars,[],Vars).
cp( Term,Vars,Term,Vars):- \+compound(Term),!.
cp([H|T],Vars,[NH|NT],NVars):- !, cp(H,Vars,NH,SVars), cp(T,SVars,NT,NVars).
cp( Term,Vars,NTerm,NVars):-    
    Term=..[F|Args],    % decompose term
    (/*logicmoo_i_sexp_reader:*/svar(F,_)-> cp( [F|Args],Vars,NTerm,NVars);
    % construct copy term
    (cp(Args,Vars,NArgs,NVars), NTerm=..[F|NArgs])).  







varnames_for_assert(A,B,After):-
     b_getval('$variable_names',Before),
     must(cp(A,Before,B,After)).

copy_term_for_assert(A,B):-
    must(cp(A,[],B,After)),
    b_setval('$variable_names',After).
    
/*

% register_var_pddl(?, ?, ?)
%
%   During copying one has to remeber copies of variables which can be used further during copying.
%   Therefore the register of variable copies is maintained.
%
register_var_pddl(N=V,IN,OUT):-register_var_pddl(N,IN,V,OUT).

register_var_pddl(N,T,V,OUT):- must(nonvar(N)),
   ((name_to_var(N,T,VOther)-> must((OUT=T,samify(V,VOther)));
     (once(nb_getval('$variable_names',Before);Before=[]),
      (name_to_var(N,Before,VOther)  -> must((samify(V,VOther),OUT= [N=V|T]));
         (var_to_name(V,T,_OtherName)                  -> OUT= [N=V|T];
           (var_to_name(V,Before,_OtherName)              -> OUT= [N=V|T];fail)))))).


register_var_pddl(N,T,V,OUT):- var(N),
   (var_to_name(V,T,N)                -> OUT=T;
     (once(nb_getval('$variable_names',Before);Before=[]),
          (var_to_name(V,Before,N)   -> OUT= [N=V|T];
               OUT= [N=V|T]))),!.


register_var_pddl(N,T,V,[N=V|T]).

% different variables (now merged)
samify(V,V0):-must(V=@=V0),V=V0. 

var_to_name(V,[N=V0|T],N):-
    V==V0 -> true ;          % same variables
    var_to_name(V,T,N).

name_to_var(N,T,V):- var(N),!,var_to_name(N,T,V).
name_to_var(N,[N0=V0|T],V):- 
   N0==N -> samify(V,V0) ; name_to_var(N,T,V).

*/ 


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
    prop_get(problem_name, P, Name),
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


% state_record(State, PreviousState, Action, Deep, StateRecord)
%
state_record(S, PS, A, D, [S, PS, A, D]).


% solution(+StateRecord, +Visited, -ListOfActions)
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


% is_goal(State)
% h_s_v(State, Value) 
% repeating(+State, +AnotherState)
:- expects_dialect(sicstus).
:-use_module(library(ordsets)).
:-use_module(library(heaps)).


% search(+InitState, +GoalState, -Solution)
%
search(Mt,I, _, Solution):-
   term_to_ord_term(I,II),!,
    a_star(Mt, II, Solution, _).
% a_star(Mt,+InitState, -Actions, -Cost).
%
a_star(Mt,S, A, C):-
    state_record(S, nil, nil, 0, SR),
    list_to_heap([0-SR], PQ),
    a_star(Mt,PQ, [], A, C).


% a_star(Mt,+Queue, +Visited, -Solution, -Cost)
%
a_star(_Mt,PQ, _, 'NO SOLUTION', _):-
  %  write('NO SOLUTION'),nl,
    empty_heap(PQ),
    !.
a_star(Mt,PQ, V, Solution, C):-
    get_from_heap(PQ, C, SR, _),
    state_record(S, _, _, _, SR),
    is_goal(Mt,S),
%    write('FOUND SOLUTION'),nl,
%    state_record(S, _, _, D, SR), write(C-D), write('   '),write(S),nl,
%    writel(V),nl,halt,
    solution(SR, V, Solution).

a_star(Mt,PQ, V, Solution, C):-
    get_from_heap(PQ, _K, SR, RPQ),
    ord_add_element(V, SR, NV),
    (    bagof(K-NS, next_node(Mt,SR, PQ, NV, K, NS), NextNodes) 
         ;
         NextNodes=[]
    ),
%    state_record(S, _, _, D, SR), write(_K-D), write('   '),write(S),length(NextNodes, L), write(L),nl,
%    write(NextNodes),nl,
    add_list_to_heap(RPQ, NextNodes, NPQ),
    
    stat_node,
    a_star(Mt,NPQ, NV, Solution, C).



% next_node(Mt,+StateRecord, +Queue, +Visited, -EstimateDeep, -NewStateRecord)
%
next_node(Mt,SR, Q, V, E, NewSR):-
    state_record(S, _, _, D, SR),
    step(Mt,S, A, NewS),
    state_record(NewS, _, _, _, Temp),
    \+ my_ord_member(NewS, V),
    heap_to_list(Q, PQL),
    \+ member(Temp, PQL),
    h_s_v(Mt,S, H),
    E is H+D,
    ND is D+1,
    state_record(NewS, S, A, ND, NewSR).


% add_list_to_heap(+OldHeap, List, NewHeap)
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




make_solution(S, S).
    
%
% FILENAME:  wa-star.pl 
% Interface:
%
% step(Mt,+State, -NewState)
    
%% step(Mt,+State, -ActionDef, -NewState)
%
%   Return descendant of State and ActionDefinition that was used.
%
step(Mt,State, ActionDef, NewState):-  
  %  get_a ction(A, ActionDef),
  %  get_precondition(A, P),    
  must(get_constrained_action_bb(Mt,action(_S, _L, P, PE, NE, _Assign, ActionDef))),
% DMILES 
%  get_constrained_action_bb(Mt,action5(ActionDef, P, PE, NE,_)),
  mysubset(P, State),  % choose suitable action
 %   get_negativ_effect(A, NE),
  ord_subtract(State, NE, State2),
 %   get_positiv_effect(A, PE), 
  ord_union(State2, PE, NewState).




is_goal(_Mt,S):-
    bb_get(goalState, G),
    ord_subset(G, S).

repeating(S1, S2):-
    S1 =@=  S2.

% FILENAME:  forward-wa-star-h_add.pl 
%:-[readFile, parseProblem, parseDomain, common].
%:-['wa-star', forward, h_add].





% FILENAME:  forward-wa-star-h_diff.pl 
%:-[readFile, parseProblem, parseDomain, common].
%:-['wa-star', forward, 'h_diff'].


% FILENAME:  h_add.pl 

% :- expects_dialect(sicstus).

% :-use_module(library(sets)).

%   Estimated distance to achive Goal.
%
% :- expects_dialect(sicstus).

% h_s_v(+State, -EstimatedValue)
%
h_s_v(Mt,S, H):- h_add(Mt,S, H).
% h_s_v(S, H):- h_diff(S, H).

% FILENAME:  h_diff.pl 
%

h_diff(S, E):-
    bb_get(fictiveGoal, G),
    ord_subtract(G, S, I),
    length(I, E).


%
%   Estimated distance to achieve Goal.
%
h_add(Mt,S, E):-
    bb_get(fictiveGoal, G),
    relax(Mt,S, G, E).
%    write(G-S-E),nl.

relax(_Mt,_, [], 0):-!.
relax(Mt,S, G, E):-
    subtract(G, S, Delta),
    setof(P, relax_step(Mt,S, P), RS),
    ord_union([S|RS], NS),
    relax(Mt,NS, Delta, NE),
    length(Delta, LD),
    E is LD+NE.

relax_step(Mt,State, PE):-    
    % get_a ction(A),
    must(get_constrained_action(Mt,action(_S, _L, P, PE0, _Neg, _Assign, _UT))),
    %get_precondition(A, P),
    % DMILES
    %get_constrained_action_bb(Mt,action5(_,P,PE,_,_)),
    mysubset(P, State),
    PE0 = PE.
    %get_positiv_effect(A, PE).


 



%init_heuristics(+InitState).
init_heuristics(_).


















h_addb([], 0).
h_addb([H|T], E):-
		bb_get(predicatesPrices, Ps),
		member(H-Price, Ps),
		h_s_v(T, Sum),
    E is Sum + Price.
    



    
    
    
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

min_sas(A,B,A):-A =< B,!.
min_sas(_,A,A).



first_n_elements(ListR,Num,List):-length(ListR,PosNum),min_sas(PosNum,Num,MinNum),length(List,MinNum),append(List,_,ListR),!.




load_file(F):- must(read_file(F, L, Filename)),load_file_rest(Filename,L).
load_file_rest(_,[]):-!.
load_file_rest_es10(F,L):- first_n_elements(L,10,ES),load_file_rest(F,L,ES).

load_file_rest_es10(F,L,ES):-  append(_,['define','(','domain',Named|_],ES),!,
   domainBNF(O, L, R1),prop_set(filename,O,F),
   save_type_named(domain,Named,O),
   load_file_rest(F,R1).
load_file_rest_es10(F,L,ES):- append(_,['(','problem',Named|_],ES),!,
   problem(O, L, R1), prop_set(filename,O,F),
   save_type_named(domain,Named,O),
   load_file_rest(F,R1).
load_file_rest_es10(F,L,_ES):- 
   ensure_struct(sexpr_file,O),prop_set(filename,O,F),
   sterm(SO, L, R1),prop_set(sterm_value,O,SO),
   load_file_rest(F,R1).

load_domain(string(DP)):-!,load_file(string(DP)).
load_domain(DP):- \+ atom(DP),forall((filematch(DP,FOUND),exists_file(FOUND)),load_domain(FOUND)),!.
load_domain(DP):- \+ exists_file(DP),!, forall(filematch(DP,MATCH),((exists_file(MATCH),load_domain(MATCH)))).
load_domain(DP):-
   format('~q.~n',[load_domain(DP)]),
  directory_file_path(D,_,DP),directory_files(D,RList),   
   forall(member(T,RList),ignore((directory_file_path(D,T,TP),exists_file(TP),load_file(TP)))).



:-export(z2p/2).
z2p(A,A).

save_type_named(Type,Named,O):- doall(retract((is_saved_type(Type,Named,_):-_))),nop(ain((is_saved_type(Type,Named,A):-z2p(O,A)))).
save_sterm(O):-nop((gensym(sterm,Named),save_type_named(sterm,Named,O))).
*/


% :- solve_files(pddl('benchmarks/mystery/domain.pddl'),pddl('benchmarks/mystery/prob01.pddl')).
:-thread_local(t_l:loading_files/0).
:-thread_local(t_l:hyhtn_solve/1).
% t_l:other_planner(hyhtn_solve).



:- flag(time_used,_,0).
:- flag(time_used_other,_,0).

probfreecell:- solve_filespddl('benchmarks/freecell/domain.pddl', '../test/pddl_tests/benchmarks/freecell/probfreecell-9-5.pddl').
%:-debug,(must(test_blocks)).

:- fixup_exports.

end_of_file.


