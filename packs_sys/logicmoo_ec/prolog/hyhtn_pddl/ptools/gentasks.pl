/*
 * GIPO COPYRIGHT NOTICE, LICENSE AND DISCLAIMER.
 *
 * Copyright 2001 - 2003 by R.M.Simpson W.Zhao T.L.McCLuskey D Liu D. Kitchin
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both the copyright notice and this permission notice and warranty
 * disclaimer appear in supporting documentation, and that the names of
 * the authors or their employers not be used in advertising or publicity 
 * pertaining to distribution of the software without specific, written 
 * prior permission.
 *
 * The authors and their employers disclaim all warranties with regard to 
 * this software, including all implied warranties of merchantability and 
 * fitness.  In no event shall the authors or their employers be liable 
 * for any special, indirect or consequential damages or any damages 
 * whatsoever resulting from loss of use, data or profits, whether in an 
 * action of contract, negligence or other tortious action, arising out of 
 * or in connection with the use or performance of this software.
 */

% ---------------------------------------------------------------
% HTN OCL Random Task Generator  (September 2001)                       
%                                                               
% Donghong Liu                                                  
% ---------------------------------------------------------------

% randomly picks up substate classes in the domain specification,
% then randomly picks up instantiation for task:

% first ground all the hierarchy structure
% only generate the ground objects' states

%% to generate the initial state: 
%% for every dynamic object, pick up a random substate class 
%% in the sort of that object belongs to,
%% replace the variables to objects,
%% remove the ne(_,_) and the atomic_invarints which are always ture,
%% check the state avaliblity by check the inconsistent_constraint,
%% if not possible, generate again, or continue.

%% to generate the goal state: 
%% pick up a random set of dynamic objects,
%% the number of the objects is the Depth of the goal,
%% pick up a random substate class every object in that set,
%% pick up random number of predicates from them,
%% replace the variables to objects,
%% remove the ne(_,_) and the atomic_invarints which are always ture,
%% check the state avaliblity by check the inconsistent_constraint,
%% if not possible, generate again, or continue.

% to generate Upto number of tasks in full random - the Depth is random, 
% run: gen_full(Upto).

% to generate Upto number of tasks in a Depth of goal 
% (contain Depth number of dynamic objects), 
% run: gen_depth(Upto,Depth).

:- unknown(error,fail).
:- op(100,xfy,'=>').
:- use_module(library(random)).
:- dynamic solution_file/1.
:- dynamic task_counter/1.
:- dynamic gpred/2,gpredicates/2.
:- dynamic gsstates/3,gsubstate_classes/3.
:- dynamic fitted_ground_pred/2.

% generate Upto number of tasks, full random goal: can be any depth.
gen_full(Upto) :-
  make_data,
  task_counter(Base),
  Top is Upto + (Base - 1),
  make_task_full(Base, Top).
gen_full(Upto) :-
  solution_file(Out),
  tell(Out),
  told,
  nl,write('tasks generated successfully.'),nl,
  del_data,!.

make_task_full(TaskNo, TotalNum) :-
  dy_obj_list(D_ls),  % get the dynamic objects list
  length(D_ls,Len),   
  rdm(No,Len),        % get a ramdom no. from length as the depth
  gen_task(Ini,Goal,No),!,
  tell(user),
  nl,write('Task '),
  write(TaskNo),
  write(' generated.'),
  solution_file(Out),
  tell(Out),
  nl, write('planner_task('), write(TaskNo),
  write(','), nl,
  write(Goal),write(','), nl,%print Goal state
  nl,write(Ini), nl, %print Initial state
  write(').'), nl,
  TaskNo < TotalNum,
  Next is TaskNo + 1,
  make_task_full(Next, TotalNum).
  
make_task_full(TaskNo, TotalNum) :- !.  

% generate Upto number of tasks, give a fixed depth.
gen_depth(Upto,Depth):-
  make_data,
  make_task_depth(1, Upto, Depth).
gen_depth(Upto,Depth):-
  solution_file(Out),
  tell(Out),
  told,  
  nl,write('task generated successfully.'),nl,
  del_data,!.

make_task_depth(TaskNo, TotalNum,Depth) :-
  tell(user),
  nl,write('Task '),
  write(TaskNo),
  gen_task(Ini,Goal,Depth),!,
  write(' generated.'),
  solution_file(Out),
  tell(Out),
  nl, write('planner_task('), write(TaskNo),
  write(','), nl,
  write(Goal),write(','), nl,%print Goal state
  nl,write(Ini), nl, %print Initial state
  write(').'), nl,
  TaskNo < TotalNum,
  Next is TaskNo + 1,
  make_task_depth(Next, TotalNum,Depth),!.
  make_task_depth(TaskNo, TotalNum,Depth) :-!.  


% generate a single state
gen_task(Ini,Goal,Depth) :-
  gen_ini(Ini),
  gen_goal(Goal,Depth),!.

/* gen initial states, genearate rdm substate of every dynamic objects, one by one, fill in the varibles which need by random, and then check the inconsistent_constraint, if ture, generate again. */

gen_ini(Iniss) :-
  dy_obj_list(D_obj),
  gen_state(D_obj,[],Iniss,[],PlainStates).
gen_ini(Iniss) :-
  gen_ini(Iniss).

/* gen goal states, pick rdm num of rdm_th substate, fit in rdm variables*/
gen_goal(Goal,Depth) :-
  rdm_obj_ls(Rdm_obj,Depth),
  gen_state(Rdm_obj,[],Goalss,[],PlainStates),
  make_goal(Goalss,[],Goal).
gen_goal(Goal,Depth) :-
  gen_goal(Goal,Depth).

% make the goal state with se(...) from ss(...) generated from gen_state()
make_goal([],Goal,Goal):-!.
make_goal([ss(Sort,Obj,State)|TGoalses],List,Goal):-
  rdm_state_ls(State,Stateout),
  append(List,[se(Sort,Obj,Stateout)],List1),
  make_goal(TGoalses,List1,Goal).

% generate substate of every dynamic object

gen_state([],SS,SS,PS,PS):-!.
gen_state([Head_D_obj|Tail_D_obj],HSS,SS,HPS,PS):-
  retractall(fitted_ground_pred(_,_)),
  obj_belong(Head_D_obj,Sort),
  gen_obj_state(Sort,Head_D_obj,NoATSub,ATSub),
  append(HPS,NoATSub,PS1),
  append(PS1,ATSub,MPS),
  check_incon_OK(MPS),
  append(HSS,[ss(Sort,Head_D_obj,NoATSub)],MSS),
  gen_state(Tail_D_obj,MSS,SS,MPS,PS).

gen_obj_state(Sort,Obj,NoATSub,ATSub):-
  rdm_sub_state(Sort,Obj,Sub),
  search_for_binding(Sub, NOneSub, NE),
  check_no_atom(NOneSub,ATSub,NoATSub),
  replace_var(ATSub),
  replace_var(NoATSub),
  binding(NE).

% replace_var(Sub,NewSub)
% change the variables of the substate to objects.
/* result:
| ?- replace_var([at_thing(Thing,Place), inside(Thing,Bag)],P).
P = [at_thing(suit,office),inside(suit,briefcase)] ?
*/
replace_var([]).
replace_var([Pred|Sub]):-
  retract(fitted_ground_pred(Pred,Predls)),
  length(Predls,Len),
  rdm(No,Len),
  pickNth(Predls,No,NPred),
  Pred=NPred,
  subtract(Predls,NPred,Predls1),
  assert(fitted_ground_pred(Pred,Predls)),
  replace_var(Sub).
replace_var([Pred|Sub]):-
  not(fitted_ground_pred(Pred,_)),
  find_fitted_gpred(Pred),
  retract(fitted_ground_pred(Pred,Predls)),
  length(Predls,Len),
  rdm(No,Len),
  pickNth(Predls,No,NPred),
  subtract(Predls,NPred,Predls1),
  assert(fitted_ground_pred(Pred,Predls)),
  Pred=NPred,
  replace_var(Sub).

find_fitted_gpred(Pred):-
  functor(Pred,FF,NN),
  functor(FPred,FF,NN),
  setof(GPredls, FPred^gpredicates(FPred,GPredls),GPredls1),
  flatten(GPredls1,[],GPredls2),
  all_fits(GPredls2,Pred,Predls),
  assert(fitted_ground_pred(Pred,Predls)),!.

all_fits(GPredls,Pred,Predls):-
  setof(Pred, Pred^member(Pred,GPredls),Predls).


% utilities

% make a database to save the memery
make_data:-
  produce_sort_info,
  ground_hierarchy,
  setof(B, A^dy_objs(A,B), D_obj_list),
  flatten(D_obj_list,[],D_obj),
  assert(dy_obj_list(D_obj)),
     % dy_obj_list(D_obj): list of all the dynamic objects:
  produce_at_list,
     % atom_name_list(AtList, Atnames): list all the atomic_inviarants.
  produce_incon_list,!.
    % incon_list(Inlist): list all inconsistent_constraint

del_data:-
  retractall(dy_obj_list(_)),
  retractall(atom_name_list(_, _)),
  retractall(incon_list(_)),
  retractall(sort_info(_,_)),
  retractall(gpred(_,_)),
  retractall(gpredicates(_,_)),
  retractall(gsstates(_,_,_)),
  retractall(gsubstate_classes(_,_,_)).

% which objects belong to which sort
obj_belong(Obj,Sort) :-
  objects(Sort,Objlist),
  member(Obj, Objlist),!. 
  
% the number of objects by sort:
num_obj(Sort, No):-
   objects(Sort, Objls),
   length(Objls,No),!.

% the number of substate classes by sort:
num_subclass(Sort, Obj,Sub_no):-
   gsubstate_classes(Sort, Obj, Sub),
   length(Sub,Sub_no),!.

% pick up the nth item of the list

pickNth([H|T],1,H):-!.
pickNth([H|T],No,Itemnth) :-
   Next is No-1,
   pickNth(T,Next,Itemnth),!.


% must use the library use_module(library(random)).
rdm(1,1):-!.
rdm(N,Upto):-
   Upper is Upto + 1,
   random(1, Upper,N).
rdm(N,Upto):-
   Upper is Upto + 1,
   random(1, Upper,N).
% produce a list with rdm num of rdm dynamic objects
rdm_obj_ls(Rdm_obj,Depth) :-
  dy_obj_list(D_obj),
  length(D_obj,Len),
  randset(Depth,Len,No_ls),
  gen_item_ls(No_ls,D_obj,Rdm_obj).

% produce a list with Depth of rdm substates
rdm_state_ls(State,Stateout) :-
  length(State,Len),
  rdm(No,Len), 
  randset(No,Len,No_ls),
  gen_item_ls(No_ls,State,Stateout).

% produce a sublist from a list by a number list, like:
/*| ?- gen_item_ls([5,3,1],[cheque,suit,dictionary,briefcase,suitcase],P).
P = [cheque,dictionary,suitcase] ? */

gen_item_ls([],List,Helpls,Helpls).
gen_item_ls([HNo_ls|Rest],List,Itemls) :-
  gen_item_ls([HNo_ls|Rest],List,[],Itemls).
gen_item_ls([HNo_ls|Rest],List,Helpls,Itemls) :-
  pickNth(List,HNo_ls,Obj1),
  gen_item_ls(Rest,List,[Obj1|Helpls],Itemls).

% pick up a random substate of a sort

rdm_sub_state(Sort,Obj,Sub) :-
  num_subclass(Sort,Obj,Total_Sub),
  rdm(No_sub, Total_Sub),
  gsubstate_classes(Sort,Obj,Sublist),
  pickNth(Sublist,No_sub,Sub).

% get rid of the atomic_invariants in the substate meet the atomic_invariants 

check_no_atom([],Atnames,No_at_Pres,AT,AT,No_at_Pres) :-!.
check_no_atom(Prels,AT,No_at_Pres):-
  atom_name_list(AtList, Atnames),
  check_no_atom(Prels,Atnames,[],[],AT,No_at_Pres),!.
check_no_atom([HPred|TPred],Atnames,Hls,HAtom,AT,No_at_Pres) :-
  HPred =.. [HP|TP],
  not(member(HP,Atnames)),
  append_v(Hls,[HPred],Mls),
  check_no_atom(TPred,Atnames,Mls,HAtom,AT,No_at_Pres),!.
check_no_atom([HPred|TPred],Atnames,Hls,HAtom,AT,No_at_Pres) :-
  check_no_atom(TPred,Atnames,Hls,[HPred|HAtom],AT,No_at_Pres),!.

% check_incon_OK States list if it is inconsistent              
check_incon_OK(States):-
  length(States,1),!.
check_incon_OK(States):-
  incon_list(InList),
  not(check_in00(InList,States)).

check_in00([HInLs|Rest],States) :-
  check_in11(HInLs,States).
check_in00([HInLs|Rest],States) :-
  check_in00(Rest,States).

check_in11(Inls,States):-
  search_for_binding(Inls,NOne, NE),
  instantiate(NOne,States,NE),
  subtract(NOne,States,[]).

instantiate([],States,NE):-!.
instantiate([H|T],States,NE):-
  member(H,States),
  binding(NE),
  instantiate(T,States,NE),!.
instantiate([H|T],States,NE):-
  instantiate(T,States,NE),!.

% search the list to find out the ne()s, 
% search_for_binding(List, NoNels, NEls)
search_for_binding([],[], []):-!.
search_for_binding([ne(A,B)|TLs],NOne, [ne(A,B)|NE1]):-
  search_for_binding(TLs,NOne,NE1),!.
search_for_binding([is_of_sort(A,B)|TLs],NOne, [is_of_sort(A,B)|NE1]):-
  search_for_binding(TLs,NOne,NE1),!.
search_for_binding([is_of_primitive_sort(A,B)|TLs],NOne, [is_of_primitive_sort(A,B)|NE1]):-
  search_for_binding(TLs,NOne,NE1),!.
search_for_binding([HInLs|TLs],[HInLs|NOne1], NE):-
  search_for_binding(TLs,NOne1, NE),!.

binding([]).
binding([ne(A,B)|Rest]):-
  A\==B,
  binding(Rest).
binding([is_of_primitive_sort(A,B)|Rest]):-
  get_sort_objects(B,Objls),
  member(A,Objls),
  binding(Rest).
binding([is_of_sort(A,B)|Rest]):-
  get_sort_objects(B,Objls),
  member(A,Objls),
  binding(Rest).

% dynamic objects with sort:
dy_objs(Sort, Dy_O_list) :-
   dy_sortlist(D_sortl),
   member(Sort,D_sortl),
   find_objects(Sort,Dy_O_list).

      
% dynamic sorts (the sorts have substate_classes).
dy_sortlist(D_sortl) :-
   setof(A, B^gsubstate_classes(A,_,B), D_sortl).

% produce a atom_name_list(AtList, Atnames).
produce_at_list:-
  atomic_invariants(AtList),
  headls(AtList,[],Atnames),
  assert(atom_name_list(AtList,Atnames)),!.
produce_at_list:-
  assert(atom_name_list([],[])),!.

% produce a incon_list(IList).
produce_incon_list:-
  setof(A,A^inconsistent_constraint(A),List),
  assert(incon_list(List)),!.
% if inconsistent_constraint not defined, ie. automatic generated domain
% find some of them for state check
produce_incon_list:-
  not(inconsistent_constraint(A)),
  find_inconsistent,
  setof(A,A^inconsistent_constraint(A),List),
  assert(incon_list(List)),!.

find_inconsistent:-
   search_from_substateclass,
   search_from_operators,!.
find_inconsistent.
% if the predicates shows the relation of two variables of same sort,
% when the two variables are same, the predicate is inconsistent
search_from_predicates:-
   predicates(Preds),
   search_from_predicates1(Preds).

search_from_predicates1([]):-!.
search_from_predicates1([Pred|Rest]):-
   Pred=..[Name|Sorts],
   check_sorts(Sorts,Samesortls),
   Pred=..[Name|List],
   assert_pred_incon(Pred,Samesortls),
   search_from_predicates1(Rest),!.

check_sorts([],[]):-!.
check_sorts([HS|TS],[HS|Rest]):-
   member(HS,TS),
   check_sorts(TS,Rest),!.
check_sorts([HS|TS],List):-
   check_sorts(TS,List),!. 

assert_pred_incon(Pred,[]):-!.
assert_pred_incon(Pred,Samesortls):-
   assert(incon([Pred])),!.

% one object can only in one of the substate classes
% group the states which are subsets
% states in different group times together and make inconsistent
% at which, get rid of atomatic invariants and the same states in both side except of the nes.
search_from_substateclass:-
   gsubstate_classes(Sort,Obj,SubClass),
   sort_list(SubClass,States),
        %sort the substates into groups which are substets
   search_from_substateclass1(States),
   fail.
search_from_substateclass.

search_from_substateclass1(State):-
   length(State,N),
   N>1,
   search_from_substateclass2(State),!.

search_from_substateclass2([H|[]]):-!.
search_from_substateclass2([H|T]):-
   get_incon(H,T),
   search_from_substateclass2(T),!.

% sort the substates into groups which are substets
% first find out the bigest sets
% then put the smaller set under the bigest set
sort_list(StateIn, StateOut):-
   find_bigest(StateIn,[],BigStates,StateIn,OthersStates),
   group_states(BigStates,OthersStates,StateOut),!.

% first find out the bigest sets
find_bigest([],States,States,StatesOthers,StatesOthers):-!.
find_bigest([H|T],List,States,List1,StatesOthers):-
   is_subset(H,T),
   find_bigest(T,List,States,List1,StatesOthers),!.
find_bigest([H|T],List,States,List1,StatesOthers):-
   is_subset(H,List),
   find_bigest(T,List,States,List1,StatesOthers),!.
find_bigest([H|T],List,States,List1,StatesOthers):-
   append(List,[[H]],List0),
   subtract(List1,[H],List2),
   find_bigest(T,List0,States,List2,StatesOthers),!.

group_states([],OStates,[]):-!.
group_states([H|T],OStates,[H1|T1]):-
   group_states1(H,OStates,H,H1),
   group_states(T,OStates,T1),!.

group_states1(State,[],List,List):-!.
group_states1(State,[H|T],List,StateOut):-
   is_subset(H,State),
   append(List,[H],List1),
   group_states1(State,T,List1,StateOut),!.
group_states1(State,[H|T],List,StateOut):-
   group_states1(State,T,List,StateOut),!.
                
get_incon(State1,[]):-!.
get_incon(State1,[State2|Rest]):-
   get_incon0(State1,State2),%the result is State1 X State2 
   get_incon(State1,Rest).

%the inconsistent states are states of State1 times State2
%State1 and State2 are lists of subset states
get_incon0([],State2):-!.
get_incon0([H1|T1],State2) :-
   get_incon1(H1,State2),
   get_incon0(T1,State2),!.

get_incon1(H1,[]):-!.
get_incon1(H1,[H2|T2]) :-
   get_incon2(H1,H2,[],State),
   assert_incon(State),
   get_incon1(H1,T2),!.

get_incon2([],State2,List,State):-
   append(List,State2,State),!.
get_incon2([H|T],State2,List,State) :-
   H=..[ne|Vars],
   vmember(H,State2),
   get_incon2(T,State2,List,State),!.
get_incon2([H|T],State2,List,State) :-
   vmember(H,State2),
   subtract(State2,[H],State21),
   get_incon2(T,State21,List,State),!.
get_incon2([H|T],State2,List,State) :-
   append(List,[H],List1),
   get_incon2(T,State2,List1,State),!.

% in the operators, states of precondition and post conditions can't exist at
% so check the neccessary changes,
% put the different objects' lhs and rhs(they must have same variables)
% together and produce inconsistents.
search_from_operators:-
   operator(Name,Prev,Nec,Cond),
   search_nec(Nec),
   search_cond(Cond),
   fail.
search_from_operators.

search_nec([H|[]]):-
   combin_lhs_rhs(H),!.
search_nec([H|T]):-
   combin_lhs_rhs(H),
   search_nec1(H,T),
   search_nec(T),!.

search_cond([]):-!.
search_cond([H|T]):-
   combin_lhs_rhs(H),
   search_cond(T),!.

search_nec1(H,[]):-!.
search_nec1(H1,[H2|T]):-
   combin2_lhs_rhs(H1,H2),
   search_nec1(H1,T),!.

combin_lhs_rhs(sc(Sort,Obj,Lhs=>Rhs)):-
   combin_lhs_rhs1(Lhs,Rhs),!.

combin2_lhs_rhs(sc(Sort1,Obj1,Lhs1=>Rhs1),sc(Sort2,Obj2,Lhs2=>Rhs2)):-
   combin_lhs_rhs1(Lhs1,Rhs2),
   combin_lhs_rhs1(Lhs2,Rhs1),!.

combin_lhs_rhs1(Lhs,Rhs):-
   used_objs(Lhs,[],Objls1),
   used_objs(Rhs,[],Objls2),
   have_same_obj(Objls1,Objls2),
   get_incon2(Lhs,Rhs,[],State),
   assert_incon(State),!.

have_same_obj([H|T],List2):-
   vmember(H,List2),!.
have_same_obj([H|T],List2):-
   have_same_obj(T,List2),!.

assert_incon([H|[]]):-!.
assert_incon(State):-
   not_smallest(State).
assert_incon(State):-
   retract_bigger(State),
   assert(inconsistent_constraint(State)),!.

not_smallest(State):-
   inconsistent_constraint(State).
not_smallest(State):-
   inconsistent_constraint(State1),
   is_subset(State1,State).

retract_bigger(State):-
   inconsistent_constraint(State1),
   is_subset(State,State1),
   retract(inconsistent_constraint(State1)),
   fail.
retract_bigger(State).


% headls(List,[],Pnames) --use for make the list of predicates names 
% from a list of predicates
headls([],Hlist,Hlist).
headls([Hls|Tls],Hlist,PNamels):-
  Hls =.. [Pre|Rest],!,
  headls(Tls, [Pre|Hlist],PNamels).
headls([Hls|Tls],Hlist,PNamels):-
  headls(Tls, Hlist,PNamels),!.

append_v([],L,L).
append_v(A,[[]],A). 
append_v([H|T],L,[H|Z]) :- 
        var(H),
        append_v(T,L,Z),!.
append_v([H|T],L,[H|Z]) :- 
        not(var(H)),
        not(H == []),
        append_v(T,L,Z),!.

% subtract_1(A,B,C): subtract B from A
% -------------------------------------
subtract_1([],_,[]) .
subtract_1([A|B],C,D) :-
        member(A,C),
        subtract_1(B,C,D),!.
subtract_1([A|B],C,[A|D]) :-
        subtract_1(B,C,D),!.

%.. sort_info(next(Box1,Box2),[is_of_sort(Box1,box),is_of_sort(Box2,box)]).
% from onprecede

produce_sort_info :-
        predicates(L),
        produce_sort_info(L),!.
produce_sort_info([]).
produce_sort_info([X|L]) :-
        generalise(X,X1),
        X =.. [H|T],
        X1 =.. [H|T1],
        produce_sort_ofs(T,T1,S),
        assert(sort_info(X1,S)),
        produce_sort_info(L).

generalise(X,X1) :-
        X =.. [H|T],
        genlist(T,T1),
        X1 =.. [H|T1],!.
genlist([],[]) :- !.
genlist([H|T],[H1|T1]) :-
        genlist(T,T1),!.

produce_sort_ofs([],[],[]).
produce_sort_ofs([T|L],[T1|L1],[is_of_sort(T1,T)|S1]) :-
        produce_sort_ofs(L,L1,S1),!.

% -------------------------------------------

not(X):- \+X.
member(X,[X|_]).
member(X,[_|L]) :- member(X,L).

% vmember -- check a Var or a [Var,_] list if Var is in a varable list. 
        /* example:
        | ?- vmember(E,[A,B,C,D]).
        no
        %compare with using:
        | ?- member(E,[A,B,C,D]).
         = A ? 
        */

vmember(Var,[HVls|Rest]) :-
  Var==HVls,!.
vmember(Var,[HVls|Rest]) :-
  vmember(Var,Rest),!.

% emptylist -- identify a emptylist

emptylist([]).

% append: list * list -> list
% -----------------------------------------------
append([],L,L):-!.
append([H|T],L,[H|Z]) :- append(T,L,Z),!.
% set_append: list * list -> list (no duplicates)
% -----------------------------------------------
set_append([], Z, Z):-! .
set_append([A|B], Z, C) :-
        not(not(member(A, Z))) ,
        set_append(B, Z, C),! .
set_append([A|B], Z, [A|C]) :-
        set_append(B, Z, C) .

% subtract(A,B,C): subtract B from A
% -------------------------------------
subtract([],_,[]):-!.
subtract([A|B],C,D):-
        vmember(A,C) ,
        subtract(B,C,D),!.
subtract([A|B],C,[A|D]):-
        subtract(B,C,D),!.

% succeeds if first arg is a subset of second
is_subset([],Set):-!.
is_subset(Set,[HSet2|TSet2]) :-
     is_subset(Set,HSet2).
is_subset(Set,[HSet2|TSet2]) :-
     is_subset(Set,TSet2).
is_subset([ne(A,B)|Set],Set2) :-
     A\==B,
     is_subset(Set,Set2).
is_subset([S|Set],Set2) :-
     vmember(S,Set2),
     is_subset(Set,Set2).

% xprod: list * list --> (list X list)
% -----------------------------------
xprod(A,B,C) :-
        xprod([A,B],C) .
 
xprod([],[]).
xprod(A,E) :-
        xprod(A,B,C,D) ,
        F =..[^,C,D] ,
        call(setof(B,F,E)) .
 
xprod([X],[A],A,member(A,X)) .
xprod([X,Y],[A,B],C,(D,E)) :-
        C =..[^,A,B] ,
        D =..[member,A,X] ,
        E =..[member,B,Y] .
xprod([X|Y],[A|E],D,(F,G)) :-
        D =..[^,A,C] ,
        F =..[member,A,X] ,
        xprod(Y,E,C,G).% list of lists -> list

flatten([HO|TO], List, O_List):-
        append(HO, List, List_tmp),
        flatten(TO, List_tmp, O_List),!.
flatten([H|TO], List,O_List):-
        append([H], List, List_tmp),
        flatten(TO, List_tmp, O_List).
flatten([], [HList|T], O_List):-
        HList = [],
        flatten(T, [], O_List).
flatten([], [HList|T], O_List):-
        list(HList),
        flatten([HList|T],[], O_List),!.
flatten([], L,L):-!.

% list: [el1,el2, ...] --> bool
% -----------------------------
list(A) :-
        var(A) ,
        ! ,
        fail .
list(A) :-
        functor(A,'.',_).

%ground the hierarchy structure of HTN planning domain
ground_hierarchy:-
     ground_predicates,
     ground_substate_class,!.

% grounding predicates to object level
% assert in prolog database as gpredicates(Pred,GPredls)
ground_predicates:-
     predicates(Preds),
     grounding_preds(Preds),
     collect_grounded_pred,!.

grounding_preds([]).
% if it is statics, only choose within atomic_invariants
% else, combine all the objects
grounding_preds([HPred|TP]):-
     ground_all_var(HPred),
     grounding_preds(TP).

% collect all the grounded predicates together
collect_grounded_pred:-
     gpred(Pred,_),
     setof(GPred,gpred(Pred,GPred),GPredls),
     retractall(gpred(Pred,_)),
     assert(gpredicates(Pred,GPredls)),
     fail.
collect_grounded_pred.% grounding substate_class to primary sort level
% assert in prolog database as gsubstate_class(Sort,Obj,States)
ground_substate_class:-
     substate_classes(Sort,Obj,Substate),
     find_prim_sort(Sort,PS),
     assert_subclass(PS,Obj,Substate),
     fail.
ground_substate_class:-
     collect_grounded_substates.

assert_subclass([],Obj,Substate).
assert_subclass([HS|TS],Obj,Substate):-
     assert(gsstates(HS,Obj,Substate)),
     assert_subclass(TS,Obj,Substate).

collect_grounded_substates:-
     gsstates(Sort,Obj,_),
     setof(SStates,gsstates(Sort,Obj,SStates),GSStates),
     retractall(gsstates(Sort,Obj,_)),
     all_combined(GSStates,GSStates0),
     assert(gsubstate_classes(Sort,Obj,GSStates0)),
     fail.
collect_grounded_substates.

all_combined(SStates,CSStates):-
     xprod(SStates,CSStates1),
     flat_interal(CSStates1,CSStates),!.

flat_interal([],[]):-!.
flat_interal([HSS1|TSS1],[HSS|TSS]):-
     flatten(HSS1,[],HSS),
     flat_interal(TSS1,TSS),!.
ground_all_var(Pred):-
     functor(Pred,FF,NN),
     functor(GPred,FF,NN),
     GPred=..[Name|Vars],
     Pred=..[Name|Sorts],
     ground_all_var0(Vars,Sorts),
     not(inconsistent_constraint([GPred])),
     assert_gpred(GPred),
     fail.
ground_all_var(Pred).


ground_all_var0([],[]).
ground_all_var0([HVars|TV],[HSorts|TS]):-
     find_objects(HSorts,Objls),
     member(HVars,Objls),
     ground_all_var0(TV,TS).

% find all the objects with sort:
find_objects(Sort,Dy_O_list):-
   find_prim_sort(Sort,PSortls),
   find_objects1(PSortls,[],Dy_O_list),!.

find_objects1([],Obj_list,Obj_list).
find_objects1([HSort|PSortls],List,Obj_list):-
   objects(HSort,Objls),
   set_append(List,Objls,List1),
   find_objects1(PSortls,List1,Obj_list),!.

% assert grounded predicates with related only prim_sorts predicates
assert_gpred(GPred):-
     functor(GPred,FF,NN),
     functor(UPred,FF,NN),
     assert_gpred0(GPred,UPred),!.

% if the predicate is not defined in atomic invariants, assert it with 
% its prim_sorts predicates
assert_gpred0(GPred,UPred):-
     functor(GPred,FF,NN),
     functor(APred,FF,NN),
     GPred=..[Name|Objs],
     UPred=..[Name|Vars],
     atomic_invariants(Atom),
     not(member(APred,Atom)),
     find_primsorts(Vars,Objs),
     assert(gpred(UPred,GPred)).
% if no atomic invariants been defined, assert with prim_sorts
assert_gpred0(GPred,UPred):-
     functor(GPred,FF,NN),
     functor(APred,FF,NN),
     GPred=..[Name|Objs],
     UPred=..[Name|Vars],
     not(atomic_invariants(_)),
     find_primsorts(Vars,Objs),
     assert(gpred(UPred,GPred)).
% if the predicate is defined in atomic invariants,
% only assert which's meets the defines
assert_gpred0(GPred,UPred):-
     functor(GPred,FF,NN),
     functor(APred,FF,NN),
     atomic_invariants(Atom),
     member(APred,Atom),
     assert_gpred_atom(GPred,UPred,APred).
% else, don't insert
assert_gpred0(GPred,UPred).

% if the predicate is defined in atomic invariants, 
% and atomic invarients are defined as specifed sorts, 
% only all sorts fits are asserted 
assert_gpred_atom(GPred,UPred,APred):-
     GPred=..[Name|Objs],
     UPred=..[Name|Vars],
     APred=..[Name|ASorts],
     obj_fit_atom_sorts(Objs,ASorts),
     find_primsorts(Vars,Objs),
     assert(gpred(UPred,GPred)).

% find out primitive sorts of a object list.
find_primsorts([],[]).
find_primsorts([Sort|TV],[HObj|TO]):-
     objects(Sort,Objls),
     member(HObj,Objls),
     find_primsorts(TV,TO).

% check if the object list fit an atom_invariants which contain sorts
obj_fit_atom_sorts([],[]).
obj_fit_atom_sorts([HObj|Objs],[HObj|ASorts]):-
     obj_fit_atom_sorts(Objs,ASorts).
obj_fit_atom_sorts([HObj|Objs],[HASort|ASorts]):-
     get_sort_objects(HASort,AObjls),
     member(HObj,AObjls),
     obj_fit_atom_sorts(Objs,ASorts).
     
% find out primitive sorts of a sort.
find_prim_sort(Sort,PS):-
  subsorts(Sort,Subsorts),
  split_prim_noprim(Subsorts,PS,NP).

% find out the objects of a sort
get_sort_objects(Sort,Objs):-
   find_prim_sort(Sort,PSorts),
   get_objects1(PSorts,Objls),
   flatten(Objls,[],Objs),!.

get_objects1([],[]):-!.
get_objects1([PS1|RS],[Objls1|Objls]):-
   objects(PS1,Objls1),
   get_objects1(RS,Objls),!.

% find subsorts of a sort(include).
subsorts(Sort,Subsorts):-
  sort_down([Sort],[Sort],Subsorts),!.

sort_down([],Subsorts,Subsorts):-!.
sort_down([HOpen|TOpen],List,Sortslist):-
  sorts(HOpen,Sorts),
  append(List,Sorts,List1),
  append(TOpen,Sorts,Open1),
  sort_down(Open1,List1,Sortslist),!.
sort_down([HOpen|TOpen],List,Sortslist):-
  sort_down(TOpen,List,Sortslist),!.
  
% find uppersorts of a sort or object(include).
uppersorts(Sort,Uppersorts):-
  objects(Sort,Objls),
  sort_up(Sort,[Sort],Uppersorts),!.
uppersorts(Sort,Uppersorts):-
  sorts(Sort,Sortls),
  sort_up(Sort,[Sort],Uppersorts),!.
uppersorts(Obj,Sortls):-
  objects(Sort,Objls),
  member(Obj, Objls),
  sort_up(Sort,[Sort],Sortls),!.

sort_up(Sort, List,List):-
  not(sorts(non_primitive_sorts,_)),!.
sort_up(Sort, List,Sortslist):-
  sorts(non_primitive_sorts,NPSorts),
  sort_up1(Sort,NPSorts,NPSorts,List,Sortslist),!.

sort_up1(Sort,[],NPSorts,Sortslist,Sortslist):-!.
sort_up1(Sort,[HNPSorts|TNPSorts],NPSorts,List,Sortslist):-
  sorts(HNPSorts,Sorts),
  member(Sort,Sorts),
  append(List, [HNPSorts], List1),
  sort_up(HNPSorts,List1,Sortslist),!. 
sort_up1(Sort,[HNPSorts|TNPSorts],NPSorts,List,Sortslist):-
  sort_up1(Sort,TNPSorts,NPSorts,List,Sortslist),!.

% find out primitive sorts from a sorts list.
split_prim_noprim([],[],[]):-!.
split_prim_noprim([HS|TS],[HS|TP],NP):-
     objects(HS,Obj),
     split_prim_noprim(TS,TP,NP),!.             
split_prim_noprim([HS|TS],PS,[HS|NP]):-
     split_prim_noprim(TS,PS,NP),!.

write_gpred:-
   gpredicates(P,Q),
   write('ground_predicates\('),
   write(P),write('\, '),
   write(Q),write('\)'),nl,
   fail.
write_gpred.

% find out all the objects used in the substate
used_objs([],Objls,Objls):-!.
used_objs([H|T],List,Objls):-
   H=..[Name|Objs],
   append(List,Objs,List1),
   used_objs(T,List1,Objls),!.
