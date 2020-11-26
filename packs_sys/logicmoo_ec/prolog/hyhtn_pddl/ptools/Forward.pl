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

:-use_module(library(system)).
:- op(100,xfy,'=>').


:- dynamic num_nodes/1,achieve_plan/2.
:- dynamic init_state/1,goal_state/1,level/2,closed_node/2.
:- dynamic gpred/2,gpredicates/2.
%Ron 21/9/01
:- dynamic kill_file/1.

:-set_prolog_flag(unknown,fail).

num_nodes(0).

solve(Id) :-
	planner_task(Id,Goal,Init),
	planner_interface(Goal,Init,Sol,_),
	solution_file(F),
%	write('RUN TASK ID '),write(Id),nl,
	tell(F),
	write('TASK '),write(Id),nl,
	write('SOLUTION'),nl,
	display_sol(Sol),
	write('END FILE'),nl,
	told,
	clean.

display_sol([]).
display_sol([H|T]) :-
	write(H),
	nl,
	display_sol(T).

clean :-
	retractall(num_nodes(_)),
	retractall(level(_,_)),
	retractall(init_state(_)),
	retractall(goal_state(_)),
	retractall(achieve_plan(_,_)),
	retractall(gpredicates(_,_)),
	retractall(closed_node(_,_)),
%        retractall(gpred(_,_)),
	assert(num_nodes(0)).
%	write('DONE CLEAN'),nl.

planner_interface(G,I, SOLN,NODES):-
	ground_predicates,
        retract(num_nodes(_)),
        assert(num_nodes(0)),
	assert(level(0,node([],I))),
	assert(init_state(I)),
	assert(goal_state(G)),
	statistics(runtime,[_,Time]),
	start_solve(SOLN,NODES).

%Ron  21/9/01 - Try to give a closedown method
start_solve(SOLN,NODES):-
	kill_file(Kill),
	file_exists(Kill).
%	write('Found kill file'),nl.
	
	
start_solve(SOLN,NODES):-
       achieve_plan(State,SOLN),
       tell(user),
%       nl,write('success plan:'),nl,
%       write(SOLN),nl,nl,
       retract(num_nodes(NODES)),
%       write(NODES),write('nodes generated '),nl,
       statistics(runtime,[_,Time1]),
       Time_in_secs is Time1/1000,
%       write(Time_in_secs), write(' seconds of cpu'),
       Nodes_per_sec is NODES/Time_in_secs,
%       nl,write(Nodes_per_sec),  write(' nodes search per second'),nl,
       told.   
start_solve(SOLN,NODES):-       
       lowest_score(Score),
       tell(user),
%       write(Score),
       retract(level(Score,node(Ops,State))),
       assert(closed_node(Score,node(Ops,State))),
       apply_all_op(Ops,State),
       start_solve(SOLN,NODES).
start_solve(SOLN,NODES):-
       tell(user),
%       nl,write('task failure'),nl,
       told.	

apply_all_op(Ops,State):-
   operator(Op,Pre,Nec,Cond),
   achieved(Pre,Nec,State),
   binding_ses(Pre),
   binding_scs(Nec),
   apply(Op,Pre,Nec,Cond,State,State1),
   assert_node(Ops,Op,State,State1),
   fail.
apply_all_op(Ops,State).

apply(Op,Pre,Nec,Cond,[],[]).
apply(Op,Pre,Nec,Cond,[ss(Sort,Obj,HS)|TS],[ss(Sort,Obj,HS)|TS1]):-
   member(se(Sort,Obj,Prev),Pre),
   achieved(Prev,HS),
   apply(Op,Pre,Nec,Cond,TS,TS1),!.
apply(Op,Pre,Nec,Cond,[ss(Sort,Obj,HS)|TS],[ss(Sort,Obj,Rhs)|TS1]):-
   member(sc(Sort,Obj,Lhs=>Rhs),Nec),
   achieved(Lhs,HS),
   apply(Op,Pre,Nec,Cond,TS,TS1),!.
apply(Op,Pre,Nec,Cond,[ss(Sort,Obj,HS)|TS],[ss(Sort,Obj,Rhs)|TS1]):-
   member(sc(Sort,Obj,Lhs=>Rhs),Cond),
   binding_vars(Lhs),
   binding_vars(Rhs),
   achieved(Lhs,HS),
%   instan_var([sc(Sort,Obj,Lhs=>Rhs)]),
   apply(Op,Pre,Nec,Cond,TS,TS1),!.
apply(Op,Pre,Nec,Cond,[ss(Sort,Obj,HS)|TS],[ss(Sort,Obj,HS)|TS1]):-
   apply(Op,Pre,Nec,Cond,TS,TS1),!.

% -----------------------------------------------------
% 1.get rid of the se() or sc()
binding_ses([]).
binding_ses([se(_,_,States)|Rest]):-
        binding_vars(States),
        binding_ses(Rest).

binding_scs([]).
binding_scs([sc(_,_,Lhs=>Rhs)|Rest]):-
	append(Lhs,Rhs,SCS),
        binding_vars(SCS),
        binding_scs(Rest).
% 2.search the state list 
binding_vars([]).
binding_vars([ne_back(A,B)|TLs]):-
        A\==B,
        binding_vars(TLs).
binding_vars([ne(A,B)|TLs]):-
	append(TLs,[ne_back(A,B)],TLs1),
        binding_vars(TLs1).
binding_vars([Pred|TLs]):-
	atomic_invariants(Atlist),
        member(Pred,Atlist),
        binding_vars(TLs).
binding_vars([Pred|TLs]):-
	functor(Pred,FF,NN),
        functor(Pred1,FF,NN),
        gpredicates(Pred1,PList),
        member(Pred,PList),
        binding_vars(TLs).

assert_node(Ops,Op,State,State1):-
   not(level(_,node(_,State1))),
   not(closed_node(_,node(_,State1))),
   append(Ops,[Op],Ops1),
   compare_goal(State1,Length,Ops1),
   length(Ops1,LL),
   Score is LL+Length*10,
   num_nodes(NN),
   NN<100, %only store the first 100 nodes
   tell(trace),
%   write('Score '),write(Score),nl,
%   write('operators '),write(Ops1),nl,
%   write('state'),write(State1),nl,
   told,
   NN1 is NN +1,
   retract(num_nodes(NN)),
   assert(num_nodes(NN1)),
   assert(level(Score,node(Ops1,State1))),!.
assert_node(Ops,Op,State,State1):-
   not(level(_,node(_,State1))),
   not(closed_node(_,node(_,State1))),
   append(Ops,[Op],Ops1),
   compare_goal(State1,Length,Ops1),
   length(Ops1,LL),
   Score is LL+Length*10,
   retract(num_nodes(NN)),
   NN1 is NN +1,
   assert(num_nodes(NN1)),
   assert(level(Score,node(Ops1,State1))),!.

instan_var([]).
instan_var([ss(Sort,Obj,State)|TS]):-
     instan_var0(State),
     instan_var(TS).
instan_var([se(Sort,Obj,State)|TS]):-
     instan_var0(State),
     instan_var(TS).
instan_var([sc(Sort,Obj,Lhs=>Rhs)|TS]):-
     instan_var0(Lhs),
     instan_var0(Rhs),
     instan_var(TS).

instan_var0([]).
instan_var0([HS|TS]):-
     functor(HS,FF,NN),
     functor(Pred,FF,NN),
     gpredicates(Pred,PList),
     member(HS,PList),
     instan_var0(TS).
instan_var0([HS|TS]):-
     instan_var0(TS).

achieved(Pre,Nec,State):-
   achieved_se(Pre,State),
   lhs_achieved(Nec,State).

achieved_se([], State).
achieved_se([se(Sort,Obj,H)|T], State) :-
    member(ss(Sort,Obj,SState),State),
    achieved(H,SState),
    achieved_se(T,State).
lhs_achieved([], State).
lhs_achieved([sc(Sort,Obj,H=>R)|T], State) :-
    member(ss(Sort,Obj,SState),State),
    achieved(H,SState),
    lhs_achieved(T,State).

achieved([], State).
achieved([ne(A,B)|T], State) :-
    A\==B,
    achieved(T,State).
achieved([H|T], State) :-
    member(H,State),
    achieved(T,State).
achieved([H|T], State) :-
    atomic_invariants(AtList),
    member(H,AtList),
    achieved(T,State).
achieved([H|T], State) :-
    is_subset(H,State),
    achieved(T,State).


% succeeds if first arg is a subset of second
is_subset([S|Set],Set2) :-
     member(S,Set2),
     is_subset(Set,Set2).
is_subset([ne(_,_)|Set],Set2) :-
     is_subset(Set,Set2).
is_subset([],_).

% find out the the differece length from current state to goal state
% the lower the better
% if no differece, assert achieved_plan(State,Plan) for further use
compare_goal(State,Length,Ops):-
     goal_state(Goal),
     diff(State,Goal,0,Length),
     Length\==0,!.
compare_goal(State,0,Ops):-
     assert(achieve_plan(State,Ops)),!.

% find out the the differece length from current state to goal state
diff(State,[],Len,Len).
diff(State,[se(Sort,Obj,GS)|TS],Len,Length):-
     member(ss(Sort,Obj,SS),State),
     achieved(GS,SS),
     diff(State,TS,Len,Length),!.
diff(State,[se(Sort,Obj,GS)|TS],Len,Length):-
     Len1 is Len+1,
     diff(State,TS,Len1,Length),!.

% find out the the differece length from current state with initial state
% the lower the better
changed_init(State,Length):-
     init_state(Init),
     diff_in(State,Init,0,Length),!.

diff_in([],Init,Len,Len).
diff_in([ss(Sort,Obj,HS)|TS],Init,Len,Length):-
     member(ss(Sort,Obj,IS),Init),
     achieved(HS,IS),
     diff_in(TS,Init,Len,Length),!.
diff_in([ss(Sort,Obj,HS)|TS],Init,Len,Length):-
     Len1 is Len+1,
     diff_in(TS,Init,Len1,Length),!.

% grounding predicates to object level
% assert in prolog database as gpredicates(Pred,GPredls)


ground_predicates:-
     predicates(Preds),
     grounding_preds(Preds),
     collect_grounded_pred,!.

grounding_preds([]).
% if it is statics, only choose within atomic_invariants
grounding_preds([HPred|TP]):-
     functor(HPred,FF,NN),
     functor(APred,FF,NN),
     atomic_invariants(Atom),
     member(APred,Atom),
     ground_all_var_atom(HPred),
     grounding_preds(TP).
% else, combine all the objects
grounding_preds([HPred|TP]):-
     functor(HPred,FF,NN),
     functor(GPred,FF,NN),
     ground_all_var(GPred,HPred),
     grounding_preds(TP).

% collect all the grounded predicates together
collect_grounded_pred:-
     gpred(Pred,_),
     setof(GPred,gpred(Pred,GPred),GPredls),
     retractall(gpred(Pred,_)),
     assert(gpredicates(Pred,GPredls)),
     fail.
collect_grounded_pred.

ground_all_var_atom(HPred):-
     functor(HPred,FF,NN),
     functor(GPred,FF,NN),
     functor(APred,FF,NN),
     atomic_invariants(Atom),
     member(APred,Atom),
     GPred=..[Name|Vars],
     APred=..[Name|Sorts],
     ground_all_var_atom0(Vars,Sorts),
     assert_gpred(GPred),
     fail.
ground_all_var_atom(HPred).

ground_all_var_atom0([],[]).
ground_all_var_atom0([HVars|TV],[HSorts|TS]):-
     subsorts(HSorts,Subsorts),
     split_prim_noprim(Subsorts,PSorts,NPSorts),
     member(LS,PSorts),
     objects(LS,Objls),
     member(HVars,Objls),
     ground_all_var_atom0(TV,TS).
ground_all_var_atom0([HVar|TV],[HASorts|TS]):-
     objects(Sorts,Objls),
     member(HASorts,Objls),
     HVar=HASorts,
     ground_all_var_atom0(TV,TS).

ground_all_var(GPred,HPred):-
     GPred=..[Name|Vars],
     HPred=..[Name|Sorts],
     ground_all_var0(Vars,Sorts),
     not(inconsistent_constraint([GPred])),
     assert_gpred(GPred),
     fail.
ground_all_var(GPred,HPred).

ground_all_var0([],[]).
ground_all_var0([HVars|TV],[HSorts|TS]):-
     objects(HSorts,Objls),
     member(HVars,Objls),
     ground_all_var0(TV,TS).
ground_all_var0([HVars|TV],[HSorts|TS]):-
     subsorts(HSorts,Subsorts),
     split_prim_noprim(Subsorts,PSorts,NPSorts),
     member(LS,PSorts),
     objects(LS,Objls),
     member(HVars,Objls),
     ground_all_var0(TV,TS).

% assert grounded predicates with related upper level predicates

assert_gpred(GPred):-
     functor(GPred,FF,NN),
     functor(UPred,FF,NN),
     assert_gpred0(GPred,UPred),!.

assert_gpred0(GPred,UPred):-
     GPred=..[Name|PSorts],
     UPred=..[Name|Vars],
     find_all_upper(Vars,PSorts),
     assert(gpred(UPred,GPred)),
     fail.
assert_gpred0(GPred,UPred).

find_all_upper([],[]).
find_all_upper([HVars|TV],[HSorts|TS]):-
     uppersorts(HSorts,Upsorts),
     member(HVars,Upsorts),
     find_all_upper(TV,TS).
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


% -------------------------------------------

not(X):- \+X.
member(X,[X|_]).
member(X,[_|L]) :- member(X,L).
append([],L,L):-!.
append([H|T],L,[H|Z]) :- append(T,L,Z),!.
 
% subtract(A,B,C): subtract B from A
% -------------------------------------
subtract([],_,[]):-!.
subtract([A|B],C,D) :-
        member(A,C),
        subtract(B,C,D),!.
subtract([A|B],C,[A|D]) :-
        subtract(B,C,D),!.

% find the lowest score of levels
lowest_score(LScore):-
     setof(level(Score,A),Score^level(Score,A),D),
     sort(D,[level(LScore,_)|SD]),!.

write_level(I):-
     I < 50,
     level(A,P),
     I1 is I +1,
%     write(A),nl,
%     write(P),nl,
     write_level(I1).
write_level(I).
