/** <module> htncode
% Provides a prolog database *env*
%
%  
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Denton, TX 2005, 2010, 2014 
% Dec 13, 2035
%
*/
:-module(hyhtn_new,[]).

end_of_file.

/* ***********************************/
/* Douglas Miles 2005, 2010, 2014 */
/* Denton, TX */
/* ***********************************/

% [Required] Load the Logicmoo Library Utils
:- ensure_loaded(library(logicmoo_common)).
:- ensure_loaded(library(planner_api)).
:- use_module(library(logicmoo_planner)).

:- kb_shared(baseKB:mpred_prop/3).
:- multifile(user:push_env_ctx/0).
:- dynamic(user:push_env_ctx/0).
:- ensure_loaded(library(logicmoo/util_structs)).
:- ensure_loaded(library(logicmoo/util_bb_env)).
:- prolog_load_context(file,File),ain(user:env_source_file(File)).

:-op(500,fx,env_call).


/* gipohyhtn.pl */
/* HyHTN planning: do preprocess first  */
/* make all method and operators primitive */

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
 /* gipohyhtn.pl */

/* htncode.pl */

/* ***********************************/
/* Donghong Liu */
/* University of Huddersfield */
/* September 2002 */
/* **********************************/

:-use_module(library(system)).

/*********************** initialisation**************/
:- dynamic op_num/1. 
:- dynamic initial_node/1, final_node/1.
:- dynamic methodC/7, operatorC/5, my_stats/1.
:- dynamic sum/1.
%%:- dynamic node/7,solved_node/5.
:- dynamic tp_node/6,tn/6.
:- dynamic solved_node/2.
:- dynamic closed_node/6.
:- dynamic gsubstate_classes/3.
:- dynamic current_num/2.
:- dynamic produce/4,goal_related/1. %%,achieved_goal/1.

%---------------------structure--------------------
% for whole search:
% node(Nodeid, Precond, Decomps, Temp, Statics)
% tn(Tnid, Name, Precond, Postcond,Temp, Decomps)
% tn is a full expanded node, it has fixed decomps and postcondtion
% step(HPid,ActName,Precond,Postcond,Expansion)
% for fwsearch achieve(Goal) only:
% tp_goal(Pre,Goal,Statics)
% goal_related(se(Sort,Obj,SEpre),Opls,LengthToGoal)
% tp_node(TPid,Pre,Statics,from(Parentid),Score,Steps)
% closed_node(TPid,Pre,from(Parentid),Score,Steps)
%---------------------structure end--------------------


:- dynamic gsstates/3, gpred/2.
%incr_op_num:- retact(op_num,X,X+1).
%set_op_num(X):-flag(op_num,_,X).
%current_op_num(X):- flag(op_num,X,X).


% for boot..
:- dynamic kill_file/1,solution_file/1.
%
%:- unknown(error,fail).
:- op(100,xfy,'=>').

op_num(0).
my_stats(0).
solution_file(fred).

%:-set_prolog_flag(unknown,fail).

:- discontiguous hyhtn_new_solve/1.

hyhtn_new_solve(Id) :-
	htn_task(Id,Goal,Init),
	pprint_ecp(red,htn_task(Id,Goal,Init)),
	once(solve_once(Id,Goal,Init)).

solve_once(Id,Goal,Init) :-	
	planner_interface(Goal,Init,Sol,_),
	solution_file(F),
	tell(F),
	write('\nTASK htn_task '),write(Id),nl,
	write('SOLUTION'),nl,
	display_sol(Sol),
	write('END FILE'),nl,nl,
%	reverse(TNLst,TNForward),
%	display_details(TNForward),
	write('END PLANNER RESULT'),
	told,
	tell(user),
	write('\nTASK htn_task '),write(Id),nl,
	write('SOLUTION'),nl,
	display_sol(Sol),
	write('END FILE'),nl,nl,
	clean_problem.

hyhtn_new_solve(Id) :- 
    !,
    planner_task(Id,Goal,Init),
	pprint_ecp(cyan,planner_task(Id,Goal,Init)),
	planner_interface(Goal,Init,Sol,_),
        show_result_and_clean(_,Id,Sol,[Id]).
        		

hyhtn_new_solve(Id) :-
	planner_task(Id,Goal,Init),
	pprint_ecp(green,planner_task(Id,Goal,Init)),
	once(time(solve_once_pt(Id,Goal,Init))).
	

solve_once_pt(Id,Goal,Init) :-	
	planner_interface(Goal,Init,Sol,_),
	solution_file(F),
	tell(F),
	write('\nTASK planner_task '),write(Id),nl,
	write('SOLUTION'),nl,
	display_sol(Sol),
	write('END FILE'),nl,nl,
%	reverse(TNLst,TNForward),
%	display_details(TNForward),
	write('END PLANNER RESULT'),
	told,
tell(user),
	write('\nTASK planner_task '),write(Id),nl,
	write('SOLUTION'),nl,
	display_sol(Sol),
	write('END FILE'),nl,nl,
	clean_problem.

hyhtn_new_solve(_Id) :- !.
		

hyhtn_new_solve(Id) :-
 
	htn_task(Id,Goal,Init),
	once(planner_interface(Goal,Init,Sol,_)),
%	solution_file(F),
%	gensym_num(F,Id,TRF),
%	tell(user),
%	write('TASK '),write(Id),nl,
%	write('SOLUTION'),nl,
%	display_sol(Sol),
%	write('END FILE'),nl,
%	told,
	clean_problem.
	
display_sol([]).
display_sol([H|T]) :-
	write(H),
	nl,
	display_sol(T).


clean_problem:-
	retractall(op_num(_)),
	retractall(current_num(_,_)),
	retractall(node(_,_,_,_,_)),
	retractall(tn(_,_,_,_,_,_)),
	retractall(gpredicates(_,_)),
	retractall(gsubstate_classes(_,_,_)),
	retractall(methodC(_,_,_,_,_,_,_)),
	retractall(operatorC(_,_,_,_,_)),
	retractall(goal_related(_)),
	retractall(produce(_,_,_,_)),
	retractall(solved_node(_,_)),
	retractall(tp_node(_,_,_,_,_,_)),
	retractall(closed_node(_,_,_,_,_,_)),
	retractall(score_list(_)),
	assert(op_num(0)),
	assert(my_stats(0)).

planner_interface(G,I, SOLN,NODES):-
    time_as('SETUP',(
    ground_hierarchy,
	change_op_representation,
	find_relate_state,
        retract(op_num(_)),
        assert(op_num(0)),
        true)),
      time_as('SOLVE',(
        statistics(runtime,[_,Time]),
        (retract(my_stats(_)) ; true),
        assert(my_stats(Time)),
        make_problem_into_node(I, G, Node),
        assert(Node),
	start_solve(SOLN,NODES))).
planner_interface(G,I, SOLN,NODES):-
	tell(user),nl,write('failure in initial node'),!.


	
/******************** HP ADT *********************/

% node(Name, Precond ,Decomps, Temp, Statics)
getN_name(node(Name, _, _, _,_),  Name).
getN_pre(node(_,Pre, _, _, _),  Pre).
getN_decomp(node(_, _, Decomp,_,_),  Decomp).
getH_temp(node(_, _, _,Temps, _),  Temps).
getN_statics(node(_,_,_,_,Statics),  Statics).

%Ron  21/9/01 - Try to give a closedown method
start_solve(SOLN,NODES):-
	kill_file(Kill),
	file_exists(Kill).
%	write('Found kill file'),nl.

start_solve(Sol,OPNUM):-
   retract(final_node(Node)),
   retractall(current_num(_,_)),
   getN_statics(Node,Statics),
   statics_consist(Statics),
   extract_solution(Node,Sol,SIZE),
   statistics(runtime,[_,CP]),
   TIM is CP/1000,
   retract(op_num(OPNUM)),
   assert(op_num(0)),
   nl, nl, write('CPU Time = '),write(CP),nl,
   write('TIME TAKEN = '),write(TIM),
   write(' SECONDS'),nl,
   write('Solution SIZE = '),write(SIZE),nl,
   write('Operator Used = '),write(OPNUM),nl,
   write('Solution:\n '),writeq(Sol),nl,
   write('***************************************'),
   assert(time_taken(CP)),  
   assert(soln_size(SIZE)),
   assert(plan_used(OPNUM)),
   retractall(tn(_,_,_,_,_,_)),!.

start_solve(Sol,OPNUM):-
   select_node(Node),
%   nl,write('processing '),write(Node),nl,
            % expand/prove its hps
   process_node(Node),
   start_solve(Sol,OPNUM).
start_solve(Sol,OPNUM):-
    tell(user), write('+++ task FAILED +++'),
    clean_problem.

/******************************** MAIN LOOP **********/

% expand a node..
process_node(Node) :-
   getN_name(Node,  Name),
   getN_pre(Node, Pre),
   getN_decomp(Node, Dec),
   getH_temp(Node, Temps),
   getN_statics(Node, Statics),
   expand_decomp(Dec,Pre,Post,Temps,Temp1,Statics,Statics1,Dec1),
   statics_consist(Statics),
   assert_node(Name,Pre,Dec1,Temp1,Statics1).

assert_node(Name,Pre,Decomp,Temp,Statics):-
   all_HP_expanded(Decomp),
   assert(final_node(node(Name,Pre,Decomp,Temp,Statics))),!.
assert_node(Name,Pre,Dec,Temp,Statics):-
   gensym_special(root,SYM),
   assert(node(SYM,Pre,Dec,Temp,Statics)),!.

/************ expand every step *********************/
expand_decomp([],Post,Post,Temp,Temp,Statics,Statics,[]):-!.
%0-1 if the step has expand already, get the state change, go to next
expand_decomp([step(HPid,Name,Pre0,Post0,exp(TN))|Decomp],Pre,Post,Temp,Temp1,Statics,Statics1,[step(HPid,Name,State1,Post1,exp(TN))|Decomp1]):-
   state_achieved(Pre0,Pre,State1),
   state_change(State1,Pre0,Post0,Post1),
   statics_consist(Statics),
   expand_decomp(Decomp,Post1,Post,Temp,Temp1,Statics,Statics1,Decomp1),!.

%0-2. if it is an achieve action
expand_decomp([step(HPid,ACH,Pre0,Post0,unexp)|Decomp],Pre,Post,Temp,Temp1,Statics,Statics1,Decomp1):-
   ACH=..[achieve|_],
   state_achieved(Pre0,Pre,State),
   statics_consist(Statics),
   expand_decomp_ach([step(HPid,ACH,State,Post0,unexp)|Decomp],State,Post,Temp,Temp1,Statics,Statics1,Decomp1),!.
/*
% 1. if the step's Name is match a exist TN also Pre meet, return TN
% tn(Tn0,Name,Pre0,Post0,Temp0,Decomp)is a task network
% which was generated by previous process,
% it has been decomposed to ordered primitive operators
% and has all states changes defiend
expand_decomp([step(HPid,Name,undefd,undefd,unexp)|Decomp],Pre,Post,Temp,Temp1,Statics,Statics1,[step(HPid,Name,Pre0,Post0,exp(Tn0))|Decomp1]):-
   tn(Tn0,Name,Pre0,Post0,Temp0,Decomp0),
   state_achieved(Pre0,Pre,State1),
   statics_consist(Statics),
   state_change(State1,Pre0,Post0,State),
%   nl,write('step '),write(HPid),
   retract(op_num(N)),
   N1 is N+1,
   assert(op_num(N1)),
%   write('can be expand by tn '),write(Tn0),nl,
   expand_decomp(Decomp,State,Post,Temp,Temp1,Statics,Statics1,Decomp1),!.
*/
% 2. if HP's name meet an method,
% and it's precondition and achieve goals are achieved
% expand it and make it to that TNs

%% when expanding by methods:
% because the conditions are not ground at upper level
% so it have to be push down to the ground level to
% find all the state changes
expand_decomp([step(HPid,Name,undefd,undefd,unexp)|Decomp],Pre,Post,Temp,Temp1,Statics,Statics1,[step(HPid,Name,Pre0,Post0,exp(TN))|Decomp1]):-
   methodC(Name,Pre0,Post0,Statics0,Temp0,achieve(ACH),Dec0),
   append_cut(Statics0,Statics,Statics2),
   statics_consist(Statics2),
   remove_unneed(Statics2,[],Statics3),
   state_achieved(Pre0,Pre,State1),
%   state_achieved(ACH,State1,State11),
   statics_consist(Statics3),
   apply_method1(HPid,TN,Name,State1,Pre0,ACH,Post0,State,Statics3,Temp0,Dec0),
%   apply_method(HPid,TN,Name,State11,Pre0,Post0,State,Statics21,Temp0,Dec0),
% changed here for if there are too many unordered tasks
% apply_method1 is directly apply method even it's achieve goal not achieved
%   nl,write('step '),write(HPid),
   retract(op_num(N)),
   N1 is N+1,
   assert(op_num(N1)),
%   write('can be expand by method '),write(Name),nl,
   expand_decomp(Decomp,State,Post,Temp,Temp1,Statics2,Statics1,Decomp1),!.








% 3. if HP's name and it's Pre meet an operator, return operator's name
expand_decomp([step(HPid,Name,undefd,undefd,unexp)|Decomp],Pre,Post,Temp,Temp1,Statics,Statics1,[step(HPid,Name,Pre0,Post0,exp(Name))|Decomp1]):-
   operatorC(Name,Pre0,Post0,Cond,Statics0),
   append_cut(Statics0,Statics,Statics2),
   statics_consist(Statics2),
   remove_unneed(Statics2,[],Statics21),
   state_achieved(Pre0,Pre,State1),
   state_change(State1,Pre0,Post0,State2),
   cond_state_change(State2,Cond,State),
%   nl,write('step '),write(HPid),
   statics_consist_instance(Statics0),
%   write('can be expand by operator '),write(Name),nl,
   retract(op_num(N)),
   N1 is N+1,
   assert(op_num(N1)),
   expand_decomp(Decomp,State,Post,Temp,Temp1,Statics21,Statics1,Decomp1),!.

% 4. get another step which matchs and not after it before it to give a try
expand_decomp([step(HP,N,Pre0,Post0,unexp)|Decomp],Pre,Post,Temp,Temp1,Statics,Statics1,Decomp1):-  
   get_another_step(step(HP2,N2,Pre2,Post2,Exp),Pre,Statics,
                                  HP,Temp,Temp2,Decomp,Decomp2),
   expand_decomp([step(HP2,N2,Pre2,Post2,Exp),step(HP,N,Pre0,Post0,unexp)|Decomp2],Pre,Post,Temp2,Temp1,Statics,Statics1,Decomp1).

/*
% 5. if HP's name meet an method,
% and it's precondition  achieved
% but failed to match achieve_goals
% expand it and make it to that TNs
expand_decomp([step(HPid,Name,undefd,undefd,unexp)|Decomp],Pre,Post,Temp,Temp1,Statics,Statics1,[step(HPid,Name,Pre0,Post0,exp(TN))|Decomp1]):-
   methodC(Name,Pre0,Post0,Statics0,Temp0,achieve(ACH),Dec0),
   append_cut(Statics0,Statics,Statics2),
   statics_consist(Statics2),
   remove_unneed(Statics2,[],Statics3),
   state_achieved(Pre0,Pre,State1),
   statics_consist(Statics3),
   apply_method1(HPid,TN,Name,State1,Pre0,ACH,Post0,State,Statics3,Temp0,Dec0),
   % need to expand achieve goals first
   expand_decomp(Decomp,State,Post,Temp,Temp1,Statics3,Statics1,Decomp1),!.

% 6. if all above failed
% get another step which matchs and not after it before it to give a try
expand_decomp([step(HP,N,Pre0,Post0,unexp)|Decomp],Pre,Post,Temp,Temp1,Statics,Statics1,Decomp1):-  
   get_another_step(step(HP2,N2,Pre2,Post2,Exp),Pre,Statics,
                                  HP,Temp,Temp2,Decomp,Decomp2),
   expand_decomp([step(HP2,N2,Pre2,Post2,Exp),step(HP,N,Pre0,Post0,unexp)|Decomp2],Pre,Post,Temp2,Temp1,Statics,Statics1,Decomp1).
*/
% Else fail expand

% get another step which is not after it before it
get_another_step(A,Pre,Statics,HP,Temp,Temp1,[],Dec2):-fail.
get_another_step(step(HP2,Name2,Pre2,Post2,Exp),Pre,Statics,HP,Temp,[before(HP2,HP)|Temp],Dec,Dec2):-
   member(step(HP2,Name2,Pre2,Post2,Exp),Dec),
   not(necessarily_before(HP,HP2, Temp)),
   state_achieved(Pre2,Pre,_),
   statics_consist(Statics),
   list_take(Dec,[step(HP2,Name2,Pre2,Post2,Exp)],Dec2).

% expand the achieve goal
% 1.if the ACH is achieved already
%   remove it from decomposion and do the next
expand_decomp_ach([step(HPid,ACH,Pre,Post0,unexp)|Decomp],Pre,Post,Temp,Temp1,Statics,Statics1,Decomp1):-
   state_achieved(Post0,Pre,State),
   statics_consist(Statics),
%   nl,write('step '),write(HPid),
%   write('is already achieved'),nl,
   remove_temp(Temp,HPid,Temp,Temp2),
   expand_decomp(Decomp,State,Post,Temp2,Temp1,Statics,Statics1,Decomp1),!.

% 2.take out the already achieved states before expanding
expand_decomp_ach([step(HPid,ACH,Pre,Post0,unexp)|Decomp],Pre,Post,Temp,Temp1,Statics,Statics1,[step(HPid,ACH,Pre,Post0,exp(TN))|Decomp1]):-
   expand_hp(HPid,TN,ACH,Pre,Post0,State,Statics,Statics2),
   expand_decomp(Decomp,State,Post,Temp,Temp1,Statics2,Statics1,Decomp1),!.

% directly achieve HP's Pre and Post by operator,method or tn
expand_hp(HPid,TN,ACH,Pre,Post,State,Statics,Statics1):-
   direct_expand_hp(HPid,TN,ACH,Pre,Post,State,Statics,Statics1),!.
% else, nothing directly can achieve HP's Pre and Post
% assert a temporely node for forward search
% tp_node(Name, Precond, Postcond, Statics,Score,Temp,Decomps)
expand_hp(HPid,TN,ACH,Pre,Post,State,Statics,Statics1):-
   make_to_steps1(Pre,Post,Steps),
   gensym_special(tp,TP),
   get_all_related_goal(Post),
   get_score(100,[],Steps,Score),
   assert(tp_node(TP,Pre,Post,Statics,Score,Steps)),
   fwsearch(TN,Statics1,State),
   clean_temp_nodes.

% make the achieve goal states [se(..),se(..),..] to separate steps
% remove states that have been achieved by precondition
% by same them for declobber
make_to_steps1(Pre,[],[]):-!.
make_to_steps1(Pre,[se(Sort,Obj,SE)|Post],Steps):-
   state_achieved([se(Sort,Obj,SE)],Pre,_),
   make_to_steps1(Pre,Post,Steps),!.
make_to_steps1(Pre,[se(Sort,Obj,SE)|Post],[step(HP,achieve(se(Sort,Obj,SE)),undefd,[se(Sort,Obj,SE)],unexp)|Steps]):-
   objects(Sort,Objls),%Sort is primitive 
   gensym_special(hp,HP),
   make_to_steps1(Pre,Post,Steps),!.
make_to_steps1(Pre,[se(Sort,Obj,SE)|Post],[step(HP,achieve(se(Sort,Obj,SE)),undefd,[se(Sort,Obj,SE)],unexp)|Steps]):-
   var(Obj),
   gensym_special(hp,HP),
   make_to_steps1(Pre,Post,Steps),!.
make_to_steps1(Pre,[se(SortN,Obj,SE)|Post],[step(HP,achieve(se(PSort,Obj,SE)),undefd,[se(PSort,Obj,SE)],unexp)|Steps]):-
   not(var(Obj)),
   find_prim_sort(SortN,PSortls),
   member(PSort,PSortls),
   objects(PSort,Objls),
   member(Obj,Objls),
   gensym_special(hp,HP),
   make_to_steps1(Pre,Post,Steps),!.


% get all the states that can achieve the goal state
get_all_related_goal(Goal):-
    retractall(goal_related(_)),
    assert(goal_related(Goal)),
    get_all_related_goal1(Goal,States),
    retractall(goal_related(_)),
    assert(goal_related(States)),!.
%    tell(user),write(States),nl,told,!.

get_all_related_goal1([],States):-
    setof(State,goal_related(State),States1),
    set_flatten(States1,[],States),!.
get_all_related_goal1([se(Sort,Obj,SE)|Rest],States):-
    produce(se(Sort,Obj,SE1),A,OPre,ST),
    state_achieved([se(Sort,Obj,SE)],[se(Sort,Obj,SE1)],_),
    retract(goal_related(List)),
    set_append_e(List,OPre,List1),
    assert(goal_related(List1)),
    fail.
get_all_related_goal1([se(SortN,Obj,SE)|Rest],States):-
    nonvar(Obj),
    objects(Sort,Objls),
    not(not(member(Obj,Objls))),
    produce(se(Sort,Obj,SE1),A,OPre,ST),
    state_achieved([se(Sort,Obj,SE)],[se(Sort,Obj,SE1)],_),
    retract(goal_related(List)),
    set_append_e(List,OPre,List1),
    assert(goal_related(List1)),
    fail.
get_all_related_goal1([se(SortN,Obj,SE)|Rest],States):-
    var(Obj),
    find_prim_sort(SortN,Sortls),
    member(Sort,Sortls),
    produce(se(Sort,Obj,SE1),A,OPre,ST),
    state_achieved([se(Sort,Obj,SE)],[se(Sort,Obj,SE1)],_),
    retract(goal_related(List)),
    set_append_e(List,OPre,List1),
    assert(goal_related(List1)),
    fail.
get_all_related_goal1([se(Sort,Obj,SE)|Rest],States):-
    get_all_related_goal1(Rest,States).

%   find out the difference of Pre and Post0 to expand
% if only one Obj in Post, don't do check.
take_out_achieved(Pre,[se(Sort,Obj,SE)],[],[se(Sort,Obj,SE)]):-!.
take_out_achieved(Pre,[],Post,Post):-!.
% Pre conditions are all instantiated states
% only Post conditions have variables
take_out_achieved(Pre,[se(Sort,Obj,ST)|Post],Post0,Post1):-
    var(Obj),
    append_dcut(Post0,[se(Sort,Obj,ST)],Post2),
    take_out_achieved(Pre,Post,Post2,Post1),!.
take_out_achieved(Pre,[se(Sort,Obj,ST)|Post],Post0,Post1):-
    member(se(Sort,Obj,ST1),Pre),
    append_diff(Sort,Obj,ST,ST1,Post0,Post2),
    list_take(Pre,[se(Sort,Obj,ST1)],Pre2),
    take_out_achieved(Pre2,Post,Post2,Post1),!.
 
% append_dcut only the different one
append_diff(Sort,Obj,ST,ST1,Post0,Post0):-
    not_conflict(Sort,Obj,ST,ST1,STN),!.
append_diff(Sort,Obj,ST,ST1,Post0,Post1):-
    append_dcut(Post0,[se(Sort,Obj,ST)],Post1),!.
% ---------------------------------------------------
 
%1. if an achieve action meets an TN Pre and post meet
direct_expand_hp(HPid,Tn0,ACH,Pre,Post,Pre1,Statics,Statics):-
   tn(Tn0,Name,Pre0,Post0,Temp0,Decomp0),
   state_achieved(Pre0,Pre,State1),
   state_achieved(Post,Post0,_),
   state_change(State1,Pre0,Post0,Pre1),
   statics_consist(Statics),
%   nl,write('step '),write(HPid),
    retract(op_num(N)),
    N1 is N+1,
    assert(op_num(N1)),!.
%    write('can be expand by tn '),write(Tn0),nl,!.

%2. if an action's name meets is an operator's,
%    return 
direct_expand_hp(HPid,Name,Name,Pre,Post,State,Statics,Statics1):-
   operatorC(Name,Pre0,Post0,Cond,Statics0),
   append_cut(Statics0,Statics,Statics1),
   statics_consist(Statics11),
   remove_unneed(Statics11,[],Statics1),
   post_instant(Post0,Cond,Statics1,Post),
   state_achieved(Pre0,Pre,State1),
   state_change(State1,Pre0,Post0,State2),
   cond_state_change(State2,Cond,State3),
   state_achieved(Post,State3,State),
   statics_consist_instance(Statics0),
%   nl,write('step '),write(HPid),
%   write('can be expand by operator '),write(Name),nl,
   retract(op_num(N)),
   N1 is N+1,
   assert(op_num(N1)),!.

%2. if an achieve action meets an operator's Pre and post meet,
%    return 
direct_expand_hp(HPid,Name,ACH,Pre,Post,State,Statics,Statics1):-
   operatorC(Name,Pre0,Post0,Cond,Statics0),
   append_cut(Statics0,Statics,Statics2),
   statics_consist(Statics2),
   remove_unneed(Statics2,[],Statics1),
   post_instant(Post0,Cond,Statics1,Post),
   state_achieved(Pre0,Pre,State1),
   state_change(State1,Pre0,Post0,State2),
   cond_state_change(State2,Cond,State3),
   state_achieved(Post,State3,State),
   statics_consist(Statics1),
%   nl,write('step '),write(HPid),
%   write('can be expand by operator '),write(Name),nl,
   retract(op_num(N)),
   N1 is N+1,
   assert(op_num(N1)),!.

%3. if an achieve action meets a method's pre and post,
%    expand it and make it to that TNs
direct_expand_hp(HPid,TN,ACH,Pre,Post,State1,Statics,Statics1):-
   methodC(Name,Pre0,Post0,Statics0,Temp0,achieve(ACH0),Dec0),
   append_cut(Statics0,Statics,Statics2),
   statics_consist(Statics2),
   remove_unneed(Statics2,[],Statics1),
   post_instant(Post0,[],Statics1,Post),
   state_achieved(Pre0,Pre,Pre1),
   state_achieved(Post,Post0,Post01),
   statics_consist(Statics1),
   apply_method1(HPid,TN,Name,Pre1,Pre0,ACH0,Post01,State1,Statics1,Temp0,Dec0),!.

% apply_method/10: apply method in the HP
% when the method name's precondition and achieve goals are achieved
% no extra operators are bring in while expansion
apply_method(HPid,TN,Name,Pre,Pre0,Post0,State,Statics,Temp,Dec):-
   make_dec0(HPid,Dec,Temp,Dec0,Temp0),
   expand_method(Dec0,Pre,Post,Temp0,Temp1,Statics,Statics1,Dec1),
   state_achieved(Post0,Post,State),
   statics_consist(Statics),
%   nl,write('step '),write(HPid),
%   write('can be expand by method '),write(Name),nl,
   retract(op_num(N)),
   N1 is N+1,
   assert(op_num(N1)),
   make_tn(TN,Name,Pre,State,Temp1,Dec1),!.

make_dec0(HPid,Dec,Temp,Dec0,Temp0):-
    make_dec01(HPid,1,Dec,Dec0),
    change_temp(HPid,Temp,[],Temp0),!.

make_dec01(HPid,_,[],[]):-!.
make_dec01(HPid,Num,[HDec|TDec],[step(STID,HDec,undefd,undefd,unexp)|TDec0]):-
   operatorC(HDec,_,_,_,_),
   gensym_num(HPid,Num,STID),
   Num1 is Num + 1,
   make_dec01(HPid,Num1,TDec,TDec0).
make_dec01(HPid,Num,[HDec|TDec],[step(STID,HDec,undefd,undefd,unexp)|TDec0]):-
   methodC(HDec,_,_,_,_,_,_),
   gensym_num(HPid,Num,STID),
   Num1 is Num + 1,
   make_dec01(HPid,Num1,TDec,TDec0).

change_temp(HPid,[],Temp2,Temp2):-!.
change_temp(HPid,[before(N1,N2)|Temp],Temp2,[before(ST1,ST2)|Temp0]):-
   gensym_num(HPid,N1,ST1),
   gensym_num(HPid,N2,ST2),
   change_temp(HPid,Temp,Temp2,Temp0),!.

% expand_method/8 expand method without bring in any other operators
% that is: all achieve goals are achieved
expand_method([],Post,Post,Temp,Temp,Statics,Statics,[]):-!.
expand_method([step(HPid,Name,undefd,undefd,unexp)|Decomp],Pre,Post,Temp,Temp1,Statics,Statics1,[step(HPid,Name,Pre0,Post0,exp(TN))|Decomp1]):-
   tn(TN,Name,Pre0,Post0,Temp0,Decomp0),
   state_achieved(Pre0,Pre,State1),
   state_change(State1,Pre0,Post0,State),
   statics_consist(Statics),
%   nl,write('step '),write(HPid),
    retract(op_num(N)),
    N1 is N+1,
    assert(op_num(N1)),
%    write('can be expand by tn '),write(TN),
   expand_method(Decomp,State,Post,Temp,Temp1,Statics,Statics1,Decomp1).
expand_method([step(HPid,Name,undefd,undefd,unexp)|Decomp],Pre,Post,Temp,Temp1,Statics,Statics1,[step(HPid,Name,Pre0,Post0,exp(Name))|Decomp1]):-
   operatorC(Name,Pre0,Post0,Cond,Statics0),
   append_cut(Statics0,Statics,Statics2),
   statics_consist(Statics2),
   remove_unneed(Statics2,[],Statics21),
   post_instant(Post0,Cond,Statics21,Post0),
   state_achieved(Pre0,Pre,State1),
   state_change(State1,Pre0,Post0,State2),
   cond_state_change(State2,Cond,State3),
   state_achieved(Post0,State3,State), 
   statics_consist_instance(Statics0),
%   nl,write('step '),write(HPid),
%   write('can be expand by operator '),write(Name),nl,
    retract(op_num(N)),
    N1 is N+1,
    assert(op_num(N1)),
   expand_method(Decomp,State,Post,Temp,Temp1,Statics21,Statics1,Decomp1).
expand_method([step(HPid,Name,undefd,undefd,unexp)|Decomp],Pre,Post,Temp,Temp1,Statics,Statics1,[step(HPid,Name,Pre0,Post0,exp(TN))|Decomp1]):-
   methodC(Name,Pre0,Post0,Statics0,Temp0,ACH,Dec0),
   append_cut(Statics0,Statics,Statics2),
   statics_consist(Statics2),
   remove_unneed(Statics2,[],Statics21),
   post_instant(Post0,[],Statics21,Post0),
   state_achieved(Pre0,Pre,State1),
   statics_consist(Statics21),
   apply_method(HPid,TN,Name,State1,Pre0,Post0,State,Statics21,Temp0,Dec0),
   expand_method(Decomp,State,Post,Temp,Temp1,Statics21,Statics1,Decomp1).
% --------------------end of apply_method/10---------------------

% apply_method1/11: apply method in the HP
% when the method name's precondition achieved
% but need bring in operators while expansion
apply_method1(HPid,TN,Name,Pre,Pre0,ACH,Post0,State,Statics,Temp,Dec):-
   make_dec1(HPid,Pre,ACH,Statics,Temp,Dec,Temp2,Dec2),
   expand_decomp(Dec2,Pre,Post,Temp2,Temp1,Statics,Statics1,Dec1),
   state_achieved(Post0,Post,State),
%   nl,write('step '),write(HPid),
%   write('can be expand by method '),write(Name),nl,
   retract(op_num(N)),
   N1 is N+1,
   assert(op_num(N1)),
   make_tn(TN,Name,Pre,State,Temp1,Dec1),!.


make_dec1(HPid,Pre,ACH,Statics,Temp,Dec,Temp1,Dec1):-
   state_achieved(ACH,Pre,_),
   make_dec01(HPid,1,Dec,Dec1),
   change_temp(HPid,Temp,[],Temp1).
make_dec1(HPid,Pre,ACH,Statics,Temp,Dec,[before(STID0,STID1)|Temp1],[step(STID0,achieve(ACH),Pre,ACH,unexp)|Dec1]):-
   gensym_num(HPid,0,STID0),
   gensym_num(HPid,1,STID1),
   make_dec01(HPid,1,Dec,Dec1),
   change_temp(HPid,Temp,[],Temp1),!.
% --------------------end of apply_method1/11---------------------

% forward search for operators can't directly solved
fwsearch(TN,ST,State):-
   retract(solved_node(ST,step(HP,Name,Pre,State,exp(TN)))).
fwsearch(TN,ST,State):-
   select_tnode(tp_node(TP,Pre,Post,Statics,Score,Dec)),
   assert(closed_node(TP,Pre,Post,Statics,Score,Dec)),
   expand_node(Statics,Statics1,Pre,Post,Dec,[],Dec1),
   assert_tnode(TP,Pre,Post,Statics1,Score,Dec1),
   solved_node(_,_),%expand every possible way until find solution
   fwsearch(TN,ST,State).
fwsearch(TN,ST,State):-
   tp_node(TP,Pre,Post,Statics,Score,Dec),
   fwsearch(TN,ST,State).

clean_temp_nodes:-
   retractall(current_num(tp,_)),
   retractall(tp_node(_,_,_,_,_,_)),
   retractall(closed_node(_,_,_,_,_,_)),!.

% expand all way possible to achieve the Post
expand_node(Statics,Statics,Pre,Post,[],List,List):-
   state_achieved(Post,Pre,_),!.
expand_node(Statics,Statics1,Pre,Post,[step(HP,Name,_,Post0,unexp)|Rest],List,Dec1):-
   state_achieved(Post0,Pre,_),
   expand_node(Statics,Statics1,Pre,Post,Rest,List,Dec1),!.
expand_node(Statics,Statics1,Pre,Post,[step(HP,Name,_,Post0,unexp)|Rest],List,Dec1):-
   direct_expand_hp(HP,TN,Name,Pre,Post0,State,Statics,Statics2),
   append_dcut(List,[step(HP,Name,Pre,State,exp(TN))],List2),
   remove_achieved_rest(State,Statics2,Rest,Rest1),
   make_to_steps(State,Post,Steps,Rest1),
   append_dcut(Rest1,Steps,Rest2),
   expand_node(Statics2,Statics1,State,Post,Rest2,List2,Dec1).
expand_node(Statics,Statics1,Pre,Post,[step(HP,Name,_,Post0,unexp)|Rest],List,Dec1):-
   apply_op(Statics,Statics1,[step(HP,Name,Pre,Post0,unexp)|Rest],List,Dec1).
expand_node(Statics,Statics1,Pre,Post,[step(HP,Name,Pre0,Post0,exp(TN))|TDec],List,Dec1):-
   append_dcut(List,[step(HP,Name,Pre0,Post0,exp(TN))],List2),
   expand_node(Statics,Statics1,Post0,Post,TDec,List2,Dec1),!.

remove_achieved_rest(State,Statics,[],[]):-!.
remove_achieved_rest(State,Statics,[step(HP,Name,_,Post0,unexp)|Rest],Rest1):-
   state_achieved(Post0,State,_),
   remove_achieved_rest(State,Statics,Rest,Rest1),!.
remove_achieved_rest(State,Statics,[HP|Rest],[HP|Rest1]):-
   remove_achieved_rest(State,Statics,Rest,Rest1),!.

/* add an operator that can achieve the goal related states*/
% This is only applied when it can't be directly achieved
apply_op(Statics,Statics1,[step(HP,Name,Pre,[se(Sort,Obj,SE)],unexp)|Rest],List,Dec):-
    produce(se(Sort,Obj,SE1),OP,OPre,ST),
    append_cut(Statics,ST,Statics2),
    statics_consist(Statics2),
    remove_unneed(Statics2,[],Statics1),
    state_achieved([se(Sort,Obj,SE)],[se(Sort,Obj,SE1)],_),
    instantiate(OPre,Statics1),
    not(member(step(_,OP,_,_,_),Rest)),
    make_to_steps(Pre,OPre,Steps,Rest),
    change_head_state(Pre,Steps,Steps1),
    append_dcut(List,Steps1,List2),
    append_dcut(List2,[step(HP,OP,undefd,[se(Sort,Obj,SE)],unexp)|Rest],Dec).
%    tell(user),write('+'),told.
apply_op(Statics,Statics1,[step(HP,Name,Pre,[se(SortN,Obj,SE)],unexp)|Rest],List,Dec):-
    find_prim_sort(SortN,PSortls),
    member(Sort,PSortls),
    produce(se(Sort,Obj,SE1),OP,OPre,ST),
    append_cut(Statics,ST,Statics2),
    statics_consist(Statics2),
    remove_unneed(Statics2,[],Statics1),
    state_achieved([se(Sort,Obj,SE)],[se(Sort,Obj,SE1)],_),
    instantiate(OPre,Statics1),
    not(member(step(_,OP,_,_,_),Rest)),
    make_to_steps(Pre,OPre,Steps,Rest),
    change_head_state(Pre,Steps,Steps1),
    append_dcut(List,Steps1,List2),
    append_dcut(List2,[step(HP,OP,undefd,[se(Sort,Obj,SE)],unexp)|Rest],Dec).
%    tell(user),write('+'),told.

% make the achieve goal states [se(..),se(..),..] to separate steps
% remove the states that have been achieved by precondition
make_to_steps(Pre,[],[],Rest):-!.
make_to_steps(Pre,[se(Sort,Obj,SE)|Post],Steps,Rest):-
   state_achieved([se(Sort,Obj,SE)],Pre,_),
   make_to_steps(Pre,Post,Steps,Rest),!.
make_to_steps(Pre,[se(Sort,Obj,SE)|Post],Steps,Rest):-
   member(step(_,achieve(se(Sort,Obj,SE)),_,_),Rest),
   make_to_steps(Pre,Post,Steps,Rest),!.
make_to_steps(Pre,[se(Sort,Obj,SE)|Post],[step(HP,achieve(se(Sort,Obj,SE)),undefd,[se(Sort,Obj,SE)],unexp)|Steps],Rest):-
   gensym_special(hp,HP),
   make_to_steps(Pre,Post,Steps,Rest),!.

change_head_state(Pre,[step(HP,A,undefd,B,unexp)|Steps],[step(HP,A,Pre,B,unexp)|Steps]):-!.

% instantiate the object in lhs states
instantiate([],Statics).
instantiate([se(Sort,Obj,SE)|States],Statics):-
   nonvar(Obj),
   instantiate(States,Statics),!.
instantiate([se(Sort,Obj,SE)|States],Statics):-
   ground(SE),
   instantiate(States,Statics),!.
instantiate([se(Sort,Obj,SE)|States],Statics):-
   var(Obj),
   get_sort_objects(Sort,Objls),
   member(Obj,Objls),
   statics_consist_instance(Statics),
   instantiate(States,Statics).


% select a tp_node with lowest score
select_tnode(tp_node(HPid,Pre,Post,Statics,Score,Dec)) :-
   retractall(score_list(LS)),
   assert(score_list([])),
   lowest_score(Score),
   retract(tp_node(HPid,Pre,Post,Statics,Score,Dec)),!.
%   tell(user),nl,write('new level'),nl,told,!.

% find the lowest_score of tp_node
lowest_score(LScore):-
     tp_node(HPid,Pre,Post,Statics,Score,Dec),
     retract(score_list(LS)),
     assert(score_list([Score|LS])),
     fail.
lowest_score(LScore):-
     retract(score_list(D)),
     sort(D,[LScore|SD]).

:- style_check(-discontiguous).
% assert tp_node if all HP expand, assert solved_node
assert_tnode(TP,Pre,Post,Statics,Score1,Dec):-
   all_HP_expanded(Dec),
   combine_exp_steps2(Dec,OneDec),
   assert(solved_node(Statics,OneDec)),!.

combine_exp_steps2([OneDec],OneDec):-!.
combine_exp_steps2([Dec1,Dec2|Rest],OneDec):-
   combine_steps1(Dec1,Dec2,OneDec1),
   combine_exp_steps2([OneDec1|Rest],OneDec),!.
% else combine the steps of expanded steps
% make it to [step(_,...,exp()),step(_,...,unexp)]
% that makes possible to find the repeated nodes
% also makes possible to make operator chains for later use
assert_tnode(TP,Pre,Post,Statics,Score,Dec):-
   combine_exp_steps(Dec,ExpDec,UnexpDec),
   assert_tnode1(TP,Pre,Post,Statics,Score,ExpDec,UnexpDec),!.

:- style_check(+discontiguous).

assert_tnode1(TP,Pre,Post,Statics,Score,ExpDec,UnexpDec):-
   existing_node(ExpDec,UnexpDec,Statics).
assert_tnode1(TP,Pre,Post,Statics,Score,ExpDec,UnexpDec):-
   get_score(Score,ExpDec,UnexpDec,Score1),
   gensym_special(tp,TP1),
   append_dcut(ExpDec,UnexpDec,Dec),
   assert(tp_node(TP1,Pre,Post,Statics,Score1,Dec)),!.

% combine the expanded steps to one
combine_exp_steps([step(A,B,C,D,unexp)|Rest],[],[step(A,B,C,D,unexp)|Rest]):-!.
combine_exp_steps([step(A,B,C,D,exp(TN)),step(A1,B1,C1,D1,unexp)|Rest],[step(A,B,C,D,exp(TN))],[step(A1,B1,C1,D1,unexp)|Rest]):-!.
combine_exp_steps([step(A1,B1,C1,D1,exp(TN1)),step(A2,B2,C2,D2,exp(TN2))|Rest],ExpDec,UnexpDec):-
    combine_steps1(step(A1,B1,C1,D1,exp(TN1)),step(A2,B2,C2,D2,exp(TN2)),ExpDec1),
    combine_exp_steps([ExpDec1|Rest],ExpDec,UnexpDec),!.

combine_steps1(step(HP1,Name1,Pre1,Post1,exp(TN1)),step(HP2,Name2,Pre2,Post2,exp(TN2)),step(HP,achieve(Post2),Pre1,Post2,exp(TN))):-
    find_only_changed(Pre1,Post2,[],Pre0,[],Post0),
    not(isemptylist(Post0)),
    gensym_special(hp,HP),
    gensym_special(tn,TN),
    assert(tn(TN,achieve(Post0),Pre0,Post0,[before(TN1,TN2)],[step(HP1,Name1,Pre1,Post1,exp(TN1)),step(HP2,Name2,Pre2,Post2,exp(TN2))])),!.

% already been.
existing_node([step(_,_,Pre,Post,exp(TN))],UnexpDec,Statics):-
    tp_node(_,_,_,Statics0,_,[step(_,_,Pre0,Post0,exp(TN0))|Rest]),
    append_st(Statics,Statics0,Statics1),
    all_equal(Pre,Pre0,Statics1),
    all_equal(Post,Post0,Statics1),
    existing_node([],UnexpDec,Statics1).
existing_node([step(_,_,Pre,Post,exp(TN))],UnexpDec,Statics):-
    closed_node(_,_,_,Statics0,_,[step(_,_,Pre0,Post0,exp(TN0))|Rest]),
    append_st(Statics,Statics0,Statics1),
    all_equal(Pre,Pre0,Statics1),
    all_equal(Post,Post0,Statics1),
    existing_node([],UnexpDec,Statics1).
existing_node([],[step(_,Name,Pre,_,unexp)|Rest],Statics):-
    tp_node(_,_,_,Statics0,_,[step(_,Name,Pre0,_,_,unexp)|Rest]),
    append_st(Statics,Statics0,Statics1),
    all_equal(Pre,Pre0,Statics1).
existing_node([],[step(_,Name,Pre,_,unexp)|Rest],Statics):-
    closed_node(_,_,_,Statics0,_,[step(_,Name,Pre0,_,_,unexp)|Rest]),
    append_st(Statics,Statics0,Statics1),
    all_equal(Pre,Pre0,Statics1).




all_equal([],[],Statics):-
    statics_consist(Statics),!.
all_equal([se(Sort,Obj,SE)|Pre],[se(Sort,Obj,SE1)|Pre1],Statics):-
    is_achieved(SE,SE1),
    all_equal(Pre,Pre1,Statics),!.
all_equal([se(Sort,Obj,SE)|Pre],Pre1,Statics):-
    member(se(Sort,Obj,SE1),Pre1),
    list_take(Pre1,[se(Sort1,Obj1,SE1)],Pre2),
    all_equal(Pre,Pre1,Statics),!.

get_score(Score,[],UnExpDec,Score1):-
    length(UnExpDec,Len),
    Score1 is Score+Len,!.
get_score(Score,[step(HP,Name,Pre0,Post0,exp(TN))],UnExpDec,Score1):-
    goal_related(States),
    state_achieved_Num(Post0,States,0,Num),
    length(UnExpDec,Len),
    Score1 is Score-Num,!.

%-------------------------------------------
% the number of postcondition achieved 
% the more the better
state_achieved_Num([],Pre0,Num,Num):-!.
state_achieved_Num([se(Sort,Obj,SE)|Post],Pre0,N,Num):-
    member(se(Sort,Obj,SE0),Pre0),
    state_achieved([se(Sort,Obj,SE0)],[se(Sort,Obj,SE)],_),
    N1 is N+1,
    state_achieved_Num(Post,Pre0,N1,Num),!.
state_achieved_Num([se(Sort,Obj,SE)|Post],Pre0,N,Num):-
    state_achieved_Num(Post,Pre0,N,Num),!.

% the states that can achieve a state
% that is:
% for a state in the rhs of operator
% all the states in the lhs
find_relate_state:-
   operatorC(A,Pre,Post,Cond,ST),
   assert_related_states(A,Pre,Post,Cond,ST),
   fail.
find_relate_state.

assert_related_states(A,Pre,Post,Cond,ST):-
   assert_related_states1(A,Pre,Post,ST),
   assert_related_states2(A,Pre,Cond,ST).

% find relate in nec
% the sorts here are primitive
assert_related_states1(A,Pre,[],ST):-!.
%when prev
assert_related_states1(A,Pre,[se(Sort,Obj,SE)|Post],ST):-
   member_e(se(Sort,Obj,SE),Pre),
   assert_related_states1(A,Pre,Post,ST),!.
%when nec
assert_related_states1(A,Pre,[se(Sort,Obj,SE)|Post],ST):-
   assert(produce(se(Sort,Obj,SE),A,Pre,ST)),
   assert_related_states1(A,Pre,Post,ST),!.

% find relate in conditional
% the sorts here are not primitive
assert_related_states2(A,Pre,SC,ST):-
   make_sc_primitive(SC,PSC),
   assert_related_states21(A,Pre,PSC,ST).

assert_related_states21(A,Pre,[],ST):-!.
assert_related_states21(A,Pre,[sc(Sort,Obj,SE=>SS)|Trans],ST):-
   rem_statics([se(Sort,Obj,SE)],[se(Sort,Obj,SER)],St1),
   rem_statics([se(Sort,Obj,SS)],[se(Sort,Obj,SSR)],St2),
   append_cut(ST,St1,ST1),
   append_cut(ST1,St2,ST21),
   remove_unneed(ST21,[],ST2),
   append_cut(Pre,[se(Sort,Obj,SER)],Pre1),
   assert(produce(se(Sort,Obj,SSR),A,Pre1,ST2)),
   assert_related_states21(A,Pre,Trans,ST),!.

%-------------------------------------------
% the number of state change compare with precondition
% the less the better
pre_changed_Num([],Pre0,Num,Num):-!.
pre_changed_Num([se(Sort,Obj,SE)|Pre],Pre0,N,Num):-
    state_achieved([se(Sort,Obj,SE)],Pre0,_),
    N1 is N+1,
    pre_changed_Num(Pre,Pre0,N1,Num),!.
pre_changed_Num([se(Sort,Obj,SE)|Pre],Pre0,N,Num):-
    pre_changed_Num(Pre,Pre0,N,Num),!.



%-------------------------------------------
% remove HP1 from the temp list
% if  HP1<HP2, then all HP3<HP1 => HP3<HP2
% if  HP2<HP1, then all HP1<HP3 => HP2<HP3
remove_temp([],HP1,List,List):-!.
remove_temp([before(HP1,HP2)|Temp],HP1,List,Temp1):-
    remove_temp_before(List,before(HP1,HP2),List2),
    remove_temp(Temp,HP1,List2,Temp1),!.
remove_temp([before(HP2,HP1)|Temp],HP1,List,Temp1):-
    remove_temp_after(List,before(HP2,HP1),List2),
    remove_temp(Temp,HP1,List2,Temp1),!.
remove_temp([before(HPX,HPY)|Temp],HP1,List,Temp1):-
    remove_temp(Temp,HP1,List,Temp1),!.

% if  HP1<HP2, remove HP1<HP2, and change all HP3<HP1 => HP3<HP2
remove_temp_before([],before(HP1,HP2),[]):-!.
remove_temp_before([before(HP1,HP2)|T],before(HP1,HP2),T1):-
   remove_temp_before(T,before(HP1,HP2),T1),!.
remove_temp_before([before(HP3,HP1)|T],before(HP1,HP2),[before(HP3,HP2)|T1]):-
   remove_temp_before(T,before(HP1,HP2),T1),!.
remove_temp_before([before(HPX,HPY)|T],before(HP1,HP2),[before(HPX,HPY)|T1]):-
   remove_temp_before(T,before(HP1,HP2),T1),!.
% if  HP2<HP1, remove HP2<HP1, and change all HP1<HP3 => HP2<HP3
remove_temp_after([],before(HP1,HP2),[]):-!.
remove_temp_after([before(HP2,HP1)|T],before(HP2,HP1),T1):-
   remove_temp_after(T,before(HP2,HP1),T1),!.
remove_temp_after([before(HP1,HP3)|T],before(HP2,HP1),[before(HP2,HP3)|T1]):-
   remove_temp_after(T,before(HP2,HP1),T1),!.
remove_temp_after([before(HPX,HPY)|T],before(HP2,HP1),[before(HPX,HPY)|T1]):-
   remove_temp_after(T,before(HP2,HP1),T1),!.

remove_dec(HPid,[],[]):-!.
remove_dec(HPid,[step(HPid,_,_,_,_)|Dec],Dec1):-
   remove_dec(HPid,Dec,Dec1),!.
remove_dec(HPid,[step(A,B,C,D,F)|Dec],[step(A,B,C,D,F)|Dec1]):-
   remove_dec(HPid,Dec,Dec1),!.
   
/******************************************************/
% state2 is achieved by state1(or they are not conflict)
% the output state is the combination of the two states
state_achieved(undefd,State,State):-!.
state_achieved(State1,State2,State3):-
    state_achieved1(State1,State2,State3).

state_achieved1([],State2,State2).
state_achieved1([se(Sort,Obj,ST1)|State1],State2,[se(Sort,Obj,STN)|State]):-
    get_sort_objects(Sort,Objls),
    member(Obj,Objls),
    member(se(Sort,Obj,ST2),State2),
    not_conflict(Sort,Obj,ST1,ST2,STN),
    list_take(State2,[se(Sort,Obj,ST2)],State21),
    state_achieved1(State1,State21,State).
state_achieved1([se(Sort1,Obj,ST1)|State1],State2,[se(Sort,Obj,STN)|State]):-
    not(objects(Sort1,Objls)),
    find_prim_sort(Sort1,Sortls),
    member(se(Sort,Obj,ST2),State2),
    member(Sort,Sortls),
    not_conflict(Sort,Obj,ST1,ST2,STN),
    list_take(State2,[se(Sort,Obj,ST2)],State21),
    state_achieved1(State1,State21,State).

% two states not conflict
not_conflict(Sort,Obj,ST,ST1,NewST):-
    gsubstate_classes(Sort,Obj,Substateclasses),
    member(State0,Substateclasses),
    is_achieved(ST,State0),
    is_achieved(ST1,State0),
    set_append_e(ST,ST1,NewST),!.

% all the element in list1 are static or in list2
is_achieved([],_):-!.
is_achieved([H|T], State) :-
    is_statics(H),
    is_achieved(T,State),!.
is_achieved([H|T], State) :-
    member(H,State),
    is_achieved(T,State),!.
%-------------------------------------------

/************ state changes by actions ********/
% if an object's state meet the precondition
% it change to the postcondition
state_change([],Pre0,Post0,[]):-!.
state_change(Pre,[],[],Pre):-!.
% if a obj pre achieves action's pre
% change the obj's post state with action's post
state_change([se(Sort,Obj,SE)|Pre],Pre0,Post0,[se(Sort,Obj,NewSS)|State]):-
    member(se(Sort,Obj,SE0),Pre0),
    member(se(Sort,Obj,SS0),Post0),
    not_conflict(Sort,Obj,SE,SE0,_),
    state_change1(SE,SE0,SS0,NewSS),
    list_take(Pre0,[se(Sort,Obj,SE0)],Pre1),
    list_take(Post0,[se(Sort,Obj,SS0)],Post1),
    state_change(Pre,Pre1,Post1,State),!.
% if a obj state not defined in  action's pre
% move it to its post
state_change([se(Sort,Obj,SE)|Pre],Pre0,Post0,[se(Sort,Obj,SE)|State]):-
    state_change(Pre,Pre0,Post0,State),!.

state_change1([],SE0,SS0,SS0):-!.
state_change1([HSE|TSE],SE0,SS0,SS1):-
    member(HSE,SE0),
    state_change1(TSE,SE0,SS0,SS1),!.
state_change1([HSE|TSE],SE0,SS0,[HSE|SS1]):-
    state_change1(TSE,SE0,SS0,SS1),!.
%-------------------------------------------
%-------------------------------------------
/************ state changes by conditions ********/
% for all the object's state meet the precondition
% it change to the postcondition
cond_state_change([],Cond,[]):-!.
cond_state_change(State,[],State):-!.
cond_state_change([se(Sort,Obj,SE)|Pre],Cond,[se(Sort,Obj,NewSS)|State]):-
    member(sc(Sort1,Obj,SE0=>SS0),Cond),
    statics_consist([is_of_sort(Obj,Sort1),is_of_sort(Obj,Sort)]),
    not_conflict(Sort,Obj,SE,SE0,_),
    state_change1(SE,SE0,SS0,NewSS),
    cond_state_change(Pre,Cond,State),!.
cond_state_change([se(Sort,Obj,SE)|Pre],Cond,[se(Sort,Obj,SE)|State]):-
    cond_state_change(Pre,Cond,State),!.

%-------------------------------------------
% check that an action's state change
% included changes of the goal states need to be achieved
post_changed(Post0,Cond,Statics,[se(Sort,Obj,SE)|Post]):-
    member(se(Sort,Obj,SE0),Post0),
    state_achieved([se(Sort,Obj,SE0)],[se(Sort,Obj,SE)],_),
    statics_consist(Statics).
post_changed(Post0,Cond,Statics,[se(Sort,Obj,SE)|Post]):-
    member(sc(Sort,Obj,SE1=>SS),Cond),
    state_achieved([se(Sort,Obj,SS)],[se(Sort,Obj,SE)],_),
    statics_consist(Statics).
post_changed(Post0,Cond,Statics,[se(Sort,Obj,SE)|Post]):-
    post_changed(Post0,Cond,Statics,Post).

all_achieved([],Statics,Post).
all_achieved([se(Sort,Obj,SL)|Pre],Statics,Post):-
    not(member(se(Sort,Obj,SR),Post)),
    all_achieved(Pre,Statics,Post).
all_achieved([se(Sort,Obj,SL)|Pre],Statics,Post):-
    member(se(Sort,Obj,SR),Post),
    is_achieved(SL,SR),
    statics_consist_instance(Statics),
    all_achieved(Pre,Statics,Post).

% flatten with no duplicate
set_flatten([HO|TO], List, O_List):-
	set_append_e(HO, List, List_tmp),
	set_flatten(TO, List_tmp, O_List),!.
set_flatten([H|TO], List,O_List):-
	set_append_e([H], List, List_tmp),
	set_flatten(TO, List_tmp, O_List).
set_flatten([], [HList|T], O_List):-
	HList = [],
	set_flatten(T, [], O_List).
set_flatten([], [HList|T], O_List):-
	list(HList),
	set_flatten([HList|T],[], O_List),!.
set_flatten([], L,L):-!.

%-------------------------------------------
% instantiate a bit
% use action's state change include the postcondition
post_instant(Post0,Cond,Statics,[]):-!.
post_instant(Post0,Cond,Statics,[se(Sort,Obj,SE)|Post]):-
    member(se(Sort,Obj,SE0),Post0),
    statics_consist(Statics).
post_instant(Post0,Cond,Statics,[se(Sort,Obj,SE)|Post]):-
    member(sc(Sort,Obj,SE1=>SS),Cond),
    statics_consist(Statics).
post_instant(Post0,Cond,Statics,[se(Sort,Obj,SE)|Post]):-
    member(sc(Sort0,Obj,SE1=>SS),Cond),
    not(objects(Sort0,_)),
    subsorts(Sort0,Sortls),
    not(not(member(Sort,Sortls))),
    statics_consist(Statics).
post_instant(Post0,Cond,Statics,[se(Sort,Obj,SE)|Post]):-
    post_instant(Post0,Cond,Statics,Post),!.
/******************************* NODE ADT *********************/

%node(Name, Precond, Decomps, Temp, Statics)
% When inputting new methods etc filter all statics into
% static slot

make_problem_into_node(I,goal(L,TM,STATS),  NN) :-
     make_problem_up(L, STEPS),
     make_num_hp(TM,Temp),
     sort_steps(STEPS,Temp,STEPS1),
     make_ss_to_se(I,I_Pre),
     NN = node(root,I_Pre,STEPS1 ,Temp, STATS),!.

% make problem to steps
make_problem_up([],[]):-!.
make_problem_up([achieve(L)|R],[step(HP,achieve(L1),undefd,[L1],unexp)|RS]):- 
                             %preconditon here is undefd
    make_ss_to_se([L],[L1]),
    gensym_special(hp,HP),
    make_problem_up(R, RS),!.
make_problem_up([achieve(L)|R],[step(HP,achieve(L1),undefd,L1,unexp)|RS]):- 
                             %preconditon here is undefd
    make_ss_to_se(L,L1),
    gensym_special(hp,HP),
    make_problem_up(R, RS),!.
make_problem_up([O|R],[step(HP,O,undefd,undefd,unexp)|RS]):-
    methodC(O,Pre,Post,Statics1,Temp,ACH,Dec),
    gensym_special(hp,HP),
    make_problem_up(R, RS),!.
make_problem_up([O|R],     
           [step(HP,O,undefd,undefd,unexp)|RS]):-
    operatorC(O,Pre,Post,Cond,Statics1),
    gensym_special(hp,HP),
    make_problem_up(R, RS),!.

make_num_hp([],[]):-!.
make_num_hp([before(N1,N2)|TM],[before(H1,H2)|Temp]):-
    gensym_num(hp,N1,H1),
    gensym_num(hp,N2,H2),
    make_num_hp(TM,Temp),!.

%**************sort steps*********************************
% sort steps by temporal constraints.
sort_steps(Steps,[],Steps):-!.
sort_steps([Steps|[]],[],[Steps]):-!.
sort_steps(Steps,Temp,OrderedST):-
   steps_in_temp(Temp,[],ST),
   sort_steps1(Temp,ST,OrderedSTID),
   sort_steps2(Steps,OrderedSTID,[],OrderedST),!.

% find out the steps in temporal constraints.
steps_in_temp([],ST,ST):-!.
steps_in_temp([before(H1,H2)|TT],List,ST):-
   set_append_e(List,[H1,H2],List1),
   steps_in_temp(TT,List1,ST),!.

% sort the steps_id(hps) by temporal constraints.
sort_steps1(Temp,[],[]):-!.
sort_steps1(Temp,[HP1|TST],[HPF|OST]):-
   earliest_step(HP1,HPF,Temp,TST,TST1),
   sort_steps1(Temp,TST1,OST),!.
   
earliest_step(HPF,HPF,Temp,[],[]):-!.
earliest_step(HP1,HPF,Temp,[HP2|TST],[HP1|TST1]):-
   member(before(HP2,HP1),Temp),
   earliest_step(HP2,HPF,Temp,TST,TST1),!.
earliest_step(HP1,HPF,Temp,[HP2|TST],[HP2|TST1]):-
   earliest_step(HP1,HPF,Temp,TST,TST1),!.

% sort the steps, put the unordered steps in the front
sort_steps2(OtherST,[],OrderedST1,OrderedST):-
   append_dcut(OrderedST1,OtherST,OrderedST),!.
sort_steps2(Steps,[HP|THPS],List,OrderedST):-
   member(step(HP,N,Pre,Post,F),Steps),
   append_dcut(List,[step(HP,N,Pre,Post,F)],List1),
   list_take(Steps,[step(HP,N,Pre,Post,F)],Steps1),
   sort_steps2(Steps1,THPS,List1,OrderedST),!.
sort_steps2(Steps,[HP|THPS],List,OrderedST):-
   sort_steps2(Steps,THPS,List,OrderedST),!.
%*******************************************************

% replace ss to se
make_ss_to_se([],[]):-!.
make_ss_to_se([ss(Sort,Obj,Post)|TPost],[se(Sort,Obj,Post)|TPre]):-
     make_ss_to_se(TPost,TPre),!.
make_ss_to_se([se(Sort,Obj,Post)|TPost],[se(Sort,Obj,Post)|TPre]):-
     make_ss_to_se(TPost,TPre),!.

%*******************************************************
% extract_solution(Node,..
% recurvise routine to work down tree and
% print out a linearisation of it
extract_solution(Node,PHPs,SIZE1) :-
       % its the name of a hierarchical op......
   getN_decomp(Node, HPs),
   push_to_primitive(HPs,[],PHPs),
   pprint(PHPs,1,SIZE),
   SIZE1 is SIZE -1,!.

%***********print out solution**************************   
push_to_primitive([],PHPs,PHPs) :-!.
push_to_primitive([step(HPID,_,_,_,exp(TN))|HPs],List,PHPs) :-
   tn(TN,Name,Pre,Post,Temp,Dec),
   push_to_primitive(Dec,List,Dec1),
   push_to_primitive(HPs,Dec1,PHPs),!.
push_to_primitive([step(HPID,_,_,_,exp(Name))|HPs],List,PHPs):-
   append_dcut(List,[Name],List1),
   push_to_primitive(HPs,List1,PHPs),!.


%*******************************************************   
pprint([],SIZE,SIZE):-!.
pprint([HS|TS],Size0,SIZE):-
    list(HS),
    pprint(HS,Size0,Size1),
    pprint(TS,Size1,SIZE),!.
pprint([HS|TS],Size0,SIZE):-
%    write('step '),write(Size0),write(': '),
%    write(HS),nl,
    Size1 is Size0+1,
    pprint(TS,Size1,SIZE),!.

delete_all_nodes :-
	retractall(node(_,_,_,_,_)),
	retractall(final_node(_)),
	retractall(tp_node(_,_,_,_,_,_)),
	retractall(closed_node(_,_,_,_,_,_)),
	retractall(solved_node(_,_,_)).
delete_all_nodes :- !.

/*********** TEMPORAL AND DECLOBBERING ************/

possibly_before(I,J,Temps) :-
    \+ necessarily_before(J,I,Temps), !.

necessarily_before(J,I,Temps) :-
    member(before(J,I),Temps),!.
necessarily_before(J,I,Temps) :-
    member(before(J,Z),Temps),
    necessarily_before(Z,I,Temps),!.

/********** retracting, asserting and reporting nodes *****************/

/***** SOLUTION ******/
select_node(node(Name,Pre,Temp,Decomp,Statics)) :-
   retract(node(Name,Pre,Temp,Decomp,Statics)),
%   nl,nl,write(Name),write(' RETRACTED'),nl,nl,
%   tell(user),
%   nl,nl,write(Name),write(' RETRACTED'),nl,nl,
%   tell(FF),
    !.

all_HP_expanded([]):-!.
all_HP_expanded([step(HPid,Name,_,_,exp(TN))|THPS]):-
   all_HP_expanded(THPS),!.


% check for statics consist and instanciate them
statics_consist_instance([]):-!.
statics_consist_instance(Statics) :-
   get_invariants(Invs),
   statics_consist_instance0(Invs,Statics).



statics_consist_instance0(Invs,[]):-!.
statics_consist_instance0(Invs,[ne_back(A,B)|TStatics]):-

   not(A==B),
   statics_consist_instance0(Invs,TStatics).
statics_consist_instance0(Invs,[ne(A,B)|TStatics]):-
   append_dcut(TStatics,[ne_back(A,B)],TStatics1),
   statics_consist_instance0(Invs,TStatics1),!.
statics_consist_instance0(Invs,[is_of_sort(Obj,Sort)|TStatics]):-
   is_of_sort(Obj,Sort),
   statics_consist_instance0(Invs,TStatics).
statics_consist_instance0(Invs,[is_of_primitive_sort(Obj,Sort)|TStatics]):-
   is_of_primitive_sort(Obj,Sort),
   statics_consist_instance0(Invs,TStatics).
statics_consist_instance0(Invs,[Pred|TStatics]):-
   member(Pred,Invs),
   statics_consist_instance0(Invs,TStatics).



% check for statics consist without instanciate them
% only instance the variable when there is one choice of from the ground lists
statics_consist([]):-!.
statics_consist(Statics):-
   get_invariants(Invs),
   statics_consist1(Invs,Statics),!.
   
statics_consist1(Invs,[]):-!.
statics_consist1(Invs,[ne_back(A,B)|TStatics]):-
   not(A==B),
   statics_consist1(Invs,TStatics),!.
statics_consist1(Invs,[ne(A,B)|TStatics]):-
   append_dcut(TStatics,[ne_back(A,B)],TStatics1),
   statics_consist1(Invs,TStatics1),!.
statics_consist1(Invs,[is_of_sort(Obj,Sort)|TStatics]):-
   get_sort_objects(Sort,Objs),
   obj_member(Obj,Objs),
   statics_consist1(Invs,TStatics),!.
statics_consist1(Invs,[is_of_primitive_sort(Obj,Sort)|TStatics]):-
   objects(Sort,Objs),
   obj_member(Obj,Objs),
   statics_consist1(Invs,TStatics),!.
statics_consist1(Invs,[Pred|TStatics]):-
   pred_member(Pred,Invs),
   statics_consist1(Invs,TStatics),!.

/***************** local utils *****************/

/*********** DOMAIN MODEL FUNCTIONS *****************/
get_invariants(Invs) :-
    atomic_invariants(Invs),!.

rem_statics([ss(S,X,Preds)|Post], [ss(S,X,PredR)|PostR],Rt1) :-
    filter_list(Preds, is_a_dynamic_pred,PredR),
    filter_list(Preds, is_a_static_pred, R),
    rem_statics(Post, PostR,Rt),
    append_dcut(Rt,[is_of_sort(X,S)|R],Rt1),!.
rem_statics([se(S,X,Preds)|Post], [se(S,X,PredR)|PostR],Rt1) :-
    filter_list(Preds, is_a_dynamic_pred,PredR),
    filter_list(Preds, is_a_static_pred, R),
    rem_statics(Post, PostR,Rt),
    append_dcut(Rt,[is_of_sort(X,S)|R],Rt1),!.
rem_statics([], [],[]) :-!.

% check if a predicate is statics or not
is_statics(ne(A,B)):-!.
is_statics(is_of_sort(A,B)):-!.
is_statics(is_of_primitive_sort(A,B)):-!.
is_statics(Pred):-
    functor(Pred,FF,NN),
    functor(Pred1,FF,NN),
    atomic_invariants(Atom),
    member(Pred1,Atom),!.
    

/***************** STATICS ************************/
% u_mem in ob_utils is SLOW!?.
% this is a fast impl.
u_mem_cut(_,[]):-!,fail.
u_mem_cut(X,[Y|_]) :- X == Y,!.
u_mem_cut(X,[_|L]) :- u_mem_cut(X,L),!.


is_of_primitive_sort(X,Y) :-
    objects(Y,L),member(X,L).
is_of_sort(X,Y) :-
    is_of_primitive_sort(X,Y).
is_of_sort(X,Y) :-
    sorts(Y,SL),member(Z,SL),is_of_sort(X,Z).

member_cut(X,[X|_]) :- !.
member_cut(X,[_|Y]) :- member_cut(X,Y),!.

% check if object X is a member of a objects list
% 1. if it is not a variable, check if it is in the list
% 2. X is a variable, and the list only has one objects, make X as that obj
% 3. X is a variable, but the list has more than one objects, leave X unchange
obj_member(X,[X|[]]):-!. 
obj_member(X,List):-     
    obj_member0(X,List),!.
obj_member0(X,[Y|_]):-
    var(X),!.%if X is var, but Y not, the leave X as that variable
obj_member0(X,[Y|_]):-
    X==Y,!.
obj_member0(X,[_|Y]) :- obj_member0(X,Y),!.

% check if a predicate is a member of a ground predicate list,
% 1. when only one predicates found
% just used in binding the predicates to a sort without instantiate it
% for efficiency, instantiate the variable if the list only have one atom
pred_member(ne(A,B),List):-
    A\==B,!.
pred_member(is_of_sort(A,B),List):-
    get_sort_objects(B,Objls),
    obj_member(A,Objls),!.
pred_member(is_of_primitive_sort(A,B),List):-
    objects(B,Objls),
    obj_member(A,Objls),!.
pred_member(X,List):-
    setof(X,member(X,List),Refined),
    pred_member0(X,Refined),!.
pred_member0(X,[X|[]]):-!.
pred_member0(X,Y):-
    pred_member1(X,Y),!.
pred_member1(X,[Y|_]):-
    X=..[H|XLs],
    Y=..[H|YLs],
    vequal(XLs,YLs),!.
pred_member1(X,[_|Y]):- pred_member1(X,Y),!.
	
vequal([],[]):-!.
vequal([X|XLs],[Y|YLs]):-
    var(X),
    vequal(XLs,YLs),!.    
vequal([X|XLs],[Y|YLs]):-
    var(Y),
    vequal(XLs,YLs),!.
vequal([X|XLs],[Y|YLs]):-
    X==Y,	
    vequal(XLs,YLs),!.

append_cut([],L,L) :- !.
append_cut([H|T],L,[H|Z]) :- append_cut(T,L,Z),!.




/************ change_op_representation ***********/
% make pre and post explicit
% filter out statics and put in a new slot
change_op_representation :-    
    method(A,B,C,Stat,T,Dec),
    make_ss_to_se(B,B0),
    make_se_primitive(B0,B1),
    make_sc_primitive(C,C1),
    get_preconditions(C1,B1,Pre,Post),
    rem_statics(Post, PostR,St1),
    rem_statics(Pre, PreR,St2),
    append_cut(St1,St2,Statics),
    append_cut(Stat,Statics,Statics1),
    remove_unneed(Statics1,[],Statics2),
    get_achieval(A,Dec,T,Dec1,T1,ACH),
    assert(methodC(A,PreR,PostR,Statics2,T1,achieve(ACH),Dec1)),
    fail.
change_op_representation :-
    operator(A,B,C,D),
    make_ss_to_se(B,B0),
    make_se_primitive(B0,B1),
    make_sc_primitive(C,C1),
%    make_sc_primitive(D,D1),
	%can't do that because it narrow the conditional change 
    get_preconditions(C1,B1,Pre,Post),
    rem_statics(Post, PostR,St1),
    rem_statics(Pre, PreR,St2),
    append_cut(St1,St2,Statics1),
    remove_unneed(Statics1,[],Statics),
    statics_consist(Statics),
    assert(operatorC(A,PreR,PostR,D,Statics)),
    fail.
change_op_representation:-
    retractall(current_num(sm,_)),!.

get_preconditions([],Prev,Prev,Prev) :-!.
get_preconditions([sc(S,X,From =>To)|Rest],Prev,[se(S,X,From1)|Pre],[se(S,X,To1)|Post]):-
     member_e(se(S,X,PSE),Prev),
     append_dcut(PSE,From,From1),
     append_dcut(PSE,To,To1),
     list_take(Prev,[se(S,X,PSE)],Prev1),
     get_preconditions(Rest,Prev1, Pre,Post),!.
get_preconditions([sc(S,X,From =>To)|Rest],Prev,[se(S,X,From)|Pre],[se(S,X,To)|Post]):-
     get_preconditions(Rest,Prev, Pre,Post),!.
get_preconditions([],Prev,Prev,Prev) :-!.

% get all achieve goals out
get_achieval(A,Dec,T,Dec1,T1,Achieval):-
     retractall(current_num(sm,_)),
     make_dec(A,Dec,Dec1,T,T1,[],Achieval),!.
make_dec(A,[],[],Temp,Temp,Achieval,Achieval):-!.
make_dec(A,[HD|TD],TD1,Temp,Temp1,Achieval,Achieval1):-
     HD=..[achieve|Goal],
     current_num(sm,Num),
     replace_achieval_temp(Temp,Temp0,Num),
     make_ss_to_se(Goal,Goal0),
     append_dcut(Achieval,Goal0,Achieval0),
     make_dec(A,TD,TD1,Temp0,Temp1,Achieval0,Achieval1),!.
make_dec(A,[HD|TD],TD1,Temp,Temp1,Achieval,Achieval1):-
     HD=..[achieve|Goal],
     not(current_num(sm,Num)),
     replace_achieval_temp(Temp,Temp0,1),
     make_ss_to_se(Goal,Goal0),
     append_dcut(Achieval,Goal0,Achieval0),
     make_dec(A,TD,TD1,Temp0,Temp1,Achieval0,Achieval1).
make_dec(A,[HD|TD],[HD|TD1],Temp,Temp1,Achieval,Achieval1):-
     HD=..[DecName|Goal],
     DecName\==achieve,
     gensym_special(sm,SM),
     current_num(sm,Num),
     make_dec(A,TD,TD1,Temp,Temp1,Achieval,Achieval1),!.

% get rid of the achievals in temp orders
replace_achieval_temp(Temp,Temp1,Num):-
     change_all_numbers(Temp,Num,Temp00),
     tidy_temp(Temp00,Temp1).

change_all_numbers([],Num,[]):-!.
change_all_numbers([HTemp|TTemp],Num,[HTemp00|TTemp00]):-
     HTemp=..[before|Nums],
     change_nums(Nums,Num,Nums1),
     HTemp00=..[before|Nums1],
     change_all_numbers(TTemp,Num,TTemp00).

change_nums([],Num,[]):-!.
change_nums([Num1|TN],Num,[Num1|TN1]):-
    Num1<Num,
    change_nums(TN,Num,TN1),!.
change_nums([Num1|TN],Num,[Num2|TN1]):-
    Num1>Num,
    Num2 is Num1-1,
    change_nums(TN,Num,TN1),!.
change_nums([Num|TN],Num,[0|TN1]):-
    change_nums(TN,Num,TN1),!.

% since assumed achieval only happen at first, so only change the after ones
tidy_temp(Temp,Temp1):-
     member(before(Num,0),Temp),
     list_take(Temp,[before(Num,0)],Temp0),
     change_laters(Temp0,Num,Temp01),
     tidy_temp(Temp01,Temp1).
tidy_temp([],[]):-!.
tidy_temp([before(0,Num)|Temp],Temp0):-
     tidy_temp(Temp,Temp0),!.
tidy_temp([before(Num1,Num2)|Temp],[before(Num1,Num2)|Temp0]):-
     tidy_temp(Temp,Temp0),!.

change_laters([before(0,Num2)|Temp],Num,[before(Num,Num2)|Temp0]):-
     change_laters(Temp,Num,Temp0).
change_laters([before(Num1,0)|Temp],Num,[before(Num1,0)|Temp0]):-
     change_laters(Temp,Num,Temp0).
change_laters([before(Num1,Num2)|Temp],Num,[before(Num1,Num2)|Temp0]):-
     change_laters(Temp,Num,Temp0).


% ------------ end of change operator ----------------------
make_tn(TN,Name,Pre,Post,Temp,Dec):-
    gensym_special(tn,TN),
    find_only_changed(Pre,Post,[],Pre1,[],Post1),
%    tell(user),nl,write(tn(TN,Name,Pre1,Post1,Temp,Dec)),nl,told,
    assert(tn(TN,Name,Pre1,Post1,Temp,Dec)),!.

find_only_changed([],[],Pre,Pre,Post,Post):-!.
% just a lazy check if they are in exactly same sequence
find_only_changed([se(Sort,Obj,ST)|Pre],[se(Sort,Obj,ST)|Post],Pre0,Pre1,Post0,Post1):-
    find_only_changed(Pre,Post,Pre0,Pre1,Post0,Post1),!.
find_only_changed([se(Sort,Obj,ST)|Pre],Post,Pre0,Pre1,Post0,Post1):-
    member(se(Sort,Obj,ST1),Post),
    list_take(Post,[se(Sort,Obj,ST1)],Post2),
    append_changed(se(Sort,Obj,ST),se(Sort,Obj,ST1),Pre0,Pre3,Post0,Post3),
    find_only_changed(Pre,Post2,Pre3,Pre1,Post3,Post1),!.
find_only_changed([se(Sort,Obj,ST)|Pre],Post,Pre0,Pre1,Post0,Post1):-
    member(se(SortN,Obj,ST1),Post),
    list_take(Post,[se(SortN,Obj,ST1)],Post2),
    append_changed(se(Sort,Obj,ST),se(SortN,Obj,ST1),Pre0,Pre3,Post0,Post3),
    find_only_changed(Pre,Post2,Pre3,Pre1,Post3,Post1),!.
% other fail. 

% append_dcut  only changed states
% not_conflict here means not changed
append_changed(se(Sort,Obj,ST),se(Sort1,Obj,ST1),Pre0,Pre0,Post0,Post0):-
    not_conflict(Sort,Obj,ST,ST1,_),!.
append_changed(se(Sort,Obj,ST),se(Sort1,Obj,ST1),Pre0,Pre3,Post0,Post3):-
    append_dcut(Pre0,[se(Sort,Obj,ST)],Pre3),
    append_dcut(Post0,[se(Sort,Obj,ST1)],Post3),!.

% change the states to primitive states
make_se_primitive([],[]).
make_se_primitive([se(Sort,Obj,ST)|SE],[se(Sort,Obj,ST)|SE0]):-
    find_prim_sort(Sort,[Sort]),!,
    make_se_primitive(SE,SE0).
make_se_primitive([se(Sort,Obj,ST)|SE],[se(PSort,Obj,ST)|SE0]):-
    find_prim_sort(Sort,PSorts),
    member(PSort,PSorts),
    make_se_primitive(SE,SE0).

% change the state changes to primitive states
make_sc_primitive([],[]).
make_sc_primitive([sc(Sort,Obj,SE1=>SE2)|ST],[sc(Sort,Obj,SE1=>SE2)|ST0]):-
    find_prim_sort(Sort,[Sort]),!,
    make_sc_primitive(ST,ST0).
make_sc_primitive([sc(Sort,Obj,SE1=>SE2)|ST],[sc(PSort,Obj,SE1=>SE2)|ST0]):-
    find_prim_sort(Sort,PSorts),
    member(PSort,PSorts),
    make_sc_primitive(ST,ST0).

/************ end of change_op_representation ***********/

isemptylist([]):-!.

gensym_num(Root,Num,Atom):-
     name(Root,Name),
     name(Num,Name1),
     append_dcut(Name,Name1,Name2),
     name(Atom,Name2),!.


% set_append_e: list1 + list2 -> list
% no duplicate, no instanciation
% ------------------------------------------
set_append_e(A,B,C):-
    append_cut(A,B,D),
    remove_dup(D,[],C),!.

% remove duplicate
remove_dup([],C,C):-!.
remove_dup([A|B],Z,C) :-
    member_e(A, Z),
    remove_dup(B, Z, C),! .
remove_dup([A|B], Z, C):-
    append_dcut(Z,[A],D),
    remove_dup(B, D, C),!.

member_e(X,[Y|_]):-
     X==Y,!.
member_e(X,[Y|L]):-
     var(Y),
     member_e(X,L),!.
member_e(ss(Sort,Obj,SE),[ss(Sort,Obj1,SE)|_]):-
     Obj==Obj1,!.
member_e(se(Sort,Obj,SE),[se(Sort,Obj1,SE)|_]):-
     Obj==Obj1,!.
member_e(sc(Sort,Obj,SE1=>SE2),[sc(Sort,Obj1,SE1=>SE2)|_]):-
     Obj==Obj1,!.
member_e(X,[Y|L]):- member_e(X,L),!.


% append_st: append_dcut two statics
% remove the constants that no need
% instanciate the viables that all ready been bind
% ------------------------------------------
append_st(ST1,ST2,ST):-
    append_cut(ST1,ST2,ST0),
    remove_unneed(ST0,[],ST),!.

% remove the constants that no need
% instanciate the variables that all ready been bind
remove_unneed([],C,C):-!.
remove_unneed([A|B], Z, C):-
    var(A),
    member_e(A,Z),
    remove_unneed(B, Z, C),! .
remove_unneed([A|B], Z, C):-
    var(A),
    append_dcut(Z,[A],D),
    remove_unneed(B, D, C),!.
remove_unneed([A|B], Z, C):-
    ground(A),
    remove_unneed(B, Z, C),!.
remove_unneed([A|B], Z, C):-
    A=..[ne|Paras],
    append_dcut(Z,[A],D),
    remove_unneed(B, D, C),!.
remove_unneed([A|B], Z, C):-
    A=..[Pred|Paras],
    same_var_member(A,Z),
    remove_unneed(B, Z, C),!.
remove_unneed([A|B], Z, C):-
    append_dcut(Z,[A],D),
    remove_unneed(B, D, C),!.

same_var_member(Pred,[Pred1|List]):-
     var(Pred1),
     same_var_member(Pred,List),!.
same_var_member(Pred,[Pred1|List]):-
     Pred==Pred1,!.
same_var_member(Pred,[Pred1|List]):-
     Pred=..[H|T],
     Pred1=..[H|T1],
     same_var_member1(T,T1),!.
same_var_member(Pred,[Pred1|List]):-
     same_var_member(Pred,List),!.

same_var_member1([],[]):-!.
same_var_member1([H1|T],[H2|T]):-
     var(H1),
     H1==H2,!.
same_var_member1([H|T1],[H|T2]):-
     var(T1),
     T1==T2,!.
same_var_member1([H1|T1],[H2|T2]):-
     H1==H2,
     same_var_member1(T1,T2),!.

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


% grounding substate_class to primary sort level
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
     get_obj_sort(Vars,PSorts),
     assert(gpred(UPred,GPred)),
     fail.
assert_gpred0(GPred,UPred).

get_obj_sort([],[]):-!.
get_obj_sort([HVars|TV],[HObj|TS]):-
     objects(HVars,Objls),
     member(HObj,Objls),
     get_obj_sort(TV,TS),!.


find_all_upper([],[]).
find_all_upper([HVars|TV],[HSorts|TS]):-
     uppersorts(HSorts,Upsorts),
     member(HVars,Upsorts),
     find_all_upper(TV,TS).
     
% find out primitive sorts of a sort.
find_prim_sort(Sort,PS):-
  subsorts(Sort,Subsorts),
  split_prim_noprim(Subsorts,PS,NP),!.

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
  append_dcut(List,Sorts,List1),
  append_dcut(TOpen,Sorts,Open1),
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

sort_up(Sort, List,Sortslist):-
  sorts(non_primitive_sorts,NPSorts),
  sort_up1(Sort,NPSorts,NPSorts,List,Sortslist),!.

sort_up1(Sort,[],NPSorts,Sortslist,Sortslist):-!.
sort_up1(Sort,[HNPSorts|TNPSorts],NPSorts,List,Sortslist):-
  sorts(HNPSorts,Sorts),
  member(Sort,Sorts),
  append_dcut(List, [HNPSorts], List1),
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


% ----------------------utilities---------------------

/*
not(X):- \+X.
member(X,[X|_]).
member(X,[_|L]) :- member(X,L).
append_dcut([],L,L):-!.
append_dcut([H|T],L,[H|Z]) :- append_dcut(T,L,Z),!.
 */







file_exists(Filename):-exists_file(Filename).

% subtract(A,B,C): subtract B from A
% -------------------------------------
subtract([],_,[]):-!.
subtract([A|B],C,D) :-
        member(A,C),
        subtract(B,C,D),!.
subtract([A|B],C,[A|D]) :-
        subtract(B,C,D),!.

/* arg1 - arg2 = arg3 */

list_take(R,[E|R1],R2):-
        remove_el(R,E,RR),
        list_take(RR,R1,R2),!.
list_take(R,[_|R1],R2):-
        list_take(R,R1,R2),!.
list_take(A,[],A) :- !.

				% remove_el: list * el -> list-el 
% ----------------------------------
remove_el([],_,[]) :- ! .
remove_el([A|B],A,B) :- ! .
remove_el([A|B],C,[A|D]) :-
        remove_el(B,C,D) .

/* generate symbol predicate  (from file futile)*/

gensym_special(Root,Atom) :-
                        getnum(Root,Num),
                        name(Root,Name1),
                        name(Num,Name2),
                        append_dcut(Name1,Name2,Name),
                        name(Atom,Name).

getnum(Root,Num) :-
                        retract(current_num(Root,Num1)),!,
                        Num is Num1+1,
                        asserta(current_num(Root,Num)).

getnum(Root,1) :- asserta(current_num(Root,1)).

% append_st: append_dcut two statics

/* is X is in the atomic_invariants then by defn its a static. */
is_a_static_pred(X) :-
        atomic_invariants( A ),
        not( not( member(X,A) )),!.
is_a_static_pred(ne(_,_)) :-!.
is_a_static_pred(is_of_sort(_,_)) :-!.
 
is_a_dynamic_pred(X) :-
        not( is_a_static_pred(X) ),!.
 
/* filter_list(X,condition(args),XO)
 
 XO is reduced list */

filter_list([X|Rest],Op,[X|Rest1]) :-
        Op =.. OL,
        append_dcut(OL,[X],OL1),
        Pred =.. OL1,
        call(Pred),
        filter_list(Rest,Op,Rest1),!.
filter_list([_|Rest],Op,Rest1) :-
        filter_list(Rest,Op,Rest1),!.
filter_list([],_,[]).



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
        xprod(Y,E,C,G).
% list of lists -> list

flatten([HO|TO], List, O_List):-
	append_dcut(HO, List, List_tmp),
	flatten(TO, List_tmp, O_List),!.
flatten([H|TO], List,O_List):-
	append_dcut([H], List, List_tmp),
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
% ***********************for multy tasks*****************
:- assert(time_taken(0)).
:- assert(soln_size(0)).
:- assert(plan_used(0)).

hyhtn_new_solve(N,FN):-
   N < FN,
   tell(user),
   nl,write('task '), write(N),write(': '),nl,
   solution_file(F),
   tell(F),
   nl,write('task '), write(N),write(': '),nl,
   hyhtn_new_solve(N),
   Ni is N+1,
   hyhtn_new_solve(Ni,FN).
hyhtn_new_solve(FN,FN):-
   tell(user),nl,write('task '),
   write(FN),write(': '),nl,
   solution_file(F),
   tell(F),
   nl,write('task '),
   write(FN),write(': '),nl,
   hyhtn_new_solve(FN),
   retractall(sum(_)),
   assert(sum(0)),
   sum_time(CP),
   retractall(sum(_)),
   assert(sum(0)),
   sum_size(SIZE),
   TIM is CP /1000,
   retractall(sum(_)),
   assert(sum(0)),
   sum_plan(PLAN),
   retractall(time_taken(_)),
   retractall(soln_size(_)),
   retractall(plan_used(_)),
   nl,write('total time '),write(TIM),write(' seconds'),
   nl,write('total size '),write(SIZE),
   nl,write('plan used '),write(PLAN),
   nl,
   told.

sum_time(TIM):-
   time_taken(CP),
   retract(sum(N)),
   N1 is N +CP,
   assert(sum(N1)),
   fail.
sum_time(TIM):-
   sum(TIM).
sum_size(SIZE):-
   soln_size(S),
   retract(sum(N)),
   N1 is N +S,
   assert(sum(N1)),
   fail.
sum_size(SIZE):-
   sum(SIZE).
sum_plan(Plan):-
   plan_used(PL),
   retract(sum(N)),
   N1 is N +PL,
   assert(sum(N1)),
   fail.
sum_plan(Plan):-
   sum(Plan).


startOCL(Goal,Init):-
  clean_problem,
   dmsg('OCL-PLANNER-TASK'(Goal)),
	must(planner_interface(Goal,Init,Sol,_,TNLst)),
        show_result_and_clean(F,Id,Sol,TNLst).


get_tasks(N,Goal,State):- htn_task(N,Goal,State).
% get_tasks(N,Goal,State):- ocl:htn_task(N,Goal,State).
get_tasks(N,Goal,State):- planner_task(N,Goal,State).

%:- set_op_num(0).
:-asserta(my_stats(0)).

l_file(File):- \+ exists_file(File),!,forall(filematch(File,FM),l_file(FM)).
l_file(F):- env_consult(ocl:F).
l_file(F):-
   if_defined(/*ocluser*/ocl:force_reload_mpred_file(F),
              if_defined(/*ocluser*/ocl:with_mpred_consult(/*ocluser*/ocl:consult(F)),/*ocluser*/ocl:consult(F))).

solve_file(F):-with_filematch(l_file(wfm(F))), doall(hyhtn_new_solve(_)).
hyhtn_new_solve:- solve_file(test_hyhtn).


:- multifile htn_task/3.
:- dynamic htn_task/3.
:- multifile planner_task/3.
:- dynamic planner_task/3.



% The following elements are expected in the sort engineered domain model
% 
:-dynamic  
  domain_name/1,
  sorts/2,
  objects/2,
  predicates/1,
  atomic_invariants/1,
  substate_classes/3,
  method/6,
  operator/4,
  implied_invariant/2,
  inconsistent_constraint/1.
incr_op_num:- 
   retract(op_num(N)),
   N1 is N+1,
   assertz(op_num(N1)).
   
:- dynamic is_hierarchy/1.      % the domain is hierarchy or not 
:- dynamic odds_in_subset_substates/3. %save the substates have subsets
:- dynamic max_length/1,lowest_score/1.

:- dynamic 
    objectsD/2, solved_node/2, current_num/2,
    gpred/2,gsstates/3,
    sum/1.
    
/*
env_retractall(G):- retractall(G).
env_retract(G):- retract(G).
env_assert(G):- assertz(G).	
env_asserta(G):- asserta(G).	
env_call(G):- call(G).
*/

with_domain_preds(Pred1):-
 maplist(Pred1,
   [domain_name/1,
    method/6,
    atomic_invariants/1,
    inconsistent_constraint/1,
    implied_invariant/2,
    objects/2,
    operator/4,
    predicates/1,
    sorts/2,
    substate_classes/3]).
    
:- with_domain_preds(abolish).    
:- with_domain_preds(multifile).
:- with_domain_preds(dynamic).

show_result_and_clean(F,Id,Sol,TNLst):-
   %ignore(solution_file(F)),
	tell(user),
	format('~NSOLUTION for TASK ~w~n',[Id]),
	display_sol(Sol),
        %write_out_test_data('.preClean',Id),
      %  write('END FILE'),nl,nl,
	nop((reverse(TNLst,TNForward), display_details(TNForward), write('END PLANNER RESULT'))),
	told,
	clean_problem.        

write_out_test_data(_MoreID,_Id):-!.
write_out_test_data(MoreID,Id):-
  must((    
    (var(Id)->statistics(walltime,[Id,_]);true),
    (push_env_ctx-> A= pushed_ ; A=nonpushed_ ),
    atom_concat(A,Id,TN),atom_concat(TN,MoreID,TTN),
      lws(TTN))),!.

display_details([]):-!.
display_details([H|T]):-!,display_details(H),display_details(T),!.
display_details(tn(TN,Name,Pre,Post,Temp,Dec)):-
%    write('method::Description:'),write(';'),
    nl,write('BEGIN METHOD'),nl,write(TN),write(';'),
    nl,write('Name:'),write(Name),write(';'),
    nl,write('Pre-condition:'),write(Pre),write(';'),
%    write('Index Transitions:'),write(Pre),write('=>'),write(Post1),write(';'),
    nl,write('Index Transitions:'),write('=>'),write(Post),write(';'),
%    write('Static:'),write(';'),
    nl,write('Temporal Constraints:'),write(Temp),write(';'),
    nl,write('Decomposition:'),write(Dec),write(';'),
    nl.

display_details(H):-dmsg((display_details:-H)).




















/* HyHTN planning: do preprocess first  */
/* make all method and operators primitive */
%:-use_module(library(system)).

% :- unknown(error,fail).

:-dynamic my_stats/1. 


:-multifile(on_call_decl_hyhtn/0).
:-export(on_call_decl_hyhtn/0).
% Tasks
on_call_decl_hyhtn :- decl_env_mpred_dom([kb(dom,tasks),stubType(dyn)], ( htn_task/3, planner_task/3, planner_task_slow/3 )).

on_call_decl_hyhtn :- decl_env_mpred_dom([stubType(dyn),kb(dom,cache)],(temp_assertIndivConds/1)). % Used for grounding operators
on_call_decl_hyhtn :- decl_env_mpred_dom([stubType(dyn),kb(dom,cache)],(is_of_primitive_sort/2, is_of_sort/2)).
on_call_decl_hyhtn :- decl_env_mpred_dom([stubType(dyn),kb(dom,cache)],(methodC/7, opParent/6,operatorC/5,gOperator/3)).
on_call_decl_hyhtn :- decl_env_mpred_dom([stubType(dyn),kb(dom,cache)],(objectsC/2,objectsD/2,atomic_invariantsC/1)).% Used only dynamic objects
on_call_decl_hyhtn :- decl_env_mpred_dom([stubType(dyn),kb(dom,cache)],(objectsOfSort/2)).      % Used to store all objects of a sort
on_call_decl_hyhtn :- decl_env_mpred_dom([stubType(dyn),kb(dom,cache) /*stubType(with_pred(bb_op(_)))*/],(related_op/2, gsubstate_classes/3, gsstates/3)).  

on_call_decl_hyhtn :- decl_env_mpred_task([stubType(dyn),kb(node,cache)],(initial_state/1)). 
on_call_decl_hyhtn :- decl_env_mpred_task([stubType(dyn),kb(node,cache) /*stubType(with_pred(bb_op(_)))*/],(op_score/2)). 
on_call_decl_hyhtn :- decl_env_mpred_task([stubType(dyn),kb(node,cache)/*stubType(rec_db)*/],(node/5,final_node/1)).
on_call_decl_hyhtn :- decl_env_mpred_task([stubType(dyn),kb(node,cache)],(tp_goal/3,closed_node/6,solved_node/2, goal_related_search/1)). 
%on_call_decl_hyhtn :- decl_env_mpred_task([stubType(rec_db),kb(node,cache)],(goal_related/3)).
on_call_decl_hyhtn :- decl_env_mpred_task([stubType(dyn),kb(node,cache)],(goal_related/3)).
on_call_decl_hyhtn :- decl_env_mpred_task([stubType(dyn),kb(node,cache)],(current_num/2)).
on_call_decl_hyhtn :- decl_env_mpred_task([stubType(dyn),kb(node,cache)],(tn/6)). % Used to store full expanded steps
on_call_decl_hyhtn :- decl_env_mpred_task([stubType(dyn),kb(node,cache)],(tp_node/6)).
% on_call_decl_hyhtn :- decl_env_mpred_task([stubType(dyn),kb(node,cache)],(tp_node_cached/6)).
% on_call_decl_hyhtn :- decl_env_mpred_task([stubType(with_pred(gvar_list(tnodeSORTED))),kb(node,cache)],tp_node/6).

% Tasks
on_call_decl_hyhtn :- decl_env_mpred_dom([kb(dom,tasks),stubType(dyn)], ( htn_task/3, planner_task/3, planner_task_slow/3 )).

% Contents of a OCLh Domain
on_call_decl_hyhtn :-  
  decl_env_mpred_dom([kb(dom,file),stubType(dyn)],[domain_name/1,sorts/2,substate_classes/3,objects/2,predicates/1,inconsistent_constraint/1,atomic_invariants/1,
  implied_invariant/2,operator/4,
   % oper/4,
   method/6]).

:-export(call_decl_hyhtn/0).

% :- rtrace.

call_decl_hyhtn:-must(doall(on_call_decl_hyhtn)).

% :- % 
  %  call_decl_hyhtn.



%%% ON :- initialization( profiler(_,walltime) ).
%%% ON :- initialization(user:use_module(library(swi/pce_profile))).
% :- qcompile_libraries.


% :- rtrace.
tryff(Call):- predicate_property(Call,_),!,once(tryf((Call,assert(passed_test_try(Call))))),fail.
tryf(Call):- predicate_property(Call,_),!,catch(Call,E,dmsg(E = Call)).
trye(Call):- catch(Call,E,((dmsg(error(E , Call)),throw(trace),Call))).

:-dynamic(passed_test_try/1).
:-dynamic(testing_already/0).

check_passed_any:-not(not(passed_test_try(_))),nl,listing(passed_test_try/1).

ttm:-retractall(passed_test_try(_)),fail.
ttm:-testing_already,!.
ttm:-asserta(testing_already), make, retractall(testing_already),fail.


:-export(banner_party/2).
banner_party(E,BANNER):- 
  ansicall(yellow,(
      format("% xxxxxxxxxxxxxxx ~w xxxxxxxxxxxxxxxxxxx~n",[E]),            
      forall(t_l:doing(X),dmsg(E,doing(X))),
      dmsg5(E,BANNER), 
       format("% xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx~n",[]))).
      
      
subst_eq_h(A,B,C,D):- nd_subst(A,B,C,D),!.
subst_eq_h(A,B,C,D):- throw(trace), nd_subst(A,B,C,D),!.


:-export(term_expansion_alias/2).
term_expansion_alias(In,Out):-term_expansion_alias([],In,Out).
term_expansion_alias(Not,In,Out):-term_alias(I,O),not(member(I,Not)),subst_eq_h(In,I,O,M), In\=@=M,!, term_expansion_alias([I|Not],M,Out).
term_expansion_alias(_Not,InOut,InOut).
term_alias(cond,se).
term_alias(se,se).
term_alias(state,ss).
term_alias(trans,sc).
term_alias(ne,dif).
term_alias(neq,dif).
% term_alias(htn_task,planner_task).
term_alias(startOcl,start).
term_alias(startOCL,start).


%get_tasks(B, C, D) :-%show_call(get_env_ctx(A)),!, 
%  if_defined(get_tasks(_A, B, C, D)).


tasks:- 
 Call = get_tasks(N,Goal,State),
   setof(Call,Call,List),list_to_set(List,Set),!,
   env_info(kb(dom,file)),!,
   once((ignore(forall(member(Call,Set),
     locally(t_l:doing(tasks(N,Goal)),
      ((
      must(nonvar(Goal)),must(nonvar(State)),
      ignore(N=Goal),      
      must((term_expansion_alias((Goal:-State),(GGoal:-GState)))),
      must(nonvar(GGoal)),must(nonvar(GState)),
      banner_party(informational,('GOAL'(N):-GGoal)),
      must(((once( once(startOCL(GGoal,GState))*->banner_party(informational,'SUCCESSS');banner_party(error,'FAILUR!!!!!!!!!!!!!!!!!!!!!!!!!!!E')))))))))))).

t:- once(run_header_tests).

tt:-catch(ttm,E,dmsg(E)),!.

tt:-tryff((tasks)).
tt:-tryff((task1)).
tt:-tryff((task2)).
tt:-tryff((task3)).
tt:-tryff((task4)).
tt:-tryff((task5)).
tt:-tryff((task6)).
tt:-tryff((task7)).
tt:-tryff((task8)).
tt:-tryff((task9)).
tt:-tryff((task10)).
tt:-tryff((task11)).
tt:-tryff((task22)).
tt:-tryff((task33)).
tt:-tryff((task44)).
tt:-check_passed_any,!.


tdom:-tdom([htn1,ty,ocl1,r3]).
tdom2:-tdom([ocl2,htn2,htn3,htn4]).

tdomfile(F):-tdomcall(load_data_file(F)).
tdomcall(Call):- trye(once((env_clear_doms_and_tasks,trye(Call),tt))).

tdom([]):-!.
tdom([H|T]):- !, tdom(H),tdom(T).
tdom(H):- predicate_property(H,_),!, throw(trace),call(H).
tdom(F):- tdom1(F),!.
tdom(F):- expand_file_name(F,List),List\=[],!,tdom1(List).
tdom(F):- throw(tdom(F)).

tdom1([]):-!.
tdom1([H|T]):- !, tdom(H),tdom(T).
tdom1(F):- tdom2(F),!.
tdom1(FIn):- atom(FIn),tdom2(FIn).

tdom2(FIn):- tdom3(FIn),!.
tdom2(FIn):- atom_concat('domains_ocl/',FIn,F), tdom3(F),!.

tdom3(FIn):- tdom4(FIn).
tdom3(FIn):- atom_concat(FIn,'htn',F),tdom4(F).
tdom3(FIn):- atom_concat(FIn,'ocl',F),tdom4(F).

tdom4(F):- exists_file(F),!,tdomfile(F).


:- discontiguous(post_header_hook/0).
post_header_hook:-retractall(canDoTermExp).
% user:term_expansion(In,Out):- canDoTermExp,term_expansion_hyhtn(In,M),In\=@=M,expand_term(M,Out).
% user:goal_expansion(In,Out):- canDoTermExp,term_expansion_hyhtn(In,M),In\=@=M,expand_goal(M,Out).
post_header_hook:-asserta(canDoTermExp).

:-export(env_clear_doms_and_tasks/0).
env_clear_doms_and_tasks:- env_clear(kb(dom,file)),env_clear(kb(dom,tasks)),env_clear(kb(dom,cache)),!.
   
:- op(100,xfy,( /*ocluser*/ocl:('=>'))).

% :-set_prolog_flag(verbose_file_search,true).
post_header_hook:-set_prolog_flag(verbose_load,full).
post_header_hook:-use_module(library(lists)).

% :- must((current_op(P,FXY,(-)),arg(_,v(fy,fx),FXY),P =< 300)).
:- style_check(-singleton).
:- style_check(+discontiguous).


%post_header_hook:-use_module(library(system)).
:-if(exists_source(library(gui_tracer))).
post_header_hook:- user:use_module(library(gui_tracer)),catch(guitracer,_,true).
:-endif.
post_header:- !.
post_header:- dmsg(post_header),fail, forall(clause(post_header_hook,G),G). 


:- discontiguous(header_tests/0).

hyhtn_run_tests(Call) :- 
  statistics_runtime(InTime),  
  locally(doing(hyhtn_run_tests(Call)),
   call_cleanup(Call, 
  ((
 statistics_runtime(OutTime),
  Time is OutTime - InTime,
  banner_party(informational,runtests(Call) = time(Time)))))).
 
run_header_tests :- hyhtn_run_tests(forall(clause(header_tests,G),hyhtn_run_tests(G))).



%:- asserta(t_l:disable_px).

% htn_task(Id,Goal,Init):-planner_task(Id,Goal,Init).




%  ss class expressions
%  invariants:
%   atomic invariants
%   -ve invariants
%   +ve invariants
%  operators

:- multifile(planner_task/3).
:- dynamic(planner_task/3).
% planner_task(A,B,C):- htn_task(A,B,C).
:- multifile(htn_task/3).
:- dynamic(htn_task/3).

:-retractall(solution_file(_)).
:-asserta(solution_file('/pack/logicmoo_ec/test/domains_ocl/freds.out')).

% :-sleep(1).
% :-tell(user),run_header_tests.



lws:- listing(ocl:[method,
operator,implied_invariant,atomic_invariants,inconsistent_constraint,predicates,objects,substate_classes,sorts,domain_name,planner_task_slow,planner_task,
htn_task,tp_node,tn,current_num,goal_related,goal_related_search,solved_node,closed_node,tp_goal,final_node,node,op_score,gsstates,gsubstate_classes,related_op,
objectsOfSort,atomic_invariantsC,objectsD,objectsC,gOperator,operatorC,opParent,methodC,is_of_sort,is_of_primitive_sort,temp_assertIndivConds]).

lws(F):-tell(F),lws,told.

:-export(test_ocl/1).
:- dynamic(unload_ocl/1).

time_as(Goal):- sformat(Name,'~w',[Goal]),time_as(Name,Goal).
time_as(Name,Goal):-
  statistics(runtime,[CP,_]),
  statistics(walltime,[WT,_]),
  notrace(Goal),
   statistics(runtime,[CPE,_]),
   statistics(walltime,[WTE,_]),
   RTIM is (CPE-CP) /1000,
   WTIM is (WTE-WT) /1000,
   format(user_error,'~N~n~w: ~f (wall ~f)~n',[Name,RTIM,WTIM]),!.

test_ocl:- update_changed_files,   
   forall(time(hyhtn_new_solve(_N)),nl).

test_ocl(File):- forall(filematch(File,FM),test_ocl0(FM)).

test_ocl0(File):- !,
  ignore(forall(retract(unload_ocl(Was)),unload_file(Was))),
  with_domain_preds(abolish),
  consult(File),
  asserta(unload_ocl(File)),!,
  test_ocl.

test_ocl0(File):- 
  time(locally(t_l:doing(test_ocl(File)), 
   once((env_clear_doms_and_tasks,clean_problem,l_file(File),tasks)))).

header_tests :-test_ocl('domains_ocl/*.ocl').
  
:-export(rr/0).
:-export(rr1/0).
:-export(t1/0).
:-export(t2/0).
rr:- test_ocl('domains_ocl/chameleonWorld.ocl').
rr1:- test_ocl('domains_ocl/translog.ocl').

t1:- test_ocl('test/domains_ocl/translogLM.pl').
t2:- test_ocl('test/domains_ocl/translogLM4.ocl').
t3:- test_ocl('test/domains_ocl/tyreLM.ocl').
t4:- test_ocl('test/domains_ocl/translog.ocl').
:- ensure_loaded(library(logicmoo_common)).
:- fixup_exports.

%:- include(translog4).

%:-rr.




planner_failure(Why,Info):-dmsg(error,Why-Info),banner_party(error,'FAILURE_PLANNER'),print_message(error,'FAILURE_PLANNER'(Why,Info)),!. %sleep(2).

:-thread_local t_l:doing/1.

statistics_runtime(CP):-statistics(runtime,[_,CP0]), (CP0==0 -> CP= 0.0000000000001 ; CP is (CP0/1000)) .  % runtime WAS process_cputime
statistics_walltime(CP):-statistics(walltime,[_,CP0]), (CP0==0 -> CP= 0.0000000000001 ; CP is (CP0/1000)) .  % runtime WAS process_cputime


