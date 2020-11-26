/*------------------------op_syn_check.pl---------------------------

checking operators:
1. every sort in a operator must be defined 
2. every  predicate in a operator must be defined in predicates 
3. the right hand side of a state transition must in substate_classes
 
-------------------------------------------------------------------*/

start_check_op :-
      retract_all_new_state,
      nl,write('  **checking operators syntax'), nl,
      get_sorts(_,_,Allsorts),
      predicates(Allpred),
      append_substates,
      check_op(Allsorts,Allpred),
      check_complete,!.
        	
check_complete :-
      nl,write('Check complete'),nl,!.

check_op(Allsorts,Allpred):-
      operator(Name,Pre,Nec,Cond),
      Name =..[OpName|Vars],
      nl,write('  **checking operator: '), write(OpName), nl,
         % cross checking  operators and sorts
      cross_op_sorts(Pre,Nec,Cond,Allsorts),
         % cross checking operators and predicates
      cross_op_pred(Pre,Nec,Cond,Allpred),
         % cross checking operators and substate_classes
      cross_op_state(Nec,Cond),
      fail.
check_op(Allsorts,Allpred).

% checking sorts in operators are defined
%----------------------------------------
cross_op_sorts(_,_,_,[]):-!.
cross_op_sorts(Pre,Nec,Cond,All):-
      cross_se_sorts(Pre,All),
      cross_sc_sorts(Nec,All),
      cross_sc_sorts(Cond,All),!.

cross_se_sorts([],_):-!.
cross_se_sorts([se(Sort,_,_)|TList],All):-
      not(not(member(Sort,All))),
      cross_se_sorts(TList,All),!.
cross_se_sorts([se(Sort,_,_)|TList],All):-
      not(member(Sort,All)),
      nl, write('   !! warning: sort '), write(Sort),
      write(' not in sort defn'),nl,
      cross_se_sorts(TList,All),!.

cross_sc_sorts([],_):-!.
cross_sc_sorts([sc(Sort,_,_)|TList],All):-
      not(not(member(Sort,All))),
      cross_sc_sorts(TList,All),!.
cross_sc_sorts([sc(Sort,_,_)|TList],All):-
      not(member(Sort,All)),
      nl, write('   !! warning: sort '), write(Sort),
      write(' not in sort defn'),nl,
      cross_sc_sorts(TList,All),!.


% checking predicates in operators are defined
%----------------------------------------
cross_op_pred(Pre,Nec,Cond,[]):-!.
cross_op_pred(Pre,Nec,Cond,Allpred):-
      cross_se_pred(Pre,Allpred),
      cross_sc_pred(Nec,Allpred),
      cross_sc_pred(Cond,Allpred),!.

cross_se_pred([],_):-!.
cross_se_pred([se(_,_,States)|TList],Allpred):-
      check_pred(States,Allpred),
      cross_se_pred(TList,Allpred),!.

cross_sc_pred([],_):-!.
cross_sc_pred([sc(_,_,Lhs=>Rhs)|TList],Allpred):-
      check_pred(Lhs,Allpred),
      check_pred(Rhs,Allpred),
      cross_sc_pred(TList,Allpred),!.

check_pred([],_):-!.
check_pred([Pred|TList],Allpred):-
      not(not(member(Pred,Allpred))),
      check_pred(TList,Allpred),!. 
check_pred([Pred|TList],Allpred):-
      not(not(is_build_in_pred(Pred))),
      check_pred(TList,Allpred),!. 
check_pred([Pred|TList],Allpred):-
      Pred=..[NPred|Vars],
      nl, write('   !! warning: predicate '), write(NPred),
      write(' not in predicates defn'),nl,
      check_pred(TList,Allpred),!. 

% cross checking operators and substate_classes
% ---------------------------------------------------------
% the substate_classes of a sort can be defined in itself 
% or mixed with its ancestor sorts' the substate_classes 
% the check is check the rhs of Operator with its substate_classes
% and  new_state(Sort,State)
%-----------------------------------------------------------

cross_op_state(Nec,Cond):-
      check_rhs(Nec),
      check_rhs(Cond),!.

check_rhs([]):-!.
check_rhs([sc(Sort,_,Lhs=>Rhs)|TList]):-
      check_rhs1(Sort,Rhs),
      check_rhs(TList),!.

check_rhs1(Sort,[]):-!.
check_rhs1(Sort,Rhs):-
      substate_classes(Sort,_,Subs),
      check_substate(Subs,Rhs,Rhs_Sub),
      check_newstate(Sort,Rhs_Sub),!.
check_rhs1(Sort,Rhs):-
      check_newstate(Sort,Rhs),!.
check_rhs1(Sort,Rhs):-
      nl, write('   !! warning: the rhs '), write(Rhs),
      write(' not in substate_classes'),nl,!.

check_substate([HSubs|TSubs],Rhs,Rhs_Sub):-
      is_a_subset(HSubs,Rhs),
      subtract(Rhs,HSubs,Rhs_Sub).
check_substate([HSubs|TSubs],Rhs,Rhs_Sub):-
      check_substate(TSubs,Rhs,Rhs_Sub).

check_newstate(Sort,Sub):-
      new_state(Sort,State),
      check_substate(State,Sub,Rest),
      check_newstate(Sort,Rest).
check_newstate(Sort,[]):-!.

% assert the new_state(Sort,States) with Sort's ancesstor's substate_classes
% ------------------------------------------------
append_substates:-
      substate_classes(Sort, _, State),
      assert_substate(Sort,State),
      fail.
append_substates:-
      new_state(Sort, State),
      assert_new_substate(Sort,State),
      fail.
append_substates:-!.

assert_substate(Sort,State):-
      assert(new_state(Sort,State)),!.

assert_new_substate(Sort,State):-
      sorts(Sort,[HSub|TSub]),
      assert_new_substate1([HSub|TSub],State),
      assert_new_substate(HSub,State),!.

assert_new_substate1([],State):-!.
assert_new_substate1([HSub|TSub],State):-
      not(new_state(HSub,State)),
      assert(new_state(HSub,State)),
      assert_new_substate1(TSub,State),!.
assert_new_substate1([HSub|TSub],State):-
      assert_new_substate1(TSub,State),!.

retract_all_new_state:-
      retract(new_state(_,_)),
      retract_all_new_state.

retract_all_new_state:-!.
