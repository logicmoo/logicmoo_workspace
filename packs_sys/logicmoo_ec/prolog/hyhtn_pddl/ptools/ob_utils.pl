%   utilities/ob_utils

% changes April 00 for OCLv1.1
 
%assigns not,\+ to 'not'.
 

:- dynamic  numberofnodes/1, totalnodes/1, totalops/1,
activation/1, node_violations/1, 
totaltime/1,  my_statistics/1, path_count/1.

:- multifile  numberofnodes/1, node_violations/1.

not(X):- \+(X).
 
append([],L,L).
append([H|T],L,[H|Z]) :- append(T,L,Z).
 
% set_difference
% -------------------------
set_difference(A,B,C) :-
        length(A,X) ,
        length(B,Y) ,
        X > Y ,
        subtract(A,B,C),!.
set_difference(A,B,C) :-
        subtract(B,A,C),!.
 
% subtract(A,B,C): subtract B from A
% -------------------------------------
subtract([],_,[]) .
subtract([A|B],C,D) :-
        not(not(member(A,C))) ,
        subtract(B,C,D),!.
subtract([A|B],C,[A|D]) :-
        subtract(B,C,D),!.

% maximum no. in set
% -------------------------------------

max_in_list([X|Y],M) :- max_in_list1(Y,X,M),!.
max_in_list1([ ],M,M) :- !.
max_in_list1([X|Y],M,M1) :- 
       max_in_list1(Y,M,M2),
       if_then_else(X > M2, X = M1, M2 = M1),!.  

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

% u_set_append_llists: [[A,S],[A,W],[R,D]] -> [A,S,W,R,D]
% ------------------------------------------------------
u_set_append_llists(A,B) :-
        u_set_append_llists(A,[],B) .
u_set_append_llists([],L,L) .
u_set_append_llists([A|B],L_in,L_out) :-
        uninstan_set_append(L_in,A,L_tmp) ,
        u_set_append_llists(B,L_tmp,L_out) .
 
% pp/1: pretty print list on new lines
% -------------------------------------
pp(L) :- pp_list(L) .
pp_list([]) .
pp_list([H|T]) :-
        write(H) ,
        nl ,
        pp_list(T) .
 
 
% pp/2: pretty print list els on new lines (arg2 is amount of tabbing)
% -----------------------------------------------------------------------
pp([],_) .
pp([H|T],Tab) :-
        tab(Tab) , write(H) ,
        nl ,
        pp(T,Tab) .
 
% pp_list/2: pretty print list els on new lines (arg2 is amount of tabbing)
% --------------------------------------------------------------------------
pp_list([L],Tab) :-
        tab(Tab) ,
        write('[') ,
        write(L) , write(']') .
pp_list([L|L_rest],Tab) :-
        tab(Tab) ,
        write('[') ,
        write(L) , write(',') , nl ,
        pp_l(L_rest,Tab) .
pp_l([],_) :-
        write(']') .
pp_l([L],Tab) :-
        tab(Tab), write(L) , write(']') .
pp_l([L|L_rest],Tab) :-
        tab(Tab) , write(L) , write(',') , nl ,
        pp_l(L_rest,Tab) .
 
% dots
% ------------
dots :-
        tell(user) ,
        write('.') ,
        current_output(Stream) ,
        flush_output(Stream) .
 

% ---------------------------------------------------------------------------
write_out_list([A|B]) :-
        nl , write('['), write(A), write(' ,') ,
        write_ol(B) .
write_ol([A]) :-
        nl , write(A), write('] .') , nl .
write_ol([A|B]) :-
        nl , write(A), write(' ,') ,
        write_ol(B) .
 

% achiever: [ground s'state] --> operator that achieves it
% ----------------------------------------------------------------------------
% ASSUMPTION: SS is a full substate with NO static vars
all_poss_achievers(SS,O) :-
        ops(Ops),
        member(O,Ops),
        get_all_effects(O,BE) ,
        is_a_ss_of_one(SS,BE).

is_a_ss_of_one(SS,[B|T]) :-
        filter_list(B,is_a_dynamic_pred,BR),
        subtract(BR,SS,[]),
        subtract(SS,BR,[]), 
        !.
is_a_ss_of_one(SS,[B|T]) :-
        is_a_ss_of_one(SS,T),!.

get_all_statics(operator(_,B,C,D),AS) :-
        get_pre_states(B,[],BS),
        get_nec_cond_states(C,[],CS),
        get_nec_cond_states(D,[],DS),
        append(BS,CS,BC),
        append(BC,DS,BCD),
        filter_list(BCD, is_a_static_pred, AS),!.

get_pre_states([],States,States):-!.
get_pre_states([se(_,_,PS)|TP],LL,States):-
        append(LL,PS,States1),
        get_pre_states(TP,States1,States).
        
get_nec_cond_states([],States,States):-!.
get_nec_cond_states([sc(_,_,LHS=>RHS)|TP],LL,States):-
        append(LL,LHS,States1),
        append(States1,RHS,States2),
        get_nec_cond_states(TP,States2,States). 

get_all_effects(operator(A,B,C,D),BE) :-
% get the RHSs of the nec. slot
        get_RHS(operator(A,B,C,D),Necs),
% get the RHS of the cond. slot
        get_rhs_cond(D, Conds),
        append(Necs,Conds,BE),!.
/* TEST:

get_all_effects(operator(move(X,A,B),[],
[[[at_bag(X,A),ne(A,B)],[at_bag(X,B)]], [[billy], [goat]]], 
[[[at_thing(T,A), inside(T,X)],[at_thing(T,B), inside(T,X)]],[[tom],[harry]]] 
   ),XX).

XX = [[at_bag(X,B)],[goat],[at_thing(T,B),inside(T,X)],[harry]]

*/

consist_lee(Atomic_invs, A_List):-
        follow_lee(A_List, Atomic_invs),!.
follow_lee([],_) :- !.
follow_lee(_,[]) :- !.
follow_lee([ne(U,V)|R],A):-
        !,not(U == V),
        follow_lee(R,A),!.
follow_lee([Y|R],A):-
        member(Y,A),
        follow_lee(R,A),!.


/* is X is in the atomic_invariants then by defn its a static. */
is_a_static_pred(X) :-
        atomic_invariants( A ),
        not( not( member(X,A) )),!.
is_a_static_pred(ne(_,_)) :-!.
is_a_static_pred(is_of_sort(_,_)) :-!.
is_a_static_pred(is_of_primitive_sort(_,_)) :-!.
 
is_a_dynamic_pred(X) :-
        not( is_a_static_pred(X) ),!.
 
/* `filter_list(X,condition(args),XO)'
 
 XO is reduced list */

filter_list([X|Rest],Op,[X|Rest1]) :-
        Op =.. OL,
        append(OL,[X],OL1),
        Pred =.. OL1,
        call(Pred),
        filter_list(Rest,Op,Rest1),!.
filter_list([_|Rest],Op,Rest1) :-
        filter_list(Rest,Op,Rest1),!.
filter_list([],_,[]).

% u_get_arg_pos: [A,S,D,F] * 3 --> D
%  --------------------------------
u_get_arg_pos(Args,Pos,A) :-
        not(var(Args)) ,
        u_get_arg_pos(Args,Pos,1,_,A) .
 
u_get_arg_pos([X|_],P,P,_,A) :-
        X==A.
u_get_arg_pos([_|B],P,P1,P3,A) :-
        P2 is P1+1 ,
        u_get_arg_pos(B,P,P2,P3,A) .
 
/* for precede .. */

% O partially in'd, SS totally in'd
nec_achieves(O ,SS) :-
   get_RHS(O,Rhs), 
% returns nec. effects
   eq_member(SS,Rhs),!.

eq_member(E,[Y|_]) :-
         E == Y,!.
eq_member(E,[_|T]) :-
        eq_member(E,T),!.

% O partially in'd, SS totally in'd
% The only way op is going to nec. clobber SS
% is if SS is equiv to the dynamic part of
% one of O.N's lhs's.
nec_clobbers(operator(_,_,P,_), SS) :-
       is_nec_covered_by(P,SS),!.

is_nec_covered_by([sc(_,_,LHS=>RHS)| _ ], SS ) :-
        filter_list(LHS,is_a_dynamic_pred,LR),
        is_equiv_list(SS,LR),!.
is_nec_covered_by([ _ | R ], SS ) :-
        is_nec_covered_by(R,SS),!.
is_equiv_list(SS,LR) :-
        uninstan_remove_all_el(LR,SS,[]),!.

% remove_all_el: [A,S,A,D,A,C,A] * A --> [S,D,C]
uninstan_remove_all_el([],_,[]) :- ! .
uninstan_remove_all_el([A|B],C,D) :-
        A == C ,
        uninstan_remove_all_el(B,C,D) .
uninstan_remove_all_el([A|B],C,[A|D]) :-
        uninstan_remove_all_el(B,C,D) .
 
/* test for nec_clobbers:

nec_clobbers(operator(move(briefcase,_56008,home),[],
 [[[at_bag(briefcase,_56008),ne(_56008,home)],[at_bag(briefcase,home)]]],
 [[[at_thing(_55977,_56008),inside(_55977,briefcase)],
   [at_thing(_55977,home),inside(_55977,briefcase)]]]), at_bag(briefcase,hi)).
SHOULD FAIL
nec_clobbers(operator(move(briefcase,_56008,home),[],
 [[[at_bag(briefcase,hi),ne(_56008,home)],[at_bag(briefcase,home)]]],
 [[[at_thing(_55977,_56008),inside(_55977,briefcase)],
   [at_thing(_55977,home),inside(_55977,briefcase)]]]), at_bag(briefcase,hi)).
SHOULD SUCCEED

*/

/*************************************************************/

/*  
Utilities for object-centred planner 4/11/96 
*/

% -------------------------------------------------------------------
% (1) operator access - object verson
% -------------------------------------------------------------------
% these assumes 4-slotter:
%       operator(name,[prevail],[necessary],[conditional])

% ops -> list
% ----------------------------------------------------------
ops(O) :-
        setof(operator(A,B,C,D),
                 A^B^C^D^operator(A,B,C,D),O).

% prevail conditions
% -------------------
get_prevail(O,P):-
	operator(O,P,_,_),!.
get_prevail(operator(_,P,_,_),P):-!.

% necessary effects slot: nec
% ------------------------
% case where O is the name of an operator..
necessary_slot(O,P1):-
	operator(O,_,P1,_),!.

% case where O is the operator struture ..
necessary_slot(operator(_,_,P1,_),P1):-!.

% conditional_effects
% ----------------------
conditional_effects(O,C):-
	operator(O,_,_,C),!.
conditional_effects(operator(_,_,_,C),C):-!.

% a new version of u_pres_prec for use by precede
uo_pres_prec(Op,P_prec) :-
	preconds(Op, P),	
	get_LHS(Op, Del),
	pres_p(P,Del,P_Prec).

 
pres_p([],_,[]) .
pres_p([P|Q],R,S) :-
        not(not(member(P,R))) ,
        pres_p(Q,R,S) .
pres_p([P|Q],R,[P|S]) :-
        pres_p(Q,R,S) .


preconds(operator(_,Pr,N,_),P):-
	%append(Pr,[],P1),
        lefths(N,[], Lhs),
        append(Lhs,Pr,P),!.


get_precons([O,Pr,N,C],P):-
        %append(Pr,[],P1),
        lefths(N,[], Lhs),
        append(Lhs,Pr,P),!.

% owns(Sort,Predicate).
owns(X,N) :- 
   substate_classes(X,L),
   member(ML,L),
   member(M,ML),
   M =.. [N|_].

% for oplan - puts precons of an operator into [[[substate]]] format

get_precons_in_goalform([O,Pr,N,C],Prg):-
	get_precons([O,Pr,N,C], Ps),
	prev2goal(Ps,[], Prg),
	!.

prev2goal([H|T],L,Prg):-
	append([[H]],L,Ltmp),
	prev2goal(T,Ltmp,Prg),!.
prev2goal([],P,P).

%for p_oplan - puts precons of an op into [[[substate],Requiring_OP],[[s],RO]]


get_precons_in_goalform2(Op_id, [O,P,N,C], Prg):-
	%get_LHS(O, Ps),
	get_precons([O,P,N,C],Ps),
	add_op_name(Op_id, Ps, [], Prg),!.

add_op_name(O,[HPs|TPs], L, Prg):-
	append([HPs], [O], HPO),
	append([HPO],L,Ltmp),
	add_op_name(O, TPs, Ltmp, Prg),!.
add_op_name(O,[],P,P).
	
% for POplanner if the goal's been achieved by cond effect, put
% lhs of cond into goal format (ie treat as Precons to form
% new subgoals)
	
get_cond_in_goalform2(Goal, Op_id, C, Precons):-
	relevant_cond(Goal, C, Crel),
	%lefths(C, [], LCs),
	%add_op_name(Op_id, LCs, [], Precons),!.
	add_op_name(Op_id, Crel, [], Precons),!.


relevant_cond(Goal, [HCond|T], Cond_out):-
	not(not(member(Goal, HCond))),
	lefths([HCond],[], Cond_out).
relevant_cond(Goal, [HCond|T], Cond_out):-
	relevant_cond(Goal, T, Cond_out).
	
%returns Left hand side of necessary effects (part of the precons)

get_LHS(M,Lhs):-
        necessary_slot(M, P1),
        lefths(P1,[], Lhs),!.

lefths([], L,L).
lefths([sc(_,_,Lh=>Rh)|T],L, Lhs):-
        append([Lh], L, Ltmp),
        lefths(T, Ltmp, Lhs),!.


% takes an op and returns right hand side of necessary effects (adds)
 
get_RHS(O,Rhs):-
        necessary_slot(O, P1),
        righths(P1, [], Rhs),!.
 
righths([], L,L).
righths([sc(_,_,Lh=>Rh)|T], L, Rhs):-
        append([Rh], L, Ltmp),
        righths(T, Ltmp, Rhs),!.
 
% special case required for init operator

primary_effects(init,_,Nec,_, PE):-
        righths(Nec,[], PE),!.

primary_effects(operator(O,_,Nec,_), PE):-
	primary_effect(Nec, PE),!.

primary_effect(Nec, PE):-
	righths(Nec, [], Rhs),
	rth(Rhs, PE),!.
	
	
rth([H|T],H).


check_and_match([HRhs|TRhs], G):-
	ne_connect_check(HRhs, HRO),
	ne_connect_check(G,GO),
	GO = HRO.
check_and_match([HRhs|TRhs], G):-
	check_and_match(TRhs, G).


% gets all possible precons - ie prevails, lhs of Nec and lhs of Cond

get_all_precons([O,P,N,C], Precons):-
	get_precons([O,P,N,C], Pres),
	get_lhs_cond(C, Lhs),
	append(Pres, Lhs, Precons).

% gets all possible adds - ie rhs of Nec and rhs of Cond

get_all_rhs([O,P,N,C], Rhs):-
	get_RHS(O,RHS),
	get_rhs_cond(C, CRhs),
	append(RHS, CRhs, Rhs),!.


% from cond effects gets rhs -

get_rhs_cond(Cond, Rcs):-
	righths(Cond, [], Rcs),!.

% from cond effects gets lhs

get_lhs_cond(Cond, Lcs):-
	lefths(Cond, [], Lcs).


get_op_name([Op,[O,P,N,C]],O).

% works out if the achiever is the nec effects of the cond effects, so
% we know what to add to the goal list

which_achiever(Gl, [O,P,N,C], Which):-
	righths(N, [], Rn),
	not(not(member(Gl,Rn))),
	Which = nec.

which_achiever(Gl, [O,P,N,C], Which):-
	righths(N, [], Rn),
	ne_connect_check(Rn, R),
	not(not(member(Gl,R))),
	Which = nec.
% also need to consider the case where we've done a partial match

which_achiever(Gl, [O,P,N,C], Which):-
	righths(N, [], Rn),
	head(Gl, G),
	flatten(Rn,[],Rn_flat),
	not(not(member(G, Rn_flat))),
	Which = nec.


which_achiever(Gl, [O,P,N,C], Which):-
	get_rhs_cond(C, Rcs),

% TLM added Oct 98
        ne_connect_check(Rcs,RR),
	not(not(member(Gl, RR))),
	Which = cond,!.




% version to deal with substate in this format [[a],[b]] 
% ie a lhs or rhs


ne_connect_check([], []):-!.

ne_connect_check([HPE|TPE], [PEO|Pout]) :-
	filter_list(HPE,is_a_dynamic_pred,PEO),
	ne_connect_check(TPE, Pout),!.


% no ne or connect to remove, just return substate

ne_connect_check(PE, PEO):-
        filter_list(PE,is_a_dynamic_pred,PEO),!.


% bit outdated but keep this for ptool7 and tool7
% need to replace with ne_connect_check which
% uses filter list

check_for_ne(CH, NE, CHO):-
        member(ne(X,Y), CH),
        tail(CH, NE),
        removeL(NE, CH, CHO),!.
	

% test
% prev2goal([[a],[b],[c]],[],Prg).

% sort_lists: [[a,s,d,e],[e,r], ..] -> [[e,r],[a,s,d,e],..]
% -----------
sort_lists(L_in,L_out) :-
        get_plan_keys(L_in,K) ,
        keysort(K,S) ,
        remove_keys(S,L_out) .


get_plan_keys([],[]) .
get_plan_keys([P|Q],[Key-P|R]) :-
        length(P,Key) ,
        get_plan_keys(Q,R) .

remove_keys([],[]) .
remove_keys([Key-P|Q],[P|R]) :-
        remove_keys(Q,R) .



% version for po planner
% goal_sort_lists: [[[a],[b,c], op1],[[a],[b], op2]]
% goal_sort_lists([[[a],[b,c],op1],[[a],[b],op3],[[f],op2]],L).
 
% L = [[[f],op2],[[a],[b],op3],[[a],[b,c],op1]]
% ------------------	
goal_sort_lists(L,L1):- swap(L,L2),!,
			goal_sort_lists(L2,L1).
goal_sort_lists(L,L).

swap([X,Y|R],[X|T]) :- get_total_length([X,Y], Total,Total1),
			Total =< Total1,!,
			swap([Y|R],T).
swap([X,Y|R],[Y|T]) :- get_total_length([X,Y], Total,Total1),
			Total > Total1,!,
			(swap([X|R],T),!;T=[X|R]).

	
get_total_length([HL_in|TLin],  Total, Total1):-
	remove_Aneed(HL_in, Hl_in),
	get_goal_keys(Hl_in, N,Total),
	get_total_length(TLin, Total1, Total),!.
get_total_length([],Total1,Total):-!.


% this works
get_goal_keys([],Key,Total):-
	Key is 0,
	Total is 0.
get_goal_keys([P|Q],Key,Total) :-
	length(P,Key),
	get_goal_keys(Q,L,Total1),
	Total is Key + Total1.


% goal_sort_lists2: [[[a],[b,c]],[[a],[b]]] -> [[[a],[b]],[[a],[b,c]]]
% goal_sort_lists2([[[a],[b,c]],[[a],[b]],[[f]]],L).
% L = [[[f]],[[a],[b]],[[a],[b,c]]]
% ------------------
% need 2 versions to cope with 2 different goal formats
% this version is for total order planner
% ie this format goal_sort_lists([[[a],[b,c]],[[a],[b]]],P).
 
goal_sort_lists2(L,L1):- swap2(L,L2),!,
                        goal_sort_lists(L2,L1).
goal_sort_lists2(L,L).
 
swap2([X,Y|R],[X|T]) :- get_total_length2([X,Y], Total,Total1),
                        Total =< Total1,!,
                        swap2([Y|R],T).
swap2([X,Y|R],[Y|T]) :- get_total_length2([X,Y], Total,Total1),
                        Total > Total1,!,
                        (swap2([X|R],T),!;T=[X|R]).
 
get_total_length2([HL_in|TLin],  Total, Total1):-
        get_goal_keys(HL_in, N,Total),
        get_total_length2(TLin, Total1, Total),!.
get_total_length2([],Total1,Total):-!.

%test goal_sort_lists([[[a],[b,c]],[[a],[b]]],P).
%test goal_sort_lists([[[a],[b,c],goal],[[d],[e],op1]],P).
%test goal_sort_lists([[[a,b,c],goal],[[d,e],op1],[[f],op2]],P).

%[[a],[b,c],goal]
remove_Aneed(HL_in, Hl_in):-
	get_last(HL_in, Aneed),
	remove_el(HL_in, Aneed, Hl_in),!.

%intersection of state and goal [[]] ,[[[ ]]]
inter_sect(State, [HGoal|TGoal],L, Int):-
	i(State, HGoal, L1),
	append(L1,L,Ltmp),
	inter_sect(State, TGoal, Ltmp, Int).
	
inter_sect(State, [], L,L).

all_goals_solved([],S):-!.

% doesn't allow for static preds 	
all_goals_solved([HG|Goals],State):-
	flatten(HG,[],G),
	member(G,State),!,
	all_goals_solved(Goals, State),!.	

	
%check for static preds
all_goals_solved([HG|Goals],State):-
	sort_id(HG, Sort),!,
	sort_object(Sort,HG,Obj),
	ob_in_state(Obj,State,Ob_State),
	filter_list(Ob_State,is_a_dynamic_pred,SO),
	flatten(HG,[],G),
	member(G,[SO]),
	all_goals_solved(Goals, State),!.
	

/*sort_id(HG, Sort),
	head(Sort, Srt),
	substate_classes(Srt, S_classes),
	member(X, S_classes),
	member(

*/


		


% hierarchy utilities 
%  ----------------------------------------------------------------------- 

%  make list copies of ops
% ------------------------
copyOps :- collectOps(OPS), copyOps1(OPS),!.

collectOps( [ operator(A,B,C,D) |  R ])   :-
	retract( operator(A,B,C,D) ),
	collectOps(R).
collectOps([]).

copyOps1( 
		[operator(N, PA, NA, CondA) | Rest] ) :-
copyOps1(Rest).
copyOps1([]).
/************************************************/

listtoand([],nil).  /* could just have one of these but ordering matters */
listtoand([X],X) :- !.
listtoand([X|Y],X&T) :- !,listtoand(Y,T).
andtolist(nil,[]).
andtolist(X&Y,[X|Z]) :-!, andtolist(Y,Z).
andtolist(X,[X]) :- !.



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
        remove_el(B,C,D),!.


% remove_els
% ----------------------------------
remove_els(A,[],A) :- !.
remove_els(L,[A|B],N) :-
        remove_el(L,A,M) ,
        remove_els(M,B,N),!.


% sometimes the goal will have a stat pred, sometimes it won't
% sometimes the equivalent pred in the state will have a stat
% pred and sometimes it won't -so need to cover all eventualities
% as otherwise will be trying to achieve already achieved goals

goal_list_take([H|T],S, L, LO):-
        list_take(H,S,L1),
        not(L1 = []),
	not(goal_stats(H,S)),
	%stats_in_goal(H),
	%all_sorts(H, Sort,O),
	%ob_in_state(O, S, ObState),
	%not(sub_in_goal(ObState, H, D)),
	%flatten(H,[],Hflat),
	%filter_list(Hflat,is_a_dynamic_pred,HO),
	%list_take([HO],S,L1),
	%not(L1 = []),
	append([L1],L,L2),
	goal_list_take(T,S,L2,LO).

% if result of list_take is []
goal_list_take([H|T],S, L, LO):-
	goal_list_take(T,S,L,LO).

goal_list_take([],S,Ll,Ll):-!.

goal_stats(Goal, State):-
	stats_in_goal(Goal),
	all_sorts(Goal,Sort,Object),
	ob_in_state(Object,State, Obstate),
	sub_in_goal(Obstate,Goal,D).

stats_in_goal(Goal):-
	flatten(Goal,[],Gflat),
	filter_list(Gflat,is_a_static_pred,GO),
	not(GO = []),!.


/* orig verison
goal_list_take([H|T],S, L, LO):-
        list_take(H,S,L1),
        not(L1 = []),
        append([L1],L,L2),
        goal_list_take(T,S,L2,LO).

% if result of list_take is []
goal_list_take([H|T],S, L, LO):-
        goal_list_take(T,S,L,LO).

goal_list_take([],S,Ll,Ll).
*/


new_f(O,S) :- usemacros(off),
		usemacroend(off),
		operator( O,
			  P,
			  _,
			  C),
	  flatten(P,[],P1),
	  listtoand(P1, P2),
	  hold(P2,S).
	  
new_f(O,S) :- usemacros(on),
		operator( O,
			  P,
			  _,
			  C),
	  flatten(P,[],P1),
	  listtoand(P1, P2),
	  hold(P,S).
	  
new_f(O,S) :- usemacros(off),
		usemacroend(on),
		operator( O,
			 P,
			 _,
			 C),
	 flatten(P,[],P1),
	 listtoand(P1, P2),
	 hold(P,S).
	 

/* finds instantiations of <arg1>& S s.t. <arg1> follows from S -
   or if it is always true in that context. On bactracking this
   will try for an alternate intantiation of course, but note that 
   the most general S will not nec. appear first !!

BUG ***** 7/95 - if ne(X,Y) are on the front of a term 
they will make no difference!! Use put_ne_at_end(X,XE) to shuffle to end */

hold(nil&X,Y) :- hold(X,Y).   /* nil IS REALLY TRUE  */
hold(nil,_).

hold(ne(U,V)&Y,S) :-!, not(U == V),hold(Y,S).
hold(ne(U,V),_) :-!, not(U == V).
hold(not(A)&Y,S) :- 
	! , not(elem(A,S)) , hold(Y,S) .
hold(not(A),S) :- 
	! , not(elem(A,S)) .
hold(X&Y,S) :- elem(X,S),hold(Y,S).
hold(X,S) :- elem(X,S).


% list of lists -> list

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


nlp_remove_list([D|Del],State,New) :-
        remove_ALL(D,State,New2),
        nlp_remove_list(Del,New2,New),!.
nlp_remove_list([_|Del],State,New) :-
        nlp_remove_list(Del,State,New),!.
nlp_remove_list([],New,New) :- !.	
remove_ALL(_,[],[]).
remove_ALL(E1,[E1|T],T1) :- remove_ALL(E1,T,T1),!.
remove_ALL(E1,[X|T1],[X|T2]) :- remove_ALL(E1,T1,T2),!.

add_list(Add,New1,New) :-
        append(Add,New1,New).
        
%list_to_set(New2,   New),!.

/* This procedure changes a list to a set */

list_to_set([],[]) :- !.
list_to_set([L|T],S) :- member(L,T),!,list_to_set(T,S),!.
list_to_set([L|T],[L|T1]) :- list_to_set(T,T1),!.

% hold_list
% -----------------------------------
% amended from hold which expects &'ed predicates
%
hold_list([],_) .
hold_list([ne(U,V)|Y],S) :-
        !,
        not(U == V),
        hold_list(Y,S).
hold_list(ne(U,V),_) :-
        !,
        not(U == V).
hold_list([X|Y],S) :-
        %not(not(member(X,S))),
	member(X,S),
        hold_list(Y,S).
hold_list(X,S) :-
        %not(not(member(X,S))).
 	member(X,S).
 
% u_hold_list: version for uninstan preds
% ----------------------------------------
u_hold_list([],_) .
u_hold_list([ne(U,V)|Y],S) :-
        !,
        not(U == V),
        u_hold_list(Y,S).
u_hold_list(ne(U,V),_) :-
        !,
        not(U == V).
u_hold_list([X|Y],S) :-
        u_mem(X,S),
        u_hold_list(Y,S).
u_hold_list(X,S) :-
        u_mem(X,S).

% copy_pred: pred(Var1,Var2,...) --> pred(Varx,Vary,..)
% ---------------------------------------------------------------------------
copy_pred(P_in,P_out) :-
        P_in =..[H|T] ,
        length(T,L) ,
        length(T1,L) ,
        P_out =..[H|T1] .

copy_preds([],[]) .
copy_preds([P|P_rest],[Pl|R_rest]) :-
        list(P) ,
        copy_preds(P,Pl) ,
        copy_preds(P_rest,R_rest) .
copy_preds([P|P_rest],[R|R_rest]) :-
        copy_pred(P,R) ,
        copy_preds(P_rest,R_rest) .



% Required by PROPOSE
% copied from /caa/julie_arch/fm_archive/newfm/FM/utilities/utils

% instantiate_predicate: relies on sort
% instantiate_predicate(Pred) :-
%         infinite_sorts([]) ,            % flag for infinite on
%         sort_info(Pred,Sorts) ,
%         listtoand(Sorts,S) ,
%         frame(_,_,always:A,_) ,
%         hold(S,A) .
% instantiate_predicate(Pred) :-
%         achiever(Pred,Ach) ,
%         check(Ach,Ch) ,
%         i_instan(Ch) ,                  
% get first instan for infinite types        ! ,
%         frame(name:_,_,always:Always,_),
%         remove_fi(Ch,Fi) ,              
% remove infinite sorts and functions        listtoand(Fi,F) ,
%         hold(F,Always) .
%  
%                                         % for infinite
% instantiate_predicates([]) .
% instantiate_predicates([P|P_rest]) :-
%         instantiate_predicate(P) ,
%         instantiate_predicates(P_rest) .
% 

i_instan(C) :-                  % get first instan -- for infinite types
        remove_fi(C,Cfi) ,
        remove_els(C,Cfi,FI) ,
        frame(_,_,always:Al,_) ,
        listtoand(FI,FIa) ,
        hold(FIa,Al) , ! .
i_instan(C) :-
        andtolist(C,Ca) ,
        i_instan(Ca) .

member(X,[X|_]).
member(X,[_|L]) :- member(X,L).

% u_members
% ---------------
u_members([], _).
u_members([B], A) :- !,
        u_mem(B, A).
u_members([B|C], A) :- !,
        u_mem(B, A),
        u_members(C, A).

u_mem(_,[]) :- !,fail.
u_mem(A,[B|_]) :-
	A == B.
u_mem(A,[_|B]) :-
	u_mem(A,B).

% set_append: list * list -> list (no duplicates)
% -----------------------------------------------
set_append([], Z, Z):-! .
set_append([A|B], Z, C) :-
        not(not(member(A, Z))) ,
        set_append(B, Z, C),! .
set_append([A|B], Z, [A|C]) :-
        set_append(B, Z, C) .


% uninstan_set_append: el * list -> list
% ------------------------------------------
uninstan_set_append([],A,A) .
uninstan_set_append([A|B],C,D) :-
        u_mem(A,C) ,
        uninstan_set_append(B,C,D) .
uninstan_set_append([A|B],C,[A|D]) :-
        uninstan_set_append(B,C,D) .


% remove_duplicates
% ------------------
remove_duplicates(A,B) :-
        remove_duplicates(A,[],B),! .
remove_duplicates(A,A) .

remove_duplicates([],L,L) .
remove_duplicates([A|B],L_in,L_out) :-
        member(A,L_in) ,
        remove_duplicates(B,L_in,L_out) .
remove_duplicates([A|B],L_in,L_out) :-
        append(L_in,[A],L_tmp) ,
        remove_duplicates(B,L_tmp,L_out) .


%u_remove_duplicates (uninstan version)
% --------------------------------------

u_remove_duplicates(A,B) :-
        u_remove_duplicates(A,[],B),! .
u_remove_duplicates(A,A) .
 
u_remove_duplicates([],L,L) .
u_remove_duplicates([A|B],L_in,L_out) :-
        u_mem(A,L_in) ,
        u_remove_duplicates(B,L_in,L_out) .
u_remove_duplicates([A|B],L_in,L_out) :-
        append(L_in,[A],L_tmp) ,
        u_remove_duplicates(B,L_tmp,L_out) .

% list: [el1,el2, ...] --> bool
% -----------------------------
list(A) :-
        var(A) ,
        ! ,
        fail .
list(A) :-
        functor(A,'.',_).

/* mea search */
max_CPU(420000):-!.      /* milli seconds */
max_numberofnodes(5000):-!.
max_numberofnodes_per_goal(100):-! .
max_activations(20000):-!.    /* max. no of modes mea  can use */


cycle_stats(Y1) :-
        statistics(runtime,[_,CP]),
        (retract(my_statistics(Y)) ; Y = 0),
        Y1 is Y + CP,
        assert(my_statistics(Y1)),!.

path_stats(P1):-
	(retract(path_count(P)) ; P = 0),
	P1 is P + 1,
	assert(path_count(P1)),!.

clock(0).               /* increment after each task */
activation(0).          /* increment after each process finishes */
                        /* must be reset by driver */
totalnodes(0).          /* increment after each task */
totalops(0).            /* increment after each task */
totaltime(0).           /* increment after each task */
numberofnodes(0).	/* increment after each node expansion */
path_count(0).


/* ------------------------------------------------------------------------- */
adjust_NODES_ACTIVATION(Nn,Tn1) :-
                retract(numberofnodes(Nn)),
                assert(numberofnodes(0)),
                retract(totalnodes(Tn)),
                Tn1 is Tn + Nn,
                assert(totalnodes(Tn1)),
                retract(activation(_)),
                assert(activation(0)).

adjust_OP_COUNT(N) :-
                retract(totalops(T)),
                T1 is T + N,
                assert(totalops(T1)).

write_nodes(_, _,_,_,skip) :-
        gensym(task,T),
        statistics_file(FFF),
      tell(FFF),
	  write(T), write(' task skipped true in init! ') ,nl ,
        tell(user).
write_nodes(T, Nn, Tn1, Len, Stat) :-
        gensym(task,T), 
	nl,write(['no. of expanded nodes: ',Nn,'(total ',Tn1,')']),nl,
       solution_file(FFF),
	 tell(FFF),
        nl,
	write(['no. of expanded nodes: ',Nn,'(total ',Tn1,')']),nl,
       statistics_file(FF),  
	tell(FF),
	nl, write(T), tab(3), write(Stat),
        tab(3),write(Nn), tab(3), write(Tn1),tab(3),write(Len),
%        tell(user) ,

        % Aug 3rd 1993
        % extra stuff to handle diff call from unix
       /* (unix_flag(on) ,
        see('unix_out') ,
        get_last_term((I1,I2,I3,I4)) ,
        nl , tell(user) ,
        (I1=init,I2=init,I3=init,I4=init,Nodes=0,Ops=0,Cpu=0
        ;
        I1=[_,Nodes],I2=[_,Ops],I3=[_,Cpu]) ,
        N_total is Nodes+Nn,
        O_total is Len+Ops,
        assert(unix_term([Nn,N_total],[Len,O_total],[_,Cpu],Stat))
        ;
        true) ,
	*/
%       if_then(Stat = failure, (passiveq(Pa),write([nl,Pa,nl])) ),
        tell(user).


% ----------------------------------------------------------------------------
% (3) list operations
% ----------------------------------------------------------------------------

% head
% -----------
head([A|_],A) .

% tail
% -----------
tail([A],A) .
tail([_|A],B) :-
        tail(A,B) .

reverse(X,Y) :- reverse_x(X,[],Y),!.
reverse_x([],C,C).
reverse_x([H|T],C,R) :- reverse_x(T,[H|C],R).


elem(X,Y&_):- elem(X,Y).
elem(X,_&C):-!,elem(X,C).
elem(X,X).



vars_inL([X|Y]) :- ((X =.. Z,vars_inLl(Z)) ; vars_inL(Y)),!.
vars_inL(X) :- X =.. Z,vars_inLl(Z).
vars_inLl([H|T]) :- ( var(H) ; vars_inLl(T) ).


% get_last: [a,b,...,z] -->  z
% ------------------------------
get_last([A],A) .
get_last([_|A],B) :-
        get_last(A,B) .


% remove_list: list1 * list2 -> list1-list2
%---------------------------------------------
remove_list(A,[],A) :- ! .
remove_list(A,[D|E],C) :-
        remove_el(A,D,B) ,
        remove_list(B,E,C) .

remove_list2(A,[],A) :- ! .
remove_list2(A,[D|E],C) :-
        remove_el2(A,D,B) ,
        remove_list2(B,E,C) .

% remove_el: list * el -> list-el
% ----------------------------------
remove_el([],_,[]) :- ! .
remove_el([A|B],A,B) :- ! .
remove_el([A|B],C,[A|D]) :-
        remove_el(B,C,D) .

% ----------------------------------
removeL(El,[El|T],T) :- !.
removeL(E1,[X|T1],[X|T2]) :- removeL(E1,T1,T2).

removeLl([El],[[El]|T],T) :-!.
removeLl([El],[[X]|T1],[[X]|T2]) :- removeLl([El], T1, T2).

% append_llists: [[a,s],[r,d]] -> [a,s,r,d]
% -----------------------------------------
append_llists(A,B) :-
        append_llists(A,[],B) .

append_llists([],L,L) .
append_llists([A|B],L_in,L_out) :-
        append(L_in,A,L_tmp) ,
        append_llists(B,L_tmp,L_out) .


% set_append_llists: [[a,s],[a,w],[r,d]] -> [a,s,w,r,d]
% ------------------------------------------------------
set_append_llists(A,B) :-
        set_append_llists(A,[],B) .
set_append_llists([],L,L) .
set_append_llists([A|B],L_in,L_out) :-
        set_append(L_in,A,L_tmp) ,
        set_append_llists(B,L_tmp,L_out) .


% list_el_pos: [a,s,d,f] * 3 --> d
% --------------------------------
% for historical reasons (?) get_arg_pos is used by PRECEDE
% and PROPOSE
list_el_pos(List,Pos,El) :-
        get_arg_pos(List,Pos,El) .

% get_arg_pos: [a,s,d,f] * 3 --> d
%  --------------------------------
get_arg_pos(Args,Pos,A) :-
        get_arg_pos(Args,Pos,1,_,A) .

get_arg_pos([A|_],P,P,_,A) .
get_arg_pos([_|B],P,P1,P3,A) :-
        P2 is P1+1 ,
        get_arg_pos(B,P,P2,P3,A) .


% not/2
% -----------------------
not(A,B) :-
        call(A) ,
        call(B) , ! ,
        fail .
not(_,_).


% instan_copy_pred
% -------------------------------
instan_copy_pred(P_in,P_out) :-
        P_in =..[H|T_in] ,
        i_copy_pred(T_in,T_out) ,
        P_out =..[H|T_out] .
i_copy_pred([],[]) .
i_copy_pred([A|B],[A|C]) :-
        atom(A) ,
        i_copy_pred(B,C) .
i_copy_pred([_|B],[A|C]) :-
        i_copy_pred(B,C) .


i([],_,[]) :- !.        /* intersection for lists */
i([E|T],Y,[E|T1]) :-    /* NOTE: error.. e.g. i([x,y],[x],[]) succeeds!!*/
                    member(E,Y),
                    !,
                    i(T,Y,T1).
i([_|T],Y,Z) :- i(T,Y,Z).





get_op_names([[O,P,N,C]|TPsol], L, Ops):-
	append(L, [O], Ltmp),
	get_op_names(TPsol, Ltmp, Ops).

get_op_names([],O,O).


print_stats :-
        /* integer only works for sicstus */
        %(prolog_is(cic) ; prolog_is(sic) ),
        task_stats(CP), CPused is integer(CP/1000 + 0.5),
        retract(totaltime(Tn)),
        Tn1 is Tn + CP/1000,
        assert(totaltime(Tn1)),
        Tn2 is integer(Tn1 + 0.5),
        solution_file(FFF),
	  tell(FFF),
        write(['CPUsed=',CPused,' secs ','(total ',Tn2,')']),nl,
        statistics_file(FF),
        tell(FF),
        tab(3), write('CPU = '), write(CPused), tab(3), write(Tn2),
        nl , tell(user) ,

        % 3rd Aug -- added for UNIX stuff
        /*(unix_flag(on) ,
        retract(unix_term([Nn,N_total],[Len,O_total],[_,Cpu],Result)) ,
        C_total is Cpu+CPused ,
        % to handle case where true from init
        (Nn=0,Len=0,CPused=0,Result1=skip
                ;
        Result1=Result) ,
        assert(unix_term([Nn,N_total],[Len,O_total],[CPused,C_total],Result1))
        ;
        true) ,
	*/
        nl,tell(user),
        write(['CPUsed=',CPused,' secs ','(total ',Tn2,')']),nl,!.


task_stats(Y) :-
        retract(my_statistics(Y)),
        assert(my_statistics(0)),!.

% init_unix_flags: used by unix driver
% ------------------------------------
init_unix_flags :-
        (retract(unix_flag(on)) , assert(unix_flag(on))
        ;
        assert(unix_flag(off)) ) .

/* generate symbol predicate  (from file futile)*/

gensym(Root,Atom) :-
                        getnum(Root,Num),
                        name(Root,Name1),
                        name(Num,Name2),
                        append(Name1,Name2,Name),
                        name(Atom,Name).

getnum(Root,Num) :-
                        retract(current_num(Root,Num1)),!,
                        Num is Num1+1,
                        asserta(current_num(Root,Num)).

getnum(Root,1) :- asserta(current_num(Root,1)).

if_then(A,B) :- call(A),call(B),!.
if_then(_,_).
if_then_else(A,B,_) :- call(A),call(B),!.
if_then_else(_,_,C) :- call(C),!.


for_all_els([X|Rest],Op,I,O) :-
        Op =.. OL,
        append(OL,[X,I,I1],OL1),
        Pred =.. OL1,
        call(Pred),
        for_all_els(Rest,Op,I1,O).
for_all_els([],_,I,I).

% check the predicate name is a build in predicate or not
is_build_in_pred(ne(_,_)).
is_build_in_pred(is_of_sort(_,_)).
is_build_in_pred(is_of_primitive_sort(_,_)).
is_build_in_pred(before(_,_)).
       
is_build_in_op(achieve(_)).
