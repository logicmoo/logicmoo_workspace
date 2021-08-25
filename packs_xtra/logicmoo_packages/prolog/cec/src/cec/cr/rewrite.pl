%------------------------------------------------------------------------------%
% 		Clausal Rewriting process for Quintus Prolog	    
%
% formats of: 
%    c-rules:  c-rule( old/new, num, Left, [[Cond,Right]..], [CCond..] )
%    clauses are always lists [l1,...,ln]
%    LHS's t1 v ... v tn, with n>1, are expressed by lists [l1,...,ln]
%    History (H) of c-terms during rewriting : [[numcrule, Instd. LHS],...]
%------------------------------------------------------------------------------%


%------------------------------------------------------------------------------%
% computing sets of normal forms
%------------------------------------------------------------------------------%
nf_set( C,L ):- 
		nf_set1( C,[],L ),!.

nf_set1(C,H,L1):-	
		red_cont(C,H,L2),
		nf_set_list( L2,L1 ),!.
nf_set1( C,_, [C] ):-!.

nf_set_list( [], [] ):-!.
nf_set_list( [[C,H]|L1], L2 ):-
		nf_set1(C,H,L3),
		nf_set_list(L1,L4),
		vappend(L3,L4,L2),!.

%------------------------------------------------------------------------------%
% aplication of a c-rule to a clause
%------------------------------------------------------------------------------%
red_cont( ['true-bool'],_, [] ):- !.
red_cont( C1,H, L  ):-
		c_rule( _, Num_c_rule, Left, SR, Cs ),
		subterm_match(Left,C1,C2,'$right$'),
		\+(vmember([Num_c_rule,Left],H)),!,
		apply_set_to_context( C2, SR, H, L2 ),
		apply_compconds(C1,Cs,[[Num_c_rule,Left]|H],L1),
		vappend(L1,L2,L),!.

%------------------------------------------------------------------------------%
% applies the reductive part of the c-rule to the clause
%------------------------------------------------------------------------------%
apply_set_to_context( _, [], _, [] ):-!.
apply_set_to_context( C1, [[Cond,Right]|Rest], H, [[C4,H]|L]):-
		replace_all('$right$',Right,C1,C2),
		vappend(C2, Cond, C3),
		norm_bool( C3, C4 ),
		apply_set_to_context( C1, Rest, H , L),!.

%------------------------------------------------------------------------------%
% adds complementary conditions and history to the clause
%------------------------------------------------------------------------------%
apply_compconds(_,[],_,[]):-!.
apply_compconds(C,[Comp|Cs], NH, [[C2,NH]|L] ):-
		vappend( C, Comp, C1),
		norm_bool( C1, C2 ),
		apply_compconds(C,Cs,NH,L),!.


%------------------------------------------------------------------------------%
% subterm_match: replaces subterm of T that matches with Left by Vsubst
%------------------------------------------------------------------------------%
subterm_match( [L|LLeft], [C|CConds], [Vsubst|Rest], Vsubst):-!,
		and_match( [L|LLeft], [C|CConds], [], Linst, Rest),
		instantiate(Linst).
subterm_match(Left,T,Vsubst,Vsubst):-
		match(Left,T,[],L),
		instantiate(L),!.
subterm_match(S,T,NT,Vsubst) :-
		nonvar(T),
		T =.. [F|Args1],
		subterm_match_list(S,Args1,Args2,Vsubst),
		NT =.. [F|Args2].

and_match( [], Rest, L, L, Rest ):-!.

and_match( [Left|RLeft], Conds, L1, L2, Rest ):-
		vdelete(T,Conds,Conds1),
		match(Left,T, L1, L3),
		and_match(RLeft,Conds1, L3, L2, Rest).

subterm_match_list(S,[T|Rest],[NT|Rest],Vsubst):-
		nonvar(T),
		subterm_match(S,T,NT,Vsubst).
subterm_match_list(S,[T|Rest1],[T|Rest2],Vsubst):-
		subterm_match_list(S,Rest1,Rest2,Vsubst).

instantiate([]):-!.
instantiate([ [X,X]|Rest]):-
		instantiate(Rest),!.

match(Left,T,L,L1):-  
		var(Left),!,
		( ( vmember( E,L ), E =  [A,B],  A == Left, !, B==T, L1=L ) ;
		  ( L1 = [ [Left,T] |L], !                               ) ),!.
match(_,T,_,_) :-
		var(T),!,fail.
match(Left,T,L,L) :-
		atom(Left),!,
		Left == T,!.
match([A1|ARGS1],[A2|ARGS2],L1,L2 ):- !,
		match(A1,A2,L1,L3),!,
		match(ARGS1,ARGS2,L3,L2),!.
match(Left,T,L1,L2) :-
		functor(Left,F,N),!,
		functor(T,F,N),!,
		Left =.. [_|ARGLeft],!,
		T =.. [_|ARGT],!,
		match(ARGLeft,ARGT,L1,L2).

replace_all( Var,Subst,T,Subst ):- Var == T,!.
replace_all( _,_,T,T ):-atom(T),!.
replace_all( _,_,T,T ):-var(T),!.
replace_all( Var,Subst,[T1|L1],[T2|L2] ):-!,
		replace_all(Var,Subst,T1,T2),
		replace_all(Var,Subst,L1,L2),!.
replace_all( Var,Subst,T1,T2 ):-
		T1 =.. [F|Args1],
		replace_all(Var,Subst,Args1,Args2),!,
		T2 =.. [F|Args2],!.
%------------------------------------------------------------------------------%


