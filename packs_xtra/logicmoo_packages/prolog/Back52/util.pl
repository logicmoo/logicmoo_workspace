 
/************************************************************************/
/*           S O R T E D  - L I S T S   */
/************************************************************************/

b5sort_member_p(Member,[Member|_]) :- !.
b5sort_member_p(Member,[Head|Tail]) :-
   Member @> Head,
   !,
   b5sort_member_p(Member,Tail).

%% sort_select(Element,ListIn,ListOut) succeeds if Element is member of 
%% ListIn and deletes the first occurence of Element in ListIn with the
%% result ListOut.  -mf- 18.6.92 
b5sort_select(E,[E|EList],EList) :-
	!.
b5sort_select(E,[E1|EList],[E1|EList1]) :-
	b5sort_select(E,EList,EList1).
/*
b5sort_very_special(A,B,C,D) :-
	b5sort_intersect(A,B,C1),
	b5sort_difference(B,C1,D),
	C = [C1].
*/
b5sort_very_special([A1|_],[A1|B],A1,B) :- !. 
b5sort_very_special([A1|A],[B1|B],C,D) :-
   	A1 @< B1,!,
	b5sort_very_special(A,[B1|B],C,D).
b5sort_very_special([_A1|A],[B1|B],C,D) :-
   	%B1 @< A1,
	b5sort_very_special(A,[B1|B],C,D).



b5sort_intersect([],_,[]) :- !.
b5sort_intersect(_,[],[]) :- !.
b5sort_intersect([Head|Tail1],[Head|Tail2],[Head|Tail]) :-
   !,
   b5sort_intersect(Tail1,Tail2,Tail).
b5sort_intersect([Head1|Tail1],[Head2|Tail2],Intersect) :-
   Head1 @< Head2,
   !,
   b5sort_intersect(Tail1,[Head2|Tail2],Intersect).
b5sort_intersect(List1,[_|Tail2],Intersect) :-
   b5sort_intersect(List1,Tail2,Intersect).
 
  
b5sort_unify(List,[],List) :- !.
b5sort_unify([],List,List) :- !.
b5sort_unify([Head|Tail1],[Head|Tail2],[Head|Tail]) :-
   !,
   b5sort_unify(Tail1,Tail2,Tail).
b5sort_unify([Head1|Tail1],[Head2|Tail2],[Head1|Tail]) :-
   Head1 @< Head2,
   !,
   b5sort_unify(Tail1,[Head2|Tail2],Tail).
b5sort_unify(List1,[Head2|Tail2],[Head2|Tail]) :-
   b5sort_unify(List1,Tail2,Tail).

b5sort_unify_plus_one(List,[],Member,NewList) :-
   !,
   b5sort_unify(List,[Member],NewList).
b5sort_unify_plus_one([],List,Member,NewList) :-
   !,
   b5sort_unify(List,[Member],NewList).
b5sort_unify_plus_one([Head|Tail1],[Head|Tail2],Head,[Head|Tail]) :-
   !,b5sort_unify(Tail1,Tail2,Tail).
b5sort_unify_plus_one([Head|Tail1],List2,Head,[Head|Tail]) :-
   !,
   b5sort_unify(Tail1,List2,Tail).
b5sort_unify_plus_one(List1,[Head|Tail2],Head,[Head|Tail]) :-
   !,
   b5sort_unify(List1,Tail2,Tail).
b5sort_unify_plus_one([Head|Tail1],[Head|Tail2],Member,[Member|Tail]) :-
   Member @< Head,
   !,
   b5sort_unify([Head|Tail1],Tail2,Tail).
b5sort_unify_plus_one([Head|Tail1],[Head|Tail2],Member,[Head|Tail]) :-
   !,
   b5sort_unify_plus_one(Tail1,Tail2,Member,Tail).
b5sort_unify_plus_one([Head1|Tail1],[Head2|Tail2],Member,[Head1|Tail]) :-
   Head1 @< Head2,
   Head1 @< Member,
   !,
   b5sort_unify_plus_one(Tail1,[Head2|Tail2],Member,Tail).
b5sort_unify_plus_one([Head1|Tail1],[Head2|Tail2],Member,[Head2|Tail]) :-
   Head2 @< Head1,
   Head2 @< Member,
   !,
   b5sort_unify_plus_one([Head1|Tail1],Tail2,Member,Tail).
b5sort_unify_plus_one(List1,List2,Member,[Member|Tail]) :-
   b5sort_unify(List1,List2,Tail).
 
b5sort_difference([],_,[]) :- !.
b5sort_difference(List,[],List) :- !.
b5sort_difference([Head|Tail1],[Head|Tail2],Diff) :-
   !,
   b5sort_difference(Tail1,Tail2,Diff).
b5sort_difference([Head1|Tail1],[Head2|Tail2],[Head1|Diff]) :-
   Head1 @< Head2,
   !,
   b5sort_difference(Tail1,[Head2|Tail2],Diff).
b5sort_difference(List1,[_|Tail2],Diff) :-
   b5sort_difference(List1,Tail2,Diff).
 
b5sort_difference_minus_one([],_,_,[]) :- !.
b5sort_difference_minus_one(List,[],Member,NewList) :-
   !,
   b5sort_difference(List,[Member],NewList).
b5sort_difference_minus_one([Head|Tail1],[Head|Tail2],Head,Tail) :-
   !,
   b5sort_difference(Tail1,Tail2,Tail).
b5sort_difference_minus_one([Head|Tail1],List2,Head,Tail) :-
   !,
   b5sort_difference(Tail1,List2,Tail).
b5sort_difference_minus_one([Head|Tail1],[Head|Tail2],Member,Tail) :-
   Member @< Head,
   !,
   b5sort_difference(Tail1,Tail2,Tail).
b5sort_difference_minus_one([Head|Tail1],[Head|Tail2],Member,Tail) :-
   !,
   b5sort_difference_minus_one(Tail1,Tail2,Member,Tail).
b5sort_difference_minus_one([Head1|Tail1],[H2|Tail2],M,[Head1|Tail]) :-
   Head1 @< H2,
   Head1 @< M,
   !,
   b5sort_difference_minus_one(Tail1,[H2|Tail2],M,Tail).
b5sort_difference_minus_one([Head1|Tail1],[Head2|Tail2],M,[Head1|Tail]) :-
   Head1 @< Head2,
   M @< Head1,
   !,
   b5sort_difference(Tail1,[Head2|Tail2],Tail).
b5sort_difference_minus_one([Head1|Tail1],[Head2|Tail2],Member,Tail) :-
   Head2 @< Head1,
   Head1 @< Member,
   !,
   b5sort_difference_minus_one([Head1|Tail1],Tail2,Member,Tail).
b5sort_difference_minus_one([Head1|Tail1],[_Head2|Tail2],_Member,Tail) :-
/*
   Head2 @< Head1,
   Member @< Head1,
*/
   !,
   b5sort_difference([Head1|Tail1],Tail2,Tail).

b5sort_common_member_p([Member|_],[Member|_]) :- !.
b5sort_common_member_p([Head1|Tail1],[Head2|Tail2]) :-
  (Head1 @< Head2,
   !,
   b5sort_common_member_p(Tail1,[Head2|Tail2]);
      b5sort_common_member_p([Head1|Tail1],Tail2)).
 
b5sort_subset_p([],_).
b5sort_subset_p([Member|Tail1],[Member|Tail2]) :-
   !,
   b5sort_subset_p(Tail1,Tail2).
b5sort_subset_p([Head1|Tail1],[Head2|Tail2]) :-
   Head2 @< Head1,
   b5sort_subset_p([Head1|Tail1],Tail2).
 
b5sort_ordered_p(Term1,Term2) :-
	Term1 @< Term2,
	!.

b5sort_compare(Order, Term1, Term2) :-
	compare(Order, Term1, Term2).

b5sort_card([_|Xs],Ca,CC) :- 
        C is Ca + 1,
        b5sort_card(Xs,C,CC).

b5sort_card([],X,X).

b5sort_card([],0) :-  !.
b5sort_card(Xs,C) :- 
	b5sort_card(Xs,0,C).

b5sort_first([X|_],X).

b5sort_last([X],X) :-
	!. 

b5sort_last([_|Xs],Y) :- 
	b5sort_last(Xs,Y).

b5sort_empty_p([]).

/*
b5sort_compare(none,_,_,Res) :- !, Res = none.
b5sort_compare(Sub,[],[],Res) :- !, Res = Sub.
b5sort_compare(Sub1,[],_,Sub2) :- !, t5sub_unify(Sub1,second,Sub2).
b5sort_compare(Sub1,_,[],Sub2) :- !, t5sub_unify(Sub1,first,Sub2).
b5sort_compare(Sub1,[H|T1],[H|T2],Sub2) :- 
        !, b5sort_compare(Sub1,T1,T2,Sub2).
b5sort_compare(Sub1,[H1|T1],[H2|T2],Sub3) :-
        b5sort_ordered_p(H1,H2),
	!,
        t5sub_unify(Sub1,first,Sub2),
        b5sort_compare(Sub2,T1,[H2|T2],Sub3).
b5sort_compare(Sub1,Ls,[_|T],Sub3) :- 
        t5sub_unify(Sub1,second,Sub2),
        b5sort_compare(Sub2,Ls,T,Sub3).
*/


b5sort_compare(none,_,_,Res) :- Res = none.
b5sort_compare(first,L1,L2,Res) :-
	b5sort_subset_p(L2,L1),
	!,
	Res = first;
	   Res = none.
b5sort_compare(second,L1,L2,Res) :-
	b5sort_subset_p(L1,L2),
	!,
	Res = second;
	   Res = none.
b5sort_compare(equal,L1,L2,Res) :-
	b5sort_compare_equal(L1,L2,Res).

b5sort_compare_equal([],L2,Res) :- 
	L2 == [],
	!,
	Res = equal;
	   Res = second.
b5sort_compare_equal([H|L1],[H|L2],Res) :-
	!,
	b5sort_compare_equal(L1,L2,Res).
b5sort_compare_equal([H1|L1],[H2|L2],Res) :-
	H1 @> H2,
	!,
	b5sort_compare(second,[H1|L1],L2,Res).
b5sort_compare_equal([_|L1],L2,Res) :-
	!,
	b5sort_compare(first,L1,L2,Res).




b5sort_unify(L1,L2,L3,L4) :-
	b5sort_unify(L1,L2,L3),
	b5sort_difference(L3,L1,L4).



%% b5sort_not_unify(+L1,+L2,-L3)
%%    L3 is L1 plus L2, sorted but without removing 
%%    duplicate elements; L1 and L2 must be sorted!
b5sort_not_unify([],L,L) :- !.
b5sort_not_unify(L,[],L) :- !.
b5sort_not_unify([H1|Tail1], [H2|Tail2], [H1|Tail3]) :-
	H1 @< H2, !,
	b5sort_not_unify(Tail1,[H2|Tail2], Tail3).
b5sort_not_unify(L1, [H2|Tail2], [H2|Tail3]) :-
	!,
	b5sort_not_unify(L1,Tail2,Tail3).


b5key_view(K,V) :- 
	nonvar(K),
	integer(K),
(b5kif_name_key(N,_,K)->V=N/K;V=key(K)/K).
keys_view([],[]).
keys_view([K|Ks],[VK|VKS]) :-
	key_view(K,VK),
	keys_view(Ks,VKS).

isort_p(X) :- var(X),!, fail.   
isort_p([]).
isort_p([Head|Tail]) :-
	integer(Head),
	isort_p(Tail, Head).

isort_p(X, _) :- var(X),!, fail. 
isort_p([], _).
isort_p([Head|Tail], Left) :-
	integer(Head),
	Head @> Left,
	isort_p(Tail, Head).

b5sort_view(L,View) :-
	isort_p(L),%[_|_] = L,
	keys_view(L,View).

t5res_view(Res,View) :-
	nonvar(Res),
	t5res_raw_create(R,VR,NR,Fs,RVM,State,DCS,Res),
	t5res_raw_create(V_R,V_VR,NR,V_Fs,RVM,State,V_DCS,View),
	key_view(R,V_R),
	key_view(VR,V_VR),
	keys_view(Fs,V_Fs),
	keys_view(DCS,V_DCS).

t5rl_view(RL,View) :-
	nonvar(RL),
	t5rl_raw_create(S,Ress,RL),
	t5rl_raw_create(S,V_Ress,View),
	b5typelist_map(Ress,V_Ress,t5res_view).	

b5re_view(RE,View) :-
	nonvar(RE),
	b5re_raw_create(Role,Filler,RE),	
	b5re_raw_create(V_Role,V_Filler,View),	
	key_view(Role,V_Role),	
	keys_view(Filler,V_Filler).

b5rel_view(REL,VIEW) :-
	nonvar(REL),
	 b5typelist_map(REL,VIEW,b5re_view).

b5rvr_view(RVR,VIEW) :-
	nonvar(RVR),
	b5rvr_raw_create(Role,VR,RVR),
	b5rvr_raw_create(V_Role,V_VR,VIEW),
	key_view(Role,V_Role),
	key_view(VR,V_VR).

b5rvrl_view(RVR,VIEW) :-
	nonvar(RVR),
	 b5typelist_map(RVR,VIEW,b5rvr_view).

b5nf_view(NF,VIEW) :-
	nonvar(NF),
	b5nf_raw_create(T,D,P,NP,RL,NR,X,Num,S,NF), 
	b5nf_raw_create(T,V_D,V_P,V_NP,V_RL,NR,V_X,Num,V_S,VIEW), 
	key_view(D,V_D),
	keys_view(P,V_P),
	keys_view(NP,V_NP),
	t5rl_view(RL,V_RL),
	keys_view(X,V_X),
	keys_view(S,V_S).

t5conc_view(C,VIEW) :-
	nonvar(C),
	t5conc_raw_create(Flag,CNF,RI,Filter,C),
	nonvar(Flag),
	t5conc_raw_create(Flag,V_NF,RI,Filter,VIEW),
	b5nf_view(CNF,V_NF).

t5cdb_view(CDB_ENTRY,VIEW) :-
	nonvar(CDB_ENTRY),
	CDB_ENTRY = t5cdb(Key,Conc),
	VIEW = t5cdb(V_Key,V_Conc),
	key_view(Key,V_Key),
	t5conc_view(Conc,V_Conc).

a5obj_view(OBJ,VIEW) :-
	nonvar(OBJ),
	a5obj_raw_create(Key,Flag,ONFs,RI,Filter,N_Fillers,OBJ),
	a5obj_raw_create(V_Key,Flag,V_ONFs,RI,Filter,V_N_Fillers,VIEW),
	key_view(Key,V_Key),
	a5onfs_view(ONFs,V_ONFs),
	keys_view(N_Fillers,V_N_Fillers).

a5onfs_view(ONFS,VIEW) :-
	nonvar(ONFS),
	a5onfs_raw_create(US,SY,IM,ONFS),
	a5onfs_raw_create(V_US,V_SY,V_IM,VIEW),
	b5nf_view(US,V_US),
	b5nf_view(SY,V_SY),
	b5nf_view(IM,V_IM).

a5odb_view(Entry,View) :-
	nonvar(Entry),
	a5odb(Key,O) = Entry,
	a5odb(V_Key,V_O) = View,
	key_view(Key,V_Key),
	a5obj_view(O,V_O).

a5objid_view(ID,View) :-
	nonvar(ID),
	a5odb(ID,C) ,
	a5odb_view(a5odb(ID,C),View).

t5res_write(X) :- t5res_view(X,Y),write(Y).
t5rl_write(X) :- t5rl_view(X,Y),write(Y).
b5re_write(X) :- b5re_view(X,Y), write(Y).
b5rel_write(X) :- b5rel_view(X,Y),write(Y).
b5rvr_write(X) :- b5rvr_view(X,Y),write(Y).
b5rvrl_write(X) :- b5rvrl_view(X,Y),write(Y).
b5nf_write(X) :- b5nf_view(X,Y),write(Y).
t5conc_write(X) :- t5conc_view(X,Y),write(Y).
t5cdb_write(X) :- t5cdb_view(X,Y),write(Y).
%a5obj_write(X) :- a5odb_view(X,Y),write(Y).
a5obj_write(X) :- a5obj_view(X,Y),write(Y).
a5onfs_write(X) :- a5onfs_view(X,Y),write(Y).
a5odb_write(X) :- a5odb_view(X,Y),write(Y).
b5sort_write(X) :- b5sort_view(X,Y),write(Y).
b5key_write(X) :- b5key_view(X,Y),write(Y).

pmod([b5sort,t5res,t5rl,b5re,b5rel,b5rvr,b5rvrl,b5nf,t5conc,t5cdb,a5obj,a5onfs,a5odb,b5key,b5desc]).

:- use_module(library(addportray),[add_portray/1,del_portray/1]).
addp :- addp(_X),fail.  
addp.

addp(X) :-
	var(X),!,
	pmod(Module),
	member(X,Module),
	name(X,Name),
	name('_write',Suffix),
	append(Name,Suffix,Pred),
	name(ThisOne,Pred),
	add_portray(ThisOne).

addp(X) :-
	nonvar(X),
	pmod(Module),
	member(X,Module),!,
	name(X,Name),
	name('_write',Suffix),
	append(Name,Suffix,Pred),
	name(ThisOne,Pred),
	add_portray(ThisOne).

delp :- delp(_X),fail.
delp.

delp(X) :-
	pmod(Module),
	member(X,Module),
	name(X,Name),
	name('_write',Suffix),
	append(Name,Suffix,Pred),
	name(ThisOne,Pred),
	del_portray(ThisOne).


view(Name) :-
	b5kif_name_key(Name,_T,Key),
	b5kif_entity(Key,E),
	b5kif_module(Key,M),
	write('Key = '),write(Key),nl,
	b5view_view(M,E).

b5view_view(obj,E) :- a5obj_write(E).
b5view_view(conc,E) :- t5conc_write(E).
b5view_view(role,E) :- write(E).
b5view_view(inst,E) :-	write(E). 

% LAST EDIT: Fri Aug 20 11:19:36 1993 by Mirjam Kuehne (madonna!mir) 
% LAST EDIT: Thu Aug 19 20:27:48 1993 by Mirjam Kuehne (madonna!mir) 
% LAST EDIT: Wed Aug 18 17:35:39 1993 by Mirjam Kuehne (madonna!mir) 
% LAST EDIT: Fri Jan 22 12:54:26 1993 by Mirjam Kuehne (anfall!mir) 
% LAST EDIT: Thu Jan 21 13:33:43 1993 by Mirjam Kuehne (madonna!mir) 
% LAST EDIT: Tue Aug  4 14:57:56 1992 by Mirjam Kuehne (madonna!mir) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              FILTER-MODULE                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              Mirjam Kuehne                             %
%                               August 1992                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded(library(lists)).

%% Build-In-predicates:delete(+Element,+List,-NewList)

%% IMPORT-predicates: b5sort_unify(+List1,+List2,-NewList),
%%		      b5sort_subset_p(+List1,+List2)

%% EXPORT-predicates: t5fil_create,
%%		      t5fil_filter(+Filter,-NewFilter),
%%		      t5fil_add(+Filter,+OldList,-NewFilter),
%%		      t5fil_unify(+FilterList1,+FilterList2,-NewList),
%%		      t5fil_holds_p(+Filter,+FilterList)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% t5fil_create: creates a new empty filter, if a concept hasn't one	 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5fil_create([]) :- !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% t5fil_filter: creates and adds only one filter for a concept.          %
%               The concept has no filter before.			 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5fil_filter(Filter,[Filter]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% t5fil_add: extends the Filterlist of a concept with one new filter	 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5fil_add(Filter,OldFilter,NewFilter) :- 
	b5sort_unify([Filter],OldFilter,NewFilter).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% t5fil_unify: unifies two lists of several filters                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5fil_unify(FilterList,OldFilterList,NewFilterList) :-               
	b5sort_unify(FilterList,OldFilterList,NewFilterList).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% t5fil_holds_p: Is the given filter an element or a subset of the       %
%                filter at the concept?					 %
% All concepts with filter primitive, user_primitive or user_defined are %
% indexing concepts as well.                                             %
% All concepts with filter user_primitive or user_defined are user-      %
% concepts.                                                              %
% Only concepts with filter user_defined are really user defined ones.   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5fil_holds_p([],_) :- !.

t5fil_holds_p(Filter,FilterList) :- 
	atom(Filter),
	!,
	t5fil_one_holds_p(Filter,FilterList).

t5fil_holds_p([Filter|FilterList1],FilterList2) :-
	t5fil_one_holds_p(Filter,FilterList2),
	t5fil_holds_p(FilterList1,FilterList2).


t5fil_one_holds_p(indexing,FilterList) :-
	!,
	b5sort_common_member_p([indexing,primitive,user_defined,
                                user_primitive],FilterList),
	\+ t5fil_one_holds_p(revised,FilterList).

t5fil_one_holds_p(recognition_candidate,_) :- !.
%%	\+ t5fil_one_holds_p(revised,FilterList).

t5fil_one_holds_p(user,FilterList) :-
	!,
	b5sort_common_member_p([user_defined,user_primitive],FilterList),
	\+ t5fil_one_holds_p(revised,FilterList).


t5fil_one_holds_p(user_defined,FilterList) :-
	!,
	b5sort_common_member_p([user_defined],FilterList),
	\+ t5fil_one_holds_p(revised,FilterList).

t5fil_one_holds_p(named,X) :-
	!,                                        %% mf 2.2.93
	( t5fil_one_holds_p(user,X), %% cmk 20.8.93; was: (user_defined,X),
	  !                                      %% mf 2.2.93
	  ;
	  t5fil_one_holds_p(predef,X)
	).
	
t5fil_one_holds_p(top,_) :- !.

t5fil_one_holds_p(Filter,FilterList) :-
	b5sort_member_p(Filter,FilterList),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% t5fil_remove: delete the filter from filterlist at the concept         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5fil_remove(Filter,OldFilter,NewFilter) :-
	delete(OldFilter,Filter,NewFilter),
	!.
	

% ffs :
t5fil_del(OldFilter,DelFilter,NewFilter) :-
	b5sort_difference(OldFilter,DelFilter,NewFilter).

/*------------------------------------------------------------ */ 
/*		sorted typed lists			       */
/*------------------------------------------------------------ */ 

/* 		i:type(content)				       */

b5typelist_subtract([],_,[]).

b5typelist_subtract([H1|T1],List2,Diff) :-
        b5typelist_subtract_x(List2,H1,T1,Diff).

b5typelist_subtract_x([],H1,T1,[H1|T1]).

b5typelist_subtract_x([H2|T2],H1,T1,Diff) :-
        b5typelist_order(H1,H2,Order),
        b5typelist_subtract_x_order(Order,H1,T1,H2,T2,Diff).        

b5typelist_subtract_x_order(<,H1,T1,H2,T2,[H1|Diff]) :-
        b5typelist_subtract_y(T1,H2,T2,Diff).

b5typelist_subtract_x_order(>,H1,T1,_H2,T2,Diff) :-
        b5typelist_subtract_x(T2,H1,T1,Diff).

b5typelist_subtract_x_order(=,H1,T1,H2,T2,Diff) :-
        b5typelist_up_call_subtract(H1,H2,Head_Diff),!,
        ((Head_Diff == []) -> Diff = Tail_Diff 
			; Diff = [Head_Diff|Tail_Diff]
	),
        b5typelist_subtract(T1,T2,Tail_Diff).

b5typelist_subtract_y([],_,_,[]).

b5typelist_subtract_y([H1|T1],H2,T2,Diff) :-
        b5typelist_order(H1,H2,Order),
        b5typelist_subtract_x_order(Order,H1,T1,H2,T2,Diff).

b5typelist_up_call_subtract(X,X,[]) :- !.

b5typelist_up_call_subtract(K-X,K-Y,XdiffY) :-
        b5typelist_upcall_subtract(X,Y,K,XdiffY).


b5typelist_upcall_subtract(re(X),re(Y),K,K-re(Z)) :- b5sort_difference(X,Y,Z).
b5typelist_upcall_subtract(res(VR,NR,Fs,RVM,_),res(VR2,NR2,Fs2,RVM,_),R,RES) :-
/* rvm not handled correct (yet) */
	t5res_role_range_nr(R,D_VR,D_NR),
	%t5res_raw_create(R,VR3,NR3,un-[],Fs3,RVM,RES),
	t5res_raw_create(R,VR3,NR3,Fs3,RVM,un-[],RES),
	(VR == VR2 -> VR3 = D_VR ; VR3 = VR2),
	(NR == NR2 -> NR3 = D_NR ; NR3 = NR2),
	b5sort_difference(Fs2,Fs,Fs3).


b5typelist_order(Key1-_,Key2-_,CMP) :-
        compare(CMP,Key1,Key2),!.
%this one for sorted list without type_id 

b5typelist_order(Key1,Key2,CMP) :-
        compare(CMP,Key1,Key2),!.


b5typelist_call(F,A,B,C,D) :-
	functor(X,F,4),
	arg(1,X,A),
	arg(2,X,B),
	arg(3,X,C),
	arg(4,X,D),
	call(X).


b5typelist_call(F,A,B) :-
	functor(X,F,2),
	arg(1,X,A),
	arg(2,X,B),
	call(X).

b5typelist_call(F,A) :-
	functor(X,F,1),
	arg(1,X,A),
	call(X).

b5typelist_not(X) :- X,!,fail.
b5typlist_not(_) :- !.

%b5typelist_get_p([],_,_).
b5typelist_get_p([H|T],Key,X) :-
        b5typelist_order(H,Key-X,Order),
        b5typelist_get_order(Order,H,T,Key,X).

b5typelist_get_order(=,H,_T,_K,H).
b5typelist_get_order(<,_H,T,Key,X) :-
        b5typelist_get_p(T,Key,X).  
%b5typelist_get_order(>,_,_,KeyX,_).



b5typelist_r_ele([],_Key,_Old,New,[New]) :- !.

b5typelist_r_ele([H|T],Key,Old,New,[NH|NT]) :-
	b5typelist_order(H,Key-_,Order),
	b5typelist_r_ele_order(Order,H,T,Key,Old,New,NH,NT).

b5typelist_r_ele_order(=,H,T,_Key,H,New,New,T) :- !.
b5typelist_r_ele_order(<,H,T,Key,Old,New,H,NT) :- 
	b5typelist_r_ele(T,Key,Old,New,NT).
b5typelist_r_ele_order(>,H,T,_Key,_Old,New,New,[H|T]) :- !.
% das ist insert, festellbar durch var(old)

		

	
b5typelist_unify([],X,X) :- !. 

b5typelist_unify([H1|T1],X,U) :-
        b5typelist_unify_x(X,H1,T1,U).

b5typelist_unify_x([],H1,T1,[H1|T1]).
b5typelist_unify_x([H2|T2],H1,T1,U) :-
        b5typelist_order(H1,H2,Order),
        b5typelist_unify_x(Order,H1,T1,H2,T2,U).

b5typelist_unify_x(<,H1,T1,H2,T2,[H1|U]) :-
        b5typelist_unify_x(T1,H2,T2,U).

b5typelist_unify_x(>,H1,T1,H2,T2,[H2|U]) :-
	b5typelist_unify_x(T2,H1,T1,U).

b5typelist_unify_x(=,Y1,T1,Y2,T2,[Y3|U]) :-
        b5typelist_up_call_unify(Y1,Y2,Y3),
        b5typelist_unify(T1,T2,U).

b5typelist_up_call_unify(X,X,X) :- !.
b5typelist_up_call_unify(K-X,K-Y,KXY) :-
        b5typelist_upcall_unify(X,Y,K,KXY),!.

b5typelist_upcall_unify(res(X1,X2,X3,X4,X5),res(Y1,Y2,Y3,Y4,Y5),K,Z) :- 
	t5res_unify(K-res(X1,X2,X3,X4,X5),K-res(Y1,Y2,Y3,Y4,Y5),Z).
b5typelist_upcall_unify(re(X),re(Y),K,K-re(Z))    :- b5typelist_unify(X,Y,Z).
b5typelist_upcall_unify(vr(X),vr(Y),K,K-vr(Z))    :- t5concid_unify(X,Y,Z).
b5typelist_upcall_unify(bre(X),bre(Y),K,K-bre(Z)) :- b5typelist_unify(X,Y,Z).
b5typelist_upcall_unify(rvar(X),rvar(X),K,K-rvar(X)). % variable unify


b5typelist_add_ele([],Ele,[Ele]).

b5typelist_add_ele([H|T],Ele,TList) :-
        b5typelist_order(H,Ele,Order),
        b5typelist_add_ele(Order,H,T,Ele,TList).

b5typelist_add_ele(<,H,T,Ele,[H|TList]) :-
        b5typelist_add_ele(T,Ele,TList).

b5typelist_add_ele(>,H,T,Ele,[Ele,H|T]).

b5typelist_add_ele(=,H,T,Ele,[N_H|T]) :-
        b5typelist_up_call_unify(H,Ele,N_H).

b5typelist_map([],[],_).
b5typelist_map([H|T],[FH|FT],Fon) :- 
        b5typelist_call(Fon,H,FH),
        b5typelist_map(T,FT,Fon).

b5typelist_filter_out([],_FilterPred,[]) :- !.
b5typelist_filter_out([X|Xs],FilterPred,Ys) :- 
	%b5typelist_not(b5typelist_call(FilterPred,X)),
        b5typelist_call(FilterPred,X),
        !,
        b5typelist_filter_out(Xs,FilterPred,Ys).

b5typelist_filter_out([X|Xs],FilterPred,[X|Ys]) :- 
        b5typelist_filter_out(Xs,FilterPred,Ys).


b5typelist_filter_in([],_FilterPred,[]) :- !.
b5typelist_filter_in([X|Xs],FilterPred,Ys) :- 
	%b5typelist_not(b5typelist_call(FilterPred,X)),
        \+ b5typelist_call(FilterPred,X),
        !,
        b5typelist_filter_in(Xs,FilterPred,Ys).

b5typelist_filter_in([X|Xs],FilterPred,[X|Ys]) :- 
        b5typelist_filter_in(Xs,FilterPred,Ys).



/* Roleextension */

/*
        RE = Role-re([Filler]) 
*/
b5re_role(Role-re(_),Role).
b5re_fillers(_R-re(F),F).
b5re_s_role(_R-re(F),R,R-re(F)).
b5re_s_fillers(R-re(_),F,R-re(F)).
b5re_r_role(R-re(F),R,N_R,N_R-re(F)).
b5re_r_fillers(R-re(F),F,N_F,R-re(N_F)).

b5re_raw_create(Role,Filler,Role-re(Filler)).

b5re_add_fillers(Re,Fs,N_Re) :-
        b5re_r_fillers(Re,O_Fs,N_Fs,N_Re),
	b5sort_unify(O_Fs,Fs,N_Fs).

b5re_diff(H1,H2,Diff1,Diff2) :-
	b5re_r_fillers(H1,F1,N_F1,Diff1),
	b5re_r_fillers(H2,F2,N_F2,Diff2),
	b5sort_difference(F1,F2,N_F1),
	b5sort_difference(F1,F2,N_F2).

/* Roleextensionlist */

b5rel_raw_create(List,List).
b5rel_create(R) :- b5rel_raw_create([],R).

b5rel_add(O_REL,RE,N_REL) :-
	b5typelist_add_ele(O_REL,RE,N_REL).

b5rel_build_rel(RL,REL) :-
	t5rl_ress(RL,RESS),
	b5rel_build(RESS,REL).

b5rel_build([],[]).
b5rel_build([R-H|T],REL) :-
	t5res_fillers(R-H,F),
	(
		b5sort_empty_p(F) ->  b5rel_build(T,REL) 
				;	REL = [R-re(F)|RT],
		b5rel_build(T,RT)) .

b5rel_diff([],X,[],X) :- !.
b5rel_diff(X,[],X,[]) :- !.

b5rel_diff([H1|T1],[H2|T2],Diff1,Diff2) :-
	b5typelist_order(H1,H2,Order),
	b5rel_diff_order(Order,H1,T1,H2,T2,Diff1,Diff2).

b5rel_diff_order(<,H1,T1,H2,T2,[H1|Diff1],Diff2) :-
	b5rel_diff(T1,[H2|T2],Diff1,Diff2).

b5rel_diff_order(>,H1,T1,H2,T2,Diff1,[H2|Diff2]) :-
	b5rel_diff([H1|T1],T2,Diff1,Diff2).

b5rel_diff_order(=,H1,T1,H2,T2,Diff1,Diff2) :-
	b5re_diff(H1,H2,D1,D2),
	(b5re_fillers(D1,[]) -> Diff1 = NDiff1;Diff1 =[D1|NDiff1]),
	(b5re_fillers(D2,[]) -> Diff2 = NDiff2;Diff2 =[D2|NDiff2]),
	b5rel_diff(T1,T2,NDiff1,NDiff2).



/* R-VR list */

/*
        RVR = Role-vr(VR) 
*/

t5res2rvr(Res,RVR) :-
	t5res_role(Res,Role),
	t5res_vr(Res,VR),
	b5rvr_raw_create(Role,VR,RVR).

b5rvr_role(Role-_,Role).
b5rvr_vr(_-vr(VR),VR).
b5rvr_s_role(_-vr(VR),R,R-vr(VR)).
b5rvr_s_vr(R-vr(_),VR,R-vr(VR)).
b5rvr_r_role(R-vr(VR),R,N_R,N_R-vr(VR)).
b5rvr_r_vr(R-vr(VR),VR,N_VR,R-vr(N_VR)).

b5rvr_raw_create(Role,VR,Role-vr(VR)).

b5rvr_add_vr(RVR,VR,N_RVR) :-
        b5re_r_vr(RVR,O_VR,N_VR,N_RVR),
	t5concid_unify(O_VR,VR,N_VR).

b5rvr_subsumes_p(RVR1,RVR2) :-
	b5rvr_raw_create(Role,VR1,RVR1),
	b5rvr_raw_create(Role,VR2,RVR2),
	t5concid_subsumes_p(VR1,VR2).


b5rvrl_raw_create(List,List).
b5rvrl_create(R) :- b5rvrl_raw_create([],R).

b5rvrl_build_rvrl(RL,RVRL) :-
	t5rl_map(RL,t5res2rvr,RVRL).



/* BackwardRoleextension */

/*
        RE = Obj-bre([Role]) 
*/

b5bre_obj(O-bre(_),O).
b5bre_roles(_-bre(R),R).
b5bre_s_obj(_O-bre(F),O,O-re(F)).
b5bre_s_roles(O-bre(_),R,O-re(R)).
b5bre_r_obj(O-bre(R),O,N_O,N_O-re(R)).
b5bre_r_roles(O-bre(R),R,N_R,O-re(N_R)).


b5bre_raw_create(Obj,Roles,Obj-bre(Roles)).

b5bre_add_roles(BRe,Role,N_BRe) :-
        b5bre_r_roles(BRe,O_R,N_R,N_BRe),
        b5typelist_unify(b5key,O_R,Role,N_R).



/* BackwardRoleextensionlist */

%b5rel_raw_create(List,rel(List)).
b5brel_raw_create(List,List).
b5brel_create(R) :- b5brel_raw_create([],R).

b5brel_build_brel(RL,BREL) :-
	t5rl_ress(RL,RESS),
	b5brel_create(Empty),  % dies muss noch wech
	b5brel_build(RESS,Empty,BREL).

b5brel_build([],X,X).
b5brel_build([R-ES|S],A,BREL) :-
	t5res_fillers(R-ES,Objs),
	t5role_inv_role(R,Inv_R), 
	(number(Inv_R) -> b5brel_do(Objs,Inv_R,A,N_A)
		;	N_A = A),
	b5brel_build(S,N_A,BREL).

b5brel_do([],_,A,A).
b5brel_do([Obj|Objs],Role,Accu,New_Accu) :-
	b5typelist_add_ele(Accu,Obj-bre([Role]),Accu2),
	b5brel_do(Objs,Role,Accu2,New_Accu).
	
/* Roleexpression */

b5rex_raw_create(Role,VE,Role:VE).



/* rvar Role-var(Var) */

b5rvar_create(Role,Var,Role-rvar(Var)).

%b5rvrl_build(EQL,rvar).
b5rvrl_build(EQs,RVARL) :- 
	b5rvrl_build(EQs,[],RVARL).

b5rvrl_build([],Accu,Accu).

b5rvrl_build([EQ|EQs],Accu,F_RVARL) :-
	t5eq2rvarl(EQ,RVARL),
	b5typelist_unify(Accu,RVARL,N_Accu),
	b5rvrl_build(EQs,N_Accu,F_RVARL).


b5rvrl_unwrap([],[]).
b5rvrl_unwrap([X|Xs],[Role-Var|Ys]) :-
	b5rvar_create(Role,Var,X),
	b5rvrl_unwrap(Xs,Ys).

	


