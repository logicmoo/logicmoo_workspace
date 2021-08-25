%%%%%%%%%%%%%%%% MISC
:- setval(skolemctr,0).
:- setval(tme_cleanlit,off).

on_backtrack( _).
on_backtrack( Goal) :-
         Goal, !,
         fail. 

pl_list( (K, L), [K | PL_L] ) :- pl_list(L, PL_L), !.
pl_list( (K; L), [K | PL_L] ) :- pl_list(L, PL_L), !.
pl_list( K, [K] ) :- K \== true.
pl_list( true, [] ).

pl_list_con( (K, L), [K | PL_L] ) :- pl_list_con(L, PL_L), !.
pl_list_con( K, [K] ).

pl_list_dis( (K; L), [K | PL_L] ) :- pl_list_dis(L, PL_L), !.
pl_list_dis( K, [K] ).

norm_dis_taut( (true; _L), true ) :- !.
norm_dis_taut( (-false; _L), true ) :- !.
norm_dis_taut( (~false; _L), true ) :- !.
norm_dis_taut( (K; L),  Res ) :- !,
	norm_dis_taut(L, PL_L), 
	( PL_L == true ->
	    Res = true
	  ; ( (K == (-true) ; K == (~true) ; K == false) ->
              Res = PL_L
	    ; (PL_L = false -> Res = K ; Res = (K ; PL_L)))).

norm_dis_taut( ~true, false ).
norm_dis_taut( -true, false ).
norm_dis_taut( false, false ).
norm_dis_taut( true, true ).
norm_dis_taut( -false, true ).
norm_dis_taut( ~false, true ).
norm_dis_taut( K, K ).

norm_dis_list([],[]).
norm_dis_list([Cl|R],Res) :-
	norm_dis_taut(Cl,ClN),
	(ClN == true ->
	    	Res = Hres
	    ;   Res = [ClN|Hres]),
	norm_dis_list(R,Hres).

pl_list_dis_or_con( (K; L), [K | PL_L] ) :- pl_list_dis_or_con(L, PL_L), !.
pl_list_dis_or_con( (K, L), [K | PL_L] ) :- pl_list_dis_or_con(L, PL_L), !.
pl_list_dis_or_con( K, [K] ).


negate(- Literal, Literal) :- !.
negate(~ Literal, Literal) :- !.
negate(Literal, - Literal).

negate_list([],[]).
negate_list([H|T],[NH|NT]) :-
	negate(H,NH),
	negate_list(T,NT).

max_list([N],N) :- !.
max_list([N|R],Max) :- max_list(R,RMax), max(N,RMax,Max).


%% write out tme clauses:
write_tme_clause(F,[],Neg,input) :-
	printf(F,"false :- ",[]),
	write_tme_litlist(F,',',Neg),
	printf(F,".\n",[]), !.
write_tme_clause(F,Pos,[],input) :-
	!,
	write_tme_litlist(F,';',Pos),
	printf(F,".\n",[]), !.
write_tme_clause(F,Pos,Neg,input) :-
	write_tme_litlist(F,';',Pos),
	printf(F," :- ",[]),
	write_tme_litlist(F,',',Neg),
	printf(F,".\n",[]), !.

write_tme_clause(F,Pos,Neg,query) :-
	negate_list(Pos,NPos),
	append(NPos,Neg,Q),
	printf(F,"?- ",[]),
	write_tme_litlist(F,',',Q),
	printf(F,".\n",[]), !.


convert_equal((S = T), equal(S,T)).
convert_equal(-(S = T), - equal(S,T)).
convert_equal(~(S = T), - equal(S,T)).
convert_equal(L,L).

write_tme_litlist(_F,_Sep,[]).
write_tme_litlist(F,Sep,[L|R]) :-
	(getval(tme_cleanlit,on) ->
	    convert_equal(L,H),
	    colon_to_s_cons(H,HL)
%	    writeln(HL)
%	    colon_to_s(L,HL)
	;   HL = L),
%%% Ingo-servicable part: Hier werden Literale geprinted
%	printf(F,"%UPVmw",[HL]),
%	printf(F,"%Dvmw",[HL]),
%	printf(F,"%Dmw",[HL]),
	printf(F,"%DVmw",[HL]),
	( R \== [] ->
	    printf(F,"%mw ",[Sep])
	  ; true),
	write_tme_litlist(F,Sep,R).

%% list stuff

add_element(Elem, Set, Set) :-
        memberchk(Elem, Set),
        !.
add_element(Elem, Set, [Elem|Set]).


mydelete(X,[X|R],R).
mydelete(X,[Y|R],[Y|S]) :- mydelete(X,R,S).

norm_clause([],[]).
% this is done by norm_dis_taut
%norm_clause([-false|_R],[true]).
%norm_clause([~false|_R],[true]).
%norm_clause([true|_R],[true]).
%norm_clause([false|L],LR) :- !, norm_clause(L,LR).
%norm_clause([-true|L],LR) :- !, norm_clause(L,LR).
%norm_clause([~true|L],LR) :- !, norm_clause(L,LR).
norm_clause([~K|L],[-K|LR]) :- norm_clause(L,LR), !.
norm_clause([K|L],[K|LR]) :- norm_clause(L,LR), !.

norm_list([],[]).
norm_list([~K|L],[-K|LR]) :- !, norm_list(L,LR).
norm_list([K|L],[K|LR]) :- norm_list(L,LR), !.

is_pos(- _) :- !, fail.
is_pos(~ _) :- !, fail.
is_pos(_).
is_neg(- _).
is_neg(~ _).

lit_atom(-A,A).
lit_atom(~A,A).
lit_atom(A,A).

split_clause_([],[],[],[]).
split_clause_([Lit|RestLits],[Lit|ResPos],ResNeg,[LitVars|ResLitVars]) :-
	is_pos(Lit) , ! ,
	term_variables(Lit,LitVars),
	split_clause_(RestLits,ResPos,ResNeg,ResLitVars), 
	!.
split_clause_([Lit|RestLits],ResPos,[NLit|ResNeg],LitVars) :-
	%% Lit is negative
	negate(Lit,NLit),
	split_clause_(RestLits,ResPos,ResNeg,LitVars), !.

%% split_clause(Clause,Pos,Neg,PosVars,SV).
%% Pos (resp Neg) is the list of positive literals occuring in clause Clause
%% SV is the list of Variables shared by the positive literals
%% PosVars is the list of variables in the Pos lits but not in the Neg lits.
%% 
split_clause(Clause,Pos,Neg,PosVars,SV) :-
	split_clause_(Clause,Pos,Neg,PosLitVarSets),
	unionqlists(PosLitVarSets,HPosVars),
	term_variables(Neg,NegVars),
	subtractq(HPosVars,NegVars,PosVars),
	commonvars(PosLitVarSets,SV), !.

%% shared_variables(LitList,SV)
%% SV is the list of shared variables in the literal list LitList
shared_variables(LitList,SV) :-
	litvarsets(LitList,LVS),
	commonvars(LVS,SV), !.

litvarsets([],[]).
litvarsets([L|R],[VL|VR]) :-
	term_variables(L,VL),
	litvarsets(R,VR).

commonvars([],[]).
commonvars([F|R],L) :-
	unionqlists(R,UR),
	intersectq(F,UR,PosLits),
	commonvars(R,R1),
	unionq(PosLits,R1,L).


univ_vars(T,RVs,Vout) :- univ_vars(T,RVs,[],Vout).

univ_vars(T,_RVs,Vin,Vin) :- atomic(T).
univ_vars(T,RVs,Vin,Vin) :- var(T), 
	(memberq(T,RVs) ; memberq(T,Vin)), !.
univ_vars(T,_RVs,Vin,[T|Vin]) :- var(T), !.

univ_vars(T,RVs,Vin,Vout) :- 
	functor(T,_F,A),
	univ_vars(T,A,RVs,Vin,Vout), !.
univ_vars(_T,0,_RVs,Vin,Vin) :- !.
univ_vars(T,M,RVs,Vin,Vout) :-
	arg(M,T,TM),
	univ_vars(TM,RVs,Vin,VH),
	N is M-1,
	univ_vars(T,N,RVs,VH,Vout).
	
r_subsumes(L1,L2,RVs) :-
	\+ \+
         (
	 copy_term(L1 / RVs, L1C / RVsC),
	 RVs = RVsC,
	 univ_vars(L2,RVs,UVs),
	 skolemize(UVs,0),
	 skolemize(RVs,10000),
	 L1C = L2).


r_notsubsumes(L1,L2,[]) :- %% special case -- occurs quite frequently
	!, \+ subsumes(L1,L2).

r_notsubsumes(L1,L2,RVs) :-
	\+ \+
          %% idea: If there are instance of L1 and L2 wrt. rigid variables such
	  %% that L1 does not subsume L2, then there must also be an instance
	  %% where all rigid variables are assigned different instances
	  %%
          %% first make terms variable disjoint
         (
	 copy_term(L1 / RVs, L1C / RVsC),
	 RVs = RVsC,
	 univ_vars(L2,RVs,UVs),
	 skolemize(UVs,0),
	 skolemize(RVs,10000),
	 L1C \= L2),
	 %% here we know that there exists an instance such that L1 does not subsume L2
	 %% we want to keep that property in the future
	 delay(RVs,(term_variables(RVs,RVNew), %% migh be instantiated now
	            r_notsubsumes(L1,L2,RVNew))).
/*
r_subsumes_delay(L1,L2,RVs) :-
	\+ \+ (skolemize(RVs,0), subsumes(L1,L2)),
	delay(RVs,r_subsumes_delay(L1,L2,RVs)).
*/


/*
rigid_var(X,RVs) :-     var(X), memberq(X,RVs).
universal_var(X,RVs) :- var(X), \+ memberq(X,RVs).

%%% notsubsumes(T1,T2,RVs):
%%% T1 does not subsume T2, where RVs are the rigid variables occuring in T1 and T2
%%% assumes that universal variables are standardised apart.
%% First, the failure conditions
%% these are successful subsumption conditions, followed by "!, fail"
notsubsumes(T1,T2,RVs) :- 
	rigid_var(T1,RVs),
	universal_var(T2,RVs), !.
notsubsumes(T1,T2,RVs) :- 
	%% This case is handled not correctly by ~= below
	rigid_var(T1,RVs),
	rigid_var(T2,RVs), !, 
	notsubsumesallrigid(T1,T2), !.
notsubsumes(T1,T2,RVs) :- 
	rigid_var(T1,RVs), 
	(compound(T2) ; atomic(T2)), !, 
	notsubsumesallrigid(T1,T2), !.
%% T1 is not a rigid var here:
notsubsumes(T1,T2,RVs) :- 
	universal_var(T1,RVs),
	compound(T2), !,
	occurs(T1,T2), !.
notsubsumes(T1,T2,RVs) :- 
	(compound(T1) ; atomic(T1)),
	universal_var(T2,RVs), !.
notsubsumes(T1,T2,RVs) :- 
	(compound(T1) ; atomic(T1)),
	rigid_var(T2,RVs), !,
	notsubsumesallrigid(T1,T2), !.
notsubsumes(T1,T2,_RVs) :- 
	nonvar(T1), nonvar(T2),
	functor(T1,F1,_A1),
	functor(T2,F2,_A2), !,
	(F1 \== F2 ;
	 false).

notsubsumes_list(T1,T2,_RVs) :- 

notsubsumesallrigid(T1,T2) :-
	T1 == T2, !, fail.
notsubsumesallrigid(T1,T2) :-
	var(T1),
	var(T2), !,
	delay([T1,T2],notsubsumesallrigid(T1,T2)).
*/
	
%% SOME literal in LitList is subsumed
r_subsumed(Branch,LitList,RVs) :-
	\+ \+ (skolemize(RVs,0), subsumed(Branch,LitList)).

r_notsubsumedbybranch(_Branch,[],_RVs).
r_notsubsumedbybranch(Branch,[Lit|Rest],RVs) :-
	r_notsubsumed_lit(Branch,Lit,RVs), !,
	r_notsubsumedbybranch(Branch,Rest,RVs).
	
r_notsubsumed_lit([],_Lit,_RVs).
r_notsubsumed_lit([First|Rest],Lit,RVs) :-
	r_notsubsumes(First,Lit,RVs), !,
	r_notsubsumed_lit(Rest,Lit,RVs).

%% a literal in LitList is subsumed
subsumed(Branch,LitList) :-
	member(Lit,LitList),
	subsumed_lit(Branch,Lit), !.


subsumes(L1,L2) :-
	%% Literals must be variable disjoint
	copy_term(L1,L1C),
	instance(L2,L1C), !.
	

subsumed_lit(Branch,Lit) :-
	%% Lit must be variable disjoint from branch
	copy_term(Lit,LitC),
	member(BranchLit,Branch),
	instance(LitC,BranchLit), !.
%subsumed_lit(Branch,Lit) :-
%	\+ \+ ( term_variables(Lit,Vars), skolemize(Vars,0),
%	        member(Lit,Branch)).


/*
skolemize([],_Nr).
skolemize([sk(Nr)| MoreVars],Nr) :-
	NNr is Nr + 1,
	skolemize(MoreVars,NNr).
*/

%% important!! skolemization must not increase the weight of the terms
%skolemize([],_Nr).
%skolemize([Nr| MoreVars],Nr) :-
%	NNr is Nr + 1,
%	skolemize(MoreVars,NNr).
skolemize([],_Nr).
skolemize([Nr| MoreVars],_Nr) :-
	getval(skolemctr,Nr),
	incval(skolemctr),
	skolemize(MoreVars,_NNr).

/*
nonmemberq(HArg,[Arg|_]) :-
        HArg == Arg, !, fail.
nonmemberq(Arg,[_|Tail]) :-
        !,
        nonmemberq(Arg,Tail).
nonmemberq(_,[]).
*/

nonmemberq(HArg,[Arg|Tail]) :-
        %HArg ~= Arg, !, nonmemberq(HArg,Tail).
        HArg \== Arg, !, nonmemberq(HArg,Tail).
nonmemberq(_,[]).

memberq(HArg,[Arg|_]) :-
        HArg == Arg, !.
memberq(Arg,[_|Tail]) :-
        memberq(Arg,Tail).


% subtractq(L1, L2, L3)
% L3 = L1 - L2

subtractq([], _, []).
subtractq([Head|L1tail], L2, L3) :-
        memberq(Head, L2),
        !,
        subtractq(L1tail, L2, L3).
subtractq([Head|L1tail], L2, [Head|L3tail]) :-
        subtractq(L1tail, L2, L3tail).



intersectq([], _, []).

intersectq([Element|Residue], Set, Result) :-
        memberq(Element, Set),
        !,                    
        Result = [Element|Intersection],
        intersectq(Residue, Set, Intersection).

intersectq([_|Rest], Set, Intersection) :-
        intersectq(Rest, Set, Intersection).

unionqlists([],[]).
unionqlists([F|R],Res) :-
	unionqlists(R,HRes),
	unionq(F,HRes,Res).

unionq([], Set2, Set2).
unionq([Element|Residue], Set, Union) :-
       memberq(Element, Set), !,
       unionq(Residue, Set, Union), !.
unionq([Element|Residue], Set, [Element|Union]) :-
       unionq(Residue, Set, Union).

unionlists([],[]).
unionlists([F|R],Res) :-
	unionlists(R,HRes),
	union(F,HRes,Res).

union([], Set2, Set2).
union([Element|Residue], Set, Union) :-
       member(Element, Set), !,
       union(Residue, Set, Union), !.
union([Element|Residue], Set, [Element|Union]) :-
       union(Residue, Set, Union).

/*
r_unify(T1/RST1, T2/RST2, RT) :-
	copy_term(T1/RST1, T1C/RST1C),
	RST1=RST1C, T1C = T2, 
	%unionq(RST1,RST2, RT).
	term_variables(RST1 + RST2, RT).
*/
r_unify(T1, T2, RVsIn, RVsOut) :-
	copy_term(T1/RVsIn, T1C/RVsInC),
	RVsIn=RVsInC, T1C = T2, 
	%unionq(RST1,RST2, RT).
	term_variables(RVsIn, RVsOut).

	

