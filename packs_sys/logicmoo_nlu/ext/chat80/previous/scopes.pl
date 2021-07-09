/*
 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/

clausify80(question80(V0,P),(answer80(V):-B)) :- !,
   quantify(P,Quants,[],R0),
   split_quants(question80(V0),Quants,HQuants,[],BQuants,[]),
   chain_apply(BQuants,R0,R1),
   head_vars(HQuants,B,R1,V,V0).

clausify80(assertion(V0,P),(assertion80(V):-B)) :- nonvar(P),
   quantify(P,Quants,[],R0),
   split_quants(question80(V0),Quants,HQuants,[],BQuants,[]),
   chain_apply(BQuants,R0,R1),
   head_vars(HQuants,B,R1,V,V0).


quantify(quant(Det,X,Head,Pred,Args,Y),Above,Right,true) :-
   close_tree(Pred,P2),
   quantify_args(Args,AQuants,P1),
   split_quants(Det,AQuants,Above,[Q|Right],Below,[]),
   pre_apply(Head,Det,X,P1,P2,Y,Below,Q).
quantify(conj(Conj,LPred,LArgs,RPred,RArgs),Up,Up,P) :-
   close_tree(LPred,LP0),
   quantify_args(LArgs,LQs,LP1),
   chain_apply(LQs,(LP0,LP1),LP),
   close_tree(RPred,RP0),
   quantify_args(RArgs,RQs,RP1),
   chain_apply(RQs,(RP0,RP1),RP),
   conj_apply(Conj,LP,RP,P).
quantify(pred(Subj,Op,Head,Args),Above,Right,P) :-
   quantify(Subj,SQuants,[],P0),
   quantify_args(Args,AQuants,P1),
   split_quants(Op,AQuants,Up,Right,Here,[]),
   conc80(SQuants,Up,Above),
   chain_apply(Here,(P0,Head,P1),P2),
   op_apply(Op,P2,P).
quantify(~(P),Q,Q,P).
quantify(P&Q,Above,Right,(S,T)) :-
   quantify(Q,Right0,Right,T),
   quantify(P,Above,Right0,S).
   
head_vars([],P,P,L,L0) :-
   strip_types(L0,L).
head_vars([Quant|Quants],(P,R0),R,[X|V],V0) :-
   extract_var(Quant,P,X),
   head_vars(Quants,R0,R,V,V0).

strip_types([],[]).
strip_types([_-X|L0],[X|L]) :-
   strip_types(L0,L).

conc80([],L,L).
conc80([H|T],L,[H|R]) :- conc80(T,L,R).


extract_var(quant(_,_-X,P,_-X),P,X).

chain_apply(Q0,P0,P) :-
   sort_quants(Q0,Q,[]),
   chain_apply0(Q,P0,P).    

chain_apply0([],P,P).
chain_apply0([Q|Quants],P0,P) :-
   chain_apply0(Quants,P0,P1),
   det_apply(Q,P1,P).

quantify_args([],[],true).
quantify_args([Arg|Args],Quants,(P,Q)) :-
   quantify_args(Args,Quants0,Q),
   quantify(Arg,Quants,Quants0,P).

pre_apply(~Head,set(I),X,P1,P2,Y,Quants,Quant) :-
   indices(Quants,I,Indices,RestQ),
   chain_apply(RestQ,(Head,P1),P),
   setify(Indices,X,(P,P2),Y,Quant).
pre_apply(~Head,Det,X,P1,P2,Y,Quants,quant(Det,X,(P,P2),Y)) :-
 ( unit_det(Det);
   index_det(Det,_)),
   chain_apply(Quants,(Head,P1),P).
pre_apply(apply(F,P0),Det,X,P1,P2,Y,
      Quants0,quant(Det,X,(P3,P2),Y)) :-
   but_last(Quants0,quant(lambda,Z,P0,Z),Quants),
   chain_apply(Quants,(F,P1),P3).
pre_apply(aggr(F,Value,L,Head,Pred),Det,X,P1,P2,Y,Quants,
      quant(Det,X,
            (S^(setof(Range:Domain,P,S),
                aggregate80(F,S,Value)),P2),Y)) :-
   close_tree(Pred,R),
   complete_aggr(L,Head,(R,P1),Quants,P,Range,Domain).

but_last([X|L0],Y,L) :-
   but_last0(L0,X,Y,L).

but_last0([],X,X,[]).
but_last0([X|L0],Y,Z,[Y|L]) :-
   but_last0(L0,X,Z,L).

close_tree(T,P) :-
   quantify(T,Q,[],P0),
   chain_apply(Q,P0,P).

meta_apply(~(G),R,Q,G,R,Q).
meta_apply(apply(F,(R,P)),R,Q0,F,true,Q) :-
   but_last(Q0,quant(lambda,Z,P,Z),Q).

indices([],_,[],[]).
indices([Q|Quants],I,[Q|Indices],Rest) :-
   open_quant(Q,Det,_,_,_),
   index_det(Det,I),
   indices(Quants,I,Indices,Rest).
indices([Q|Quants],I,Indices,[Q|Rest]) :-
   open_quant(Q,Det,_,_,_),
   unit_det(Det),
   indices(Quants,I,Indices,Rest).

setify([],Type-X,P,Y,quant(set,Type-([]:X),true:P,Y)).
setify([Index|Indices],X,P,Y,Quant) :-
   pipe(Index,Indices,X,P,Y,Quant).

pipe(quant(int_det(_,Z),Z,P1,Z),
      Indices,X,P0,Y,quant(det(a),X,P,Y)) :-
   chain_apply(Indices,(P0,P1),P).
pipe(quant(index(_),_-Z,P0,_-Z),Indices,Type-X,P,Y,
      quant(set,Type-([Z|IndexV]:X),(P0,P1):P,Y)) :-
   index_vars(Indices,IndexV,P1).

index_vars([],[],true).
index_vars([quant(index(_),_-X,P0,_-X)|Indices],
      [X|IndexV],(P0,P)) :-
   index_vars(Indices,IndexV,P).

complete_aggr([Att,Obj],~(G),R,Quants,(P,R),Att,Obj) :-
   chain_apply(Quants,G,P).
complete_aggr([Att],Head,R0,Quants0,(P1,P2,R),Att,Obj) :-
   meta_apply(Head,R0,Quants0,G,R,Quants),
   set_vars(Quants,Obj,Rest,P2),
   chain_apply(Rest,G,P1).
complete_aggr([],~(G),R,[quant(set,_-(Obj:Att),S:T,_)],
      (G,R,S,T),Att,Obj).

set_vars([quant(set,_-(I:X),P:Q,_-X)],[X|I],[],(P,Q)).
set_vars([],[],[],true).
set_vars([Q|Qs],[I|Is],R,(P,Ps)) :-
   open_quant(Q,Det,X,P,Y),
   set_var(Det,X,Y,I), !,
   set_vars(Qs,Is,R,Ps).
set_vars([Q|Qs],I,[Q|R],P) :-
   set_vars(Qs,I,R,P).

set_var(Det,_-X,_-X,X) :-
   setifiable(Det).

sort_quants([],L,L).
sort_quants([Q|Qs],S,S0) :-
   open_quant(Q,Det,_,_,_),
   split_quants(Det,Qs,A,[],B,[]),
   sort_quants(A,S,[Q|S1]),
   sort_quants(B,S1,S0).

split_quants(_,[],A,A,B,B).
split_quants(Det0,[Quant|Quants],Above,Above0,Below,Below0) :-
   compare_dets(Det0,Quant,Above,Above1,Below,Below1),
   split_quants(Det0,Quants,Above1,Above0,Below1,Below0).

compare_dets(Det0,Q,[quant(Det,X,P,Y)|Above],Above,Below,Below) :-
   open_quant(Q,Det1,X,P,Y),
   governs_det(Det1,Det0), !,
   bubble(Det0,Det1,Det).
compare_dets(Det0,Q0,Above,Above,[Q|Below],Below) :-
   lower(Det0,Q0,Q).

open_quant(quant(Det,X,P,Y),Det,X,P,Y).

% =================================================================
% Determiner Properties

index_det(index(I),I).
index_det(int_det(I,_),I).

unit_det(set).
unit_det(lambda).
unit_det(quant(_,_)).
unit_det(det(_)).
unit_det(question80(_)).
unit_det(id(_Why)).
unit_det(void(_Meaning)).
unit_det(not(_Why)).
unit_det(generic).
unit_det(int_det(_)).
unit_det(proportion(_)).

det_apply(quant(Det,Type-X,P,_-Y),Q0,Q) :-
   apply(Det,Type,X,P,Y,Q0,Q).

apply(generic,_,X,P,X,Q,X^(P,Q)).
apply(proportion(_Type-V),_,X,P,Y,Q,
      S^(setof(X,P,S),
         N^(numberof(Y,(one_of(S,Y),Q),N),
            M^(card(S,M),ratio(N,M,V))))).
apply(id(_Why),_,X,P,X,Q,(P,Q)).
apply(void(_Meaning),_,X,P,X,Q,X^(P,Q)).
apply(set,_,Index:X,P0,S,Q,S^(P,Q)) :-
   apply_set(Index,X,P0,S,P).
apply(int_det(Type-X),Type,X,P,X,Q,(P,Q)).
apply(index(_),_,X,P,X,Q,X^(P,Q)).
apply(quant(Op,N),Type,X,P,X,Q,R) :-
   value(N,Type,Y),
   quant_op(Op,Z,Y,numberof(X,(P,Q),Z),R).
apply(det(Det),_,X,P,Y,Q,R) :-
   apply0(Det,X,P,Y,Q,R).

apply0(Some,X,P,X,Q,X^(P,Q)) :-
   some(Some).
apply0(All,X,P,X,Q,\+X^(P,\+Q)) :-
   all(All).
apply0(no,X,P,X,Q,\+X^(P,Q)).
apply0(notall,X,P,X,Q,X^(P,\+Q)).

quant_op(same,X,X,P,P).
quant_op(Op,X,Y,P,X^(P,F)) :-
   quant_op(Op,X,Y,F).

quant_op(not(_Why)+more,X,Y,X=<Y).
quant_op(not(_Why)+less,X,Y,X>=Y).
quant_op(less,X,Y,X<Y).
quant_op(more,X,Y,X>Y).

value(wh(Type-X),Type,X).
value(nquant(X),_,X).

all(all).
all(every).
all(each).
all(any).

some(a).
some(the(sg)).
some(some).

apply_set([],X,true:P,S,setof(X,P,S)).
apply_set([I|Is],X,Range:P,S,
      setof([I|Is]:V,(Range,setof(X,P,V)),S)).

governs_det(Det,_Det0):-must(nonvar(Det)),fail.
governs_det(Det,set(J)) :-
   index_det(Det,I),
   I \== J.
governs_det(Det0,Det) :-
   index_det(Det0,_),
 ( index_det(Det,_);
   Det=det(_);
   Det=quant(_,_)).
governs_det(_,void(_There)).
governs_det(_,lambda).
governs_det(_,id(_Why)).
governs_det(det(each),question80([_|_])).
governs_det(det(each),det(each)).
governs_det(det(any),not(_Why)).
governs_det(quant(same,wh(_)),Det) :-
   weak(Det).
governs_det(det(Strong),Det) :-
   strong0(Strong),
   weak(Det).

strong(det(Det)) :-
   strong0(Det).

strong0(each).
strong0(any).

weak(det(Det)) :-
   weak0(Det).
weak(quant(_,_)).
weak(index(_)).
weak(int_det(_,_)).
weak(set(_)).
weak(int_det(_)).
weak(generic).
weak(proportion(_)).

weak0(no).
weak0(a).
weak0(all).
weak0(some).
weak0(every).
weak0(the(sg)).
weak0(notall).

lower(question80(_),Q,quant(det(a),X,P,Y)) :-
   open_quant(Q,det(any),X,P,Y), !.
lower(_,Q,Q).

setifiable(generic).
setifiable(det(a)).
setifiable(det(all)).

% =================================================================
% Operators (currently, identity, negation and 'and')

op_apply(id(_Why),P,P).
op_apply(not(_Why),P,\+P).

bubble(not(_Why),det(any),det(every)) :- !.
bubble(_,D,D).


conj_apply(and,P,Q,(P,Q)).
