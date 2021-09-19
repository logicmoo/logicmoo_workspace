%:- set_module(class(development)).
:- nop('$set_source_module'( baseKB)).
:- expects_dialect(pfc).

:- set_prolog_flag(do_renames,restore).
:- set_prolog_flag_until_eof(do_renames,term_expansion).

/*
flatTrans(G)==> 
 ((
  t(G,A,B)/A\=B, 
  t(G,B,C)/(C\=B,C\=A))==> t(G,A,C)).

flatTrans(G)==> (((t(G,A,B)/(A\=B,dif(A,C)),t(G,B,C)/(B\=C)))  ==> t(G,A,C)).

*/
flatTrans(F)==>transIxyz(F,F,F).

((transIxyz(X,Y,Z)/(maplist(into_mpred_form,[t(X,A,B),t(Y,B,C),t(Z,A,C)],[AB,BC,AC])))
  ==> ((AB/dif(A,C),BC/(B\=C))==>AC)).

tooSlow==> flatTrans(genls).
flatTrans(subFormat).
flatTrans(genlPreds).
flatTrans(genlFuncs).
flatTrans(genlFunctions).
flatTrans(genlMt).

%genlsUpTo(TT) ==> ((isa(T,TT),genls(C,T))==>isa(C,TT)).
genlsUpTo(ttTemporalType).
genlsUpTo(ttRelationType).
genlsUpTo(ttTypeType).


% genls(C,S):- cwc, (ground(C);ground(S)),C\==tCol, loop_check(tran_by_trans(genls,C,S)),C\=S.


% to load this files use  ?- ensure_mpred_file_loaded('logicmoo/pfc/system_genls.pfc').
:- dynamic(mudIsa/2).
:- expects_dialect(pfc).


tooSlow==>((type_checking ==> (((genls(X,Y),{X\=Y},genls(Y,X))) ==> {mpred_withdraw(genls(Y,X))}))).

% (genls(C,SC)==>(tCol(SC),tCol(C),{repropagate(SC)})).

:- sanity(get_lang(pfc)).

% TODO (genls(C,SC)==>(tCol(C),tCol(SC))).

% rtAvoidForwardChain(functorDeclares).
rtAvoidForwardChain(C):- cwc, tCol(C),compound(C).
rtAvoidForwardChain(meta_argtypes).
% rtAvoidForwardChain(completeIsaAsserted).

ttExpressionType(C)==>rtAvoidForwardChain(C).

% TODO ((completeIsaAsserted(I), isa(I,Sub), {dif(Sub, Super)}, genls(Sub,Super),{ground(Sub:Super)}, \+ genls/*Fwd*/(Sub,Super), \+ ttExpressionType(Super))) ==> isa(I,Super).
%    \+ genlsFwd(Sub,Super), \+ ttExpressionType(Super))) ==> isa(I,Super).

completeIsaAsserted(I) ==> ((isa(I,Sub)/ (\+ rtAvoidForwardChain(Sub))) ==> mudIsa(I,Sub)).
mudIsa(I,C),genls(C,P) ==> mudIsa(I,P).
/*

% isRuntime ==> 
% (mudIsa(I,Sub)/(ground(mudIsa(I,Sub)), \+ rtAvoidForwardChain(Sub))) ==> isa(I,Sub).
((completelyAssertedCollection(Sub) / (\+ rtAvoidForwardChain(Sub)))) ==> ttMudIsaCol(Sub).
ttMudIsaCol(Sub) ==> (isa(I,Sub) ==> mudIsa(I,Sub)).
((completeIsaAsserted(I),mudIsa(I,Sub), {dif(Sub, Super)}, genls(Sub,Super),{ground(Sub:Super)}, \+ rtAvoidForwardChain(Super))) ==> mudIsa(I,Super).
*/



:- set_prolog_flag(do_renames,restore).



end_of_file.



























































/*

tCol(tCol).
tCol(tSet).
tCol(C)/( \+ ttExpressionType(C))==>tSet(C).
tCol(mtHybrid).
tCol(col_as_isa).
tCol(col_as_unary).
col_as_unary(col_as_unary).
col_as_unary(col_as_isa).
tCol(C)/( \+ col_as_unary(C))==>col_as_isa(C).
mtHybrid(baseKB).
tSet(tCol).
tSet(tSet).
genls(tSet,tCol).
genls(ttExpressionType,tCol).
genls(ttExpressionType,col_as_unary).
tSet(tExisting).
tCol(tRR).
genls(tRR,tRRP).
genls(tRRP,tRRP2).
col_as_unary(tRRP2).
tRR(iRR7).
% rtAvoidForwardChain(functorDeclares).
rtAvoidForwardChain(C):- loop_check(tCol(C)),compound(C).
rtAvoidForwardChain(meta_argtypes).
% rtAvoidForwardChain(completeIsaAsserted).
ttExpressionType(C)==>rtAvoidForwardChain(C).
genls(C,P)==>tCol(C),tCol(P).
(genls(C,P)/(C\=P)), completelyAssertedCollection(P)  ==> genlsFwd(C,P).
(genls(C,P)/(C\=P, \+ ttExpressionType(C) , \+ ttExpressionType(P) , \+ rtAvoidForwardChain(P) )) ==> genlsFwd(C,P).
genlsFwd(C,P) ==> (isa(I,C) ==> isa(I,P)).

not_undoable(G):-call_u(G).

ttTypeType(col_as_unary).
genl(ttTypeType,tCol).

tCol(col_as_unary).
col_as_unary(mtHybrid).
col_as_unary(completelyAssertedCollection).

tCol(C) ==> {atom(C),not_undoable((CI=..[C,I],assertz_if_new((CI:- (cwc,   loop_check(isa(I,C)))))))}.

col_as_unary(C) ==> {atom(C),not_undoable((CI=..[C,I],forall(retract(isa(I,C):-true),mpred_post1(CI)),retractall(col_as_isa(C))))}.
col_as_isa(C) ==> {atom(C),not_undoable((CI=..[C,I],forall(retract(CI:-true),mpred_post1(isa(I,C))),retractall(col_as_unary(C))))}.

% genls(tSet,functorDeclares).
rtArgsVerbatum(functorDeclares).
genls(completelyAssertedCollection,tSet).

isa(I,ttRelationType):-I==col_as_unary,!,fail.
isa(I,ttRelationType):-I==col_as_unary,!,fail.
isa(I,ttRelationType):-I==col_as_isa,!,fail.
isa(I,C):- functorDeclares==C, I== (==>) ,!,fail.
isa(I,C):- cwc, atom(C),loop_check((col_as_unary(C);\+col_as_isa(C))),loop_check(isa_w_type_atom(I,C)).

genlsFwd(C,P) ==> (isa(I,C) ==> isa(I,P)).

:- must( baseKB:isa(iRR7,tRR) ).
:- must( baseKB:isa(iRR7,tRRP) ).

:-  tRRP(iRR7) .

% :- break.

%:- defconcept(tCol,[or([ttExpressionType,tSet])]).
%:- defconcept([and([tCol,naf((ttExpressionType))])],tSet).


% tExisting(iRR7).
:- set_prolog_flag(logicmoo_motel,false).





:- if(current_prolog_flag(logicmoo_motel,true)).

isa(I,C)==>{assert_ind(I,C)}.
tSet(C)==> 
  ( tCol(C), % defprimconcept(C),
  (isa(I,C)==>{assert_ind(I,C)}),
  {CI=..[C,I],assertz_if_new((CI:- (cwc,   isa_backchaing(I,C))))}).

genls(Sub,Sup)==>(tCol(Sup),tCol(Sub),{defprimconcept(Sub,Sup),sb_primconcept(Sub,[supers([Sup])])}).
:- show_dag.
:- showEnvironment.
:- printAll(isa(_X,_Y)).
:- must( baseKB:isa(iRR7,tRR) ).
:- must( baseKB:isa(iRR7,tRRP) ).
:- must( baseKB:tRRP(iRR7) ).
%:-defconcept(tPred,[or([prologBuiltin,prologHybrid])]).


% :- break.
% :- \+ baseKB:tRRP(iRR7) -> (xlisting(iRR7),xlisting(tRRP)) ; true.

installedMotelHook.

:- endif.



% :- xlisting(tKnownID).
%?- isa(tKnownID,W).
%tExisting(X):-atom(X).



tSet(tKnownID).


*/

