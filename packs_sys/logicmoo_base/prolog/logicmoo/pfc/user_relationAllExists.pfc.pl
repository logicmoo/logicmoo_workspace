%:- set_module(class(development)).
:- '$set_source_module'(baseKB).
%:- use_module(library(pfc)).
:- use_module(library(pfc)).

:- set_prolog_flag_until_eof(do_renames,term_expansion).

:- file_begin(pfc).
:- expects_dialect(pfc).


:- set_fileAssertMt(baseKB).
% ensure this file does not get unloaded with mpred_reset
:- prolog_load_context(file,F), ain(mpred_unload_option(F,never)).

:- dynamic(pass4/0).

/*
colen syntax and Mts

ist(mt1,isa(foo,bar)) == mt1:isa(foo,bar).



*/

/*

isa(foo,bar):-mt1.  % 5
isa(baz,bar):-mt1.  % 5
isa(X,Y,mt1):-with_assertion(mt1,isa(X,Y)). % 11

% x50 = 261


vs

mt1_isa(foo,bar).  % 4
mt1_isa(baz,bar). % 4
isa(X,Y,mt1):-mt1_isa(X,Y).  % 8

mung(mt1_isa(_,_),mt1). % 6 
mung(mt1_genls(_,_),mt1). % 6 

% x50 = 208 + 6 = 214

mt1_genls(_,_):-mt1_isa(_,_),mt1_genls(_,_).


vs.


mt1:isa(foo,bar). % 4
mt1:isa(baz,bar). % 4 
isa(X,Y,mt1):-mt1:isa(X,Y). % 8

% x50 = 208

vs.

isa(foo,bar,mt1). % 5
isa(baz,bar,mt1). % 5


genls(_,_,MT):-isa(_,_,MT),genls(_,_,MT).

% x50 = 250

*/



:- op(500,fx,'~').
:- op(1050,xfx,('==>')).
:- op(1050,xfx,'<==>').
:- op(1050,xfx,('<-')).
:- op(1100,fx,('==>')).
:- op(1150,xfx,('::::')).
:- kb_shared(tCol/1).
:- kb_shared(tHominid/1).

:- kb_shared(relationAllOnly/3).
:- kb_shared(rtTransitiveBinaryPredicate/1).
:- kb_shared(predInterArgIsa/1).

end_of_file.

rtArgsVerbatum(predInterArgIsa).

predInterArgIsa(mudSubPart(tBodyPart,tBodyPart)).

relationAllOnly(mudSubPart,tHumanBody,tBodyPart).

tCol(tHumanBody).

((relationAllOnly(Pred,Col1,Col2)/(G=..[Pred,VAL,Value])) ==> 
   (isa(VAL,Col1) ==> (( G ==> isa(Value,Col2))))).

cycl(kif('
   (implies 
       (and 
           (isa ?BPRED SymmetricBinaryPredicate) 
           (transitiveViaArg ?PRED ?BPRED ?N)) 
       (transitiveViaArgInverse ?PRED ?BPRED ?N))'
   )).




/*
:- sanity(( 
   fully_expand_real(((t(foo,a)/bar)=>baz),OUT),
   OUT = (((foo(a))/bar)=>baz))).

:- show_failure((
           P= genls,
           fully_expand(((t(P,A,B),t(P,B,C))/(ground(v(A,B,C)),A\==C,B\==C,A\==B) ==> t(P,A,C)),OUT),
   OUT= (((genls(A,B),genls(B,C)) /(ground(v(A,B,C)),A\==C,B\==C,A\==B))==> genls(A,C)))).
*/


rtTransitiveBinaryPredicate(mudSubPart).

% all_different_vals
(pass4,rtTransitiveBinaryPredicate(P)/ground(P) ==>
     ((t(P,A,B),t(P,B,C))/(ground(v(A,B,C)),A\==C,B\==C,A\==B) ==> t(P,A,C))).

/*
rtTransitiveBinaryPredicate(genls).
:- sanity(is_entailed_u(
  (((t(genls,A,B),t(genls,B,C))/( ground(v(A,B,C)),A\==C,B\==C,A\==B) ) ==> t(genls,A,C)))).
*/
% ((t(isa,A,B),t(genls,B,C)) ==> t(isa,A,C)).

:- dynamic(transitiveViaArgInverse/3).


((transitiveViaArg(PRED,BPRED,2),arity(PRED,2)) /ground(PRED:BPRED)) ==> clif((t(PRED,A,B) , t(BPRED,B,C)) => t(PRED,A,C)).
((transitiveViaArgInverse(PRED,BPRED,2),arity(PRED,2))/ground(PRED:BPRED)) ==> clif((t(PRED,A,B) & t(BPRED,C,B)) => t(PRED,A,C)).

((transitiveViaArg(PRED,BPRED,2),arity(PRED,3)) /ground(PRED:BPRED)) ==> clif((t(PRED,A,B,Z) , t(BPRED,B,C)) => t(PRED,A,C,Z)).
((transitiveViaArgInverse(PRED,BPRED,2),arity(PRED,3))/ground(PRED:BPRED)) ==> clif((t(PRED,A,B,Z) & t(BPRED,C,B)) => t(PRED,A,C,Z)).

((transitiveViaArg(PRED,BPRED,3),arity(PRED,3)) /ground(PRED:BPRED)) ==> clif((t(PRED,Z,A,B) , t(BPRED,B,C)) => t(PRED,Z,A,C)).
((transitiveViaArgInverse(PRED,BPRED,3),arity(PRED,3))/ground(PRED:BPRED)) ==> clif((t(PRED,Z,A,B) , t(BPRED,C,B)) => t(PRED,Z,A,C)).

:- dynamic(relationAllExists/3).


(relationAllExists(Pred,Col1,Col2) ==> (rtBinaryPredicate(Pred),tCol(Col1),tCol(Col2))).

% version that works but not safe
/*
relationAllExists(Pred,Col1,Col2)/(G=..[Pred,VAL,Value]) ==> 
  (isa(VAL,Col1) ==> 
    ((( {ignore(cnstrn(Value,isa(Value,Col2))},(~ (G/isa(Value,Col2)))) ==> ({Value=skRelationInstanceExistsFn(Pred,VAL,Col2)},isa(Value,Col2), G))))).
*/

/*
relationAllExists(Pred,Col1,Col2)/(G=..[Pred,VAL,Value]) ==> 
  (isa(VAL,Col1) ==> 
    ((~ (G/isa(Value,Col2))) ==> ({Value=skRelationInstanceExistsFn(Pred,VAL,Col2)},isa(Value,Col2), G))).


relationAllExists(Pred,Col1,Col2) ==>
 ({G1=..[Pred,VAL,Value1],G2=..[Pred,VAL,Value2],Value2=skRelationInstanceExistsFn(Pred,VAL,Col2)},
  (isa(VAL,Col1) ==> (
    ((((~ (G1/(isa(Value1,Col2))))) ==> (isa(Value2,Col2), G2)))))).

*/

:- dynamic(relationInstanceExists/3).

relationInstanceExists(Pred,VAL,D_COL) ==>
 ({SK= skRelationInstanceExistsFn(Pred,VAL,D_COL), G1=..[Pred,VAL,Missing],G2=..[Pred,VAL,SK],ISA=..[D_COL,SK]},
  (( ~ (G1/(isa(Missing,D_COL),is_non_skolem(Missing)))) ==> (G2,ISA))).


(relationAllExists(Pred,I_COL,D_COL)==> 
  clif(all(I,exists(D,  t(Pred,I,D) & isa(I,I_COL) & isa(D,D_COL))))).

((someTimesBuggy3rdOrder,relationAllExists(Pred,I_COL,D_COL))==>
 ({SK= skRelationAllExistsFn(Pred,VAL,D_COL), G1=..[Pred,VAL,Missing],G2=..[Pred,VAL,SK],ISA=..[D_COL,SK]},
  (isa(VAL,I_COL) ==> 
    ((((~ (G1/(isa(Missing,D_COL),is_non_skolem(Missing))))) ==> (G2,ISA)))))).


:- dynamic(relationExistsAll/3).
relationExistsAll(Pred,D_COL,I_COL) ==>
 ({SK= skRelationExistsAllFn(VAL,Pred,D_COL,I_COL), G1=..[Pred,Missing,VAL],G2=..[Pred,SK,VAL],ISA=..[D_COL,SK]},
   (isa(VAL,I_COL) ==>   
  (( ~ (G1/(isa(Missing,D_COL),is_non_skolem(Missing)))) ==> (G2,ISA)))).

:- dynamic(relationExistsInstance/3).
relationExistsInstance(Pred,D_COL,VAL) ==>
 ({SK= skRelationExistsInstanceFn(Pred,D_COL,VAL), G1=..[Pred,Missing,VAL],G2=..[Pred,SK,VAL],ISA=..[D_COL,SK]},
  (( ~ (G1/(isa(Missing,D_COL),is_non_skolem(Missing)))) ==> (G2,ISA))).



prologHybrid(relationMostInstance(rtBinaryPredicate,tCol,vtValue)).
relationMostInstance(BP,_,_)==>(rtBinaryPredicate(BP),rtRolePredicate(BP)).
prologHybrid(relationAllInstance(rtBinaryPredicate,tCol,vtValue)).
relationAllInstance(BP,_,_)==>rtBinaryPredicate(BP).

relationMostInstance(BP,TCol,_)==>rtBinaryPredicate(BP),tCol(TCol).
(relationMostInstance(Pred,_,Value),{\+number(Value)},argIsa(Pred,2,Type))==> isa(Value,Type).
%((relationMostInstance(Pred,Type,Value),{G=..[Pred,Inst,Value],GI=..[Pred,Inst,_]})) ==> (({GI=..[Pred,Inst,_]},isa(Inst,Type), ~GI) ==> G ).
relationMostInstance(Pred,Type,Value) ==> mdefault(isa(Inst,Type) ==> t(Pred,Inst,Value)).
% relationMostInstance(Pred,Type,Value) ==> mdefault( isa(Inst,Type) ==> ?Pred(Inst,Value) ).

% rtBinaryPredicate(P)<==>(tPred(P),arity(P,2)).

prologHybrid(relationAllInstance(rtBinaryPredicate,tCol,vtValue)).
relationAllInstance(BP,_,_)==>rtBinaryPredicate(BP).
relationAllInstance(Pred,_,Value),{\+number(Value)},argIsa(Pred,2,Type)==>(isa(Value,Type),isa(Pred,rtRolePredicate)).

relationAllInstance(Pred,I_COL,VAL) ==>
 ({G1=..[Pred,INST,_Missing],G2=..[Pred,INST,VAL]},
  (isa(INST,I_COL) ==> ( ~ G1 ==> G2))).

prologHybrid(relationInstanceAll(rtBinaryPredicate,vtValue,tCol)).

relationInstanceAll(Pred,VAL,I_COL) ==>
 ({G2=..[Pred,VAL,INST]},
  (isa(INST,I_COL) ==>  G2 )).

/*
(relationAllExists(Pred,Col1,Col2)/(Value = skRelationInstanceExistsFn(Pred,VAL,Col2),G=..[Pred,VAL,Value]))
  ==> 
  ((isa(VAL,Col1), G) ==> G).
*/

% version that works best but wrong
/*
(relationAllExists(Pred,Col1,Col2)/(G=..[Pred,VAL,Value])) ==> 
  ((isa(VAL,Col1),G,isa(Value,Col2)) ==> unneeded(relationAllExists(Pred,VAL,Col2))).

((relationAllExists(Pred,Col1,Col2)/(G=..[Pred,VAL,Value]))==>
   (isa(VAL,Col1) ==> ((~G , ~unneeded(relationAllExists(Pred,VAL,Col2))) ==> ({Value=skRelationInstanceExistsFn(Pred,VAL,Col2)},isa(Value,Col2), G)))).
*/


/*
relationAllExists(Pred,Col1,Col2)/(G=..[Pred,VAL,Value]) ==> 
  (isa(VAL,Col1) ==> 
    ((( ~((G,isa(Value,Col2)))) ==> ({Value=skRelationInstanceExistsFn(Pred,VAL,Col2)},isa(Value,Col2), G)))).
*/


:- if(baseKB:startup_option(datalog,sanity);baseKB:startup_option(clif,sanity)).

%isa(iExplorer1,tHumanControlled).
%isa(iExplorer2,tHumanControlled).
%genls(tHumanControlled,tHominid).
genls(tHumanControlled,tAgent).

isa(iExplorer1,tHominid).


%mudSubPart(iExplorer2, iBody2).
%mudSubPart(iExplorer1, iHumanBody1).

%isa(iHumanBody1,tHumanBody).


tCol(tHumanBody).
genls(tHumanBody,tBodyPart).


%:- mpred_trace_exec.
:- mpred_trace.
:- mpred_warn.

% :- cls.

:- printAll(tHominid(_)).
:- printAll(tHumanBody(_)).

:-dmsg("-------------------------------------SDFDFSDFSDFSDFSDFSDDDDDDDDDDDDDDDDDDDD").


pass4,relationAllExists(mudSubPart,tHominid,tHumanBody).

:- dbreak,read(_),read(_).

:- dbreak,read(_),read(_),dmsg("SDFDFSDFSDFSDFSDFSDDDDDDDDDDDDDDDDDDDD-------------------------------------").



==>((
pass4,relationAllExists(mudSubPart,tHumanBody,isEach(tHumanHead,tHumanNeck,tHumanUpperTorso,tHumanLowerTorso,tHumanPelvis,tHumanArms,tHumanLegs))
)).
==>((pass4,relationAllExists(mudSubPart,tHumanHead,isEach(tHumanFace,tHumanHair)))).


:- endif.


:- if(baseKB:startup_option(datalog,sanity);baseKB:startup_option(clif,sanity)).

% :- mpred_spy_all.

:- endif.

/*
have to confirm how *most* works
==>
'(==>
  (relationInstanceMost ?BINPRED ?THING ?COL2)
     (=> 
       (isa ?THING ?COL1)        
       (relationExistsMost ?BINPRED ?COL1 ?COL2)))'.


(relationInstanceMost ?BINPRED ?THING ?COL2) <==> 
   (most :ARG3 
   (SubcollectionOfWithRelationFromFn :ARG3 :ARG1 :ARG2))

*/

isa(skRelationAllExistsFn(P,A,C),C):- nonvar(P),nonvar(A),tCol(C).

:- set_prolog_flag(do_renames,restore).
end_of_file.

:- if(baseKB:startup_option(datalog,sanity);baseKB:startup_option(clif,sanity)).

:- if((current_prolog_flag(runtime_debug,D),D>2)).
:- listing(mudSubPart).
:- endif.
%:-rtrace((isa(Inst,tHumanNeck),mudSubPart(iExplorer1,Inst))).
%:-rtrace((mudSubPart(iExplorer1,Inst),isa(Inst,tHumanNeck))).
%:- must((isa(Inst,tHumanHair),mudSubPart(iExplorer1,Inst))).
%:- must((mudSubPart(iExplorer1,Inst),isa(Inst,tHumanNeck))).
%:- must((mudSubPart(iExplorer1,Inst),isa(Inst,tHumanHair))).
%:- listing(mudSubPart).


:- endif.



%prologHybrid(defnIff(ttExpressionType,ftTerm)).
%defnIff(X,_)==>ttExpressionType(X).


