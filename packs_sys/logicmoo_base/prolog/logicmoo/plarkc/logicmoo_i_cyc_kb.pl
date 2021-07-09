/* 
% ===================================================================
% File 'logicmoo_i_cyc_kb.pl'
% Purpose: Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface.pl' 1.0.0
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which make us more like Cyc
% special module hooks into the logicmoo engine allow
% syntax to be recocogized via our CycL/KIF handlers 
%
% Dec 13, 2035
% Douglas Miles
*/

% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/plarkc/logicmoo_i_cyc_kb.pl
:- module(logicmoo_i_cyc_kb,[]).
:- set_module(class(development)).
isa_db(I,C):-clause(isa(I,C),true).
:- '$set_source_module'(baseKB).

/*
:- multifile(kb7166:assertion_content/3).
:- dynamic(kb7166:assertion_content/3).

:- multifile(kb7166:assertion_content/4).
:- dynamic(kb7166:assertion_content/4).

:- multifile(kb7166:assertion_forward/1).
:- dynamic(kb7166:assertion_forward/1).

:- multifile(kb7166:assertion_code/1).
:- dynamic(kb7166:assertion_code/1).

:- multifile(kb7166:assertion_mt/2).
:- dynamic(kb7166:assertion_mt/2).

:- multifile(kb7166:assertion_variable_guard/2).
:- dynamic(kb7166:assertion_variable_guard/2).

:- multifile(kb7166:assertion_varnames/2).
:- dynamic(kb7166:assertion_varnames/2).
*/

cyc_defined(assertion_content(A,B)):- if_defined(nlkb7166:acnl(A,B)).
cyc_defined(assertion_content(A,B)):- !, if_defined(kb7166:assertion_content(A,B)).
cyc_defined(assertion_content(A,B,C)):- if_defined(nlkb7166:acnl(A,B,C)).
cyc_defined(assertion_content(A,B,C)):- !, if_defined(kb7166:assertion_content(A,B,C)).
cyc_defined(assertion_content(A,B,C,D)):- if_defined(nlkb7166:acnl(A,B,C,D)).
cyc_defined(assertion_content(A,B,C,D)):- !, if_defined(kb7166:assertion_content(A,B,C,D)).
cyc_defined(assertion_content(A,B,C,D,E)):- if_defined(nlkb7166:acnl(A,B,C,D,E)).
cyc_defined(assertion_content(A,B,C,D,E)):- !, if_defined(kb7166:assertion_content(A,B,C,D,E)).
cyc_defined(ELSE):- if_defined(kb7166:ELSE).

iface_tinyKB(P):- if_defined(tinyKB(P)).

:- baseKB:export((is_better_backchained/1,
          addCycL/1,
          addCycL1/1,
          addCycL1/1,
          cycl_to_mpred/2,
          as_cycl/2,
          is_better_backchained/1,
          rtEscapeFunction/1)).
% ===================================================================
% OPERATOR PRECEDANCE
% ===================================================================

:- op(500,fx,'~').
:- op(1050,xfx,('==>')).
:- op(1050,xfx,'<==>').
:- op(1050,xfx,('<-')).
:- op(1100,fx,('==>')).
:- op(1150,xfx,('::::')).
:- 
 system:((
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'))).


is_simple_gaf(V):-not(compound(V)),!.
is_simple_gaf(V):-needs_canoncalization(V),!,fail.
is_simple_gaf(V):-functor(V,F,A),member(F/A,[isa/2,genls/2,argQuotedIsa/3,afterAdding/2,afterRemoving/2]),!.
is_simple_gaf(V):-needs_indexing(V),!,fail.
is_simple_gaf(_).

needs_indexing(V):-compound(V),arg(_,V,A),not(is_simple_arg(A)),!,fail.

is_simple_arg(A):-not(compound(A)),!.
is_simple_arg(A):-functor(A,Simple,_),rtEscapeFunction(Simple).

% :- dynamic(vtUnreifiableFunction/1).
rtEscapeFunction('TINYKB-ASSERTION').
rtEscapeFunction('uQuoteFn').
rtEscapeFunction(X):- clause_b('rtUnreifiableFunction'(X)).

needs_canoncalization(CycL):-is_ftVar(CycL),!,fail.
needs_canoncalization(CycL):-functor(CycL,F,_),logicmoo_u_cyc_kb_tinykb:isa_db(F,'rtSentenceOperator').
needs_canoncalization(CycL):-needs_indexing(CycL).

is_better_backchained(CycL):-is_ftVar(CycL),!,fail.
is_better_backchained(CycL):-functor(CycL,F,_),logicmoo_u_cyc_kb_tinykb:isa_db(F,'rtSentenceOperator').
is_better_backchained(V):-unnumbervars(V,FOO),(((each_subterm(FOO,SubTerm),nonvar(SubTerm),logicmoo_u_cyc_kb_tinykb:isa_db(SubTerm,rtAvoidForwardChain)))),!.


as_cycl(VP,VE):-subst(VP,('-'),(~),V0),subst(V0,('v'),(or),V1),subst(V1,('exists'),(exists),V2),subst(V2,('&'),(and),VE),!.

kif_to_boxlog_ex(I,O):- cyc_defined(kif_to_boxlog(I,M)),cyc_defined(boxlog_to_pfc(M,O)).

:- kb_global(baseKB:and/7).

tiny_to_kif:- 
  forall(load_order(C),forall(tinyKB(C,MT,STR),with_current_why(tinyKB(C,MT,STR),addCycL(C)))).

load_order(isa(_,tFunction)).
load_order(isa(_,ttExpressionType)).
load_order(isa(_,tRelation)).
load_order(isa(_,tCol)).
load_order(P):- iface_tinyKB(isa(F,rtWFFConstraintSatisfactionPredicate)),iface_tinyKB(arity(F,A)),functor(P,F,A).
load_order(P):- iface_tinyKB(isa(F,rtWFFConstraintPredicate)),iface_tinyKB(arity(F,A)),functor(P,F,A).
load_order(P):- when(nonvar(P),\+ kif_hook(P)).
load_order(P):- when(nonvar(P), kif_hook(P)).

:- dynamic(addTiny_added/1).
addCycL(V):- must_be(nonvar,V),cycl_to_mpred(V,VC),addCycL1(VC),!.

addCycL1(V):-addTiny_added(V),!.
addCycL1(V):-asserta(addTiny_added(V)),unnumbervars(V,VE),
  % must(nop(remQueuedTinyKB(VE))),
  show_call(ain(VE)).


sent_to_conseq(CycLIn,Consequent):- into_mpred_form_locally(CycLIn,CycL),
  ignore((tiny_support(CycL,_MT,CALL),retract(CALL))),
  must(cycl_to_mpred(CycL,Consequent)),!.


cycl_to_mpred(V,Out):-cycl_to_mpred0(V,Out).

cycl_to_mpred0(V,V):- var(V),!.
cycl_to_mpred0((V1 , V2),Out):-!,cycl_to_mpred0(V1,Out),cycl_to_mpred0(V2,Out),!.
cycl_to_mpred0([V1 | V2],Out):-!,cycl_to_mpred0(V1,Out),cycl_to_mpred0(V2,Out),!.
cycl_to_mpred0((TRUE=>V),Out):-is_src_true(TRUE),cycl_to_mpred0(V,Out),!.
cycl_to_mpred0(<=(V , TRUE),Out):-is_src_true(TRUE),cycl_to_mpred0(V,Out),!.
cycl_to_mpred0((V :- TRUE),Out):-is_src_true(TRUE),cycl_to_mpred0(V,Out),!.
cycl_to_mpred0(V,Out):- into_mpred_form_locally(V,M),V\=@=M,!,cycl_to_mpred0(M,Out),!.
cycl_to_mpred0((V :- A),(V :- A)):- !.
cycl_to_mpred0(V,Out):- cyc_to_pdkb(V,VE),cycl_to_mpred1(VE,Out).

cycl_to_mpred1(V,Out):-defunctionalize('implies',V,VE),V\=@=VE,!,cycl_to_mpred1(VE,Out).
cycl_to_mpred1(V,Out):-is_simple_gaf(V),!,cycl_to_mpred2(V,Out),!.
cycl_to_mpred1(V,Out):-kif_to_boxlog_ex(V,VP),[V]\=@=VP,!,as_cycl(VP,VE),cycl_to_mpred0(VE,Out).
cycl_to_mpred1(V,Out):-cycl_to_mpred2(V,Out),!.

cycl_to_mpred2(V,Out):-unnumbervars(V,Out),!.

%  cycl_to_mpred( (grandparent('$VAR'('G'),'$VAR'('C')) => exists('$VAR'('P'), and(parent('$VAR'('G'),'$VAR'('P')),parent('$VAR'('P'),'$VAR'('C'))))),O).



into_mpred_form_locally(V,V):- current_prolog_flag(logicmoo_load_state,making_renames),!.
into_mpred_form_locally(V,R):- maybe_notrace((into_mpred_form(V,R), R\= isa(_,not))),!. 

freeze_u(V,G):- freeze(V,call_u(G)).

:- kb_global(baseKB:rtLogicalConnective/1).

get_LogicalConnective(F):- call_u(rtLogicalConnective(F)).

as_compound(G):- compound(G),!.
as_compound(G):- get_LogicalConnective(F),connective_arity(F,A),functor(G,F,A).
as_compound(G):- between(2,11,A),functor(G,t,A).

connective_arity(F,A):- connective_arity0(F,A).
connective_arity(_,A):- between(2,11,A).
connective_arity0(equiv,2):-!.
connective_arity0(implies,2):-!.
connective_arity0(not,1):-!.

inner_connective(F) :- get_LogicalConnective(F), \+ connective_arity0(F,_).

:- multifile(baseKB:istAsserted/2).
:- kb_global(baseKB:istAsserted/2).


:- assert(((istAsserted(MT,P):- cyc_defined(assertion_content(ist,MT,P,_))))).
%istAsserted(P,MT):- as_compound(P),istAsserted0(P,MT).
:- assert(((istAsserted(MT,P):- asserted_id(P,ID),cyc_defined(assertion_mt(ID,MT))))).

baseKB:tAsserted(ist(MT,P)):- !, istAsserted(MT,P).
baseKB:tAsserted(P):- 
  asserted_id(P,_).

:- multifile(baseKB:ist/2).
:- kb_global(baseKB:ist/2).
:- assert(((ist(MT,P):- istAsserted(MT,P)))).


% Y=verbSemTrans(xIndicateTheWord,X,xTransitiveThatClauseFrame,and(isa('ACTION',eventInformationTransferEvent),informationOrigin('ACTION','SUBJECT'),infoTransferred('ACTION','CLAUSE'))),rtrace(kif_to_boxlog(Y,BL)).


asserted_id(P,ID):- compound(P), P=..[F,F1|ARGS],append(ARGS,[ID],ARGSID),
   (F==t -> (AP=..[assertion_content,F1|ARGSID],nop(freeze_u(F1,\+ get_LogicalConnective(F1))));
            AP=..[assertion_content,F,F1|ARGSID]),!,
   cyc_defined(AP),
   varnamify(ID),
   guardify(ID).
asserted_id(PO,ID):- var(PO),
   % current_predicate(assertion_content/N),
   between(3,13,N2),N is 16-N2,
   current_predicate(assertion_content/N),
   functor(AP,assertion_content,N),
   AP=..[assertion_content,F|PARGS],
   append(ARGS,[ID],PARGS),
   cyc_defined(AP),
   ((
    varnamify(ID),
    ((atom(F) -> P=..[F|ARGS]; P=..[t,F|ARGS])),
    litterally_guard(ID,P,PO))).


litterally_guard(ID,I,O):- get_guard(ID,Guard),!,must_det(and_conj_to_list(Guard,List)),must_det(add_guard_list_needed(I,List,O)),!.
litterally_guard(_,IO,IO).

add_guard_list(IO,[],IO):-!.
add_guard_list(I,[List],List=>I):-!.
add_guard_list(I,List,AList=>I):- AList=..[and|List].

add_guard_list_needed(IO,[],IO):-!.
add_guard_list_needed(I,List,O):-remove_unneeded(I,List,AList),!,add_guard_list(I,AList,O).

remove_unneeded(I,[List|ListIS],AList):- !,
  (skip_guard(I,List) -> remove_unneeded(I,ListIS,AList);
    (remove_unneeded(I,ListIS,MList),AList=[List|MList])).
remove_unneeded(_,[],[]).



guardify(ID):- get_guard(ID,Guard),!,must_det(and_conj_to_list(Guard,List)),must_maplist(maybe_add_guard,List),!.
guardify(_).

get_guard(ID,Guard):- compound(ID),functor(ID,F,A),get_guard(ID,F,A,Guard).
get_guard(ID,_,1,Guard):- !, cyc_defined(assertion_variable_guard(ID,Guard)).
get_guard(ID,F,_,Guard):- functor(GID,F,1),cyc_defined(assertion_variable_guard(GID,Guard)),!,
                          term_variables(GID+Guard,GVars), 
                          (GVars=[GV] -> ID=..[F,GV|_] ; (ID=..[F|GVars])).

get_guard(ID,_,_,Guard):- cyc_defined(assertion_variable_guard(ID,Guard)).

  

skip_guard(_,'quotedIsa'(_,EXPR)):-EXPR='ftExpression'.
skip_guard(I,Var):- sub_var(Var,I).

maybe_add_guard(G):- skip_guard(_,G),!.
maybe_add_guard(G):- term_variables(G,Vars),maplist(maybe_add_guard_2(G),Vars).
maybe_add_guard_2(G,Var):-add_cond(Var,G).

and_conj_to_list(C,[C]):- var(C),!.
and_conj_to_list([],[]):- !.
and_conj_to_list(C,C):- \+ compound(C),!.
and_conj_to_list(AND,List):- AND=..[and|List],!.
and_conj_to_list(C,[C]).

varnamify(ID):- cyc_defined(assertion_varnames(ID,NAMES)),!,ID=..[_|VARS],maplist(varnamify,VARS,NAMES).
varnamify(_).
varnamify(Var,String):- string_to_atom(String,Atom),nb_current('$variable_names',Was),!,b_setval('$variable_names',[Atom=Var|Was]),
 name_variable(Var,Atom).

badz:- asserted_id(t(P,A,zzzz),ID),dmsg(badz(asserted_id(t(P,A,zzzz),ID))),fail.
badz:- asserted_id(t(P,zzzz,B),ID),dmsg(asserted_id(t(P,zzzz,B),ID)),fail.
badz:- asserted_id(t(zzzz,A,B),ID),dmsg(asserted_id(t(zzzz,A,B),ID)),fail.

test_kb_boxlog:- asserted_id(P,ID),nl,nl,compound(ID),wdmsg(asserted_id(P,ID)),test_boxlog(P).


%:- baseKB:ain((tAsserted(rtLogicalConnective(F))==>rtLogicalConnective(F))).

:- baseKB:ain(rtArgsVerbatum(tAsserted)).

:- baseKB:ain(((tAsserted(mtCycInternalAnthropacity(MT))==> mtUndressedMt(MT)))).

:- ain((baseKB:mtUndressedMt(iEnglishParaphraseMt))).

cyc_ain(P):- mpred_ainz(P,(kb7166,ax)),writeq(P),nl.

fix_head(HEAD,FHEAD):- fully_expand(HEAD,FHEAD).

pred_in_mt(F,A,MT,Type):- no_repeats(pred_in_mt0(F,A,MT,Type)).

pred_in_mt0(F,A,MT0,Type0):-
  pred_in_mt1(_QQ,F,A,MT0,Type0).

pred_in_mt1(QQ,F,A,MT0,Type0):- 
  asserted_id(QQ,ID),
  cyc_defined(assertion_mt(ID,MT)),
  append_dir(ID,fact,Type),  
  preds_fa_s(Type,Type0,MT,QQ,F,A,MT0).

append_dir(ID,I,O):- cyc_defined(assertion_forward(ID)),!,append_dir0(f,I,O).
append_dir(ID,I,O):- cyc_defined(assertion_backward(ID)),!,append_dir0(b,I,O).
append_dir(ID,I,O):- cyc_defined(assertion_code(ID)),!,append_dir0(c,I,O).
append_dir(_, I,O):- append_dir0(u,I,O).

append_dir0(C,fact,C):-!.
append_dir0(C,I,O):- O=..[C,I].


preds_fa_s(_Type,_Type0,_MT,QQ,_,_,_MT0):- \+ compound(QQ),!,fail.
preds_fa_s(Type,Type0,_,ist(MT,QQ),F0,A0,MT0):- !,preds_fa_s(Type,Type0,MT,QQ,F0,A0,MT0).
preds_fa_s(Type,Type0,MT,not(Q),F0,A0,MT0):- !,preds_fa_s(not(Type),Type0,MT,Q,F0,A0,MT0).
preds_fa_s(Type,Type0,MT,knows(A,Q),F0,A0,MT0):- !, preds_fa_s(knows(Type),Type0,modal(A,MT),Q,F0,A0,MT0).
preds_fa_s(Type,Type0,MT,implies(P,Q),F0,A0,MT0):- !, (preds_fa_s(antec(Type),Type0,MT,P,F0,A0,MT0); preds_fa_s(consq(Type),Type0,MT,Q,F0,A0,MT0)).
preds_fa_s(Type,Type0,MT,QQ,F0,A0,MT0):- 
   functor(QQ,F,A),
   ((get_LogicalConnective(F),append_dir0(F,Type,FType))
      -> (arg(_,QQ,Arg),preds_fa_s(FType,Type0,MT,Arg,F0,A0,MT0)) ;
       (F0=F,A0=A,MT=MT0,Type=Type0)).

scyc:- forall(pred_in_mt(F,A,MT,Type), cyc_ain(cycPredMtType(F,A,MT,Type))).

% ?- assertion_content(isa,X,Y,O),assertion_mt(O,MT).
/*

:- forall(
 ((% current_predicate(assertion_content/N),
   between(3,13,N),
   functor(P,assertion_content,N),P=..[assertion_content,F|PARGS],
   functor(PP,assertion_content,N),PP=..[assertion_content,F|FARGS],
   append(ARGS,[ID],PARGS),
   assertion_mt(ID,MT))),
   forall(no_repeats(F-MT,(P,assertion_mt(ID,MT))),
    (writeq(cycPredArity(F,N,MT)),nl))).

    ((atom(F) -> HEAD=..[F|ARGS]; HEAD=..[ac,F|ARGS]),
  fix_head(HEAD,FHEAD),
    dmsg((FHEAD :- PP))))).

*/

:- fixup_exports.

