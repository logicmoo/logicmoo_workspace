/*
 _________________________________________________________________________
|       Copyright (C) 1982                                                |
|                                                                         |
|       David Warren,                                                     |
|               SRI International, 333 Ravenswood Ave., Menlo Park,       |
|               California 94025, USA;                                    |
|                                                                         |
|       Fernando Pereira,                                                 |
|               Dept. of Architecture, University of Edinburgh,           |
|               20 Chambers St., Edinburgh EH1 1JZ, Scotland              |
|                                                                         |
|       This program may Be used, copied, altered or included in other    |
|       programs only for academic purposes and provided that the         |
|       authorship of the initial program is aknowledged.                 |
|       Use for commercial purposes without the previous written          |
|       agreement of the authors is forbidden.                            |
|_________________________________________________________________________|

*/

:- dynamic(adv_template_db/4).
:- dynamic(ditrans_lex80/13).
:- dynamic(standard_adj_db/4).

:- op(450,xfy,((:))).
:- op(400,xfy,((&))).
:- op(300,fx,(('`'))).
:- op(200,xfx,((--))).

must80(G):- \+ current_prolog_flag(debug_chat80,true),!, call(G).
must80(G):- call(G)*->true;(nop((wdmsg(failed(G)),ignore(on_x_fail(ftrace(G))))),wdmsg(failed(G)),fail,call(G)).

% logical form checker for chat80
lf80(Conj,G):- compound(Conj), (Conj=(Type1-TypeS)),!,lf80(Type1,lf80(TypeS,G)).
lf80(Type,G):- 
  subst(G,Type,Type0,P),
  must80(P),
  ((var(Type0);var(Type)) -> Type0=Type ; writeln(Type0=Type),must80(Type=Type0)).

i_sentence(S,G):- i_sentence1(S,G) *-> true ; i_sentence2(S,G).

i_sentence1(q(S),question80([],P)) :-
   i_s(S,P,[],0).
i_sentence1(decl(S),assertion80(P)) :-
   i_s(S,P,[],0).
i_sentence1(whq(X,S),question80([X],P)) :-
   i_s(S,P,[],0).
i_sentence1(imp(U,Ve,s(_,Verb,VArgs,VMods)),imp(U,Ve,V,Args)) :-
   must80(i_verb(Verb,V,_,active,posP(_Modal),Slots0,[],transparent)),
   must80(i_verb_args(VArgs,[],[],Slots0,Slots,Args,Args0,Up,-0)),
   append(Up,VMods,Mods),
   must80(i_verb_mods(Mods,_,[],Slots,Args0,Up,+0)).

:- create_prolog_flag(debug_chat80,false,[keep(true)]).
i_sentence2(S,G):- locally(set_prolog_flag(debug_chat80,true), i_sentence1(S,G)),!.
i_sentence2(Dunno,dunno(Dunno)).

i_np(there,Y,quantV(voidQ(_ArgInfo),_X,'`'(true),'`'(true),[],Y),[],_,_,XA,XA).
i_np(NP,Y,Q,Up,Id0,Index,XA0,XA) :-
   i_np_head(NP,Y,Q,Det,Det0,X,Pred,QMods,Slots0,Id0),
   held_arg(XA0,XA,Slots0,Slots,Id0,Id),
   i_np_rest(NP,Det,Det0,X,Pred,QMods,Slots,Up,Id,Index).

i_np_head(np(_,Kernel,_),Y,
      quantV(Det,T,Head,Pred0,QMods,Y),
      Det,Det0,X,Pred,QMods,Slots,_Id) :-
   lf80(Type,i_np_head0(Kernel,X,T,Det0,Head,Pred0,Pred,Slots)),
   Type-_=Y, Type-_=T.

i_np_rest(np(_,_,Mods),Det,Det0,X,Pred,QMods,Slots,Up,Id,Index) :-
   index_args(Det0,Index,Id,Det,IndexA),
   i_np_mods(Mods,X,Slots,Pred,QMods,Up,Id,IndexA).

held_arg(held_arg(Case,-Id,X),[],S0,S,Id,+Id) :-
   in_slot(S0,Case,X,Id,S,_).
held_arg(XA,XA,S,S,Id,Id).

% np(3+sg,nameOf(iran),[])
i_np_head0(nameOf(Name), Type-Name,Type-Name,identityQ,'`'(true),Pred,Pred,[]) :- 
  ignore(lf80(Type,name_template_LF(Name,Type))),!.
i_np_head0(wh(X),X,X,identityQ,'`'(true),Pred,Pred,[]):-!.
% np(3+sg,pronoun(neut),[])
i_np_head0(Else, Type-Name,Type-Name,identityQ,'`'(P),Pred,Pred,[]):-  Else \= np_head(_,_,_), !,
  lf80(Type,make_qualifiedBy(i_np_head0,Name,Type,Else,P)).

i_np_head0(np_head(Det,Adjs,Noun),X,T,Det,Head0,Pred0,Pred,Slots) :-
   i_adjs(Adjs,X,T,X,Head0,Head,Pred0,Pred),
   i_noun(Noun,X,Head,Slots).

i_np_head0(np_head(wh_det(V),Adjs,Noun),
      Type-X,Type-X,Det,'`'(true),Pred,Pred,
      [slot(prep(of),Type,X,_,comparator)]) :-
   lf80(Type,comparator_LF(Noun,Type,V,Adjs,Det)).

i_np_head0(np_head(quantV(Op0,N),Adjs,Noun),
      Type-X,Type-X,voidQ(_ArgInfo),'`'(P),Pred,Pred,[]) :- 
   lf80(Type,measure_LF(Noun,Type,Adjs,Units)),
   lf80(Type,pos_conversion_db(N,Op0,Type,V,Op)),
   must80(measure_op(Op,X,V--Units,P)).

i_np_head0(np_head(generic(ArgInfo),[],Value), Type-X,Type-X,voidQ(ArgInfo),'`'(X=Value),Pred,Pred,[]) :- !.


i_np_head0(Else, Type-Name,Type-Name,identityQ,'`'(P),Pred,Pred,[]):- may_qualify(Else),
   lf80(Type,make_qualifiedBy(i_np_head0,Name,Type,Else,P)).


make_qualifiedBy(_,Name,Type,Else,P):- show_call(always,P = qualifiedBy(Name,Type,Else)).
%may_qualify(_):- !,fail.
may_qualify(np_head(det(each),[],_)):-!,fail.
may_qualify(np_head(_,[],Act)):- atom(Act),atom_concat('actioned',_,Act), !,fail.
may_qualify(Else):- wdmsg(may_qualify(Else)).

%i_np_mods([],_,[],'`'(true),[],[],_,_).
i_np_mods(Mods,_,[],'`'(true),[],Mods,_,_).
i_np_mods([Mod|Mods],X,Slots0,Pred0,QMods0,Up,Id,Index) :-
   i_np_mod(Mod,X,Slots0,Slots,
            Pred0,Pred,QMods0,QMods,Up0,-Id,Index),
   append(Up0,Mods,Mods0),
   i_np_mods(Mods0,X,Slots,Pred,QMods,Up,+Id,Index).
i_np_mods(Mods,_,[Slot|Slots],'`'(true),QMods,Mods,Id,_) :-
   i_voids([Slot|Slots],QMods,Id).

i_voids([],[],_).
i_voids([Slot|Slots],[quantV(voidQ(_ArgInfo),X,'`'(true),'`'(true),[],_)|QMods],Id) :-
   nominal_slot(Slot,X,-Id), !,
   i_voids(Slots,QMods,+Id).
i_voids([_|Slots],QMods,Id) :-
   i_voids(Slots,QMods,Id).

i_rel(rel(X,S),X,P&Pred,Pred,QMods,QMods,Up,Id) :-
   i_s(S,P,Up,Id).
i_rel(reduced_rel(X,S),X,Pred,Pred,[A|QMods],QMods,Up,Id) :-
   i_s(S,A,Up,Id).
i_rel(conj(Conj,Left,Right),X,
      conj(Conj,LPred,LQMods,RPred,RQMods)&Pred,Pred,
      QMods,QMods,Up,Id) :-
   i_rel(Left,X,LPred,'`'(true),LQMods,[],[],-Id),
   i_rel(Right,X,RPred,'`'(true),RQMods,[],Up,+Id).

i_np_mod(prep_phrase(Prep,NP),
      X,Slots0,Slots,Pred,Pred,[QMod|QMods],QMods,Up,Id0,Index0) :-
   i_np_head(NP,Y,Q,LDet,LDet0,LX,LPred,LQMods,LSlots0,Id0),
   i_bind(Prep,Slots0,Slots1,X,Y,Id0,Function,P,PSlots,XArg),
   append(PSlots,Slots1,Slots),
   i_np_modify(Function,P,Q,QMod,Index0,Index),
   held_arg(XArg,[],LSlots0,LSlots,Id0,Id),
   i_np_rest(NP,LDet,LDet0,LX,LPred,LQMods,LSlots,Up,Id,Index).
i_np_mod(Mod,X,Slots,Slots,Pred0,Pred,QMods0,QMods,Up,Id,_) :-
   i_rel(Mod,X,Pred0,Pred,QMods0,QMods,Up,Id).

i_noun(Noun,Type-X,P,Slots) :-
   lf80(Type,noun_template(Noun,Type,X,P,Slots)).

i_bind(Prep,Slots0,Slots,_,X,Id,arg,P,[],[]) :-
   in_slot(Slots0,Case,X,Id,Slots,P),
   deepen_case(Prep,Case).
i_bind(prep(Prep),Slots,Slots,X,Y,_,adjoin,'`'(P),PSlots,XArg) :-
   i_adjoin(Prep,X,Y,PSlots,XArg,P).

i_np_modify(adjoin,P,N,N&P,_,unit).
i_np_modify(arg,F,N,N,Index0,Index) :-
   index_slot(F,Index0,Index).

in_slot([Slot|Slots],Case,X,Id,Slots,F) :-
   slot_match(Slot,Case,X,Id,F).
in_slot([Slot|Slots0],Case,X,Id,[Slot|Slots],F) :-
   in_slot(Slots0,Case,X,Id,Slots,F).

slot_match(slot(Case,Type,X,Id,F),Case,Type-X,Id,F).

i_adjs([],_X,T,T,Head,Head,Pred,Pred).
i_adjs([Adj|Adjs],X,T,T0,Head0,Head,Pred0,Pred) :-
   lf80(T-T1,i_adj(Adj,X,T,T1,Head0,Head1,Pred0,Pred1)),
   i_adjs(Adjs,X,T1,T0,Head1,Head,Pred1,Pred).


i_adj(sup(Op0,adj(Adj)),Type-X,Type-V,_,
      aggr(F,V,[Y,X],Head,'`'(P)&Pred),Head,'`'(true),Pred) :-
   must80(adj_sign_LF(Adj,Sign)),
   op_inverse(Op0,Sign,Op),
   i_sup_op(Op,F),
   lf80(Type,attribute_LF(Adj,Type,X,_,Y,P)).

i_adj(adj(Adj),Type-X,T,T,Head,Head,'`'(P)&Pred,Pred) :-
   lf80(Type,restriction_LF(Adj,Type,X,P)).
i_adj(adj(Adj),TypeX-X,TypeV-V,_,
   aggr(F,V,[X],Head,Pred),Head,'`'(true),Pred) :-
   lf80(TypeV+TypeX,aggr_adj_LF(Adj,TypeV,TypeX,F)).
i_adj(adj(Adj),TypeX-X,T,T,_,
      Head,Head,quantV(voidQ(_ArgInfo),TypeX-Y,'`'(P),'`'(Q)&Pred,[],_),Pred) :-
   lf80(TypeX,attribute_LF(Adj,TypeX,X,_,Y,P)),
   lf80(TypeX,standard_adj_db(Adj,TypeX,Y,Q)).


i_s(s(Subj,Verb,VArgs,VMods),Pred,Up,Id) :-
   i_verb(Verb,P,Tense,Voice,Neg,Slots0,XA0,Meta),
   i_subj(Voice,Subj,Slots0,Slots1,QSubj,SUp,'-'('-'(Id))),
   append(SUp,VArgs,TArgs),
   i_verb_args(TArgs,XA0,XA,Slots1,Slots,Args0,Args,Up0,+(-Id)),
   append(Up0,VMods,Mods),
   i_verb_mods(Mods,Tense,XA,Slots,Args,Up,+Id),
   reshape_pred(Meta,QSubj,Neg,P,Args0,Pred).

i_verb(verb(Root,Voice,Tense,_Aspect,Neg),
      PP,Tense,Voice,Det,Slots,XArg,Meta) :-
   slot_verb_template(Root,P,Slots,XArg,Meta),
   %(Neg\=posP(_)->trace;true),
   i_neg(Neg,Det),
   maybe_modalize(Neg,P,PP).

maybe_modalize(negP(Modal), P, PP):- atom(Modal),!,PP=..[Modal,P].
maybe_modalize(posP(Modal), P, PP):- atom(Modal),!,PP=..[Modal,P].
maybe_modalize(_,P,P).

reshape_pred(transparent,S,N,P,A,pred(S,N,P,A)).
reshape_pred(have(_MODAL),Subj,Neg,Verb0,
      [quantV(Det,X,Head0,Pred,QArgs,Y)|MRest],
      pred(Subj,Neg,Verb,[quantV(Det,X,Head,Pred,QArgs,Y)|MRest])) :-
   have_pred(Head0,Verb0,Head,Verb).

have_pred('`'(Head),Verb,'`'(true),(Head,Verb)).
have_pred(Head,Verb,Head,Verb) :-
   meta_head(Head).

meta_head(apply80(_,_)).
meta_head(aggr(_,_,_,_,_)).

i_neg(posP(_Modal),identityQ).
i_neg(negP(_Modal),not).

i_subj(Voice,Subj,Slots0,Slots,Quant,Up,Id) :-
   (active_passive_subjcase(Voice,Case)*->true;true),
   must80(verb_slot(arg(Case,Subj),[],[],Slots0,Slots,[Quant],[],Up,Id)).

i_verb_args(VArgs,XA0,XA,Slots0,Slots,Args0,Args,Up,Id) :-
   must80(fill_verb(VArgs,XA0,XA,Slots0,Slots,Args0,Args,Up,Id)).

active_passive_subjcase(active,subj).
active_passive_subjcase(passive,s_subj).

fill_verb([],XA,XA,Slots,Slots,Args,Args,[],_).
fill_verb([Node|Nodes0],XA0,XA,Slots0,Slots,Args0,Args,Up,Id) :-
   must80(verb_slot(Node,XA0,XA1,Slots0,Slots1,Args0,Args1,Up0,-Id)),
   append(Up0,Nodes0,Nodes),
   fill_verb(Nodes,XA1,XA,Slots1,Slots,Args1,Args,Up,+Id).

verb_slot(prep_phrase(Prep,NP),
      XArg0,XArg,Slots0,Slots,[Q|Args],Args,Up,Id) :-
   i_np(NP,X,Q,Up,Id,unit,XArg0,XArg),
   in_slot(Slots0,Case,X,Id,Slots,_),
   deepen_case(Prep,Case).
verb_slot(voidQ(ArgInfo),XA,XA,Slots,Slots,Args,Args,[],_) :-
   in_slot(Slots,arg_pred(ArgInfo),_,_,_,_).
verb_slot(prep_phrase(prep(Prep),NP),
      TXArg,TXArg,Slots0,Slots,[Q& '`'(P)|Args],Args,Up,Id0) :-
   in_slot(Slots0,arg_pred(_ArgInfo),X,Id0,Slots1,_),
   i_adjoin(Prep,X,Y,PSlots,XArg,P),
   i_np_head(NP,Y,Q,LDet,LDet0,LX,LPred,LQMods,LSlots0,Id0),
   held_arg(XArg,[],LSlots0,LSlots,Id0,Id),
   i_np_rest(NP,LDet,LDet0,LX,LPred,LQMods,LSlots,Up,Id,free),
   append(PSlots,Slots1,Slots).
verb_slot(arg(SCase,NP),
      XArg0,XArg,Slots0,Slots,[Q|Args],Args,Up,Id) :-
   i_np(NP,X,Q,Up,Id,unit,XArg0,XArg),
   in_slot(Slots0,Case,X,Id,Slots,_),
   deepen_case(SCase,Case).
verb_slot(adverb(Adv),XA,XA,Slots0,Slots,['`'(P)|Args],Args,[],Id) :-
   adv_template_db(Adv,Case,X,P),
   in_slot(Slots0,Case,X,Id,Slots,_).
verb_slot(arg(arg_pred(ArgInfo),AP),XA,XA,Slots0,Slots,Args0,Args,Up,Id) :-
   in_slot(Slots0,arg_pred(ArgInfo),X,Id,Slots,_),
   must80(i_pred(AP,X,Args0,Args,Up,Id)).

i_pred(conj(Conj,Left,Right),X,
      [conj(Conj,'`'(true),LQMods,'`'(true),RQMods)|QMods],
      QMods,Up,Id) :-
   i_pred(Left,X,LQMods,[],[],-Id),
   i_pred(Right,X,RQMods,[],Up,+Id).
i_pred(AP,T,['`'(Head)&Pred|As],As,[],_) :-
   i_adj(AP,T,_,_,Head,true,Pred,'`'(true)).
i_pred(value80(adj(Adj),wh(TypeY-Y)),Type-X,['`'(H)|As],As,[],_) :-
   lf80(Type,attribute_LF(Adj,Type,X,TypeY,Y,H)).

i_pred(comp(more,adj(less),NP),X,P,As,Up,Id) :-
  i_pred(comp(less,adj(great),NP),X,P,As,Up,Id).

i_pred(comp(Op0,adj(Adj),NP),X,[P1 & P2 & '`'(P3),Q|As],As,Up,Id) :-
   i_np(NP,Y,Q,Up,Id,unit,[],[]),
   must80(adj_sign_LF(Adj,Sign)),
   lf80(Type,i_measure(X,Adj,Type,U,P1)),
   lf80(Type,i_measure(Y,Adj,Type,V,P2)),
   op_inverse(Op0,Sign,Op),
   measure_op(Op,U,V,P3).
i_pred(prep_phrase(prep(Prep),NP),X,['`'(H),Q|As],As,Up,Id) :-
   i_np(NP,Y,Q,Up,Id,unit,[],[]),
   lf80(X-Y,adjunction_LF(Prep,X,Y,H)).

i_adjoin(with,TS-S,TV-Y,[slot(prep(of),TV,Z,_,free)],
        held_arg(poss,-_Id,TS-S),
        Y=Z).
i_adjoin(Prep,X,Y,[],[],P) :-
   lf80(X-Y,adjunction_LF(Prep,X,Y,P)).

i_measure(Type-X,Adj,Type,X,'`'(true)) :-
   lf80(Type,units_db(Adj,Type)).
i_measure(TypeX-X,Adj,TypeY,Y,quantV(voidQ(_ArgInfo),TypeY-Y,'`'(P),'`'(true),[],_)) :-
   lf80(TypeX+TypeY,attribute_LF(Adj,TypeX,X,TypeY,Y,P)).

i_verb_mods(Mods,_,XA,Slots0,Args0,Up,Id) :-
   fill_verb(Mods,XA,[],Slots0,Slots,Args0,Args,Up,-Id),
   i_voids(Slots,Args,+Id).

nominal_slot(slot(Kind,Type,X,Id,_),Type-X,Id) :-
   nominal_kind(Kind).

nominal_kind(prep(_)).
nominal_kind(poss).
nominal_kind(subj).
nominal_kind(dirO(_ArgInfo)).
nominal_kind(indO(_ArgInfo)).

i_sup_op(least,min).
i_sup_op(most, max).

pos_conversion_db(wh(Type-X),same,Type,X,identityQ).
pos_conversion_db(N,Op,_,N,Op):- number(N).
pos_conversion_db(N,Op,_,N,Op):- bind_pos('value',N).

measure_op(identityQ, X,X,    true).
measure_op(same,      X,Y,    X=Y).
measure_op(less,      X,Y,    exceeds(Y,X)).
measure_op(not+less,  X,Y, \+ exceeds(Y,X)).
measure_op(more,      X,Y,    exceeds(X,Y)).
measure_op(not+more,  X,Y, \+ exceeds(X,Y)).

op_inverse(most,-,least).
op_inverse(least,-,most).
op_inverse(same,-,same).
op_inverse(less,-,more).
op_inverse(more,-,less).
op_inverse(X,+,X).

noun_template(Noun,TypeV,V,'`'(P),
      [slot(poss,TypeO,O,Os,index)|Slots]) :-
   lf80(TypeV-TypeO,property_LF(Noun,TypeV,V,TypeO,O,P,Slots,Os,_)).

noun_template(Noun,TypeV,V,aggr(F,V,[],'`'(true),'`'(true)),
   [slot(prep(of),TypeS,_,_,free)]) :-
   lf80(TypeV-TypeS,aggr_noun_LF(Noun,TypeV,TypeS,F)).

noun_template(Noun,Type,X,'`'(P),Slots) :-
   lf80(Type,thing_LF_access(Noun,Type,X,P,Slots,_)).

noun_template(Noun,TypeV,V,apply80(F,P),
      [slot(prep(Of),TypeX,X,_,apply)]) :-
   lf80(TypeV,meta_noun_LF(Noun,Of,TypeV,V,TypeX,X,P,F)).

slot_verb_template(have(MODAL),Y=Z,
                [slot(subj,TypeS,S,-Id,free),
                 slot(dirO(_ArgInfo),TypeV,Y,_,free),
                 slot(prep(of),TypeV,Z,_,free)],
                held_arg(poss,-(-(+Id)),TypeS-S), have(MODAL)).
slot_verb_template(have(MODAL),Y=Z,
        [slot(subj,TypeS,S,-(-(Id)),free),
         slot(dirO(_ArgInfo),TypeV,Y,_,free),
         slot(prep(as),TypeV,Z,_,free)],
        held_arg(poss,-(-(-(+Id))),TypeS-S), have(MODAL)).
slot_verb_template(Verb,Pred,
      [slot(subj,TypeS,S,_,free)|Slots],[],transparent) :-
   must80(verb_type_lex(Verb,_+Kind)),
   slot_verb_kind(Kind,Verb,TypeS,S,Pred,Slots).

% BE
% slot_verb_kind(be(_MODAL),_,TypeS,S,subsumed_by(A,S),[slot(dirO(_ArgInfo),TypeS,A,_,free)]).
slot_verb_kind(be(_MODAL),_,TypeS,S,S=A,[slot(dirO(_ArgInfo),TypeS,A,_,free)]).
slot_verb_kind(be(_MODAL),_,TypeS,S,true,[slot(arg_pred(_ArgInfo),TypeS,S,_,free)]).
slot_verb_kind(iv,Verb,TypeS,S,Pred,Slots) :-
   lf80(TypeS,intrans_LF(Verb,TypeS,S,Pred,Slots,_)).
slot_verb_kind(tv,Verb,TypeS,S,Pred,
      [slot(dirO(_ArgInfo),TypeD,D,SlotD,free)|Slots]) :-
   lf80(TypeS-TypeD,trans_LF(Verb,TypeS,S,TypeD,D,Pred,Slots,SlotD,_)).
slot_verb_kind(dv(Prep),Verb,TypeS,S,Pred,
      [slot(dirO(_ArgInfo1),TypeD,D,SlotD,free),
       slot(indO(_ArgInfo2),TypeI,I,SlotI,free)|Slots]) :-
   fail,fail,fail,fail,
   lf80(TypeS+TypeD+TypeI,ditrans_lex80(Verb,Prep,TypeS,S,TypeD,D,TypeI,I,Pred,Slots,SlotD,SlotI,_)).
   % see no_repeats_dc(DC0,subj_obj_indirect_slots_LF(ditrans,verb_prep(Verb,Prep),TypeS,S,TypeD,D,TypeI,I,Pred,Slots,SlotI,SlotD,DC0)).

deepen_case(prep(at),time).
deepen_case(s_subj,dirO(_ArgInfo)).
deepen_case(s_subj,indO(_ArgInfo)).
deepen_case(prep(by),subj).
deepen_case(prep(to),indO(_ArgInfo)).
deepen_case(prep(of),poss).
deepen_case(X,X).

% ================================================================
% Determiner Indexing Table

index_slot(index,I,I).
index_slot(free,_,unit).
index_slot(apply,_,apply).
index_slot(comparator,_,comparator).

index_args(det(the(pl)),unit,I,set(I),index(I)) :- !.
index_args(wh_det(X),index(I),_,wh_det(I,X),unit) :- !.
index_args(generic(_ArgInfo),apply,_,lambda,unit) :-!.
index_args(D,comparator,_,identityQ,unit) :-
 ( indexable_arg(D); D=generic(_ArgInfo)), !.
index_args(D,unit,_,D,unit) :- !.
index_args(det(D),I,_,I,I) :-
   indexable_arg(D),
   is_my_index(I), !.
index_args(D,I,_,D,I).

indexable_arg(the(pl)).
indexable_arg(all).
indexable_arg(de(pl)).
indexable_arg(alle).

is_my_index(index(_I)).

:- fixup_exports.
