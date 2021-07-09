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
|       This program may be used, copied, altered or included in other    |
|       programs only for academic purposes and provided that the         |
|       authorship of the initial program is aknowledged.                 |
|       Use for commercial purposes without the previous written          |
|       agreement of the authors is forbidden.                            |
|_________________________________________________________________________|

*/

:- op(450,xfy,:).
:- op(400,xfy,&).
:- op(300,fx,'`' ).
:- op(200,xfx,--).

:- dynamic(adv_template/4).
:- dynamic(standard/4).
:- dynamic(ditrans_db/13).

i_sentence(q(S),question80([],P)) :-
   i_s(S,P,[],0).
i_sentence(decl(S),assertion80(P)) :-
   i_s(S,P,[],0).
i_sentence(whq(X,S),question80([X],P)) :-
   i_s(S,P,[],0),!.
i_sentence(imp(U,Ve,s(_,Verb,VArgs,VMods)),imp(U,Ve,V,Args)) :-
   i_verb(Verb,V,_,active,posP,Slots0,[],transparent),
   i_verb_args(VArgs,[],[],Slots0,Slots,Args,Args0,Up,-0),
   append(Up,VMods,Mods),
   i_verb_mods(Mods,_,[],Slots,Args0,Up,+0).
i_sentence(Dunno,dunno(Dunno)) :-!.

i_np(there,Y,quantV(voidQ,_X,'`' true,'`' true,[],Y),[],_,_,XA,XA).
i_np(NP,Y,Q,Up,Id0,Index,XA0,XA) :-
   i_np_head(NP,Y,Q,Det,Det0,X,Pred,QMods,Slots0,Id0),
   held_arg(XA0,XA,Slots0,Slots,Id0,Id),
   i_np_rest(NP,Det,Det0,X,Pred,QMods,Slots,Up,Id,Index).

i_np_head(np(_,Kernel,_),Y,
      quantV(Det,T,Head,Pred0,QMods,Y),
      Det,Det0,X,Pred,QMods,Slots,_Id) :-
   i_np_head0(Kernel,X,T,Det0,Head,Pred0,Pred,Slots),
   Type-_=Y, Type-_=T.

i_np_rest(np(_,_,Mods),Det,Det0,X,Pred,QMods,Slots,Up,Id,Index) :-
   index_args(Det0,Index,Id,Det,IndexA),
   i_np_mods(Mods,X,Slots,Pred,QMods,Up,Id,IndexA).

held_arg(held_arg(Case,-Id,X),[],S0,S,Id,+Id) :-
   in_slot(S0,Case,X,Id,S,_).
held_arg(XA,XA,S,S,Id,Id).

i_np_head0(np_head(Det,Adjs,Noun),X,T,Det,Head0,Pred0,Pred,Slots) :-
   i_adjs(Adjs,X,T,X,Head0,Head,Pred0,Pred),
   i_noun(Noun,X,Head,Slots).
i_np_head0(np_head(int_det(V),Adjs,Noun),
      Type-X,Type-X,Det,'`' true,Pred,Pred,
      [slot(prep(of),Type,X,_,comparator_LF)]) :-
   comparator_LF(Noun,Type,V,Adjs,Det).
i_np_head0(np_head(quantV(Op0,N),Adjs,Noun),
      Type-X,Type-X,voidQ,'`' P,Pred,Pred,[]) :-
   measure_LF(Noun,Type,Adjs,Units),
   conversion(N,Op0,Type,V,Op),
   measure_op(Op,X,V--Units,P).
i_np_head0(nameOf(Name),
      Type-Name,Type-Name,identityQ,'`' true,Pred,Pred,[]) :-
   name_template_LF(Name,Type).
i_np_head0(wh(X),X,X,identityQ,'`' true,Pred,Pred,[]).

%i_np_mods([],_,[],'`' true,[],[],_,_).
i_np_mods(Mods,_,[],'`' true,[],Mods,_,_).
i_np_mods([Mod|Mods],X,Slots0,Pred0,QMods0,Up,Id,Index) :-
   i_np_mod(Mod,X,Slots0,Slots,
            Pred0,Pred,QMods0,QMods,Up0,-Id,Index),
   append(Up0,Mods,Mods0),
   i_np_mods(Mods0,X,Slots,Pred,QMods,Up,+Id,Index).
i_np_mods(Mods,_,[Slot|Slots],'`' true,QMods,Mods,Id,_) :-
   i_voids([Slot|Slots],QMods,Id).

i_voids([],[],_).
i_voids([Slot|Slots],[quantV(voidQ,X,'`' true,'`' true,[],_)|QMods],Id) :-
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
   i_rel(Left,X,LPred,'`' true,LQMods,[],[],-Id),
   i_rel(Right,X,RPred,'`' true,RQMods,[],Up,+Id).

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
   noun_template(Noun,Type,X,P,Slots).

i_bind(Prep,Slots0,Slots,_,X,Id,arg,P,[],[]) :-
   in_slot(Slots0,Case,X,Id,Slots,P),
   deepen_case(Prep,Case).
i_bind(prep(Prep),Slots,Slots,X,Y,_,adjoin,'`' P,PSlots,XArg) :-
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
   i_adj(Adj,X,T,T1,Head0,Head1,Pred0,Pred1),
   i_adjs(Adjs,X,T1,T0,Head1,Head,Pred1,Pred).

i_adj(adj(Adj),Type-X,T,T,Head,Head,'`' P&Pred,Pred) :-
   restriction_LF(Adj,Type,X,P).
i_adj(adj(Adj),TypeX-X,TypeV-V,_,
   aggr(F,V,[X],Head,Pred),Head,'`' true,Pred) :-
   aggr_adj(Adj,TypeV,TypeX,F).
i_adj(sup(Op0,adj(Adj)),Type-X,Type-V,_,
      aggr(F,V,[Y,X],Head,'`' P&Pred),Head,'`' true,Pred) :-
   chat_sign(Adj,Sign),
   op_inverse(Op0,Sign,Op),
   i_sup_op(Op,F),
   attribute_LF(Adj,Type,X,_,Y,P).
i_adj(adj(Adj),TypeX-X,T,T,_,
      Head,Head,quantV(voidQ,TypeX-Y,'`' P,'`' Q&Pred,[],_),Pred) :-
   attribute_LF(Adj,TypeX,X,_,Y,P),
   standard(Adj,TypeX,Y,Q).

i_s(s(Subj,Verb,VArgs,VMods),Pred,Up,Id) :-
   i_verb(Verb,P,Tense,Voice,Neg,Slots0,XA0,Meta),
   i_subj(Voice,Subj,Slots0,Slots1,QSubj,SUp,'-'('-'(Id))),
   append(SUp,VArgs,TArgs),
   i_verb_args(TArgs,XA0,XA,Slots1,Slots,Args0,Args,Up0,+(-Id)),
   append(Up0,VMods,Mods),
   i_verb_mods(Mods,Tense,XA,Slots,Args,Up,+Id),
   reshape_pred(Meta,QSubj,Neg,P,Args0,Pred).

i_verb(verb(Root,Voice,Tense,_Aspect,Neg),
      P,Tense,Voice,Det,Slots,XArg,Meta) :-
   verb_template(Root,P,Slots,XArg,Meta),
   i_neg(Neg,Det).

reshape_pred(transparent,S,N,P,A,pred(S,N,P,A)).
reshape_pred(have,Subj,Neg,Verb0,
      [quantV(Det,X,Head0,Pred,QArgs,Y)|MRest],
      pred(Subj,Neg,Verb,[quantV(Det,X,Head,Pred,QArgs,Y)|MRest])) :-
   have_pred(Head0,Verb0,Head,Verb).

have_pred('`' Head,Verb,'`' true,(Head,Verb)).
have_pred(Head,Verb,Head,Verb) :-
   meta_head(Head).

meta_head(apply80(_,_)).
meta_head(aggr(_,_,_,_,_)).

i_neg(posP,identityQ).
i_neg(negP,not).

i_subj(Voice,Subj,Slots0,Slots,Quant,Up,Id) :-
   (subj_case_opt(Voice,Case)*->true;true),
   verb_slot(arg(Case,Subj),[],[],Slots0,Slots,[Quant],[],Up,Id).

i_verb_args(VArgs,XA0,XA,Slots0,Slots,Args0,Args,Up,Id) :-
   fill_verb(VArgs,XA0,XA,Slots0,Slots,Args0,Args,Up,Id).

subj_case_opt(active,subj).
subj_case_opt(passive,s_subj).

fill_verb([],XA,XA,Slots,Slots,Args,Args,[],_).
fill_verb([Node|Nodes0],XA0,XA,Slots0,Slots,Args0,Args,Up,Id) :-
   verb_slot(Node,XA0,XA1,Slots0,Slots1,Args0,Args1,Up0,-Id),
   append(Up0,Nodes0,Nodes),
   fill_verb(Nodes,XA1,XA,Slots1,Slots,Args1,Args,Up,+Id).

verb_slot(prep_phrase(Prep,NP),
      XArg0,XArg,Slots0,Slots,[Q|Args],Args,Up,Id) :-
   i_np(NP,X,Q,Up,Id,unit,XArg0,XArg),
   in_slot(Slots0,Case,X,Id,Slots,_),
   deepen_case(Prep,Case).
verb_slot(voidQ,XA,XA,Slots,Slots,Args,Args,[],_) :-
   in_slot(Slots,pred,_,_,_,_).
verb_slot(prep_phrase(prep(Prep),NP),
      TXArg,TXArg,Slots0,Slots,[Q& '`' P|Args],Args,Up,Id0) :-
   in_slot(Slots0,pred,X,Id0,Slots1,_),
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
verb_slot(adverb(Adv),XA,XA,Slots0,Slots,['`' P|Args],Args,[],Id) :-
   adv_template(Adv,Case,X,P),
   in_slot(Slots0,Case,X,Id,Slots,_).
verb_slot(arg(pred,AP),XA,XA,Slots0,Slots,Args0,Args,Up,Id) :-
   in_slot(Slots0,pred,X,Id,Slots,_),
   i_pred(AP,X,Args0,Args,Up,Id).

i_pred(conj(Conj,Left,Right),X,
      [conj(Conj,'`' true,LQMods,'`' true,RQMods)|QMods],
      QMods,Up,Id) :-
   i_pred(Left,X,LQMods,[],[],-Id),
   i_pred(Right,X,RQMods,[],Up,+Id).
i_pred(AP,T,['`' Head&Pred|As],As,[],_) :-
   i_adj(AP,T,_,_,Head,true,Pred,'`' true).
i_pred(value80(adj(Adj),wh(TypeY-Y)),Type-X,['`' H|As],As,[],_) :-
   attribute_LF(Adj,Type,X,TypeY,Y,H).
i_pred(comp(Op0,adj(Adj),NP),X,[P1 & P2 & '`' P3,Q|As],As,Up,Id) :-
   i_np(NP,Y,Q,Up,Id,unit,[],[]),
   chat_sign(Adj,Sign),
   i_measure(X,Adj,Type,U,P1),
   i_measure(Y,Adj,Type,V,P2),
   op_inverse(Op0,Sign,Op),
   measure_op(Op,U,V,P3).
i_pred(prep_phrase(prep(Prep),NP),X,['`' H,Q|As],As,Up,Id) :-
   i_np(NP,Y,Q,Up,Id,unit,[],[]),
   adjunction_LF(Prep,X,Y,H).

i_adjoin(with,TS-S,TV-Y,[slot(prep(of),TV,Z,_,free)],
        held_arg(poss,-_Id,TS-S),
        Y=Z).
i_adjoin(Prep,X,Y,[],[],P) :-
   adjunction_LF(Prep,X,Y,P).

i_measure(Type-X,Adj,Type,X,'`' true) :-
   units(Adj,Type).
i_measure(TypeX-X,Adj,TypeY,Y,quantV(voidQ,TypeY-Y,'`' P,'`' true,[],_)) :-
   attribute_LF(Adj,TypeX,X,TypeY,Y,P).

i_verb_mods(Mods,_,XA,Slots0,Args0,Up,Id) :-
   fill_verb(Mods,XA,[],Slots0,Slots,Args0,Args,Up,-Id),
   i_voids(Slots,Args,+Id).

nominal_slot(slot(Kind,Type,X,Id,_),Type-X,Id) :-
   nominal_kind(Kind).

nominal_kind(prep(_)).
nominal_kind(poss).
nominal_kind(subj).
nominal_kind(dir).
nominal_kind(ind).

i_sup_op(least,min).
i_sup_op(most, max).

conversion(wh(Type-X),same,Type,X,identityQ).
conversion(N,Op,_,N,Op):- number(N).

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

noun_template(Noun,TypeV,V,'`' P,
      [slot(poss,TypeO,O,Os,index)|Slots]) :-
   property_LF(Noun,TypeV,V,TypeO,O,P,Slots,Os,_).
noun_template(Noun,TypeV,V,aggr(F,V,[],'`' true,'`' true),
   [slot(prep(of),TypeS,_,_,free)]) :-
   aggr_noun(Noun,TypeV,TypeS,F).
noun_template(Noun,Type,X,'`' P,Slots) :-
   thing_LF_access(Noun,Type,X,P,Slots,_).
noun_template(Noun,TypeV,V,apply80(F,P),
      [slot(prep(Of),TypeX,X,_,apply80)]) :-
   meta_noun_LF(Noun,Of,TypeV,V,TypeX,X,P,F).

verb_template(have,Y=Z,
                [slot(subj,TypeS,S,-Id,free),
                 slot(dir,TypeV,Y,_,free),
                 slot(prep(of),TypeV,Z,_,free)],
                held_arg(poss,-(-(+Id)),TypeS-S), have).
verb_template(have,Y=Z,
        [slot(subj,TypeS,S,-(-(Id)),free),
         slot(dir,TypeV,Y,_,free),
         slot(prep(as),TypeV,Z,_,free)],
        held_arg(poss,-(-(-(+Id))),TypeS-S), have).
verb_template(Verb,Pred,
      [slot(subj,TypeS,S,_,free)|Slots],[],transparent) :-
   verb_type_lex(Verb,_+Kind),
   verb_kind(Kind,Verb,TypeS,S,Pred,Slots).

verb_kind(be,_,TypeS,S,S=A,[slot(dir,TypeS,A,_,free)]).
verb_kind(be,_,TypeS,S,true,[slot(pred,TypeS,S,_,free)]).
verb_kind(iv,Verb,TypeS,S,Pred,Slots) :-
   intrans_LF(Verb,TypeS,S,Pred,Slots,_).
verb_kind(tv,Verb,TypeS,S,Pred,
      [slot(dir,TypeD,D,SlotD,free)|Slots]) :-
   trans_LF(Verb,TypeS,S,TypeD,D,Pred,Slots,SlotD,_).
verb_kind(dv(Prep),Verb,TypeS,S,Pred,
      [slot(dir,TypeD,D,SlotD,free),
       slot(ind,TypeI,I,SlotI,free)|Slots]) :-
   fail,fail,fail,fail,ditrans_lex80(Verb,Prep,TypeS,S,TypeD,D,TypeI,I,Pred,Slots,SlotD,SlotI,_).

deepen_case(prep(at),time).
deepen_case(s_subj,dir).
deepen_case(s_subj,ind).
deepen_case(prep(by),subj).
deepen_case(prep(to),ind).
deepen_case(prep(of),poss).
deepen_case(X,X).

% ================================================================
% Determiner Indexing Table

index_slot(index,I,I).
index_slot(free,_,unit).
index_slot(apply80,_,apply80).
index_slot(comparator_LF,_,comparator_LF).

index_args(det(the(pl)),unit,I,set(I),index(I)) :- !.
index_args(int_det(X),index(I),_,int_det(I,X),unit) :- !.
index_args(generic,apply80,_,lambda,unit) :-!.
index_args(D,comparator_LF,_,identityQ,unit) :-
 ( indexable_arg(D); D=generic), !.
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
