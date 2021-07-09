% ===================================================================
% File 'parser_e2c.pl'
% Purpose: Attempto Controlled English to CycL conversions from SWI-Prolog  
% This implementation is an incomplete proxy for CycNL and likely will not work as well
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'parser_e2c.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2002/06/06 15:43:15 $
% ===================================================================

:- shared_parser_data(in_continent/2).

:- use_module(library(clpr),[]).
:- multifile(baseKB:expect_file_mpreds/1).
:- dynamic(baseKB:expect_file_mpreds/1).
% WHY DID I HAVE? :- prolog_load_context(file, File),(baseKB:expect_file_mpreds(File)->true;asserta(baseKB:expect_file_mpreds(File))).
:- install_constant_renamer_until_eof.
:- set_prolog_flag(do_renames_sumo,never).

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
:-discontiguous(verb_type_db_0/2).
:-discontiguous(verb_root_db/1).
:-discontiguous(verb_form_db/4).
:-discontiguous(trans_LF/9).
:-kb_shared(ditrans_LF/13).
:-discontiguous(regular_pres_db/1).
:-discontiguous(regular_past_db/2).
:-discontiguous(noun_form_db/3).
:-discontiguous(noun_form_db_0/3).
:-discontiguous(loc_pred_prep_db/3).

% :- ensure_loaded(library(dra/tabling3/swi_toplevel)).


:- kb_shared((contains0/2,country/8,city/3,borders/2,in_continent/2)).
%:- kb_shared contains/2.

% :- kb_shared fact_always_true/1.

:- style_check(+discontiguous).
:- style_check(-discontiguous).
:- op(600,xfy,(('--'))).
:- op(450,xfy,((:))).
:- op(400,xfy,((&))).
:- op(300,fx,(('`'))).
:- op(200,xfx,(('--'))).

:-shared_parser_data((trans_LF/9)).

:-retractall(nldata_BRN_WSJ_LEXICON:text_bpos(the,nn)).

% :- begin_dynamic_reader.
:- asserta((t_l:enable_src_loop_checking)).

verb_type_to_kind(Var,Var2):-(var(Var);nonvar(Var2)),!,trace_or_throw(var_verb_type_to_kind(Var,Var2)).
verb_type_to_kind(Aux+Have,tv):-Aux == aux, Have == have.
verb_type_to_kind(_+Type,Kind):-must(nonvar(Type)),!,verb_type_to_kind(Type,Kind),!,must(not(Kind=have)).
verb_type_to_kind(Kind,Kind):-must(not(Kind=have)).


verb_type_db(Verb,Type):- no_repeats((one_must(verb_type_db_0(Verb,Type),verb_type_db_1(Verb,Type)))).

verb_type_db_1( Look,main+iv):- iv_finsg(_Looks, Look),!.
verb_type_db_1(Verb,main+ditrans(_)):-!, plt2, talk_db(iv,Verb,_,_,_,_). % ditrans(_)
verb_type_db_1(_Verb,main+tv):-!.
verb_type_db_1(_Verb,main+iv).

txt_there_db(there,there).
txt_not_db(not,not).
% txt_not_db(never,never).



:- must( \+ is_pfc_file).

txt_no_db(not,not).
txt_no_db(no,no).

:- listing(txt_no_db/2).


% =================================================================
% General Dictionary
% LEXICAL Data from newdic.pl

terminator_db(.,_).
terminator_db(?,?).
terminator_db(!,!).

plt:- t_l:usePlTalk,!.
plt2:- t_l:useAltPOS,!.
% plt:- t_l:chat80_interactive,!.

loop_check_chat80(Call):-loop_check_chat80(Call,fail).
loop_check_chat80(Call,Else):-loop_check(Call,Else).

not_violate(NotCCW,POS):-loop_check(not_violate0(NotCCW,POS)).
not_violate0(NotCCW,POS):-loop_check_chat80((ccw_db(NotCCW,CC)->CC=POS;true),fail).

:-meta_predicate(plt_call(+,+,0)).
:-meta_predicate(plt2_call(+,+,0)).
% plt_call(Goal):-plt,!,no_repeats(Goal),dmsg(succeed_plt_call(Goal)).
plt_call(NotCCW,_POS,_Goal):-member(NotCCW,['?','river','borders']),!,fail.
plt_call(NotCCW,POS,Goal):-plt,!,loop_check_chat80(no_repeats(Goal)),not_violate(NotCCW,POS),must(once(not_violate(NotCCW,POS);(dmsg(succeed_plt_call(NotCCW,POS,Goal)),!,fail))).
plt2_call(NotCCW,_POS,_Goal):-member(NotCCW,['?','river','borders']),!,fail.
plt2_call(NotCCW,POS,Goal):-plt2,!,loop_check_chat80(no_repeats(Goal)),not_violate(NotCCW,POS),must(once(not_violate(NotCCW,POS);(dmsg(succeed_plt_call(NotCCW,POS,Goal)),!,fail))).

adverb_db(Quickly):-plt_call(Quickly,'Adverb',(talk_db(adv,Quickly))).

conj_db(and).
conj_db(or).


conj_db(But):-cyckb_t(partOfSpeech,_,'CoordinatingConjunction',But).

int_pron_db(what,undef).
int_pron_db(which,undef).
int_pron_db(who,subj).
int_pron_db(whom,compl).

int_art_db(how,X,_,int_det(X)).
int_art_db(what,X,_,int_det(X)).
int_art_db(which,X,_,int_det(X)).

det_db(the,No,the(No),def).
det_db(a,sg,a,indef).
det_db(an,sg,a,indef).
det_db(every,sg,every,indef).
det_db(some,_,some,indef).
det_db(any,_,any,indef).
det_db(all,pl,all,indef).
det_db(each,sg,each,indef).
det_db(no,_,no,indef).

det_db(Det):-det_db(Det,_,_,_).
det_db(W):-det_db0(W), \+ det_db(W,_,_,_), dif(CCW,'Determiner'), \+ ccw_db(W,CCW).
det_db0(W):- (cycQuery80('determinerStrings'(_,W));cyckb_t('determinerStrings',_,W)),atom(W).

:- listing(det_db/1).

number_db(W,I,Nb) :-
   tr_number(W,I),
   ag_number(I,Nb).

tr_number(nb(I),I).
tr_number(one,1).
tr_number(two,2).
tr_number(three,3).
tr_number(four,4).
tr_number(five,5).
tr_number(six,6).
tr_number(seven,7).
tr_number(eight,8).
tr_number(nine,9).
tr_number(ten,10).

ag_number(1,sg).
ag_number(N,pl) :- N>1.

quantifier_pron_db(everybody,every,person).
quantifier_pron_db(everyone,every,person).
quantifier_pron_db(everything,every,thing).
quantifier_pron_db(somebody,some,person).
quantifier_pron_db(someone,some,person).
quantifier_pron_db(something,some,thing).
quantifier_pron_db(anybody,any,person).
quantifier_pron_db(anyone,any,person).
quantifier_pron_db(anything,any,thing).
quantifier_pron_db(nobody,no,person).
quantifier_pron_db(nothing,no,thing).

prep_db(as).
prep_db(at).
noun_plu_db(times,time).
noun_plu_db(Liaisons, Liaison):- plt, lexicon_interface:noun_pl(Liaisons, Liaison, _Human).
adverb_db(yesterday).
adverb_db(tomorrow).

prep_db(of).
prep_db(to).
prep_db(by).
prep_db(with).
prep_db(in).
prep_db(on).
prep_db(from).
prep_db(into).
prep_db(through).
prep_db(Above):-plt_call(Above,'Preposition',talk_db(preposition,Above)).


noun_form_db(Plu,Sin,PS):- noun_form_db_0(Plu,Sin,PS),must(Sin\==the).

noun_form_db_0(Plu,Sin,pl) :- noun_plu_db(Plu,Sin),not_ccw(Plu).
noun_form_db_0(Sin,Sin,sg) :- noun_sin_db(Sin),not_ccw(Sin).
noun_form_db_0(Sin,Sin,sg):-text_bpos(Sin,'nn'),not_ccw(Sin).
noun_form_db_0(Sin,Sin,sg):-text_bpos(N,Sin,'nn'),N>=0.5 ,not_ccw(Sin).

verb_form_db(V,V,inf,_) :- verb_root_db(V).
verb_form_db(V,V,pres+fin,Agmt) :-
   regular_pres_db(V),
   root_form_db(Agmt),
   verb_root_db(V).
verb_form_db(Past,Verb,past+_,_) :-
   regular_past_db(Past,Verb).

root_form_db(1+sg).
root_form_db(2+_).
root_form_db(1+pl).
root_form_db(3+pl).

verb_root_db(BE):-aux_verb_root_db(BE).


aux_verb_root_db(be).
aux_verb_root_db(have).
aux_verb_root_db(do).
   
verb_form_db(am,be,pres+fin,1+sg).
verb_form_db(are,be,pres+fin,2+sg).
verb_form_db((is),be,pres+fin,3+sg).
verb_form_db(are,be,pres+fin,_+pl).
verb_form_db(was,be,past+fin,1+sg).
verb_form_db(were,be,past+fin,2+sg).
verb_form_db(was,be,past+fin,3+sg).
verb_form_db(were,be,past+fin,_+pl).
verb_form_db(been,be,past+part,_).
verb_form_db(being,be,pres+part,_).



memoize_pos_to_db(WHY,_CYCPOS,W1,W1):- t_l:old_text,!, WHY. 
memoize_pos_to_db(WHY,CYCPOS,W2,W1):- use_open_marker,!, memoize_pos_to_db_old(WHY,CYCPOS,W2,W1).
memoize_pos_to_db(WHY,_CYCPOS,W2,W1):- W2= w(W1,_),!,WHY,must(nb_setarg(2,W2,set(WHY))).
memoize_pos_to_db(WHY,_CYCPOS,W2,W1):- W2= w(W1,LIST),member(penn(WHY2),LIST),!,WHY2=WHY.
memoize_pos_to_db(WHY,CYCPOS,W2,W1):- W2= w(W1,LIST),
  once(member(penn(DATA),LIST);DATA=open),
  ( DATA==open -> ((pos_to_db_precache(W1,GOODL),dmsg(W1=GOODL),nb_setarg(2,W2,GOODL))) ; DATA=GOODL ),!,
 (GOODL==[] -> WHY ; member(WHY-CYCPOS,GOODL)),must((nonvar(W1),nonvar(W2))).


memoize_pos_to_db_old(WHY,_CYCPOS,W2,W1):- W2= w(W1,open),!,WHY,must(nb_setarg(2,W2,set(WHY))).
memoize_pos_to_db_old(WHY,_CYCPOS,W2,W1):- W2= w(W1,set(WHY2)),!,WHY2=WHY.
memoize_pos_to_db_old(WHY,CYCPOS,W2,W1):-  W2= w(W1,DATA),
  ( DATA==open -> ((pos_to_db_precache(W1,GOODL),dmsg(W1=GOODL),nb_setarg(2,W2,GOODL))) ; DATA=GOODL ),!,
 (GOODL==[] -> WHY ; member(WHY-CYCPOS,GOODL)),must((nonvar(W1),nonvar(W2))).

pos_to_db_precache(W,GOODL):-   
 findall(WHY-CYCPOS,
  (member(theTextC(W,CYCPOS,WHY),
    [    theTextC(W,'Pronoun',context_pron_db(_In,_Place,W)),
         theTextC(W,'Pronoun',rel_pron_db(W,Case)),
         theTextC(W,'Pronoun',quantifier_pron_db(W,Det,Noun)),
         theTextC(W,'Pronoun',poss_pron_db(W,Gender,Person,Number)),
         theTextC(W,'Pronoun',pers_pron_db(W,Gender,Person,Number,Case)),
         theTextC(W,'Preposition',loc_pred_prep_db(W,_P,_OFPREP)),
         theTextC(W,'Number-SP',number_db(W,_I,_Number)),
         theTextC(W,'Determiner',det_db(W,Number,Det,_Def)),
         theTextC(W,'Symbol-SP',terminator_db(W,Type)),
         theTextC(W,'Symbol-SP',W=','),
         theTextC(W,'Pronoun',int_pron_db(W,Case)),
         theTextC(Prep,'Preposition',prep_db(Prep)),
         theTextC(Name,'ProperNoun',name_db([Name],_)),
         theTextC(Conj,'Conjunction',conj_db(Conj)),      
         theTextC(Art,'Determiner', int_art_db(Art,_X,Agmt,_DX)),
         theTextC(W,'Noun',noun_form_db_0(W,Noun,Agmt)),
         theTextC(W,'Adjective',rel_adj_db(W,NW)),
         theTextC(W,'Adjective',sup_adj_db(W,NW)),
         theTextC(W,'Adjective',adj_db(W,Type)),
         theTextC(W,'Adverb',comp_adv_db(W)),
         theTextC(W,'Adverb',sup_adv_db(W)),
         theTextC(W,'Adverb',adverb_db(W)),
         theTextC(W,VerbPOS,v_db(W,_Verb,_Tense,Agmt,VerbPOS))]),
            no_repeats(WHY)),GOODL),
            ignore((GOODL==[],dmsg(W=[]))),!.

posVerbOrAuxVerb(being,be,pres+part,_BEING).
posVerbOrAuxVerb(_,be,_,'BeAux').
posVerbOrAuxVerb(_,have,_,'HaveAux').
posVerbOrAuxVerb(_,can,_,'Verb'). % _TINCAN
posVerbOrAuxVerb(_,do,_,'DoAux').
posVerbOrAuxVerb(_,_,_+part,'Verb').
posVerbOrAuxVerb(_,_,_,'Verb').

v_db(W,Verb,Tense,Agmt,POSVerbOrAuxVerb):-no_repeats(v_db0(W,Verb,Tense,Agmt,POSVerbOrAuxVerb)).

v_db0(W,Verb,Tense,Agmt,POSVerbOrAuxVerb):- verb_form_db(W,Verb,Tense,Agmt),once(posVerbOrAuxVerb(W,Verb,Tense,AuxVerb)),domain(POSVerbOrAuxVerb,['Verb',AuxVerb]).
% v_db(W,Verb,Tense,Agmt,'Verb'):-!,verb_form_db(W,Verb,Tense,Agmt).
v_db0(W,Verb,pres+_,_,'Modal'):- plt_call(W,'Modal',((el_holds(partOfSpeech,Should_TheWord,'Modal',W,_,_),el_holds('presentTense-Universal',Should_TheWord,Verb,_,_)))).

verb_type_db_0(be,aux+be).

regular_pres_db(have).

regular_past_db(had,have).

verb_form_db(have,have,pres+fin,3+sg).
verb_form_db(having,have,pres+part,_).

verb_type_db_0(have,aux+have).

regular_pres_db(do).

verb_form_db(does,do,pres+fin,3+sg).
verb_form_db(did,do,past+fin,_).
verb_form_db(doing,do,pres+part,_).
verb_form_db(done,do,past+part,_).

verb_type_db_0(do,aux+ditrans(_)).


modal(can,could,able).
%may,might
%must,should,Ought to
% obligated
%Have to 
% have got to 
modal(will,would,shall).
 

:-style_check(-singleton).
regular_pres_db(Can):-modal(Can,Could,Able).
verb_form_db(Could,Can,pres+fin,3+sg):-modal(Can,Could,Able).
verb_form_db(Could,Can,past+fin,_):-modal(Can,Could,Able).
verb_form_db(Can,Can,pres+part,_):-modal(Can,Could,Able).
verb_form_db(Able,Can,past+part,_):-modal(Can,Could,Able).
% verb_type_db_0(Can,aux+ditrans(_)):-modal(Can,Could,Able).
:-style_check(+singleton).
% =================================================================
% PRONOUN DB
% =================================================================
pron_db(W):-rel_pron_db(W,_).
pron_db(W):-poss_pron_db(W,_,_,_).
pron_db(W):-pers_pron_db(W,_,_,_,_).
pron_db(W):-int_pron_db(W,_).
pron_db(W):-quantifier_pron_db(W,_,_).

rel_pron_db(who,subj).
rel_pron_db(whom,compl).
rel_pron_db(which,undef).

poss_pron_db(my,agent,1,sg).
poss_pron_db(your,agent,2,_). % might be a group
poss_pron_db(his,masc,3,sg).
poss_pron_db(her,fem,3,sg).
poss_pron_db(its,neut,3,sg).
poss_pron_db(our,agentgroup,1,pl).
poss_pron_db(their,agentgroup,3,pl).

pers_pron_db(i,agent,1,sg,subj).
pers_pron_db(you,agent,2,_,_). % might be a group
pers_pron_db(he,masc,3,sg,subj).
pers_pron_db(she,fem,3,sg,subj).
pers_pron_db(it,neut,3,sg,_).
pers_pron_db(we,agentgroup,1,pl,subj).
% dmiles added
pers_pron_db(they,agent,3,pl,subj).
% dmiles removed
% pers_pron_db(them,agentgroup,3,pl,subj).
pers_pron_db(me,agent,1,sg,compl(_)).
pers_pron_db(him,masc,3,sg,compl(_)).
pers_pron_db(her,fem,3,sg,compl(_)).
pers_pron_db(us,agentgroup,1,pl,compl(_)).
pers_pron_db(them,agentgroup,3,pl,compl(_)).


how_many_db([how,many]).

pronoun_LF(_Argree2B,np_head(generic,ADJS,Type),_MoreIn,_X,_Y,_ADJLIST,_OUT):-contains_var(typeOf,some(Type,ADJS)),!,fail.
pronoun_LF(Argree2B,pronoun(FemMasc),MoreIn,X,_Y,[adj(lf(X,nlAgreement(X,Argree2B)&dbase_t(typeOf,X,FemMasc)))|MoreIn],FemMasc):-nonvar(Argree2B),nonvar(FemMasc), t_l:useAltPOS,!.
pronoun_LF(Argree2B,FemMasc,MoreIn,X,_Y,[adj(lf(X,nlAgreement(X,Argree2B)&dbase_t(typeOf,X,FemMasc)))|MoreIn],FemMasc):-nonvar(Argree2B),nonvar(FemMasc),t_l:useAltPOS.

kyqdhq(K,Kernel,Y,Q,Det,T,Head,Pred0,QMods):-  K= np(_,Kernel,_),Y=Y, Q = quant(Det,T,Head,Pred0,QMods,Y),nop(dmsg(good_kyqdhq(K,Kernel,Y,Q,Det,T,Head,Pred0,QMods))),!.
kyqdhq(K,Kernel,Y,Q,Det,T,Head,Pred0,QMods):-  t_l:useAltPOS,t_l:chat80_interactive,dmsg(unused_kyqdhq(K,Kernel,Y,Q,Det,T,Head,Pred0,QMods)),fail.


np_head_rewrite(np(Argree2B,C,MoreIn),Y,Quant,X,Det,np(Argree2B,np_head(generic,MoreOut,PronounTypePath),MoreIn),Y,Quant,Det):- not(contains_var(typeOf,C)),
   t_l:chat80_interactive,!,show_call(pronoun_LF(Argree2B,C,MoreIn,X,Y,MoreOut,PronounTypePath)).
%np_head_rewrite(np(Argree2B,pronoun(B),[]),Y,Quant,X,Det,np(Argree2B,np_head(generic,[],dbase_t(typeOf,X,Y,B,Argree2B),[]),[]),Y,Quant,Det):- !.
          
:-dynamic(subject_LF/5).
:-dynamic(subject_slots_LF/7).
:-dynamic(subj_obj_LF/7).
:-dynamic(subj_obj_slots_LF/10).


clause_head_arg1_wrong(Call):-arg(1,Call,LFType),arg(2,Call,Word),clause_head_arg1_wrong(Call,Word,LFType).

clause_head_arg1_wrong(_,Word,LFType):- Word==river,!,LFType \== thing.
clause_head_arg1_wrong(_,Word,LFType):- Word==country,!,LFType \== thing.
clause_head_arg1_wrong(_,Word,_LFType):- Word==percentage,!.
clause_head_arg1_wrong(_Call,_Word,_LFType):-!,fail.
/*
clause_head_arg1_wrong(Call,Word,LFType):- must(nonvar(LFType)),
 functor(Call,F,A),functor(Post,F,A),
 arg(1,Post,Other),arg(2,Post,Word),
 dif(LFType,Other),
 clause(Post,PreMaybe),
 call_if_required(PreMaybe),
 nonvar(Other),
 dmsg((error_wrong_clause(Word->Other,not(LFType)):-PreMaybe)),!.
*/

call_if_required(_PreMaybe).



deduce_subject_LF(LFKind,Word,SType,SVar,LFProlog):-subject_slots_LF(LFKind,Word,SType,SVar,LFProlog,[],SlotS),
   var(SlotS),not(contains_var(SlotS,this(LFKind,Word,SType,SVar,LFProlog))),not_wrong_subj_lf(LFKind,Word).
deduce_subject_LF(LFKind,Word,SType,SVar,LFProlog):-subject_LF(LFKind,Word,SType,SVar,LFProlog),not_wrong_subj_lf(LFKind,Word).

deduce_subject_slots_LF(LFKind,Word,SType,SVar,LFProlog,Slots,SlotS):-subject_slots_LF(LFKind,Word,SType,SVar,LFProlog,Slots,SlotS),not_wrong_subj_lf(LFKind,Word).
deduce_subject_slots_LF(LFKind,Word,SType,SVar,LFProlog,[],_):-subject_LF(LFKind,Word,SType,SVar,LFProlog),not_wrong_subj_lf(LFKind,Word).

not_wrong_subj_lf(A,B):-not(wrong_subj_lf(A,B)).

wrong_subj_lf(LFKind,Word):-clause_head_arg1_wrong(subject_slots_LF(LFKind,Word,_,_,_,_,_)),!.
wrong_subj_lf(LFKind,Word):-clause_head_arg1_wrong(subject_LF(LFKind,Word,_,_,_)),!.
wrong_subj_lf(_OK_LFKind,_OK_Word):-!,fail.

deduce_subj_obj_LF(LFKind,Word,SType,SVar,DType,DVar,LFProlog):-subj_obj_LF(LFKind,Word,SType,SVar,DType,DVar,LFProlog),not_wrong_subj_obj_lf(LFKind,Word).
deduce_subj_obj_LF(LFKind,Word,SType,SVar,DType,DVar,LFProlog):-
  subj_obj_slots_LF(LFKind,Word,SType,SVar,DType,DVar,LFProlog,EmptyList,SlotS,SlotD), [] == EmptyList,
  once((var(SlotS),var(SlotD),
  THIS=this(LFKind,Word,SType,SVar,DType,DVar,LFProlog),
  not(contains_var(SlotS,THIS)),
  not(contains_var(SlotD,THIS)))),
  not_wrong_subj_obj_lf(LFKind,Word).

deduce_subj_obj_slots_LF(LFKind,Word,SType,SVar,DType,DVar,LFProlog,Slots,SlotD,SlotS):-subj_obj_slots_LF(LFKind,Word,SType,SVar,DType,DVar,LFProlog,Slots,SlotD,SlotS),not_wrong_subj_obj_lf(LFKind,Word).
deduce_subj_obj_slots_LF(LFKind,Word,SType,SVar,DType,DVar,LFProlog,[],_,_):-subj_obj_LF(LFKind,Word,SType,SVar,DType,DVar,LFProlog),not_wrong_subj_obj_lf(LFKind,Word).


not_wrong_subj_obj_lf(A,B):-not(wrong_subj_obj_lf(A,B)).

wrong_subj_obj_lf(LFKind,Word):- clause_head_arg1_wrong(subj_obj_slots_LF(LFKind,Word,_,_,_,_,_,_,_,_)),!.
wrong_subj_obj_lf(LFKind,Word):- clause_head_arg1_wrong(subj_obj_LF(LFKind,Word,_,_,_,_,_)),!.
wrong_subj_obj_lf(_OK_LFKind,_OK_Word):-!,fail.

% =================================================================
% PROPER INSTANCES OF
% =================================================================

noun_plu_db(places,place).

noun_plu_db(P,S):-plt_call(P,'Noun',plt_call(S,'Noun',(talk_db(noun1,S,P),not_ccw(P),not_ccw(S)))).
noun_plu_db(continents,continent).
noun_plu_db(oceans,ocean).
noun_plu_db(regions,region).
noun_plu_db(rivers,river).
noun_plu_db(seas,sea).
noun_plu_db(seamasses,seamass).

% should inhereit from e2c

% ==========================================================
% meetsForm(String,CycWord,Form)
% ==========================================================
meetsForm80(String,RootString,form80(MainPlusTrans,main+tv)):-!,fail,nop((String,RootString,form80(MainPlusTrans,main+tv))).


noun_plu_db(PluralString,SingularString):- meetsForm80(PluralString,SingularString,noun+plural).
noun_sin_db(Singular):- meetsForm80(Singular,Singular,noun+singular).
noun_sin_db(Singular):-noun_plu_db(_,Singular).

subject_LF(thing,continent,feature&place&continent,X,continent(X)).
subject_LF(thing,ocean,feature&place&seamass,X,ocean(X)).
subject_LF(thing,river,feature&river,X,river(X)).
subject_LF(thing,sea,feature&place&seamass,X,sea(X)).
subject_LF(thing,seamass,feature&place&seamass,X,seamass(X)).
subject_LF(thing,region,feature&place&_,X,region80(X)).


/* WHICH WHICH DENOTES A  */

subject_LF(thing,place,feature&place&_,X,place(X)).

/* WHICH EXISTENCE STEMS FROM A  */

place(X) :- continent(X); region80(X); seamass(X); country(X).

region80(R) :- in_continent(R,_).


continent(africa).
continent(america).
continent(antarctica).
continent(asia).
continent(australasia).
continent(europe).

circle_of_latitude(equator).
circle_of_latitude(tropic_of_cancer).
circle_of_latitude(tropic_of_capricorn).
circle_of_latitude(arctic_circle).
circle_of_latitude(antarctic_circle).


seamass(X) :- ocean(X).
seamass(X) :- sea(X).

ocean(arctic_ocean).
ocean(atlantic).
ocean(indian_ocean).
ocean(pacific).
ocean(southern_ocean).

sea(baltic).
sea(black_sea).
sea(caspian).
sea(mediterranean).
sea(persian_gulf).
sea(red_sea).

river(R) :- river_pathlist(R,_L).



==>in_continent(north_america, america).

% ------------------------------
% "Whoable Count Nouns" 
% ------------------------------
noun_plu_db(persons,person).  noun_plu_db(people,person).
noun_plu_db(men, man). noun_plu_db(wommen, woman).
subject_LF(thing,person,_,X,person(X)).




/* A CAPITOL (IS NOT _JUST_ A PLACE)  */


noun_plu_db(capitals,capital).
subject_LF(thing,capital,feature&city,X,isa_capital(X)).
subj_obj_LF(property,capital,feature&city,X,feature&place&country,Y, capital(Y,X)).
capital(C,Cap) :- country(C,_,_,_,_,_,Cap,_).

isa_capital(C) :- capital(_X,C).

/* IS A SPECILIZATION OF A CITY */

noun_plu_db(cities,city).
subject_LF(thing,city,feature&city,X,city(X)).
city(C) :- city(C,_,_).
city(tehran,iran,1010).

/* THAT INVOKES ACTION */

subj_obj_LF(trans,govern,feature&_,X,feature&place&country,Y,capital(Y,X)).
verb_root_db(govern).
regular_pres_db(govern).
regular_past_db(governed,govern).
verb_form_db(governs,govern,pres+fin,3+sg).
verb_form_db(governing,govern,pres+part,_).
verb_type_db_0(govern,main+tv).

/* UPON A COUNTRY */

noun_plu_db(countries,country).
subject_LF(thing,country,feature&place&country,X,country(X)).
country(C) :- country(C,_,_,_,_,_,_,_).
country(iran,middle_east,33,-53,636363,32001000,tehran,rial).


% =================================================================
%  A PROPERTY IS SPECIALIZION OF A MPRED THAT IS PRESENT ON TYPE
% =================================================================


noun_plu_db(types,type).
subject_LF(thing,tSet,feature&tSet&_,X,tSet(X)).
noun_plu_db(datatypes,datatype).
subject_LF(thing,datatype,feature&datatype&_,X,ttFormatType(X)).

subject_LF(thing,Type,feature&Type&_,X,isa(X,Type)):-plt,tSet(Type).

noun_plu_db(TS,T):- noun_plu_db_via_types(TS,T).

subject_LF(thing,Type,TYPEMASK,X,isa(X,Type)):- atom(Type), plt_call(Type,'Noun',((loop_check_chat80(tSet(Type)),atom(Type),gen_typemask(Type,TYPEMASK)))).
subject_LF(restriction,Type,TYPEMASK,X,isa(X,Type)):- atom(Type), plt_call(Type,'Noun',((loop_check_chat80(tSet(Type)),atom(Type),gen_typemask(Type,TYPEMASK)))).
subject_LF(restriction,Type,TYPEMASK,X,isa(X,Type)):- atom(Type), plt2, once(cw_db(Type,Adj)),Adj='Adjective',gen_typemask(Type,TYPEMASK).



gen_typemask(Type,measure&Type&_):-ttFormatType(Type),!.
gen_typemask(Type,feature&Type&_).
gen_typemask(_,feature&_).


noun_plu_db_via_types(TS,T):- maybe_noun_or_adj(T),maybe_noun_or_adj(TS), (atom(TS)->atom_concat(T,'s',TS);true),tSet(T),atom(T),atom_concat(T,'s',TS).
maybe_noun_or_adj(T):- var(T)->true;(atom(T),not_ccw(T)).

% 
% chat80("how many types are there?").
% chat80("what formattypes are types?").
% test_chat80( [which,is,the,largest,african,country,?]).
%  chat80("how are you?").
% test_chat80("you flow").

parser_chat80:t11:- 
   test_chat80("how many postures are there?"),
   test_chat80("what are the postures?"),
   test_chat80("what are the types?"),
   %test_chat80("how many oceans are seas?"),
   %test_chat80("how many oceans are seamasses?"),
   test_chat80("how many types are there?"),
   test_chat80("how many formattypes are there?"),
   test_chat80("what formattypes are there?"),
   !.

parser_chat80:t22 :-
   test_chat80("how many countries are in antarctica?"),
   test_chat80("how many countries are in australia?").

t33:- test_chat80('I do not know if the can can do the dance called the can can.').


subj_obj_indirect_slots_LF(ditrans,verb_prep(Verb,Prep),
   feature&X1,X,
   feature&Y1,Y,
   feature&Z1,Z,
   callDitrans(Verb,Prep,X1,Y1,Z1,X,Y,Z),[],_DC2,_DC1,_DC0):-!.


callDitrans(Verb,Prep,X1,Y1,Z1,verbPrep(subj:X1,Verb,Prep),verbPrep(o:Y1,Verb,Prep),verbPrep(io:Z1,Verb,Prep)).

/* A PROPERTY  */


noun_plu_db(properties,property).
subject_LF(thing,property,feature&mpred,X,objectProperty(X)).
subj_obj_LF(property,property,feature&mpred,X,feature&tSet&_,Y, hasPropertyOrValue(Y,X)).

/* IS A SPECILIZATION OF A MPRED */

noun_plu_db(mpreds,mpred).
subject_LF(thing,mpred,feature&mpred,X,mpred(X)).

objectProperty(P) :- hasProperty(_,P).

hasPropertyOrValue(T,PorV):- (PorV=P;PorV=V), hasPropertyValue(T,P,V),(PorV=P;PorV=V).

hasProperty(Type,P):-hasPropertyValue(Type,P,_).

hasPropertyValue(SomeType,P,SomeVType):-mpred_arity(P,A),A>=2,argIsa_call(P,1,SomeType),argIsa_call(P,A,SomeVType).
hasPropertyValue(Type,P,Area) :- deduce_subj_obj_LF(property,Area,_Measure&Area,_X,feature&TYPELIST,_Y,Pred),deepestType(TYPELIST,Type),get_1st_order_functor(Pred,P),deepestType(TYPELIST,Type).

get_1st_order_functor(Pred,P):-not(compound(Pred)),!,P=Pred.
get_1st_order_functor(Pred,P):-functor_h(Pred,F),(is_2nd_order_holds(F)->((arg(1,Pred,A),!,get_1st_order_functor(A,P)));P=F).

/* THAT IS HAD */
/*
subj_obj_LF(trans,have,feature&mpred,X,feature&tSet,Y,hasProperty(Y,X)).
verb_root_db(have).
regular_pres_db(have).
regular_past_db(had,have).
verb_form_db(has,have,pres+fin,3+sg).
verb_form_db(having,have,pres+part,_).
verb_type_db_0(have,main+tv).
*/
/* BY  SOME TYPE */


deepestType(TYPE,_):-var(TYPE),!,fail.
deepestType(TYPE&Next,Type):-var(Next),!,Type=TYPE.
deepestType(_&LIST,Type):-!,deepestType(LIST,Type).
deepestType(TYPE,Type):-Type=TYPE.

typeAssignableTo(Type,SomeType):-subclass_backchaing(Type,SomeType).
typeAssignableTo(_Type,SomeType):-ttFormatType(SomeType).


% TODO DECIDE IF UNEEDED hook:fact_always_true(isa(Type,type)):- clause(subject_LF(thing,Type,feature&_,_X,_),true).
% TODO DECIDE IF UNEEDED hook:fact_always_true(isa(Type,type)):- clause(subject_LF(restriction,Type,feature&_,_X,_),true).


type_allowed(feature&TYPEMASK,Type):-nonvar(TYPEMASK),!,type_allowed(TYPEMASK,Type),!.
type_allowed(TM,Type):-gen_typemask(Type,TM).

type_allowed0(NV&TypeM,Type):-nonvar(NV),!,type_allowed(TypeM,Type).
type_allowed0(TypeM,Type):-Type==TypeM,!.
type_allowed0(TypeM&_,Type):- Type==TypeM,!.



% =================================================================
% Having Referant Proper nouns
% =================================================================
:- shared_parser_data(name_template_db/2).
name_db([black,sea],black_sea).
name_db([upper,volta],upper_volta).
name_db([Name],Name) :-
   name_template_db(Name,_), !.
name_db([Name],Name) :- t_l:useAltPOS,downcase_atom(Name,DCName),loop_check(not(cw_db_code(DCName,_))).

name_template_db(X,feature&circle) :- circle_of_latitude(X).
name_template_db(X,feature&city) :- city(X).
name_template_db(X,feature&place&continent) :- continent(X).
name_template_db(X,feature&place&country) :- country(X).
name_template_db(X,feature&place&_) :- region80(X).
name_template_db(X,feature&river) :- river(X).
name_template_db(X,feature&place&seamass) :- seamass(X).

name_template_db(X,feature& ISA) :- plt2_call(X,'Noun',( nonvar(ISA), isa(X,ISA),not_ccw(X))).
name_template_db(X,feature& ISA & _) :- plt2_call(X,'Noun',( nonvar(ISA), isa(X,ISA),not_ccw(X))).
name_template_db(X,feature& _ & ISA ) :-plt2_call(X,'Noun',( nonvar(ISA), isa(X,ISA),not_ccw(X))).


% =================================================================
% FACETS (Adjectives) 
% =================================================================


subject_LF(restriction,happy,feature&_,X,isa(X,adjColFn(happy))).
subject_LF(restriction,african,feature&_,X,african(X)).
subject_LF(restriction,american,feature&_,X,american(X)).
subject_LF(restriction,asian,feature&_,X,asian(X)).
subject_LF(restriction,european,feature&_,X,european(X)).

african(X) :- in_ploc(X,africa).
american(X) :- in_ploc(X,america).
asian(X) :- in_ploc(X,asia).
european(X) :- in_ploc(X,europe).

adj_db(african,restr).
adj_db(american,restr).
adj_db(asian,restr).
adj_db(european,restr).

:-dynamic(not_ccw_db/1).
:-dynamic(is_ccw_db/1).
not_ccw(ok(W)):-must(nonvar(W)),!,not_ccw(W),!.
not_ccw(W):-not_ccw_db(W),!.
not_ccw(W):-is_ccw_db(W),!,fail.
not_ccw(W):-not(lmcache:ilc(_,'new_dict.pl':_)),!,show_call(not((ccw_db0(W,G),!,ground(G))) -> asserta(not_ccw_db(W)) ; ((asserta(is_ccw_db(W)),!,fail))),!.
not_ccw(W):-not((ccw_db0(W,G),!,ground(G))),!.
% closed class words
ccw_db(W,C):-loop_check_chat80(no_repeats(ccw_db0(W,C))).
ccw_db0(W,C):-one_must(ccw_db1(W,C),one_must(ccw_db2(W,C),ccw_db3(W,C))).
ccw_db1(W,'Number-SP'):-no_repeats(W,number_db(W,_,_)).
ccw_db1(W,'Symbol-SP'):-no_repeats(W,terminator_db(W,_)).
ccw_db1(W,'Preposition'):-prep_db(W),not(ccw_db1(W,'Determiner')).
ccw_db1(W,'Pronoun'):-pron_db(W).
ccw_db1(W,'Conjunction'):-conj_db(W).
ccw_db1(W,'Determiner'):-det_db(W).
ccw_db1(W,'Conjunction'):-no_repeats(W,partOfSpeech(_,'SubordinatingConjunction',W)).
ccw_db1(W,'Pronoun'):-no_repeats(W,cyckb_t('pronounStrings',_,W)).

ccw_db2(W,'Preposition'):-no_repeats(W,talk_db(preposition,W)).
ccw_db3(W,C):-ccw_db4(W,C),not(loop_check_chat80(adj_db(W,_))),not((ccw_db1(W,OTHER),OTHER\==C)).


obviously_verb(W,Pres+Part,Num+Plu,VPOS):-part==Part->true;(Pres==pres,Num==3,Plu==sg,VPOS\='Verb',not(loop_check_chat80(once(noun_plu_db(W,_)),fail))).

ccw_db4(W,VPOS):-nonvar(W),v_db(W,_,PP,NumPlu,VPOS),once((loop_check_chat80(obviously_verb(W,PP,NumPlu,VPOS)))),!.
ccw_db4(W,VPOS):-v_db(W,_,PP,NumPlu,VPOS),once(loop_check_chat80(obviously_verb(W,PP,NumPlu,VPOS))).

compatible_pos_db(_MostLikley,_Wanted).

cw_db(W,C):-deepen_pos(cw_db_code(W,C)).

cw_db_code(W,C):-ccw_db(W,C).
cw_db_code(W,C):-nonvar(W),!,cw_db0(W,WC),!,C=WC.
cw_db_code(W,C):-cw_db0(W,C).

cw_db0(W,C):-one_must(cw_db1(W,C),cw_db2(W,C)).
cw_db1(W,C):-ccw_db(W,C).
cw_db1(W,C):-ocw_db(W,C),dif(C,OC),not(ocw_db(W,OC)).
cw_db2(W,C):-one_must(ocw_db0(W,C),ocw_db1(W,C)).

ocw_db(W,'Verb'):-no_repeats(W,(verb_form_db(W,_,_,_),not((ccw_db(W,CCW),!,CCW\=W)))).
ocw_db(W,C):-ocw_db0(W,C).

ocw_db0(W,C):-one_must(ocw_db1(W,C),ocw_db3(W,C)).
ocw_db1(W,'Interjection'):-talk_db(interj,W).
ocw_db1(W,'Adverb'):-adverb_db(W),not(ocw_db2(W,_)).
ocw_db1(W,C):- ocw_db2(W,C),not_ccw(W).
ocw_db2(W,'Noun'):-noun_plu_db(W,_).
ocw_db2(W,'Adjective'):-adj_db(W,_).
ocw_db2(W,'Noun'):-noun_plu_db(_,W).
ocw_db3(W,SPOS):-plt_call(W,POS,with_assertions(t_l:useOnlyExternalDBs, (( 'suffixString'(CycWord,String),String\='',atom_concat(_First,String,W),'derivationalAffixResultPOS'(CycWord,POS),simplePOS(POS,SPOS))))).

simplePOS(POS,SIMP):-posName(SIMP),atom_concat(_,SIMP,POS).

subject_LF(restriction,lf(X,Formula),feature&_,X,Formula):-nonvar(Formula),!.
subject_LF(restriction,lf(Formula),feature&_,_X,Formula):-nonvar(Formula),!.
subject_LF(restriction,AdjNounEan,feature&_,X,adjIsa(X,AdjNounEan)):-atom(AdjNounEan), plt_call(AdjNounEan,'Adjective',( adj_db(AdjNounEan,restr),not_ccw(AdjNounEan) )).
adjIsa(E,C):- call_mpred(isa_backchaing(E,C)).
adj_db(AdjNounEan,restr):- plt_call(AdjNounEan,'Adjective',(talk_db(adj,AdjNounEan),talk_db(noun1,AdjNounEan,_))).
adj_db(AdjRestr,restr):-plt_call(AdjRestr,'Adjective',(talk_db(adj,AdjRestr),not(adj_db(AdjRestr,quant)),not(adverb_db(AdjRestr)))).
adj_db(AdjRestr,restr):-meetsForm80(AdjRestr,AdjRestr,form80(adj+restr)).


verb_type_db_0(Verb,main+tv)  :-plt_call(Verb,'Verb', deduce_subj_obj_LF(trans,Verb,_,_,_,_,_)).
verb_type_db_0(Verb,main+iv):-plt_call(Verb,'Verb',  verb_LF(iv,Verb,_,_,_,_,_)).
verb_type_db_0(Verb,main+ditrans(Prep)):- plt_call(Verb,'Verb',  subj_obj_indirect_slots_LF(trans,verb_prep(Verb,Prep),_,_,_,_,_,_, _, _,_,_,_)).

verb_type_db_0(Verb,main+TIV):-plt_call(Verb,'Verb', talk_db(TIV,Verb,_,_,_,_)).


:- dmsg(warn(error(remove_falure_next_line))).

verb_root_db(Govern):-plt_call(Govern,'Verb',(nop(verb_root_db),fail,talk_db(_Verb_i,Govern,_Governs,_GovernedImperfect,_Governing,_Governed))).
regular_pres_db(Govern):-plt_call(Govern,'Verb',(nop(regular_pres_db),talk_db(_,Govern,_Governs,_GovernedImperfect,_Governing,_Governed))).
regular_past_db(Governed,Govern):-plt_call(Governed,'Verb',(nop(regular_past_db),talk_db(_,Govern,_Governs,_GovernedImperfect,_Governing,Governed))).
verb_form_db(Active,Verb,pres+part,_):-plt_call((Active),'Verb',((nop(active),talk_db(_,Verb,_VerbPL,_Imperfect,Active,_PastPart)))).
verb_form_db(VerbPL,Verb,pres+fin,_):- t_l:useAltPOS ,plt2_call(VerbPL,'Verb',(nop(verb_ptl),talk_db(_,Verb,VerbPL,_Imperfect,_Active,_PastPart))).
verb_form_db(Imperfect,Verb,past+fin,_):-plt_call(Imperfect,'Verb',(nop(imperfect),talk_db(_,Verb,_VerbPL,Imperfect,_Active,_PastPart))).
verb_form_db(PastPart,Verb,past+part,_):-plt_call((PastPart),'Verb',(nop(past_part),talk_db(_,Verb,_VerbPL,_Imperfect,_Active,PastPart))).

verb_form_db(VerbS,Verb,pres+part,_+sg):-plt_call(VerbS,'Verb',((nop(root_active),(atom(VerbS)->atom_concat(Verb,'s',VerbS);true),cw_db_code(Verb,'Verb'),atom(Verb),atom_concat(Verb,'s',VerbS)))).


verb_root_db(Verb):-meetsForm80(Verb,Verb,form80(verb+root)).
verb_root_db(Verb):-meetsForm80(Verb,Verb,form80(3+sg)).
verb_type_db_0(Verb,MainPlusTrans):-verb_root_db(Verb),meetsForm80(_Form,Verb,form80(MainPlusTrans,main+tv)).
regular_pres_db(Verb):- meetsForm80(Verb,Verb,form80(regular_pres)).
regular_past_db(Form,Verb):- meetsForm80(Form,Verb,form80(regular_past)).
verb_form_db(Form,Verb,AsPresFin,As3_plus_sin):- meetsForm80(Form,Verb,form80(AsPresFin,pres+fin)), meetsForm80(Verb,Verb,form80(As3_plus_sin,3+sg)).
verb_form_db(Form,Verb,TensePlusPart,_):- meetsForm80(Form,Verb,form80(TensePlusPart)).
regular_pres_db(Verb):- verb_root_db(Verb).

:-style_check(+singleton).



verb_root_db(border).
regular_pres_db(border).
regular_past_db(bordered,border).
verb_form_db(borders,border,pres+fin,3+sg).
verb_form_db(bordering,border,pres+part,_).
verb_type_db_0(border,main+tv).
subj_obj_LF(trans,border, feature&place&_,X,feature&place&_,Y,borders(X,Y)).

borders(X,C) :- var(X), nonvar(C), !, borders(C,X).
borders(afghanistan,iran).
borders(iran,afghanistan).


/* THAT HAS COUNTABLE ATTRIBUTES SUCH AS.. */

subject_LF(thing,longitude,measure&position,X,longitude80(X)).
subject_LF(thing,latitude,measure&position,X,latitude80(X)).
subj_obj_LF(property,longitude,measure&position,X,feature&_,Y,longitude80(Y,X)).
subj_obj_LF(property,latitude, measure&position,X,feature&_,Y,latitude80(Y,X)).
noun_plu_db(longitudes,longitude). noun_plu_db(latitudes,latitude).

longitude80(C,L--degrees) :- country(C,_,_,L,_,_,_,_).
latitude80(C,L--degrees) :- country(C,_,L,_,_,_,_,_).

longitude80(_X--degrees).
latitude80(_X--degrees).

latitude80(tropic_of_capricorn,-23--degrees).
latitude80(tropic_of_cancer,23--degrees).
latitude80(equator,0--degrees).
latitude80(arctic_circle,67--degrees).
latitude80(antarctic_circle,-67--degrees).


% ------------------------------
% "N/S/E/W/of" 
% ------------------------------
loc_pred_prep_db(east,prep(eastof),of).
loc_pred_prep_db(west,prep(westof),of).
loc_pred_prep_db(north,prep(northof),of).
loc_pred_prep_db(south,prep(southof),of).

adjunction_lf(eastof,feature&_-X,feature&_-Y,eastof(X,Y)).
adjunction_lf(westof,feature&_-X,feature&_-Y,westof(X,Y)).
adjunction_lf(northof,feature&_-X,feature&_-Y,northof(X,Y)).
adjunction_lf(southof,feature&_-X,feature&_-Y,southof(X,Y)).

eastof(X1,X2) :- longitude(X1,L1), longitude(X2,L2), exceeds(L2,L1).
northof(X1,X2) :- latitude80(X1,L1), latitude80(X2,L2), exceeds(L1,L2).
southof(X1,X2) :- latitude80(X1,L1), latitude80(X2,L2), exceeds(L2,L1).
westof(X1,X2) :- longitude(X1,L1), longitude(X2,L2), exceeds(L1,L2).



% ------------------------------
% "Population is having a quantitity"
% ------------------------------
noun_plu_db(populations,population).

subject_LF(thing,population,measure&countables,X,population(X)).

subj_obj_LF(property,population, measure&countables,X,feature&_,Y,population(Y,X)).

population(C,P--thousand) :- city(C,_,P).
population(C,P--million) :- country(C,_,_,_,_,P0,_,_), P is integer(P0/1.0E6).

population(_X--million).
population(_X--thousand).

measure_unit_type_db(thousand,measure&countables,[],thousand).
measure_unit_type_db(million,measure&countables,[],million).

% ------------------------------
/*

 % BREAKS THINGS?

noun_sin_db(QuantProp):- quantity_props_db(_OfType,QuantProp).

subject_LF(thing,QuantProp,measure&OfType,X,denotesQuantity(X,OfType)):- quantity_props_db(OfType,QuantProp).
subj_obj_LF(property,QuantProp, measure&OfType,X,feature&_,Y,holds_t(QuantProp,Y,X)):- quantity_props_db(OfType,QuantProp).

quantity_props_db(inches,height).

denotesQuantity(_X--million,Countables):-quantity_props_db(Countables,_).
denotesQuantity(_X--thousand,Countables):-quantity_props_db(Countables,_).
denotesQuantity(N, Countables):-number(N),quantity_props_db(Countables,_).
*/


% ------------------------------
% "Contains" Inversion of the 'in' relation.
% ------------------------------
verb_root_db(contain).
verb_type_db_0(contain,main+tv).
regular_pres_db(contain).
regular_past_db(contained,contain).
verb_form_db(contains,contain,pres+fin,3+sg).
verb_form_db(containing,contain,pres+part,_).
subj_obj_LF(trans,contain,feature&place&_,X,feature&_,Y,in_ploc(Y,X)).

contains80(X,Y) :- contains0(X,Y).
contains80(X,Y) :- contains0(X,W), contains80(W,Y).

contains0(america,north_america).


% ------------------------------
% "In" 
% ------------------------------
context_pron_db(in,place,where).
context_pron_db(at,time,when).
% TODO context_pron_db(for,reason,why).
% TODO context_pron_db(by,method,how).

adjunction_lf(in,feature&_-X,feature&place&_-Y,in_ploc(X,Y)).

:- share_mp(in_ploc/2).
in_ploc(X,Y) :- var(X), nonvar(Y), !, contains80(Y,X).
in_ploc(X,Y) :- in_01(X,W), ( W=Y ; in_ploc(W,Y) ).

in_01(X,Y) :- in_continent(X,Y).
in_01(X,Y) :- city(X,Y,_).
in_01(X,Y) :- country(X,Y,_,_,_,_,_,_).
in_01(X,Y) :- flows(X,Y).

in_continent(middle_east,  asia).




% =================================================================
% INTERACTION OF TYPES
% =================================================================



/* Verbs */

verb_root_db(rise).
regular_pres_db(rise).
verb_form_db(rises,rise,pres+fin,3+sg).
verb_form_db(rose,rise,past+fin,_).
verb_form_db(risen,rise,past+part,_).
verb_type_db_0(rise,main+iv).
verb_LF(iv,rise,feature&river,X,rises(X,Y), [slot(prep(in),feature&place&_,Y,_,free)],_).

rises(R,C) :- river_pathlist(R,L), last(L,C).


verb_root_db(drain).
regular_pres_db(drain).
regular_past_db(drained,drain).
verb_form_db(drains,drain,pres+fin,3+sg).
verb_form_db(draining,drain,pres+part,_).
verb_type_db_0(drain,main+iv).
verb_LF(iv,drain,feature&river,X,drains(X,Y), [slot(prep(into),feature&place&_,Y,_,free)],_).

drains(R,S) :- river_pathlist(R,L), first(L,S).



verb_root_db(flow).
regular_pres_db(flow).
regular_past_db(flowed,flow).
verb_form_db(flows,flow,pres+fin,3+sg).
verb_form_db(flowing,flow,pres+part,_).
verb_type_db_0(flow,main+iv).
verb_LF(iv,flow,feature&river,X,call(flows,X,Y), [slot(prep(through),feature&place&_,Y,_,free)],_).
verb_LF(iv,flow,feature&river,X,call(flows,X,Y,Z), [slot(prep(into),feature&place&_,Z,_,free), slot(prep(from),feature&place&_,Y,_,free)],_). 



verb_root_db(Look):- clex_verb(_Formed,Look,_Iv,_Finsg).
regular_pres_db(Look):- no_loop_check(verb_root_db(Look)).
regular_past_db(Looked,Look):-clex_verb(Looked,Look,_Iv,prep_phrase).
verb_form_db(Looks,Look,pres+fin,3+sg):- clex_verb(Looks,Look,_,finsg).
verb_form_db(LookPL,Look,pres+fin,3+pl):- clex_verb(LookPL,Look,_,infpl).
verb_form_db(Looking,Look,pres+part,_):- (atom(Looking)->atom_concat(Look,'ing',Looking);var(Looking)),no_loop_check(verb_root_db(Look)),atom(Look),atom_concat(Look,'ing',Looking).
verb_type_db_0(Look,main+ITDV):- clex_verb(_Formed,Look,ITDV,_Finsg).
verb_LF(_,Assign,feature&_,X,dbase_t(Assign,X,Y), [slot(prep(To),feature&_,Y,_,free)],_):- clex_verb(_Assigned, Assign, dv(To),_).
verb_LF(_,Look,feature&_,X,dbase_t(Look,X,Y), [slot(prep(At),feature&_,Y,_,free)],_):- (tv_infpl(S,S);tv_finsg(S,S)),atomic_list_concat([Look,At],'-',S).

% verb_LF(iv,Look,feature&_,X,dbase_t(Look,X,Y,Z), [slot(prep(at),feature&_,Z,_,free), slot(prep(with),feature&_,Y,_,free)],_). 



flows(R,C) :- flows(R,C,_).
flows(R,C1,C2) :- river_pathlist(R,L), flow_links(L,C2,C1).
flow_links([X1,X2|_],X1,X2).
flow_links([_|L],X1,X2) :- flow_links(L,X1,X2).



% ------------------------------
/* Measure of Mass Nouns*/
% ------------------------------

noun_plu_db(areas,area).
subject_LF(thing,area,measure&area,X,isa_area(X)).
subj_obj_LF(property,area,measure&area,X,feature&place&_,Y,areaOf(Y,X)).
areaOf(C,A--ksqmiles) :- country(C,_,_,_,A0,_,_,_), A is A0/1000.
isa_area(_X--ksqmiles).


measure_unit_type_db(sqmile,measure&area,[],sqmiles).
measure_unit_type_db(ksqmile,measure&area,[],ksqmiles).

ratio_db(sqmiles,ksqmiles,1,1000).
ratio_db(ksqmiles,sqmiles,1000,1).
noun_plu_db(ksqmiles,ksqmile).
noun_plu_db(sqmiles,sqmile).

/*

property_measured_in_db(area,sqmile,sqmiles).
+
subj_obj_LF(property,area,measure&area,X,feature&place&_,Y,areaOf(Y,X)).

->

noun_plu_db(areas,area).
subject_LF(thing,area,measure&area,X,isa_area(X)).
subj_obj_LF(property,area,measure&area,X,feature&place&_,Y,areaOf(Y,X)).
areaOf(C,A--ksqmiles) :- country(C,_,_,_,A0,_,_,_), A is integer(A0/1000).
isa_area(_X--ksqmiles).


measure_unit_type_db(sqmile,measure&area,[],sqmiles).
measure_unit_type_db(ksqmile,measure&area,[],ksqmiles).

ratio_db(sqmiles,ksqmiles,1,1000).
ratio_db(ksqmiles,sqmiles,1000,1).
noun_plu_db(ksqmiles,ksqmile).
noun_plu_db(sqmiles,sqmile).


*/
:- shared_parser_data(clex_iface:clex_noun/5).

subject_LF(thing,CountNoun,feature&_,X,isa(X,RootNoun)):-  plt, clex_noun(CountNoun, RootNoun,_,_,_),not(ratio_db(RootNoun,_,_,_)).
subject_LF(thing,CountNoun,feature&_,X,adjIsa2(X,RootNoun)):- plt, clex_adj(CountNoun, RootNoun,_),not(ratio_db(RootNoun,_,_,_)).



/* Measure of Proportions and the like */
noun_form_db_0(proportion,proportion,X):-member(X,[pl,sg]).
comparator_db(proportion,_,V,[],proportion(V)).
noun_plu_db(degrees,degree).
measure_unit_type_db(degree,measure&position,[],degrees).
comparator_db(percentage,_,V,[],proportion(V)).
noun_form_db_0(percentage,percentage,X):-member(X,[pl,sg]).
noun_plu_db(thousand,thousand).
noun_plu_db(million,million).
ratio_db(million,thousand,1000,1).
ratio_db(thousand,million,1,1000).


adj_db(average,restr).
aggr_adj_db(average,_,_,average).
noun_plu_db(averages,average).
aggr_noun_db(average,_,_,average).


aggr_adj_db(minimum,_,_,minimum).
adj_db(minimum,restr).
aggr_adj_db(maximum,_,_,maximum).
adj_db(maximum,restr).

meta_noun_db(number,_,V,feature&_,X,P,numberof(X,P,V)).
noun_plu_db(numbers,number).


adj_db(total,restr).
noun_plu_db(totals,total).
aggr_adj_db(total,_,_,total).
aggr_noun_db(total,_,_,total).
aggr_noun_db(sum,_,_,total).
noun_plu_db(sums,sum).


/* Measure of Greater or Lesser amounts*/
verb_root_db(exceed).
verb_type_db_0(exceed,main+tv).
regular_pres_db(exceed).
regular_past_db(exceeded,exceed).
verb_form_db(exceeds,exceed,pres+fin,3+sg).
verb_form_db(exceeding,exceed,pres+part,_).
subj_obj_LF(trans,   exceed,measure&Type,X,measure&Type,Y,exceeds(X,Y)).
subj_obj_LF(attribute,great,measure&Type,X,measure&Type,Y,exceeds(X,Y)).


measure_op_db(id(_Why),X,X,true).
measure_op_db(same,X,Y,X=Y).
measure_op_db(less,X,Y,exceeds(Y,X)).
measure_op_db(not(_Why)+less,X,Y,\+exceeds(Y,X)).
measure_op_db(more,X,Y,exceeds(X,Y)).
measure_op_db(not(_Why)+more,X,Y,\+exceeds(X,Y)).

inverse_db(most,-,least).
inverse_db(least,-,most).
inverse_db(same,-,same).
inverse_db(less,-,more).
inverse_db(more,-,less).
inverse_db(X,+,X).

exceeds('--'(X1,U1),'--'(X2,U1)) :- 
  \+ \+ (U1==U2 -> X1 > X2 ; (ratio_db(U1,U2,M1,M2), X1*M1 > X2*M2)).

sup_adj_db(Biggest,Big):-plt2_call(Big,'Adjective',plt2_call(Biggest,'Adjective',talk_db(superl,Big,Biggest))).
sup_adj_db(Biggest,Big):-plt_call(Big,'Adjective',plt_call(Biggest,'Adjective',adj_itr_sup(Biggest,Big))).
% /* Comparative */
rel_adj_db(Bigger,Big):-plt2_call(Big,'Adjective',plt_call(Bigger,'Adjective',talk_db(comp,Big,Bigger))).
rel_adj_db(Bigger,Big):-plt_call(Big,'Adjective',plt_call(Bigger,'Adjective',adj_itr_comp(Bigger,Big))).

subj_obj_LF(attribute,small,feature&place&_,X,measure&area,Y,areaOf(X,Y)).
subj_obj_LF(attribute,large,feature&place&_,X,measure&area,Y,areaOf(X,Y)).

subj_obj_LF(attribute,small,feature&Place&_,X,measure&Area,Y,holds_t(AreaPred,X,Y)):-type_measured_by_pred_db(Place,Area,AreaPred).
subj_obj_LF(attribute,large,feature&Place&_,X,measure&Area,Y,holds_t(AreaPred,X,Y)):-type_measured_by_pred_db(Place,Area,AreaPred).

type_measured_by_pred_db(human,feet,height).

units_db(small,measure&_).
units_db(large,measure&_).
rel_adj_db(smaller,small).
sup_adj_db(smallest,small).
rel_adj_db(larger,large).
sup_adj_db(largest,large).
adj_sign_db(large,+).
adj_sign_db(small,-).
adj_sign_db(great,+).


comp_adv_db(less).
comp_adv_db(more).

sup_adv_db(least).
sup_adv_db(most).

rel_adj_db(less,small).
rel_adj_db(greater,great).


rel_adj_db(bigger,big).
sup_adj_db(biggest,big).
adj_db(small,quant).
adj_db(large,quant).
adj_db(great,quant).
adj_db(big,quant).

adj_db(Big,quant):-plt_call(Big,'Adjective',(talk_db(superl,Big,_Biggest))).
adj_db(Big,quant):-plt_call(Big,'Adjective',(talk_db(comp,Big,_Bigger))).

adj_db(old,quant).
adj_db(new,quant).
rel_adj_db(older,old).
sup_adj_db(oldest,old).
rel_adj_db(newer,new).
sup_adj_db(newest,new).



/*
We can parse:

which is the largest X?

but also need to parse:

which X is the largest?


chat80 what are the items?
chat80 how many items are there?
chat80 how many types are there?
chat80 how many postures are there?
chat80("what are the postures that are verbs?")
chat80("how many rivers are rivers?").
chat80 you flow to the ocean
*/


current_dcg_predicate(F/A):-current_predicate(F/A).

toDCPred(Type,Pred,In,Out):-compound(Type),!,functor(Type,F,A),A2 is A + 2,current_dcg_predicate(F/A2),once((length(Args,A),Type=..[F|Args],append(Args,[In,Out],Dargs))),Pred=..[F|Dargs].

toDCPred(Type,Pred,In,Out):-atom(F),current_dcg_predicate(F/A2),A is A2 - 2,once((length(Args,A),Type=..[F|Args],append(Args,[In,Out],Dargs))),Pred=..[F|Dargs].

probeDCG(Left,Content,Right,Type):-length_between(0,1,Left),length_between(0,1,Right),append(Left,Content,Fisrt),append(Fisrt,Right,In),toDCPred(Type,Pred,In,[]),Pred.
:-share_mp(ph/2).
ph(Type,Content):-show_call(probeDCG(_,Content,_,Type)).

length_between(S,E,Left):-between(S,E,X),length(Left,X).


:-kb_shared(must_test_801/3).

must_test_801([what, rivers, are, there, ?], [sent([what, rivers, are, there, ?]), parse(whq(feature&river-B, s(np(3+pl, np_head(int_det(feature&river-B), [], river), []), verb(be, active, pres+fin, [], pos), [void], []))), sem((answer80([A]):-river(A), A^true)), qplan((answer80([B]):-river(B), B^true)), 
answers([amazon, amu_darya, amur, brahmaputra, colorado, congo_river, cubango, danube, don, elbe, euphrates, ganges, hwang_ho, indus, irrawaddy, lena, limpopo, mackenzie, mekong, mississippi, murray, niger_river, nile, ob, oder, orange, orinoco, parana, rhine, rhone, rio_grande, salween, senegal_river, tagus, vistula, volga, volta, yangtze, yenisei, yukon, zambesi])],[time(0.0)]).
must_test_801([does, afghanistan, border, china, ?], [sent([does, afghanistan, border, china, ?]), parse(q(s(np(3+sg, nameOf(afghanistan), []), verb(border, active, pres+fin, [], pos), [arg(dir, np(3+sg, name(china), []))], []))), sem((answer80([]):-borders(afghanistan, china))), qplan((answer80([]):-{borders(afghanistan, china)})), 
answers([true])],[time(0.0)]).
must_test_801([what, is, the, capital, of, upper,volta, ?], [sent([what, is, the, capital, of, upper,volta, ?]), parse(whq(feature&city-B, s(np(3+sg, wh(feature&city-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sg, np_head(det(the(sg)), [], capital), [prep_phrase(prep(of), np(3+sg, name(upper_volta), []))]))], []))), sem((answer80([A]):-capital(upper_volta, A))), qplan((answer80([A]):-capital(upper_volta, A))), 
answers([ouagadougou])],[time(0.0010000000000000009)]).
must_test_801([where, is, the, largest, country, ?], [sent([where, is, the, largest, country, ?]), parse(whq(feature&place&A-B, s(np(3+sg, np_head(det(the(sg)), [sup(most, adj(large))], country), []), verb(be, active, pres+fin, [], pos), [arg(pred, prep_phrase(prep(in), np(_, np_head(int_det(feature&place&A-B), [], place), [])))], []))), sem((answer80([A]):-B^ (C^ (setof(D:E, (country(E), area(E, D)), C), aggregate(max, C, B)), place(A), in_ploc(B, A)))), qplan((answer80([F]):-E^D^ (setof(C:B, (country(B), area(B, C)), D), aggregate(max, D, E), in_ploc(E, F), {place(F)}))), 
answers([asia, northern_asia])],[time(0.0009999999999999731)]).
must_test_801([which, countries, are, european, ?], [sent([which, countries, are, european, ?]), parse(whq(feature&place&country-B, s(np(3+pl, np_head(int_det(feature&place&country-B), [], country), []), verb(be, active, pres+fin, [], pos), [arg(pred, adj(european))], []))), sem((answer80([A]):-country(A), european(A))), qplan((answer80([A]):-european(A), {country(A)})), 
answers([albania, andorra, austria, belgium, bulgaria, cyprus, czechoslovakia, denmark, east_germany, eire, finland, france, greece, hungary, iceland, italy, liechtenstein, luxembourg, malta, monaco, netherlands, norway, poland, portugal, romania, san_marino, spain, sweden, switzerland, united_kingdom, west_germany, yugoslavia])],[time(0.0)]).
must_test_801([which, is, the, largest, african, country, ?], [sent([which, is, the, largest, african, country, ?]), parse(whq(feature&place&country-B, s(np(3+sg, wh(feature&place&country-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sg, np_head(det(the(sg)), [sup(most, adj(large)), adj(african)], country), []))], []))), sem((answer80([A]):-B^ (setof(C:D, (country(D), area(D, C), african(D)), B), aggregate(max, B, A)))), qplan((answer80([D]):-C^ (setof(B:A, (african(A), {country(A)}, area(A, B)), C), aggregate(max, C, D)))), 
answers([sudan])],[time(0.0)]).
must_test_801([what, is, the, ocean, that, borders, african, countries, and, that, borders, asian, countries, ?], [sent([what, is, the, ocean, that, borders, african, countries, and, that, borders, asian, countries, ?]), parse(whq(feature&place&seamass-B, s(np(3+sg, wh(feature&place&seamass-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sg, np_head(det(the(sg)), [], ocean), [conj(and, rel(feature&place&seamass-C, s(np(3+sg, wh(feature&place&seamass-C), []), verb(border, active, pres+fin, [], pos), [arg(dir, np(3+pl, np_head(generic, [adj(african)], country), []))], [])), rel(feature&place&seamass-C, s(np(3+sg, wh(feature&place&seamass-C), []), verb(border, active, pres+fin, [], pos), [arg(dir, np(3+pl, np_head(generic, [adj(asian)], country), []))], [])))]))], []))), sem((answer80([A]):-ocean(A), B^ (country(B), african(B), borders(A, B)), C^ (country(C), asian(C), borders(A, C)))), qplan((answer80([A]):-B^C^ (ocean(A), {borders(A, B), {african(B)}, {country(B)}}, {borders(A, C), {asian(C)}, {country(C)}}))), 
answers([indian_ocean])],[time(0.0020000000000000018)]).
must_test_801([what, are, the, capitals, of, the, countries, bordering, the, baltic, ?], [sent([what, are, the, capitals, of, the, countries, bordering, the, baltic, ?]), parse(whq(feature&city-B, s(np(3+pl, wh(feature&city-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+pl, np_head(det(the(pl)), [], capital), [prep_phrase(prep(of), np(3+pl, np_head(det(the(pl)), [], country), [reduced_rel(feature&place&country-D, s(np(3+pl, wh(feature&place&country-D), []), verb(border, active, inf, [prog], pos), [arg(dir, np(3+sg, name(baltic), []))], []))]))]))], []))), sem((answer80([D]):-setof([A]:C, (country(A), borders(A, baltic), setof(B, capital(A, B), C)), D))), qplan((answer80([H]):-setof([E]:G, (country(E), borders(E, baltic), setof(F, capital(E, F), G)), H))), 
answers([[[denmark]:[copenhagen], [east_germany]:[east_berlin], [finland]:[helsinki], [poland]:[warsaw], [soviet_union]:[moscow], [sweden]:[stockholm], [west_germany]:[bonn]]])],[time(0.0010000000000000009)]).
must_test_801([how, many, countries, does, the, danube, flow, through, ?], [sent([how, many, countries, does, the, danube, flow, through, ?]), parse(whq(feature&place&country-B, s(np(3+sg, name(danube), []), verb(flow, active, pres+fin, [], pos), [], [prep_phrase(prep(through), np(3+pl, np_head(quant(same, wh(feature&place&country-B)), [], country), []))]))), sem((answer80([A]):-numberof(B, (country(B), flows(danube, B)), A))), qplan((answer80([B]):-numberof(A, (flows(danube, A), {country(A)}), B))), 
answers([6])],[time(0.0010000000000000009)]).
must_test_801([what, is, the, average, area, of, the, countries, in, each, continent, ?], [sent([what, is, the, average, area, of, the, countries, in, each, continent, ?]), parse(whq(A-C, s(np(3+sg, wh(A-C), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sg, np_head(det(the(sg)), [adj(average)], area), [prep_phrase(prep(of), np(3+pl, np_head(det(the(pl)), [], country), [prep_phrase(prep(in), np(3+sg, np_head(det(each), [], continent), []))]))]))], []))), sem((answer80([B, E]):-continent(B), [ (0--ksqmiles):[andorra], (0--ksqmiles):[liechtenstein], (0--ksqmiles):[malta], (0--ksqmiles):[monaco], (0--ksqmiles):[san_marino], (1--ksqmiles):[luxembourg], (4--ksqmiles):[cyprus], (11--ksqmiles):[albania], (12--ksqmiles):[belgium], (14--ksqmiles):[netherlands], (16--ksqmiles):[switzerland], (17--ksqmiles):[denmark], (27--ksqmiles):[eire], (32--ksqmiles):[austria], (35--ksqmiles):[portugal], (36--ksqmiles):[hungary], (40--ksqmiles):[iceland], (41--ksqmiles):[east_germany], (43--ksqmiles):[bulgaria], (49--ksqmiles):[czechoslovakia], (51--ksqmiles):[greece], (92--ksqmiles):[romania], (94--ksqmiles):[united_kingdom], (96--ksqmiles):[west_germany], (99--ksqmiles):[yugoslavia], (116--ksqmiles):[italy], (120--ksqmiles):[poland], (125--ksqmiles):[norway], (130--ksqmiles):[finland], (174--ksqmiles):[sweden], (195--ksqmiles):[spain], (213--ksqmiles):[france]]^ (setof(D:[C], (area(C, D), country(C), in_ploc(C, B)), [ (0--ksqmiles):[andorra], (0--ksqmiles):[liechtenstein], (0--ksqmiles):[malta], (0--ksqmiles):[monaco], (0--ksqmiles):[san_marino], (1--ksqmiles):[luxembourg], (4--ksqmiles):[cyprus], (11--ksqmiles):[albania], (12--ksqmiles):[belgium], (14--ksqmiles):[netherlands], (16--ksqmiles):[switzerland], (17--ksqmiles):[denmark], (27--ksqmiles):[eire], (32--ksqmiles):[austria], (35--ksqmiles):[portugal], (36--ksqmiles):[hungary], (40--ksqmiles):[iceland], (41--ksqmiles):[east_germany], (43--ksqmiles):[bulgaria], (49--ksqmiles):[czechoslovakia], (51--ksqmiles):[greece], (92--ksqmiles):[romania], (94--ksqmiles):[united_kingdom], (96--ksqmiles):[west_germany], (99--ksqmiles):[yugoslavia], (116--ksqmiles):[italy], (120--ksqmiles):[poland], (125--ksqmiles):[norway], (130--ksqmiles):[finland], (174--ksqmiles):[sweden], (195--ksqmiles):[spain], (213--ksqmiles):[france]]), aggregate(average, [ (0--ksqmiles):[andorra], (0--ksqmiles):[liechtenstein], (0--ksqmiles):[malta], (0--ksqmiles):[monaco], (0--ksqmiles):[san_marino], (1--ksqmiles):[luxembourg], (4--ksqmiles):[cyprus], (11--ksqmiles):[albania], (12--ksqmiles):[belgium], (14--ksqmiles):[netherlands], (16--ksqmiles):[switzerland], (17--ksqmiles):[denmark], (27--ksqmiles):[eire], (32--ksqmiles):[austria], (35--ksqmiles):[portugal], (36--ksqmiles):[hungary], (40--ksqmiles):[iceland], (41--ksqmiles):[east_germany], (43--ksqmiles):[bulgaria], (49--ksqmiles):[czechoslovakia], (51--ksqmiles):[greece], (92--ksqmiles):[romania], (94--ksqmiles):[united_kingdom], (96--ksqmiles):[west_germany], (99--ksqmiles):[yugoslavia], (116--ksqmiles):[italy], (120--ksqmiles):[poland], (125--ksqmiles):[norway], (130--ksqmiles):[finland], (174--ksqmiles):[sweden], (195--ksqmiles):[spain], (213--ksqmiles):[france]], E)))), qplan((answer80([F, J]):-continent(F), I^ (setof(H:[G], (area(G, H), country(G), in_ploc(G, F)), I), aggregate(average, I, J)))), 
answers([[europe, 58.84375--ksqmiles]])],[time(0.0040000000000000036)]).
must_test_801([is, there, more, than, one, country, in, each, continent, ?], [sent([is, there, more, than, one, country, in, each, continent, ?]), parse(q(s(there, verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sg, np_head(quant(more, nb(1)), [], country), [prep_phrase(prep(in), np(3+sg, np_head(det(each), [], continent), []))]))], []))), sem((answer80([]):- \+A^ (continent(A), \+C^ (numberof(B, (country(B), in_ploc(B, A)), C), C>1)))), qplan((answer80([]):- \+D^ (continent(D), \+F^ (numberof(E, (country(E), in_ploc(E, D)), F), F>1)))), 
answers([false])],[time(0.0010000000000000009)]).
must_test_801([is, there, some, ocean, that, does, not, border, any, country, ?], [sent([is, there, some, ocean, that, does, not, border, any, country, ?]), parse(q(s(there, verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sg, np_head(det(some), [], ocean), [rel(feature&place&seamass-B, s(np(3+sg, wh(feature&place&seamass-B), []), verb(border, active, pres+fin, [], neg), [arg(dir, np(3+sg, np_head(det(any), [], country), []))], []))]))], []))), sem((answer80([]):-A^ (ocean(A), \+B^ (country(B), borders(A, B))))), qplan((answer80([]):-A^{ocean(A), {\+B^ (borders(A, B), {country(B)})}})), 
answers([true])],[time(0.0010000000000000009)]).
must_test_801([what, are, the, countries, from, which, a, river, flows, into, the, black, sea, ?], [sent([what, are, the, countries, from, which, a, river, flows, into, the, black_sea, ?]), parse(whq(feature&place&country-B, s(np(3+pl, wh(feature&place&country-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+pl, np_head(det(the(pl)), [], country), [rel(feature&place&country-D, s(np(3+sg, np_head(det(a), [], river), []), verb(flow, active, pres+fin, [], pos), [], [prep_phrase(prep(from), np(3+pl, wh(feature&place&country-D), [])), prep_phrase(prep(into), np(3+sg, name(black_sea), []))]))]))], []))), sem((answer80([A]):-setof(B, (country(B), C^ (river(C), flows(C, B, black_sea))), A))), qplan((answer80([C]):-setof(B, A^ (flows(A, B, black_sea), {country(B)}, {river(A)}), C))), 
answers([[romania]])],[time(0.0010000000000000009)]).
must_test_801([which, countries, have, a, population, exceeding, nb(10), million, ?], [sent([which, countries, have, a, population, exceeding, nb(10), million, ?]), parse(whq(feature&place&country-B, s(np(3+pl, np_head(int_det(feature&place&country-B), [], country), []), verb(have, active, pres+fin, [], pos), [arg(dir, np(3+sg, np_head(det(a), [], population), [reduced_rel(measure&countables-C, s(np(3+sg, wh(measure&countables-C), []), verb(exceed, active, inf, [prog], pos), [arg(dir, np(3+pl, np_head(quant(same, nb(10)), [], million), []))], []))]))], []))), sem((answer80([A]):-country(A), B^ (exceeds(B, 10--million), population(A, B)))), qplan((answer80([A]):-B^ (country(A), {population(A, B), {exceeds(B, 10--million)}}))), 
answers([malaysia, uganda])],[time(0.0010000000000000009)]).
must_test_801([which, countries, with, a, population, exceeding, nb(10), million, border, the, atlantic, ?], [sent([which, countries, with, a, population, exceeding, nb(10), million, border, the, atlantic, ?]), parse(whq(feature&place&country-B, s(np(3+pl, np_head(int_det(feature&place&country-B), [], country), [prep_phrase(prep(with), np(3+sg, np_head(det(a), [], population), [reduced_rel(measure&countables-C, s(np(3+sg, wh(measure&countables-C), []), verb(exceed, active, inf, [prog], pos), [arg(dir, np(3+pl, np_head(quant(same, nb(10)), [], million), []))], []))]))]), verb(border, active, pres+fin, [], pos), [arg(dir, np(3+sg, name(atlantic), []))], []))), sem((answer80([A]):-B^ (population(A, B), exceeds(B, 10--million), country(A)), borders(A, atlantic))), qplan((answer80([A]):-B^ (borders(A, atlantic), {population(A, B), {exceeds(B, 10--million)}}, {country(A)}))), 
answers([venezuela])],[time(0.0010000000000000009)]).
must_test_801([what, percentage, of, countries, border, each, ocean, ?], [sent([what, percentage, of, countries, border, each, ocean, ?]), parse(whq(A-C, s(np(3+pl, np_head(int_det(A-C), [], percentage), [prep_phrase(prep(of), np(3+pl, np_head(generic, [], country), []))]), verb(border, active, pres+fin, [], pos), [arg(dir, np(3+sg, np_head(det(each), [], ocean), []))], []))), sem((answer80([B, E]):-ocean(B), [afghanistan, albania, algeria, andorra, angola, argentina, australia, austria, bahamas, bahrain, bangladesh, barbados, belgium, belize, bhutan, bolivia, botswana, brazil, bulgaria, burma, burundi, cambodia, cameroon, canada, central_african_republic, chad, chile, china, colombia, congo, costa_rica, cuba, cyprus, czechoslovakia, dahomey, denmark, djibouti, dominican_republic, east_germany, ecuador, egypt, eire, el_salvador, equatorial_guinea, ethiopia, fiji, finland, france, french_guiana, gabon, gambia, ghana, greece, grenada, guatemala, guinea, guinea_bissau, guyana, haiti, honduras, hungary, iceland, india, indonesia, iran, iraq, israel, italy, ivory_coast, jamaica, japan, jordan, kenya, kuwait, laos, lebanon, lesotho, liberia, libya, liechtenstein, luxembourg, malagasy, malawi, malaysia, maldives, mali, malta, mauritania, mauritius, mexico, monaco, mongolia, morocco, mozambique, nepal, netherlands, new_zealand, nicaragua, niger, nigeria, north_korea, norway, oman, pakistan, panama, papua_new_guinea, paraguay, peru, philippines, poland, portugal, qatar, romania, rwanda, san_marino, saudi_arabia, senegal, seychelles, sierra_leone, singapore, somalia, south_africa, south_korea, south_yemen, soviet_union, spain, sri_lanka, sudan, surinam, swaziland, sweden, switzerland, syria, taiwan, tanzania, thailand, togo, tonga, trinidad_and_tobago, tunisia, turkey, uganda, united_arab_emirates, united_kingdom, united_states, upper_volta, uruguay, venezuela, vietnam, west_germany, western_samoa, yemen, yugoslavia, zaire, zambia, zimbabwe]^ (setof(C, country(C), [afghanistan, albania, algeria, andorra, angola, argentina, australia, austria, bahamas, bahrain, bangladesh, barbados, belgium, belize, bhutan, bolivia, botswana, brazil, bulgaria, burma, burundi, cambodia, cameroon, canada, central_african_republic, chad, chile, china, colombia, congo, costa_rica, cuba, cyprus, czechoslovakia, dahomey, denmark, djibouti, dominican_republic, east_germany, ecuador, egypt, eire, el_salvador, equatorial_guinea, ethiopia, fiji, finland, france, french_guiana, gabon, gambia, ghana, greece, grenada, guatemala, guinea, guinea_bissau, guyana, haiti, honduras, hungary, iceland, india, indonesia, iran, iraq, israel, italy, ivory_coast, jamaica, japan, jordan, kenya, kuwait, laos, lebanon, lesotho, liberia, libya, liechtenstein, luxembourg, malagasy, malawi, malaysia, maldives, mali, malta, mauritania, mauritius, mexico, monaco, mongolia, morocco, mozambique, nepal, netherlands, new_zealand, nicaragua, niger, nigeria, north_korea, norway, oman, pakistan, panama, papua_new_guinea, paraguay, peru, philippines, poland, portugal, qatar, romania, rwanda, san_marino, saudi_arabia, senegal, seychelles, sierra_leone, singapore, somalia, south_africa, south_korea, south_yemen, soviet_union, spain, sri_lanka, sudan, surinam, swaziland, sweden, switzerland, syria, taiwan, tanzania, thailand, togo, tonga, trinidad_and_tobago, tunisia, turkey, uganda, united_arab_emirates, united_kingdom, united_states, upper_volta, uruguay, venezuela, vietnam, west_germany, western_samoa, yemen, yugoslavia, zaire, zambia, zimbabwe]), 4^ (numberof(D, (one_of([afghanistan, albania, algeria, andorra, angola, argentina, australia, austria, bahamas, bahrain, bangladesh, barbados, belgium, belize, bhutan, bolivia, botswana, brazil, bulgaria, burma, burundi, cambodia, cameroon, canada, central_african_republic, chad, chile, china, colombia, congo, costa_rica, cuba, cyprus, czechoslovakia, dahomey, denmark, djibouti, dominican_republic, east_germany, ecuador, egypt, eire, el_salvador, equatorial_guinea, ethiopia, fiji, finland, france, french_guiana, gabon, gambia, ghana, greece, grenada, guatemala, guinea, guinea_bissau, guyana, haiti, honduras, hungary, iceland, india, indonesia, iran, iraq, israel, italy, ivory_coast, jamaica, japan, jordan, kenya, kuwait, laos, lebanon, lesotho, liberia, libya, liechtenstein, luxembourg, malagasy, malawi, malaysia, maldives, mali, malta, mauritania, mauritius, mexico, monaco, mongolia, morocco, mozambique, nepal, netherlands, new_zealand, nicaragua, niger, nigeria, north_korea, norway, oman, pakistan, panama, papua_new_guinea, paraguay, peru, philippines, poland, portugal, qatar, romania, rwanda, san_marino, saudi_arabia, senegal, seychelles, sierra_leone, singapore, somalia, south_africa, south_korea, south_yemen, soviet_union, spain, sri_lanka, sudan, surinam, swaziland, sweden, switzerland, syria, taiwan, tanzania, thailand, togo, tonga, trinidad_and_tobago, tunisia, turkey, uganda, united_arab_emirates, united_kingdom, united_states, upper_volta, uruguay, venezuela, vietnam, west_germany, western_samoa, yemen, yugoslavia, zaire, zambia, zimbabwe], D), borders(D, B)), 4), 156^ (card([afghanistan, albania, algeria, andorra, angola, argentina, australia, austria, bahamas, bahrain, bangladesh, barbados, belgium, belize, bhutan, bolivia, botswana, brazil, bulgaria, burma, burundi, cambodia, cameroon, canada, central_african_republic, chad, chile, china, colombia, congo, costa_rica, cuba, cyprus, czechoslovakia, dahomey, denmark, djibouti, dominican_republic, east_germany, ecuador, egypt, eire, el_salvador, equatorial_guinea, ethiopia, fiji, finland, france, french_guiana, gabon, gambia, ghana, greece, grenada, guatemala, guinea, guinea_bissau, guyana, haiti, honduras, hungary, iceland, india, indonesia, iran, iraq, israel, italy, ivory_coast, jamaica, japan, jordan, kenya, kuwait, laos, lebanon, lesotho, liberia, libya, liechtenstein, luxembourg, malagasy, malawi, malaysia, maldives, mali, malta, mauritania, mauritius, mexico, monaco, mongolia, morocco, mozambique, nepal, netherlands, new_zealand, nicaragua, niger, nigeria, north_korea, norway, oman, pakistan, panama, papua_new_guinea, paraguay, peru, philippines, poland, portugal, qatar, romania, rwanda, san_marino, saudi_arabia, senegal, seychelles, sierra_leone, singapore, somalia, south_africa, south_korea, south_yemen, soviet_union, spain, sri_lanka, sudan, surinam, swaziland, sweden, switzerland, syria, taiwan, tanzania, thailand, togo, tonga, trinidad_and_tobago, tunisia, turkey, uganda, united_arab_emirates, united_kingdom, united_states, upper_volta, uruguay, venezuela, vietnam, west_germany, western_samoa, yemen, yugoslavia, zaire, zambia, zimbabwe], 156), ratio(4, 156, E)))))), qplan((answer80([F, L]):-ocean(F), H^ (setof(G, country(G), H), J^ (numberof(I, (one_of(H, I), borders(I, F)), J), K^ (card(H, K), ratio(J, K, L)))))), 
answers([[arctic_ocean, 2.5641025641025643]])],[time(0.0020000000000000018)]).
must_test_801([what, countries, are, there, in, europe, ?], [sent([what, countries, are, there, in, europe, ?]), parse(whq(feature&place&country-B, s(np(3+pl, np_head(int_det(feature&place&country-B), [], country), []), verb(be, active, pres+fin, [], pos), [void], [prep_phrase(prep(in), np(3+sg, name(europe), []))]))), sem((answer80([A]):-country(A), in_ploc(A, europe))), qplan((answer80([A]):-in_ploc(A, europe), {country(A)})), 
answers([albania, andorra, austria, belgium, bulgaria, cyprus, czechoslovakia, denmark, east_germany, eire, finland, france, greece, hungary, iceland, italy, liechtenstein, luxembourg, malta, monaco, netherlands, norway, poland, portugal, romania, san_marino, spain, sweden, switzerland, united_kingdom, west_germany, yugoslavia])],[time(0.0010000000000000009)]).

must_test_801(U,R,O):-must_test_802(U,R,O).
must_test_801(U,R,O):-must_test_803(U,R,O).

must_test_802([how, large, is, the, smallest, american, country, ?], [sent([how, large, is, the, smallest, american, country, ?]), parse(whq(measure&area-B, s(np(3+sg, np_head(det(the(sg)), [sup(most, adj(small)), adj(american)], country), []), verb(be, active, pres+fin, [], pos), [arg(pred, value(adj(large), wh(measure&area-B)))], []))), sem((answer80([A]):-B^ (C^ (setof(D:E, (country(E), area(E, D), american(E)), C), aggregate(min, C, B)), area(B, A)))), qplan((answer80([E]):-D^C^ (setof(B:A, (american(A), {country(A)}, area(A, B)), C), aggregate(min, C, D), area(D, E)))), 
answers([0--ksqmiles])],[time(0.0)]).
must_test_802([what, is, the, total, area, of, countries, south, of, the, equator, and, not, in, australasia, ?], [sent([what, is, the, total, area, of, countries, south, of, the, equator, and, not, in, australasia, ?]), parse(whq(A-B, s(np(3+sg, wh(A-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sg, np_head(det(the(sg)), [adj(total)], area), [prep_phrase(prep(of), np(3+pl, np_head(generic, [], country), [conj(and, reduced_rel(feature&place&country-F, s(np(3+pl, wh(feature&place&country-F), []), verb(be, active, pres+fin, [], pos), [arg(pred, prep_phrase(prep(southof), np(3+sg, name(equator), [])))], [])), reduced_rel(feature&place&country-F, s(np(3+pl, wh(feature&place&country-F), []), verb(be, active, pres+fin, [], neg), [arg(pred, prep_phrase(prep(in), np(3+sg, name(australasia), [])))], [])))]))]))], []))), sem((answer80([A]):-B^ (setof(C:[D], (area(D, C), country(D), southof(D, equator), \+in_ploc(D, australasia)), B), aggregate(total, B, A)))), 
qplan((answer80([E]):-D^ (setof(C:[B], (southof(B, equator), area(B, C), {country(B)}, {\+in_ploc(B, australasia)}), D), aggregate(total, D, E)))), 
answers([10239--ksqmiles])],[time(0.0010000000000000009)]).
must_test_802([which, countries, are, bordered, by, two, seas, ?], [sent([which, countries, are, bordered, by, two, seas, ?]), parse(whq(feature&place&country-B, s(np(3+pl, np_head(int_det(feature&place&country-B), [], country), []), verb(border, passive, pres+fin, [], pos), [], [prep_phrase(prep(by), np(3+pl, np_head(quant(same, nb(2)), [], sea), []))]))), 
sem((answer80([A]):-country(A), numberof(B, (sea(B), borders(B, A)), 2))), 
qplan((answer80([B]):-numberof(A, (sea(A), borders(A, B)), 2), {country(B)})), 
answers([egypt, iran, israel, saudi_arabia, turkey])],[time(0.0)]).

must_test_803([which, country, bordering, the, mediterranean, borders, a, country, that, is, bordered, by, a, country, whose, population, exceeds, the, population, of, india, ?], [sent([which, country, bordering, the, mediterranean, borders, a, country, that, is, bordered, by, a, country, whose, population, exceeds, the, population, of, india, ?]), parse(whq(feature&place&country-B, s(np(3+sg, np_head(int_det(feature&place&country-B), [], country), [reduced_rel(feature&place&country-B, s(np(3+sg, wh(feature&place&country-B), []), verb(border, active, inf, [prog], pos), [arg(dir, np(3+sg, name(mediterranean), []))], []))]), verb(border, active, pres+fin, [], pos), [arg(dir, np(3+sg, np_head(det(a), [], country), [rel(feature&place&country-C, s(np(3+sg, wh(feature&place&country-C), []), verb(border, passive, pres+fin, [], pos), [], [prep_phrase(prep(by), np(3+sg, np_head(det(a), [], country), [rel(feature&place&country-D, s(np(3+sg, np_head(det(the(sg)), [], population), [prep_phrase(poss, np(3+sg, wh(feature&place&country-D), []))]), verb(exceed, active, pres+fin, [], pos), [arg(dir, np(3+sg, np_head(det(the(sg)), [], population), [prep_phrase(prep(of), np(3+sg, name(india), []))]))], []))]))]))]))], []))), 
sem((answer80([A]):-country(A), borders(A, mediterranean), B^ (country(B), C^ (country(C), D^ (population(C, D), E^ (population(india, E), exceeds(D, E))), borders(C, B)), borders(A, B)))), qplan((answer80([B]):-C^D^E^A^ (population(india, A), borders(B, mediterranean), {country(B)}, {borders(B, C), {country(C)}, {borders(D, C), {country(D)}, {population(D, E), {exceeds(E, A)}}}}))), 
answers([turkey])],[time(0.0020000000000000018)]).
must_test_803([which, country, '\'', s, capital, is, london, ?], [sent([which, country, '\'', s, capital, is, london, ?]), parse(whq(feature&place&country-B, s(np(3+sg, np_head(det(the(sg)), [], capital), [prep_phrase(poss, np(3+sg, np_head(int_det(feature&place&country-B), [], country), []))]), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sg, name(london), []))], []))), sem((answer80([A]):-country(A), capital(A, london))), 
qplan((answer80([A]):-capital(A, london), {country(A)})), 
answers([united_kingdom])],[time(0.0010000000000000009)]).
must_test_803([what, are, the, continents, no, country, in, which, contains, more, than, two, cities, whose, population, exceeds, nb(1), million, ?], [sent([what, are, the, continents, no, country, in, which, contains, more, than, two, cities, whose, population, exceeds, nb(1), million, ?]), parse(whq(feature&place&continent-B, s(np(3+pl, wh(feature&place&continent-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+pl, np_head(det(the(pl)), [], continent), [rel(feature&place&continent-D, s(np(3+sg, np_head(det(no), [], country), [prep_phrase(prep(in), np(3+pl, wh(feature&place&continent-D), []))]), verb(contain, active, pres+fin, [], pos), [arg(dir, np(3+pl, np_head(quant(more, nb(2)), [], city), [rel(feature&city-G, s(np(3+sg, np_head(det(the(sg)), [], population), [prep_phrase(poss, np(3+pl, wh(feature&city-G), []))]), verb(exceed, active, pres+fin, [], pos), [arg(dir, np(3+sg, np_head(quant(same, nb(1)), [], million), []))], []))]))], []))]))], []))), 
sem((answer80([F]):-setof(A, (continent(A), \+B^ (country(B), in_ploc(B, A), E^ (numberof(C, (city(C), D^ (population(C, D), exceeds(D, 1--million)), in_ploc(C, B)), E), E>2))), F))), qplan((answer80([L]):-setof(G, (continent(G), \+H^ (country(H), in_ploc(H, G), K^ (numberof(I, (city(I), J^ (population(I, J), exceeds(J, 1--million)), in_ploc(I, H)), K), K>2))), L))), 
answers([[africa, america, antarctica, asia, australasia, europe]])],[time(0.05499999999999999)]).
must_test_801([which, are, the, largest, african, countries, ?], [ parse(whq(feature&place&country-B, s(np(3+sg, wh(feature&place&country-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sg, np_head(det(the(sg)), [sup(most, adj(large)), adj(african)], country), []))], []))), sem((answer80([A]):-B^ (setof(C:D, (country(D), area(D, C), african(D)), B), aggregate(max, B, A)))), qplan((answer80([D]):-C^ (setof(B:A, (african(A), {country(A)}, area(A, B)), C), aggregate(max, C, D)))), 
answers([sudan])],[time(0.0)]).


:-share_mp((t1/0,t12/0,t13/0)).
t1:- with_no_assertions(lmconf:use_cyc_database,with_assertions(t_l:tracing80, forall(must_test_801(U,R,O),once(ignore(must_det(process_run_diff(report,U,R,O))))))).
t12:- with_no_assertions(lmconf:use_cyc_database,with_assertions(t_l:tracing80, forall(must_test_802(U,R,O),once(ignore(must_det(process_run_diff(report,U,R,O))))))).
t13:- with_no_assertions(lmconf:use_cyc_database,with_assertions(t_l:tracing80, forall(must_test_803(U,R,O),once(ignore(must_det(process_run_diff(report,U,R,O))))))).

