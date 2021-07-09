

:- import(memoize_pos_to_db/4).
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

