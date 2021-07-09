% =================================================================
% Verb Phrase
% =================================================================

verb_phrase(Frame, X, Out) -->
  dcg_thru_2args(verb_pre_mod(X, Frame), LF, Mid),
  verb_phrase1(Frame, X, LF),
  dcg_thru_2args(verb_phrase_post_mod(X, Frame), Mid, Out).

% verb_phrase1(Frame, X, AssnOut) --> verb_phrase1(Frame, X, AssnOut).
verb_phrase1( Frame, X, ~(LFOut)) --> theText1(not), !, verb_phrase1(Frame, X, LFOut).

% verb_phrase1( Frame, X, LFOut) --> verb_phrase1_ditrans( Frame, X, LFOut).

verb_phrase1( Frame, X, AssnOut) --> is_be(Frame, X, LF, AssnOut), verb_phrase1(Frame, X, LF).

verb_phrase1( Frame, X, LFOut) --> verb_mod_surround(X, Frame, trans_verb(Frame, X, Y, Assn1), Assn1, Assn2),
    noun_phrase(obj(_K), Y, Assn2, LFOut).
verb_phrase1( Frame, X, LFOut) --> verb_mod_surround(X, Frame, intrans_verb(Frame, X, Assn1), Assn1, LFOut).

verb_phrase1(Frame, X, AssnOut) --> is_be(Frame, X, Assn, AssnOut), noun_phrase(obj(_K), X, true, Assn), !.
%verb_phrase1(Frame, X, AssnOut) --> is_be(Frame, X, AdjProp, AssnOut), adjective(X, AdjProp), !, nvd(is, Frame).
verb_phrase1(Frame, X, AssnOut) --> is_be(Frame, X, equals(X, Y) , Assn), optionalText1(equal), optionalText1(to),
   (pronoun(obj(_K), Y, Assn, AssnOut);value_obj(obj(_K), Y, Assn, AssnOut)).


verb_prep(VerbString, PrepString, TenseUniversal, DitransitivePPFrameType, LF):-
 nl_call(acnl, 'verbSemTrans', CycVerb, _, nartR('xPPCompFrameFn', DitransitivePPFrameType, CycPrep), LF, _),
 nl_call(acnl, TenseUniversal, CycVerb, VerbString, _),
 nl_call(acnl, prepositionStrings, CycPrep, PrepString, _).




scan_for_verb_prep(LeftOfVerb, AtomVerb, LeftOfPrep, AtomPrep, RightOfPrep, CycVerb, CycPrep) -->
   scan_words(LeftOfVerb, AtomVerb, RightOfVerb, TxtVerb^nl_call(acnl, Infinitive, CycVerb, TxtVerb, _)),
    {nl_call(acnl, posBaseForms, CycVerb, xtVerb, _), Infinitive=_},
    {phrase(scan_words(LeftOfPrep, AtomPrep, RightOfPrep, TxtPrep^nl_call(acnl, prepositionStrings, CycPrep, TxtPrep, _)), RightOfVerb)}.

% get_lf_for(LeftOfVerb, CycVerb, LeftOfPrep, CycPrep, RightOfPrep).

scan_words(Left, AtomWord, Right, StrWord^Goal, S, []):-
  append(Left, [AtomWord|Right], S),
  atom_string(AtomWord, StrWord),
  call(Goal).



verb1(V) --> theText1(Verb), {into_active1(Verb, Active), to_evt_name(Active, V)}.

%ditrans_verb1(Frame, TenseUniversal, X, Y, Z, LF) --> theText1(W), {atom_string(W, S),


% =================================================================
% Ditrans Verb
% =================================================================
ditrans_verb(Frame, X, Y, Z, LFO) --> ditrans_verb1(Frame, Time, X, Y, Z, LF),
% OLD
  {make_time_info(Frame, Time, LF, LFO)}.
% NEW expand_lf(iza(Frame, timeFn(Time)) & LF, LFO)}.



% paits her a picture
ditrans_verb1(_Frame, pres+fin, X, Y, Z, doesAgentRecipientSomething(paints, X, Y, Z)) --> theText1(paints).
ditrans_verb1(_Frame, pres+fin, X, Y, Z, doesAgentRecipientSomething(gave, X, Y, Z)) --> theText1(gave).
ditrans_verb1( Frame, pres+fin, X, Y, Z, LFOut) --> named_var_match(startsWith('DTV'), Var, holdsIn(Frame, z(Var, X, Y, Z)), LFOut).
ditrans_verb1(Frame, Time, X, Y, Z, LF) --> talk_verb(Frame, IV, dtv(X, Y, Z), Time, LF), nvd(IV, Frame).  % & iza(Frame, timeFn(Time)

   talk_verb_LF(Frame, dtv(X, Y, Z), Write, Writes, Wrote, Written, Writing,
                                            ( iza(Frame, a(event, Writing))
                                             & doer(Frame, X)
                                             & t([Writing, "To"], Frame, Y)
                                             & t(["object", Written], Frame, Z))) :-
     talk_db_safer(ditransitive, Write, Writes, Wrote, Writing, Written).


  talk_verb_LF(_Frame, dtv(X, Y, Z), write, writes, wrote, written, writing, doesAgentRecipientSomething(writes, X, Y, Z)).

verb_phrase1_ditrans( Frame, X, verbPrep(Frame, X, LeftOfVerb, CycVerb, LeftOfPrep, CycPrep, RightOfPrep)) -->
  {fail}, scan_for_verb_prep(LeftOfVerb, _AtomVerb, LeftOfPrep, _AtomPrep, RightOfPrep, CycVerb, CycPrep).

% NEW
verb_phrase1_ditrans( Frame, X, LFOut) --> verb_mod_surround(X, Frame, ditrans_verb(Frame, X, Y, Z, Assn1), Assn1, Assn2),
  noun_phrase(obj(dir), Y, Assn2, Assn3),
  noun_phrase(obj(indir), Z, Assn3, LFOut).
%verb_phrase1( Frame, X, LFOut) --> verb_mod_surround(X, Frame, trans_verb(Frame, X, Y), true, Assn2), noun_phrase(obj(_K), Y, Assn2, LFOut).
%verb_phrase1( Frame, X, LFOut) --> verb_mod_surround(X, Frame, intrans_verb(Frame, X), true, LFOut).
% OLD


% =================================================================
% Trans Verb
% =================================================================
trans_verb(Frame, X, Y, LFO) --> trans_verb1(Frame, Time, XY, YX, LF), ((theText1(by;of), {Y=XY, X=YX});([], {X=XY, Y=YX})), !,
% OLD
{make_time_info(Frame, Time, LF, LFO)}.
% NEW {expand_lf(iza(Frame, timeFn(Time)) & LF, LFO)}.

trans_verb1(Frame, Time, X, Y, LF)--> trans_verb2(Frame, Time, X, Y, LF), \+ dcg_peek(theText1(by)).

trans_verb2( Frame, pres+fin, X, Y, LFOut) --> theText1([wants, to]),
  add_traits(Y, iza(Y, 'evtState'), holdsIn(Frame, z(actWantingTo, X, Y)), LFOut).



trans_verb2( Frame, pres+fin, X, Y, holdsIn(Frame, z(painting, X, Y))) --> theText1(paints).
trans_verb2( Frame, pres+fin, X, Y, holdsIn(Frame, z(admiring, X, Y))) --> theText1(admires).
trans_verb2( Frame, pres+fin, X, Y, LFOut) --> named_var_match(
                        [endsWith('ing'), endsWith('Event'), startsWith('V'), startsWith('TV')],
                                                                        Var, holdsIn(Frame, z(Var, X, Y)), LFOut).

trans_verb2( Frame, Time, X, Y, LF) --> talk_verb(Frame, IV, tv(X, Y), Time, LF), nvd(IV, Frame).  % & iza(Frame, timeFn(Time)

trans_verb2( Frame, Type, X, Y, holdsIn(Frame, z(ActVerbing, X, Y))) --> 
  theText1(Formed),
    { my_clex_verb(Formed, _Verb, tv, Type, ActVerbing)}.


my_clex_verb(Formed, Verb, Tv, Type, ActVerbing):- clex_verb(Formed, Verb, Tv, Type), to_evt_name(Verb, ActVerbing).


talk_db_safer(Transitive, Write, Writes, Wrote, Writing, Written):- 
  boosted_call(talkdb:talk_db(Transitive, Write, Writes, Wrote, Writing, Written)).

% helper for talk_verb_LF/. . .
talk_verb(Frame, IV, Type, Mod, LF) --> theText1(IV),
 {boosted_call(form_talk_verb(Frame, IV, Type, Mod, LF, Root)), put_attr(Frame, '$root', Root)}.


boosted_order(0, [pp,past+fin,infpl]).

boosted_call(Call):-
  findall(Call,Call,Possibles),
  predsort(boosted_sort,Possibles,PossiblesSorted),
  member(Call,PossiblesSorted).

% @TODO
boosted_sort(E1,E2,RO):- E1=@=E2, !, RO = (=).
boosted_sort(E1,E2,RO):- 
  boosted_value(E1,V1), 
  boosted_value(E2,V2),
  %wdmsg(compare(V1,V2)),
  compare(V1,V2,RO), RO \== (=),!.

boosted_value(E1,V1):- boosted_order(Prio,List), once((nth0(Nth,List,E), sub_term(S,E1),S==E, V1 is (Prio * 10 + Nth))).
boosted_value(V1,V1).

form_talk_verb(Frame, IV, Type, past+part, LF, IW ) :- talk_verb_LF(Frame, Type, IW, _, _, IV, _, LF).
form_talk_verb(Frame, IV, Type, past+fin, LF, IW ) :- talk_verb_LF(Frame, Type, IW, _, IV, _, _, LF).
form_talk_verb(Frame, IV, Type, pres+part, LF, IW ) :- talk_verb_LF(Frame, Type, IW, _, _, _, IV, LF).
form_talk_verb(Frame, IV, Type, pres+fin, LF, IW ) :- talk_verb_LF(Frame, Type, IW, IV, _, _, _, LF).
form_talk_verb(Frame, IV, Type, nonfinite, LF, IV ) :- talk_verb_LF(Frame, Type, IV, _, _, _, _, LF).
form_talk_verb(Frame, IV, dtv(X, Y, Z), Type, holdsIn(Frame, z(ActVerbing, X, Y, Z)), Verb):- my_clex_verb(IV, Verb, dt, Type, ActVerbing).
form_talk_verb(Frame, IV, tv(X, Y), Type, holdsIn(Frame, z(ActVerbing, X, Y)), Verb):- my_clex_verb(IV, Verb, tv, Type, ActVerbing).
form_talk_verb(Frame, IV, iv(X), Type, holdsIn(Frame, z(ActVerbing, X, _)), Verb):- my_clex_verb(IV, Verb, iv, Type, ActVerbing).


/*nlac(verbSemTrans, xGiveTheWord, 0, nartR(xPPCompFrameFn, ttTransitivePPFrameType, xOffTheWord),
  and(iza('ACTION', eventEmittingAnObject), objectEmitted('ACTION', 'OBLIQUE-OBJECT'), emitter('ACTION', 'SUBJECT')), 764838).
*/

%                           nonfinite, pres+fin, past+fin, past+part, pres+part, LF
 talk_verb_LF(_Frame, tv(X, Y), meet, meets, met, met, meeting, z(meeting, X, Y)).
 talk_verb_LF(_Frame, tv(X, Y), concern, concerns, concerned, concerned, concerning, z(concerning, X, Y)).
 talk_verb_LF(_Frame, tv(X, Y), run, runs, ran, run, running, z(running, X, Y)).
%talk_verb_LF(_Frame, tv(X, Y), write, writes, wrote, written, writing, z(writing, X, Y)).
% OLD
 talk_verb_LF( Frame, tv(X, Y), Write, Writes, Wrote, Written, Writing,
                                           iza(Frame, ProperEvent) & doer(Frame, X) & MadeObj) :-
   talk_db_safer(transitive, Write, Writes, Wrote, Writing, Written),
   to_evt_name(Writing, ProperEvent),
   make_object(Frame, Written, Y, MadeObj).

make_object(Frame, Written, Y, t(["object", Written], Frame, Y)).

% NEW
/*
  talk_verb_LF( Frame, tv(X, Y), Write, Writes, Wrote, Written, Writing,
                        talk_db_safer(transitive, Write, Writes, Wrote, Writing, Written).
*/
% =================================================================
% Intrans Verb
% =================================================================
intrans_verb(Frame, X, LFO) --> intrans_verb1(Frame, Time, X, LF),
% OLD
  {make_time_info(Frame, Time, LF, LFO)}.
% NEW expand_lf(iza(Frame, timeFn(Time)) & LF, LFO)}.

intrans_verb1(Frame, Time, X, LF)--> intrans_verb2(Frame, Time, X, LF), nop( \+  dcg_peek(theText1(by))).

intrans_verb2(_Frame, pres+fin, X, z(painting, X)) --> theText1(paints).
intrans_verb2(_Frame, past+fin, X, z(writting, X, _)) --> theText1(wrote).
intrans_verb2( Frame, pres+fin, X, LFOut) --> named_var_match(startsWith('IV'), Var, holdsIn(Frame, z(Var, X, _Y)), LFOut).
intrans_verb2(_Frame, Type, X, z(ActVerbing, X, _)) --> theText1(Formed), { my_clex_verb(Formed, _Verb, iv, Type, ActVerbing)}.

intrans_verb2(Frame, Time, X, LF) --> talk_verb(Frame, IV, iv(X), Time, LF), nvd(IV, Frame).
% fallback
intrans_verb2(Frame, Time, X, LF) --> talk_verb(Frame, IV, tv(X, _), Time, LF), nvd(IV, Frame).

%                nonfinite, pres+fin, past+fin, past+part, pres+part, LF


talk_verb_LF(_Frame, iv(X), halt, halts, halted, halted, halting, z(halting, X)).
%OLD
talk_verb_LF(Frame, iv(X), Write, Writes, Wrote, Written, Writing,
    iza(Frame, ProperEvent)
    & doer(Frame, X)) :-
    to_evt_name(Writing, ProperEvent),
    talk_db_safer(intransitive, Write, Writes, Wrote, Writing, Written).
%NEW
/*
 talk_verb_LF(Frame, iv(X), Write, Writes, Wrote, Written, Writing,
   iza(Frame, a(event, Writing))
   & doer(Frame, X)) :-
                  talk_db_safer(intransitive, Write, Writes, Wrote, Writing, Written).
*/




% =================================================================
% Infinitival Verbs % @TODO
% =================================================================
talk_verb_LF(_Frame, infinitival(X, Y), want, wants, wanted, wanted, wanting, 
  ((z(wanting, Y, X, Comp) & LFOut) & Comp & LFOut )).

%semantics is partially execution of
% NP ^VP ^Y ^NP(X want(Y, X, VP(X)))
%((X^ '`'(want(Y, X, Comp)))^LFOut) ^(X^Comp) ^Y ^LFOut, % form of VP required:
%infinitival).

% =================================================================
% Auxilary Forms
% =================================================================
aux(Form, LFIn, LFOut) --> theText1(Aux), {aux_lf(Aux, Form, LFIn, LFOut)}.
 aux_lf(to , infinitival/nonfinite , VP, VP).
 aux_lf(does , _Tense+fin/nonfinite , VP, VP).
 aux_lf(did , _Tense+fin/nonfinite , VP, VP).
 aux_lf(to , _/_ , VP, VP).

is_be( Frame, X, LF, will(LFO)) --> theText1(will), !, is_be1( Frame, X, LF, LFO).
is_be( Frame, X, LF, ~will(LFO)) --> theText1(wont), !, is_be1( Frame, X, LF, LFO).
is_be( Frame, X, LF, possible(LFO)) --> theText1(can), !, is_be1( Frame, X, LF, LFO).
is_be( Frame, X, LF, ~possible(LFO)) --> theText1(cant), !, is_be1( Frame, X, LF, LFO).

is_be( Frame, X, NounProp, NounPropO) --> is_be1( Frame, X, NounProp, NounPropM),
  (theText1(not) -> {NounPropO = ~(NounPropM)} ; {NounPropO = NounPropM}).


is_be1(_Frame, _X, NounProp, NounProp ) --> theText1(is);theText1(be);theText1(am).
is_be1(_Frame, X, NounProp, NounProp & traits(X, pl)) --> theText1(are).
is_be1(_Frame, _X, NounProp, holdsIn('vPast', NounProp)) --> theText1(was).
is_be1(_Frame, X, NounProp, holdsIn('vPast', NounProp & traits(X, pl))) --> theText1(were).

copula_is_does --> theText1(C), {copula_is_does_dict(C)}.

  copula_is_does_dict(is).
  copula_is_does_dict(does).

% =================================================================
% Prepostional Phrase / Verb Satellites
% =================================================================
verb_phrase_post_mod(X, Frame, LFIn, LFOut) -->  prepositional_phrase(obj(indir), X, Frame, LFIn, LFOut).

verb_phrase_post_mod(X, Frame, LFIn, LFIn & LFOut) --> optionalText1(', '), theText1('and'), verb_phrase(_Frame2, X, LFOut), {put_attr(Frame, '$frame_conjunction', and)}.
verb_phrase_post_mod(X, Frame, LFIn, LFIn;LFOut) --> optionalText1(', '), theText1('or'), verb_phrase(Frame, X, LFOut), {put_attr(Frame, '$frame_conjunction', or)}.
verb_phrase_post_mod(X, Frame, LFIn, conj(Frame, LFIn, LFOut)) --> theText1(', '), verb_phrase(_Frame2, X, LFOut).

prepositional_phrase(_SO, X, _Frame, LF, TAG & LF) --> tag(X, prep_phrase, TAG), !.
prepositional_phrase(SO, X, Frame, LF, Out) --> theText1(Prep), {prep_dict(Prep), ok_prep(Prep)},
  {get_attr(Frame, '$root', Root) % , freeze(Y, (wdmsg(y(Y)), dumpST))
  },
  noun_phrase(SO, Y, p(c(Root, Prep), X, Y) & LF, Out).
prepositional_phrase(SO, X, _Frame, LF, Out) --> theText1(Prep), {prep_dict(Prep), ok_prep(Prep)},
  noun_phrase(SO, Y, p(Prep, X, Y) & LF, Out).

prepositional_phrase(SO, X, _Frame, LF, Out) --> theText1(about), noun_phrase(SO, Y, about(X, Y) & LF, Out).

   prep_dict(to).
  % prep_dict(X):- loc_pred_prep_db(X, _, _).
   prep_dict(X):- talkdb:talk_db(preposition, X), X \== a.

   ok_prep(M):- M\==a.


% OLD
verb_mod_surround(X, Frame, Verb, In, Out) -->
  dcg_thru_2args(verb_pre_mod(X, Frame), In, Mid),
  Verb,
  dcg_thru_2args(verb_post_mod(X, Frame), Mid, Out).
/*
% NEW
verb_mod_surround(X, Frame, Verb, In, Out) -->
  dcg_thru_2args(verb_pre_mod(X, Frame), In, Mid),
  dcg_call(Verb, VerbMid),
  dcg_thru_2args(verb_post_mod(X, Frame), VerbMid, VerbPost),
  conjoin_lf(Mid, VerbPost, Out).


*/


% quickly <jumped>
verb_pre_mod(_, Frame, LF, Out) --> adverb(Frame, MProps), conjoin_lf(LF, MProps, Out).

% <jumped> quickly
verb_post_mod(_, Frame, LF, Out) --> {fail}, adverb(Frame, MProps), conjoin_lf(LF, MProps, Out).
verb_post_mod(X, Frame, LFIn, FLOut) -->  prepositional_phrase(obj(indir), X, Frame, LFIn, FLOut).

% adverb(X, MProps) --> quietly(maybe_negated_dcg(adverb1(X), MProps)).
adverb(X, MProps) --> quietly(adverb1(X, MProps)).
adverb1(X, MProps) --> named_var_match(startsWith('ADV'), Var, iza(X, Var), MProps).
adverb1(X, MProps)  -->      theText1(Adv), {adv_lf(X, Adv, MProps)}.

adv_lf(X, Adv, ISA) :- is_really_adv(Adv, RAdv), into_isa3(X, advFn(RAdv), ISA).

is_really_adv(Adv):- if_defined(parser_chat80:comp_adv_lex(Adv)).
is_really_adv(Adv):- if_defined(parser_chat80:sup_adv_lex(Adv)).
is_really_adv(Adv):- talkdb:talk_db(adv, Adv).
%is_really_adv(Adv):-  if_defined(parser_chat80:adverb_lex(Adv)).

is_really_adv(Adv, RAdv):- clex_iface:clex_adv(Adv, RAdv, _).
%is_really_adv(Adv, RAdv):- talkdb:talk_db(adv, RAdv, Adv).
is_really_adv(Adv, RAdv):- is_really_adv(Adv), RAdv= Adv.



to_evt_name(A, O):- \+ atom(A), toPropercase(A, O) , !.
to_evt_name(A, _Out):- assertion(atom(A)), fail.
to_evt_name(active, 'ActiveEvent').
to_evt_name(passive, 'PassiveEvent').
to_evt_name(A, Out):- is_captitalized(A), atom_concat(_, 'Event', A), !, A=Out.
to_evt_name(A, Out):-atom_contains(A, 'act'), atom_contains(A, 'ing'), Out = A.
to_evt_name(A, Out):- atom_concat('act', APC, A), !, (is_captitalized(APC)-> A=Out ; to_evt_name2(APC, Out)).
to_evt_name(A, Out):- atom_concat(APC, 'ing', A), !, (is_captitalized(APC)-> A=Out ; to_evt_name2(APC, Out)).
to_evt_name(A, Out):- to_evt_name2(A, Out).

into_active(A, ING):- (into_active1(A, ING);into_active2(A, ING)), !.
into_active1(A, ING):- talk_db_safer(_, A, _, _, ING, _), !.
into_active1(A, ING):- talk_db_safer(_, _, A, _, ING, _), !.
into_active1(A, ING):- talk_db_safer(_, _, _, A, ING, _), !.
into_active1(A, ING):- talk_db_safer(_, _, _, _, ING, A), !.
into_active1(A, ING):- talk_db_safer(_, _, _, _,   A, _), !, A = ING.
into_active1(A, ING):- talkdb:talk_db(noun_or_verb, _, ING, A).
into_active1(A, ING):- atom_contains(A, 'ing'), A = ING.
into_active2(A, ING):- talkdb:talk_db(noun_or_verb, A, ING, _).

to_evt_name2(A, Out):- atom_contains(A, 'act'), atom_contains(A, 'ing'), Out = A.
to_evt_name2(A, Out):- downcase_atom(A, DC), into_active(DC, ING), first_char_to_upper(ING, APC), atomic_list_concat(['act', APC], Out), !.
to_evt_name2(A, Out):- downcase_atom(A, DC), A==DC, first_char_to_upper(A, APC), atomic_list_concat(['act', APC, 'ing'], Out), !.
to_evt_name2(A, Out):- first_char_to_upper(A, APC), atomic_list_concat(['act', APC, 'ing'], Out), !.
% into_split_o(prep_phrase).

