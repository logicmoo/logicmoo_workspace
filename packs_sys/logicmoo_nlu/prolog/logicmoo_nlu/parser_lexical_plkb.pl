% ===================================================================
% File 'parser_plkb.pl'
% Purpose: English to KIF conversions from SWI-Prolog
% This implementation is incomplete
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'parser_ProNTo.pl' 1.0.0
% Revision:  $Revision: 1.666 $
% Revised At:   $Date: 2002/06/06 15:43:15 $
% ===================================================================

:-module(parser_lexical_plkb, [ ]).


:- set_module(class(library)).
:- set_module(base(system)).

:- use_module(parser_lexical). 
% ((nlfw(N, N, cycpos(xtWHDeterminer, _, _), Ace)/M is N+1) ==>nlfw(M, M, xclude(xtWHDeterminer,
/*
:- (prolog_load_context(reloading, true)
      -> (prolog_load_context(source, File), mpred_remove_file_support(File))
     ; true).


((nlfw(_N, _M, text80(Words), Ace)/nth0(NM, Words, W), maybe_text(W, WW))==> nlfw(NM, NM, WW, Ace)).
((do_e2c_fwd(String, ID)/text_into_wall(String, ID, Ace, WalledWords, M, N)) ==> (nlfw(M, N, ace_text(Ace), ID), nlfw(M, N, text80(WalledWords), ID))).
:- ain((nlfw(M, N, cycpos(xtPreposition, C, W), Ace)==> nlfw(M, N, xclude(xtAdverb, C, W), Ace))).
:- ain((nlfw(M, N, cycpos(xtAdjectiveGradable, C, W), Ace)==> nlfw(M, N, xclude(xtClosedClassWord, C, W), Ace))).
:- ain((nlfw(M, N, cycpos(xtAdjectiveGradable, C, W), Ace)==> nlfw(M, N, xclude(xtNoun, C, W), Ace))).
:- ain((nlfw(M, N, cycpos(xtPronoun, C, W), Ace)==> nlfw(M, N, xclude(xtDeterminer, C, W), Ace))).
:- ain((nlfw(M, N, cycpos(xtDeterminer, C, W), Ace)==> nlfw(M, N, xclude(xtAdverb, C, W), Ace))).
:- ain((nlfw(M, N, cycpos(xtDeterminer, C, W), Ace)==> nlfw(M, N, xclude(xtAdjective, C, W), Ace))).


((later_on, nlfw(M, N, txt(W), Ace)/member(Pos, [xtNoun, xtVerb, xtAdjective, xtAdverb, xtPronoun, xtPreposition, xtDeterminer]))
  ==> nlfw(M, N, maybe_pos(Pos, W), Ace)).
((nlfw(M, N, txt(W), Ace)/text_to_cycword(W, P, C, _Why)) ==> nlfw(M, N, cycWord(P, C, W), Ace)).
((nlfw(M, N, txt(W), Ace)/clex_word(P, W, C, T)) ==> nlfw(M, N, clex_word(P, clexFn(C), T, W), Ace)).
%((nlfw(M, N, cycWord(P, C, W), Ace)/cycpred_to_cycpos(P, Pos) ==> nlfw(M, N, cycpos(Pos, C, W), Ace))).
((
   nlfw(M, N, cycpos(Pos, C, W), Ace),
   \+ nlfw(M, N, xclude(Pos, C, W), Ace),
 %  nlfw(M, N, cycWord(_, C, W), Ace),
  {cyckb_lex(denotation, C, Pos, _, Subj)})==> nlfw(M, N, value(Subj, C, W), Ace)).

:- ain((nlfw(M, N, cycWord(PosL, C, W), Ace)/(pos_inherit(PosL, PosH), \+ notInheritPos(PosH)))==>nlfw(M, N, cycpos(PosH, C, W), Ace)).
:- ain((nlfw(M, N, cycpos(PosL, C, W), Ace)/(pos_inherit(PosL, PosH), \+ notInheritPos(PosH)))==>nlfw(M, N, cycpos(PosH, C, W), Ace)).
:- ain((nlfw(M, N, xclude(PosL, C, W), Ace)/(pos_inherit(PosH, PosL)))==>nlfw(M, N, xclude(PosH, C, W), Ace)).
:- ain((nlfw(M, N, xclude(Pos, C, W), Ace))==> \+ nlfw(M, N, cycpos(Pos, C, W), Ace)).
%:- ain((nlfw(M, N, xclude(Pos, C, W), Ace))==> \+ nlfw(M, N, cycWord(Pos, C, W), Ace)).
%:- ain((nlfw(M, N, cycpos(Pos, C, W), Ace))==> \+ nlfw(M, N, xclude(Pos, C, W), Ace)).
*/


:- share_mp(common_logic_kb_hooks:cyckb_t/1).
:- share_mp(common_logic_kb_hooks:cyckb_t/2).
:- share_mp(common_logic_kb_hooks:cyckb_t/3).
:- share_mp(common_logic_kb_hooks:cyckb_t/4).
:- share_mp(common_logic_kb_hooks:cyckb_t/5).
:- share_mp(common_logic_kb_hooks:cyckb_t/6).
:- share_mp(common_logic_kb_hooks:cyckb_t/7).
:- forall(between(1, 8, N), share_mp(common_logic_kb_hooks:cyckb_t/N)).


guess_strip_module(M:F,M,F):- !.
guess_strip_module(MF,M,MF):- atom(MF), functor(P,MF,2),!,guess_strip_module(P,M,_).
guess_strip_module(MF,M,MF):- predicate_module(MF,M).
guess_strip_module(MF,M,F):- strip_module(MF,M,F).

connect_preds(HMF, BMF):- 
 guess_strip_module(HMF,HM,HF),
 guess_strip_module(BMF,BM,BF),
 forall(between(1, 13, N),
 ( length(ARGS, N),
   share_mp(BM:BF/N),multifile(HM:HF/N),dynamic(HM:HF/N),
   H=..[HF|ARGS],
   B=..[BF|ARGS],
   multifile(BM:BF/N),dynamic(BM:BF/N),   
   asserta_if_new(HM:(H:- BM:B)))).


common_logic_kb_hooks:cyckb_t(A, B, C):- cyckb_p2(A, [B, C]).
common_logic_kb_hooks:cyckb_t(A, B, C, D):- cyckb_p2(A, [B, C, D]).
common_logic_kb_hooks:cyckb_t(A, B, C, D, E):- cyckb_p2(A, [B, C, D, E]).
common_logic_kb_hooks:cyckb_t(A, B, C, D, E, F):- cyckb_p2(A, [B, C, D, E, F]).

cyckb_lex1(A,B,C):- common_logic_kb_hooks:cyckb_t(A,B,C).
cyckb_lex1(A,B,C,D):- common_logic_kb_hooks:cyckb_t(A,B,C,D).
cyckb_lex1(A,B,C,D,E):- common_logic_kb_hooks:cyckb_t(A,B,C,D,E).

trace_break:- nop(dmsg(trace_break)),!.
%trace_break:- trace,break.

cyckb_lex(A,B,C):- trace_break,cyckb_lex1(A,B,C).
cyckb_lex(A,B,C,D):- trace_break,cyckb_lex1(A,B,C,D).
cyckb_lex(A,B,C,D,E):- trace_break,cyckb_lex1(A,B,C,D,E).

:- connect_preds(common_logic_kb_hooks:cyckb_t, cyckb_h).
:- connect_preds(cyckb_h, kb0988:ac).
%:- connect_preds(ac, t).

cyckb_p2(A, BC):- \+ is_list(BC), !, between(2, 10, N), length(BC, N), cyckb_p2(A, BC).
cyckb_p2(A, [B, C|D]):- atom(C), downcase_atom(C, C), cvt_to_real_string(C, S), cyckb_p3(A, B, [S|D]).
cyckb_p2(A, [B, B1, C|D]):- atom(C), downcase_atom(C, C), cvt_to_real_string(C, S), cyckb_p3(A, B, [B1, S|D]).
cyckb_p2(A, [B, C|D]):- string(C), into_text100_atoms(C, O), O\=[_], maplist(cvt_to_real_string, O, S), ST=..[s|S], cyckb_p3(A, B, [ST|D]).
cyckb_p2(A, [B, B1, C|D]):- string(C), into_text100_atoms(C, O), O\=[_], maplist(cvt_to_real_string, O, S), ST=..[s|S], cyckb_p3(A, B, [B1, ST|D]).
%cyckb_p2(A, [B, C|D]):- \+ ((arg(_, v(B, C), X), compound(X))), between(2, 10, N), functor(S, s, N), arg(_, cyckb_h(B, C), S), cyckb_p3(A, B, [C|D]).
%cyckb_p2(A, [B, C|D]):- cyckb_p3(A, B, [C|D]).

cyckb_p3(A, B, [H|T]):- apply(cyckb_h(A, B), [H|T]).


%lex_mws(genTemplateConstrained).
%lex_mws(genTemplate).
lex_mws(headMedialString).
lex_mws(compoundString).

lex_mws(prepCollocation).
lex_mws(abbreviationForMultiWordString).
lex_mws(multiWordStringDenotesArgInReln).
lex_mws(compoundSemTrans).
lex_mws(multiWordSemTrans).
lex_mws(multiWordString).
lex_mws(mws).
lex_mws(xPPCompFrameFn).
lex_mws(hyphenString).


notInheritPos(xtSententialConstituent).
notInheritPos(xtWHAdverb).
notInheritPos(xtWHWord).
notInheritPos(tIndividual).
notInheritPos(tThing).
notInheritPos(xtNLWordForm).

pos_upwards(N,xtAdjective):- member(N,[adjSemTrans,xRegularAdjFrame,xtAdjectiveGradable]).
pos_upwards(N,xtVerb):- 
 member(N,[verbSemTrans,
  verbSemTransCanonical,
  templateExpressionForVerbWRTClassAndFrame,
  sententialPhraseForVerbWithFrameGeneric,
  verbSenseGuessedFromVerbClass]).
pos_upwards(N,xtNoun):- member(N,[nounSemTrans,agentiveNounSemTrans]).
pos_upwards(PosL, PosH):- cyckb_lex(genls, PosL, PosH).



pos_inherit_u(PosL, PosH):- pennSyntacticTags(PosL, PosH).
pos_inherit_u(Pred,  Pos):- cyckb_lex(speechPartPreds, Pos, Pred).
pos_inherit_u(PosL, PosH):- pos_upwards(PosL, PosH).
pos_inherit_u(PosL, PosH):- cyckb_lex(syntacticCategoryTags,PosH,PosL).


pos_inherit_d(PosL, PosH):- cyckb_lex(syntacticCategoryTags,PosH,PosL).
pos_inherit_d(PosL, PosH):- pos_upwards(PosL, PosH).
pos_inherit_d(Pred,  Pos):- cyckb_lex(speechPartPreds, Pos, Pred).
pos_inherit_d(PosL, PosH):- pennSyntacticTags(PosL, PosH).

pos_inherit_all(Pos, Pos).
pos_inherit_all(Pred, PosO):- nonvar(PosO),!, pos_inherit_d(Mid, PosO), pos_inherit_all(Pred, Mid).
pos_inherit_all(Pred, PosO):- var(Pred),pos_list(List),!,member(PosH,[xtDeterminer,xtNoun,xtPronoun|List]), pos_inherit_all(PosO, PosH), pos_inherit_all(Pred, PosO).
pos_inherit_all(Pred, PosO):- pos_inherit_u(Pred, Pos), \+ notInheritPos(Pos), pos_inherit_all(Pos, PosO).

pos_inherit(Pred, PosO):- no_repeats(pos_inherit_all(Pred, PosO)).



extend_brillPos(In,[Out]):- freeze(cvt_to_real_string(In,Str)),cyckb_lex(pennTagString,Out,Str).
extend_brillPos('PRP$',['Possessive','xtNoun']):- !.
extend_brillPos('PRP$',['Possessive'|Rest]):- extend_brillPos('PRP',Rest).
%extend_brillPos('PRP',['SpecialDeterminer','xtDeterminer','second']).
extend_brillPos(In,Out):- bposToCPos(In,Out).
extend_brillPos(In,form(Out)):- bposToCPosForm(In,Out).
extend_brillPos(In,Out):- brillPos([In|Out]) *-> true 
 ; (freeze(In,downcase_atom(In,DC)),freeze(DC,upcase_atom(DC,In)),In\==DC,brillPos([DC|Out])).



avoid_cycword_f(talk_db).
avoid_cycword_f(clex_word).

text_to_cycinfo_hook(String, P, C, How):- text_to_cycword(String, P, C, How).

text_to_cycword(String, P, C, How):- %dmsg(text_to_cycword(String)),
 text_to_cycword0(String, P, C, How),
 nop((compound(P) -> \+ (functor(P,F,_), avoid_cycword_f(F)) ; true)).

%xc(W):- between(2,8,N),functor(P,ac,N),arg(_,P,W),call(P),fail.
xc(W):- between(2,8,N),functor(P,ac,N),call(P),(contains_dirrectly(P,W)->dmsg(P)),fail.
xc(_).

contains_dirrectly(E,W):-E==W.
contains_dirrectly(E,W):- compound(E), \+ is_list(E), compound_name_arity(E,F,_),
  (F==W ; (F\==s, arg(_,E,EE),contains_dirrectly(EE,W))).

%text_to_cycword(String, P, C, How):- !, first_clause_only(text_to_cycword(String, P, C, How)).
text_to_cycword0(String, P, C, How):- \+ string(String), cvt_to_real_string(String, RealString), !, text_to_cycword(RealString, P, C, How).
text_to_cycword0(String, P, C, Out):- base_to_cycword(String, P, C), base_to_cycword_out(String, P, C, Out).
text_to_cycword0(String, P, C, How):- string_lower(String, DCString), DCString\==String, !, text_to_cycword(DCString, P, C, How).

text_to_cycword0(String, Pos, C, (to_base_form(String, Pos, BaseWord), ac(Pred, C, BaseWord))):- fail,
  to_base_form(String, Pos, BaseWord), BaseWord\==String,
  base_to_cycword(BaseWord, Pred, C).

text_to_cycword0(String, Pos, C, ac(Pred, C, BaseWord)):-
  to_base_form(String, Pos, BaseWord), BaseWord\==String,
  base_to_cycword(BaseWord, Pred, C).

base_to_cycword_out(String, P, C, ac(P, C, String)).
base_to_cycword_out(_, P, C, Out):- cyc_mine_compat(P,C,Out),call(Out).

cyc_mine_compat(P,C,nop(cycTerm(Subj,Col))):- cycword_to_cycconcept(P, C, Subj),compat_spp(P),
  once((findall(N-Col,
   (ac(isa,Subj,Col), (dist_to_thing(Col,N) -> true; N= -1)),
   L),sort(L,LL),last(LL,_-Col))).

cyc_mine_compat(_,C,nop(fooooooooooooooooooooooooooo(P))):- between(1,4,N),length(List,N), 
   P =.. [ac,A,B,C|List], call(P), ok_pred_for_lex(A),
   once((contains_dirrectly(P,C), some_pos([A,B,C|List],Pos),compat_spp(Pos))).

ok_pred_for_lex(multiWordString):-!,fail.
ok_pred_for_lex(_).

is_pos_term(Pos):- \+ atom(Pos),!,fail.
is_pos_term(Pos):- is_pos_term0(Pos),!.
is_pos_term0(Pos):- atom_concat('xt',_,Pos).
is_pos_term0(Pos):- atom_contains(Pos,'SemTrans').
is_pos_term0(Pos):- atom_contains(Pos,'Frame').
some_pos(List,Pos):- (member(Pos,List),is_pos_term(Pos))*->true;some_pos2(List,Pos).
some_pos2(List,Pos):- member(Pos,List).
%cyc_mine_compat(P2,C,cyckb_h(_,C,P,_,_)):- cyckb_h(genls,P,P2).

dist_to_thing(Col,N):- dist_to_thing([Col],Col,N),!.
dist_to_thing(_,Col,1):- ac(genls,Col,tThing),!.
dist_to_thing(_,Col,0):- Col==tThing,!.
dist_to_thing(NotIn,Col,N):- ac(genls,Col,Super),\+ member(Super,NotIn),dist_to_thing([Super|NotIn],Super,M),N is M+1.
had_info(Info):- tmplex:had(Had),member(Info,Had).

to_base_form(String, Used, BaseWord):- \+ atom(String), string_to_atom(String, Atom), !, to_base_form(Atom, Used, BaseWord).
to_base_form(String, Used, BaseWord):- call_lex_arg_type(text(a), text(base), String, BaseWord, Used).
to_base_form(String, 'xtAgentitiveNoun', BaseWord):- morph_stem(String, BaseWord, 'er').
to_base_form(String, 'xtAdverb', BaseWord):- morph_stem(String, BaseWord, 'ly').
to_base_form(String, 'xtUn', BaseWord):- morph_stem(String, 'un', BaseWord).

morph_stem(String, Base, Suffix):- atom_concat(Base, Suffix, String).
morph_stem(String, Base, Suffix):- pronto_morph_engine:morph_atoms(String, [[Base, -Suffix]]).

base_to_cycword(String, Pos, C):- cyckb_lex(partOfSpeech, C, Pos, String), ok_speech_part_pred(Pos).
base_to_cycword(String, P, C):-
  nonvar(String), cvt_to_real_string(String, QAString), 
  base_to_cycword_string(QAString, P, C).

base_to_cycword_string(QAString, P, C):- 
  cyckb_h(P, C, QAString),
  ok_speech_part_pred(P).
base_to_cycword_string(QAString, Plural, C):- 
  string_concat(Str,"s",QAString),!,
  base_to_cycword_string(Str, Singular, C),
  s_pred(Singular,Plural).

s_pred(singular,plural).
s_pred(infinitive, thirdPersonSgPresent).
%morph_atoms(causer, [[W, -er]]). W = cause

string_to_info(String, P):- fail,
 catch(downcase_atom(String, Atom), _, fail),
 atom_length(Atom, Len), Len > 1,
 cyc_term_to_info(Atom, P). % , functor(P, F, _), guess_pred_pos(P, String, Pos).

% string_to_pos(String, Pos):- atom_ string(Atom, String), cyc_term_to_info(Atom, P), guess_pred_pos(P, String, Pos).

guess_pred_pos(P, _String, Pos):- arg(_, P, Pos), nonvar(Pos), member(Pos, [n, a, s, v, a, j, r, jj, adv, adj, nn, pp, prep]), !.
guess_pred_pos(P, String, Pos):- arg(_, P, Pos), nonvar(Pos), Pos \== String, !.
%guess_pred_pos(P, _, Pos):- functor(P, Pos, _).

ok_speech_part_pred(P):-
 P\==firstNameInitial, P\==middleNameInitial,
 (
 cyckb_h(isa, P, rtSpeechPartPredicate); \+ cyckb_h(isa, P, _)),!,
 compat_spp(P),!.

compat_spp(P):- had_info(pos(Pos)),!,check_compat_spp(P,Pos),!.
compat_spp(_).

%~ trace_break.


compat_spp(determinerStrings,cd).
compat_spp(determinerStrings,dt).
compat_spp(determinerStrings,wdt).
compat_spp(infinitive,v_).
compat_spp(pastTenseUniversal,v_).
compat_spp(plural,n_).
compat_spp(prepositionStrings,in).
compat_spp(pronounStrings,p_).
compat_spp(pronounStrings,wdt).
compat_spp(regularAdverb,in).
compat_spp(regularAdverb,wdt).
compat_spp(singular,n_).
compat_spp(xtCountNoun,n_).
compat_spp(xtDeterminerDefinite,wdt).
compat_spp(xtDeterminerIndefinite,dt).
compat_spp(xtNumberSP,cd).
compat_spp(xtPreposition,in).
compat_spp(xtPrepositionDirectionalTelic,in).
compat_spp(xtSubjectPronoun,prp).
compat_spp(xtVerb,v_).
compat_spp(xtGerundiveNoun,nn).
compat_spp(xtProperCountNoun,nns).

compat_spp(massNumber,nn).
compat_spp(nounPrep,in).
compat_spp(verbPrepTransitivetemplate,in).
compat_spp(xtGerundiveCountNoun,nn).
compat_spp(xtMassNoun,nn).

incompat_spp(hasVerbAsMember,nn).
incompat_spp(verbClassCoversVerbSense,nn).
incompat_spp(verbClassExcludesVerbSense,nn).
incompat_spp(xGetVerbClass,nns).
incompat_spp(nartR(xVNVerbClassFn,_,_),n_).

incompat_spp(hasVerbAsMember,nns).
incompat_spp(xtAdjectiveGradable,in).
incompat_spp(xtMassNoun,dt).
incompat_spp(xtGerundiveNoun,nns).
incompat_spp(infinitive,n_).
incompat_spp(pronounStrings,cd).
incompat_spp(singular,aux).
incompat_spp(singular,cd).
incompat_spp(singular,v_).
incompat_spp(thirdPersonSgPresent,n_).
incompat_spp(xtAdjectiveGradable,n_).
incompat_spp(xtAdjectiveGradable,v_).
incompat_spp(xtCountNoun,cd).
incompat_spp(xtCountNoun,v_).
incompat_spp(xtPronoun,cd).
incompat_spp(xtVerb,n_).
incompat_spp(Atom,_):- atom(Atom),atom_concat('act',_,Atom).
incompat_spp(Atom,_):- number(Atom).
incompat_spp(Atom,_):- atom(Atom),atom_concat(_,'TheWord',Atom).
incompat_spp(nartR(actMakingAvailableFn,_),nn).

compat_spp_tf(P,Pos,false):- incompat_spp(P,Pos),!.
compat_spp_tf(P,Pos,true):- compat_spp(P,Pos),!.


check_compat_spp(P,Pos):- compat_spp_tf(P,Pos,TF),!,TF==true.
check_compat_spp(P,Pos):- dash_atom(Pos,Pos_),compat_spp_tf(P,Pos_,TF),!,TF==true.
check_compat_spp(P,Pos):- wdmsg(compat_spp(P,Pos)),!,fail.

dash_atom(Pos,Pos_):- \+ atom_chars(Pos,[_,'_'|_]), atom_chars(Pos,[P|_]),atom_chars(Pos_,[P,'_']).

dont:- forall((clause(ac(_, xBadTheWord, _, _, TakingABath), true, R);clause(ac(_, xBadTheWord, _, _, _, TakingABath), true, R)),
       ignore((member(X, [actTakingABath, tGroupedSpa, tObjectHotTub]), sub_var(X, TakingABath),
        erase(R)))).

text_pos_cycword(String, MorePos, [cycWord(_WasP,C)|Out]):- 
  cvt_to_atom(String,AString),text_to_cycword(AString, P, C, How), 
  (not_violate_pos(MorePos,[P,How])
    ->Out=[P]
     ;(Out=[y_violate(How)])). 

cycword_sem(CycWord, MorePos, Out):- 
  cyc_term_to_info(CycWord, Info),
   ((true;not_violate_pos(MorePos,Info))
    ->Out=Info
     ;(Out=y_skip(violate),dmsg(y_violate(Info)))).

cycword_sem(CycWord, MorePos, Out):- fail,
  cyc_term_to_info(CycWord, Info),
  (y_skip_reason(Info,Why) -> Out=y_skip(Why); 
   (not_violate_pos(MorePos,Info)
    ->Out=Info
     ;(Out=y_skip(violate),dmsg(y_violate(Info))))).

% :- forall(cyckb_lex(mostSpeechPartPreds, B, C), retract(cyckb_lex(speechPartPreds, B, C))).
cycpred_to_cycpos(Pred, Pos):- atom(Pred), pos_inherit(Pred, M),atom(M),atom_concat(xt,_,M),M\==xtNLWordForm,
  Pred\==M,(M=Pos;cycpred_to_cycpos(M, Pos)),atom(Pos).

cycpred_to_cycpos_1(Pred, Pos):- nonvar(Pred),
 cyckb_lex(speechPartPreds, Pos, Pred), \+ cyckb_lex(mostSpeechPartPreds, Pos, _), \+ cyckb_lex(mostSpeechPartPreds, _, Pred).




filter_lex(OutS,[PennPos|MorePos],OutF):-
 include(not_violate_pos([PennPos|MorePos]),OutS,OutF).

%not_violate_pos(_,_):-!.
not_violate_pos(_MorePos,Var):- var(Var),!.
not_violate_pos(_MorePos,[]):-!.
not_violate_pos(MorePos,[H|T]):- !, not_violate_pos(MorePos,H),not_violate_pos(MorePos,T).
not_violate_pos(MorePos,OutS):- violate_pos(MorePos,OutS),!,fail.
not_violate_pos(_MorePos,_OutS).

violate_pos(MorePos,OutS):- \+ compound(OutS), !, violate_pos1(MorePos,OutS).
violate_pos(MorePos,Did):- Did =..[T, F|Rest], member(T, [acnl, cyckb_h, t, talk_db]),
 atom(F), Do =..[F|Rest], !,                                     
 violate_pos(MorePos,Do).
% violate_pos(MorePos,OutS):- functor(OutS,F,_),violate_pos1(MorePos,F),!.
violate_pos(MorePos,OutS):- functor(OutS,F,_),pos_inherit(F, Pos),violate_pos1(MorePos,Pos).
violate_pos(MorePos,OutS):- arg(_,OutS,E), atom(E), violate_pos1(MorePos,E),!.
violate_pos(MorePos,OutS):- violate_pos1(MorePos,OutS).

pos_list([xtCoordinatingConjunction,xtVerb,xtAdjective,xtAdverb,xtPreposition,xtPunctuationSP]).
         

incompatible_pos(Pos1,Pos2):- pos_list(PosList),member(Pos2,PosList),member(Pos1,PosList),Pos1\==Pos2.
incompatible_pos(Pos1,Pos2):- pos_list(PosList),member(PosA,PosList),member(PosB,[xtDeterminer,xtNoun,xtPronoun]),
  ((Pos1=PosA,Pos2=PosB);(Pos1=PosB,Pos2=PosA)).

% cyckb_h(denotation,xUseTheWord,xtMassNoun,0,actUsingAnObject)

violate_pos1(MorePos,OutS):- incompatible_pos(XtNoun,XtVerb),member(XtNoun,MorePos),pos_inherit(OutS,XtVerb),
  dmsg(incompatible_pos(XtVerb,XtNoun,MorePos)).
violate_pos1(_,todo).
violate_pos1(_,txt).
violate_pos1(_,comment).
violate_pos1(_,flexicon).
violate_pos1(_,M):- atom(M),member(M,[mws,flexicon,fsr]).
%violate_pos(MorePos,OutS,OutF).

cycword_to_cycconcept(Pred, C, Subj):- cyckb_lex(speechPartPreds, Pos, Pred), cyckb_lex(denotation, C, Pos, _, Subj).
% cycword_to_cycconcept(_P, C, Subj):- acnl(denotation, C, _, _, Subj, _).


lex_info_impl_cyc_hook(Kind, Level, [cycWord(P, CycWord)|Todo], Done, Out):-
 findall_set(concept(Subj), cycword_to_cycconcept(P, CycWord, Subj), More1),
 findall_set(Info, term_to_infolist(CycWord, Info), More2),
 add_if_new(Done, cycWord(P, CycWord), NewDone),
 add_do_more([More1, More2], Todo, NewDone, NewTodo),
 lex_info(Kind, Level, NewTodo, NewDone, Out).

lex_info_impl_cyc_hook(Kind, Level, [TOK|Todo], Done, Out):- 
 TOK = tok(Index,PennPos,_Base,String, Info), !,
 functor(TOK,_,A),
  must_or_rtrace((   
   findall_set(CPos,extend_brillPos(PennPos,CPos),CPosSet),
   POSINFO = [PennPos|CPosSet],
   append(Info,POSINFO,PropsTOK),
   nb_setarg(A,TOK,PropsTOK),
   findall_set(How, (text_pos_cycword(String, POSINFO, How)), CycWordInfo),
   nb_set_add(PropsTOK,CycWordInfo),   
   append(Done,Todo,AllInfo),
   findall_set(How, (find_coref(Index, AllInfo, How)), CorefInfo),
   nb_set_add(PropsTOK,CorefInfo),
   forall(member(cycWord(_WasPred,CycWord),PropsTOK),
   (findall_set(How, (cycword_sem(CycWord, _UnusedPropsTOK, How)), CycSem),
    nb_set_add(PropsTOK,CycSem))),
   sort(PropsTOK,PropsTOKS),
   nb_setarg(A,TOK,PropsTOKS),     
   lex_info(Kind, Level, Todo, Done, Out))).

%cyc_term_to_info(C, P):- between(2, 12, A), functor(P, cyckb_h, A), call(P), sub_term(X, P), X==C.
cyc_term_to_info_hook(Term, Info):- cyc_term_to_info(Term, Info).
%cyc_term_to_info(C, P):- ac_nl_info_1(C, Results), member(P, Results).
%cyc_term_to_info(C, P):- between(3, 12, A), functor(P, acnl, A), arg(N, P, C), N<A, N>1, call(P).
%cyc_term_to_info(Term, Info):- in_call(Term, Info, Template, cyckb_h('genTemplate', _, Template)).
%cyc_term_to_info(Term, Info):- in_call(Term, Info, Template, cyckb_h('genTemplateConstrained', _, _, Template)).
cyc_term_to_info(Term, Info):- Info=cyckb_h(_Pred, Cont), call(Info), sub_var(Term, Cont).
cyc_term_to_info(Term, Info):- Info=cyckb_h(_Pred, Term, S), call(Info), \+ string(S).
cyc_term_to_info(Term, Info):- Info=cyckb_h(_Pred, Term, _, S), call(Info), \+ string(S).
cyc_term_to_info(Term, Info):- between(5, 12, A), functor(Info, cyckb_h, A), arg(N, Info, Term), N>1, call(Info).

in_call(C, P, Template, Call):- P=Call, call(P), once(sub_var(C, Template)).

cyc_lex:- cyc_lex("I saw two books sitting on the shelf by the fire").
cyc_lex(W):- cls,debug,make,into_lexical_segs(W,X),wdmsg(X).

:- system:import(parser_lexical_plkb:cyc_lex/1).
:- system:import(cyc_lex/0).

:- multifile(check:list_undefined/1).
:- dynamic(check:list_undefined/1).
:- system:use_module(library(make)), system:use_module(library(check)), nop(redefine_system_predicate(check:list_undefined/1)).
:- asserta((check:list_undefined(Stuff):- Stuff==[], wdmsg(list_undefined(Stuff)),!)).
/*
%:- abolish(check:list_undefined/1).
:- listing(check:list_undefined).
% :- break.
*/

