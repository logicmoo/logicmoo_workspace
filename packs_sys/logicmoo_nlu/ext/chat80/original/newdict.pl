/*
 _____________________________________
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
|_____________________________________|

*/
:- [chatops].
% =================================================================
% General Dictionary

ag_number(1,sg).
ag_number(N,_):- bind_pos('value',N).
ag_number(N,pl) :- number(N), N>1.

chk_word(Word):- word80(Word).

word80(Word) :- '`' (Word).
word80(Word) :- conj_lex(Word).
word80(Word) :- adverb_lex(Word).
word80(Word) :- sup_adj_lex(Word,_).
word80(Word) :- comp_adj_lex(Word,_).
word80(Word) :- adj_lex(Word,_).
word80(Word) :- name_LF(Word).
word80(Word) :- terminator_lex(Word,_).
word80(Word) :- pers_pron_lex(Word,_,_,_,_).
word80(Word) :- poss_pron_lex(Word,_,_,_).
word80(Word) :- wh_rel_pron_lex(Word,_).
%word80(Word) :- noun_form_lex(Word,_,_).
word80(Word) :- quantifier_pron_lex(Word,_,_).
word80(Word) :- number_lex(Word,_,_).
word80(Word) :- det_lex(Word,_,_,_).
word80(Word) :- wh_art_lex(_,Word,_,_,_).
word80(Word) :- wh_pron_lex(Word,_).
word80(Word) :- loc_pred_lex(_,Word,_).
word80(Word) :- verb_type_db(chat80,Word,_).
word80(Word) :- word81(Word).

word81(Word) :- verb_form_aux(Word,_,_,_).
word81(Word) :- prep_db(chat80,Word).

word(Word) :- word80(Word).
word(Word) :- verb_form_lex(Word,_,_,_).
word(Word) :- prep_lex(Word).

'`' how.
'`' whose.
'`' there.
'`' of.
'`' ('''').
'`' (',').
'`' s.
'`' than.
'`' at.
'`' the.
'`' not.
'`' (as).
'`' that.
'`' less.
'`' more.
'`' least.
'`' most.
'`' many.
'`' where.
'`' when.
conj_lex(and).
conj_lex(or).

det_lex(a,_Sg,a,indef).
det_lex(all,pl,all,indef).
det_lex(an,sg,a,indef).
det_lex(any,_,any,indef).
det_lex(each,sg,each,indef).
det_lex(every,sg,every,indef).
det_lex(no,_,no,indef).
det_lex(some,_,some,indef).
det_lex(the,No,the(No),def).
det_lex(this,No,the(No),def).
det_lex(that,No,the(No),def).


wh_art_lex(Kind,what,X,_,wh_det(Kind,X)).
wh_art_lex(Kind,which,X,_,wh_det(Kind,X)).

wh_pron_lex(what,undef).
wh_pron_lex(which,undef).
wh_pron_lex(hoo,subJ(_ArgInfo1)).
wh_pron_lex(whom,compl).


ace_varname(Name) :- upcase_atom(Name,Name), \+ downcase_atom(Name,Name), Name\=='I',Name\=='A'.
% name_LF(Name) :- ace_varname(Name).
name_LF(Name) :- bind_pos('object',Name).
name_LF(Name) :- name_template_LF(Name,_).

number_lex(W,I,PlOrSg) :- 
        tr_number(W,I),
        ag_number(I,PlOrSg).

pers_pron_lex(he,masc,3,sg,subJ(_ArgInfo1)).
pers_pron_lex(her,fem,3,sg,compl(_)).
pers_pron_lex(him,masc,3,sg,compl(_)).
pers_pron_lex(i,_,1,sg,subJ(_ArgInfo1)).
pers_pron_lex(it,neut,3,sg,_).
pers_pron_lex(me,_,1,sg,compl(_)).
pers_pron_lex(myself,_,1,sg,_).
pers_pron_lex(she,fem,3,sg,subJ(_ArgInfo1)).
pers_pron_lex(them,_,3,pl,compl(_)).
pers_pron_lex(they,_,3,pl,subJ(_ArgInfo1)).

pers_pron_lex(them,_,3,pl,subJ(_ArgInfo1)).
pers_pron_lex(us,_,1,pl,compl(_)).
pers_pron_lex(we,_,1,pl,subJ(_ArgInfo1)).
pers_pron_lex(you,_,2,Sg,_):- pl_or_sg(Sg).
pers_pron_lex(yourself,_,2,sg,_).
pers_pron_lex(yourselves,_,2,pl,_).
pers_pron_lex(A,B,C,D):- det_pron_lex(A,B,C,D).

det_pron_lex(those,neut,3,pl,subJ(_ArgInfo1)).
det_pron_lex(that,neut,3,sg,_).
det_pron_lex(this,neut,3,sg,_).

pl_or_sg(sg).
pl_or_sg(pl).

poss_pron_lex(her,fem,3,sg).
poss_pron_lex(his,masc,3,sg).
poss_pron_lex(its,neut,3,sg).
poss_pron_lex(my,_,1,sg).
poss_pron_lex(our,_,1,pl).
poss_pron_lex(their,_,3,pl).
poss_pron_lex(your,_,2,_).

prep_lex(X):- prep_db(chat80,X).
prep_lex(X):- try_lex(prep_db(X)).

prep_db(chat80,as).
prep_db(chat80,at).
prep_db(chat80,by).
prep_db(chat80,from).
prep_db(chat80,in).
prep_db(chat80,into).
prep_db(chat80,of).
prep_db(chat80,on).
prep_db(chat80,through).
prep_db(chat80,to).
prep_db(chat80,with).

prep_db(talkdb,X):- talkdb:talk_db(preposition, X), X\==a.

quantifier_pron_lex(anybody,any,person).
quantifier_pron_lex(anyone,any,person).
quantifier_pron_lex(anything,any,thing).
quantifier_pron_lex(everybody,every,person).
quantifier_pron_lex(everyone,every,person).
quantifier_pron_lex(everything,every,thing).
quantifier_pron_lex(nobody,no,person).
quantifier_pron_lex(nothing,no,thing).
quantifier_pron_lex(somebody,some,person).
quantifier_pron_lex(someone,some,person).
quantifier_pron_lex(something,some,thing).

% NEW TRY regular_past_lex(Had,Have):- try_lex(regular_past_db(Had,Have)).

%regular_past_db(chat80,had,have(_MODAL)).

%superceeded regular_past_db(chat80,contained,contain).
%superceeded regular_past_db(chat80,exceeded,exceed).

%superceeded regular_past_db(chat80,governed,govern).

%superceeded regular_pres_lex(V):- % superceeded regular_pres_db(chat80,V).

% superceeded regular_pres_db(chat80,contain).
% superceeded regular_pres_db(chat80,exceed).


% superceeded regular_pres_db(chat80,govern).


% superceeded regular_pres_db(chat80,do(_MODAL)).
% superceeded regular_pres_db(chat80,have(_MODAL)).

wh_rel_pron_lex(which,undef).
%wh_rel_pron_lex(who,subJ(_ArgInfo1)).
wh_rel_pron_lex(whom,compl).

% wordt niet gebruikt:
root_form(1+pl).
root_form(1+sg).
root_form(2+_).
root_form(3+pl).

terminator_lex(!,!).
terminator_lex(.,_).
terminator_lex(?,?).

tr_number(eight,8).
tr_number(five,5).
tr_number(four,4).
tr_number(I,N):- atomic(I), atom_number(I,N),!.
tr_number(nine,9).
tr_number(one,1).
tr_number(seven,7).
tr_number(six,6).
tr_number(ten,10).
tr_number(three,3).
tr_number(two,2).
tr_number(X,X):- bind_pos('value',X).

ctx_pron_lex(in,place,where).
ctx_pron_lex(at,time,when).
ctx_pron_lex(because,condition,why).
ctx_pron_lex(poss,agent,who).
%ctx_pron_lex(isa,type,what).
ctx_pron_lex(cp(by,how),manner,how).

% prepositions of time, place, movement, manner, agent, measure, source and possession.

how_many_lex([how,many]).


show_tries_except(_Which,_TF,_G):- \+ tracing,!.
show_tries_except(Which,TF,G):- !, forall((available_lexicon(_,Other),Other\==Which,get_lex_call(Other,G,CALL),clause(CALL,_)),warn_when(TF,CALL)).
warn_when(fail,G):- G -> true ; wdmsg(warn_when(failed,G)).
warn_when(true,G):- G *-> wdmsg(warn_when(succeeded,G)) ; true.
get_lex_call(How, G, CALL):- G=..[F|ARGS], CALL=..[F,How|ARGS].

try_lex(G):- try_first_lex(G).

try_first_lex(G):- first_lexicon(Which),try_only_lex(Which,G).
first_lexicon(X):- available_lexicon(_,X).

try_all_lex(G):- available_lexicon(_,Which),try_one_lex(Which,G).
available_lexicon(1,talkdb).
available_lexicon(2,chat80).
available_lexicon(3,clex).

try_lex_order(Order,G):-  member(Which,Order), try_one_lex(Which,G).

try_one_lex(Which,G):- get_lex_call(Which,G,CALL),call(CALL).

try_only_lex(Which,G):- get_lex_call(Which,G,CALL),
 copy_term(G,CopyG),
 (CALL
    *-> show_tries_except(Which,fail,CopyG)
    ; (show_tries_except(_,true,CopyG),!, fail)).


%correct_root(do(MODAL),do(MODAL)).
correct_root(R,R).

verb_form_wlex(L,W,RootVerb,Tense,Agmt):-
  verb_form_wlex0(L,W,RootVerb,Tense,Agmt),
  nop(wdmsg(verb_form_wlex(W,RootVerb,Tense,Agmt))).

verb_form_wlex0(_,A,B,C,D):- verb_form_lex(A,B,C,D), !.
verb_form_wlex0(L,_,Root,_,_):- is_list(L),member(pos(V),L),atom_concat('vb',_,V),member(root(Root0),L),!,correct_root(Root0,Root).

% BE
verb_form_aux(am,be(_MODAL),pres+fin,1+sg).
verb_form_aux(as,be(_MODAL),pres+fin,3+_).
verb_form_aux(are,be(_MODAL),pres+fin,2+sg).
verb_form_aux(are,be(_MODAL),pres+fin,_+pl).
verb_form_aux(been,be(_MODAL),past+part,_).
verb_form_aux(be,be(_MODAL),inf,_).
verb_form_aux(being,be(_MODAL),pres+part,_).
verb_form_aux(is,be(_MODAL),pres+fin,3+sg).
verb_form_aux(was,be(_MODAL),past+fin,1+sg).
verb_form_aux(was,be(_MODAL),past+fin,3+sg).
verb_form_aux(were,be(_MODAL),past+fin,2+sg).
verb_form_aux(were,be(_MODAL),past+fin,_+pl).

% CAN
verb_form_aux(can,do(_),_,_).

% DO
verb_form_aux(do,do(_MODAL),pres+fin,_+pl).
verb_form_aux(did,do(_MODAL),past+fin,_).
verb_form_aux(does,do(_MODAL),pres+fin,3+sg).
verb_form_aux(doing,do(_MODAL),pres+part,_).
verb_form_aux(done,do(_MODAL),past+part,_).

/*
verb_form_aux(will,will,pres+fin,3+sg).
verb_form_aux(would,will,past+fin,_).

verb_form_aux(could,can,past+fin,_).
verb_form_aux(can,can,pres+fin,3+sg).
*/

% HAVE
verb_form_aux(has,have(_MODAL),pres+fin,3+sg).
verb_form_aux(have,have(_MODAL),pres+fin,_+pl).
verb_form_aux(having,have(_MODAL),pres+part,_).
verb_form_aux(had,have(_MODAL),past+part,_).

%verb_form_aux(A,B,C,D):- modal_verb_form_aux(A,B,C,D).

verb_form_lex(A,B,C,_):- verb_form_aux(A,B,C,_).
%verb_form_lex(A,B,C,D):- modal_verb_form_aux(A,B,C,D).
verb_form_lex(A,B,C,D):- try_lex(verb_form_db(A,B,C,D)), \+ avoided_verb(A).

avoided_verb(A):- var(A),!, freeze(A,avoided_verb(A)).
avoided_verb(A):- clause(modal_verb_form_aux(A,_,_,_),true).
avoided_verb(A):- clause(verb_form_aux(A,_,_,_),true).

% TODO FIX THESE
modal_verb_form_aux(shall,will,pres+fin,3+sg).
modal_verb_form_aux(will,will,pres+fin,3+sg).
modal_verb_form_aux(would,will,past+fin,_).
modal_verb_form_aux(wont,[will,not],past+fin,_).
modal_verb_form_aux(should,should,pres+fin,3+sg).
modal_verb_form_aux(must,should,pres+fin,3+sg).
modal_verb_form_aux(may,might,pres+fin,3+sg).
modal_verb_form_aux(might,might,pres+fin,3+sg).
modal_verb_form_aux(possibly,can,pres+fin,3+sg).
modal_verb_form_aux(could,can,past+fin,_).
modal_verb_form_aux(can,can,pres+fin,3+sg).
modal_verb_form_aux(cannot,[can,not],pres+fin,3+sg).
%modal_verb_form_aux(not,not,_+_,_+_).

maybe_apply_modal(_W,Modal9,_ModalInfo,RootVerb):- 
   ignore((compound(RootVerb),
      (arg(1,RootVerb,Modal9)->true;nb_arg(1,RootVerb,Modal9)))).
/*
verb_form_db(chat80,containing,contain,pres+part,_).
verb_form_db(chat80,contains,contain,pres+fin,3+sg).
verb_form_db(chat80,exceeding,exceed,pres+part,_).
verb_form_db(chat80,exceeds,exceed,pres+fin,3+sg).
verb_form_db(chat80,governing,govern,pres+part,_).
verb_form_db(chat80,governs,govern,pres+fin,3+sg).
*/

%verb_form_lex(Are,Be,PresFin,NthPlOrSing):-  use_lexicon_80(chat80), verb_form_db(chat80,Are,Be,PresFin,NthPlOrSing).
%verb_form_lex(Verb,Verb,pres+fin,_+pl) :- 
% NEW TRY 
verb_form_db(chat80,Verb,Verb,pres+fin,_+pl) :-  Verb = V, verb_type_lex(V,_).
% ... because [which,countries,border,france,?] was not properly parsed (the singular form was)
% NEW TRY 
%verb_form_lex(Verb,Verb,inf,_) :-  Verb = V, verb_type_lex(V,_).
% ... because [does,france,border,belgium,?] was not properly parsed
% NEW TRY verb_form_lex(Verb,Inf,past+part,_) :- use_lexicon_80(chat80_extra), regular_past_lex(Verb,Inf).
% ... because [is,france,bordered,by,belgium,?] was not properly parsed. Deduced from verb_form_db(chat80,done,do(_MODAL),past+part,_) bellow.
%verb_form_lex(A,A,C,D) :-
%  writef("********************************** verb_form_db {0} failed", [[A,A,C,D]]).
%  !,
%  fail.

%verb_root_lex(Root):- use_lexicon_80(chat80), %verb_root_db(chat80,Root).
%  verb_type_db(chat80,Root,_MainTv).
%verb_root_lex(be(_MODAL)).
%verb_root_lex(do(_MODAL)).
%verb_root_lex(have(_MODAL)).


%superceeded verb_root_db(chat80,contain).
%superceeded verb_root_db(chat80,exceed).


%superceeded verb_root_db(chat80,govern).

verb_type_lex(be(MODAL),aux+be(MODAL)).
verb_type_lex(do(_MODAL),aux+dv(_Prep)).
verb_type_lex(have(MODAL),aux+have(MODAL)).
verb_type_lex(V,MainTv):- no_repeats(V+MainTv,try_lex(verb_type_db0(V,MainTv))).
%verb_type_lex(V,MainTv):- nonvar(V),nonvar(MainTv).


verb_type_db0(Sys,Border,MainTv):- verb_type_db(Sys,Border,MainTv),
  (incompatable_verb_type(Sys,MainTv,Incompatible)->
  (dif(chat80,Sys),dif(Sys2,chat80), \+ verb_type_db(Sys,Border,Incompatible), \+ verb_type_db(Sys2,Border,Incompatible)); true).

incompatable_verb_type(W,Main+Iv,Main+tv):- fail, W\==chat80, Iv==iv,!.

verb_type_db(chat80,contain,main+tv).
verb_type_db(chat80,exceed,main+tv).
verb_type_db(chat80,govern,main+tv).



must_member(P,L):- compound(P),P=pos(E),!, must80(member(pos(E2),L)),!,E=E2.
must_member(E,L):- must80(member(E,L)).

% =================================================================
% Specialised Dictionary

never_adj(More):- More==more.
jj_adj_type(jj,restr).
jj_adj_type(jj,quantV).

adj_lex_w2(More,_,_,_):- never_adj(More),!,fail.
adj_lex_w2(_,W2,Adj,Type):- must_member(pos(JJ),W2),must_member(root(Adj),W2),jj_adj_type(JJ,Type),!.
adj_lex_w2(Adj,_,Adj,Type):- adj_lex(Adj,Type).
adj_lex(African,restr):- agentitive_trans(_,_,African).
adj_lex( Baltic,restr):- agentitive_symmetric_type(_,Baltic).
adj_lex(African,Restr):-  adj_db(chat80,African,Restr).
% now using POS tagger
%adj_lex(African,Restr):-  adj_db(talkdb,African,Restr).
%adj_lex(African,Restr):-  adj_db(clex,African,Restr).

expands_pos(Var,O):- var(Var), !, fail, O = Var.
expands_pos(List,O):- is_list(List),!,member(E,List),expands_pos(E,O).
expands_pos(I,pos(O)):- atom(I),atom_concat(L,'_',I), !, freeze(O,(atom(O),atom_concat(L,_,O))).
expands_pos(I,pos(I)):- atom(I), !.
expands_pos(A;B,O):-!, expands_pos(A,O);expands_pos(B,O).

match_pos(L,L):-!.
match_pos(A+B,O):-!,match_pos(A,O),match_pos(B,O).
match_pos(A;B,O):-!,match_pos(A,O);match_pos(B,O).
match_pos(Pos,L):- expands_pos(Pos,PosA),expands_pos(L,PosB),match_pos(PosA,PosB).
%adj_db(chat80,american,restr).
%adj_db(chat80,asian,restr).
%adj_db(chat80,european,restr).

adj_db(chat80,big,quantV).
adj_db(chat80,great,quantV).
adj_db(chat80,great,quantV).
adj_db(chat80,large,quantV).
%adj_db(chat80,new,quantV).
adj_db(chat80,old,quantV).
adj_db(chat80,small,quantV).

adj_db(chat80,average,restr).
adj_db(chat80,maximum,restr).
adj_db(chat80,minimum,restr).
adj_db(chat80,total,restr).
adj_db(clex,Y,RestrOrQuantV):- if_search_expanded(1),show_success(always,adj_db_clex(_,Y,RestrOrQuantV)).
adj_db(talkdb,Adj,restr):- if_search_expanded(1),show_success(always,(talkdb_adj(Adj))).

talkdb_adj(Adj):- fail, talkdb:talk_db(adj,Adj), (\+ talkdb:talk_db(adv,Adj);if_search_expanded(2)).

%adj_db_clex(X,Y,quantV):- clex:adj_itr(X,Y).
adj_db_clex(X,Y,restr):- clex:adj_itr(X,Y), (\+ clex:adv(X,_);if_search_expanded(2)).

adv_lex_w2(Adv,W2,Adv):- adv_lex_w2(Adv,W2).
adv_lex_w2(_,W2):- must_member(pos(rb),W2).
adv_lex_w2(Adv,_):- adverb_db(Adv).
adverb_db(X):- try_lex(adverb_lex_db(X)).
adverb_lex_db(chat80,tomorrow).
adverb_lex_db(chat80,yesterday).
adverb_lex_db(clex,X):- clex:adv(_,X), (\+ clex:adj_itr(X,_);if_search_expanded(2)).
adverb_lex_db(talkdb,X):- talk_db(adv,X), (\+ talk_db(adj,X);if_search_expanded(2)).


loc_pred_lex(of,east,prep(cp(east,of))).
loc_pred_lex(of,north,prep(cp(north,of))).
loc_pred_lex(of,south,prep(cp(south,of))).
loc_pred_lex(of,west,prep(cp(west,of))).

noun_w2(L,Plu,Root,Agmt) :-
  noun_form_wlex0(L,Plu,Root,Agmt),!,
  ((Root==flow;Root==border)->(fail,dumpST_ERR,break);true).

noun_form_wlex0(_,Plu,Root,Agmt) :- noun_form_lex(Plu,Root,Agmt),!.
noun_form_wlex0(L,_,Root,sg) :- member(pos(nn),L),member(root(Root),L),!.
noun_form_wlex0(L,_,Root,pl) :- member(pos(nns),L),member(root(Root),L),!.

noun_form_lex(Word,Root,_) :- noun_sing_plu_lex(chat80,Word),!,Root=Word.
% fail,fail noun_form_lex(Word,Root,_) :- try_lex(noun_sing_plu_lex(Word)),!,Root=Word.
noun_form_lex(Word,Root,Agmt) :- noun_plu_lex(Word,Sin),Root=Sin,!,(Word==Root-> Agmt=_;pl=Agmt).
noun_form_lex(Word,Root,Agmt) :- noun_plu_lex(Plu,Word),Root=Word, (Plu==Root-> Agmt=_;sg=Agmt).

noun_sing_plu_lex(chat80,proportion).
noun_sing_plu_lex(chat80,percentage).
noun_sing_plu_lex(chat80,million).
noun_sing_plu_lex(chat80,thousand).
noun_sing_plu_lex(chat80,fish).
%noun_sing_plu_lex(clex,Y):- clex:noun_mass(Y, _, _).

%hide_plur_root_noun(1,Millions,Million):- noun_sing_plu_lex(Million), !, Millions\==Million.
hide_plur_root_noun(1,_Twos,Two):-tr_number(Two,_).
hide_plur_root_noun(1,_Ins,In):- notrace(prep_lex(In)).
hide_plur_root_noun(1,_Mores,More):- comp_adv_lex(More,_).
hide_plur_root_noun(1,ares,are).
hide_plur_root_noun(1,_Noes,No):-det_lex(No,_,_,_).
hide_plur_root_noun(1,_Whats,What):- talkdb:talk_db(pronoun,What).
%hide_plur_root_noun(1,does,doe).
hide_plur_root_noun(2,Does,_):- atom(Does), verb_form_aux(Does,do(_MODAL),_Y,_Z).
hide_plur_root_noun(N,_,River):- N\==0, atom(River), verb_form_lex(River,_,_,_).

noun_plu_lex(ksqmiles,ksqmile).
noun_plu_lex(seamasses,seamass).
noun_plu_lex(sqmiles,sqmile).
noun_plu_lex(Averages,Average):- try_lex(noun_plu_db(Averages,Average)).

which_var(Rivers,River,N):- arg(N,v(Rivers,River),V),var(V),!.
which_var(_,_,0).

noun_plu_db(talkdb,Rivers,River):- which_var(Rivers,River,N),talkdb:talk_db(noun1,River,Rivers),  \+ hide_plur_root_noun(N,Rivers,River).
noun_plu_db(talkdb,Rivers,River):- noun_plu_db(clex,Rivers,River).
noun_plu_db(clex,Rivers,River):- which_var(Rivers,River,N), clex:noun_pl(Rivers,River,_), 
  \+ (hide_plur_root_noun(N,Rivers,River),
  nop(dmsg(warn(hide_plur_root_noun(N,Rivers,River)))),
  nop(rtrace(hide_plur_root_noun(N,Rivers,River)))).

noun_plu_db(chat80,areas,area).
noun_plu_db(chat80,averages,average).
noun_plu_db(chat80,capitals,capital).
noun_plu_db(chat80,cities,city).
noun_plu_db(chat80,continents,continent).
noun_plu_db(chat80,countries,country).
noun_plu_db(chat80,nations,nation).
noun_plu_db(chat80,states,state).
noun_plu_db(chat80,degrees,degree).
noun_plu_db(chat80,latitudes,latitude).
noun_plu_db(chat80,longitudes,longitude).
noun_plu_db(chat80,numbers,number).
noun_plu_db(chat80,oceans,ocean).
noun_plu_db(chat80,persons,person).
noun_plu_db(chat80,people,person).
noun_plu_db(chat80,places,place).
noun_plu_db(chat80,populations,population).
noun_plu_db(chat80,regions,region).
noun_plu_db(chat80,rivers,river).
noun_plu_db(chat80,Types,Type):- bind_pos('type',Type,'s',Types).
noun_plu_db(chat80,seas,sea).
noun_plu_db(chat80,sums,sum).
noun_plu_db(chat80,times,time).
noun_plu_db(chat80,totals,total).

comp_adj_lex_w2(More,_,_):- never_adj(More),!,fail.
comp_adj_lex_w2(_,W2,_):-  is_list(W2), \+ member(pos(jjr),W2),!,fail.
comp_adj_lex_w2(Smaller,_,Small):- must80(comp_adj_lex(Smaller,Small)),!.
comp_adj_lex_w2(_,W2,Root):-  is_list(W2), member(root(Root),W2),!.
comp_adj_lex(Smaller,Small):- try_lex_order([chat80,clex,talkdb],comp_adj_db(Smaller,Small)).
comp_adj_db(talkdb,Smaller,Small):- talkdb:talk_db(comp,Small,Smaller).
comp_adj_db(talkdb,Smaller,Small):- comp_adj_db(clex,Smaller,Small).
comp_adj_db(clex,Smaller,Small):- clex:adj_itr_comp(Smaller, Small).

comp_adj_db(chat80,bigger,big).
comp_adj_db(chat80,greater,great).
comp_adj_db(chat80,larger,large).
comp_adj_db(chat80,lesser,small).
comp_adj_db(chat80,less,small).
comp_adj_db(chat80,newer,new).
comp_adj_db(chat80,older,old).
comp_adj_db(chat80,smaller,small).

sup_adj_lex_w2(More,_,_):- never_adj(More),!,fail.
sup_adj_lex_w2(_,W2,_):- is_list(W2), \+ member(pos(jjs),W2),!,fail.
sup_adj_lex_w2(Smaller,_,Small):- must80(sup_adj_lex(Smaller,Small)).
% @TODO DMILES Cant also be adverb specifically "more"
sup_adj_lex_w2(_,W2,Root):-  is_list(W2), member(root(Root),W2),Root\==more.
sup_adj_lex(Smallest,Small):- try_lex(sup_adj_db(Smallest,Small)).
sup_adj_db(talkdb,Smallest,Small):- talkdb:talk_db(superl,Small,Smallest).
sup_adj_db(talkdb,Smallest,Small):- sup_adj_db(clex,Smallest,Small).
sup_adj_db(clex,Smallest,Small):- clex:adj_itr_sup(Smallest, Small).

sup_adj_db(chat80,biggest,big).
sup_adj_db(chat80,largest,large).
sup_adj_db(chat80,newest,new).
sup_adj_db(chat80,oldest,old).
sup_adj_db(chat80,smallest,small).


:- if(false).
comp_adv_lex_w2(_,W2,_):- is_list(W2), \+ must_member(pos(rbr),W2), !, fail.
comp_adv_lex_w2(Smaller,_,Small):- comp_adv_lex(Smaller,Small).
comp_adv_lex_w2(_,W2,Root):-  is_list(W2), must_member(root(Root),W2),!.
:- else.
comp_adv_lex_w2(_,W2,Small):- must_member(pos(rbr),W2),!,must_member(root(Small),W2).
comp_adv_lex_w2(Smaller,_,Small):- comp_adv_lex(Smaller,Small).
%comp_adv_lex_w2(_,W2,Root):-  is_list(W2), member(root(Root),W2),!.
:- endif.
comp_adv_lex(Lesser, Less):- try_one_lex(chat80,comp_adv_db(Lesser, Less)).
% @TODO DMiles I thinnk this was backwards (So i left it that way) "less than"
comp_adv_db(Least, Less):- try_one_lex(chat80,sup_adv_db(Least, Less)).
comp_adv_db(chat80,lesser,less).
comp_adv_db(chat80,more,more).
%comp_adv_db(talkdb,Smallest,Small):- talkdb:talk_db(comperl,Small,Smallest).
comp_adv_db(talkdb,Lesser,Less):- clex:adv_comp(Lesser, Less).
comp_adv_db(clex,Lesser,Less):- clex:adv_comp(Lesser, Less).

sup_adv_lex_w2(_,W2,_):- is_list(W2), \+ must_member(pos(rbs),W2),\+ must_member(pos(rb),W2), !, fail.
sup_adv_lex_w2(Smaller,_,Small):- sup_adv_lex(Smaller,Small).
sup_adv_lex_w2(_,W2,Root):-  is_list(W2), member(root(Root),W2),!.
sup_adv_lex(Least, Less):- try_one_lex(chat80,sup_adv_db(Least, Less)).
sup_adv_db(chat80,least,less).
sup_adv_db(chat80,most,more).
%sup_adv_db(talkdb,Smallest,Small):- talkdb:talk_db(superl,Small,Smallest).
sup_adv_db(talkdb,Least,Less):- sup_adv_db(clex,Least,Less).
sup_adv_db(clex,Least,Less):- clex:adv_sup(Least,Less).

:- fixup_exports.
