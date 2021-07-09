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
|       This program may be used, copied, altered or included in other    |
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
ag_number(N,pl) :- N>1.

chk_word(Word):- word(Word).
word(Word) :- '`' (Word).
word(Word) :- conj_lex(Word).
word(Word) :- adverb_lex(Word).
word(Word) :- sup_adj_lex(Word,_).
word(Word) :- comp_adj_lex(Word,_).
word(Word) :- adj_lex(Word,_).
word(Word) :- name_LF(Word).
word(Word) :- terminator_lex(Word,_).
word(Word) :- pers_pron_lex(Word,_,_,_,_).
word(Word) :- poss_pron_lex(Word,_,_,_).
word(Word) :- rel_pron(Word,_).
word(Word) :- verb_form_lex(Word,_,_,_).
word(Word) :- noun_form_lex(Word,_,_).
word(Word) :- prep_lex(Word).
word(Word) :- quantifier_pron_lex(Word,_,_).
word(Word) :- number_lex(Word,_,_).
word(Word) :- det_lex(Word,_,_,_).
word(Word) :- int_art_lex(Word,_,_,_).
word(Word) :- int_pron_lex(Word,_).
word(Word) :- loc_pred_lex(_,Word,_).

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

det_lex(a,sg,a,indef).
det_lex(all,pl,all,indef).
det_lex(an,sg,a,indef).
det_lex(any,_,any,indef).
det_lex(each,sg,each,indef).
det_lex(every,sg,every,indef).
det_lex(no,_,no,indef).
det_lex(some,_,some,indef).
det_lex(the,No,the(No),def).

int_art_lex(what,X,_,int_det(X)).
int_art_lex(which,X,_,int_det(X)).
int_pron_lex(what,undef).
int_pron_lex(which,undef).
int_pron_lex(who,subj).
int_pron_lex(whom,compl).

name_LF(Name) :- name_template_LF(Name,_).



number_lex(W,I,Nb) :- 
        tr_number(W,I),
        ag_number(I,Nb).

pers_pron_lex(he,masc,3,sg,subj).
pers_pron_lex(her,fem,3,sg,compl(_)).
pers_pron_lex(him,masc,3,sg,compl(_)).
pers_pron_lex(i,_,1,sg,subj).
pers_pron_lex(it,neut,3,sg,_).
pers_pron_lex(me,_,1,sg,compl(_)).
pers_pron_lex(she,fem,3,sg,subj).
pers_pron_lex(them,_,3,pl,compl(_)).
pers_pron_lex(them,_,3,pl,subj).
pers_pron_lex(us,_,1,pl,compl(_)).
pers_pron_lex(we,_,1,pl,subj).
pers_pron_lex(you,_,2,_,_).

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

prep_db(talkdb,X):- talkdb:talk_db(preposition, X).

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

%regular_past_db(chat80,had,have).

%superceeded regular_past_db(chat80,contained,contain).
%superceeded regular_past_db(chat80,exceeded,exceed).

%superceeded regular_past_db(chat80,governed,govern).

%superceeded regular_pres_lex(V):- % superceeded regular_pres_db(chat80,V).

% superceeded regular_pres_db(chat80,contain).
% superceeded regular_pres_db(chat80,exceed).


% superceeded regular_pres_db(chat80,govern).


% superceeded regular_pres_db(chat80,do).
% superceeded regular_pres_db(chat80,have).

rel_pron_lex(which,undef).
rel_pron_lex(who,subj).
rel_pron_lex(whom,compl).

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

ctx_pron_lex(in,place,where).
ctx_pron_lex(at,time,when).

how_many_lex([how,many]).

first_lexicon(X):- available_lexicon(X),!.
available_lexicon(talkdb).
available_lexicon(chat80).
%available_lexicon(clex).
try_lex(How, G, CALL):- G=..[F|ARGS], CALL=..[F,How|ARGS].
show_tries_except(_Which,_TF,_G):- \+ tracing,!.
show_tries_except(Which,TF,G):- !, forall((available_lexicon(Other),Other\==Which,try_lex(Other,G,CALL),clause(CALL,_)),warn_when(TF,CALL)).
warn_when(fail,G):- G -> true ; wdmsg(warn_when(failed,G)).
warn_when(true,G):- G *-> wdmsg(warn_when(succeeded,G)) ; true.
try_lex(G):- first_lexicon(Which),try_lex(Which,G,CALL),copy_term(G,CopyG),
 (CALL
    *-> show_tries_except(Which,fail,CopyG)
    ; (show_tries_except(_,true,CopyG),!, fail)).


correct_root(do,do).
correct_root(R,R).

verb_form_wlex(_,A,B,C,D):- verb_form_lex(A,B,C,D),!.
verb_form_wlex(L,_,Root,_,_):- member(pos(V),L),atom_concat('vb',_,V),member(root(Root0),L),!,correct_root(Root0,Root).
verb_form_aux(am,be,pres+fin,1+sg).
verb_form_aux(are,be,pres+fin,2+sg).
verb_form_aux(are,be,pres+fin,_+pl).
verb_form_aux(been,be,past+part,_).
verb_form_aux(be,be,inf,_).
verb_form_aux(being,be,pres+part,_).
verb_form_aux(is,be,pres+fin,3+sg).
verb_form_aux(was,be,past+fin,1+sg).
verb_form_aux(was,be,past+fin,3+sg).
verb_form_aux(were,be,past+fin,2+sg).
verb_form_aux(were,be,past+fin,_+pl).


verb_form_aux(do,do,pres+fin,_+pl).
verb_form_aux(did,do,past+fin,_).
verb_form_aux(does,do,pres+fin,3+sg).
verb_form_aux(can,can,pres+fin,3+sg).
verb_form_aux(doing,do,pres+part,_).
verb_form_aux(done,do,past+part,_).

verb_form_aux(will,will,pres+fin,3+sg).
verb_form_aux(would,will,past+fin,_).

verb_form_aux(could,can,past+fin,_).
verb_form_aux(can,can,pres+fin,3+sg).


verb_form_aux(has,have,pres+fin,3+sg).
verb_form_aux(have,have,pres+fin,_+pl).
verb_form_aux(having,have,pres+part,_).
verb_form_aux(had,have,past+part,_).

verb_form_lex(A,B,C,D):- verb_form_aux(A,B,C,D).
verb_form_lex(A,B,C,D):- \+ avoided_verb(A), try_lex(verb_form_db(A,B,C,D)), \+ avoided_verb(A).

avoided_verb(A):- nonvar(A), clause(verb_form_aux(A,_,_,_),true).



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
% ... because [is,france,bordered,by,belgium,?] was not properly parsed. Deduced from verb_form_db(chat80,done,do,past+part,_) bellow.
%verb_form_lex(A,A,C,D) :-
%  writef("********************************** verb_form_db {0} failed", [[A,A,C,D]]).
%  !,
%  fail.

%verb_root_lex(Root):- use_lexicon_80(chat80), %verb_root_db(chat80,Root).
%  verb_type_db(chat80,Root,_MainTv).
%verb_root_lex(be).
%verb_root_lex(do).
%verb_root_lex(have).


%superceeded verb_root_db(chat80,contain).
%superceeded verb_root_db(chat80,exceed).


%superceeded verb_root_db(chat80,govern).

verb_type_lex(be,aux+be).
verb_type_lex(do,aux+dv(_Prep)).
verb_type_lex(have,aux+have).
verb_type_lex(V,MainTv):- try_lex(verb_type_db(V,MainTv)).


verb_type_db(chat80,contain,main+tv).
verb_type_db(chat80,exceed,main+tv).
verb_type_db(chat80,govern,main+tv).





% =================================================================
% Specialised Dictionary

adj_lex(African,restr):- agentitive_trans(_,_,African).
adj_lex( Baltic,restr):- agentitive_symmetric_type(_,Baltic).
adj_lex(African,Restr):-  adj_db(chat80,African,Restr).

%adj_db(chat80,american,restr).
%adj_db(chat80,asian,restr).
%adj_db(chat80,european,restr).

adj_db(chat80,big,quantV).
adj_db(chat80,great,quantV).
adj_db(chat80,great,quantV).
adj_db(chat80,large,quantV).
adj_db(chat80,new,quantV).
adj_db(chat80,old,quantV).
adj_db(chat80,small,quantV).

adj_db(chat80,average,restr).
adj_db(chat80,maximum,restr).
adj_db(chat80,minimum,restr).
adj_db(chat80,total,restr).

adverb_lex(tomorrow).
adverb_lex(yesterday).

loc_pred_lex(of,east,prep(cp(east,of))).
loc_pred_lex(of,north,prep(cp(north,of))).
loc_pred_lex(of,south,prep(cp(south,of))).
loc_pred_lex(of,west,prep(cp(west,of))).

noun_form_wlex(L,Plu,Root,Agmt) :-
  noun_form_wlex0(L,Plu,Root,Agmt),!,
  (Root==flow->(fail,dumpST,break);true).

noun_form_wlex0(_,Plu,Root,Agmt) :- noun_form_lex(Plu,Root,Agmt),!.
noun_form_wlex0(L,_,Root,sg) :- member(pos(nn),L),member(root(Root),L),!.
noun_form_wlex0(L,_,Root,pl) :- member(pos(nns),L),member(root(Root),L),!.

noun_form_lex(Word,Root,_) :- noun_sing_plu_lex(Word),!,Root=Word.
noun_form_lex(Word,Root,Agmt) :- noun_plu_lex(Word,Sin),Root=Sin,!,(Word==Root-> Agmt=_;pl=Agmt).
noun_form_lex(Word,Root,Agmt) :- noun_plu_lex(Plu,Word),Root=Word, (Plu==Root-> Agmt=_;sg=Agmt).

noun_sing_plu_lex(proportion).
noun_sing_plu_lex(percentage).
noun_sing_plu_lex(million).
noun_sing_plu_lex(thousand).
noun_sing_plu_lex(fish).

hide_plur_root_noun(1,Millions,Million):- noun_sing_plu_lex(Million), !, Millions\==Million.
hide_plur_root_noun(1,_Twos,Two):-tr_number(Two,_).
hide_plur_root_noun(1,_Ins,In):- notrace(prep_lex(In)).
hide_plur_root_noun(1,_Mores,More):- comp_adv_lex(More).
hide_plur_root_noun(1,ares,are).
hide_plur_root_noun(1,_Noes,No):-det_lex(No,_,_,_).
hide_plur_root_noun(1,_Whats,What):- talkdb:talk_db(pronoun,What).
%hide_plur_root_noun(1,does,doe).
hide_plur_root_noun(2,Does,_):- atom(Does), verb_form_aux(Does,do,_Y,_Z).
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
  \+ (hide_plur_root_noun(N,Rivers,River),nop(dmsg(warn(hide_plur_root_noun(N,Rivers,River)))),
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
noun_plu_db(chat80,seas,sea).
noun_plu_db(chat80,sums,sum).
noun_plu_db(chat80,times,time).
noun_plu_db(chat80,totals,total).


comp_adj_lex(Smaller,Small):- try_lex(comp_adj_db(Smaller,Small)).
comp_adj_db(talkdb,Smaller,Small):- talkdb:talk_db(comp,Small,Smaller).
comp_adj_db(talkdb,Smaller,Small):- comp_adj_db(clex,Smaller,Small).
comp_adj_db(clex,Smaller,Small):- clex:adj_itr_comp(Smaller, Small).

comp_adj_db(chat80,bigger,big).
comp_adj_db(chat80,greater,great).
comp_adj_db(chat80,larger,large).
comp_adj_db(chat80,less,small).
comp_adj_db(chat80,newer,new).
comp_adj_db(chat80,older,old).
comp_adj_db(chat80,smaller,small).


sup_adj_lex(Smallest,Small):- try_lex(sup_adj_db(Smallest,Small)).
sup_adj_db(talkdb,Smallest,Small):- talkdb:talk_db(superl,Small,Smallest).
sup_adj_db(talkdb,Smallest,Small):- sup_adj_db(clex,Smallest,Small).
sup_adj_db(clex,Smallest,Small):- clex:adj_itr_sup(Smallest, Small).

sup_adj_db(chat80,biggest,big).
sup_adj_db(chat80,largest,large).
sup_adj_db(chat80,newest,new).
sup_adj_db(chat80,oldest,old).
sup_adj_db(chat80,smallest,small).


comp_adv_lex(less).
comp_adv_lex(more).

sup_adv_lex(least).
sup_adv_lex(most).
