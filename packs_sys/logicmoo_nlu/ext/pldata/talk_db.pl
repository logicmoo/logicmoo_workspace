/*
% NomicMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%

  Talk_DB is a bit less organized most lexicons!

   But every once in a while there's a treasure in it 
          (so worth using)


   If I remember correctly, I got it from

      https://github.com/crossbowerbt/prolog-talk 

      and then wrote/used one of my S-expression translators     

*/

:- module(talkdb, [getPos/4, talk_db/1]).

:- style_check(-(discontiguous)).

decl_talk_db_data(F/A):-dynamic(F/A),multifile(F/A),export(F/A).

:- decl_talk_db_data(talk_db/1).
:- decl_talk_db_data(talk_db/2).
:- decl_talk_db_data(talk_db/3).
:- decl_talk_db_data(talk_db/4).
:- decl_talk_db_data(talk_db/5).
:- decl_talk_db_data(talk_db/6).


/*

 ?-  bagof(F, T^talk_db([F|T]), O),list_to_set(O,S),writeq(S).


[preposition,adj,adv,noun2,verb,interj,superl,conj,sing_only,fem,pronoun,p,
  masc,noun1,b_t,noun_verb,agentive,ncollect,pres_indic,comp,possessive,
  m,personal,impersonal,indef,auxiliary,pl_pronoun,interrog,auxilary,
  domain,noun_or_verb,transitive,intransitive]


 ?- bagof(F/A,X^(talk_db([F|X]),length(X,A)),O),list_to_set(O,S),writeq(S).

  [preposition/1,adj/1,adv/1,noun2/1,verb/1,interj/1,superl/1,conj/1,sing_only/1,fem/1,pronoun/1,p/1,
   masc/1,b_t/1,noun_verb/1,agentive/1,ncollect/1,pres_indic/1,comp/1,possessive/1,
   m/1,personal/1,impersonal/1,indef/1,auxiliary/1,pl_pronoun/1,interrog/1,auxilary/1,
   noun1/2,superl/2,domain/2,comp/2,noun_or_verb/3,
   transitive/5,intransitive/5]

 

       [noun2/1,preposition/1,adj/1,adv/1,verb/1,interj/1,superl/1,conj/1,sing_only/1,fem/1,pronoun/1,
       p/1,masc/1,b_t/1,noun_verb/1,agentive/1,ncollect/1,pres_indic/1,comp/1,possessive/1,
       m/1,personal/1,impersonal/1,indef/1,auxiliary/1,pl_pronoun/1,interrog/1,auxilary/1,
       domain/2,noun1/2,superl/2,comp/2,noun_or_verb/3,
       transitive/5,intransitive/5]


*/

talk_db_argsIsa(comp,1,adjective(comparative)).
talk_db_argsIsa(superl,1,adjective(superlative)).
talk_db_argsIsa(noun1,1,singular(plural)).
talk_db_argsIsa(intransitive,4,base(activeverb,imperfect,ingform,past_part)).
talk_db_argsIsa(transitive,4,base(activeverb,imperfect,ingform,past_part)).
talk_db_argsIsa(noun_or_verb,0,activeverb(ingform,noun)).
talk_db_argsIsa(adj,0,(adjective)).
talk_db_argsIsa(auxiliary,0,(verb)). % will shall wont
talk_db_argsIsa(conj,0,(conjuntion)).
talk_db_argsIsa(fem,0,(feminine)).
talk_db_argsIsa(fem,0,(noun)).
talk_db_argsIsa(impersonal,0,(meseems)).
talk_db_argsIsa(indef,0,(indefinate)).
talk_db_argsIsa(indef,0,(pronoun)).
talk_db_argsIsa(interj,0,(interjection)).
talk_db_argsIsa(interrog,0,(whpron)). % only instance
talk_db_argsIsa(agentive,0,(noun)).
talk_db_argsIsa(domain,0,noun(ology)).
talk_db_argsIsa(masc,0,(masculine)).
talk_db_argsIsa(masc,0,(noun)).
talk_db_argsIsa(ncollect,0,(massnoun)).
talk_db_argsIsa(noun2,0,(noun)).
talk_db_argsIsa(noun2,0,(plural)).
talk_db_argsIsa(noun2,0,(singular)).
talk_db_argsIsa(noun_verb,0,(verb)). 
talk_db_argsIsa(noun_verb,0,(noun)). 
talk_db_argsIsa(p,0,(adjective)).
talk_db_argsIsa(personal,0,(firstperson)).
talk_db_argsIsa(pl_pronoun,0,(plural)).
talk_db_argsIsa(pl_pronoun,0,(pronoun)).
talk_db_argsIsa(possessive,0,(pronoun)).
talk_db_argsIsa(preposition,0,(preposition)).
talk_db_argsIsa(pres_indic,0,(verb)).
talk_db_argsIsa(sing_only,0,(noun)).
talk_db_argsIsa(sing_only,0,(singular)).
talk_db_argsIsa(verb,0,(adj_verb)). % verb/nouns maybe
talk_db_argsIsa(pronoun,0,(pronoun)).
talk_db_argsIsa(adverb,0,(adverb)).


talk_db_pos_trans(massnoun,noun).
talk_db_pos_trans(superlative,adjective).
talk_db_pos_trans(comparative,adjective).
talk_db_pos_trans(superl,adj).
talk_db_pos_trans(comp,adj).
talk_db_pos_trans(intransitive,verb).
talk_db_pos_trans(transitive,verb).
talk_db_pos_trans(imperfect,verb).
talk_db_pos_trans(imperfect,past).
talk_db_pos_trans(past_part,verb).
talk_db_pos_trans(past_part,past).
talk_db_pos_trans(past_part,particple).
talk_db_pos_trans(past_part,adjectival).
talk_db_pos_trans(activeverb,verb).
%talk_db_pos_trans(activeverb,plural).
%talk_db_pos_trans(activeverb,noun).
talk_db_pos_trans(activeverb,active).
talk_db_pos_trans(ingform,verb).
talk_db_pos_trans(ingform,active).
talk_db_pos_trans(ingform,pres).
talk_db_pos_trans(ingform,particple).
talk_db_pos_trans(ingform,adjectival).
talk_db_pos_trans(A,A).

getPos(_,FPOS,_,FPOS).
getPos(0,_,POSVV,POS):-!,functor(POSVV,POS,_);POS=base.
getPos(AT,_,POSVV,POS):-arg(AT,POSVV,POS),!.



:- export(talk_db_pos/2).
talk_db_pos(POS,String):-nonvar(POS),nonvar(String),!,talk_db_t_0(POS,String),!.
talk_db_pos(POS,String):-talk_db_t_0(POS,String).

talk_db_t_0(POS,String):-talk_db_argsIsa(F,N,POSVV),talk_db_pos(String,POSVV,PPOS,F,N),talk_db_pos_trans(PPOS,POS).

talk_db_pos(String,POSVV,POS,F,0):- !, talk_db(F,String), (F=POSVV -> POS=F ; (POS=POSVV;POS=F)).
talk_db_pos(String,POSVV,POS,F,N):- nonvar(String),!, length(List,N),Search=[_|List],C=..[talk_db,F|Search],nth0(AT,Search,String,_),C,getPos(AT,F,POSVV,POS).
talk_db_pos(String,POSVV,POS,F,N):- length(List,N),Search=[_|List],C=..[talk_db,F|Search],C,nth0(AT,Search,String,_),getPos(AT,F,POSVV,POS).

show_num_clauses(F/A):- (functor(P,F,A),predicate_property(P, number_of_clauses(C)))->format(' ~w~t ~t ',[F/A=C]);format(' ~w~t ',[F/A='(none)']).
show_size_left(Message):-
   format('% ~w~t ~t',[Message]),
   show_num_clauses(talk_db/2),
   show_num_clauses(talk_db/3),
   show_num_clauses(talk_db/4),
   show_num_clauses(talk_db/5),
   show_num_clauses(talk_db/6),
   nl.

use_new_morefile:- fail.

check_marker(Type,Possibles):- 
  notrace(\+ compound(Type)), !,
  notrace((member(W, Possibles), atom(W), 
  concat_atom_safe([Type,Base],':',W), !,
  fill_in_blanks(Base,Possibles))).

check_marker(Marks,Possibles):- 
   notrace((member(W,Possibles), atom(W),
   arg(_,Marks,Type),
   concat_atom_safe([Type,Base],':',W), !,
   fill_in_blanks(Base,Possibles))).

fill_in_blanks( Base,[H|T]):- ignore(H=Base), !, fill_in_blanks(Base,T).
fill_in_blanks(_Base,[]).

:- dynamic(parser_chat80:plt/0).
:- multifile(parser_chat80:plt/0).

% was talk_db([F, A|List]):- talk_db_argsIsa(F, N_Minus1, _), length(List, N_Minus1), apply(talk_db, [F, A|List]).
% talk_db([F,A|List]):- talk_db_argsIsa(F,N,_), length(List,N),apply(talk_db,[F,A|List]).
talk_db(X):- quietly(talk_db0(X)).
  talk_db0([F,A|List]):- between(0,4,N), length(List,N), apply(talk_db,[F,A|List]).

talk_db(Ditransitive, Jacket,Jackets,Jacketed,Jacketing,Jacketen):-
  quietly(talk_db0(Ditransitive, Jacket,Jackets,Jacketed,Jacketing,Jacketen)).

  talk_db0(VerbType,Jacket,Jackets,Jacketed,Jacketing,Jacketed):- nonvar(Jacket), \+ parser_chat80:plt,
    talk_db(noun_or_verb,Jackets,Jacketing,Jacket),
    clause(talkdb:talk_db(VerbType,Jackets,Jackets,Jacketed,Jacketing,Jacketed),true).
  
  
  talk_db0(transitive, Jacket,Jackets,Jacketed,Jacketing,Jacketed):- 
              check_marker(v('tv','v'),[Jacket,Jackets,Jacketed,Jacketing]).
  
  talk_db0(intransitive, Jacket,Jackets,Jacketed,Jacketing,Jacketed):- 
              check_marker(v('iv','v'),[Jacket,Jackets,Jacketed,Jacketing]).
  
  talk_db0(ditransitive, Jacket,Jackets,Jacketed,Jacketing,Jacketed):- 
              check_marker(v('dv','v'),[Jacket,Jackets,Jacketed,Jacketing]).


talk_db(Type,A):- quietly(talk_db0(Type,A)).
  talk_db0(adj,Word):- check_marker('jj',[Word]).
  talk_db0(adv,Word):- check_marker('av',[Word]).
  talk_db0(Type,A):- atom(A), concat_atom_safe([Type,_],':', A).

talk_db(Type,A,B):- quietly(talk_db0(Type,A,B)).
  talk_db0(noun1,Sing,Plural):- check_marker(v('n','cn'),[Sing,Plural]).
  talk_db0(noun2,Sing,Sing):- check_marker(v('n','mn'),[Sing]).
  talk_db0(Type,A,B):- check_marker(Type,[A,B]).
  talk_db0(noun1,Sing,Sing):- talk_db(noun2,Sing).
  talk_db0(superl, far, aftermost).
  talk_db0(superl, close, formest).
  talk_db0(superl, far, furthest).

talk_db(Type,A,B,C):- check_marker(Type,[A,B,C]).
talk_db(Type,A,B,D,C):- check_marker(Type,[A,B,C,D]).



:- show_size_left("Including talk_db.nldata").
%:- include('talk_db.nldata').
:- absolute_file_name(pldata('talk_db.nldata'),
        File, [access(read)]),
   setup_call_cleanup(
    open(File, read, In),
    (set_stream(In, encoding(iso_latin_1)),
     repeat,
     read(In, P),
     (P= (:- (G)) -> call(G) ; talkdb:assertz_if_new(P)),
     P==end_of_file),
    close(In)).



%kill_talk_db_bad_verbs:-!.
kill_talk_db_bad_verbs:-
         show_size_left("correcting..."),        
         talk_db(VerbType,Jacket,Jackets,Jacketed,Jacketing,Jacketed),
         \+ \+ clause(talkdb:talk_db(noun1,Jacket,Jackets),true),
         %fail, retract(talkdb:talk_db(VerbType,Jacket,Jackets,Jacketed,Jacketing,Jacketed)), assertz_if_new(talkdb:talk_db(VerbType,Jackets,Jackets,Jacketed,Jacketing,Jacketed)),
         assertz_if_new(talkdb:talk_db(noun_or_verb,Jackets,Jacketing,Jacket)),
         nop(dmsg(fixed_talkdb_noun_verb(VerbType,(Jacket-->Jackets/Jacketed/Jacketing)))),
         fail.
kill_talk_db_bad_verbs:-  
         
         talk_db(VerbType,Fish,Fishes,Fished,Fishing,Fished),
         \+ \+ clause(talkdb:talk_db(noun2,Fish),true),
         %fail, retract(talkdb:talk_db(VerbType,Fish,Fishes,Fished,Fishing,Fished):-true), assertz_if_new(talkdb:talk_db(VerbType,Fishes,Fishes,Fished,Fishing,Fished)),
         assertz_if_new(talkdb:talk_db(noun_or_verb,Fishes,Fishing,Fish)),
         nop(dmsg(fixed_talkdb_noun_verb(VerbType,(Fish-->Fishes/Fished/Fishing)))),
         fail.

% noun1/3
kill_talk_db_bad_verbs:-
         retract(talkdb:talk_db(noun1,Sing,Plural,Subject):-true),
         asserta_if_new(talkdb:talk_db(noun1,Sing,Plural)),
         asserta_if_new(talkdb:talk_db(domain,Sing,Subject)),
         fail.
% noun1/1
kill_talk_db_bad_verbs:-
         retract(talkdb:talk_db(noun1,Mass):-true),
         asserta_if_new(talkdb:talk_db(noun3,Mass)),
         fail.
% domain/3
kill_talk_db_bad_verbs:-
         retract(talkdb:talk_db(domain,Mass,Subject1,Subject2):-true),
         asserta_if_new(talkdb:talk_db(noun3,Mass)),
         asserta_if_new(talkdb:talk_db(domain,Mass,[Subject1,Subject2])),
         fail.
% domain/5
kill_talk_db_bad_verbs:-
         retract(talkdb:talk_db(domain,Mass,S1,S2,S3,S4):-true),
         asserta_if_new(talkdb:talk_db(noun3,Mass)),
         asserta_if_new(talkdb:talk_db(domain,Mass,[S1,S2,S3,S4])),
         fail.
% noun3/2
kill_talk_db_bad_verbs:-
         retract(talkdb:talk_db(noun3,Mass):-true),
         \+ clause(talkdb:talk_db(noun1,Mass,_),true),
         \+ clause(talkdb:talk_db(noun1,_,Mass),true),         
         asserta_if_new(talkdb:talk_db(noun2,Mass)),
         fail.

kill_talk_db_bad_verbs:- \+ use_new_morefile, !, show_size_left("Loaded"),
  nop(save_to_file('talk_db.more',clause_always,talk_db)).

kill_talk_db_bad_verbs:-         
        prolog_load_context(file,This),
        show_size_left("Saving now..."),
        save_to_file('talk_db.more',erase_when(clause_from_assert),talk_db),   
        save_to_file('talk_db.prev',clause_not_here(This),talk_db),
        show_size_left("Saved...").


erase_when(When,Ref,Loc):- call(When,Ref,Loc),!,erase(Ref). 
clause_not_here(This,Ref,_Loc):- (clause_from_assert(Ref,_); \+ clause_belongs(Ref,This)),!.
clause_belongs(Ref,Loc):- (clause_from_assert(Ref,_);clause_from_file(Ref,Loc)),!.
clause_always(_Ref,_Loc).

clause_from_file(Ref,File):- clause_property(Ref,file(Other)), (Other=File;same_file(Other,File)).
clause_from_assert(Ref, _):- \+ clause_property(Ref,file(_)), \+ clause_property(Ref,source(_)).

save_to_file(Name,Belongs,F):-
  File = pldata(Name),
  absolute_file_name(File,Loc,[access(read)]),
  tell(Loc),
  format(':- style_check(-(discontiguous)).~n:- style_check(-(singleton)). ~n'),
 forall(current_predicate(F/A),
 ((functor(P,F,A),
  format(':- multifile(~q).~n:- dynamic(~q).~n',[(F/A),(F/A)]),
   forall((clause(P,true,Ref),call(Belongs,Ref,Loc)),
     (display(P),write('.'),nl))
           ))),
      told,
      dmsg(compete_save_to_file(Name,F)).

:- kill_talk_db_bad_verbs.

:- if(use_new_morefile).
:- include('talk_db.more').
:- show_size_left("Loaded talk_db.more").
:- endif.       
%:-share_mp(kill_talk_db_bad_verbs/0).

:- ignore(retract(talkdb:talk_db(adv,there))).

:- forall((
  member(S,[adj,noun2,verb,preposition,interj,conj,sing_only,
  pronoun,p,b_t,noun_verb,agentive,ncollect,pres_indic,
  possessive,m,personal,impersonal,indef,auxiliary,pl_pronoun,interrog,auxilary]),
  talkdb:talk_db(S,X)),ignore((\+ atom_concat(_,'ly',X),show_success((S=S,retract(talkdb:talk_db(adv,X))))))).

:- forall((S=adj,talkdb:talk_db(S,X)),
  ignore((S\==adv,show_success((S=S,retract(talkdb:talk_db(interj,X))))))).


:- forall((
  member(S,[interj,
  pronoun,p,noun_verb,agentive,ncollect,pres_indic,
  possessive,m,personal,impersonal,indef,auxiliary,pl_pronoun,interrog,auxilary]),
  talkdb:talk_db(S,X)),ignore((show_success((S=S,retract(talkdb:talk_db(adj,X))))))).

% :- forall((S=adv,talkdb:talk_db(S,X)), ignore((show_success((S=S,retract(talkdb:talk_db(adj,X))))))).

:- fixup_exports. 
:- fixup_exports_system.%  system:re export(talk_db).

% =================================
% some random talk_db/2-6 from the other file (to help see the meanings)
% =================================

/*

talk_db(adj, aaronic).
talk_db(adj, aaronical).
talk_db(adj, abactinal).
talk_db(adj, abandoned).
talk_db(adj, abased).
talk_db(adj, abatable).
talk_db(adj, abominable).
talk_db(adj, absent).
talk_db(adj, usurpatory).
talk_db(adj, uterine).
talk_db(adv, yesterday).
talk_db(adv, yesternight).
talk_db(adv, yet).
talk_db(adv, youngly).
talk_db(adv, ysame).
talk_db(adv, yvel).
talk_db(adv, ywis).
talk_db(agentive, doer).
talk_db(auxilary, wont).
talk_db(auxiliary, shall).
talk_db(auxiliary, will).
talk_db(b_t, crimson).
talk_db(b_t, crossbite).
talk_db(b_t, lumber).
talk_db(b_t, pulley).
talk_db(comp, angry, angrier).
talk_db(comp, wordy, wordier).
talk_db(comp, wormy, wormier).
talk_db(comp, worthy, worthier).
talk_db(comp, wry, wrier).
talk_db(comp, yellow, yellower).
talk_db(comp, young, younger).
talk_db(conj, albe).
talk_db(conj, albee).
talk_db(conj, albeit).
talk_db(conj, all).
talk_db(conj, also).
talk_db(conj, altho).
talk_db(conj, although).
talk_db(conj, an).
talk_db(conj, and).
talk_db(conj, so).
talk_db(conj, syne).
talk_db(conj, than).
talk_db(conj, then).
talk_db(conj, therefore).
talk_db(conj, tho).
talk_db(conj, til).
talk_db(conj, till).
talk_db(conj, unless).
talk_db(conj, until).
talk_db(domain, abalone, zoology).
talk_db(domain, abandonee, law).
talk_db(domain, abator, law).
talk_db(domain, abelian, "eccl., hist").
talk_db(domain, abelite, "eccl., hist").
talk_db(domain, abelonian, "eccl., hist").
talk_db(domain, abietite, chem).
talk_db(domain, abirritation, med).
talk_db(domain, ablegate, "r., c., ch").
talk_db(domain, aboma, zoology).
talk_db(domain, absinthate, chem).
talk_db(domain, absinthin, chem).
talk_db(domain, absolute, geom).
talk_db(fem, alumna).
talk_db(fem, buffa).
talk_db(fem, chiffonier).
talk_db(fem, gitana).
talk_db(fem, her).
talk_db(fem, lanner).
talk_db(fem, marseillaise).
talk_db(fem, masseuse).
talk_db(fem, poseuse).
talk_db(fem, religieuse).
talk_db(fem, she).
talk_db(impersonal, meseems).
talk_db(impersonal, methinks).
talk_db(impersonal, tacet).
talk_db(indef, one).
talk_db(indef, whatso).
talk_db(interj, adieu).
talk_db(interj, so).
talk_db(interj, soft).
talk_db(interj, soho).
talk_db(interj, tallyho).
talk_db(interj, tush).
talk_db(interj, walaway).
talk_db(interj, waly).
talk_db(interj, wayleway).
talk_db(interj, welaway).
talk_db(interj, welladay).
talk_db(interj, weyleway).
talk_db(interj, whist).
talk_db(interj, whoa).
talk_db(interj, yoicks).
talk_db(interj, zounds).
talk_db(interrog, what).
talk_db(intransitive, abort, aborts, aborted, aborting, aborted).
talk_db(intransitive, abound, abounds, abounded, abounding, abounded).
talk_db(intransitive, abstain, abstains, abstained, abstaining, abstained).
talk_db(intransitive, zighyr, zighyrs, zighyred, zighyring, zighyred).
talk_db(intransitive, zigzag, zigzags, zigzaged, zigzaging, zigzaged).
talk_db(intransitive, zip, zips, ziped, ziping, ziped).
talk_db(m, kit).
talk_db(m, sacerdotalism).
talk_db(masc, buffo).
talk_db(masc, he).
talk_db(masc, his).
talk_db(masc, lanneret).
talk_db(masc, marseillais).
talk_db(masc, masseur).
talk_db(masc, poseur).
talk_db(masc, religieux).
talk_db(masc, solus).
talk_db(ncollect, folk).
talk_db(ncollect, folks).
talk_db(noun1, aam, aams).
talk_db(noun1, ab, abs).
talk_db(noun1, abandon, abandons). % note verbs are snuck into here (and need to be filtered)
talk_db(noun1, ability, abilities).
talk_db(noun1, abolishment, abolishments).
talk_db(noun2, abdominales).
talk_db(noun2, abdominalia).
talk_db(noun2, aborigines).
talk_db(noun2, abranchiata).
talk_db(noun2, fish).
talk_db(noun2, zygobranchia).
talk_db(noun2, zygodactyli).
talk_db(noun_verb, deprave).
talk_db(noun_verb, harlequin).
talk_db(noun_verb, hobble).
talk_db(noun_verb, hold).
talk_db(noun_verb, kemb).
talk_db(noun_verb, ken).
talk_db(noun_verb, loan).
talk_db(noun_verb, trip).
talk_db(p, bounden).
talk_db(p, collied).
talk_db(p, dustman).
talk_db(p, laden).
talk_db(personal, me).
talk_db(pl_pronoun, they).
talk_db(pl_pronoun, tho).
talk_db(preposition, a). % note articles are snuck into here (and need to be filtered
talk_db(preposition, about).
talk_db(preposition, above).
talk_db(preposition, with).
talk_db(preposition, withal).
talk_db(preposition, within).
talk_db(preposition, without).
talk_db(preposition, withouten).
talk_db(preposition, yer).
talk_db(preposition, ymel).
talk_db(pres_indic, forewot).
talk_db(pronoun, another).
talk_db(pronoun, any).
talk_db(pronoun, echon).
talk_db(pronoun, echoon).
talk_db(pronoun, either).
talk_db(pronoun, yours).
talk_db(pronoun, yourself).
talk_db(pronoun, yow).
talk_db(sing_only, alms).
talk_db(sing_only, amends).
talk_db(sing_only, bellows).
talk_db(sing_only, vermin).
talk_db(sing_only, vers).
talk_db(superl, aftermost).
talk_db(superl, angry, angriest).
talk_db(superl, bad, worst).
talk_db(superl, blue, bluest).
talk_db(superl, brave, bravest).
talk_db(transitive, abandon, abandons, abandoned, abandoning, abandoned).
talk_db(transitive, abduce, abduces, abduced, abducing, abduced).
talk_db(transitive, able, ables, abled, abling, abled).
talk_db(transitive, abolish, abolishes, abolished, abolishing, abolished).
talk_db(transitive, abscond, absconds, absconded, absconding, absconded).
talk_db(transitive, absent, absents, absented, absenting, absented).
talk_db(transitive, absinthiate, absinthiates, absinthiated, absinthiating, absinthiated).
talk_db(transitive, absorb, absorbs, absorbed, absorbing, absorbed).
talk_db(transitive, twit, twits, twitted, twitting, twitted).
talk_db(transitive, twitter, twitters, twittered, twittering, twittered).
talk_db(verb, abray).

*/
