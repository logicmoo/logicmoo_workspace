:- module(ac_xnl_7166_nlkb,[killBadNL/0]).


:- export(ac_nl_info/2).

findset(T,G,L):- setof(T,G,L0)->(flatten([L0],List),list_to_set(List,L)); L=[].

ac_nl_info(Str,Infos):-
       findset(Info,ac_nl_info_0(Str,Info),Info0),
       maplist(ac_nl_info_1,Info0,More),
       append([Info0|More],ListM),list_to_set(ListM,Infos),!.

ac_nl_info_0(Str,[cycWord(CycWord),cycPOS(RegularAdverb)]):- acnl(RegularAdverb,CycWord,Str,_645343).
ac_nl_info_0(Str,[str(Str)]):- string(Str).
ac_nl_info_0(CycWord,[cycWord(CycWord)]):-  acnl('isa',CycWord,_,_645347).
ac_nl_info_0(Word,Results):-
   atom(Word), % \+ acnl('isa',xCleverTheWord,_,_645347),
   downcase_atom(Word,Word),atom_string(Word,String),!,ac_nl_info_0(String,Results).


ac_nl_info_1(CycWord,Results):- findset(Info,ac_nl_info_2(cycWord(CycWord),Info), Results),!.

ac_nl_info_2(cycWord(CycWord),t(P,CycWord,Y)):-      acnl(P,CycWord,Y,_).
ac_nl_info_2(cycWord(CycWord),t(P,Y,CycWord)):-      acnl(P,Y,CycWord,_).
ac_nl_info_2(cycWord(CycWord),t(P,CycWord,Y,Z)):-    acnl(P,CycWord,Y,Z,_).
ac_nl_info_2(cycWord(CycWord),t(P,Y,CycWord,Z)):-    acnl(P,Y,CycWord,Z,_).
ac_nl_info_2(cycWord(CycWord),t(P,CycWord,Y,Z,OO)):- acnl(P,CycWord,Y,Z,OO,_).
ac_nl_info_2(cycWord(CycWord),t(P,CycWord,Y,Z,OO)):- acnl(P,Y,CycWord,Z,OO,_).

:- retractall(acnl(retainTerm,_,_)).

bork_arg(E):- ignore('$BORKED_ARG'=E).

killBadNLC(AC):- forall(call(AC),killBadNLQ(AC)).
killBadNLQ(AC):- forall(retract(AC), assertz('$BORKED'(AC))).
:- export(killBadNLR/1).
killBadNLR(AC):- forall(retract(AC),(assertz('$BORKED'(AC)),wdmsg(retract(AC)))).

:- export(killBadNL/0).
killBadNL:- killBadNLC(nlkb7166:acnl(retainTerm,_,_)),fail.
%killBadNL:- killBadNLC(nlkb7166:acnl(_,xxxxx,_,_)),fail.
%killBadNL:- killBadNLC(nlkb7166:acnl(_,_,xxxxx,_)),fail.
killBadNL:- between(1,8,N),
  once((length(L,N),append([acnl,F|L],[_ID],Out),P=..Out)),
  % volatile(nlkb7166:acnl/N),
  member('$BORKED_ARG',L), call(nlkb7166:P),
  once((subst(P,'$BORKED_ARG',_,OrigP),
    OrigP=..[acnl,F|OrigPL],
    copy_term(OrigPL,OrigPLCopy),
    OrigPUA=..[acnl_ua,F|OrigPLCopy],
    maplist(bork_arg,L))),
  ((F==verbSemTrans)-> 
    (wdmsg(retain(OrigPUA)),killBadNLQ(nlkb7166:P),assert(nlkb7166:P)) ; 
    (wdmsg(retainq(OrigPUA)),killBadNLQ(nlkb7166:P),assert(nlkb7166:P))),
  fail.
killBadNL.

:- killBadNL.

:- fixup_exports.

