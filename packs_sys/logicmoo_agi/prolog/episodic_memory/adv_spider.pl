/*
% NomicMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
% Bits and pieces:
%
% LogicMOO, Inform7, FROLOG, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
%
% Copyright (C) 2004 Marty White under the GNU GPL
% Sept 20, 1999 - Douglas Miles
% July 10, 1996 - John Eikenberry
%
% Logicmoo Project changes:
%
% Main file.
%
*/
:- '$set_source_module'(mu).
:- ensure_loaded(adv_loader).

:- discontiguous map_of/2.

map_of(spider_2nd_person,
"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%P1 S1        L1             P2%
%     L4                L2     %
%L3                            %
%             P5             L5%
%                              %
%P3           L6             P4%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" ).

:- discontiguous example_mentalese/2.

example_mentalese(spider_2nd_person,"
S2 is an examplar.
E2 is an environment.
E2 has a place P1, P2, P3, and P4 that is for building a web W2.
P5 is unbuilt.
W2 is a thing.
W2 is a spider web.
W2 is unbuilt.
S2 does attach W1 to P1.
W2 is attached to P1.
W2 has begun being built.
W2 is attached to P2.
W2 is attached to P3.
W2 is attached to P4.
S2 makes L1 connect P1 to P2.
S2 makes L2 connect P2 to P3.
S2 makes L3 connect P3 to P1.
S2 makes L4 connect P1 to P4.
S2 makes L5 connect P4 to P2.
S2 go to P4.
S2 makes L6 connect P4 to P3.
S2 makes L2 fuse to L4 at P5.
W2 is built.
P5 is built.
S2 go to P5. ").

example_mentalese(spider_autobiography, "
S1 sees the map of spider_1st_person S1P.
S1 imagines spider_2nd_person S2P. 
;; S1 cant see map of spider_2nd_person.
;; S2 cant see map of spider_1st_person.


").

% real world
map_of(spider_1st_person,
"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Q4           N1               %
%     N4         Q2            %
%                              %
%             Q5               %
%                              %
%                              %
%N3         %%%%%%%%%%         %
%      S1                  N5  %
%                              %
%         Q3  N6          S2 Q1%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%").

extract_objects(Str,Objs):- atom_chars(Str,Chars),extract_objects(Chars,0,0,Objs),ground(Objs).

extract_objects([],_,_,[]):-  !.
extract_objects(['\n'|Str],_,Y,Objs):- !,
  Y2 is Y+1, extract_objects(Str,0,Y2,Objs).
extract_objects([' '|Str],X,Y,Objs):-  !,
  X2 is X+1, extract_objects(Str,X2,Y,Objs).

extract_objects([S,S1|Str],X,Y,Objs):- 
   ascii_2_obj([S,S1],X,Y,Os),Os\==[],
   X2 is X+2, extract_objects(Str,X2,Y,OOs),
   append(Os,OOs,Objs).
extract_objects([S|Str],X,Y,Objs):- 
   ascii_2_obj([S],X,Y,Os),!,
   X2 is X+1, extract_objects(Str,X2,Y,OOs),
   append(Os,OOs,Objs).
extract_objects([_|Str],X,Y,Objs):- 
   X2 is X+1, extract_objects(Str,X2,Y,Objs),!.
  

ascii_2_obj([S,N],X,Y,[obj(Var,X,Y)]):- !, char_type(N,digit),!, atom_concat(S,N,Var).
ascii_2_obj([' '],_,_,[]):-!.
ascii_2_obj([S],X,Y,[obj(S,X,Y)]).
:- use_module(library(logicmoo_nlu)).
%?- c80("S1 attaches W1 to Q1.").


objs_2_ascii(Objs,Str):- objs_2_ascii([],0,0,Objs,Text),text_to_string(Text,Str).

objs_2_ascii(Str,X,Y,Objs,StrOut):- select(obj('%',X,Y),Objs,RObjs), append(Str,['%'],Str0),
   X2 is X+1, objs_2_ascii(Str0,X2,Y,RObjs,StrOut).
objs_2_ascii(Str,X,Y,Objs,StrOut):- select(obj(N,X,Y),Objs,RObjs), atom_chars(N,[C1,C2]),!,
   append(Str,[C1,C2],Str0),
   X2 is X+2, objs_2_ascii(Str0,X2,Y,RObjs,StrOut).
objs_2_ascii(Str,X,Y,Objs,StrOut):- select(obj(N,X,Y),Objs,RObjs), atom_chars(N,[C1]),!,
   append(Str,[C1],Str0),
   X2 is X+1, objs_2_ascii(Str0,X2,Y,RObjs,StrOut).
objs_2_ascii(Str,_,Y,Objs,Str):- forall(member(obj(_,_,OY),Objs),OY<Y),!.
objs_2_ascii(Str,X,Y,Objs,Out):- forall(member(obj(_,OX,_),Objs),OX<X),!,
   append(Str,['\n'],Str10), Y2 is Y+1, objs_2_ascii(Str10,0,Y2,Objs,Out).
objs_2_ascii(Str,X,Y,Objs,Out):- 
   append(Str,[' '],Str10), X2 is X+1, 
   objs_2_ascii(Str10,X2,Y,Objs,Out).


:- forall(map_of(Name,Map),(
    must_det_l((writeln(1=Name),
       extract_objects(Map,Objs),!,
       wdmsg(Objs),
       objs_2_ascii(Objs,Ascii))),!,
       writeln((Ascii)))).
%:- break.

example_mentalese(spider_1st_person, "
S1 is me.
E1 is here.
E1 is simular to E2.
; S1 sees E1 as E2.
;   Therefore E1 has a place Q1, Q2, Q3, and Q4 that is for building a web W1.
S1 is simular to S2.
W1 is simular to W2.
W1 is a thing.
W1 is a spider web.


;; the rest below has to be guessed by system

S1 does attach W1 to Q1.
W1 is attached to Q1.
W1 has begun being built.
W1 is attached to Q2.
W1 is attached to Q3.
W1 is attached to Q4.
S1 makes N1 connect Q1 to Q2.
S1 makes N2 connect Q2 to Q3.
S1 makes N3 connect Q3 to Q1.
S1 makes N4 connect Q1 to Q4.
S1 makes N5 connect Q4 to Q2.
S1 go to Q4.
S1 makes N6 connect Q4 to Q3.
S1 makes N2 fuse to N4 at Q5.
W1 is built.
S1 go to Q5.
").

