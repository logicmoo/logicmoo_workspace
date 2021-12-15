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
% Aept 20, 1999 - Douglas Miles
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
%P1 A1        L1             P2%
%     L4                L2     %
%L3                            %
%             P5             L5%
%                              %
%P3           L6             P4%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" ).

:- discontiguous example_mentalese/2.

example_mentalese(spider_2nd_person,"
A2 is an examplar.
E2 is an environment.
E2 has a place P1, P2, P3, and P4 that is for building a web W2.
P5 is unbuilt.
W2 is a thing.
W2 is a spider web.
W2 is unbuilt.
A2 does attach W1 to P1.
W2 is attached to P1.
W2 has begun being built.
W2 is attached to P2.
W2 is attached to P3.
W2 is attached to P4.
A2 makes L1 connect P1 to P2.
A2 makes L2 connect P2 to P3.
A2 makes L3 connect P3 to P1.
A2 makes L4 connect P1 to P4.
A2 makes L5 connect P4 to P2.
A2 go to P4.
A2 makes L6 connect P4 to P3.
A2 makes L2 fuse to L4 at P5.
W2 is built.
P5 is built.
A2 goes to P5. ").

example_mentalese(startup_act_1, "
there exists scene A1P.
there exists scene A2P.
A1P contains spider_1st_person.
A2P contains spider_2nd_person.

A1 can see map of A1P.
A2 can see map of A2P.
A1 can not see map of A2P.
A2 can not see map of A1P
A1 imagines A2P.


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
%      A1                  N5  %
%                              %
%         Q3  N6          A2 Q1%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%").

extract_objects(Atr,Objs):- atom_chars(Atr,Chars),extract_objects(Chars,0,0,Objs),ground(Objs).

extract_objects([],_,_,[]):-  !.
extract_objects(['\n'|Atr],_,Y,Objs):- !,
  Y2 is Y+1, extract_objects(Atr,0,Y2,Objs).
extract_objects([' '|Atr],X,Y,Objs):-  !,
  X2 is X+1, extract_objects(Atr,X2,Y,Objs).

extract_objects([A,A1|Atr],X,Y,Objs):- 
   ascii_2_obj([A,A1],X,Y,Os),Os\==[],
   X2 is X+2, extract_objects(Atr,X2,Y,OOs),
   append(Os,OOs,Objs).
extract_objects([A|Atr],X,Y,Objs):- 
   ascii_2_obj([A],X,Y,Os),!,
   X2 is X+1, extract_objects(Atr,X2,Y,OOs),
   append(Os,OOs,Objs).
extract_objects([_|Atr],X,Y,Objs):- 
   X2 is X+1, extract_objects(Atr,X2,Y,Objs),!.
  

ascii_2_obj([A,N],X,Y,[obj(Var,X,Y)]):- !, char_type(N,digit),!, atom_concat(A,N,Var).
ascii_2_obj([' '],_,_,[]):-!.
ascii_2_obj([A],X,Y,[obj(A,X,Y)]).
:- use_module(library(logicmoo_nlu)).
%?- c80("A1 attaches W1 to Q1.").


objs_2_ascii(Objs,Atr):- objs_2_ascii([],0,0,Objs,Text),text_to_string(Text,Atr).

objs_2_ascii(Atr,X,Y,Objs,AtrOut):- select(obj('%',X,Y),Objs,RObjs), append(Atr,['%'],Atr0),
   X2 is X+1, objs_2_ascii(Atr0,X2,Y,RObjs,AtrOut).
objs_2_ascii(Atr,X,Y,Objs,AtrOut):- select(obj(N,X,Y),Objs,RObjs), atom_chars(N,[C1,C2]),!,
   append(Atr,[C1,C2],Atr0),
   X2 is X+2, objs_2_ascii(Atr0,X2,Y,RObjs,AtrOut).
objs_2_ascii(Atr,X,Y,Objs,AtrOut):- select(obj(N,X,Y),Objs,RObjs), atom_chars(N,[C1]),!,
   append(Atr,[C1],Atr0),
   X2 is X+1, objs_2_ascii(Atr0,X2,Y,RObjs,AtrOut).
objs_2_ascii(Atr,_,Y,Objs,Atr):- forall(member(obj(_,_,OY),Objs),OY<Y),!.
objs_2_ascii(Atr,X,Y,Objs,Out):- forall(member(obj(_,OX,_),Objs),OX<X),!,
   append(Atr,['\n'],Atr10), Y2 is Y+1, objs_2_ascii(Atr10,0,Y2,Objs,Out).
objs_2_ascii(Atr,X,Y,Objs,Out):- 
   append(Atr,[' '],Atr10), X2 is X+1, 
   objs_2_ascii(Atr10,X2,Y,Objs,Out).


:- forall(map_of(Name,Map),(
    must_det_l((writeln(1=Name),
       extract_objects(Map,Objs),!,
       wdmsg(Objs),
       objs_2_ascii(Objs,Ascii))),!,
       writeln((Ascii)))).
%:- break.

example_mentalese(spider_1st_person, "
A1 is me.
E1 is here.
E1 is simular to E2.
; A1 sees E1 as E2.
;   Therefore E1 has a place Q1, Q2, Q3, and Q4 that is for building a web W1.
A1 is simular to A2.
W1 is simular to W2.
W1 is a thing.
W1 is a spider web.


;; the rest below has to be guessed by system

A1 does attach W1 to Q1.
W1 is attached to Q1.
W1 has begun being built.
W1 is attached to Q2.
W1 is attached to Q3.
W1 is attached to Q4.
A1 makes N1 connect Q1 to Q2.
A1 makes N2 connect Q2 to Q3.
A1 makes N3 connect Q3 to Q1.
A1 makes N4 connect Q1 to Q4.
A1 makes N5 connect Q4 to Q2.
A1 go to Q4.
A1 makes N6 connect Q4 to Q3.
A1 makes N2 fuse to N4 at Q5.
W1 is built.
A1 go to Q5.
").

rexample_mentalese:- forall(example_mentalese(N,V), (dmsg(example_mentalese=N),c88(V))).

