/* 
% =============================================
% File 'mpred_builtin.pfc'
% Purpose: Agent Reactivity for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface' 1.0.0
% Revision: $Revision: 1.9 $
% Revised At: $Date: 2002/06/27 14:13:20 $
% =============================================
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
%
% props(Obj,[height(ObjHt)]) == t(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == t(height(Obj,ObjHt)).
% padd(Obj,[height(ObjHt)]) == prop_set(height,Obj,ObjHt,...) == ain(height(Obj,ObjHt))
% [pdel/pclr](Obj,[height(ObjHt)]) == [del/clr](height,Obj,ObjHt) == [del/clr]svo(Obj,height,ObjHt) == [del/clr](height(Obj,ObjHt))
% keraseall(AnyTerm).
%
%                      ANTECEEDANT                                   CONSEQUENT
%
%         P =         test nesc true                         assert(P),retract(~P) , enable(P).
%       ~ P =         test nesc false                        assert(~P),retract(P), disable(P)
%
%   ~ ~(P) =         test possible (via not impossible)      retract( ~(P)), enable(P).
%  \+ ~(P) =         test impossiblity is unknown            retract( ~(P))
%   ~ \+(P) =        same as P                               same as P
%     \+(P) =        test naf(P)                             retract(P)
%
% Dec 13, 2035
% Douglas Miles
*/
% swipl -g "ensure_loaded(pack(logicmoo_base/t/examples/csp/'einstein.pfc'))."

:- module(zebra,[]).

:- ensure_loaded(library(logicmoo_user)).

:- op(600,xfy, (/\)).
:- op(0,xfx,'=>').
:- op(1150,xfy,'=>').

:- file_begin(pfc).

% add this to our vocab
props((/\),ftSentenceOp,tLogicalConjunction).

% Source http://www.iflscience.com/editors-blog/solving-einsteins-riddle

%= There are five houses in a row.
leftof(house1, house2). leftof(house2, house3). leftof(house3, house4). leftof(house4, house5).

% forward chain these into houses
leftof(HA, HB) ==> (house(HA) , house(HB)).

%= In each house lives a person with a unique nationality.
% we write this in SUMO

all(H,
 exists(P,
  exists(U,
  (house(H) => 
    (person(P) /\ lives(P, H) /\ unique(U,nationality(P,U))))))).

% SANITY count the persons (shouild be 5)
% :- sanity(( findall(P,person(P),L),length(L,5))).

% Helper functions
% 
% nextto/2 is symmetric
nextto(HA, HB) :- (leftof(HA, HB); leftof(HB, HA)).
%
% next door house (symmetricalness was inherited from nextto/2)
lives(P, H) /\ nextto(H, HB) => lives_nextto_house(P,HB).
% 
% helper - next door neighbours (symmetricalness was inherited from lives_nextto_house/2)
lives_nextto_house(P,HB) /\ lives(PB, HB) => next_door_neighbours(P,PB).



% Other facts:
% 
% 1. The Brit lives in the red house. 
nationality(P, brit) => (lives(P, H) => colored(H, red)).

% 2. The Swede keeps dogs as pets. 
nationality(P, swedish) => pet(P, dog).

% 3. The Dane drinks tea. 
nationality(P, danish) => drink(P, tea).

% 4. The green house is on the immediate left of the white house. 
exists(L,exists(R,colored(L, green) /\ leftof(L, R) /\ colored(R, white))).

% 5. The green house's owner drinks coffee. 
lives(P, H) /\ colored(H, green) => drink(P, coffee).

% 6. The owner who smokes Pall Mall rears birds. 
smoke(P, pallmall) => pet(P, bird).

% 7. The owner of the yellow house smokes Dunhill. 
lives(P, H) /\ colored(H, yellow) => smoke(P, dunhill).

% 8. The owner living in the center house drinks milk. 
lives(P, house3) => drink(P, milk).

% 9. The Norwegian lives in the first house. 
exists(P,nationality(P, norwegian) => lives(P, house1)).

% 10. The owner who smokes Blends lives next to the one who keeps cats. 
smoke(P, blend) => (next_door_neighbours(P,PB) /\ pet(PB, cat)).

% 11. The owner who keeps the horse lives next to the one who smokes Dunhill. 
pet(P, horse) => (next_door_neighbours(P,PB)  /\ smoke(PB, dunhill)).

% :- prolog.

% 12. The owner who smokes Bluemasters drinks beer. 
smoke(P, bluemasters) => drink(P, beer).

% 13. The German smokes Prince. 
nationality(P, german) => trait(P, smoke, prince).

% 14. The Norwegian lives next to the blue house. 
nationality(P, norwegian) => (lives_nextto_house(P, H) => colored(H, blue)).

% 15. The owner who smokes Blends lives next to the one who drinks water. 
smoke(P, blend) => (next_door_neighbours(P,PB) => drink(PB, water)).


% The five owners drink a certain type of beverage, smoke a certain brand of
% cigar and keep a certain pet. 

trait(drink). trait(smoke). trait(pet).
trait(nationality). % we add nationality 

:- if(true).  % No HiLog

all(P,
 all(Trait,
   exists(Value,
    person(P) => (trait(Trait) =>  t(Trait,P,Value))))).

% No owners have the same pet, smoke the same
% brand of cigar, or drink the same beverage.
different_insts(person,PA,PB) /\ trait(Trait) /\ t(Trait,PA,What) => ~t(Trait,PB,What).

:- else.  % Yes HiLog
/*
:- set_functor_wrap(t).
% the '&' functor next means to expand uppercase 

all(P,
 all(Trait,
   exists(Value,
    person(P) => (trait(Trait) =>  $Trait(P,Value))))).

% No owners have the same pet, smoke the same
% brand of cigar, or drink the same beverage.
different_insts(person,PA,PB) /\ trait(Trait) /\ Trait(PA,What) => ~Trait(PB,What).
*/
:- endif. % End HiLog

% Helper functions
% 
%
% same representation, (tested with quotedIsa/2) they may be eaier compared
same_repr(HA,HB) <- quotedIsa(HA, QCLASS) /\ quotedIsa(HB, QCLASS).

% different is when two terms of the same class using the same representation
different_insts(HCLASS,HA,HB) <- {dif:dif(HA , HB)} /\ isa(HA, HCLASS) /\ same_repr(HA,HB) /\ isa(HB, HCLASS).

% different is when two terms of the same class using the same representation
different(HA,HB) <- different_insts(_HCLASS, HA,HB).

% no two houses are the same color
different_insts(house,HA,HB) /\ colored(HA, C) => ~colored(HB, C).
% … five different colors
color(red). color(green). color(white). color(yellow). color(blue).

/* 
 other examples might be...

     % or any two people have differnt same trait values
     dif_people(PA,PB) /\ trait(Trait) /\ Trait(PA,WhatA) /\ Trait(PB,WhatB) => different(WhatA,WhatB).
*/      



% The question is: who owns the fish?

:- forall((C <== A) ,  (dynamic(C),ain(A ==> C))).


