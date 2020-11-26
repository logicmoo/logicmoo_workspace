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
/*

 exists(X, lives(X, green) /\ drinks(X, coffee)).

  

*/

% swipl -g "ensure_loaded(pack(logicmoo_base/t/examples/csp/'einstein_simpler.pfc'))."

:- ensure_loaded(library(logicmoo_user)).

:- file_begin(pfc).

:- op(600,xfy, (/\)).
:- op(0,xfx,'=>').
:- op(1150,xfy,'=>').

never_assert_u(boxlog((lives(A, _):-neighbor(A, _))),singletons).

% add this to our vocab
props((/\),ftSentenceOp,tLogicalConjunction).

% Source http://www.iflscience.com/editors-blog/solving-einsteins-riddle

%= There are five houses in a row.
exists(H1,exists(H2,exists(H3,exists(H4,exists(H5,
  leftof(H1, H2) /\ leftof(H2, H3) /\ leftof(H3, H4) /\ leftof(H4, H5)))))).

%= In each house lives a person with a unique nationality.
% we write this in SUMO
% all(H,exists(P,exists(U,(house(H) => (person(P) /\ lives(P, H) /\ unique(U,nationality(P,U))))))).
% SANITY count the persons (shouild be 5)
% :- sanity(( findall(P,person(P),L),length(L,5))).

% Helper functions
% 
(lives(PA, HA) /\ (leftof(HA, HB); leftof(HB, HA)) /\ lives(PB, HB)) <=> neighbor(PA,PB).

meta_argtypes(lives(person,house)).
meta_argtypes(pet(person,animal)).
meta_argtypes(position(person,int)).
meta_argtypes(smokes(person,brand)).
meta_argtypes(drinks(person,beverage)).
meta_argtypes(leftof(house,house)).

exists([P1,I1,I2,P2,H1,H2],
  ((position(P1,I1) /\ lives(P1,H1) /\ leftof(H1,H2) /\ ({plus(I1, 1, I2)}) /\ position(P2,I2) /\ lives(P2,H2)))).

% Other facts:
% 
%= 1. The Brit lives in the red house. 
lives(englishman, red).

:- break.

% forward chain these into houses
leftof(HA, HB) ==> (house(HA) , house(HB)).

:- break.

%= 2. The Swede keeps dogs as pets. 
pet(swede, dogs).

%= 3. The Dane drinks tea. 
drinks(dane, tea).

%= 4. The green house is on the immediate left of the white house. 
leftof(green, white).

%= 5. The green house's owner drinks coffee. 
exists(X, lives(X, green) /\ drinks(X, coffee)).

%= 6. The owner who smokes Pall Mall rears birds. 
exists(X, smokes(X, pallmalls) /\ pet(X, birds)).

%= 7. The owner of the yellow house smokes Dunhill. 
exists(X, lives(X, yellow) /\ smokes(X, dunhills)).

%= 8. The owner living in the center house drinks milk. 
exists(X, position(X, 3) /\ drinks(X, milk)).

%= 9. The Norwegian lives in the first house. 
position(norwegian, 1).

%= 10. The owner who smokes Blends lives next to the one who keeps cats. 
exists(X,exists(Y, smokes(X, blend) /\ neighbor(X, Y) /\ pet(Y, cat))).

%= 11. The owner who keeps the horse lives next to the one who smokes Dunhill. 
exists(X,exists(Y ,pet(X, horses) /\ neighbor(X, Y) /\ smokes(Y, dunhill))).

%= 12. The owner who smokes Bluemasters drinks beer. 
exists(X, smokes(X, bluemasters) /\ drinks(X, bier)).

%= 13. The German smokes Prince. 
smokes(german, prince).

%= 14. The Norwegian lives next to the blue house. 
exists(X, neighbor(norwegian, X) /\ lives(X, blue)).

%= 15. The owner who smokes Blends lives next to the one who drinks water. 
exists(X,exists(Y,smokes(X, blends) /\ neighbor(X,Y) /\ drinks(Y, water))).



%= The five owners drinks a certain type of beverage, smokes a certain brand of
%= cigar and keep a certain pet. 

trait(drinks). trait(smokes). trait(pet).
trait(position). % we add position 

:- if(true).  % No HiLog

all(P,
 all(Trait,
   exists(Value,
    person(P) => (trait(Trait) =>  t(Trait,P,Value))))).

% No owners have the same pet, smokes the same
% brand of cigar, or drinks the same beverage.
different_insts(person,PA,PB) /\ trait(Trait) /\ t(Trait,PA,What) => ~t(Trait,PB,What).

:- else.  % Yes HiLog
/*
:- set_functor_wrap(t).
% the '&' functor next means to expand uppercase 

all(P,
 all(Trait,
   exists(Value,
    person(P) => (trait(Trait) =>  $Trait(P,Value))))).

% No owners have the same pet, smokes the same
% brand of cigar, or drinks the same beverage.
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

%= no two houses are the same color
% five different colors
house(red). house(green). house(white). house(yellow). house(blue).

/* 
 other examples might be...

     % or any two people have differnt same trait values
     dif_people(PA,PB) /\ trait(Trait) /\ Trait(PA,WhatA) /\ Trait(PB,WhatB) => different(WhatA,WhatB).
*/      



% The question is: who owns the fish?

:- forall((C <== A) ,  (dynamic(C),ain(A ==> C))).


