:- include(test_header).

%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% props(Obj,[height(ObjHt)]) == t(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == t(height(Obj,ObjHt)).
% padd(Obj,[height(ObjHt)]) == prop_set(height,Obj,ObjHt,...) == ain(height(Obj,ObjHt))
% [pdel/pclr](Obj,[height(ObjHt)]) == [del/clr](height,Obj,ObjHt) == [del/clr]svo(Obj,height,ObjHt) == [del/clr](height(Obj,ObjHt))
% keraseall(AnyTerm).
%
%                      ANTECEEDANT                                   CONSEQUENT
%
%        P =         test nesc true                         assert(P),retract(~P) , enable(P).
%       ~P =         test nesc false                        assert(~P),retract(P), disable(P).
%
%   ~ ~(P) =         test possible (via not impossible)      retract(~P), enable(P).
%  \+ ~(P) =         test impossiblity is unknown            retract(~P).
%   ~\+(P) =        same as P                               same as P
%    \+(P) =        test naf(P)                             retract(P)
%
% Dec 13, 2035
% Douglas Miles

:- expects_dialect(pfc).


tCol(tFly).
tCol(tCanary).
tCol(tPenguin).


tCol(tBird).


:- mpred_test(predicate_property(tBird(_),dynamic)).

genls(tCanary,tBird).
genls(tPenguin,tBird).



:- dmsg("chilly is a penguin.").
tPenguin(iChilly).

:- mpred_test((tBird(iChilly))).



:- dmsg("tweety is a canary.").
tCanary(iTweety).

:- mpred_test((tBird(iTweety))).


:- dmsg("birds fly by default.").
mdefault(( tBird(X) => tFly(X))).

:- dmsg("make sure chilly can fly").
:- mpred_test((isa(I,tFly),I=iChilly)).

:- dmsg("make sure tweety can fly (and again chilly)").
:- mpred_test((tFly(iTweety))).
:- mpred_test((tFly(iChilly))).


:- dmsg("penguins do not tFly.").
tPenguin(X) =>  ~tFly(X). 

:- dmsg("confirm chilly now cant fly").
:- mpred_test((\+ tFly(iChilly))).
:- mpred_test(( ~tFly(iChilly))).

%= repropigate that chilly was a bird again
tBird(iChilly).

%= this helps show the real differnce in ~and \+ 
:- dmsg("confirm chilly still does not fly").
:- mpred_test((\+ tFly(iChilly))).
:- dmsg("confirm chilly still cant fly").
:- mpred_test(( ~tFly(iChilly))).

/*

This wounld be a good TMS test it should throw.. but right now it passes wrongly
tFly(iChilly).

:- dmsg("confirm chilly is flying penguin").
:- mpred_test(( tFly(iChilly))).
:- mpred_test(( tPenguin(iChilly))).
:- mpred_test((\+ ~tFly(iChilly))).

\+ tFly(iChilly).

:- dmsg("confirm chilly is a normal penguin who cant fly").
:- mpred_test((\+ tFly(iChilly))).

% fails rightly
:- mpred_test(( tPenguin(iChilly))).

*/

:- dmsg("chilly is no longer a penguin").
:- debug_logicmoo(logicmoo(_)).

:- mpred_trace_exec.

\+ tPenguin(iChilly).

:- mpred_test(( \+ tPenguin(iChilly))).

:- dmsg("chilly is still a bird").
:- mpred_test((tBird(iChilly))).

:- repropagate(tBird(iChilly)).

:- dmsg("confirm chilly is flying bird").
:- mpred_test(( tFly(iChilly))).
:- mpred_test(( \+ tPenguin(iChilly))).
:- mpred_test(( \+ ~tFly(iChilly))).



% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/76 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/fol_birdt_01.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/FOL_BIRDT_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3AFOL_BIRDT_01 

