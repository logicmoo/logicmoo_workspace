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


:- sanity(is_pfc_file).


:- mpred_unload_file.

:- file_begin(pfc).
:- expects_dialect(pfc).

:- sanity(is_pfc_file).

:- set_fileAssertMt(baseKB).
% ensure this file does not get unloaded with mpred_reset
:- prolog_load_context(file,F), ain(mpred_unload_option(F,never)).

meta_argtypes(mdefault(ftAssertable)).

% BWD chaining
mdefault((Q <- P))/mpred_literal(Q) ==> (Q <-(P, \+ ~(Q))).

% FWD chaining
mdefault(P==>Q)/nonvar(Q) ==> (((P ==> mdefault(Q)))).

% NEG chaining
mdefault(~Q)/mpred_positive_fact(Q)  ==>  (( \+ Q ) ==> ~ Q ).


% POS chaining 1 (Neg Trigger)
mdefault(Q)/(mpred_positive_fact(Q),if_missing_mask(Q,R,Test)) ==> (  ( ( \+R /(ground(R),Test), (\+ ~Q )) ==> Q )).

% POS chaining 2 (Pos Trigger)
mdefault(Q)/(mpred_positive_fact(Q),if_missing_mask(Q,R,Test)) ==> ( ((R/(ground(R), Test, \+(R=Q))) ==> (\+ Q))).


% mdefault(Q) ==> if_missing(Q,Q).
%(mdefault(P=>Q)/(mpred_literal_nv(Q),if_missing_mask(Q,R,Test)))  ==> ((P, \+ R/Test) => Q).
%(mdefault(P=>Q)/nonvar(Q)) ==> (P => mdefault(Q)).

%:- listing(baseKB:mdefault/1).
%:- listing(( (==>) /2)).

:- if(baseKB:startup_option(datalog,sanity);baseKB:startup_option(clif,sanity)).

% :- reconsult(pack(logicmoo_base/t/examples/pfc/'sanity_birdt.pfc')).

:- endif.
