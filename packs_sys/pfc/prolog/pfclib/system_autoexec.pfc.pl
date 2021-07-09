/*
:- if((set_prolog_flag(expect_pfc_file,always), current_prolog_flag(xref,true) ;  current_prolog_flag(pfc_booted,false) )).
%:- module(system_autoexec,[]).
:- else.
:- pfc_lib:use_module(library(pfc_lib)).
:- set_fileAssertMt(baseKB).
% :- '$set_source_module'(baseKB).
:- endif.
*/ 
/** <module> system_autoexec
% =============================================
% File 'system_autoexec.pfc'
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

%:- use_module(library(no_repeats)).

%:- use_module(library(pfc)).
:- add_to_search_path(pfclib,'.').
:- expects_dialect(pfc).
:- must(prolog_load_context(dialect,pfc)).
  /*
  :-
         op(990,xfx,(':=')),
         op(250,yfx,('?')),
         op(1,fx,('$')),
         op(200,fy,('@')),
         op(100,yfx,('.')),
         op(400,yfx,('rdiv')),
         op(1150,fx,('meta_predicate')),
         op(400,yfx,('//')),
         op(500,yfx,('/\\')),
         op(1200,fx,('?-')),
         op(1150,fx,('module_transparent')),
         op(1150,fx,('multifile')),
         op(1150,fx,('public')),
         op(1150,fx,('thread_initialization')),
         op(200,fy,('-')),
         op(500,yfx,('-')),
         op(700,xfx,('=:=')),
         op(1150,fx,('thread_local')),
         op(700,xfx,('as')),
         op(700,xfx,('=\\=')),
         op(400,yfx,('mod')),
         op(700,xfx,('=@=')),
         op(700,xfx,('@>')),
         op(200,xfy,('^')),
         op(1200,xfx,('-->')),
         op(700,xfx,('=..')),
         op(1100,xfy,(';')),
         op(700,xfx,('>:<')),
         op(700,xfx,(':<')),
         op(700,xfx,('@<')),
         op(700,xfx,('@=<')),
         op(700,xfx,('@>=')),
         op(400,yfx,('div')),
         op(400,yfx,('/')),
         op(700,xfx,('\\=@=')),
         op(1150,fx,('discontiguous')),
         op(400,yfx,('rem')),
         op(700,xfx,('\\=')),
         op(1050,xfy,('->')),
         op(400,yfx,('>>')),
         op(200,fy,('\\')),
         op(900,fy,('\\+')),
         op(1105,xfy,('|')),
         op(700,xfx,('\\==')),
         op(200,xfx,('**')),
         op(1150,fx,('volatile')),
         op(500,yfx,('\\/')),
         op(1150,fx,('initialization')),
         op(400,yfx,('*')),
         op(1150,fx,('dynamic')),
         op(700,xfx,('>=')),
         op(700,xfx,('>')),
         op(200,fy,('+')),
         op(500,yfx,('+')),
         op(1050,xfy,('*->')),
         op(700,xfx,('=<')),
         op(700,xfx,('<')),
         op(700,xfx,('=')),
         op(700,xfx,('is')),
         op(600,xfy,(':')),
         op(400,yfx,('<<')),
         op(1200,fx,(':-')),
         op(1200,xfx,(':-')),
         op(400,yfx,('xor')),
      %   op(1000,xfy,(',')),
         op(700,xfx,('==')).
         */
/*
:-kb_shared((rtSententialOperator/1,
tReifiableFunction/1,
rtVariableArityRelation/1,
rtEvaluatableRelation/1,
tFunction/1,
rtCommutativeRelation/1,
prologHybrid/1,
rtUnaryPredicate/1,
first_std_provider/3)).
*/

%:- ensure_abox_hybrid(baseKB).

:- set_fileAssertMt(baseKB).

:- thread_local(t_l:disable_px/0).

:- must(retractall( t_l:disable_px)).
:- must(\+ t_l:disable_px).

:- system:use_module(library(dif)).
:- dynamic(mpred_unload_option/2).

%:- rtrace.
assert_if_newt(G):- (cwc,(clause_asserted_i(G)->true;call(assert,G))).
%:- break.
%:- nortrace,notrace.
:- sleep(1.0).

:-if(exists_file(bkb_neever)).

:- [bkb].

:- else.

:- flag(auto_exec_loaded,N,N+1),( N==0 -> true ; (dumpST,break)).

:- baseKB:ensure_loaded('system_base.pfc').

%mtHybrid(M) ==> {dmsg(note_mtHybrid(M))}.

% ensure this file does not get unloaded with mpred_reset
%:- mpred_trace_exec.
:- prolog_load_context(file,F), ain(mpred_unload_option(F,never)).

%:- mpred_notrace_exec.
%:- listing(mpred_unload_option/2).

baseKB:mtHybrid(baseKB).

:- baseKB:ensure_loaded('system_mdefault.pfc').
:- baseKB:ensure_loaded('system_module_inheritance.pfc').
:- baseKB:ensure_loaded('system_singleValued.pfc').

:- multifile(baseKB:locked_baseKB/0).
:- dynamic(baseKB:locked_baseKB/0).
:- asserta((baseKB:locked_baseKB)).

:- endif.

:- set_prolog_flag(expect_pfc_file,unknown).

end_of_file.


:- if(false).
:- statistics.
:- endif.

:- if(false).

% :- mpred_test(ensure_loaded('pttpFWC.pfc')).

save_m_ain(UM):- 
 doall((user:predicate_property(M:P,multifile),
 (( UM = M,
   ( \+ predicate_property(M:P, foreign)),
    once(predicate_property(M:P,imported_from(From))->true;From=M),
    functor(P,F,A),
    save_mfa_ain(From,F,A))))).

save_mf_ain(M,F):-
 forall(current_predicate(F,M:P),
 (functor(P,F,A),save_mfa_ain(M,F,A))).

:- dynamic(tmp:saved_mfa_ain/3).

save_mfa_ain(M,F,A):- tmp:saved_mfa_ain(M,F,A),!.
save_mfa_ain(M,F,A):- asserta(tmp:saved_mfa_ain(M,F,A)),
 functor(P,F,A),
 doall((system:clause(M:P,Body,_),
    ((Body==true-> 
      save_p_ain(M,P); save_p_ain(M,P:-Body))))).
      
%save_p_ain(M,(H:-B)):-!, format(':- assert_if_new(~q:',[M]), display(H), write(' :- '), display(B),writeln(').').
%save_p_ain(M,P):- format(':- assert_if_new(~q:',[M]), display(P),writeln(').').
save_p_ain(M,P):- display(:- call(assert_if_new(M:P))),writeln('.').

:- if(exists_file(bkb_neever)).
:- tell(bkb).
:- forall(predicateConventionMt(F, M),save_mf_ain(M,F)).
:- save_m_ain(lmconf).
% :- save_m_ain(lmcache).
:- save_m_ain(baseKB).
:- writeln('end_of_file.').
:- listing(tmp:saved_mfa_ain/3).
:- told.
:- endif.
:- endif.

