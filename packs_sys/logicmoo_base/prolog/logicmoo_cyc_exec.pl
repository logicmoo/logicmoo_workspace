/* <module> logicmoo_plarkc - special module hooks into the logicmoo engine allow
%   clif syntax to be recocogized via our CycL/KIF handlers
%
% Logicmoo Project: A LarKC Server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:- module(lm_cyc,[cyc_exec/1,cyc_exec/2]).

% ============================================
% Prolog to Cyc Predicate Mapping
%
%  the following will all do the same things:
%
% :- decl_mpred('BaseKB':isa/2). 
% :- decl_mpred('BaseKB':isa(_,_)). 
% :- decl_mpred(isa(_,_),'BaseKB'). 
% :- decl_mpred('BaseKB',isa,2). 
%
%  Will make calls 
% :- isa(X,Y)
%  Query into #$BaseKB for (#$isa ?X ?Y) 
%
% decl_mpred/N
%
% ============================================

:- dynamic(lmcache:isCycUnavailable_known/1).

/*
https://www.dropbox.com/sh/9jexwgm9amw80mj/AADieFX-yQ_p6AfoF-Yy4muAa?dl=0

https://www.dropbox.com/sh/n506umkk6tqqhkm/AACFbLDyCAkf392zE2Z05u2ta?dl=0

:- was_export(isCycAvailable/0).
isCycAvailable:-lmcache:isCycUnavailable_known(_),!,fail.
isCycAvailable:-lmcache:isCycAvailable_known,!.
isCycAvailable:-checkCycAvailablity,isCycAvailable.

:- was_export(isCycUnavailable/0).
isCycUnavailable:-lmcache:isCycUnavailable_known(_),!.
isCycUnavailable:-lmcache:isCycAvailable_known,!,fail.
isCycUnavailable:-checkCycAvailablity,isCycUnavailable.

:- was_export(checkCycAvailablity/0).
checkCycAvailablity:- (lmcache:isCycAvailable_known;lmcache:isCycUnavailable_known(_)),!.
checkCycAvailablity:- catchv((current_predicate(invokeSubL/2),ignore((invokeSubL("(+ 1 1)",R))),(R==2->assert_if_new(lmcache:isCycAvailable_known);assert_if_new(lmcache:isCycUnavailable_known(R)))),E,assert_if_new(lmcache:isCycUnavailable_known(E))),!.
*/






%:- use_module(library(jpl)).


cyc_exec(X):-cyc_exec(X,_).

cyc_exec(X,X).

:- fixup_exports.


