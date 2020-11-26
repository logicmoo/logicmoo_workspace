%   File   : pfcdb.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Author :  Dan Corpron
%   Updated: 10/11/87, ...
%   Purpose: predicates to manipulate a pfc database (e.g. save,
%%	restore, reset, etc.0

:- module(pfcdb, []).
:- use_module(library(pfc_pack_xform)).

% pfcDatabaseTerm(P/A) is true iff P/A is something that pfc adds to
% the database and should not be present in an empty pfc database

pfcDatabaseTerm(support1/3):- ifNotDMiles(true,fail).
pfcDatabaseTerm(support2/3):- ifNotDMiles(true,fail).
pfcDatabaseTerm(support3/3):- ifNotDMiles(true,fail).
pfcDatabaseTerm(spft/3):- ifNotDMiles(fail,true).

pfcDatabaseTerm(pt/2).
pfcDatabaseTerm(bct/2).
pfcDatabaseTerm(nt/3).
pfcDatabaseTerm('==>'/2).
pfcDatabaseTerm('<==>'/2).
pfcDatabaseTerm('<-'/2).
pfcDatabaseTerm(pfcQueue/2).

% removes all forward chaining rules and justifications from db.

pfcReset :-
  pfcGetSupport(P,(F,Trigger)),
  pfcAddDbToHead(P,PDb),
  pfcRetractOrWarn(PDb),
  pfcRetractOrWarn(support1(P,F,Trigger)),
  pfcRetractOrWarn(support2(F,Trigger,P)),
  pfcRetractOrWarn(support3(Trigger,P,F)),
  fail.
pfcReset :-
  pfcDatabaseItem(T),
  pfcError("Pfc database not empty after pfcReset, e.g., ~p.~n",[T]).
pfcReset.

% true if there is some pfc crud still in the database.
pfcDatabaseItem(Term) :-
  pfcDatabaseTerm(P/A),
  functor(Term,P,A),
  clause(Term,_).

pfcRetractOrWarn(X) :-  retract(X), !.
pfcRetractOrWarn(X) :- 
  pfcWarn("Couldn't retract ~p.",[X]).


:- fixup_exports.
