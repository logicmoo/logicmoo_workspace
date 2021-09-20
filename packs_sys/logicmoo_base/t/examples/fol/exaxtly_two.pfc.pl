
% test dealing  with instances of type counts with modality

:- module(cute6,[]).

:- include(test_header).
:- user:use_module(library(editline)).
:- use_module(library(occurs)). % sub_term/2
:- use_module(library(sort)). % predsort/3
:- use_module(library(backcomp)). % concat_atom/2
:- user:autoload.

:- module_transparent(system: = /2).
:- module_transparent('$attvar':'$wakeup'/1).
:- module_transparent('$attvar':'call_all_attr_uhooks'/2).
:- module_transparent('$attvar':'begin_call_all_attr_uhooks'/2).
:- module_transparent('$attvar':'uhook'/3).

:- '$current_source_module'(M),install_retry_undefined(M,error).
:- install_retry_undefined(user,error).
:- install_retry_undefined(kbii,error).
:- install_retry_undefined(kbi,error).
% :- set_prolog_flag(autoload,false).
:- set_prolog_flag(retry_undefined, false).
:- set_prolog_flag(access_level, system).

% Option Examples: nesc($sentence),  poss($sentence),  poss($sentence)=>nesc($sentence).
% ==> feature_setting(default_modality,nesc($sentence)).

/*

Feature Notes:

P.  % P happens to be the feature_setting default_modality
poss(P).  % possibly P
nesc(P).  % necessarily P
~nesc(P).  % not necessarily P
nesc(~P).  % necessarily not P
~poss(P).  % not possibly P
poss(~P).  % possibly not P

poss(P)=>nesc(P).  % P is true by default (allows other axioms to override)
poss(P)&~nesc(P).  % possibly, but not necessarily P

~naf(P).  % P is default
naf(~P).  % possibly P
naf(P).  % possibly not P

there are many Logically equivalent settings like   

~poss(P)    == nesc(~P)

falsify(~P) ==  poss(P) v nesc(P).

*/

test_sanity(G):- 
  add_boxlog_history(G),
  test_sanity0(G),!.

test_sanity0(G):- mpred_test(G),!.
test_sanity0((A,B)):- !, mpred_test(A),test_sanity0(B).

:- kbi_define(cute/1).
:- kbi_define(ugly/1).

:- kb_shared(baseKB:cute/1).
:- kb_local(ugly/1).
:- kb_local(isa/2).
:- kbi_define(poss/1).


%===== axioms =======


:- set_prolog_flag(debugger_write_options,[fullstop(true), ignore_ops(true), quoted(true), portray(true), max_depth(100000), attributes(dots)]).


:- test_boxlog([+assert],exactly(3, X, exactly3(X))).
:- test_boxlog([+assert],atleast(3, X, least3(X))).
:- test_boxlog([+assert],atmost(3, X, most3(X))).

:- test_boxlog([+assert],atleast(2, X, least2(X))).
:- test_boxlog([+assert],atmost(2, X, most2(X))).

:- test_boxlog([+assert],exactly(2, X, exactly2(X))).

:- add_boxlog_history((xnr_var(X),exactly3(X))).
:- add_boxlog_history((exactly2(X),exactly3(X))).
:- add_boxlog_history((exactly3(X),exactly2(X))).


:- add_boxlog_history((exactly2(X),exactly2(Y),exactly2(Z),Y\=Z,X==Z)).
:- add_boxlog_history((findall(X,exactly3(X),L1),findall(X,exactly3(X),L2),sort(L1,S1),sort(L2,S2),S1==S2)).
:- add_boxlog_history((exactly3(X),X=X)).


