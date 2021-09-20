:- include(test_header).


/*
:- use_module(library(pce)).
:- use_module(library(gui_tracer)).
:- use_module(library(pce_debug)).
:- use_module(library(pce_emacs)).
:- use_module(library(trace/trace)).
:- use_module(library(emacs/window)).
:- use_module(library(emacs/emacs)).
:- use_module(library(swi_ide)).
:- emacs_toplevel.
% :- guitracer,leash(-all),visible(-all),trace,member(_,[_]),!,notrace,leash(+all),visible(+all).
*/

:- expects_dialect(pfc).

:- ensure_mpred_file_loaded('logicmoo/pfc/autoexec.pfc').
% :- ensure_mpred_file_loaded('logicmoo/pfc/system_genls.pfc').
:- mpred_trace_exec.
% :- mpred_autoload(genls/2).

tCol(tFly).
tCol(tCanary).
tCol(tPenguin).
tCol(tBird).

genls(tCanary,tBird).
genls(tPenguin,tBird).


% chilly is a penguin.
tPenguin(iChilly).

:-mpred_test((tBird(iChilly))).


% tweety is a canary.
tCanary(iTweety).

:-mpred_test((tBird(iTweety))).


% birds fly by default.
(mdefault(( tBird(X) ==> tFly(X)))).

:-mpred_test((isa(I,tFly),I=iChilly)).

:-mpred_test((tFly(iTweety))).
:-mpred_test((tFly(iChilly))).


% penguins do  ~ tFly.
tPenguin(X) ==>  ~(tFly(X)).

:-mpred_test((\+ tFly(iChilly))).
:-mpred_test(( ~(tFly(iChilly)))).




% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/415 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/zenls_01z.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/ZENLS_01Z/logicmoo_base_examples_fol_ZENLS_01Z_JUnit/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3AZENLS_01Z 

