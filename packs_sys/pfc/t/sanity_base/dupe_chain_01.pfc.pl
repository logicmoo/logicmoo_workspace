
:- if(\+ exists_source(library(logicmoo_utils_all))).
:-  prolog_load_context(directory,X),absolute_file_name('../../..',O,[relative_to(X),file_type(directory)]),
    attach_packs(O).
:- endif.

:- if(\+ exists_source(library(pfc_lib))).
:-  prolog_load_context(directory,X),absolute_file_name('../../prolog',O,[relative_to(X),file_type(directory)]),
    asserta(user:file_search_path(library,O)).
:- endif.


:- nop(module( baseKB)).
:- expects_dialect(pfc).
:- set_fileAssertMt(baseKB).
:- ensure_loaded(library(logicmoo_test)).

:- expects_dialect(pfc).
% :- mpred_trace_exec.

notice_fc(P) ==>  ( P ==> {wdmsg(notice_fc(P))}).


notice_fc(a(1)).
notice_fc(b(2)).
notice_fc(c(3)).

c(3)==>a(1).
a(1)==>c(3).
c(3)==>c(3).

b(2).
% c(3) ==> {cls,C=c(3),wdmsg(ttExpressionTypeB1(C)),dumpST,wdmsg(ttExpressionTypeC1(C)),break}.
b(2)==>c(3).

% :- break.


end_of_file.

ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/395 
EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/pfc/t/sanity_base/dupe_chain_01.pfc.pl 
JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.pfc.test.sanity_base/DUPE_CHAIN_01/ 
ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ADUPE_CHAIN_01 

