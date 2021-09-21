/* <module>
%
%  PFC is a language extension for prolog.
%
%  It adds a new type of module inheritance
%
% Dec 13, 2035
% Douglas Miles
*/
%  was_module(header_sane,[]).

:- include(library(logicmoo_test_header)).

% :- set_defaultAssertMt(myMt).

:- expects_dialect(pfc).

arity(loves,2).
mtHybrid(socialMt).
baseKB:predicateConventionMt(loves,socialMt).


:- if((pfc_test_feature(mt,X=1),X==1)).

:- if(true).
mt1:like(sally,joe).
:- else.
:- rtrace(ain(==>mt1:like(sally,joe))).
%:- (ain(==>mt1:like(sally,joe))).
:- xlisting(like).
:- break.
:- rtrace(ain(==>mt1:like(sally,joe))).
:- break.
:- endif.

:- mt1:export(mt1:like/2).
:- header_sane:import(mt1:like/2).
:- xlisting(like/2).

genlMt(mt1,socialMt).

% this will raise upward the assertion.. is this OK?
:- (ain(like(A,B)==>loves(B,A))).

:- xlisting(loves/2).

:- mpred_must(socialMt:loves(joe,sally)).

:- endif.


% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/pfc/t/sanity_base/mt_02.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.pfc.test.sanity_base/MT_02/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3AMT_02 

% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/552
