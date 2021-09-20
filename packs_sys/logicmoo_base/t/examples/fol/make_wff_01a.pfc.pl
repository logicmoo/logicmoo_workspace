:- include(test_header).

% =================================================================================
% Set our engine up
% =================================================================================

:- expects_dialect(clif).
% deduce instances from usages in args having the effect of deducing human,dwelling,beverage_class are classes
==> feature_setting(make_wff,true).
==> feature_setting(add_admitted_arguments,true).
% set truth maintainance system to remove previous assertions that new assertions disagree with 
==> feature_setting(tms_mode,remove_conflicting).
:- set_prolog_flag(runtime_debug,3). % mention it when we remove previous assertions
:- set_prolog_flag_until_eof(do_renames,mpred_expansion).
:- kif_compile.



% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/make_wff_01a.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/MAKE_WFF_01A/logicmoo_base_examples_fol_MAKE_WFF_01A_JUnit/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3AMAKE_WFF_01A 

% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/612
