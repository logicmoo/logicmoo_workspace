:- include(test_header).



% =================================================================================
% Load the system
% =================================================================================

:- expects_dialect(clif).

% =================================================================================
% Set our engine up
% =================================================================================


% deduce instances from usages in args having the effect of deducing human,dwelling,beverage_class are classes
==> feature_setting(make_wff,true).
% set truth maintainance system to remove previous assertions that new assertions disagree with 
==> feature_setting(tms_mode,remove_conflicting).

:- set_prolog_flag(runtime_debug,3). % mention it when we remove previous assertions

:- set_prolog_flag_until_eof(do_renames,mpred_expansion).


% =================================================================================
% Define a couple predicates
% =================================================================================

:- set_prolog_flag(gc,false).

:- mpred_trace_all.
:- mpred_trace_exec.

exists(X, livesAt(X, green_house) & drinks(X, coffee)).

:- listing([clif,modal_clif,boxlog/1]).
:- break.
:- kif_compile.
:- listing([clif,modal_clif,boxlog/1]).
:- break.
% Does anyone live at the green house? (Should be one right?)
:- mpred_test(livesAt(_X,green_house)).


% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/425 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/exists_04.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/EXISTS_04/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3AEXISTS_04 

