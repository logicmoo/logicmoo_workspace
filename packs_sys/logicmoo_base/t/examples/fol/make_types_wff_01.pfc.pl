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
%:- set_prolog_flag_until_eof(runtime_speed,0). % but dont gripe about speed
:- kif_compile.

% =================================================================================
% Define a couple classes
% =================================================================================

% this creates the servant datatype
==> all(X, if(isa(X,servant),livesAt(X, green_house))).

% this creates the human datatype as well as functions that in the future that are create to accept humans must also accept servants 
==> all(X, if(livesAt(X, green_house),isa(X,human))).

% =================================================================================
% Test 1
% =================================================================================

:- mpred_trace_exec.


% ?- mpred_test(all(X,if(isa(X,servant),isa(X,human)))).

==> isa(fred,servant).  

:- mpred_test(isa(fred,human)).

%?- all(X,t(OP,isa(X,human),~isa(X,servant))).
%OP = {A,B}/( if(~A,B) ).

:- mpred_test(all(X,if(isa(X,servant),isa(X,human)))). 
:- mpred_test(all(X,if(~isa(X,human),~isa(X,servant)))). 
% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/67 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/make_types_wff_01.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/MAKE_TYPES_WFF_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3AMAKE_TYPES_WFF_01 

