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

:- nop(module( baseKB)).
:- nop('$set_source_module'( baseKB)).



% =================================================================================
% Define a couple predicates
% =================================================================================

:-kb_shared(livesAt/2).
% maximum cardinality of livesAt/2 is 1
==> isa(livesAt,'FunctionalBinaryPredicate').
==> argIsa(livesAt,1,human).
==> argIsa(livesAt,2,dwelling).

% define drinks/2
:-kb_shared(drinks/2).
==> argIsa(drinks,1,human).
==> argIsa(drinks,2,beverage_class).

% =================================================================================
% But given the above: 
%
%   Only things that possibly can drink coffee live in the green house?
%
% =================================================================================

:- show_kif_to_boxlog(all(X, livesAt(X, green_house) & drinks(X, coffee))).

% this should have meant: 

:- show_kif_to_boxlog(all(X, (poss(livesAt(X, green_house) & drinks(X, coffee))) => livesAt(X, green_house) & drinks(X, coffee))).

:- show_kif_to_boxlog(all(X, (poss(livesAt(X, green_house)) & poss(drinks(X, coffee))) => livesAt(X, green_house) & drinks(X, coffee))).

~poss(livesAt(fred,green_house)).

% Does fred drink coffee? (should be unknown)
:- \+ drinks(fred,coffee).

poss(livesAt(joe,green_house)).
:- drinks(joe,coffee).


% =================================================================================
% These two assertions are a bit weird but are for fun
% =================================================================================

% all objects in the universe that may drink coffee do drink coffee
%all(X, if(poss(drinks(X, coffee)),drinks(X, coffee))).

% all objects in the universe that may live in the green house do live in the green house
%all(X, if(poss(livesAt(X, green_house)),lives(X, green_house) )).





% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/73 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/poss_forall_exists_01.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/POSS_FORALL_EXISTS_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3APOSS_FORALL_EXISTS_01 

