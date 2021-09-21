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

% :- kif_compile.

% =================================================================================
% Define a couple predicates
% =================================================================================

:- set_prolog_flag(gc,false).
% maximum cardinality of livesAt/2 is 1
:- mpred_trace_all.
:- mpred_trace_exec.
instance(livesAt,'FunctionalBinaryPredicate').

% thus implies
arity(livesAt,2).

domain(livesAt,1,human).
domain(livesAt,2,dwelling).

% define drinks/2
arity(drinks,2).
domain(drinks,1,human).
domain(drinks,2,beverage_class).

% =================================================================================
% Note these two assertions are implicit to the system and have no side effect
% =================================================================================

% all objects in the universe that do drink coffee, may drink coffee
all(X, if(drinks(X, coffee),poss(drinks(X, coffee)))).

% for any objects in the universe that live in the green house must obvously have that as a possibility
all(X, if(livesAt(X, green_house),poss(livesAt(X, green_house)))).

all(X, if(livesAt(X, Y),poss(livesAt(X, Y)))).

all(X, all(Y, all(Z, if((livesAt(X,Y),livesAt(X,Z)),equals(Y,Z))))).
% =================================================================================
% Some facts about the world
% =================================================================================

livesAt(joe,red_house).

:- listing([clif,modal_clif]).

:- kif_compile.

:- printAll(mpred_why(~poss(livesAt(joe,green_house)),_Why)).

%:- break.

%:- must(~poss(livesAt(joe,green_house))).

~poss(drinks(bob,coffee)).

~poss(livesAt(fred,green_house)).

% =================================================================================
% But given the above: 
%
%   Only things that possibly can drink coffee live in the green house?
%  
%   Only currently individuals whom are not living in the red house live in the green_house?
%
% =================================================================================
exists(X, livesAt(X, green_house) & drinks(X, coffee)).

:- break.

% Does anyone live at the green house? (Should be one right?)
:- mpred_test(livesAt(_X,green_house)).

% Can anyone live at the green house? (Should be everyone but the one listed above?)
:- mpred_test(~poss(livesAt(_X,green_house))).



% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/442 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/poss_forall_exists_04.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/POSS_FORALL_EXISTS_04/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3APOSS_FORALL_EXISTS_04 

