:- include(test_header).



% =================================================================================
% Load the system
% =================================================================================

:- set_lang(clif).
:- begin_pfc.

% =================================================================================
% Set our engine up
% =================================================================================

% deduce instances from usages in args having the effect of deducing human,dwelling,beverage_class are classes
==> feature_setting(make_wff,true).
% set truth maintainance system to remove previous assertions that new assertions disagree with 
==> feature_setting(tms_mode,remove_conflicting).

:- set_prolog_flag(runtime_debug,3). % mention it when we remove previous assertions

:- set_prolog_flag_until_eof(do_renames,mpred_expansion).

:- kif_compile.

% =================================================================================
% Define a couple predicates
% =================================================================================

:-kb_shared(livesAt/2).
% maximum cardinality of livesAt/2 is 1
==> isa(livesAt,'FunctionalBinaryPredicate').
==> argIsa(livesAt,1,human).
==> argIsa(livesAt,2,dwelling).

singleValuedInArg(livesAt,2).

% define drinks/2
:-kb_shared(drinks/2).
==> argIsa(drinks,1,human).
==> argIsa(drinks,2,beverage_class).

% =================================================================================
% But given the above: 
%
%   Only things that possibly can drink coffee live in the green house?
%  
%   Only currently individuals whom are not living in the red house live in the green?
%
% =================================================================================

:- mpred_trace_exec.

all(X, livesAt(X, green_house) & drinks(X, coffee)).

% this should have meant: 

%:- show_kif_to_boxlog(all(X, (poss(livesAt(X, green_house) & drinks(X, coffee))) => livesAt(X, green_house) & drinks(X, coffee))).

%:- show_kif_to_boxlog(all(X, (poss(livesAt(X, green_house)) & poss(drinks(X, coffee))) => livesAt(X, green_house) & drinks(X, coffee))).

% =================================================================================
% poss / ~poss sanity tests
% =================================================================================

:- dynamic(a/1).

:- mpred_test(poss(a(b))).

poss(a(b)).

:- mpred_test(poss(a(b))).

~poss(a(b)).

:- mpred_test(\+ poss(a(b))).

:- mpred_test(~ poss(a(b))).

:- listing(poss).

:- break.

% =================================================================================
% Bob Test ~poss
% =================================================================================


:- mpred_test(poss(livesAt(bob,green_house))).

:- mpred_test(poss(drinks(bob,coffee))).

:- mpred_test(livesAt(bob,green_house)).

:- mpred_trace_exec.

~poss(drinks(bob,coffee)).

:- mpred_test(\+ poss(drinks(bob,coffee))).

:- mpred_test(\+ livesAt(bob,green_house)).


:- mpred_test(~poss(drinks(bob,coffee))).


% =================================================================================
% Fred Test ~poss
% =================================================================================

~poss(livesAt(fred,green_house)).

% Does fred drink coffee? (should be unknown)
:- mpred_test(\+ drinks(fred,coffee)).


% =================================================================================
% Joe Test ~poss
% =================================================================================

poss(livesAt(joe,green_house)).

:- mpred_test(drinks(joe,coffee)).


% =================================================================================
%  Joe Test - Some changing facts about the world
% =================================================================================
:- mpred_trace_exec.

livesAt(joe,red_house).



% Does anyone live at the green house? (Should be none?)
:- mpred_test(\+ livesAt(_X,green_house)).

% Can anyone live at the green house? (Should be every cant?)
:- mpred_test(~poss(livesAt(_X,green_house))).




