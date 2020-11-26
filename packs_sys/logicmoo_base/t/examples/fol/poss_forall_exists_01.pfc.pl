:- include(test_header).



% =================================================================================
% Load the system
% =================================================================================



:- make.

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




