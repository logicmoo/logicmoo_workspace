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
% temp test
% =================================================================================

ff1:- show_kif_to_boxlog(if(nesc(livesAt(X, green_house)),nesc(drinks(X, coffee)))).
ff2:- show_kif_to_boxlog(if((livesAt(X, green_house)),(drinks(X, coffee)))).

ff3:- show_kif_to_boxlog(nesc(drinks(_X, coffee))).
ff4:- show_kif_to_boxlog(~nesc(drinks(_X, coffee))).
ff5:- show_kif_to_boxlog(~poss(drinks(_X, coffee))).
ff6:- show_kif_to_boxlog(poss(drinks(_X, coffee))).


:- show_kif_to_boxlog(all(X, if(nesc(livesAt(X, green_house)),poss(livesAt(X, green_house))))).

:- show_kif_to_boxlog(all(X, if(poss(livesAt(X, green_house)),nesc(livesAt(X, green_house))))).

:- break.

% =================================================================================
% Note these two assertions are implicit to the system and have no side effect 
% (they are here to serve as a reminder)
% =================================================================================

% for any objects in the universe that live in the green house must obvously have that as a possibility
all(X, if(livesAt(X, green_house),poss(livesAt(X, green_house)))).

% all objects in the universe that do drink coffee, may drink coffee
if(nesc(drinks(X, coffee)),poss(drinks(X, coffee))).


% =================================================================================
% Define a couple predicates
% =================================================================================

% maximum cardinality of livesAt/2 is 1
instance(livesAt,'FunctionalBinaryPredicate').
% thus implies
==> arity(livesAt,2).
==> domain(livesAt,1,human).
==> domain(livesAt,2,dwelling).

% define drinks/2
==> arity(drinks,2).
domain(drinks,1,human).
domain(drinks,2,beverage_class).

% =================================================================================
% Note these two assertions are implicit to the system and have no side effect 
% (they are here to serve as a reminder)
% =================================================================================

% all objects in the universe that do drink coffee, may drink coffee
all(X, if(drinks(X, coffee),possible(drinks(X, coffee)))).

% for any objects in the universe that live in the green house must obvously have that as a possibility
all(X, if(livesAt(X, green),possible(livesAt(X, green)))).

% =================================================================================
% But given the above: 
%
%   Only things that possibly can drink coffee live in the green house?
%
% =================================================================================
all(X, livesAt(X, green_house) & drinks(X, coffee)).

~poss(livesAt(fred,green_house)).

% Does fred drink coffee? (should be unknown)
:- \+ drinks(fred,coffee).

possible(livesAt(joe,green_house)).
:- drinks(joe,coffee).


% =================================================================================
% These two assertions are a bit weird but are for fun
% =================================================================================

% all objects in the universe that may drink coffee do drink coffee
%all(X, if(possible(drinks(X, coffee)),drinks(X, coffee))).

% all objects in the universe that may live in the green house do live in the green house
%all(X, if(possible(livesAt(X, green_house)),lives(X, green_house) )).


