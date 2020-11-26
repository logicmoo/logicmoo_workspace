:- include(test_header).

:- process_this_script.

:- prolog_load_context(module,Module),
   listing(Module:_).

:- set_prolog_flag(verbose_autoload,false).

% ==============================================================
% TESTS: Retractions
% ==============================================================

% Rule 1: If no cute puppies exist, then Joan will buy a horse  (authored by Joan)
(~exists(X,cute_puppy(X)) => buys(joan,horse)).

% Rule 2: It is impossible for broke people to buy things  (authored by a Shop Keeper)
forall([P,A], broke(P) => ~buys(P,A)).

% Fact A: Joan is a broke person  (authored by Joan)
broke(joan).  


%:- prolog_load_context(module,Module),
%   listing(Module:_).


:- prolog_load_context(module,Module),
   mpred_reset_kb(Module).
   

:- prolog_load_context(module,Module),
   listing(Module:_).




