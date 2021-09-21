:- include(test_header).

% :- process_this_script.

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





% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/445 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/mpred_reset_db_01.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/MPRED_RESET_DB_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3AMPRED_RESET_DB_01 

