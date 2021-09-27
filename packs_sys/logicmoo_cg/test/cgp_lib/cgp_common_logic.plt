
:- use_module(library(plunit)).
:- use_module(library(test_cover)).

:- ensure_loaded(library(cgp_lib/cgp_common_logic)).

:- begin_tests(cgp_common_logic).

% ==========================================================================
%% run_1_test(+String)
% Converts InS string into Clif
% ==========================================================================
run_1_test(String):-
  write('\n\n\n'),
  dmsg("================================================="), 
  mpred_test(mort(cgp_common_logic:kif_to_term(String, Clif))),
  pprint_ecp(magenta, (?- run_1_test(String))), 
  pprint_ecp(yellow, clif=Clif), 
  mpred_test(mort(cgp_common_logic:convert_clif_to_cg(Clif, CG))),
  pprint_ecp(cyan, cg(CG)), 
  dmsg("================================================="), !.

test(cgp_common_logic_all):- 
   forall(cl_example(String), run_1_test(String)).


:- end_tests(cgp_common_logic).

%plunit_cgp_common_logic
