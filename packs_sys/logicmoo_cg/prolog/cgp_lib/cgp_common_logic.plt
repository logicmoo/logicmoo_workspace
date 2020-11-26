
:- begin_tests(cgp_common_logic).

:- ensure_loaded(library(cgp_lib/cgp_common_logic)).

% ==========================================================================
%% run_1_test(+String)
% Converts InS string into Clif
% ==========================================================================
run_1_test(String):-
   write('\n\n\n'), 
   dmsg("================================================="), 
   kif_to_term(String, Clif), 
   pprint_ecp(magenta, (?- run_1_test(String))), 
   pprint_ecp(yellow, clif=Clif), 
   convert_clif_to_cg(Clif, CG), 
   pprint_ecp(cyan, cg(CG)), 
   dmsg("================================================="), !.

test(cgp_common_logic_all):- 
   forall(cl_example(String), run_1_test(String)).


:- end_tests(cgp_common_logic).
