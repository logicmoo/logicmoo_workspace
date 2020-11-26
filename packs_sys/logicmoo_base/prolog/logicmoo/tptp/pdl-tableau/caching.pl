%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Copyright 2009-2010, Ullrich Hustadt, University of Liverpool
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 

store_consistent(Fmls,Branch,NewBranch,Eventualities,NewEventualities) :-
	list_to_ord_set(Fmls,OrdFmls),
	(user:consistent(OrdFmls,_,_) ->
	    true
	;
	    append(BranchForFmls,Branch,NewBranch),
	    append(Eventualities,EventualitiesForFmls,NewEventualities),
	    pdl_write('Storing consistent set '), pdl_write(OrdFmls), pdl_nl,
	    assert(user:consistent(OrdFmls,BranchForFmls,EventualitiesForFmls))
	),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

store_inconsistent(Fmls) :-
	list_to_ord_set(Fmls,OrdFmls),
	(user:inconsistent(OrdFmls) ->
	    true
	;
	    assert(user:inconsistent(OrdFmls)),
	    pdl_write('Storing inconsistent set '), pdl_write(OrdFmls), pdl_nl,
	    print_proof_step(g, inconsistent_set(OrdFmls), '[stored]')
	),
	!.
