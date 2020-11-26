%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 30/01/95   File: merge2.pl                    %%
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 30/01/95 Created                                                          %%
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Zoltan Rigo                                                      %%
%%                                                                           %%
%% Usage:   prolog merge2.pl                                                 %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- op(1100, xfy, 'implies'),
   op(1000, xfy, 'and'),
   op(1050, xfy, 'or'),
   op( 400,  fy, 'not').

:- op( 600,  fy, 'forall'),
   op( 600,  fy, 'exists').

:- op( 600,  fy, 'box'),
   op( 600,  fy, 'diamond').

merge([],_,[]).
merge([X | Xs],Ys,PXYs0):-
	merge(Ys,X,PXYs0,PXYs1),
	merge(Xs,Ys,PXYs1).

merge([],_) --> [].
merge([X|Xs],Y) -->
	{ merge_to_formula(X,Y,PXY) },
	[PXY],
	merge(Xs,Y).

merge_to_formula(L1, L2, Clause):-
	( L1 =.. [implies, Prem1, Conc1] ->
	    ( L2 =.. [implies, Prem2, Conc2] ->
	         Clause =.. [implies, Prem1 and Prem2, Conc1 or Conc2]
	    ; Clause =.. [implies, Prem1, Conc1 or L2]
	    )
	;   ( L2 =.. [implies, Prem2, Conc2] ->
	        Clause =.. [implies, Prem2, Conc2 or L1]
	    ; Clause =.. [or, L1, L2]
	    )
       ).
