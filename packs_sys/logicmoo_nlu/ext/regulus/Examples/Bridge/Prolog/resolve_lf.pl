 
:- module(resolve_lf,
	[resolve_lf/4]
    ).

%======================================================================

:- use_module(library(lists)).
:- use_module('$REGULUS/PrologLib/utilities').
 
%======================================================================

resolve_lf(InLF, _InState, OutLF, Substitutions) :-
	OutLF = InLF,
	Substitutions = [trivial],
	!.

