%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

% tracing.pl
% Barney Pell
%
% Supports verbose tracing of modules for software development.

:- dynamic '$tracing'/2.

% TRACING(+Module,+Call)
% If tracing is turned on for Module (which might be a pattern), calls Call,
% otherwise succeeds without it.  

tracing(Module,Call) :- 
	( tracing(Module) -> call(Call) ; true ).

% TRACING_FORMAT(Module,String,Args)
% Like format/2, but only when we're tracing Module.
% Might cause trouble later when want to use streams also.
tracing_format(Module,String,Args) :- 
	( tracing(Module)
	-> format(String,Args)
	; true 
	).

% SET_TRACING(Module,Status)
% Sets an individual module's status (usually on or off).
set_tracing(Module,Status) :- 
	retractall('$tracing'(Module,_Status1)),
	assert('$tracing'(Module,Status)).

% SET_TRACING(Module,Component,Status)
% Sets a component of an individual module to status (usually on or off).
% Example: set_tracing(ab,iterations,on).
set_tracing(Mod,Component,Status) :- 
	functor(Module,Mod,1),
	arg(1,Module,Component),
	set_tracing(Module,Status).


% tracing(+Pattern)
% Pattern is either an atomic module name, or conjunct of disjunct of patterns.
%
tracing(M) :- var(M), !, format("~nError in tracing: Variable Module~n",[]).
tracing((M1;M2)) :- !, tracing(M1) ;  tracing(M2).
tracing((M1,M2)) :- !, tracing(M1), tracing(M2).
tracing(Module) :- tracing_module(Module).

tracing_module(Module) :- '$tracing'(Module,on).

traced_modules(Modules) :- setof(M,tracing_module(M),Modules), !.
traced_modules([]).



%================================================================================
% TIMING WHEN TRACING
%================================================================================
% TRACE_TIMING(module,Call) 
% just calls Call, if tracing is off for the module, otherwise calls Call and 
% outputs the time used. 
% Again Module can be a pattern.

trace_timing(Module,Call) :- 
	tracing(Module), !,
	runtime(Call).
trace_timing(_,Call) :- call(Call).

