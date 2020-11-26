The examples in this folder demonstrate that the PDT call graph works properly 
also for calls to module-transparent predicates, including in particular 
calls from supermodules (imported modules) that are bound at run-time to 
definitions in a submodule (importing module) that is the current context module. 

This is the equivelent of dynamic binding in object-oriented languages and a 
challenge for the analysis since, syntactically, these calls are addressed to 
a free variable that represents the context module. Typically:

:- module_transparent mt/0.
mt :- 
	context_module(M),
	...,
	M:p(X),   % <-- call addressed to statically unknown module
	...
	
Consult the files in this folder and then open the PDT's Context View or 
Global View and to see the call graph. 