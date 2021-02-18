
:- expects_dialect(lps).

/*
Minimal example for hooking a Prolog predicate.
my_hook(-ExternalObservations)
Predicate to be called from LPS interpreter, e.g. by specifying its name in the LPS options:

	go(YourProgram,[...,cycle_hook(my_hook,FluentTemplates,ActionTemplates),..])
	
For example the following causes the hook to report on all actions and events, but bob's balance only:
	['...examples/external_interfaces/myHook.pl'].
	golps('...examples/CLOUT_workshop/bankTransfer.pl',[dc,cycle_hook(my_hook,[balance(bob,_)],[transfer(_,_,_)])]).
	
If the hook predicate fails, the LPS computation is terminated, otherwise it attemps to continue.
Notice that it is necessary for the LPS program to still define the observe/2 predicate,
even if with observe([], _AnyAcceptableCycle).
*/
my_hook(Observations) :- 
	interpreter:option(cycle_hook(my_hook,Fluents,Actions)), % only one hook, but check name anyway
	% The hook should fetch these templates to potentially identify what it cares about:
	interpreter:collect_current_fluents(Fluents,F), 
	interpreter:collect_current_actions(Actions,A),
	interpreter:current_time(T),
	format("~nRunning my_hook in Prolog, cycle ~w.~n Fluents:~w~n Events:~w~n",[T,F,A]),
	Observations = [some_event("Hi from Capt. Hook!")].

