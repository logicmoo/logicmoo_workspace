
:- expects_dialect(lps).

/*
Example hooking a Java computation to each LPS cycle, using InterProlog and Java as a SWI-Prolog subprocess.
See also myHook.pl for more details.

To try this one:

Launch LPS
	swipl -l .../lps_corner/utils/psyntax.P 

Get the interprolog.P file from https://github.com/mcalejo/interprolog/blob/master/src/com/declarativa/interprolog/interprolog.P
and the interprolog jar file from https://github.com/mcalejo/interprolog/blob/master/interprolog.jar?raw=true . Place them in some directory D.
First load interprolog:

	:- ['D/interprolog.P']

Launch the Java subprocess:
	spawn_java('D/interprolog.jar').

More info at http://interprolog.com/wiki/index.php?title=Java-Prolog_bridge#Using_Java_as_a_Prolog_subprocess

Load this example:
	['...examples/external_interfaces/myJavaHook.pl'].

Execute a LPS program (sending neither fluents nor events):

	golps('...examples/CLOUT_workshop/bankTransfer.pl',[dc, cycle_hook(my_hook,[],[])]).
	
If the hook predicate fails, the LPS computation is terminated, otherwise it attemps to continue.
Notice that it is necessary for the LPS program to still define the observe/2 predicate,
even if with observe([], _AnyAcceptableCycle).
*/
my_hook(Observations) :- 
	% interpreter:option(cycle_hook(my_hook,Fluents,Actions)), % only one hook, but check name anyway
	% The hook should fetch these templates to potentially identify what it cares about:
	% interpreter:collect_current_fluents(Fluents,F), 
	% interpreter:collect_current_actions(Actions,A),
	% interpreter:current_time(T),
	%
	% The following InterProlog call is synchronous, and could fetch results from Java; 
	% see http://interprolog.com/wiki/index.php?title=Java-Prolog_bridge#Prolog_side_API:_easier for java(...) variants
	% To send fluents etc, simply pass them to your method as TermModel arrays, e.g.
	%
	%	java(MyObject, R, myMethod(terms(F),terms(A)))
	% ... would send all fluents and actions/events and bind R to the result of the Java method
	%
	java( 'java.lang.System'-out, println(string("Hi from Capt. Hook in Java!")) ),
	Observations = [some_event("Capt. Hook is sailing")].

