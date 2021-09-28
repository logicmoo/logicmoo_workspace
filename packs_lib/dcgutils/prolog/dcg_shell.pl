/* Part of dcgutils
	Copyright 2012-2015 Samer Abdallah (Queen Mary University of London; UCL)
	 
	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:- module(dcg_shell,
	[	dcgshell/2
	,	dcgshell/3
	,	dcgshell/4
	,	make//0
	,	help/3
	,	time//1, time//2, time//3
	,	profile_phrase//1
	,	trace//1
	,	catch//3
	]).


:- module_transparent 	
		dcgshell/2, dcgshell/3, dcgshell/4,
		dcgshell_x/6, shell_prompt/4, make//0.

:- use_module(library(termutils), [get_key/2]).

%%	dcgshell( +Interp, +Id, ?S1, ?S2) is semidet.
%%	dcgshell( +Id, ?S1, ?S2) is semidet.
%%	dcgshell( ?S1, ?S2) is semidet.
%
%	Runs an interactive shell where typed commands are interepreted
%	as DCG phrases. The cumulative result of all these phrases takes
%	the DCG state from S1 to S2. If present, Id is used to identify
%	the shell and is written as part of the prompt. The default prompt
%	is 'dcg'.
%
%	Pressing Ctrl-D at the prompt or typing 'halt' or 'end_of_file'
%	terminates the shell unifying S2 with the final state. The command
%	'fail' terminates the shell and causes dcgshell/2 or dcgshell/3 as
%	a whole to fail, without leaving any choice points.
%
%	dcgshell/4 allows the specification of an alternate interpreter
%	other than call_dcg/3, which is the default in the other forms.
%
%	Special commands
%		* module(M)
%		Switches context module of interpreter to M.
%		* interp(I)
%		Change to a new interpreter. Eg, interp(time) will
%		cause timing information to be printed after each
%		command is interpreted.
%		* X^Phrase
%		Causes Phrase to be interpreted after binding X to the
%		id of the current interpreter. Since the id can be any
%		term, you can use it as simple sort of environment in which
%		you can keep data that might be useful later on.

dcgshell(S1,S2) :- dcgshell(call_dcg,dcg,S1,S2). 
dcgshell(Id,S1,S2) :- dcgshell(call_dcg,Id,S1,S2). 
dcgshell(Interp,Id,S1,S2) :- !,
	shell_prompt(Id,Interp,Goal,Bindings),
	dcgshell_x(Goal,Bindings,Interp,Id,S1,S2).


shell_prompt(Id,Interp,Goal,Bindings) :-
	context_module(Mod),
	format(atom(NA),'~p: ~p (~W) >> ',[Mod,Interp,Id,[portray(true),quoted(true),max_depth(3)]]),
	read_history(h,'!h',[trace,end_of_file],NA, Goal, Bindings).

dcgshell_x(fail,_,_,_,_,_) :- !, fail.
dcgshell_x(halt,_,_,_,S,S) :- !, nl.
dcgshell_x(end_of_file,_,_,_,S,S) :- !, nl.
dcgshell_x(module(Mod),_,Interp,Id,S1,S2) :- !, @(dcgshell(Interp,Id,S1,S2), Mod).
dcgshell_x(interp(Int2),_,Int1,Id,S1,S2) :- !, 
	format('Changing interpreter from ~w to ~w.\n',[Int1,Int2]),
	dcgshell(Int2,Id,S1,S2).

dcgshell_x(X^Goal,Bindings,Interp,Id,S1,S2) :- !,
	X=Id, dcgshell_x(Goal,Bindings,Interp,Id,S1,S2).

dcgshell_x(Goal,Bindings,Interp,Id,S0,S2) :- !,
	catch( 
		(	current_prolog_flag(prompt_alternatives_on,PromptOn),
			call(Interp,Goal,S0,S1), 
			include(dcg_shell:bound,Bindings,BoundBindings),
			(	BoundBindings=[] 
			->	(	PromptOn=determinism
				->	write('\nok? [.,<return>=yes,;=no] '), 
               dcg_shell:get_key([';','\r','.'],K), (K='\r';K='.'), nl 
				;	true)
			;	dcg_shell:check_bindings(BoundBindings)
			),
			write('  Yes.\n\n')
		;	write('\n  No.\n'), S0=S1
		),
		Exception,
		(	Exception=dcg_shell:escape(Ex) -> throw(Ex)
		;	nl, print_message(error,Exception), S1=S0, nl)
	), !, 
	dcgshell(Interp,Id,S1,S2).

bound(_=Value) :- nonvar(Value).



% useful DCG shell commands

%% make// is det.
%
%  DCG shell command to update loaded files, just as with make/0.
make --> {make}.

%% help(+Topic)// is det.
%
%  Look-up help on Topic, just as with help/1.
help(A) --> {help(A)}.


%% time( +G:phrase(S))// is semidet.
%% time( +G:pred(A,S,S), ?X:A)// is semidet.
%% time( +G:pred(A,B,S,S), ?X:A, ?Y:B)// is semidet.
%
%  Time execution of DCG phrase G. Any extra arguments are passed
%  to G as in call/N. 
:- meta_predicate time(//,?,?), time(3,?,?,?), time(4,?,?,?,?).
time(G,A,B) :- time(call_dcg(G,A,B)).
time(G,A,B,C) :- time(call(G,A,B,C)).
time(G,A,B,C,D) :- time(call(G,A,B,C,D)).

%% profile_phrase( +G:phrase(_))// is semidet.
%
%  Profile execution of DCG phrase G. 
:- meta_predicate profile_phrase(//,?,?).
profile_phrase(G,A,B) :- profile(call_dcg(G,A,B)).


:- meta_predicate catch(//,?,//,?,?).
catch(Phrase,Ex,Handler,S1,S2) :-
	catch(call_dcg(Phrase,S1,S2),
		Ex, call_dcg(Handler,S1,S2)).

:- meta_predicate trace(//,?,?).
trace(Goal,S1,S2) :-
	setup_call_cleanup( trace, call_dcg(Goal,S1,S2), notrace).

% ----------------------------- Extract from meta.pl ------------------------

%% check_bindings( +Bindings:list(binding)) is semidet.
%
%  Allow user to review variable bindings. Fails if the
%  user rejects the current set of values.
%  Bindings is a list of Name=Value pairs, ie
%  ==
%  binding ---> (atom=term).
%  ==
%
%  The current Prolog flags are used to determine the print format 
%  (see answer_write_options in current_prolog_flag/2).

check_bindings([]) :- nl. % !! Do we always want this?
check_bindings([B|BT]) :-
	current_prolog_flag(answer_write_options,Opts),
	write_bindings(Opts,[B|BT]),
	get_key([';','\r','.'],K), (K='\r';K='.'), nl. 


%% write_bindings( +Bindings:list(binding)) is semidet.
%
%  Allow user to view variable bindings without any interaction.
%  Bindings is a list of Name=Value pairs.
%
%  The current Prolog flags are used to determine the print format 
%  (see answer_write_options in current_prolog_flag/2).

write_bindings([]) :- !. % !! Do we always want this?
write_bindings(B) :-
	current_prolog_flag(answer_write_options,Opts),
	write_bindings(Opts,B), nl, nl.

write_bindings(Opts,[N=V]) :- 
	(	true % nonvar(V)
	->	format('\n  ~w = ~@ ',[N,write_term(V,Opts)])
	;	true
	).

write_bindings(Opts,[N=V,X|T]) :- 
	(	true % nonvar(V)
	->	format('\n  ~w = ~@ ',[N,write_term(V,Opts)])
	;	true
	),
	write_bindings(Opts,[X|T]).
