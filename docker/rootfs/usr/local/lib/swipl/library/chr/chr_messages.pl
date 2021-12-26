/*  Part of CHR (Constraint Handling Rules)

    Author:        Jan Wielemaker and Tom Schrijvers
    E-mail:        Tom.Schrijvers@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2011, K.U. Leuven
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(chr_messages,
	  [ chr_message/3		% +CHR Message, Out, Rest
	  ]).
:- use_module(chr(chr_runtime)).

:- discontiguous
	chr_message/3.

%	compiler messages

chr_message(compilation_failed(From)) -->
	[ 'CHR Failed to compile ~w'-[From] ].

%	debug messages

chr_message(prompt) -->
	[ at_same_line, ' ? ', flush ].
chr_message(command(Command)) -->
	[ at_same_line, '[~w]'-[Command] ].
chr_message(invalid_command) -->
	[ nl, 'CHR: Not a valid debug option.  Use ? for help.' ].
chr_message(debug_options) -->
	{ bagof(Ls-Cmd,
		bagof(L, 'chr debug command'(L, Cmd), Ls),
		Lines)
	},
	[ 'CHR Debugger commands:', nl, nl ],
	debug_commands(Lines),
	[ nl ].

debug_commands([]) -->
	[].
debug_commands([Ls-Cmd|T]) -->
	[ '\t' ], chars(Ls), [ '~t~28|~w'-[Cmd], nl ],
	debug_commands(T).
	
chars([C]) --> !,
	char(C).
chars([C|T]) -->
	char(C), [', '],
	chars(T).

char(' ') --> !, ['<space>'].
char('\r') --> !, ['<cr>'].
char(end_of_file) --> !, ['EOF'].
char(C) --> [C].


chr_message(ancestors(History, Depth)) -->
	[ 'CHR Ancestors:', nl ],
	ancestors(History, Depth).

ancestors([], _) -->
	[].
ancestors([Event|Events], Depth) -->
	[ '\t' ], event(Event, Depth), [ nl ],
	{ NDepth is Depth - 1
	},
	ancestors(Events, NDepth).


%	debugging ports

chr_message(event(Port, Depth)) -->
	[ 'CHR: ' ],
	event(Port, Depth),
	[ flush ].			% do not emit a newline

event(Port, Depth) -->
	depth(Depth),
	port(Port).
event(apply(H1,H2,G,B), Depth) -->
	depth(Depth),
	[ 'Apply: ' ],
	rule(H1,H2,G,B).
event(try(H1,H2,G,B), Depth) -->
	depth(Depth),
	[ 'Try: ' ],
	rule(H1,H2,G,B).
event(insert(#(_,Susp)), Depth) -->
	depth(Depth),
	[ 'Insert: ' ],
	head(Susp).

port(call(Susp)) -->
	[ 'Call: ' ],
	head(Susp).
port(wake(Susp)) -->
	[ 'Wake: ' ],
	head(Susp).
port(exit(Susp)) -->
	[ 'Exit: ' ],
	head(Susp).
port(fail(Susp)) -->
	[ 'Fail: ' ],
	head(Susp).
port(redo(Susp)) -->
	[ 'Redo: ' ],
	head(Susp).
port(remove(Susp)) -->
	[ 'Remove: ' ],
	head(Susp).


depth(Depth) -->
	[ '~t(~D)~10| '-[Depth] ].

head(Susp) -->
	{ Susp =.. [_,ID,_,_,_,_|GoalArgs], Goal =.. GoalArgs
	},
	[ '~w # <~w>'-[Goal, ID] ].

heads([H]) --> !,
	head(H).
heads([H|T]) -->
	head(H),
	[ ', ' ],
	heads(T).


%	rule(H1, H2, G, B)
%	
%	Produce text for the CHR rule "H1 \ H2 [<=]=> G | B"

rule(H1, H2, G, B) -->
	rule_head(H1, H2),
	rule_body(G, B).

rule_head([], H2) --> !,
	heads(H2),
	[ ' ==> ' ].
rule_head(H1, []) --> !,
	heads(H1),
	[ ' <=> ' ].
rule_head(H1, H2) -->
	heads(H2), [ ' \\ ' ], heads(H1), [' <=> '].


rule_body(true, B) --> !,
	[ '~w.'-[B] ].
rule_body(G, B) -->
	[ '~w | ~w.'-[G, B] ].
