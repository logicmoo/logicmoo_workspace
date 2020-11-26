/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, VU University Amsterdam
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

:- module(r_expand_dot, []).

%%	expand_dotted_name(+TermIn, -TermOut) is det.
%
%	Translate Atom1.Atom2 and Atom.Compound   into 'Atom1.Atom2' and
%	'Atom1.Name'(Args).

expand_dotted_name(TermIn, TermOut) :-
	compound(TermIn), !,
	(   join_dot(TermIn, Out)
	->  TermOut = Out
	;   contains_dot(TermIn)
	->  compound_name_arguments(TermIn, Name, ArgsIn),
	    maplist(expand_dotted_name, ArgsIn, ArgsOut),
	    compound_name_arguments(TermOut, Name, ArgsOut)
	;   TermOut = TermIn
	).
expand_dotted_name(Term, Term).

join_dot(In, Out) :-
	compound_name_arguments(In, '.', [A,B]),
	atom(A),
	(   atom(B)
	->  atomic_list_concat([A,'.',B], Out)
	;   compound(B)
	->  compound_name_arguments(B, Name, Args),
	    atomic_list_concat([A,'.',Name], Name2),
	    compound_name_arguments(Out, Name2, Args)
	;   Out = In
	).

contains_dot(Term) :-
	compound(Term),
	(   compound_name_arity(Term, '.', 2)
	->  true
	;   arg(_, Term, Arg),
	    contains_dot(Arg)
	->  true
	).

system:goal_expansion(In, Out) :-
	contains_dot(In), !,
	expand_dotted_name(In, Out).
