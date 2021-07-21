/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1999-2020, University of Amsterdam
                              VU University Amsterdam
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

:- module(latex2html4xpce, []).
:- autoload(library(apply),[maplist/3]).
:- autoload(library(lists),[append/3]).
:- autoload(library(latex2html/latex2html),
	    [ latex2html_module/0,
	      tex_load_commands/1,
	      translate/3,
	      add_to_index/1,
	      clean_tt/2,
	      add_to_index/2,
	      translate_table/3
	    ]).

:- latex2html_module.
:- tex_load_commands(xpce).

%       XPCE <-> ProWindows switch

:- dynamic
    pwtrue/0.                               % ProWindows 3.1

cmd(makepw, _, []) :-
    assert(pwtrue).
cmd(ifpw({If}, {Else}), Mode, HTML) :-
    (   pwtrue
    ->  translate(If, Mode, HTML)
    ;   translate(Else, Mode, HTML)
    ).

env(pwonly(_, Tokens), HTML) :-
    (   pwtrue
    ->  translate(Tokens, normal, HTML)
    ;   HTML = []
    ).
env(xpceonly(_, Tokens), HTML) :-
    (   pwtrue
    ->  HTML = []
    ;   translate(Tokens, normal, HTML)
    ).

%cmd(product, 'ProWindows') :- pwtrue.
%cmd(product, 'XPCE').
%cmd(productpl, 'ProWindows') :- pwtrue.
%cmd(productpl, 'XPCE/Prolog').
%cmd(productversion, '3.1') :- pwtrue.
%cmd(productversion, '4.9.3').          % dynamic!

cmd(objectname({Name}),         #b([nospace(@), Name])).
cmd(noclass({Name}),            #b(Name)).
cmd(class({Name}),              #lref(Label, Name)) :-
    atom_concat('class:', Name, Label),
    add_to_index(Name).
cmd(classs({Name}),             #lref(Label, NameS)) :-
    atom_concat('class:', Name, Label),
    atom_concat(Name, s, NameS),
    add_to_index(Name).
cmd(tool({Name}),               #strong(+Name)).
cmd(demo({Name}),               #strong(+Name)).
cmd(type({Name}),               #b([#code(+Name)])).
cmd(send({Name}),               #b([#code(nospace(->)), Name])).
cmd(get({Name}),                #b([#code(nospace(<-)), Name])).
cmd(both({Name}),               #b([#code(nospace(<->)), Name])).
cmd(classsend({Class}, {Name}), #b([+Class, #code(nospace(->)), +Name])).
cmd(classget({Class}, {Name}),  #b([+Class, #code(nospace(<-)), +Name])).
cmd(classboth({Class}, {Name}), #b([+Class, #code(nospace(<->)), +Name])).
cmd(sendmethod(_M, {Class}, {Selector}, {Args}),
    #defitem([ #strong([Class, ' ', nospace('->'), Selector, nospace(':')]),
               ' ', #var(+Args)
             ])).
cmd(getmethod(_M, {Class}, {Selector}, {Args}),
    #defitem([ #strong([Class, ' ', nospace('<-'), Selector, nospace(':')]),
               ' ', #var(+Args)
             ])).
cmd(bothmethod(_M, {Class}, {Selector}, {Args}),
    #defitem([ #strong([Class, ' ', nospace('<->'), Selector, nospace(':')]),
               ' ', #var(+Args)
             ])).
cmd(manualtool({Descr}, {Menu}),
    #defitem([ #strong(+Descr), ' ', #i(#embrace(+Menu))])).
cmd(secoverview({Label}, {Title}),
    [ html('<li>'), #lref(RefName, +Title) ]) :-
    format(string(RefName), 'sec:~w', Label).
cmd(classsummary(_M, {RawClass}, {Args}, {_FigRef}),
    #defitem(#label(Label, [#strong(Class), #embrace(#var(+Args))]))) :-
    clean_tt(RawClass, Class),
    atom_concat('class:', Class, Label),
    add_to_index(Class, +Label).
cmd(fontalias({Alias}, {Term}), #defitem([#code(Alias), #i(+Term)])).
cmd(noargpredicate(Name), HTML) :-
    cmd(predicate(Name, {'0'}, {[]}), HTML).
%cmd(idx({Term}), nospace(Term)) :-     % If only index to section is wanted
%       add_to_index(Term).
cmd(glossitem({Term}), #defitem(#label(RefName, #strong(Term)))) :-
    canonicalise_glossitem(Term, Ref),
    format(string(RefName), 'gloss:~w', [Ref]).
cmd(g({Term}),  #lref(RefName, Term)) :-
    canonicalise_glossitem(Term, Ref),
    format(string(RefName), 'gloss:~w', [Ref]).
cmd(line({Tokens}), #quote(Line)) :-
    translate(Tokens, normal, Line).
cmd(classvar({Class}, {Var}), #b([#code([+Class,nospace('.'),+Var])])).
cmd(tab, #code(verb('\t'))).
cmd(opt({Arg}), #embrace("[]", +Arg)).
cmd(zom({Arg}), #embrace("{}", +Arg)).
cmd(fnm({Mark}), +Mark).
cmd(hr, html('<hr>')).
cmd(nameof({Names}), #embrace("{}", #code(Names))).

cmd(setupfancyplain, []).

env(tabularlp(_, Tokens), HTML) :-
    translate_table('|l|p{3in}|', Tokens, HTML).

canonicalise_glossitem(In, Out) :-
    downcase_atom(In, In1),
    atom_codes(In1, Chars0),
    (   append(Chars1, "s", Chars0)
    ->  true
    ;   Chars1 = Chars0
    ),
    maplist(canonical_char, Chars1, Chars2),
    atom_codes(Out, Chars2).

canonical_char(0' , 0'-) :- !.
canonical_char(0'_, 0'-) :- !.
canonical_char(X, X).

