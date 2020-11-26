/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2001-2011, University of Amsterdam
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

:- module(plot_demo,
          [ plot_function/0,
            barchart/0,
            barchart/1
          ]).
:- use_module(library(pce)).
:- use_module(library('plot/plotter')).
:- use_module(library('plot/barchart')).
:- use_module(library(autowin)).

                 /*******************************
                 *      FUNCTION PLOTTING       *
                 *******************************/

plot_function :-
    plot_function(X:sin(X)).

plot_function(Template) :-
    To is 2*pi,
    PlotStep is To/100,
    Step is pi/4,
    new(W, auto_sized_picture('Plotter demo')),
    send(W, display, new(P, plotter)),
    send(P, axis, new(X, plot_axis(x, 0, To, Step, 300))),
    send(P, axis, plot_axis(y, -1, 1, @default, 200)),
    send(X, format, '%.2f'),
    send(P, graph, new(G, plot_graph)),
    plot_function(0, To, PlotStep, Template, G),
    send(W, open).

plot_function(X, To, _, _, _) :-
    X >= To,
    !.
plot_function(X, To, Step, Template, G) :-
    copy_term(Template, X:Func),
    Y is Func,
    send(G, append, X, Y),
    NewX is X + Step,
    plot_function(NewX, To, Step, Template, G).

                 /*******************************
                 *            BARCHART          *
                 *******************************/

barchart :-
    barchart(vertical).
barchart(HV) :-
    new(W, picture),
    active_classes(Classes),
    length(Classes, N),
    required_scale(Classes, Scale),
    send(W, display, new(BC, bar_chart(HV, 0, Scale, 200, N))),
    forall(member(class(Name, Created, Freed), Classes),
           send(BC, append,
                bar_group(Name,
                          bar(created, Created, green),
                          bar(freed, Freed, red)))),
    send(W, open).

                 /*******************************
                 *          TEST/DEMO           *
                 *******************************/

active_classes(ClassTerms) :-
    new(Classes, chain),
    send(@classes, for_all,
         if(@arg2?no_created > 250,
            message(Classes, append, @arg2))),
    send(Classes, sort,
         ?(@arg1?name, compare, @arg2?name)),
    chain_list(Classes, ClassList),
    maplist(class_term, ClassList, ClassTerms).

class_term(Class, class(Name, Created, Freed)) :-
    get(Class, name, Name),
    get(Class, no_created, Created),
    get(Class, no_freed, Freed).

required_scale([class(_, Created, _)], Created) :- !.
required_scale([class(_, Created, _)|T], Scale) :-
    required_scale(T, Scale0),
    Scale is max(Created, Scale0).
