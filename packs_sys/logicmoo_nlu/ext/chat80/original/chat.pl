/*  Copyright 1986-2020 David H. D. Warren and Fernando C. N. Pereira

    Permission is hereby granted, free of charge, to any person obtaining a
    copy of this software and associated documentation files (the
    "Software"), to deal in the Software without restriction, including
    without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to
    permit persons to whom the Software is furnished to do so, subject to
    the following conditions:

    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
    OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/* SWI-Prolog modifications:

   - include library Quintus for enhanced compatibility
   - put discontiguous between brackets
   - rename plus/3 and index/1 to be my_plus; my_index
   - remove last/2: system predicate with equivalent definition.
*/

:- use_module(library(quintus), [no_style_check/1]).
:- op(1150, fx, [(mode), (public)]).

:- no_style_check(single_var).
:- no_style_check((discontiguous)).

:- consult(chatops).

:- consult(readin).		% sentence input, ASCII VERSION
:- consult(ptree).		% print trees
:- consult(xgrun).		% XG runtimes
:- consult(newg).		% clone + lex
:- consult(clotab).		% attachment tables
:- consult(newdict).	% syntactic dictionary
:- consult(slots).		% fits arguments into predicates
:- consult(scopes).		% quantification and scoping
:- consult(templa).		% semantic dictionary
:- consult(qplan).		% query planning
:- consult(talkr).		% query evaluation
:- consult(ndtabl).		% relation info.
:- consult(aggreg).		% aggregation operators
:- consult(world0).		% geographic data base
:- consult(rivers).
:- consult(cities).
:- consult(countries).
:- consult(contain).
:- consult(borders).
:- consult(newtop).		% top level control

save_chat :-
   qsave_program(chat, [goal(hi80)]).

