/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2011, University of Amsterdam
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

:- module(emacs_logtalk_mode, []).
:- use_module(library(pce)).
:- use_module(prolog_mode).
:- use_module(library(operators)).      % push/pop operators
:- use_module(library(trace/emacs_debug_modes)).
:- use_module(library(emacs_extend)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module deals with colourisation of .lgt files.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


                 /*******************************
                 *            LOGTALK MODE              *
                 *******************************/

:- emacs_begin_mode(logtalk, prolog,
                    "Mode for editing Logtalk documents",
                    % BINDINGS
                    [
                    ],
                    % SYNTAX TABLE
                    [
                    ]).

class_variable(tab_width, int, 4).
class_variable(body_indentation, int, 4).
class_variable(cond_indentation, int, 4).
class_variable(indent_tabs, bool, @on).

colourise_buffer(M) :->
    "Cross-reference the buffer and set up colours"::
    push_logtalk_operators,
    setup_call_cleanup(
            true,
            send_super(M, colourise_buffer),
            pop_logtalk_operators).

:- emacs_end_mode.


:- emacs_begin_mode(logtalk_debug, logtalk,
                    "Submode for the debugger",
                    [], []).
:- use_class_template(prolog_debug_methods).
:- emacs_end_mode.

:- initialization
   declare_emacs_mode(logtalk_debug, []).


                 /*******************************
                 *         SYNTAX RULES         *
                 *******************************/

:- multifile
    prolog_colour:style/2,
    prolog_colour:term_colours/2,
    prolog_colour:goal_colours/2,
    prolog_colour:directive_colours/2,
    prolog_colour:goal_classification/2.


object_opening_directive_relations(Relations, entity_relation-Colours) :-
    nonvar(Relations),
    Relations =.. [Functor| Entities],
    valid_object_relation(Functor),
    entity_relation_colours(Entities, Colours).

protocol_opening_directive_relations(Relations, entity_relation-Colours) :-
    nonvar(Relations),
    Relations =.. [Functor| Entities],
    valid_protocol_relation(Functor),
    entity_relation_colours(Entities, Colours).

category_opening_directive_relations(Relations, entity_relation-Colours) :-
    nonvar(Relations),
    Relations =.. [Functor| Entities],
    valid_category_relation(Functor),
    entity_relation_colours(Entities, Colours).

entity_relation_colours([], []).
entity_relation_colours(['::'(_, _)| Entities], [built_in-[classify,entity_identifier]| Colours]) :-
    !,
    entity_relation_colours(Entities, Colours).
entity_relation_colours([_| Entities], [entity_identifier| Colours]) :-
    entity_relation_colours(Entities, Colours).

valid_object_relation(implements).
valid_object_relation(imports).
valid_object_relation(extends).
valid_object_relation(instantiates).
valid_object_relation(specializes).

valid_protocol_relation(extends).

valid_category_relation(implements).
valid_category_relation(extends).
valid_category_relation(complements).


info_directive_items([], []).
info_directive_items([_ is _| Items], [built_in-[classify,classify]| Colours]) :-
    info_directive_items(Items, Colours).


entity_identifier(Object, Colour) :-
    (       atom(Object) ->
            Colour = entity_identifier
    ;       compound(Object) ->
            Object =.. [_| Parameters],
            entity_parameters(Parameters, Colours),
            Colour = entity_identifier-[Colours]
    ).

entity_parameters([], []).
entity_parameters([_| Parameters], [parameter| Colours]) :-
    entity_parameters(Parameters, Colours).


term_colours((:- encoding(_)), classify-[source_file_directive-[classify]]).


directive_colours(object(Object), entity_directive-[Colour]) :-
    entity_identifier(Object, Colour).
directive_colours(object(_, Relations), entity_directive-[entity_identifier,Colours]) :-
    object_opening_directive_relations(Relations, Colours).
directive_colours(object(_, Relations1, Relations2), entity_directive-[entity_identifier,Colours1,Colours2]) :-
    object_opening_directive_relations(Relations1, Colours1),
    object_opening_directive_relations(Relations2, Colours2).
directive_colours(object(_, Relations1, Relations2, Relations3), entity_directive-[entity_identifier,Colours1,Colours2,Colours3]) :-
    object_opening_directive_relations(Relations1, Colours1),
    object_opening_directive_relations(Relations2, Colours2),
    object_opening_directive_relations(Relations3, Colours3).
directive_colours(object(_, Relations1, Relations2, Relations3, Relations4), entity_directive-[entity_identifier,Colours1,Colours2,Colours3,Colours4]) :-
    object_opening_directive_relations(Relations1, Colours1),
    object_opening_directive_relations(Relations2, Colours2),
    object_opening_directive_relations(Relations3, Colours3),
    object_opening_directive_relations(Relations4, Colours4).

directive_colours(protocol(_), entity_directive-[entity_identifier]).
directive_colours(protocol(_, Relations), entity_directive-[entity_identifier,Colours]) :-
    protocol_opening_directive_relations(Relations, Colours).

directive_colours(category(_), entity_directive-[entity_identifier]).
directive_colours(category(_, Relations), entity_directive-[entity_identifier,Colours]) :-
    category_opening_directive_relations(Relations, Colours).
directive_colours(category(_, Relations1, Relations2), entity_directive-[entity_identifier,Colours1,Colours2]) :-
    category_opening_directive_relations(Relations1, Colours1),
    category_opening_directive_relations(Relations2, Colours2).

% source file directives
directive_colours(encoding(_), source_file_directive-[classify]).
directive_colours(set_prolog_flag(_, _), source_file_directive-[classify,classify]).
% conditional compilation directives
directive_colours(if(_), conditional_compilation_directive-[classify]).
directive_colours(elif(_), conditional_compilation_directive-[classify]).
directive_colours(else, conditional_compilation_directive-[]).
directive_colours(endif, conditional_compilation_directive-[]).
% entity directives
directive_colours(built_in, entity_directive-[]).
directive_colours(set_logtalk_flag(_, _), entity_directive-[classify,classify]).
directive_colours(calls(_), entity_directive-[entity_identifier]).
directive_colours(dynamic, entity_directive-[]).
directive_colours(end_category, entity_directive-[]).
directive_colours(end_object, entity_directive-[]).
directive_colours(end_protocol, entity_directive-[]).
directive_colours(info(_), entity_directive-[classify]).
directive_colours(initialization(_), entity_directive-[classify]).
directive_colours(threaded, entity_directive-[]).
directive_colours(uses(_), entity_directive-[entity_identifier]).
% module directives
directive_colours(use_module(_), entity_directive-[entity_identifier]).
directive_colours(use_module(_, _), entity_directive-[entity_identifier,classify]).
% predicate directives
directive_colours(alias(_, _), predicate_directive-[entity_identifier,classify]).
directive_colours(coinductive(_), predicate_directive-[classify]).
directive_colours(discontiguous(_), predicate_directive-[classify]).
directive_colours(dynamic(_), predicate_directive-[classify]).
directive_colours(info(_, _), predicate_directive-[classify,classify]).
directive_colours(meta_predicate(_), predicate_directive-[classify]).
directive_colours(meta_non_terminal(_), predicate_directive-[classify]).
directive_colours(mode(_, _), predicate_directive-[classify,classify]).
directive_colours(multifile(_), predicate_directive-[classify]).
directive_colours(op(_, _, _), predicate_directive-[classify,classify,classify]).
directive_colours(private(_), predicate_directive-[classify]).
directive_colours(protected(_), predicate_directive-[classify]).
directive_colours(public(_), predicate_directive-[classify]).
directive_colours(synchronized(_), predicate_directive-[classify]).
directive_colours(uses(_, _), predicate_directive-[entity_identifier,classify]).


%       goal_colours(+Goal, -Colours)
%
%       Colouring of special goals.

% enumerating objects, categories and protocols
goal_colours(current_category(_), built_in-[classify]).
goal_colours(current_object(_), built_in-[classify]).
goal_colours(current_protocol(_), built_in-[classify]).
% enumerating objects, categories and protocols properties
goal_colours(category_property(_, _), built_in-[classify,classify]).
goal_colours(object_property(_, _), built_in-[classify,classify]).
goal_colours(protocol_property(_, _), built_in-[classify,classify]).
% creating new objects, categories and protocols
goal_colours(create_category(_, _, _, _), built_in-[classify,classify,classify,classify]).
goal_colours(create_object(_, _, _, _), built_in-[classify,classify,classify,classify]).
goal_colours(create_protocol(_, _, _), built_in-[classify,classify,classify,classify]).
% abolishing objects, categories and protocols
goal_colours(abolish_category(_), built_in-[classify]).
goal_colours(abolish_object(_), built_in-[classify]).
goal_colours(abolish_protocol(_), built_in-[classify]).
% objects, categories and protocols relations
goal_colours(extends_object(_, _), built_in-[classify,classify]).
goal_colours(extends_object(_, _, _), built_in-[classify,classify,classify]).
goal_colours(extends_protocol(_, _), built_in-[classify,classify]).
goal_colours(extends_protocol(_, _, _), built_in-[classify,classify,classify]).
goal_colours(extends_category(_, _), built_in-[classify,classify]).
goal_colours(extends_category(_, _, _), built_in-[classify,classify,classify]).
goal_colours(implements_protocol(_, _), built_in-[classify,classify]).
goal_colours(implements_protocol(_, _, _), built_in-[classify,classify,classify]).
goal_colours(conforms_to_protocol(_, _), built_in-[classify,classify]).
goal_colours(conforms_to_protocol(_, _, _), built_in-[classify,classify,classify]).
goal_colours(imports_category(_, _), built_in-[classify,classify]).
goal_colours(imports_category(_, _, _), built_in-[classify,classify,classify]).
goal_colours(instantiates_class(_, _), built_in-[classify,classify]).
goal_colours(instantiates_class(_, _, _), built_in-[classify,classify,classify]).
goal_colours(specializes_class(_, _), built_in-[classify,classify]).
goal_colours(specializes_class(_, _, _), built_in-[classify,classify,classify]).
goal_colours(complements_object(_, _), built_in-[classify,classify]).
% event handling
goal_colours(abolish_events(_, _, _, _, _), built_in-[classify,classify,classify,classify,classify]).
goal_colours(current_event(_, _, _, _, _), built_in-[classify,classify,classify,classify,classify]).
goal_colours(define_events(_, _, _, _, _), built_in-[classify,classify,classify,classify,classify]).
% multi-threading meta-predicates
goal_colours(threaded(_), built_in-[classify]).
goal_colours(threaded_call(_), built_in-[classify]).
goal_colours(threaded_call(_, _), built_in-[classify,classify]).
goal_colours(threaded_once(_), built_in-[classify]).
goal_colours(threaded_once(_, _), built_in-[classify,classify]).
goal_colours(threaded_ignore(_), built_in-[classify]).
goal_colours(threaded_exit(_), built_in-[classify]).
goal_colours(threaded_exit(_, _), built_in-[classify,classify]).
goal_colours(threaded_peek(_), built_in-[classify]).
goal_colours(threaded_peek(_, _), built_in-[classify,classify]).
goal_colours(threaded_wait(_), built_in-[classify]).
goal_colours(threaded_notify(_), built_in-[classify]).
% threaded engines
goal_colours(threaded_engine_create(_, _, _), built_in-[classify,classify,classify]).
goal_colours(threaded_engine_destroy(_), built_in-[classify]).
goal_colours(threaded_engine_self(_), built_in-[classify]).
goal_colours(threaded_engine(_), built_in-[classify]).
goal_colours(threaded_engine_next(_, _), built_in-[classify,classify]).
goal_colours(threaded_engine_next_reified(_, _), built_in-[classify,classify]).
goal_colours(threaded_engine_yield(_), built_in-[classify]).
goal_colours(threaded_engine_post(_, _), built_in-[classify,classify]).
goal_colours(threaded_engine_fetch(_), built_in-[classify]).
% compiling and loading objects, categories and protocols
goal_colours(logtalk_compile(_), built_in-[classify]).
goal_colours(logtalk_compile(_, _), built_in-[classify,classify]).
goal_colours(logtalk_load(_), built_in-[classify]).
goal_colours(logtalk_load(_, _), built_in-[classify,classify]).
goal_colours(logtalk_library_path(_, _), built_in-[classify,classify]).
goal_colours(logtalk_load_context(_, _), built_in-[classify,classify]).
% make
goal_colours(logtalk_make, built_in-[classify,classify]).
goal_colours(logtalk_make(_), built_in-[classify]).
% flags
goal_colours(current_logtalk_flag(_, _), built_in-[classify,classify]).
goal_colours(set_logtalk_flag(_, _), built_in-[classify,classify]).
goal_colours(create_logtalk_flag(_, _, _), built_in-[classify,classify,classify]).
/*
% others
goal_colours(forall(_, _), built_in-[classify,classify]).
goal_colours(retractall(_), built_in-[classify]).
*/
% execution context methods
goal_colours(self(_), built_in-[classify]).
goal_colours(sender(_), built_in-[classify]).
goal_colours(this(_), built_in-[classify]).
goal_colours(parameter(_, _), built_in-[classify,classify]).
% reflection methods
goal_colours(current_predicate(_), built_in-[classify]).
goal_colours(predicate_property(_, _), built_in-[classify,classify]).
% database methods
goal_colours(abolish(_), built_in-[db]).
goal_colours(asserta(_), built_in-[db]).
goal_colours(assertz(_), built_in-[db]).
goal_colours(clause(_, _), built_in-[db,classify]).
goal_colours(retract(_), built_in-[db]).
goal_colours(retractall(_), built_in-[db]).
% meta-call methods
goal_colours(call(_), built_in-[classify]).
goal_colours(ignore(_), built_in-[classify]).
goal_colours(once(_), built_in-[classify]).
goal_colours(\+ _, built_in-[classify]).
% exception-handling methods
goal_colours(catch(_, _, _), built_in-[classify,classify,classify]).
goal_colours(throw(_), built_in-[classify]).
% all solutions methods
goal_colours(findall(_, _, _, _), built_in-[classify,classify,classify,classify]).
goal_colours(findall(_, _, _), built_in-[classify,classify,classify]).
goal_colours(forall(_, _), built_in-[classify,classify]).
goal_colours(bagof(_, _, _), built_in-[classify,setof,classify]).
goal_colours(setof(_, _, _), built_in-[classify,setof,classify]).
% event handler methods
goal_colours(before(_, _, _), built_in-[classify,classify,classify]).
goal_colours(after(_, _, _), built_in-[classify,classify,classify]).
% DCGs rules parsing methods and non-terminals
%call//1
goal_colours(phrase(_, _), built_in-[classify,classify]).
goal_colours(phrase(_, _, _), built_in-[classify,classify,classify]).
% term and goal expansion methods
goal_colours(expand_term(_, _), built_in-[classify,classify]).
goal_colours(term_expansion(_, _), built_in-[classify,classify]).
goal_colours(expand_goal(_, _), built_in-[classify,classify]).
goal_colours(goal_expansion(_, _), built_in-[classify,classify]).
% message sending
goal_colours('::'(_, _), built_in-[classify,classify]).
goal_colours('::'(_), built_in-[classify]).
% "super" calls
goal_colours('^^'(_), built_in-[classify]).
% message forwarding
goal_colours([_], built_in-[classify]).
% calling external code
goal_colours('{}'(_), built_in-[classify]).
% context-switching calls
goal_colours('<<'(_, _), built_in-[classify,classify]).


goal_classification(_, normal).

style(goal(source_file_directive,_),             [colour(blue)]).
style(goal(conditional_compilation_directive,_), [colour(blue)]).
style(goal(entity_directive,_),                  [colour(blue)]).
style(goal(predicate_directive,_),               [colour(blue)]).

style(directive,                                 [bold(false)]).
style(entity_directive,                          [colour(navy_blue)]).
style(entity_identifier,                         [colour(dark_slate_blue)]).
style(identifier,                                [bold(true)]).
style(entity_relation,                           [colour(blue)]).
style(head(_),                                   [bold(false)]).
style(built_in,                                  [colour(blue)]).
style(parameter, Style) :-
    prolog_colour:def_style(var, Style).


prolog_colour:style(Pattern, Style) :-
    style(Pattern, Style).


prolog_colour:term_colours(Term, Colours) :-
    term_colours(Term, Colours).


prolog_colour:directive_colours(Directive, Colours) :-
    directive_colours(Directive, Colours).


prolog_colour:goal_colours(Term, Colours) :-
    goal_colours(Term, Colours).


prolog_colour:goal_classification(Goal, Classification) :-
    goal_classification(Goal, Classification).


                 /*******************************
                 *         SYNTAX HOOKS         *
                 *******************************/

:- multifile
    prolog:alternate_syntax/4.


prolog:alternate_syntax(logtalk, Module,
                        emacs_logtalk_mode:push_logtalk_operators(Module),
                        emacs_logtalk_mode:pop_logtalk_operators).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note that we could generalise this to deal with all included files.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

push_logtalk_operators :-
    '$set_source_module'(M, M),
    push_logtalk_operators(M).

push_logtalk_operators(Module) :-
    logtalk_operators(Ops),
    push_operators(Module:Ops).

pop_logtalk_operators :-
    pop_operators.

logtalk_operators([
        % message sending operators
        op(600, xfy, ::),       % send to object
        op(600,  fy, ::),       % send to self
        op(600,  fy, ^^),       % "super" call (calls an overriden, inherited method definition)
        % mode operators
        op(200,  fy, +),        % input argument (instantiated)
        op(200,  fy, ?),        % input/output argument
        op(200,  fy, @),        % input argument (not modified by the call)
        op(200,  fy, -),        % output argument (not instantiated)
        % ground argument
        op(200,  fy, ++),
        % unbound argument (typically when returning an opaque term)
        op(200,  fy, --)
]).
