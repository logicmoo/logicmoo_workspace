/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2017, University of Amsterdam
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

:- module(prolog_cover,
          [ show_coverage/1,            % :Goal
            show_coverage/2
          ]).
:- autoload(library(apply),[exclude/3,maplist/3,include/3,maplist/2]).
:- autoload(library(edinburgh),[nodebug/0]).
:- autoload(library(ordsets),
	    [ord_intersect/2,ord_intersection/3,ord_subtract/3]).
:- autoload(library(pairs),[group_pairs_by_key/2]).

:- set_prolog_flag(generate_debug_info, false).

/** <module> Clause cover analysis

The purpose of this module is to find which part of the program has been
used by a certain goal. Usage is defined   in terms of clauses that have
fired, separated in clauses that  succeeded   at  least once and clauses
that failed on each occasion.

This module relies on the  SWI-Prolog   tracer  hooks. It modifies these
hooks and collects the results, after   which  it restores the debugging
environment.  This has some limitations:

        * The performance degrades significantly (about 10 times)
        * It is not possible to use the debugger during coverage analysis
        * The cover analysis tool is currently not thread-safe.

The result is  represented  as  a   list  of  clause-references.  As the
references to clauses of dynamic predicates  cannot be guaranteed, these
are omitted from the result.

@bug    Relies heavily on SWI-Prolog internals. We have considered using
        a meta-interpreter for this purpose, but it is nearly impossible
        to do 100% complete meta-interpretation of Prolog.  Example
        problem areas include handling cuts in control-structures
        and calls from non-interpreted meta-predicates.
@tbd    Provide detailed information organised by predicate.  Possibly
        annotate the source with coverage information.
*/


:- dynamic
    entered/1,                      % clauses entered
    exited/1.                       % clauses completed

:- meta_predicate
    show_coverage(0),
    show_coverage(0,+).

%!  show_coverage(:Goal) is semidet.
%!  show_coverage(:Goal, +Modules:list(atom)) is semidet.
%
%   Report on coverage by Goal. Goal is   executed  as in once/1. Report
%   the details of the uncovered clauses  for   each  module in the list
%   Modules

show_coverage(Goal) :-
    show_coverage(Goal, []).
show_coverage(Goal, Modules):-
    setup_call_cleanup(
        setup_trace(State),
        once(Goal),
        cleanup_trace(State, Modules)).

setup_trace(state(Visible, Leash, Ref)) :-
    set_prolog_flag(coverage_analysis, true),
    asserta((user:prolog_trace_interception(Port, Frame, _, continue) :-
                    prolog_cover:assert_cover(Port, Frame)), Ref),
    port_mask([unify,exit], Mask),
    '$visible'(Visible, Mask),
    '$leash'(Leash, Mask),
    trace.

port_mask([], 0).
port_mask([H|T], Mask) :-
    port_mask(T, M0),
    '$syspreds':port_name(H, Bit),
    Mask is M0 \/ Bit.

cleanup_trace(state(Visible, Leash, Ref), Modules) :-
    nodebug,
    '$visible'(_, Visible),
    '$leash'(_, Leash),
    erase(Ref),
    set_prolog_flag(coverage_analysis, false),
    covered(Succeeded, Failed),
    file_coverage(Succeeded, Failed, Modules).


%!  assert_cover(+Port, +Frame) is det.
%
%   Assert coverage of the current clause. We monitor two ports: the
%   _unify_ port to see which  clauses   we  entered, and the _exit_
%   port to see which completed successfully.

assert_cover(unify, Frame) :-
    running_static_pred(Frame),
    prolog_frame_attribute(Frame, clause, Cl),
    !,
    assert_entered(Cl).
assert_cover(exit, Frame) :-
    running_static_pred(Frame),
    prolog_frame_attribute(Frame, clause, Cl),
    !,
    assert_exited(Cl).
assert_cover(_, _).

%!  running_static_pred(+Frame) is semidet.
%
%   True if Frame is not running a dynamic predicate.

running_static_pred(Frame) :-
    prolog_frame_attribute(Frame, goal, Goal),
    \+ predicate_property(Goal, dynamic).

%!  assert_entered(+Ref) is det.
%!  assert_exited(+Ref) is det.
%
%   Add Ref to the set of entered or exited clauses.

assert_entered(Cl) :-
    entered(Cl),
    !.
assert_entered(Cl) :-
    assert(entered(Cl)).

assert_exited(Cl) :-
    exited(Cl),
    !.
assert_exited(Cl) :-
    assert(exited(Cl)).

%!  covered(+Ref, +VisibleMask, +LeashMask, -Succeeded, -Failed) is det.
%
%   Restore state and collect failed and succeeded clauses.

covered(Succeeded, Failed) :-
    findall(Cl, (entered(Cl), \+exited(Cl)), Failed0),
    findall(Cl, retract(exited(Cl)), Succeeded0),
    retractall(entered(Cl)),
    sort(Failed0, Failed),
    sort(Succeeded0, Succeeded).


                 /*******************************
                 *           REPORTING          *
                 *******************************/

%!  file_coverage(+Succeeded, +Failed, +Modules) is det.
%
%   Write a report on the clauses covered   organised by file to current
%   output. Show detailed information about   the  non-coverered clauses
%   defined in the modules Modules.

file_coverage(Succeeded, Failed, Modules) :-
    format('~N~n~`=t~78|~n'),
    format('~tCoverage by File~t~78|~n'),
    format('~`=t~78|~n'),
    format('~w~t~w~64|~t~w~72|~t~w~78|~n',
           ['File', 'Clauses', '%Cov', '%Fail']),
    format('~`=t~78|~n'),
    forall(source_file(File),
           file_coverage(File, Succeeded, Failed, Modules)),
    format('~`=t~78|~n').

file_coverage(File, Succeeded, Failed, Modules) :-
    findall(Cl, clause_source(Cl, File, _), Clauses),
    sort(Clauses, All),
    (   ord_intersect(All, Succeeded)
    ->  true
    ;   ord_intersect(All, Failed)
    ),
    !,
    ord_intersection(All, Failed, FailedInFile),
    ord_intersection(All, Succeeded, SucceededInFile),
    ord_subtract(All, SucceededInFile, UnCov1),
    ord_subtract(UnCov1, FailedInFile, Uncovered),

    %if doc_collect (from pldoc) is active, pldoc comments are recorded as
    % clauses but we do not want to count them in the statistics
    exclude(is_pldoc, All, All_wo_pldoc),
    exclude(is_pldoc, Uncovered, Uncovered_wo_pldoc),
    exclude(is_pldoc, FailedInFile, Failed_wo_pldoc),

    %We do not want to count clauses such as :-use_module(_) in the statistics
    exclude(is_system_clause, All_wo_pldoc, All_wo_system),
    exclude(is_system_clause, Uncovered_wo_pldoc, Uncovered_wo_system),
    exclude(is_system_clause, Failed_wo_pldoc, Failed_wo_system),

    length(All_wo_system, AC),
    length(Uncovered_wo_system, UC),
    length(Failed_wo_system, FC),

    CP is 100-100*UC/AC,
    FCP is 100*FC/AC,
    summary(File, 56, SFile),
    format('~w~t ~D~64| ~t~1f~72| ~t~1f~78|~n', [SFile, AC, CP, FCP]),
    detailed_report(Uncovered_wo_system, Modules).
file_coverage(_,_,_,_).


is_system_clause(Clause) :-
    clause_name(Clause, Name),
    Name = system:_.

is_pldoc(Clause) :-
    clause_name(Clause, _Module:Name2/_Arity),
    pldoc_predicate(Name2).

pldoc_predicate('$pldoc').
pldoc_predicate('$mode').
pldoc_predicate('$pred_option').

summary(Atom, MaxLen, Summary) :-
    atom_length(Atom, Len),
    (   Len < MaxLen
    ->  Summary = Atom
    ;   SLen is MaxLen - 5,
        sub_atom(Atom, _, SLen, 0, End),
        atom_concat('...', End, Summary)
    ).


%!  clause_source(+Clause, -File, -Line) is det.
%!  clause_source(-Clause, +File, -Line) is det.

clause_source(Clause, File, Line) :-
    nonvar(Clause),
    !,
    clause_property(Clause, file(File)),
    clause_property(Clause, line_count(Line)).
clause_source(Clause, File, Line) :-
    Pred = _:_,
    source_file(Pred, File),
    \+ predicate_property(Pred, multifile),
    nth_clause(Pred, _Index, Clause),
    clause_property(Clause, line_count(Line)).
clause_source(Clause, File, Line) :-
    Pred = _:_,
    predicate_property(Pred, multifile),
    nth_clause(Pred, _Index, Clause),
    clause_property(Clause, file(File)),
    clause_property(Clause, line_count(Line)).

%! detailed_report(+Uncovered:list(clause), +Modules:list(atom)) is det

detailed_report(Uncovered, Modules):-
    maplist(clause_line_pair, Uncovered, Pairs),
    include(pair_in_modules(Modules), Pairs, Pairs_in_modules),
    (   Pairs_in_modules \== []
    ->  sort(Pairs_in_modules, Pairs_sorted),
        group_pairs_by_key(Pairs_sorted, Compact_pairs),
        nl,
        format('~2|Clauses not covered from modules ~p~n', [Modules]),
        format('~4|Predicate ~59|Clauses at lines ~n', []),
        maplist(print_clause_line, Compact_pairs),
        nl
    ;   true
    ).

pair_in_modules(Modules,(Module:_Name)-_Line):-
    memberchk(Module, Modules).

clause_line_pair(Clause, Name-Line):-
    clause_property(Clause, line_count(Line)),
    clause_name(Clause, Name).

clause_name(Clause,Name):-
    clause(Module:Head, _, Clause),
    functor(Head,F,A),
    Name=Module:F/A.

print_clause_line((Module:Name/Arity)-Lines):-
    term_to_atom(Module:Name, Complete_name),
    summary(Complete_name, 54, SName),
    format('~4|~w~t~59|~p~n', [SName/Arity, Lines]).
