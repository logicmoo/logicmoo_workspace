/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2006-2021, University of Amsterdam
                              VU University Amsterdam
                              SWI-Prolog Solutions b.v.
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
            show_coverage/2             % :Goal, +Modules
          ]).
:- autoload(library(apply), [exclude/3, maplist/2, convlist/3]).
:- autoload(library(edinburgh), [nodebug/0]).
:- autoload(library(ordsets),
            [ord_intersect/2, ord_intersection/3, ord_subtract/3]).
:- autoload(library(pairs), [group_pairs_by_key/2]).
:- autoload(library(ansi_term), [ansi_format/3]).
:- autoload(library(filesex), [directory_file_path/3, make_directory_path/1]).
:- autoload(library(lists), [append/3]).
:- autoload(library(option), [option/2, option/3]).
:- autoload(library(readutil), [read_line_to_string/2]).

:- set_prolog_flag(generate_debug_info, false).

/** <module> Clause coverage analysis

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


:- meta_predicate
    show_coverage(0),
    show_coverage(0,+).

%!  show_coverage(:Goal) is semidet.
%!  show_coverage(:Goal, +Options) is semidet.
%!  show_coverage(:Goal, +Modules:list(atom)) is semidet.
%
%   Report on coverage by Goal. Goal is executed as in once/1. Options
%   processed:
%
%     - modules(+Modules)
%       Provide a detailed report on Modules. For backwards
%       compatibility this is the same as providing a list of
%       modules in the second argument.
%     - annotate(+Bool)
%       Create an annotated file for the detailed results.
%       This is implied if the `ext` or `dir` option are
%       specified.
%     - ext(+Ext)
%       Extension to use for the annotated file. Default is
%       `.cov`.
%     - dir(+Dir)
%       Dump the annotations in the given directory.  If not
%       given, the annotated files are created in the same
%       directory as the source file.   Each clause that is
%       related to a physical line in the file is annotated
%       with one of:
%
%         | ### | Clause was never executed.                       |
%         | ++N | Clause was executed N times and always succeeded |
%         | --N | Clause was executed N times and never succeeded  |
%         | +N-M | Clause was succeeded N times and failed M times |

show_coverage(Goal) :-
    show_coverage(Goal, []).
show_coverage(Goal, Modules) :-
    maplist(atom, Modules),
    !,
    show_coverage(Goal, [modules(Modules)]).
show_coverage(Goal, Options) :-
    clean_output(Options),
    setup_call_cleanup(
        setup_trace(State),
        once(Goal),
        cleanup_trace(State, Options)).

setup_trace(state(Visible, Leash, Ref)) :-
    nb_setval(cover_count, 0),
    nb_setval(cover_enter, [](0)),
    nb_setval(cover_exits, [](0)),
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

cleanup_trace(state(Visible, Leash, Ref), Options) :-
    nodebug,
    '$visible'(_, Visible),
    '$leash'(_, Leash),
    erase(Ref),
    set_prolog_flag(coverage_analysis, false),
    covered(Succeeded, Failed),
    file_coverage(Succeeded, Failed, Options),
    clean_data.

%!  assert_cover(+Port, +Frame) is det.
%
%   Assert coverage of the current clause. We monitor two ports: the
%   _unify_ port to see which  clauses   we  entered, and the _exit_
%   port to see which completed successfully.

:- public assert_cover/2.

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
    \+ '$get_predicate_attribute'(Goal, (dynamic), 1).
%   \+ predicate_property(Goal, dynamic).

%!  assert_entered(+Ref) is det.
%!  assert_exited(+Ref) is det.
%
%   Add Ref to the set of entered or exited clauses.

assert_entered(Cl) :-
    add_clause(Cl, I),
    bump(cover_enter, I).

assert_exited(Cl) :-
    add_clause(Cl, I),
    bump(cover_exits, I).

bump(Var, I) :-
    nb_getval(Var, Array),
    arg(I, Array, Old),
    New is Old+1,
    nb_setarg(I, Array, New).

:- dynamic
    clause_index/2.

add_clause(Cl, I) :-
    clause_index(Cl, I),
    !.
add_clause(Cl, I) :-
    nb_getval(cover_count, I0),
    I is I0+1,
    nb_setval(cover_count, I),
    assertz(clause_index(Cl, I)),
    expand_arrays(I).

expand_arrays(I) :-
    nb_getval(cover_enter, Array),
    functor(Array, _, Arity),
    I =< Arity,
    !.
expand_arrays(_) :-
    grow_array(cover_enter),
    grow_array(cover_exits).

grow_array(Name) :-
    nb_getval(Name, Array),
    functor(Array, F, Arity),
    NewSize is Arity*2,
    functor(NewArray, F, NewSize),
    copy_args(1, Arity, Array, NewArray),
    FillStart is Arity+1,
    fill_args(FillStart, NewSize, NewArray),
    nb_setval(Name, NewArray).

copy_args(I, End, From, To) :-
    I =< End,
    !,
    arg(I, From, A),
    arg(I, To, A),
    I2 is I+1,
    copy_args(I2, End, From, To).
copy_args(_, _, _, _).

fill_args(I, End, To) :-
    I =< End,
    !,
    arg(I, To, 0),
    I2 is I+1,
    fill_args(I2, End, To).
fill_args(_, _, _).

clean_data :-
    nb_delete(cover_count),
    nb_delete(cover_enter),
    nb_delete(cover_exits),
    retractall(clause_index(_,_)).

%!  count(+Which, +Clause, -Count) is semidet.
%
%   Get event counts for Clause.

count(Which, Cl, Count) :-
    clause_index(Cl, I),
    nb_getval(Which, Array),
    arg(I, Array, Count).

entered(Cl) :-
    count(cover_enter, Cl, Count),
    Count > 0.
exited(Cl) :-
    count(cover_exits, Cl, Count),
    Count > 0.

entered(Cl, Count) :-
    count(cover_enter, Cl, Count).
exited(Cl, Count) :-
    count(cover_exits, Cl, Count).

%!  covered(-Succeeded, -Failed) is det.
%
%   Collect failed and succeeded clauses.

covered(Succeeded, Failed) :-
    findall(Cl, (entered(Cl), \+exited(Cl)), Failed0),
    findall(Cl, exited(Cl), Succeeded0),
    sort(Failed0, Failed),
    sort(Succeeded0, Succeeded).


                 /*******************************
                 *           REPORTING          *
                 *******************************/

%!  file_coverage(+Succeeded, +Failed, +Options) is det.
%
%   Write a report on the clauses covered   organised by file to current
%   output. Show detailed information about   the  non-coverered clauses
%   defined in the modules Modules.

file_coverage(Succeeded, Failed, Options) :-
    format('~N~n~`=t~78|~n'),
    format('~tCoverage by File~t~78|~n'),
    format('~`=t~78|~n'),
    format('~w~t~w~64|~t~w~72|~t~w~78|~n',
           ['File', 'Clauses', '%Cov', '%Fail']),
    format('~`=t~78|~n'),
    forall(source_file(File),
           file_coverage(File, Succeeded, Failed, Options)),
    format('~`=t~78|~n').

file_coverage(File, Succeeded, Failed, Options) :-
    findall(Cl, clause_source(Cl, File, _), Clauses),
    sort(Clauses, All),
    (   ord_intersect(All, Succeeded)
    ->  true
    ;   ord_intersect(All, Failed)
    ),                                  % Clauses from this file are touched
    !,
    ord_intersection(All, Failed, FailedInFile),
    ord_intersection(All, Succeeded, SucceededInFile),
    ord_subtract(All, SucceededInFile, UnCov1),
    ord_subtract(UnCov1, FailedInFile, Uncovered),

    clean_set(All, All_wo_system),
    clean_set(Uncovered, Uncovered_wo_system),
    clean_set(FailedInFile, Failed_wo_system),

    length(All_wo_system, AC),
    length(Uncovered_wo_system, UC),
    length(Failed_wo_system, FC),

    CP is 100-100*UC/AC,
    FCP is 100*FC/AC,
    summary(File, 56, SFile),
    format('~w~t ~D~64| ~t~1f~72| ~t~1f~78|~n', [SFile, AC, CP, FCP]),
    (   list_details(File, Options),
        clean_set(SucceededInFile, Succeeded_wo_system),
        ord_union(Failed_wo_system, Succeeded_wo_system, Covered)
    ->  detailed_report(Uncovered_wo_system, Covered, File, Options)
    ;   true
    ).
file_coverage(_,_,_,_).

clean_set(Clauses, UserClauses) :-
    exclude(is_pldoc, Clauses, Clauses_wo_pldoc),
    exclude(is_system_clause, Clauses_wo_pldoc, UserClauses).

is_system_clause(Clause) :-
    clause_name(Clause, Name),
    Name = system:_.

is_pldoc(Clause) :-
    clause_name(Clause, _Module:Name2/_Arity),
    pldoc_predicate(Name2).

pldoc_predicate('$pldoc').
pldoc_predicate('$mode').
pldoc_predicate('$pred_option').
pldoc_predicate('$exported_op').        % not really PlDoc ...

summary(String, MaxLen, Summary) :-
    string_length(String, Len),
    (   Len < MaxLen
    ->  Summary = String
    ;   SLen is MaxLen - 5,
        sub_string(String, _, SLen, 0, End),
        string_concat('...', End, Summary)
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

%!  list_details(+File, +Options) is semidet.

list_details(File, Options) :-
    option(modules(Modules), Options),
    source_file_property(File, module(M)),
    memberchk(M, Modules),
    !.
list_details(File, Options) :-
    (   source_file_property(File, module(M))
    ->  module_property(M, class(user))
    ;   true     % non-module file must be user file.
    ),
    annotate_file(Options).

annotate_file(Options) :-
    (   option(annotate(true), Options)
    ;   option(dir(_), Options)
    ;   option(ext(_), Options)
    ),
    !.

%! detailed_report(+Uncovered, +Covered, +File:atom, +Options) is det

detailed_report(Uncovered, Covered, File, Options):-
    annotate_file(Options),
    !,
    convlist(line_annotation(File, uncovered), Uncovered, Annot1),
    convlist(line_annotation(File, covered),   Covered,   Annot2),
    append(Annot1, Annot2, AnnotationsLen),
    pairs_keys_values(AnnotationsLen, Annotations, Lens),
    max_list(Lens, MaxLen),
    Margin is MaxLen+1,
    annotate_file(File, Annotations, [margin(Margin)|Options]).
detailed_report(Uncovered, _, File, _Options):-
    convlist(uncovered_clause_line(File), Uncovered, Pairs),
    sort(Pairs, Pairs_sorted),
    group_pairs_by_key(Pairs_sorted, Compact_pairs),
    nl,
    file_base_name(File, Base),
    format('~2|Clauses not covered from file ~p~n', [Base]),
    format('~4|Predicate ~59|Clauses at lines ~n', []),
    maplist(print_clause_line, Compact_pairs),
    nl.

line_annotation(File, Class, Clause, (Line-Annot)-Len) :-
    clause_property(Clause, file(File)),
    clause_property(Clause, line_count(Line)),
    annot(Class, Clause, Annot, Len).

annot(uncovered, _Clause, ansi(error,###), 3).
annot(covered,    Clause, Annot, Len) :-
    entered(Clause, Entered),
    exited(Clause, Exited),
    (   Exited == Entered
    ->  format(string(Text), '++~D', [Entered]),
        Annot = ansi(comment, Text)
    ;   Exited == 0
    ->  format(string(Text), '--~D', [Entered]),
        Annot = ansi(warning, Text)
    ;   Failed is Entered - Exited,
        format(string(Text), '+~D-~D', [Exited, Failed]),
        Annot = ansi(comment, Text)
    ),
    string_length(Text, Len).

uncovered_clause_line(File, Clause, Name-Line) :-
    clause_property(Clause, file(File)),
    clause_name(Clause, Name),
    clause_property(Clause, line_count(Line)).

%!  clause_name(+Clause, -Name) is det.
%
%   Return the clause predicate indicator as Module:Name/Arity.

clause_name(Clause, Name) :-
    clause(Module:Head, _, Clause),
    functor(Head,F,A),
    Name=Module:F/A.

print_clause_line((Module:Name/Arity)-Lines):-
    term_string(Module:Name, Complete_name),
    summary(Complete_name, 54, SName),
    format('~4|~w~t~59|~p~n', [SName/Arity, Lines]).


		 /*******************************
		 *           ANNOTATE		*
		 *******************************/

clean_output(Options) :-
    option(dir(Dir), Options),
    !,
    option(ext(Ext), Options, cov),
    format(atom(Pattern), '~w/*.~w', [Dir, Ext]),
    expand_file_name(Pattern, Files),
    maplist(delete_file, Files).
clean_output(Options) :-
    forall(source_file(File),
           clean_output(File, Options)).

clean_output(File, Options) :-
    option(ext(Ext), Options, cov),
    file_name_extension(File, Ext, CovFile),
    (   exists_file(CovFile)
    ->  E = error(_,_),
        catch(delete_file(CovFile), E,
              print_message(warning, E))
    ;   true
    ).


%!  annotate_file(+File, +Annotations, +Options) is det.
%
%   Create  an  annotated  copy  of  File.  Annotations  is  a  list  of
%   `LineNo-Annotation`,  where  `Annotation`  is  atomic    or  a  term
%   Format-Args,  optionally  embedded   in    ansi(Code,   Annotation).

annotate_file(Source, Annotations, Options) :-
    option(ext(Ext), Options, cov),
    (   option(dir(Dir), Options)
    ->  file_base_name(Source, Base),
        file_name_extension(Base, Ext, CovFile),
        directory_file_path(Dir, CovFile, CovPath),
        make_directory_path(Dir)
    ;   file_name_extension(Source, Ext, CovPath)
    ),
    keysort(Annotations, SortedAnnotations),
    setup_call_cleanup(
        open(Source, read, In),
        setup_call_cleanup(
            open(CovPath, write, Out),
            annotate(In, Out, SortedAnnotations, Options),
            close(Out)),
        close(In)).

annotate(In, Out, Annotations, Options) :-
    set_stream(Out, tty(true)),
    annotate(In, Out, Annotations, 0, Options).

annotate(In, Out, Annotations, LineNo0, Options) :-
    read_line_to_string(In, Line),
    (   Line == end_of_file
    ->  true
    ;   succ(LineNo0, LineNo),
        option(margin(Margin), Options, 4),
        (   annotation(LineNo, Annotations, Annot, Annotations1)
        ->  write_annotation(Out, Annot)
        ;   Annotations1 = Annotations
        ),
        format(Out, '~t~*|~s~n', [Margin, Line]),
        annotate(In, Out, Annotations1, LineNo, Options)
    ).

annotation(Line, [Line-Annot|T], Annot, T).

write_annotation(Out, ansi(Code, Fmt-Args)) =>
    with_output_to(Out, ansi_format(Code, Fmt, Args)).
write_annotation(Out, ansi(Code, Fmt)) =>
    with_output_to(Out, ansi_format(Code, Fmt, [])).
write_annotation(Out, Fmt-Args) =>
    format(Out, Fmt, Args).
write_annotation(Out, Fmt) =>
    format(Out, Fmt, []).

