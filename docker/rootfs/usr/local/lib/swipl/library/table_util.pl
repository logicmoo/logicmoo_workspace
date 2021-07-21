/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1999-2020, University of Amsterdam
                              CWI, Amsterdam
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

:- module(table_util,
          [ sort_table/2,               % +Handle, +OutputFile
            verify_table_order/1        % +Handle
          ]).
:- autoload(library(backcomp),[flush/0]).
:- autoload(library(table),
	    [ get_table_attribute/3,
	      read_table_record_data/4,
	      compare_strings/4,
	      read_table_fields/4
	    ]).

/** <module> Tabular file handling utilities
*/

%!  sort_table(+Table, +File)
%
%   Read the records from the given table,   sort  them according to the
%   ordering information on the key field and   write  the result to the
%   given filename. Note this may require a lot of memory.

sort_table(Table, File) :-
    open(File, write, OutFd),       % fail early :-)
    get_table_attribute(Table, key_field, Key),
    !,
    get_table_attribute(Table, field(Key), Term),
    get_table_attribute(Table, file, InFile),
    functor(Term, KeyName, _),
    arg(2, Term, Attributes),
    format('Sorting table "~w" ', [InFile]),
    (   memberchk(sorted(Order), Attributes)
    ->  true
    ;   memberchk(sorted, Attributes),
        Order = exact
    ),
    format('sorted(~w) on field "~w" ... ', [Order, KeyName]),
    flush,
    read_table(Table, KeyName, Fields),
    sort_fields(Order, Fields, SortedFields),
    write_table(SortedFields, Table, OutFd),
    close(OutFd),
    format('done.~n', []).

read_table(Table, KeyName, Fields) :-
    format('(reading) ... ', []), flush,
    read_table(Table, KeyName, 0, Fields).

read_table(Table, KeyName, From, [KeyValue-From|T]) :-
    read_field(Table, From, To, KeyName, KeyValue),
    !,
    read_table(Table, KeyName, To, T).
read_table(_, _, _, []).

sort_fields(Order, Fields, Sorted) :-
    length(Fields, N),
    format('(sorting ~D records) ... ', [N]), flush,
    sort_keyed_strings(Order, Fields, Sorted).

write_table(Records, Table, OutFd) :-
    format('(writing) ... ', []), flush,
    get_table_attribute(Table, record_separator, Sep),
    write_records(Records, Table, Sep, OutFd).

write_records([], _, _, _).
write_records([_-From|T], Table, Sep, OutFd) :-
    read_table_record_data(Table, From, _To, RecordData),
    format(OutFd, '~s~c', [RecordData, Sep]),
    write_records(T, Table, Sep, OutFd).


%!  sort_keyed_strings(+Table, +List, -Sorted)
%
%   Sort a list of KeyName-Index pairs on  their KeyName using the given
%   ordering table.

sort_keyed_strings(Table, List, Sorted) :-
    length(List, Length),
    do_sort(Length, Table, List, _, Result),
    Sorted = Result.

do_sort(2, Table, [X1, X2|L], L, R) :-
    !,
    X1 = K1-_,
    X2 = K2-_,
    compare_strings(Table, K1, K2, Cmp),
    merge2(Cmp, X1, X2, R).
do_sort(1, _, [X|L], L, [X]) :- !.
do_sort(0, _, L, L, []) :- !.
do_sort(N, Table, L1, L3, R) :-
    N1 is N // 2,
    N2 is N - N1,
    do_sort(N1, Table, L1, L2, R1),
    do_sort(N2, Table, L2, L3, R2),
    do_merge(R1, R2, Table, R).

do_merge([], R, _, R) :- !.
do_merge(R, [], _, R) :- !.
do_merge(R1, R2, Table, [X|R]) :-
    R1 = [X1|R1a],
    R2 = [X2|R2a],
    X1 = K1-_,
    X2 = K2-_,
    (   compare_strings(Table, K1, K2, >)
    ->  X = X2, do_merge(R1, R2a, Table, R)
    ;   X = X1, do_merge(R1a, R2, Table, R)
    ).

merge2(>, A, B, [B, A]) :- !.
merge2(_, A, B, [A, B]).


                 /*******************************
                 *             VERIFY           *
                 *******************************/

%!  verify_table_order(+Table)
%
%   If Table is a handle to a  defined   table  and the table contains a
%   key-fields, check that the fields  in   the  table are really sorted
%   according to the order defined in the table. Errors are reported.

verify_table_order(Table) :-
    get_table_attribute(Table, key_field, Key),
    !,
    get_table_attribute(Table, field(Key), Term),
    get_table_attribute(Table, file, File),
    functor(Term, KeyName, _),
    arg(2, Term, Attributes),
    format('Checking "~w" ', [File]),
    (   memberchk(sorted(Order), Attributes)
    ->  true
    ;   memberchk(sorted, Attributes),
        Order = exact
    ),
    (   memberchk(unique, Attributes)
    ->  Cmp = >,
        format('uniquely ', [])
    ;   Cmp = [>, =]
    ),
    format('sorted(~w) on field "~w" ... ', [Order, KeyName]),
    flush,
    read_field(Table, 0, To, KeyName, KeyValue),
    verify_table(Table, To, KeyName, KeyValue, Order, Cmp),
    format('done.~n', []).

verify_table(Table, From, KeyName, PrevValue, Order, Cmp) :-
    read_field(Table, From, To, KeyName, KeyValue),
    !,
    (   compare_strings(Order, KeyValue, PrevValue, Rval),
        ok_cmp(Rval, Cmp)
    ->  verify_table(Table, To, KeyName, KeyValue, Order, Cmp)
    ;   format('~N!! Order conflict: ~w < ~w~n', [KeyValue, PrevValue]),
        verify_table(Table, To, KeyName, KeyValue, Order, Cmp)
    ).
verify_table(_, _, _, _, _, _).

ok_cmp(Cmp, Cmp) :- !.
ok_cmp(Cmp, List) :-
    memberchk(Cmp, List).

read_field(Table, From, To, Field, Value) :-
    functor(Term, Field, 1),
    read_table_fields(Table, From, To,  [Term]),
    arg(1, Term, Value).
