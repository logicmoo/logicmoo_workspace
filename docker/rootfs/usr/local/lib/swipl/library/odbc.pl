/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2013, University of Amsterdam
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

:- module(odbc,
          [ odbc_connect/3,             % +DSN, -Conn, +Options
            odbc_driver_connect/3,      % +DriverString, -Conn, +Options
            odbc_disconnect/1,          % +Conn
            odbc_current_connection/2,  % ?Conn, -DSN
            odbc_set_connection/2,      % +Conn, +Option
            odbc_get_connection/2,      % +Conn, ?Option
            odbc_end_transaction/2,     % +Conn, +CommitRollback

            odbc_query/4,               % +Conn, +SQL, -Row, +Options
            odbc_query/3,               % +Conn, +SQL, -Row
            odbc_query/2,               % +Conn, +SQL

            odbc_prepare/4,             % +Conn, +SQL, +Parms, -Qid
            odbc_prepare/5,             % +Conn, +SQL, +Parms, -Qid, +Options
            odbc_execute/2,             % +Qid, +Parms
            odbc_execute/3,             % +Qid, +Parms, -Row
	    odbc_fetch/3,               % +Qid, -Row, +Options
	    odbc_next_result_set/1,     % +Qid
            odbc_close_statement/1,     % +Statement
            odbc_clone_statement/2,     % +Statement, -Clone
            odbc_free_statement/1,      % +Statement
            odbc_cancel_thread/1,       % +ThreadId
                                        % DB dictionary info
            odbc_current_table/2,       % +Conn, -Table
            odbc_current_table/3,       % +Conn, -Table, ?Facet
            odbc_table_column/3,        % +Conn, ?Table, ?Column
            odbc_table_column/4,        % +Conn, ?Table, ?Column, ?Facet
            odbc_type/3,                % +Conn, ?Type, -Facet
            odbc_data_source/2,         % ?DSN, ?Description

            odbc_table_primary_key/3,   % +Conn, ?Table, ?Column
            odbc_table_foreign_key/5,   % +Conn, ?PkTable, ?PkColumn, ?FkTable, ?FkColumn

	    odbc_set_option/1,          % -Option
            odbc_statistics/1,          % -Value
            odbc_debug/1                % +Level
          ]).
:- autoload(library(lists),[member/2]).

:- use_foreign_library(foreign(odbc4pl)).

%!  odbc_current_connection(?Conn, ?DSN) is nondet.
%
%   True if Conn is an open ODBC connection to DSN.

odbc_current_connection(Conn, DSN) :-
    odbc_current_connections(Conn, DSN, Pairs),
    member(Conn-DSN, Pairs).

%!  odbc_driver_connect(+DriverString, -Connection, +Options) is det.
%
%   Connects to a database using SQLDriverConnect(). This API allows
%   for driver-specific additional options.   DriverString is passed
%   without  checking.  Options  should  *not*  include  =user=  and
%   =password=.
%
%   Whenever possible, applications should   use  odbc_connect/3. If
%   you need this predicate,  please   check  the  documentation for
%   SQLDriverConnect() and the documentation of your driver.
%
%   @tbd    Add facilities to deal with prompted completion of the
%           driver options.

odbc_driver_connect(DriverString, Connection, Options) :-
    odbc_connect(-, Connection, [driver_string(DriverString)|Options]).

%!  odbc_query(+Connection, +SQL, -Row)
%
%   Run query without options.

odbc_query(Connection, SQL, Row) :-
    odbc_query(Connection, SQL, Row, []).

%!  odbc_query(+Connection, +SQL)
%
%   Execute SQL-statement that does not produce a result

odbc_query(Connection, SQL) :-
    odbc_query(Connection, SQL, Row),
    !,
    (   Row = affected(_)
    ->  true
    ;   print_message(warning, odbc(unexpected_result(Row)))
    ).

odbc_execute(Statement, Parameters) :-
    odbc_execute(Statement, Parameters, Row),
    !,
    (   Row = affected(_)
    ->  true
    ;   print_message(warning, odbc(unexpected_result(Row)))
    ).

odbc_prepare(Connection, SQL, Parameters, Statement) :-
    odbc_prepare(Connection, SQL, Parameters, Statement, []).

                 /*******************************
                 *          SCHEMA STUFF        *
                 *******************************/

%!  odbc_current_table(-Table, -Facet)
%
%   Enumerate the existing tables.

odbc_current_table(Connection, Table) :-
    odbc_tables(Connection, row(_Qualifier, _Owner, Table, 'TABLE', _Comment)).

odbc_current_table(Connection, Table, Facet) :-
    odbc_tables(Connection, Tuple),
    arg(3, Tuple, Table),
    table_facet(Facet, Connection, Tuple).

table_facet(qualifier(Qualifier), _, Tuple) :- arg(1, Tuple, Qualifier).
table_facet(owner(Owner), _, Tuple) :-         arg(2, Tuple, Owner).
table_facet(type(Type), _, Tuple) :-           arg(4, Tuple, Type).
table_facet(comment(Comment), _, Tuple) :-     arg(5, Tuple, Comment).
table_facet(arity(Arity), Connection, Tuple) :-
    arg(3, Tuple, Table),
    findall(C, odbc_table_column(Connection, Table, C), Cs),
    length(Cs, Arity).

%!  odbc_table_column(+Connection, +Table, +Column) is semidet.
%!  odbc_table_column(+Connection, +Table, -Column) is nondet.
%
%   True if Column appears in Table on Connection.

odbc_table_column(Connection, Table, Column) :-
    table_column(Connection, Table, Column, _Tuple).

table_column(Connection, Table, Column, Tuple) :-
    (   var(Table)
    ->  odbc_current_table(Connection, Table)
    ;   true
    ),
    (   ground(Column)              % force determinism
    ->  odbc_column(Connection, Table, Tuple),
        arg(4, Tuple, Column), !
    ;   odbc_column(Connection, Table, Tuple),
        arg(4, Tuple, Column)
    ).

%!  odbc_table_column(+Connection, +Table, ?Column, -Facet)

odbc_table_column(Connection, Table, Column, Facet) :-
    table_column(Connection, Table, Column, Tuple),
    column_facet(Facet, Tuple).

column_facet(table_qualifier(Q), T) :- arg(1, T, Q).
column_facet(table_owner(Q), T)     :- arg(2, T, Q).
column_facet(table_name(Q), T)      :- arg(3, T, Q).
%column_facet(column_name(Q), T)    :- arg(4, T, Q).
column_facet(data_type(Q), T)       :- arg(5, T, Q).
column_facet(type_name(Q), T)       :- arg(6, T, Q).
column_facet(precision(Q), T)       :- non_null_arg(7, T, Q).
column_facet(length(Q), T)          :- non_null_arg(8, T, Q).
column_facet(scale(Q), T)           :- non_null_arg(9, T, Q).
column_facet(radix(Q), T)           :- non_null_arg(10, T, Q).
column_facet(nullable(Q), T)        :- non_null_arg(11, T, Q).
column_facet(remarks(Q), T)         :- non_null_arg(12, T, Q).
column_facet(type(Type), T) :-
    arg(6, T, TypeName),
    sql_type(TypeName, T, Type).

%!  sql_type(+TypeName, +Row, -Type)
%
%   Create a canonical Prolog representation for the type.  This
%   is very incomplete code.

sql_type(dec, T, Type) :-
    !,
    sql_type(decimal, T, Type).
sql_type(numeric, T, Type) :-
    !,
    sql_type(decimal, T, Type).
sql_type(decimal, T, Type) :-
    !,
    column_facet(precision(Len), T),
    (   column_facet(scale(D), T),
        D \== 0
    ->  Type = decimal(Len, D)
    ;   Type = decimal(Len)
    ).
sql_type(char, T, char(Len)) :-
    !,
    column_facet(length(Len), T).
sql_type(varchar, T, varchar(Len)) :-
    !,
    column_facet(length(Len), T).
sql_type(TypeName, _T, Type) :-
    downcase_atom(TypeName, Type).

%!  odbc_type(+Connection, +TypeSpec, ?Facet).

odbc_type(Connection, TypeSpec, Facet) :-
    odbc_types(Connection, TypeSpec, Row),
    type_facet(Facet, Row).

type_facet(name(V), Row)           :- arg(1, Row, V).
type_facet(data_type(V), Row)      :- arg(2, Row, V).
type_facet(precision(V), Row)      :- arg(3, Row, V).
type_facet(literal_prefix(V), Row) :- non_null_arg(4, Row, V).
type_facet(literal_suffix(V), Row) :- non_null_arg(5, Row, V).
type_facet(create_params(V), Row)  :- non_null_arg(6, Row, V).
type_facet(nullable(V), Row)       :- arg(7, Row, I), nullable_arg(I, V).
type_facet(case_sensitive(V), Row) :- bool_arg(8, Row, V).
type_facet(searchable(V), Row)     :- arg(9, Row, I), searchable_arg(I, V).
type_facet(unsigned(V), Row)       :- bool_arg(10, Row, V).
type_facet(money(V), Row)          :- bool_arg(11, Row, V).
type_facet(auto_increment(V), Row) :- bool_arg(12, Row, V).
type_facet(local_name(V), Row)     :- non_null_arg(13, Row, V).
type_facet(minimum_scale(V), Row)  :- non_null_arg(14, Row, V).
type_facet(maximum_scale(V), Row)  :- non_null_arg(15, Row, V).

non_null_arg(Index, Row, V) :-
    arg(Index, Row, V),
    V \== '$null$'.
bool_arg(Index, Row, V) :-
    arg(Index, Row, I),
    int_to_bool(I, V).

int_to_bool(0, false).
int_to_bool(1, true).

nullable_arg(0, false).
nullable_arg(1, true).
nullable_arg(2, unknown).

searchable_arg(0, false).
searchable_arg(1, like_only).
searchable_arg(2, all_except_like).
searchable_arg(4, true).


%!  odbc_data_source(?DSN, ?Description)
%
%   Enumerate the available data-sources

odbc_data_source(DSN, Description) :-
    odbc_data_sources(List),
    member(data_source(DSN, Description), List).

                 /*******************************
                 *    Primary & foreign keys    *
                 *******************************/

%!  odbc_table_primary_key(+Connection, +Table, ?Column)
%
%   Enumerate columns in primary key for table

odbc_table_primary_key(Connection, Table, Column) :-
    (   var(Table)
    ->  odbc_current_table(Connection, Table)
    ;   true
    ),
    (   ground(Column)              % force determinism
    ->  odbc_primary_key(Connection, Table, Tuple),
        arg(4, Tuple, Column), !
    ;   odbc_primary_key(Connection, Table, Tuple),
        arg(4, Tuple, Column)
    ).

%!  odbc_table_foreign_key(+Connection, ?PkTable, ?PkCol, ?FkTable, ?FkCol)
%
%   Enumerate foreign keys columns

odbc_table_foreign_key(Connection, PkTable, PkColumn, FkTable, FkColumn) :-
    odbc_foreign_key(Connection, PkTable, FkTable, Tuple),
    ( var(PkTable) -> arg(3, Tuple, PkTable) ; true ),
    arg(4, Tuple, PkColumn),
    ( var(FkTable) -> arg(7, Tuple, FkTable) ; true ),
    arg(8, Tuple, FkColumn).


                 /*******************************
                 *           STATISTICS         *
                 *******************************/

odbc_statistics(Key) :-
    statistics_key(Key),
    '$odbc_statistics'(Key).

statistics_key(statements(_Created, _Freed)).


                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile
    prolog:message/3.

prolog:message(error(odbc(ODBCCode, _NativeCode, Comment), _)) -->
    [ 'ODBC: State ~w: ~w'-[ODBCCode, Comment] ].
prolog:message(error(context_error(Obj, Error, What), _)) -->
    [ 'Context error: ~w ~w: '-[What, Obj] ],
    context(Error).

prolog:message(odbc(ODBCCode, _NativeCode, Comment)) -->
    [ 'ODBC: State ~w: ~w'-[ODBCCode, Comment] ].
prolog:message(odbc(unexpected_result(Row))) -->
    [ 'ODBC: Unexpected result-row: ~p'-[Row] ].

context(in_use) -->
    [ 'object is in use' ].
