/*  Part of SWI-Prolog

    Author:        Matt Lilley
    E-mail:        matt.s.lilley@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014, Mike Elston, Matt Lilley
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

/*  PostgreSQL is a trademark of the PostgreSQL Global Development Group.
    Microsoft, SQL Server, and Windows are either registered trademarks or
    trademarks of Microsoft Corporation in the United States and/or other
    countries. SQLite is a registered trademark of Hipp, Wyrick & Company,
    Inc in the United States. All other trademarks or registered trademarks
    are the property of their respective owners.
*/

:-module(sql_write, [sql_write/3,
                     sql_quote_codes/3,
                     format_sql_error/3]).

:-use_module(library(cql/sql_keywords)).
:-use_module(library(cql/sql_parser), [strip_sql_comments/2]).
:-use_module(library(cql/cql), [cql_normalize_name/3]).

sql_write(Stream, Term, Options):-
        new_sql_stream(Output),
        sql_write_term(Term, '', Options, Output, Result),
        dump_sql_stream(Result, Stream).

new_sql_stream(sql_stream(T, T, unknown, 0)).
dump_sql_stream(sql_stream(Tokens, [], _, _), Stream):-
        atomic_list_concat(Tokens, '', Atom),
        format(Stream, '~w', [Atom]).

sql_emit_token(Format, Args, Class, Options, sql_stream(Tokens, Tail, OldClass, Indent), sql_stream(Tokens, NewTail, Class, NewIndent)):-
        memberchk(errors(html), Options),
        !,
        format(atom(T2), Format, Args),
        ( fail, Class == OldClass ->
            Tail = [T2|NewTail]
        ; otherwise->
            format(atom(T1), '<span class="~w">', [Class]),
            format(atom(T3), '</span>', []),
            Tail = [T1, T2, T3|NewTail]
        ),
        atomic_list_concat(Lines, '\n', T2),
        ( Lines = [SingleLine]->
            atom_length(SingleLine, Length),
            NewIndent is Indent + Length
        ; otherwise->
            append(_, [LastLine], Lines),
            atom_length(LastLine, NewIndent)
        ).

sql_emit_token(Format, Args, _Class, _Options, sql_stream(Tokens, [Token|NewTail], Class, Indent), sql_stream(Tokens, NewTail, Class, NewIndent)):-
        format(atom(Token), Format, Args),
        atomic_list_concat(Lines, '\n', Token),
        ( Lines = [SingleLine]->
            atom_length(SingleLine, Length),
            NewIndent is Indent + Length
        ; otherwise->
            once(append(_, [LastLine], Lines)), % Apparently this is nondet!
            atom_length(LastLine, NewIndent)
        ).

sql_append_raw_token(Token, sql_stream(Tokens, [Token|NewTail], Class,Indent), sql_stream(Tokens, NewTail, Class, Indent)).

tab_stop(Stop, sql_stream(Tokens, Tail, Class, Indent), sql_stream(Tokens, Tail, Class, Indent)):-
        findall(32, between(1, Indent, _), Spaces),
        atom_codes(Stop, Spaces).


sql_write_term(Var, _, _)--> {var(Var), !, throw(var)}.
sql_write_term(Comments:Term, Indent, Options)--> !,
        sql_write_comments(Comments, Indent, Options),
        sql_write_term(Term, Indent, Options),
        sql_end_comment(Comments, Indent, Options).

sql_write_term(table_definition(Name, Columns), Indent, Options)--> !,
        sql_emit_token('CREATE TABLE ', [], keyword, Options),
        !,
        sql_write_term(Name, Indent, Options),
        ( {Columns == {all}} ->
            {true}
        ; {otherwise}->
            sql_emit_token('(', [], punctuation, Options),
            sql_write_list_with_newlines(Columns, Indent, Options),
            sql_emit_token(')', [], punctuation, Options)
        ).

sql_write_term(domain_definition(Name, Type), Indent, Options)--> !,
        sql_emit_token('CREATE DOMAIN ', [], keyword, Options),
        !,
        sql_write_term(Name, Indent, Options),
        sql_emit_token(' AS ', [], keyword, Options),
        sql_write_type(Type, Indent, Options).


sql_write_term(view_definition(Name, Columns, Expression, With), Indent, Options)--> !,
        sql_emit_token('CREATE VIEW ', [], keyword, Options),
        !,
        sql_write_term(Name, Indent, Options),
        sql_write_term(With, Indent, Options),
        ( {Columns == {all}} ->
            {true}
        ; {otherwise}->
            sql_emit_token('(', [], punctuation, Options),
            sql_write_term(Columns, Indent, Options),
            sql_emit_token(')', [], punctuation, Options)
        ),
        sql_emit_token(' AS~n', [], keyword, Options),
        sql_write_term(Expression, Indent, Options).

sql_write_term(parameter(I), _Indent, Options)--> !,
        ( {memberchk(parameter_bindings(Bindings), Options)}->
            {nth0(I, Bindings, Value)},
            ( {Value = parameter(Name)}->
                sql_emit_token('~w', [Name], parameter, Options)
            ; {otherwise}->
                sql_emit_token('~C', [Value], parameter, Options)
            )
        ; {otherwise}->
            sql_emit_token('?', [], punctuation, Options)
        ).
sql_write_term(table(Name), Indent, Options)--> !,
        ( {memberchk(errors(html), Options),
           strip_sql_comments(Name, identifier(_,RawName))}->
            {format(atom(Token), '<a href="/sql_explorer/~w">', [RawName])},
            sql_append_raw_token(Token),
            sql_write_term(Name, Indent, Options),
            sql_append_raw_token('</a>')
        ; {otherwise}->
            sql_write_term(Name, Indent, Options)
        ).

sql_write_term(domain(Name), Indent, Options)--> !,
        sql_write_term(Name, Indent, Options).

sql_write_term(derived_table(Derivation, Correlation, _Type), Indent, Options)--> !,
        sql_write_term(Derivation, Indent, Options),
        sql_emit_token(' AS ', [], keyword, Options),
        sql_write_term(Correlation, Indent, Options).


sql_write_term(identifier(Schema, Name), Indent, Options)--> !,
        ( {Schema == {no_schema}}->
            {true}
        ; {memberchk(dbms('PostgreSQL'), Options)}->
            % No schema for 'PostgreSQL'
            {true}
        ; {otherwise}->
            sql_write_term(Schema, Indent, Options),
            sql_emit_token('.', [], punctuation, Options)
        ),
        ( {memberchk(dbms('PostgreSQL'), Options)}->
            {strip_sql_comments(Name, NameNoComments),
             cql_normalize_name('PostgreSQL', NameNoComments, Normalized)},
            sql_write_term(Normalized, Indent, Options)
        ; {otherwise}->
            sql_write_term(Name, Indent, Options)
        ).

sql_write_term(schema(Catalog, Name), Indent, Options)--> !,
        ( {Catalog == {no_catalog}}->
            {true}
        ; {memberchk(dbms('PostgreSQL'), Options)}->
            % No catalog for 'PostgreSQL' either
            {true}
        ; {otherwise}->
            sql_write_term(Catalog, Indent, Options),
            sql_emit_token('.', [], punctuation, Options)
        ),
        sql_write_term(Name, Indent, Options).

sql_write_term(literal(Value, decimal(_,_)), _Indent, Options)--> !,
        sql_emit_token('~w', [Value], literal, Options).
sql_write_term(literal(Value, string), _Indent, Options)--> !,
        sql_emit_token('\'', [], literal, Options),
        sql_write_literal(Value, Options),
        sql_emit_token('\'', [], literal, Options).
sql_write_term(literal(Value, identifier), _Indent, Options)--> !,
        ( {memberchk(dbms('PostgreSQL'), Options)},
          sql_emit_token('"', [], literal, Options),
          sql_write_literal(Value, Options),
          sql_emit_token('"', [], literal, Options)
        ; {otherwise}->
            sql_emit_token('[~q]', [Value], unknown, Options)
        ).
sql_write_term(literal(Value, int(_)), _Indent, Options)--> !,
        sql_emit_token('~q', [Value], literal, Options).

sql_write_term(set_function(Functor, Quantifier, Arg), Indent, Options)--> !,
        ( {Functor = Comments:RealFunctor}->
            sql_write_comments(Comments, Indent, Options),
            {upcase_atom(RealFunctor, FunctorUC)},
            sql_write_term(FunctorUC, Indent, Options),
            sql_end_comment(Comments, Indent, Options)
        ; {otherwise}->
            {upcase_atom(Functor, FunctorUC)},
            sql_write_term(FunctorUC, Indent, Options)
        ),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(Quantifier, Indent, Options),
        sql_write_term(Arg, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(count(all), _Indent, Options)--> !,
        sql_emit_token('COUNT', [], function, Options),
        sql_emit_token('(*)', [], punctuation, Options).

sql_write_term(query(Query), Indent, Options)--> !,
        sql_write_term(Query, Indent, Options).

sql_write_term({no_quantifier}, _, _)--> !.
sql_write_term({no_limit}, _, _)--> !.
sql_write_term(all, _, Options)--> !, sql_emit_token(' ALL ', [], operator, Options).
sql_write_term(distinct, _, Options)--> !, sql_emit_token(' DISTINCT ', [], keyword, Options).

sql_write_term(update(Table, Set, From, Where), Indent, Options)--> !,
        sql_emit_token('UPDATE ', [], keyword, Options),
        sql_write_term(Table, Indent, Options),
        sql_emit_token('~n~wSET ', [Indent], keyword, Options),
        tab_stop(NewIndent),
        sql_write_list_with_newlines(Set, NewIndent, Options),
        sql_write_term(From, Indent, Options),
        sql_write_term(Where, Indent, Options).

sql_write_term(delete(Table, Where), Indent, Options)--> !,
        sql_emit_token('DELETE FROM ', [], keyword, Options),
        sql_write_term(Table, Indent, Options),
        sql_write_term(Where, Indent, Options).


sql_write_term(insert(Table, Values), Indent, Options)--> !,
        sql_emit_token('INSERT INTO ', [], keyword, Options),
        sql_write_term(Table, Indent, Options),
        sql_emit_token(' ', [], keyword, Options),
        sql_write_term(Values, Indent, Options).

sql_write_term(insert_source(Source, _Override, Target), Indent, Options)--> !,
        sql_emit_token('(', [], keyword, Options),
        sql_write_list_compact(Source, Indent, Options),
        sql_emit_token(') ', [], keyword, Options),
        sql_write_term(Target, Indent, Options).

sql_write_term(values(List), Indent, Options)--> !,
        sql_emit_token('~n~wVALUES ', [Indent], keyword, Options),
        tab_stop(NewIndent),
        sql_write_list_with_newlines(List, NewIndent, Options).


sql_write_term(set(Target, Source), Indent, Options)--> !,
        sql_write_term(Target, Indent, Options),
        sql_emit_token(' = ', [], operator, Options),
        tab_stop(NewIndent),
        sql_write_term(Source, NewIndent, Options).

sql_write_term(select(Quantifier, Selections, Source, Limit, {no_for}), Indent, Options)--> !,
        sql_emit_token('SELECT ', [], keyword, Options),
        sql_write_term(Quantifier, Indent, Options),
        ( {Selections = _:all}->
            sql_emit_token('*', [], punctuation, Options)
        ; {otherwise}->
            sql_write_list_with_newlines(Selections, Indent, [explicit_literals(true)|Options])
        ),
        sql_emit_token(' ', [], punctuation, Options),
        ( {memberchk(dbms('Microsoft SQL Server'), Options)}->
            sql_write_term(Limit, Indent, Options)
        ; {otherwise}->
            {true}
        ),
        sql_write_term(Source, Indent, Options),
        ( {memberchk(dbms('PostgreSQL'), Options),
           Limit \== {no_limit}}->
            sql_write_term(Limit, Indent, Options)
        ; {otherwise}->
            {true}
        ).

sql_write_term(column(Name, Type, AllowsNulls, IsIdentity, _Default), Indent, Options)--> !,
        ( {memberchk(dbms(DBMS), Options)}->
            {cql_normalize_name(DBMS, Name, NormalizedName)}
        ; {otherwise}->
            {Name = NormalizedName}
        ),
        sql_emit_token('~w ', [NormalizedName], unknown, Options),
        ( {memberchk(dbms('PostgreSQL'), Options),
           IsIdentity == is_identity(true)} ->
            sql_emit_token(' SERIAL', [], keyword, Options)
        ; {Type = domain(Domain)} ->
            {format(atom(Token), '<a href="/sql_explorer/~w">', [Domain])},
            sql_append_raw_token(Token),
            sql_write_term(Type, Indent, Options),
            sql_append_raw_token('</a>')
        ; {otherwise}->
            sql_write_term(Type, Indent, Options)
        ),
        ( {IsIdentity == is_identity(true)}->
            sql_emit_token(' PRIMARY KEY', [], keyword, Options)
        ; {AllowsNulls == allows_nulls(true)}->
            {true}
        ; {otherwise}->
            sql_emit_token(' NOT NULL', [], keyword, Options)
        ).


sql_write_term(select(Quantifier, Selections, Source, Limit, For), Indent, Options)-->
        {memberchk(dbms('PostgreSQL'), Options),
        strip_sql_comments(For, for(ForClause)),
        strip_sql_comments(ForClause, xml_path(Separator)),
        strip_sql_comments(Selections, [derived_column(SingleItem, 'text()')])},
        !,
        sql_emit_token('array_to_string', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_emit_token('ARRAY', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_emit_token('SELECT ', [], keyword, Options),
        sql_write_term(Quantifier, Indent, Options),
        sql_write_term(SingleItem, Indent, Options),
        sql_emit_token(' ', [], punctuation, Options),
        sql_write_term(Source, Indent, Options),
        ( {Limit \== {no_limit}}->
            sql_write_term(Limit, Indent, Options)
        ; {otherwise}->
            {true}
        ),
        sql_emit_token(')', [], punctuation, Options),
        sql_emit_token(', ', [], comma, Options),
        sql_write_term(Separator, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(select(Quantifier, Selections, Source, Limit, For), Indent, Options)-->
        {memberchk(dbms('Microsoft SQL Server'), Options),
        strip_sql_comments(For, for(xml_path(Separator)))},
        !,
        sql_write_term(select(Quantifier, Selections, Source, Limit, {no_for}), Indent, Options),
        sql_emit_token('FOR XML PATH', [], keyword, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(Separator, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(routine(Name, Args), Indent, Options)--> !,
        sql_write_term(Name, Indent, Options),
        sql_emit_token('(', [], punctuation, Options),
        tab_stop(NewIndent),
        {sql_list_length(Args, L)},
        ( {L =:= 0} ->
            % Special case - routine argument lists may be empty. No other SQL lists may be
            {true}
        ; {L < 2}->
            sql_write_list_compact(Args, NewIndent, Options)
        ; {otherwise}->
            sql_write_list_with_newlines(Args, NewIndent, Options)
        ),
        sql_emit_token(')', [], punctuation, Options).


sql_write_term(top(percent(N)), Indent, Options)--> !,
        ( {memberchk(dbms('Microsoft SQL Server'), Options)}->
            sql_emit_token('TOP ', [], keyword, Options),
            sql_write_term(N, Indent, Options),
            sql_emit_token('PERCENT ', [], keyword, Options)
        ; {otherwise}->
            % Ignore TOP 100 PERCENT in 'PostgreSQL'
            {true}
        ).

sql_write_term(top(N), Indent, Options)--> !,
        ( {memberchk(dbms('Microsoft SQL Server'), Options)}->
            sql_emit_token('TOP ', [], keyword, Options),
            sql_write_term(N, Indent, Options)
        ; {otherwise}->
            sql_emit_token('~n~wLIMIT ', [Indent], keyword, Options),
            sql_write_term(N, Indent, Options)
        ).

sql_write_term(column(Qualifier, Name), Indent, Options)--> !,
        ( {Qualifier == {no_qualifier}}->
            {true}
        ; {otherwise}->
            sql_write_term(Qualifier, Indent, Options),
            sql_emit_token('.', [], punctuation, Options)
        ),
        sql_write_and_strip_comments(Name, Indent, Options, StrippedName, Comments),
        ( {reserved_sql_keyword(StrippedName)}->
            ( {memberchk(dbms('PostgreSQL'), Options)}->
                sql_emit_token('"', [], punctuation, Options),
                sql_write_term(StrippedName, Indent, Options),
                sql_emit_token('"', [], punctuation, Options)
            ; {otherwise}->
                sql_emit_token('[', [], punctuation, Options),
                sql_write_term(StrippedName, Indent, Options),
                sql_emit_token(']', [], punctuation, Options)
            )
        ; {otherwise}->
            sql_write_term(StrippedName, Indent, Options)
        ),
        sql_end_comments(Comments, Indent, Options).

sql_write_term(group_expression(Expression, Collation), Indent, Options)--> !,
        sql_write_term(Expression, Indent, Options),
        ( {Collation == {no_collation}} ->
            {true}
        ; {otherwise}->
            sql_emit_token(' COLLATE ', [], keyword, Options),
            sql_write_term(Collation, Indent, Options)
        ).


sql_write_term(group_column(Name, Collation), Indent, Options)--> !,
        sql_write_term(Name, Indent, Options),
        ( {Collation == {no_collation}} ->
            {true}
        ; {otherwise}->
            sql_emit_token(' COLLATE ', [], keyword, Options),
            sql_write_term(Collation, Indent, Options)
        ).


sql_write_term(derived_column(Column, Alias), Indent, Options)--> !,
        ( {Alias \== {no_alias}}->
            sql_write_and_strip_comments(Column, Indent, Options, RawColumn, Comments1),
            ( {memberchk(dbms('PostgreSQL'), Options),
              RawColumn = column(_Qualifier, PossibleLiteral),
              strip_sql_comments(PossibleLiteral, literal(Literal, string))}->
                % If the DBMS is 'PostgreSQL' then when writing out something like
                %   SELECT 'foo' AS bar
                % we have to instead output
                %   SELECT 'foo'::text AS bar
                % if we want the type of bar to be well-defined. The same is probably true of numeric literals
                sql_emit_token('\'', [], punctuation, Options),
                sql_write_literal(Literal, Options),
                sql_emit_token('\'::text', [], punctuation, Options)
            ; {otherwise}->
                sql_write_term(RawColumn, Indent, Options)
            ),
            sql_end_comments(Comments1, Indent, Options),
            sql_emit_token(' AS ', [], keyword, Options),
            sql_write_and_strip_comments(Alias, Indent, Options, Identifier, Comments2),
            ( {atom(Identifier)} ->
                % Must quote any identifiers!
                sql_write_term(literal(Identifier, identifier), Indent, Options)
            ; {Identifier = literal(Value, string)}->
                sql_write_term(literal(Value, identifier), Indent, Options)
            ; {otherwise}->
                {throw(bad_column_alias(Identifier))}
            ),
            sql_end_comments(Comments2, Indent, Options)
        ; {otherwise}->
            sql_write_term(Column, Indent, Options)
        ).

sql_write_term(from(From), Indent, Options)--> !,
        sql_emit_token('~n~wFROM ', [Indent], keyword, Options),
        sql_write_list_with_newlines(From, Indent, Options).

/* Matt-style joins
sql_write_term(join(LHS, RHS), Indent, Options)-->!,
        tab_stop(NewIndent),
        sql_write_term(LHS, Indent, Options),
        sql_emit_token('~n~w', [NewIndent], punctuation, Options),
        sql_write_term(RHS, Indent, Options).

sql_write_term(qualified_join(Type, RHS, On), Indent, Options)--> !,
        tab_stop(NewIndent),
        sql_emit_token('   ', [], punctuation, Options),
        sql_write_term(Type, Indent, Options),
        sql_emit_token('~n~w', [NewIndent], punctuation, Options),
        sql_write_term(RHS, Indent, Options),
        sql_write_term(On, Indent, Options).

sql_write_term(cross_join(RHS), Indent, Options)--> !,
        tab_stop(NewIndent),
        sql_emit_token('  CROSS JOIN~n~w', [NewIndent], operator, Options),
        sql_write_term(RHS, Indent, Options).
*/

/* Chris-style joins */
sql_write_term(join(LHS, RHS), Indent, Options)-->!,
        tab_stop(NewIndent),
        sql_write_term(LHS, Indent, Options),
        sql_emit_token('~n~w', [NewIndent], punctuation, Options),
        sql_write_term(RHS, Indent, Options).

sql_write_term(qualified_join(Type, RHS, On), Indent, Options)--> !,
        tab_stop(NewIndent),
        sql_write_term(Type, Indent, Options),
        sql_emit_token(' ', [], punctuation, Options),
        sql_write_term(RHS, Indent, Options),
        sql_emit_token('~n  ~w', [NewIndent], punctuation, Options),
        sql_write_term(On, Indent, Options).

sql_write_term(cross_join(RHS), Indent, Options)--> !,
        tab_stop(NewIndent),
        sql_emit_token('  CROSS JOIN~n~w', [NewIndent], operator, Options),
        sql_write_term(RHS, Indent, Options).


sql_write_term(correlation(Name, Columns), Indent, Options)-->!,
        sql_write_term(Name, Indent, Options),
        ( {Columns == {no_columns}}->
            {true}
        ; {otherwise}->
            sql_emit_token('(', [], punctuation, Options),
            sql_write_list_compact(Columns, Indent, Options),
            sql_emit_token(')', [], punctuation, Options)
        ).

sql_write_term(correlated_table(Name, Correlation), Indent, Options)--> !,
        sql_write_term(Name, Indent, Options),
        ( {Correlation == {no_correlation}}->
            {true}
        ; {otherwise}->
            sql_emit_token(' AS ', [], keyword, Options),
            sql_write_term(Correlation, Indent, Options)
        ).

sql_write_term(on(Condition), Indent, Options)--> !,
        sql_emit_token(' ON ', [], keyword, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(Condition, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(predicate(P), Indent, Options)--> !,
        sql_write_term(P, Indent, Options).

sql_write_term(comparison(Op, LHS, RHS), Indent, Options)--> !,
        sql_write_term(LHS, Indent, Options),
        sql_emit_token(' ', [], punctuation, Options),
        sql_write_term(Op, Indent, Options),
        sql_emit_token(' ', [], punctuation, Options),
        sql_write_term(RHS, Indent, Options).

sql_write_term(element(A), Indent, Options)--> !,
        sql_write_term(A, Indent, Options).

sql_write_term(and(A, B), Indent, Options)-->
        {memberchk(suppress_collations, Options)},
        {should_suppress_collation(A)},
        !,
        sql_write_term(B, Indent, Options).

sql_write_term(and(A, B), Indent, Options)-->
        {memberchk(suppress_trivial_conditions, Options)},
        {should_suppress_condition(B)},
        !,
        sql_write_term(A, Indent, Options).


sql_write_term(and(A, B), Indent, Options)--> !,
        tab_stop(S),
        sql_write_term(A, Indent, Options),
        sql_emit_token(' AND~n~w', [S], operator, Options),
        sql_write_term(B, Indent, Options).

sql_write_term(or(A, B), Indent, Options)--> !,
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(A, Indent, Options),
        sql_emit_token(') ', [], punctuation, Options),
        sql_emit_token('OR', [], operator, Options),
        sql_emit_token(' (', [], punctuation, Options),
        sql_write_term(B, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(multiply(A,B), Indent, Options)--> !,  % WARNING
        sql_write_term(A, Indent, Options),
        sql_emit_token(' * ', [], operator, Options),
        sql_write_term(B, Indent, Options).

sql_write_term(add(A,B), Indent, Options)--> !,  % WARNING
        sql_write_term(A, Indent, Options),
        sql_emit_token(' + ', [], operator, Options),
        sql_write_term(B, Indent, Options).

sql_write_term(subtract(A,B), Indent, Options)--> !,  % WARNING
        sql_write_term(A, Indent, Options),
        sql_emit_token(' - ', [], operator, Options),
        sql_write_term(B, Indent, Options).

sql_write_term(divide(A,B), Indent, Options)--> !,  % WARNING
        sql_write_term(A, Indent, Options),
        sql_emit_token(' / ', [], operator, Options),
        sql_write_term(B, Indent, Options).

sql_write_term(not(X), Indent, Options)--> !,
        sql_emit_token('NOT', [], operator, Options),
        sql_emit_token(' (', [], punctuation, Options),
        sql_write_term(X, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(round(X, P), Indent, Options)--> !,
        sql_emit_token('ROUND', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(X, Indent, Options),
        sql_emit_token(',', [], comma, Options),
        sql_write_term(P, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(floor(X), Indent, Options)--> !,
        sql_emit_token('FLOOR', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(X, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(ceiling(X), Indent, Options)--> !,
        sql_emit_token('CEILING', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(X, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(float(X), Indent, Options)--> !,
        sql_emit_token('FLOAT', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(X, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(username(X), Indent, Options)--> !, % TBD: Force normalization
        sql_emit_token('USERNAME', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(X, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(permissions(X), Indent, Options)--> !, % TBD: Force normalization
        sql_emit_token('PERMISSIONS', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(X, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(getdate({}), _Indent, Options)-->
        ( {memberchk(dbms('PostgreSQL'), Options) ; memberchk(normalize, Options)}),
        !,
        sql_emit_token('CURRENT_TIMESTAMP', [], function, Options).

sql_write_term(getdate({}), _Indent, Options)--> % TBD: Force normalization
        {memberchk(dbms('Microsoft SQL Server'), Options)},
        !,
        sql_emit_token('GETDATE', [], function, Options),
        sql_emit_token('()', [], punctuation, Options).

sql_write_term(dbname({}), _Indent, Options)--> !, % TBD: Force normalization
        sql_emit_token('DBNAME', [], function, Options),
        sql_emit_token('()', [], punctuation, Options).

sql_write_term(fn_now({}), _Indent, Options)-->
        ( {memberchk(dbms('PostgreSQL'), Options) ; memberchk(normalize, Options)}), !,
        sql_emit_token('CURRENT_TIMESTAMP', [], function, Options).

sql_write_term(fn_now({}), _Indent, Options)--> !, % TBD: Force normalization
        sql_emit_token('{ fn now() }', [], legacy, Options).

sql_write_term(len(X), Indent, Options)-->
        {memberchk(dbms('PostgreSQL'), Options)}, !,
        % The ANSI string-length function is called CHAR_LENGTH. This is, incredibly, unsupported by SQL Server
        sql_emit_token('CHAR_LENGTH', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(X, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(len(X), Indent, Options)--> !,
        sql_emit_token('LEN', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(X, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(str(X), Indent, Options)-->
        {memberchk(dbms('PostgreSQL'), Options)},
        !,
        % STR in SQL Server is used to convert floats to strings.
        % The default length is 9, and the default precision is 0
        % The 'PostgreSQL' equivalent is therefore like to_char(X, '9999999999')
        sql_emit_token('TO_CHAR', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(X, Indent, Options),
        sql_emit_token(', ', [], comma, Options),
        sql_emit_token('\'9999999999\'', [], literal, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(str(X), Indent, Options)--> !,
        sql_emit_token('STR', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(X, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(concatenate(A,B), Indent, Options)--> !,
        sql_write_term(A, Indent, Options),
        ( {memberchk(dbms('Microsoft SQL Server'), Options)}->
            sql_emit_token(' + ', [], punctuation, Options)
        ; {otherwise}->
            sql_emit_token(' || ', [], punctuation, Options)
        ),
        sql_write_term(B, Indent, Options).

sql_write_term(add_interval(A,B), Indent, Options)--> !,
        sql_write_term(A, Indent, Options),
        sql_emit_token(' + ', [], punctuation, Options),
        ( {memberchk(dbms('Microsoft SQL Server'), Options)}->
            sql_write_term(B, Indent, Options)
        ; {otherwise}->
            sql_emit_token('CAST', [], function, Options),
            sql_emit_token('(', [], punctuation, Options),
            sql_write_term(B, Indent, Options),
            sql_emit_token(' || ', [], punctuation, Options),
            sql_emit_token(' \' days\'', [], literal, Options),
            sql_emit_token(' AS', [], keyword, Options),
            sql_emit_token(' interval', [], function, Options),
            sql_emit_token(')', [], punctuation, Options)
        ).

sql_write_term(left(V, N), Indent, Options)--> !,
        sql_emit_token('LEFT', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(V, Indent, Options),
        sql_emit_token(', ', [], comma, Options),
        sql_write_term(N, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(right(V, N), Indent, Options)--> !,
        sql_emit_token('RIGHT', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(V, Indent, Options),
        sql_emit_token(', ', [], comma, Options),
        sql_write_term(N, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(rtrim(V), Indent, Options)--> !,
        sql_emit_token('RTRIM', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(V, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(ltrim(V), Indent, Options)--> !,
        sql_emit_token('LTRIM', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(V, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(upper(V), Indent, Options)--> !,
        sql_emit_token('UPPER', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(V, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(lower(V), Indent, Options)--> !,
        sql_emit_token('LOWER', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(V, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(day(A), Indent, Options)-->
        {memberchk(dbms('PostgreSQL'), Options)}, !,
        sql_emit_token('DATE_PART', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_emit_token('\'day\'', [], literal, Options),
        sql_emit_token(', ', [], comma, Options),
        sql_write_term(A, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(month(A), Indent, Options)-->
        {memberchk(dbms('PostgreSQL'), Options)}, !,
        sql_emit_token('DATE_PART', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_emit_token('\'month\'', [], literal, Options),
        sql_emit_token(', ', [], comma, Options),
        sql_write_term(A, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(year(A), Indent, Options)-->
        {memberchk(dbms('PostgreSQL'), Options)}, !,
        sql_emit_token('DATE_PART', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_emit_token('\'year\'', [], literal, Options),
        sql_emit_token(', ', [], comma, Options),
        sql_write_term(A, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(day(A), Indent, Options)--> !, % TBD: Force normalization
        sql_emit_token('DAY', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(A, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(month(A), Indent, Options)--> !, % TBD: Force normalization
        sql_emit_token('MONTH', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(A, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(year(A), Indent, Options)--> !, % TBD: Force normalization
        sql_emit_token('YEAR', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(A, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(dateadd(A,B,C), Indent, Options)-->
        {memberchk(dbms('PostgreSQL'), Options)}, !,
        sql_write_and_strip_comments(A, Indent, Options, Class, Comments),
        % Quirk. SQL Server allows implicit cast of 0 to a datetime to get 1/1/1901.
        sql_write_date(C, Indent, Options),
        sql_emit_token(' + ', [], punctuation, Options),
        sql_emit_token('CAST', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_emit_token('CAST', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(B, Indent, Options),
        sql_emit_token(' AS ', [], keyword, Options),
        sql_emit_token('text', [], function, Options),
        sql_emit_token(')', [], punctuation, Options),
        sql_emit_token(' || ', [], punctuation, Options),
        sql_emit_token('\' ~w\'', [Class], literal, Options),
        sql_emit_token(' AS ', [], keyword, Options),
        sql_emit_token('interval', [], function, Options),
        sql_emit_token(')', [], punctuation, Options),
        sql_end_comments(Comments, Indent, Options).

sql_write_term(dateadd(A,B,C), Indent, Options)--> !, % TBD: Force normalization
        sql_emit_token('DATEADD', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(A, Indent, Options),
        sql_emit_token(', ', [], comma, Options),
        sql_write_term(B, Indent, Options),
        sql_emit_token(', ', [], comma, Options),
        sql_write_term(C, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(datepart(A,B), Indent, Options)-->
        ( {memberchk(dbms('PostgreSQL'), Options) ; memberchk(normalize, Options)}),
        !,
        sql_emit_token('EXTRACT', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_emit_token('\'', [], literal, Options),
        sql_write_term(A, Indent, Options),
        sql_emit_token('\'', [], literal, Options),
        sql_emit_token(' FROM ', [], keyword, Options),
        sql_write_term(B, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).


sql_write_term(datepart(A,B), Indent, Options)--> !, % TBD: Force normalization
        sql_emit_token('EXTRACT', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_emit_token('\'', [], literal, Options),
        sql_write_term(A, Indent, Options),
        sql_emit_token('\'', [], literal, Options),
        sql_emit_token(', ', [], comma, Options),
        sql_write_term(B, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).


sql_write_term(datename(A,B), Indent, Options)-->
        {memberchk(dbms('PostgreSQL'), Options)}, !, % Also Oracle
        sql_emit_token('TO_CHAR', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(B, Indent, Options),
        sql_emit_token(', ', [], comma, Options),
        sql_write_and_strip_comments(A, Indent, Options, AA, Comments),
        ( {normalize_date_type(AA, Type)}->
            {true}
        ; {otherwise}->
            {throw(cql_error(cannot_canonicalize_date_part, AA))}
        ),
        ( {Type == day_of_week} ->
            sql_emit_token('\'Day\'', [], literal, Options)
        ; {otherwise}->
            {throw(cql_error(cannot_map_date_type, Type))}
        ),
        sql_end_comments(Comments, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(datename(A,B), Indent, Options)--> !, % TBD: Force normalization
        sql_emit_token('DATENAME', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(A, Indent, Options),
        sql_emit_token(', ', [], comma, Options),
        sql_write_term(B, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(datediff(A,B,C), Indent, Options)-->
        {memberchk(dbms('PostgreSQL'), Options)}, !,
        sql_write_and_strip_comments(A, Indent, Options, AA, Comments),
        ( {normalize_date_type(AA, Type)}->
            {true}
        ; {otherwise}->
            {throw(cql_error(cannot_canonicalize_date_part, AA))}
        ),
        ( {Type == day}->
            sql_emit_token('DATE_PART', [], function, Options),
            sql_emit_token('(', [], punctuation, Options),
            sql_emit_token('\'day\'', [], literal, Options),
            sql_emit_token(', ', [], comma, Options),
            sql_write_date(C, Indent, Options),
            sql_emit_token(' - ', [], punctuation, Options),
            sql_write_date(B, Indent, Options),
            sql_emit_token(') ', [], punctuation, Options)
        ; {Type == week} ->
            sql_emit_token('TRUNC', [], function, Options),
            sql_emit_token('(', [], punctuation, Options),
            sql_emit_token('DATE_PART', [], function, Options),
            sql_emit_token('(', [], punctuation, Options),
            sql_emit_token('\'day\'', [], literal, Options),
            sql_emit_token(', ', [], comma, Options),
            sql_write_date(C, Indent, Options),
            sql_emit_token(' - ', [], punctuation, Options),
            sql_write_date(B, Indent, Options),
            sql_emit_token(')', [], punctuation, Options),
            sql_emit_token(' / ', [], punctuation, Options),
            sql_emit_token('7', [], literal, Options),
            sql_emit_token(')', [], punctuation, Options)
        ; {Type == second} ->
            % This is unfortunately quite complicated. Basically:
            % days_diff = DATE_PART('day', end - start)
            % hours_diff = days_diff * 24 + DATE_PART('hour', end - start )
            % minutes_diff = hours_diff * 60 + DATE_PART('minute', end - start )
            % seconds_diff = minutes_diff * 60 + DATE_PART('second', end - start )
            % So overall
            % ((DATE_PART('day', end - start) * 24 + DATE_PART('hour', end - start )) * 60 + DATE_PART('minute', end - start )) * 60 + DATE_PART('second', end - start )

            sql_emit_token('(', [], punctuation, Options),
            sql_emit_token('(', [], punctuation, Options),
            sql_emit_token('DATE_PART', [], function, Options),
            sql_emit_token('(', [], punctuation, Options),
            sql_emit_token('\'day\'', [], literal, Options),
            sql_emit_token(', ', [], comma, Options),
            sql_write_term(C, Indent, Options),
            sql_emit_token(' - ', [], punctuation, Options),
            sql_write_term(B, Indent, Options),
            sql_emit_token(') ', [], punctuation, Options),
            sql_emit_token(' * ', [], punctuation, Options),
            sql_emit_token('24', [], literal, Options),
            sql_emit_token(' + ', [], punctuation, Options),
            sql_emit_token('DATE_PART', [], function, Options),
            sql_emit_token('(', [], punctuation, Options),
            sql_emit_token('\'hour\'', [], literal, Options),
            sql_emit_token(', ', [], comma, Options),
            sql_write_term(C, Indent, Options),
            sql_emit_token(' - ', [], punctuation, Options),
            sql_write_term(B, Indent, Options),
            sql_emit_token(')', [], punctuation, Options),
            sql_emit_token(')', [], punctuation, Options),
            sql_emit_token(' * ', [], punctuation, Options),
            sql_emit_token('60', [], literal, Options),
            sql_emit_token(' + ', [], punctuation, Options),
            sql_emit_token('DATE_PART', [], function, Options),
            sql_emit_token('(', [], punctuation, Options),
            sql_emit_token('\'minute\'', [], literal, Options),
            sql_emit_token(', ', [], comma, Options),
            sql_write_term(C, Indent, Options),
            sql_emit_token(' - ', [], punctuation, Options),
            sql_write_term(B, Indent, Options),
            sql_emit_token(')', [], punctuation, Options),
            sql_emit_token(')', [], punctuation, Options),
            sql_emit_token(' * ', [], punctuation, Options),
            sql_emit_token('60', [], literal, Options),
            sql_emit_token(' + ', [], punctuation, Options),
            sql_emit_token('DATE_PART', [], function, Options),
            sql_emit_token('(', [], punctuation, Options),
            sql_emit_token('\'second\'', [], literal, Options),
            sql_emit_token(', ', [], comma, Options),
            sql_write_term(C, Indent, Options),
            sql_emit_token(' - ', [], punctuation, Options),
            sql_write_term(B, Indent, Options),
            sql_emit_token(') ', [], punctuation, Options)
        ; {Type == year} ->
            sql_emit_token('DATE_PART', [], function, Options),
            sql_emit_token('(', [], punctuation, Options),
            sql_emit_token('\'year\'', [], literal, Options),
            sql_emit_token(', ', [], comma, Options),
            sql_write_date(C, Indent, Options),
            sql_emit_token(' - ', [], punctuation, Options),
            sql_write_date(B, Indent, Options),
            sql_emit_token(') ', [], punctuation, Options)
        ; {Type == month}->
            sql_emit_token('DATE_PART', [], function, Options),
            sql_emit_token('(', [], punctuation, Options),
            sql_emit_token('\'year\'', [], literal, Options),
            sql_emit_token(', ', [], comma, Options),
            sql_write_date(C, Indent, Options),
            sql_emit_token(' - ', [], punctuation, Options),
            sql_write_date(B, Indent, Options),
            sql_emit_token(') ', [], punctuation, Options),
            sql_emit_token(' * ', [], punctuation, Options),
            sql_emit_token('12', [], literal, Options),
            sql_emit_token(' + ', [], punctuation, Options),
            sql_emit_token('DATE_PART', [], function, Options),
            sql_emit_token('(', [], punctuation, Options),
            sql_emit_token('\'month\'', [], literal, Options),
            sql_emit_token(', ', [], comma, Options),
            sql_write_date(C, Indent, Options),
            sql_emit_token(' - ', [], punctuation, Options),
            sql_write_date(B, Indent, Options),
            sql_emit_token(')', [], punctuation, Options)
        ; {otherwise}->
            {throw(cql_error(cannot_datediff, AA))}
        ),
        sql_end_comments(Comments, Indent, Options).

sql_write_term(datediff(A,B,C), Indent, Options)--> !, % TBD: Force normalization
        sql_emit_token('DATEDIFF', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(A, Indent, Options),
        sql_emit_token(', ', [], comma, Options),
        sql_write_term(B, Indent, Options),
        sql_emit_token(', ', [], comma, Options),
        sql_write_term(C, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(replace(A,B,C), Indent, Options)--> !,
        sql_emit_token('REPLACE', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(A, Indent, Options),
        sql_emit_token(', ', [], comma, Options),
        sql_write_term(B, Indent, Options),
        sql_emit_token(', ', [], comma, Options),
        sql_write_term(C, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(substring(A,B,C), Indent, Options)--> !,
        sql_emit_token('SUBSTRING', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(A, Indent, Options),
        sql_emit_token(', ', [], comma, Options),
        sql_write_term(B, Indent, Options),
        sql_emit_token(', ', [], comma, Options),
        sql_write_term(C, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(charindex(ExpressionToFind, ExpressionToSearch, StartLocation), Indent, Options)-->
        ( {memberchk(dbms('PostgreSQL'), Options) ; memberchk(normalize, Options)}),
        !,
        ( {strip_sql_comments(StartLocation, {no_start})}->
            sql_emit_token('POSITION', [], function, Options),
            sql_emit_token('(', [], punctuation, Options),
            sql_write_term(ExpressionToFind, Indent, Options),
            sql_emit_token(' IN ', [], keyword, Options),
            sql_write_term(ExpressionToSearch, Indent, Options),
            sql_emit_token(')', [], punctuation, Options)
        ; {otherwise}->
            sql_emit_token('POSITION', [], function, Options),
            sql_emit_token('(', [], punctuation, Options),
            sql_write_term(ExpressionToFind, Indent, Options),
            sql_emit_token(' IN SUBSTRING', [], keyword, Options),
            sql_emit_token('(', [], punctuation, Options),
            sql_write_term(ExpressionToSearch, Indent, Options),
            sql_emit_token(' FROM ', [], keyword, Options),
            sql_write_term(StartLocation, Indent, Options),
            sql_emit_token(')', [], punctuation, Options),
            sql_emit_token(')', [], punctuation, Options)
        ).

sql_write_term(charindex(A,B,C), Indent, Options)--> !,
        sql_emit_token('CHARINDEX', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(A, Indent, Options),
        sql_emit_token(', ', [], comma, Options),
        sql_write_term(B, Indent, Options),
        ( {C == {no_start}} ->
            {true}
        ; {otherwise}->
            sql_emit_token(', ', [], comma, Options),
            sql_write_term(C, Indent, Options)
        ),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(precision_cast(A,B,C), Indent, Options)--> !,
        ( {memberchk(dbms('Microsoft SQL Server'), Options),
           \+memberchk(normalize, Options)}->
            sql_emit_token('CONVERT', [], function, Options),
            sql_emit_token('(', [], punctuation, Options),
            sql_write_term(A, Indent, Options),
            sql_emit_token(', ', [], comma, Options),
            sql_write_term(B, Indent, Options),
            ( {C == {no_precision}} ->
                {true}
            ; {otherwise}->
                sql_emit_token(', ', [], comma, Options),
                sql_write_term(C, Indent, Options)
            ),
            sql_emit_token(')', [], punctuation, Options)
        ; {otherwise}->
            ( {C == {no_precision}} ->
                sql_emit_token('CAST', [], function, Options),
                sql_emit_token('(', [], punctuation, Options),
                sql_write_term(B, Indent, Options),
                sql_emit_token(' AS ', [], keyword, Options),
                sql_write_term(A, Indent, Options)
            ; {A = _:native_type(NativeType),
              strip_sql_comments(NativeType, varchar(_))}->
                sql_emit_token('CAST', [], function, Options),
                sql_emit_token('(', [], punctuation, Options),
                sql_write_term(B, Indent, Options),
                sql_emit_token(' AS ', [], keyword, Options),
                sql_emit_token('VARCHAR', [], keyword, Options),
                sql_emit_token('(', [], punctuation, Options),
                sql_write_term(C, Indent, Options),
                sql_emit_token(')', [], punctuation, Options)
            ; {otherwise}->
                {throw(unnormalizable(precision_cast(A,C)))}
            ),
            sql_emit_token(')', [], punctuation, Options)
        ).

sql_write_term(cast(A, B), Indent, Options)--> !,
        sql_emit_token('CAST', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(A, Indent, Options),
        sql_emit_token(' AS ', [], keyword, Options),
        sql_write_term(B, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(native_type(A), Indent, Options)--> !,
        sql_write_type(A, Indent, Options).

sql_write_term(like(LHS,Pattern,Escape), Indent, Options)--> !,
        sql_write_term(LHS, Indent, Options),
        sql_emit_token(' LIKE ', [], operator, Options),
        sql_write_term(Pattern, Indent, Options),
        ( {Escape == {no_escape}}->
            {true}
          ; {otherwise}->
            sql_emit_token(' ESCAPE ', [], keyword, Options),
            sql_write_term(Escape, Indent, Options)
        ).

sql_write_term(not_like(LHS,Pattern,Escape), Indent, Options)-->!,
        sql_write_term(LHS, Indent, Options),
        sql_emit_token(' NOT LIKE ', [], operator, Options),
        sql_write_term(Pattern, Indent, Options),
        ( {Escape == {no_escape}}->
            {true}
        ; {otherwise}->
            sql_emit_token(' ESCAPE ', [], keyword, Options),
            sql_write_term(Escape, Indent, Options)
        ).

sql_write_term({no_from}, _, _)--> !.
sql_write_term({no_where}, _, _)--> !.
sql_write_term({no_groupby}, _, _)--> !.
sql_write_term({no_orderby}, _, _)--> !.
sql_write_term({no_having}, _, _)--> !.
sql_write_term({default_values}, _Indent, Options)--> !,
        sql_emit_token(' DEFAULT VALUES ', [], keyword, Options).

sql_write_term(source(From, Where, GroupBy, OrderBy, Having), Indent, Options)--> !,
        sql_write_term(From, Indent, Options),
        sql_write_term(Where, Indent, Options),
        sql_write_term(GroupBy, Indent, Options),
        ( {memberchk(dbms('PostgreSQL'), Options)}->
            sql_write_term(Having, Indent, Options),
            sql_write_term(OrderBy, Indent, Options)
        ; {otherwise}->
            sql_write_term(OrderBy, Indent, Options),
            sql_write_term(Having, Indent, Options)
        ).

sql_write_term(exists(A), _Indent, Options)--> !,
        sql_emit_token('EXISTS ', [], operator, Options),
        tab_stop(S),
        sql_write_term(A, S, Options).

sql_write_term(cast(A,B), Indent, Options)--> !,
        sql_emit_token('CAST', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(A, Indent, Options),
        sql_emit_token(', ', [], comma, Options),
        sql_write_term(B, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(coalesce(List), Indent, Options)--> !,
        sql_emit_token('COALESCE', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_list_compact(List, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(isnull(A, B), Indent, Options)--> !,
        ( {memberchk(dbms('Microsoft SQL Server'), Options),
           \+memberchk(normalize, Options)}->
            sql_emit_token('ISNULL', [], function, Options),
            sql_emit_token('(', [], punctuation, Options),
            sql_write_term(A, Indent, Options),
            sql_emit_token(', ', [], comma, Options),
            sql_write_term(B, Indent, Options),
            sql_emit_token(')', [], punctuation, Options)
        ; {otherwise}->
            sql_emit_token('COALESCE', [], function, Options),
            sql_emit_token('(', [], punctuation, Options),
            sql_write_term(A, Indent, Options),
            sql_emit_token(', ', [], comma, Options),
            sql_write_term(B, Indent, Options),
            sql_emit_token(')', [], punctuation, Options)
        ).

sql_write_term(negative(A), Indent, Options)--> !,
        sql_emit_token('-', [], punctuation, Options), % WARNING: Order of operations
        sql_write_term(A, Indent, Options).

sql_write_term(abs(A), Indent, Options)--> !,
        sql_emit_token('ABS', [], function, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(A, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(else(Else), Indent, Options)--> !,
        sql_write_term(Else, Indent, Options).

sql_write_term(simple_case(Operand, Cases, Else), _Indent, Options)-->!,
        tab_stop(S),
        sql_emit_token('CASE ', [], keyword, Options),
        tab_stop(SS),
        sql_write_term(Operand, SS, Options),
        sql_emit_token('~n~w', [SS], punctuation, Options),
        sql_write_list_with_newlines_and_no_commas(Cases, SS, Options),
        ( {Else == {no_else}}->
            {true}
        ; {otherwise}->
            sql_emit_token('~n~w     ELSE ', [S], keyword, Options),
            tab_stop(SSS),
            sql_write_term(Else, SSS, Options)
        ),
        sql_emit_token('~n~wEND', [S], keyword, Options).

sql_write_term(case(Cases, Else), _Indent, Options)-->!,
        tab_stop(S),
        sql_emit_token('CASE ', [], keyword, Options),
        tab_stop(SS),
        sql_write_list_with_newlines_and_no_commas(Cases, SS, Options),
        ( {Else == {no_else}}->
            {true}
        ; {otherwise}->
            sql_emit_token('~n~w     ELSE ', [S], keyword, Options),
            tab_stop(SSS),
            sql_write_term(Else, SSS, Options)
        ),
        sql_emit_token('~n~wEND', [S], keyword, Options).

sql_write_term(when(searched(S), R), Indent, Options)--> !,
        sql_emit_token('WHEN ', [], keyword, Options),
        sql_write_term(S, Indent, Options),
        sql_emit_token('~n~w  THEN ', [Indent], keyword, Options),
        sql_write_term(R, Indent, Options).

sql_write_term(when(Match, R), Indent, Options)--> !,
        sql_emit_token('WHEN ', [], keyword, Options),
        sql_write_term(Match, Indent, Options),
        sql_emit_token('~n~w  THEN ', [Indent], keyword, Options),
        sql_write_term(R, Indent, Options).

sql_write_term(having(Having), Indent, Options)--> !,
        sql_emit_token('~n~w', [Indent], punctuation, Options),
        sql_emit_token('HAVING ', [], keyword, Options),
        sql_write_term(Having, Indent, Options).

sql_write_term(where(Where), Indent, Options)--> !,
        sql_emit_token('~n~w', [Indent], punctuation, Options),
        sql_emit_token('WHERE ', [], keyword, Options),
        sql_write_term(Where, Indent, Options).

sql_write_term(group_by(Groupings), Indent, Options)--> !,
        sql_emit_token('~n~w', [Indent], punctuation, Options),
        sql_emit_token('GROUP BY ', [], keyword, Options),
        sql_write_list_with_newlines(Groupings, Indent, Options).

sql_write_term(order_by(Orderings), Indent, Options)--> !,
        sql_emit_token('~n~w', [Indent], punctuation, Options),
        sql_emit_token('ORDER BY ', [], keyword, Options),
        sql_write_list_with_newlines(Orderings, Indent, Options).

sql_write_term(subquery(Q), _Indent, Options)--> !,
        sql_emit_token('( ', [], punctuation, Options),
        tab_stop(S),
        sql_write_term(Q, S, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(collate(C), Indent, Options)--> !,
        sql_write_term(C, Indent, Options).

sql_write_term(collation(C), Indent, Options)--> !,
        sql_write_term(C, Indent, Options).

sql_write_term(collated_factor(F, C), Indent, Options)-->
        {memberchk(dbms('PostgreSQL'), Options) ; memberchk(suppress_collations, Options)},
        !,
        sql_write_term(F, Indent, Options),
        sql_write_and_strip_comments(C, Indent, Options, _Collation, Comments),
        sql_end_comments(Comments, Indent, Options).
        % TBD: All collations for 'PostgreSQL' are just ignored.
        %sql_write_term(Collation, Indent, Options).


sql_write_term(collated_factor(F, C), Indent, Options)-->!,
        sql_write_term(F, Indent, Options),
        sql_emit_token(' COLLATE ', [], keyword, Options),
        sql_write_term(C, Indent, Options).

sql_write_term(sort_column(C), Indent, Options)--> !,
        sql_write_term(C, Indent, Options).
sql_write_term(index(I), Indent, Options)--> !, % Should we normalize this?
        sql_write_term(I, Indent, Options).
sql_write_term(sort_expression(Expression), Indent, Options)--> !,
        sql_write_term(Expression, Indent, Options).


sql_write_term(sort_key(Key, Collate, Order), Indent, Options)--> !,
        sql_write_term(Key, Indent, Options),
        ( {Collate == {no_collation}} ->
            {true}
        ; {otherwise}->
            sql_emit_token(' COLLATE ', [], keyword, Options),
            sql_write_term(Collate, Indent, Options)
        ),
        ( {Order == {no_order}} ->
            ( {memberchk(normalize, Options)}->
                sql_emit_token(' ASC ', [], keyword, Options)
            ; {otherwise}->
                {true}
            )
        ; {otherwise}->
            sql_write_term(Order, Indent, Options)
        ).

sql_write_term(desc, _, Options)-->!, sql_emit_token(' DESC ', [], keyword, Options).
sql_write_term(asc, _, Options)-->!, sql_emit_token(' ASC ', [], keyword, Options).

sql_write_term(search(S), Indent, Options)--> !,
        sql_write_term(S, Indent, Options).

sql_write_term(in(Value, List), Indent, Options)--> !,
        sql_write_term(Value, Indent, Options),
        sql_emit_token(' IN ', [], operator, Options),
        sql_write_term(List, Indent, Options).

sql_write_term(not_in(Value, List), Indent, Options)--> !,
        sql_write_term(Value, Indent, Options),
        sql_emit_token(' NOT IN ', [], operator, Options),
        sql_write_term(List, Indent, Options).

sql_write_term(between(Value, Min, Max), Indent, Options)--> !,
        sql_write_term(Value, Indent, Options),
        sql_emit_token(' BETWEEN ', [], operator, Options),
        sql_write_term(Min, Indent, Options),
        sql_emit_token(' AND ', [], operator, Options),
        sql_write_term(Max, Indent, Options).

sql_write_term(list(Values), Indent, Options)--> !,
        sql_emit_token('(', [], punctuation, Options),
        sql_write_list_compact(Values, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_term(join_type(Type), Indent, Options)--> !,
        sql_write_term(Type, Indent, Options).

sql_write_term(inner, _, Options)--> !,
        sql_emit_token('INNER JOIN ', [], operator, Options).
sql_write_term(outer(T1), Indent, Options)--> !,
        sql_write_term(T1, Indent, Options),
        sql_emit_token(' OUTER JOIN ', [], operator, Options).

sql_write_term(left, _, Options)--> !, sql_emit_token('LEFT', [], operator, Options).
sql_write_term(right, _, Options)--> !, sql_emit_token('RIGHT', [], operator, Options).
sql_write_term(full, _, Options)--> !, sql_emit_token('FULL', [], operator, Options).

sql_write_term(is_not_null(X), Indent, Options)--> !,
        sql_write_term(X, Indent, Options),
        sql_emit_token(' IS NOT NULL', [], operator, Options).

sql_write_term(is_null(X), Indent, Options)--> !,
        sql_write_term(X, Indent, Options),
        sql_emit_token(' IS NULL', [], operator, Options).

sql_write_term(union(LHS, RHS, Corresponding), Indent, Options)--> !,
        ( {memberchk(unions(left), Options)}->
            sql_emit_token('    ', [], punctuation, Options),
            tab_stop(S),
            sql_write_term(LHS, S, Options),
            sql_emit_token('~n~wUNION~n~w', [Indent, Indent], keyword, Options),
            ( {memberchk(unroll_unions(true), Options),
              RHS = union(_, _)}->
                sql_write_term(RHS, Indent, Options)
            ; {otherwise}->
                sql_emit_token('    ', [], punctuation, Options),
                sql_write_term(RHS, S, Options)
            )
        ; {otherwise}->
            tab_stop(S),
            sql_write_term(LHS, Indent, Options),
            sql_emit_token('~n~w  UNION~n~w', [S, S], keyword, Options),
            sql_write_term(RHS, Indent, Options)
        ),
        ( {Corresponding == {no_corresponding}}->
            {true}
        ; {otherwise}->
            sql_write_term(Corresponding, Indent, Options)
        ).

sql_write_term(union_all(LHS, RHS, Corresponding), Indent, Options)--> !,
        tab_stop(S),
        sql_write_term(LHS, Indent, Options),
        sql_emit_token('~n~w  UNION', [S], keyword, Options),
        sql_emit_token(' ALL~n~w', [S], operator, Options),
        sql_write_term(RHS, Indent, Options),
        ( {Corresponding == {no_corresponding}}->
            {true}
        ; {otherwise}->
            sql_write_term(Corresponding, Indent, Options)
        ).

sql_write_term(except(LHS, RHS, Corresponding), Indent, Options)--> !,
        tab_stop(S),
        sql_write_term(LHS, Indent, Options),
        sql_emit_token('~n~w  EXCEPT~n~w', [S, S], keyword, Options),
        sql_write_term(RHS, Indent, Options),
        ( {Corresponding == {no_corresponding}}->
            {true}
        ; {otherwise}->
            sql_write_term(Corresponding, Indent, Options)
        ).

sql_write_term(except_all(LHS, RHS, Corresponding), Indent, Options)--> !,
        tab_stop(S),
        sql_write_term(LHS, Indent, Options),
        sql_emit_token('~n~w  EXCEPT', [S], keyword, Options),
        sql_emit_token(' ALL~n~w', [S], operator, Options),
        sql_write_term(RHS, Indent, Options),
        ( {Corresponding == {no_corresponding}}->
            {true}
        ; {otherwise}->
            sql_write_term(Corresponding, Indent, Options)
        ).

sql_write_term({no_with}, _, _)--> !.
sql_write_term(with(schemabinding), _, Options)--> !,
        ( {memberchk(dbms('Microsoft SQL Server'), Options)}->
            sql_emit_token(' WITH SCHEMABINDING', [], keyword, Options)
        ; {otherwise}->
            {true}
        ).

sql_write_term({null}, _Indent, Options)--> !,
        sql_emit_token('NULL', [], null, Options).

sql_write_term(join, _Indent, Options)--> !,
        sql_emit_token('JOIN', [], keyword, Options).

sql_write_term(Atom, _Indent, Options)-->
        {atomic(Atom)}, !,
        sql_emit_token('~w', [Atom], unknown, Options).

sql_write_term({Foo}, _, _)-->
        {throw(sql_write_curly(Foo))}.
sql_write_term(Other, _, _)-->
        {functor(Other, Functor, Arity),
         throw(sql_write_term(Functor/Arity))}.

sql_write_list_compact(Comments:List, Indent, Options)--> !,
        sql_write_comments(Comments, Indent, Options),
        sql_write_list_compact(List, Indent, Options),
        sql_end_comment(Comments, Indent, Options).

sql_write_list_compact([Tail], Indent, Options)--> !,
        sql_write_term(Tail, Indent, Options).

sql_write_list_compact([Head|Tail], Indent, Options)--> !,
        sql_write_term(Head, Indent, Options),
        sql_emit_token(', ', [], comma, Options),
        sql_write_list_compact(Tail, Indent, Options).

sql_write_list_with_newlines(Comments:List, Indent, Options)--> !,
        sql_write_comments(Comments, Indent, Options),
        sql_write_list_with_newlines(List, Indent, Options),
        sql_end_comment(Comments, Indent, Options).

sql_write_list_with_newlines(List, _ExistingIndent, Options)-->
        tab_stop(S),
        sql_write_list_with_newlines_1(List, S, Options).

sql_write_list_with_newlines_1(Comments:List, Indent, Options)--> !,
        sql_write_comments(Comments, Indent, Options),
        sql_write_list_with_newlines(List, Indent, Options),
        sql_end_comment(Comments, Indent, Options).

sql_write_list_with_newlines_1([Tail], Indent, Options)--> !,
        sql_write_term(Tail, Indent, Options).

sql_write_list_with_newlines_1([Head|Tail], Indent, Options)--> !,
        sql_write_term(Head, Indent, Options),
        sql_emit_token(',~n~w', [Indent], comma, Options),
        sql_write_list_with_newlines_1(Tail, Indent, Options).

sql_write_list_with_newlines_and_no_commas(Comments:List, Indent, Options)--> !,
        sql_write_comments(Comments, Indent, Options),
        sql_write_list_with_newlines_and_no_commas(List, Indent, Options),
        sql_end_comment(Comments, Indent, Options).

sql_write_list_with_newlines_and_no_commas(List, _ExistingIndent, Options)-->
        tab_stop(S),
        sql_write_list_with_newlines_and_no_commas_1(List, S, Options).

sql_write_list_with_newlines_and_no_commas_1(Comments:List, Indent, Options)--> !,
        sql_write_comments(Comments, Indent, Options),
        sql_write_list_with_newlines_and_no_commas(List, Indent, Options),
        sql_end_comment(Comments, Indent, Options).

sql_write_list_with_newlines_and_no_commas_1([Tail], Indent, Options)--> !,
        sql_write_term(Tail, Indent, Options).

sql_write_list_with_newlines_and_no_commas_1([Head|Tail], Indent, Options)--> !,
        sql_write_term(Head, Indent, Options),
        sql_emit_token('~n~w', [Indent], punctuation, Options),
        sql_write_list_with_newlines_and_no_commas_1(Tail, Indent, Options).

sql_write_and_strip_comments(Comments:Term, Indent, Options, X, [Comments|Y])-->
        !,
        sql_write_comments(Comments, Indent, Options),
        sql_write_and_strip_comments(Term, Indent, Options, X, Y).

sql_write_and_strip_comments(Term, _Indent, _Options, Term, [])--> [].


sql_write_comments(meta(Comments, Errors), Indent, Options)--> !,
        ( {Errors == {null}} ->
            {true}
        ; {memberchk(errors(ErrorMode), Options)}->
            ( {ErrorMode == ansi} ->
                sql_emit_token('~A', [[foreground-red]], machinery, Options)
            ; {ErrorMode == html} ->
                {format_sql_error(Errors, Index, Atom)},
                ( {Index == {null}} ->
                    {format(atom(Token), '<span class="error" title="~w">', [Atom])},
                    sql_append_raw_token(Token)
                ; {otherwise}->
                    {format(atom(Token), '<span class="error error_~w" data-index="error_~w" title="~w" onMouseOver="mouseOver(event)" onMouseOut="mouseOut(event)" onClick="mouseClick(event)">', [Index, Index, Atom])},
                    sql_append_raw_token(Token)
                )
            ; {otherwise}->
                {true}
            )
        ; {otherwise}->
            {true}
        ),
        sql_write_comments_1(Comments, Indent, Options).

sql_write_comments_1([], _Indent, _Options)--> [].
sql_write_comments_1([Comment|Comments], Indent, Options)-->
        sql_write_comment(Comment, Indent, Options),
        sql_write_comments_1(Comments, Indent, Options).

format_sql_error(type_mismatch(I, A, B), I, Atom):-
        !,
        format(atom(Atom), 'Type mismatch between ~w and ~w', [A, B]).

format_sql_error(order_by(top_level), {null}, 'ORDER BY is meaningless in the top level expression'):- !.
format_sql_error(coalesce(null_argument), {null}, 'NULL as an argument to COALESCE() is meaningless'):- !.
format_sql_error(order(having, order_by), {null}, 'HAVING clause should follow ORDER BY clause'):- !.
format_sql_error(sql_escape, {null}, 'Escape from SQL with { fn ... }'):- !.
format_sql_error(superfluous_quote(X), {null}, Message):- !, format(atom(Message), '~w does not require quoting here. It is quoted in the original source', [X]).
format_sql_error(percent, {null}, 'PERCENT clause used, but has no effect in SQL2005 and greater').
format_sql_error(for_clause, {null}, 'FOR clause?'):- !.
format_sql_error(deprecated(D, R), {null}, Message):- !, format(atom(Message), 'Deprecated function ~w: Use ~w instead', [D, R]).
format_sql_error(null_value, {null}, 'NULL is not actually allowed here. Use CAST(NULL AS <some type>)'):- !.

format_sql_error(A, {null}, Atom):-
        format(atom(Atom), 'Unknown error: ~q', [A]).

sql_write_comment(comment(long, Codes), _Indent, Options)--> !,
        sql_emit_token('/* ~s */ ', [Codes], comment, Options).

sql_write_comment(comment(short, Codes), Indent, Options)--> !,
        sql_emit_token('-- ~s~n~w', [Codes, Indent], comment, Options).

sql_end_comments([], _Indent, _Options)--> !.
sql_end_comments([Comment|Comments], Indent, Options)-->
        sql_end_comment(Comment, Indent, Options),
        sql_end_comments(Comments, Indent, Options).

sql_end_comment(meta(_, Errors), _Indent, Options)--> !,
        ( {Errors == {null}} ->
            {true}
        ; {memberchk(errors(ErrorMode), Options)}->
            ( {ErrorMode == ansi} ->
                {format(atom(Code), '~A', [{reset}])},
                sql_append_raw_token(Code)
            ; {ErrorMode == html} ->
                sql_append_raw_token('</span>')
            ; {otherwise}->
                {true}
            )
        ; {otherwise}->
            {true}
        ).


sql_write_type(Comments:Type, Indent, Options)--> !,
        sql_write_comments(Comments, Indent, Options),
        sql_write_type(Type, Indent, Options),
        sql_end_comment(Comments, Indent, Options).

sql_write_type(varchar(L), Indent, Options)--> !,
        ( {L == {unknown}} ->
            sql_emit_token('VARCHAR', [], keyword, Options)
        ; {otherwise}->
            sql_emit_token('VARCHAR', [], keyword, Options),
            sql_emit_token('(', [], punctuation, Options),
            sql_write_term(L, Indent, Options),
            sql_emit_token(')', [], punctuation, Options)
        ).

sql_write_type(int, _Indent, Options)--> !,
        sql_emit_token('INTEGER', [], keyword, Options).
sql_write_type(smallint, _Indent, Options)--> !,
        sql_emit_token('SMALLINT', [], keyword, Options).
sql_write_type(tinyint, _Indent, Options)--> !,
        ( {memberchk(dbms('PostgreSQL'), Options)}->
            % 'PostgreSQL' does not have a TINYINT (which is 1 byte). Use SMALLINT (2 bytes) instead
            sql_emit_token('SMALLINT', [], keyword, Options)
        ; {otherwise}->
            sql_emit_token('TINYINT', [], keyword, Options)
        ).

sql_write_type(decimal(Precision, Scale), Indent, Options)--> !,
        sql_emit_token('DECIMAL', [], keyword, Options),
        ( {Precision == {no_precision}} ->
            {true}
        ; {otherwise}->
          sql_emit_token('(', [], punctuation, Options),
          sql_write_term(Precision, Indent, Options),
          ( {Scale == {no_scale}} ->
              {true}
          ; {otherwise}->
              sql_emit_token(',', [], comma, Options),
              sql_write_term(Scale, Indent, Options)
          ),
          sql_emit_token(')', [], punctuation, Options)
        ).

sql_write_type(float(Precision), Indent, Options)--> !,
        ( {Precision == {no_precision}}->
            sql_emit_token('FLOAT', [], keyword, Options)
        ; {otherwise}->
            sql_emit_token('FLOAT', [], keyword, Options),
            sql_emit_token('(', [], punctuation, Options),
            sql_write_term(Precision, Indent, Options),
            sql_emit_token(')', [], punctuation, Options)
        ).

sql_write_type(real, _Indent, Options)--> !,
        sql_emit_token('REAL', [], keyword, Options).

sql_write_type(double(Precision), Indent, Options)--> !,
        sql_emit_token('DOUBLE', [], keyword, Options),
        sql_emit_token('(', [], punctuation, Options),
        sql_write_term(Precision, Indent, Options),
        sql_emit_token(')', [], punctuation, Options).

sql_write_type(datetime, _Indent, Options)--> !, % Should normalize
        ( {memberchk(dbms('PostgreSQL'), Options)}->
            sql_emit_token('TIMESTAMP', [], keyword, Options)
        ; {otherwise}->
            sql_emit_token('DATETIME', [], keyword, Options)
        ).

sql_write_type(date, _Indent, Options)--> !, % Should normalize
        sql_emit_token('DATE', [], keyword, Options).


sql_list_length(_:X, Y):- !, sql_list_length(X, Y).
sql_list_length([], 0):- !.
sql_list_length([_A|B], N):-
        sql_list_length(B, NN),
        N is NN+1.

normalize_date_type(day, day).
normalize_date_type(dd, day).
normalize_date_type(wk, week).
normalize_date_type(week, week).
normalize_date_type(second, second).
normalize_date_type(weekday, day_of_week).
normalize_date_type(year, year).
normalize_date_type(month, month).

sql_write_literal(Value, Options)-->
        {atom_codes(Value, Codes),
         sql_quote_codes(QuotedCodes, Codes, [])},
        sql_emit_token('~s', [QuotedCodes], literal, Options).


sql_quote_codes([], [], []):- !.
sql_quote_codes([39, 39|Codes])-->
        [39], !,
        sql_quote_codes(Codes).
sql_quote_codes([Code|Codes])-->
        [Code],
        sql_quote_codes(Codes).

% Quirk. SQL Server allows implicit cast of 0 to a datetime to get 1/1/1901.
sql_write_date(X, Indent, Options)-->
        sql_write_and_strip_comments(X, Indent, Options, Date, Comments),
        ( {Date == 0}->
            sql_emit_token('CAST', [], function, Options),
            sql_emit_token('(', [], punctuation, Options),
            sql_emit_token('\'Jan 1 1901\' ', [], literal, Options),
            sql_emit_token('AS timestamp without time zone', [], keyword, Options),
            sql_emit_token(')', [], punctuation, Options)
        ; {otherwise}->
            sql_write_term(Date, Indent, Options)
        ),
        sql_end_comments(Comments, Indent, Options).

should_suppress_collation(X):-
        strip_sql_comments(X, predicate(comparison(_, Lhs, Rhs))),
        ( Lhs = element(collated_factor(_, _))->
            true
        ; Rhs = element(collated_factor(_, _))->
            true
        ).

should_suppress_condition(X):-
        strip_sql_comments(X, predicate(comparison(_, element(1), element(1)))), !.
