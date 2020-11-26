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

:-module(sql_parser, [sql_gripe_level/1,
                      sql_parse/4,
                      strip_sql_comments/2]).

/** <module> SQL Parser

This module contains an SQL parser

---+ sql_parse/4
Parsing is invoked with sql_parse(+Term, -TrailingComments, +Options, +Tokens). Notice that all terms are bound when the predicate is called: you must direct the parser where to start. For a view definition, an example invocation might be
sql_tokens(Tokens, "CREATE VIEW foo AS SELECT bar FROM qux", []), sql_parse(view_definition(Definition, Types), TrailingComments, [], Tokens).
---++ Comments
Because comments can appear literally anywhere in the input text, every parse node has both a syntax element (such as view_definition/2) and a list of comments which preceed the element. This means comments are pushed as far as possible down the syntax tree. Any transformations of the input with the intention that it should be printed out again need to take the comments into account. Any other uses of the parse tree may pass it to strip_sql_comments(+InTree, -OutTree) to simply remove them all, leaving the tree with just the syntactic elements.

Finally, there may be trailing comments at the end of the input which are not followed by any token. This means they're not absorbed into the parse tree - so that they're not lost, they are returned as a list from sql_parse/4.
---++ Options
Current options include:
   * dbms(+DBMS): If omitted, DBMS will be set to 'Microsoft SQL Server'. For the most part only 'Microsoft SQL Server' views are parseable, but it would not be difficult to extend the parser if this was ever required (just search the source for dbms('Microsoft SQL Server') conditionals)
   * view_name(+Name): Passed to gripe/6 so that complaints about view syntax can be associated with a view name
Internally used options include (these should not be passed in under normal circumstances)
   * query_id(-Qid): Used to logically separate distinct parts of the query
   * subquery: Used to flag whether currently in the top-level query or a subquery
---++ Parse tree
The parse tree returned can be very complicated. The best documentation for this is probably either the sql_write or the sql_check module, which take the tree as an input and do processing on it.
---++ Type inference
Type inference makes the parser take almost 4 times longer, but the resulting information is very useful. It is rarely possible to tell as the input is read what the type of each element is. Where possible, the types are defined (for example, the type of count(*) is always native_type(int)) but where the type is unknown, a new variable is created and a constraint is made.

Type inference is done with CHR, and types are in one of three states:
   1 Known, and bound (ie committed)
   2 Unknown with one unresolved dependency
   3 Unknown with two unresolved dependencies

A dependency here refers to something which would influence the eventual type.
Some examples of the slightly more complicated case 2:
   * The type of a column selected from a table that we have not yet resolved
      * Consider SELECT foo FROM bar: Until we read the FROM clause we cannot begin to guess what the type of foo is, even though it has only one dependency
   * The type of a scalar subquery
      * SELECT (SELECT TOP 1 foo FROM bar) AS q: In this case, the type of q is not actually a subquery, it is the single element that subquery returns. A constraint-handling rule checks for a type constraint of type scalar(_) and replaces it with the single element it contains.
   * The type of an set operation (ie aggregation)
      * SELECT SUM(foo) AS q FROM bar: The type of q may be coerced to a decimal, depending on the eventual type of foo, which is not known until we have read the FROM clause

Some examples of case 3:
   * The type of a column which is the union of two selects
   * The type of an arithmetic expression

---+ Internal Details
---++ Syntax of the grammar
The grammar started out as an EBNF format, and is based roughly on http://savage.net.au/SQL/sql-92.bnf.html
{}/1 are escaped Prolog, like in a DCG
[...] denote optional clauses
| denotes options
@foo matches the token foo (case-insensitive matching is employed)
#Foo matches the next token with Foo
#Foo:Type matches the next token with Foo if it is a literal of type Type

---++ Left factoring types
SQL Server has some very complicated rules for inferring the type of decimal arithmetic (see http://msdn.microsoft.com/en-us/library/ms190476). The crucial, yet sadly missing information from that page deals with overflows. This is half-explained at http://blogs.msdn.com/b/sqlprogrammability/archive/2006/03/29/564110.aspx.

Because we have truncation, the order of operations is crucial: Although (x * y) / z is mathematically equivalent to x * (y / z), the types of the two expressions in SQL Server are actually different due to truncation. The parser is LL, but this means we will always read x * y / z as x * (y / z), whereas SQL Server does the type inference in reverse. This is only a problem for division and multiplication since the handling of addition and subtraction are symmetric, but without a transformation, we will compute the wrong type. After a term/2 is parsed, left_factor_types/3 is called, which translates just the types in the term from LL into LR form.

---+ Uses
   * Automatic determination of the view interface from the view SQL. These are prone to bit rot:
      * Someone changes the SQL but forgets to change the interface
      * Someone changes the underlying type of a table or view which is directly or indirectly referenced by the view. It's a burden to find all views which might reference the table which has been changes
   * Determination of view hierarchies
      * This can be used to determine the order in which views should be refreshed or replaced/installed from metadata
      * Can also be used with the load dependency analysis
   * Determination of indices
      * Parsing the view allows us to build a set of disjunctions used in WHERE clauses
   * Sanity checking
      * Some of the views contain some very, very weird things. Currently there is no oversight or review. If gripes are enabled at compile-time, the quality of code in views can be brought up to a higher standard
      * Some views may contain extremely inefficient logical structures. If the parse-tree can be suitably analysed, more efficient equivalent queries can be automatically generated. In any case, uses of patterns which are known to be inefficient can be reported at compile-time
    * DBMS indepdence
       * Transforming the views is relatively simple once we have the tree isolated. This can allow us to customize views to take advantage of features in later version of SQL Server while not alienating clients using older versions
       * If necessary, we can translate views to run on other DBMS, such as Oracle, 'PostgreSQL' or MySQL

---+ Known problems
% It is not practical to determine what + means ahead of time if the source view is MS T-SQL. We would have to guess and backtrack if wrong, and that is horribly inefficient. Instead if we read + in 'Microsoft SQL Server' mode, we should delay determining whether it is really + or actually || until the types of the LHS and RHS are resolved.
*/


:-op(95, fx, @).
:-op(100, fx, #).
% Disable some operators defined elsewhere!
:-op(0, xfx, as).
:-op(0, xfx, on).
:-op(0, xfx, or).
:-op(0, xfx, and).
:-op(0, xfx, not).
:-op(0, xfx, in).
:-op(0, xfx, like).
:-op(100, fx, ??).
:-op(1200, xfx, --->).

:-use_module(library(chr)).
:-use_module(library(quintus), [otherwise/0]).
:-use_module(library(dcg/basics)).
:-use_module(library(cql/sql_keywords)).
:-use_module(library(cql/sql_write)).
:-use_module(library(cql/sql_tokenizer)).

:-use_module(library(cql/cql), [default_schema/1,
                                cql_normalize_name/3,
                                dbms/2,
                                database_attribute/8,
                                domain_database_data_type/2,
                                routine_return_type/3,
                                sql_gripe/3]).

:-chr_option(line_numbers, on).
:-chr_option(check_guard_bindings, error).
:-chr_option(debug, off).
:-chr_option(optimize, full).
:-chr_option(guard_simplification, off). % Added to stop trail overflowing

:-chr_type 'Table' == any.
:-chr_type 'Alias' == any.
:-chr_type 'Type' == any.
:-chr_type 'Column' == any.
:-chr_type 'Constraint' == any.
:-chr_type 'Vars' == any.
:-chr_type 'N' == any.
:-chr_type 'QueryId' == any.
:-chr_type 'Source' == any.

:-chr_constraint type_constraint(-'QueryId', ?'Source', ?'Type', ?'Constraint').
:-chr_constraint type_constraint_ready(-'QueryId', ?'Type').
:-chr_constraint type_merge_hint(?'Type', ?'Constraint').
:-chr_constraint query_table(-'QueryId', ?'Alias', ?'Table').
:-chr_constraint derived_query_column(-'QueryId', ?'Alias', ?'Column', ?'Type').
:-chr_constraint subquery(-'QueryId', -'QueryId').
:-chr_constraint peer_query(-'QueryId', -'QueryId').
:-chr_constraint query_is_xml(-'QueryId').
:-chr_constraint resolve_types(-'QueryId').
:-chr_constraint commit(-'QueryId').
:-chr_constraint union_type(-'QueryId', ?'Constraint', ?'Constraint', ?'Constraint').
:-chr_constraint derived_table(-'QueryId', +'Table', ?'Constraint').
:-chr_constraint find_all_column_types(-'QueryId', ?'Source', ?'Type').
:-chr_constraint force_type_not_domain(?'Type').
:-chr_constraint frozen_reverse(-'QueryId', ?'Constraint', ?'Constraint').
:-chr_constraint cleanup(-'QueryId').

:-dynamic(cached_gripe_level/1).
sql_gripe_level(N):-
        cached_gripe_level(N), !.
sql_gripe_level(N):-
        ( getenv('SQL_GRIPE_LEVEL', Atom),
          atom_number(Atom, N)->
            true
        ; otherwise->
            N = 0 % FIXME: Should be 1, Testing only
        ),
        assert(cached_gripe_level(N)).


stream_to_tokens(Stream, Tokens):-
        stream_to_lazy_list(Stream, List),
        sql_tokens(Tokens, List, []), !.


sql_parse(Head, TrailingComments, Options, Tokens):-
        Head =.. [Functor, Arg, Types|Args],
        reverse(Tokens, TR),
        trailing_comments_reversed(TR, TrailingCommentsReversed, Tail),
        reverse(Tail, TokensWithoutTrailingComments),
        reverse(TrailingCommentsReversed, TrailingComments),
        Goal =.. [Functor, TokensWithoutTrailingComments, [], [query_id(QueryId)|Options], _, 0, _, Arg, T1 |Args],
        Goal,
        !,
        %chr_show_store(sql_parser),
        %format(user_error, '---------------~n', []),
        resolve_types(QueryId),
        map_nulls_to_ints(T1, Types),
        consolidate_errors(Arg),
        cleanup(QueryId),
        true.


map_nulls_to_ints([], []):- !.
map_nulls_to_ints([merged(A, _, N)|As], [A-native_type(int)|Bs]):-
        N == {nulltype}, !,
        map_nulls_to_ints(As, Bs).
map_nulls_to_ints([merged(A, _, N)|As], [A-native_type(int)|Bs]):-
        nonvar(N), N = native_type(int(_)), !,
        map_nulls_to_ints(As, Bs).
map_nulls_to_ints([merged(A, _, AT)|As], [A-AT|Bs]):-
        map_nulls_to_ints(As, Bs).

trailing_comments_reversed([], [], []):- !.
trailing_comments_reversed([comment(A,B)|In], [comment(A,B)|More], Tail):- !,
        trailing_comments_reversed(In, More, Tail).
trailing_comments_reversed(Tail, [], Tail):- !.

term_expansion(OldHead ---> OldBody, NewHead :- NewBody):-
        OldHead =.. [Functor, A1|Args],
        NewHead =.. [Functor, In, Out, ContextIn, _, P0, P1, meta(Comments, Source):A1|Args],
        transform_body(OldBody, In, Out, Source, ContextIn, _, Comments, [], P0, P1, NewBody).

transform_body({A}, In, Out, _, C, C, C1, C2, P0, P0, (A, Out = In, C1 = C2)):- !.
transform_body(\+(X), In, In, Source, C, C, C1, C1, P0, P0, \+G):-
        transform_body(X, In, _, Source, C, _, C1, _, P0, _, G).
transform_body(??(X), In, Out, Source, CIn, COut, C1, C2, P0, P1, Transformed):-
        transform_body(X, In, Out, Source, CIn, COut, C1, C2, P0, P1, G),
        functor(X, Functor, Arity),
        Transformed = (( length(First, 20), append(First, _, In)-> true ; otherwise-> First = In),
                         format(user_error, '----- CALL ~w (~q) ~w~n', [Functor/Arity, G, First]),
                       setup_call_catcher_cleanup(true,
                                                  G,
                                                  Reason,
                                                  ( Reason == ! ->
                                                      ( length(Last, 20), append(Last, _, Out)-> true ; otherwise-> Last = Out),
                                                      format(user_error, '----- CUT ~w ~w~n', [Functor/Arity, Last])
                                                  ; Reason == fail->
                                                      format(user_error, '----- FAIL ~w~n', [Functor/Arity])
                                                  ; Reason == exit->
                                                      ( length(Last, 20), append(Last, _, Out)-> true ; otherwise-> Last = Out),
                                                      format(user_error, '----- EXIT ~w ~w~n', [Functor/Arity, Last])
                                                  )),
                         ( var(Reason) ->
                         ( length(Last, 20), append(Last, _, Out)-> true ; otherwise-> Last = Out),
                             format(user_error, '----- PEND ~w ~w~n', [Functor/Arity, Last])
                         ; otherwise->
                             true
                         )
                      ).

transform_body(get_source(Source), In, In, Source, C, C, C1, C1, P0, P0, true):- !.
transform_body(get_parameter(P0), In, In, _, C, C, C1, C1, P0, P1, P1 is P0 + 1):- !.
transform_body(!, InOut, InOut, _, C, C, C1, C2, P0, P0, (!, C1 = C2)):- !.
transform_body(@Functor, In, Out, Source, C, C, C1, C2, P0, P0, get_token(Token, C, C1, C2, In, Out)):- Functor =.. [Token, Source], !.
transform_body(@Token, In, Out, _, C, C, C1, C2, P0, P0, get_token(Token, C, C1, C2, In, Out)):- !.
transform_body(#Identifier : Type, In, Out, _, C, C, C1, C2, P0, P0, get_identifier(Identifier, Type, C, C1, C2, In, Out)):-!.
transform_body(#Identifier, In, Out, _, C, C, C1, C2, P0, P0, get_identifier(Identifier, any, C, C1, C2, In, Out)):-!.

transform_body((A | B), In, Out, Source, CIn, COut, C1, C2, P0, P1, (C, COut = COut1, P1 = P1a ; D, COut = COut2, P1 = P1b)):-
        !,
        transform_body(A, In, Out, Source, CIn, COut1, C1, C2, P0, P1a, C),
        transform_body(B, In, Out, Source, CIn, COut2, C1, C2, P0, P1b, D).
transform_body((A,B), In, Out, Source, CIn, COut, C1, C2, P0, P2, (C,D)):-
        !,
        transform_body(A, In, Intermediate, Source, CIn, CInt, C1, C1b, P0, P1, C),
        transform_body(B, Intermediate, Out, Source, CInt, COut, C1b, C2, P1, P2, D).

transform_body(List, In, Out, Source, CIn, COut, C1, C2, P0, P1Out, (Goals, P1Out = P1 ; In = Out, CIn = COut, C1 = C2, P1Out = P0)):-
        is_list(List), !,
        transform_list_body(List, In, Out, Source, CIn, COut, C1, C2, P0, P1, Goals).
transform_body(Rule, In, Out, _, CIn, CX, C1, C2, P0, P1, ( C1 = C2, NewRule )):-
        Rule =.. [Functor|Args],
        NewRule =.. [Functor, In, Out, CIn, COut, P0, P1|Args],
        ( ( Functor == set_qid ; Functor == add_option)->
            CX = COut
        ; otherwise->
            CX = CIn
        ).
transform_list_body([Tail], In, Out, Source, CIn, COut, C1, C2, P0, P1, Goals):-
        transform_body(Tail, In, Out, Source, CIn, COut, C1, C2, P0, P1, Goals).
transform_list_body([Head|Tail], In, Out, Source, CIn, COut, C1, C2, P0, P2, (G, G2)):-
        transform_body(Head, In, Intermediate, Source, CIn, CInt, C1, C1b, P0, P1, G),
        transform_list_body(Tail, Intermediate, Out, Source, CInt, COut, C1b, C2, P1, P2, G2).

get_token(Token, Options, [comment(A,B)|C1], C2, [comment(A,B)|X], Out):- !,
        get_token(Token, Options, C1, C2, X, Out).
get_token(Token, Options, C1, C1, [TopToken|Out], Out):-
        ( reverse_lex(TopToken, Options, Token)->
            true
        ; atom(TopToken),
          downcase_atom(TopToken, Token)->
            true
        ).

get_identifier(Identifier, Type, Options, [comment(A,B)|C1], C2, [comment(A,B)|X], Out):- !,
        get_identifier(Identifier, Type, Options, C1, C2, X, Out).
get_identifier(Identifier, Type, Options, C1, C1, [Top|Out], Out):-
        ( Type == any ->
            Identifier = Top
        ; otherwise->
            Top = literal(Identifier, Type)
        ),
        ( atom(Identifier)->
            downcase_atom(Identifier, IdentifierLC),
            \+reserved_sql_keyword(IdentifierLC)
        ; otherwise->
            true
        ),
        \+reverse_lex(Identifier, Options,  _).

reverse_lex('*', _Options, asterisk).
reverse_lex('/', _Options, solidus).
reverse_lex('+', _Options, plus_sign).
reverse_lex('-', _Options, minus_sign).
reverse_lex(',', _Options, comma).
reverse_lex('.', _Options, period).
reverse_lex('(', _Options, left_paren).
reverse_lex(')', _Options, right_paren).
reverse_lex('{', _Options, left_curly).
reverse_lex('}', _Options, right_curly).
reverse_lex('IS', _Options, is_keyword).
reverse_lex('Is', _Options, is_keyword).
reverse_lex('iS', _Options, is_keyword).
reverse_lex('is', _Options, is_keyword).
reverse_lex('<', _Options, less_than_operator).
reverse_lex('=', _Options, equals_operator).
reverse_lex('<>', _Options, not_equals_operator).
reverse_lex('>', _Options, greater_than_operator).
reverse_lex('<=', _Options, less_than_or_equals_operator).
reverse_lex('>=', _Options, greater_than_or_equals_operator).
reverse_lex('+', Options, concatenation_operator):- dbms([], [], Options, _, _, _, 'Microsoft SQL Server').
reverse_lex('||', Options, concatenation_operator):- \+dbms([], [], Options, _, _, _, 'Microsoft SQL Server').


add_option(L, L, O, [X|O], P, P, X).
set_qid(L, L, O, O2, P, P, X):-
        change_qid(O, X, O2).

change_qid([query_id(_)|T], Qid, [query_id(Qid)|T]):- !.
change_qid([A|T], Qid, [A|T2]):- !, change_qid(T, Qid, T2).

get_option(L, L, O, O, P, P, X):- memberchk(X, O).
qid(L, L, O, O, P, P, Qid):- memberchk(query_id(Qid), O).
default_precision_and_scale(L, L, O, O, P0, P0, P, S):-
        ( dbms(L, L, O, O, P0, P0, 'Microsoft SQL Server')->
            P = 18,
            S = 0
        ; otherwise->
            throw(default_unknown)
        ).
dbms(L, L, Options, Options, P, P, DBMS):-
        ( memberchk(dbms(X), Options)->
            DBMS = X
        ; otherwise->
            DBMS = 'Microsoft SQL Server'
        ).

check_order_by_is_in_top_query(L, L, Options, Options, P, P, Source):-
        ( memberchk(subquery, Options)->
            true
        ; \+memberchk(view_name(_), Options)->
            % ORDER BY is fine in the top level of an actual query, of course!
            true
        ; otherwise->
            semantic_error(Source, order_by(top_level), 1)
        ).

action(Action, Types)--->
        query_expression(Action, Types) | delete_statement_searched(Action, Types) | insert_statement(Action, Types) | update_statement_searched(Action, Types).
delete_statement_searched(delete(TableName, Where), [])--->
        @delete, @from, !, table_name(TableName), (@where, search_condition(Condition), {Where = where(Condition)} | {Where = {no_where}}).
insert_statement(insert(TableName, Values), [])--->
        @insert, @into, !, table_name(TableName), insert_columns_and_source(Values), [dbms('PostgreSQL'), @returning, query_expression(_,_)].
insert_columns_and_source(Values)--->
        from_subquery(Values) | from_constructor(Values) | from_default(Values).
from_default({default_values})---> @default, @values, !.
from_subquery(insert_source(Source, Override, Target))--->
        ( ( @left_paren, insert_column_list(Source), @right_paren) | {Source = {default}} ),
        ( override_clause(Override) | {Override = {no_override}} ),
        query_expression(Target, _).
from_constructor(insert_source(Source, Override, Target))--->
        ( ( @left_paren, insert_column_list(Source), @right_paren) | {Source = {default}} ),
        ( override_clause(Override) | {Override = {no_override}} ),
        table_value_constructor(Target, _).
update_statement_searched(update(TableName, List, From, Condition), [])--->
        % Actually should be table_name here. Apparently it is not legal to use an alias?
        @update, table_reference(TableName),
        qid(Qid), {strip_sql_comments(TableName, Stripped), determine_tables(Qid, Stripped)},
        @set, set_clause_list(List),
        ( @from, from_clause_1(F), {strip_sql_comments(F, CleanedFrom), ( determine_tables(Qid, CleanedFrom)-> From = from(F) ; otherwise->throw(failed_tables(CleanedFrom)))}
        | {From = {no_from}}),
        ( @where, search_condition(Where), {Condition = where(Where)} | {Condition = {no_where}}).
set_clause_list([Head|Tail])--->
        set_clause(Head), (@comma, set_clause_list(Tail) | {Tail = []}).
set_clause(set(Target, Source))--->
        update_target(Target), @equals_operator, update_source(Source). % Or mutated-set-clause, but we probably dont need to worry about that
update_target(Target)---> column_name(Target). % Actually also allows foo[expression]
update_source(Source)---> value_expression(Source, _) | default_specification(Source, _) | null_specification(Source, _). % Also allows ARRAY[]
insert_column_list(List)---> column_name_list(List).
override_clause(overriding_user_value)---> @overriding, @user, @value, !.
override_clause(overriding_system_value)---> @overriding, @system, @value, !.


view_definition(view_definition(Name, Columns, Expression, With), Types)---> (@create), @view, table_name(Name),
        {
         strip_sql_comments(Name, NameNoComments),
         ( NameNoComments = table(identifier(schema(_, literal(dbo, identifier)), _))->
             true
         ; otherwise->
             throw(illegal_view_name(no_schema))
         )},
        ( @left_paren, view_column_list(Columns), @right_paren | {Columns = {all}}), with_attribute(With), @as, query_expression(Expression, Types).
table_name(table(Name))---> qualified_name(Name).
view_column_list(List)---> column_name_list(List).
query_expression(Term, T)--->
        qid(Qid),
        non_join_query_term(LHS, LT),
        ( @union, (@all, {Term = union_all(LHS, RHS, Corresponding)} | {Term = union(LHS, RHS, Corresponding)}), (corresponding_spec(Corresponding) | {Corresponding = {no_corresponding}}), set_qid(SubQid), query_expression(RHS, RT), {peer_query(Qid, SubQid), union_type(Qid, LT, RT, T)}
        | @except, (@all, {Term = except_all(LHS, RHS, Corresponding)} | {Term = except(LHS, RHS, Corresponding)}), (corresponding_spec(Corresponding) | {Corresponding = {no_corresponding}}), query_expression(RHS, RT), {union_type(Qid, LT, RT, T)}
        | {Term = LHS, T = LT}).
non_join_query_term(Term, T)---> (non_join_query_primary(LHS, LT) | free_joined_table(LHS, LT)),
        ( @intersect, ( @all, {Term = intersect_all(LHS, RHS, Corresponding)} | {Term = intersect(LHS, RHS, Corresponding)}),
          ( corresponding_spec(Corresponding) | {Corresponding = {no_corresponding}}), non_join_query_term(RHS, RT), qid(Qid), {union_type(Qid, LT, RT, T)} | {Term = LHS, T = LT}).
non_join_query_primary(Primary, T)---> (simple_table(Primary, T) | @left_paren, query_expression(Primary, T), @right_paren).
simple_table(Table, T)---> query_specification(Query, T), {Table = query(Query)} | table_value_constructor(Values, T), {Table = values(Values)} | explicit_table(Explicit), {Table = explicit_table(Explicit), T = {fixme1}}.
query_specification(select(Q, Selections, Source, Limit, For), QueryType)--->
        @select, ( set_quantifier(Q) | {Q = {no_quantifier}} ), [ dbms('Microsoft SQL Server'), top_clause(Limit) ],
                select_list(Selections, Sources, Types), table_expression(Source), [dbms('PostgreSQL'), limit_clause(Limit)], {var(Limit)->Limit = {no_limit} ; true},
                ( dbms('Microsoft SQL Server'), for_clause(For), get_source(S1), {semantic_error(for_clause, S1, 2)} | {For = {no_for}}),
                {(strip_sql_comments(Selections, S), merge_types(S, Sources, Types, QueryType)-> true ; throw(failed_to_resolve))}.
select_list(N, S, T)---> (@asterisk, qid(Qid), get_source(Source), {N = all, find_all_column_types(Qid, Source, T1), frozen_reverse(Qid, T1, T)} | select_list_1(N, S, T)).
select_list_1([Head|Tail], [Source|Sources], [Type|Types])---> select_sublist(Head, Source, Type), (@comma, select_list_1(Tail, Sources, Types) | {Tail = [], Sources = [], Types = []}).
select_sublist(S, Source, Type)---> get_source(Source), derived_column(Column, Type), {S = Column} | qualifier(Qualifier), @period, @asterisk, {S = all(Qualifier), Type = {fixme3}}.
derived_column(derived_column(Column, As), Type)---> (illegal_null_specification(Column, Type) | value_expression(Column, Type)), (as_clause(As) | {As = {no_alias}}). % Added @null to allow for SELECT NULL AS foo, since null is not a value
as_clause(Name)---> [@as], column_name(Name).
table_expression(source(From, Where, GroupBy, OrderBy, Having))--->
        from_clause(From),
        qid(Qid),
        {((From = _:from(F))->
            strip_sql_comments(F, CleanedFrom),
            ( determine_tables(Qid, CleanedFrom)-> true ; otherwise->throw(failed_tables(CleanedFrom)))
         ; From = _:{no_from}->
            true
         )},
        ( where_clause(Where) | {Where = {no_where}}),
        ( group_by_clause(GroupBy) | {GroupBy = {no_groupby}}),
        % Some hacks here. table_expression is not supposed to have an order-by clause, but it is used in a lot of views
        % Further, some views put the order-by AFTER the having

        % To allow the order-by to refer to expressions in the view, we have to make the entire query become a sub-query of the order-by clause
        % However, confusingly, the HAVING clause needs to be part of the same query so that things like SUM(x) which resolve in the select list also resolve in the HAVING clause
        qid(Qid), set_qid(SubqueryId), {subquery(SubqueryId, Qid)},
        ( order_by_clause(OrderBy), set_qid(Qid), (having_clause(Having) | {Having = {no_having}})
        | set_qid(Qid), having_clause(Having), ( set_qid(SubqueryId), order_by_clause(OrderBy), get_source(Source), {semantic_error(order(having, order_by), Source, 2)} | {OrderBy = {no_orderby}})
        | {OrderBy = {no_orderby}, Having = {no_having}}),
        set_qid(Qid).
from_clause(from(From))---> @from, from_clause_1(From).
from_clause({no_from})---> {true}.
from_clause_1([Head|Tail])---> table_reference(Head), get_source(Source), (@comma, {semantic_error(Source, deprecated('SQL89-style join', 'Explicit JOIN clauses'), 1)}, from_clause_1(Tail) | {Tail = []}).

table_reference(Reference)---> (@left_paren, table_reference(LHS), @right_paren
                               | derived_table(Derivation, T), correlation_specification(Correlation), {LHS = derived_table(Derivation, Correlation, T)}
                               | table_name(Name), (correlation_specification(Correlation) | {Correlation = {no_correlation}}), {LHS = correlated_table(Name, Correlation)}),
        [ dbms('Microsoft SQL Server'), with_clause(_) ], % TBD: Preserve WITH
        ( more_join(RHS), {Reference = join(LHS, RHS)} | {Reference = LHS}).
more_join(X)--->
        ( cross_join_rhs(LHS), {Reference = cross_join(LHS)}
        | qualified_join_rhs(Type, LHS, On), {Reference = qualified_join(Type, LHS, On)}),
        ( more_join(RHS2), {X = join(Reference, RHS2)} | {X = Reference}).
correlation_specification(correlation(Name, Columns))---> [@as], #NameMC, (@left_paren, derived_column_list(Columns), @right_paren | {Columns = {no_columns}}), {name_from_identifier(NameMC, Name)}.
derived_column_list(L)--->column_name_list(L).
derived_table(Table, T)---> table_subquery(Table, T).
table_subquery(Query, T)---> subquery(Query, T).
cross_join_rhs(Reference)---> @cross, @join, table_reference(Reference).
qualified_join_rhs(Type, Reference, Spec)---> ( @natural, {Type = natural(T1)} | {Type = T1} ), ( join_type(T1) | {T1 = join} ), (@join), table_reference(Reference), ( join_specification(Spec) | {Spec = {no_on}} ).
free_joined_table(Table, {fixme4})---> table_reference(Table).
join_type(join_type(Type))---> (@inner, {Type = inner} | outer_join_type(T1), {Type = outer(T1)}, [ @outer ] | @union, {Type = union}).
outer_join_type(T)---> (@left, {T=left} | @right, {T=right} | @full, {T=full}).
join_specification(Spec)---> ( join_condition(Spec) | named_columns_join(Spec) ).
join_condition(on(On)) ---> @on, search_condition(On).
named_columns_join(columns(Columns))---> @using, @left_paren, join_column_list(Columns), @right_paren.
join_column_list(Columns)---> column_name_list(Columns).
set_quantifier(Q)---> ( @distinct, {Q = distinct} | @all, {Q = all} ).
where_clause(where(Where))---> @where, search_condition(Where).
corresponding_spec(Columns)---> @corresponding, ( @by, @left_paren, corresponding_column_list(Columns), @right_paren | {Columns = {no_columns}} ).
corresponding_column_list(List)---> column_name_list(List).
query_primary(Primary, T)---> ( non_join_query_primary(Primary, T) | free_joined_table(Primary, T) ).
subquery(subquery(Query), T)---> (@left_paren, add_option(subquery), qid(Qid), set_qid(SubqueryId), {subquery(Qid, SubqueryId)}, query_expression(Query, T), @right_paren).
column_name_list([Head|Tail])---> column_name(Head), (@comma, column_name_list(Tail) | {Tail = []}).
column_name(Name)---> #Identifier, \+(@left_paren),
        {( Identifier = literal(Name, identifier)->
             true
         ; Identifier = literal(A,B)-> % Quirks. This is for "SELECT '01' AS foo. The 'column' here is actually the literal 01
            Name = literal(A,B)
        ; otherwise->
             downcase_atom(Identifier, Name)
        )}.
explicit_table(table(Table)) ---> @(table), table_name(Table).
qualifier(Qualifier)---> qualified_name(Qualifier).
% If this were a numeric_value_expression followed by /plus/ (as distinct but indistiguishable from /concat/) then it
% would have been absorbed into the numeric_value_expression. The only time we would exist numeric_value_expression and consume a +
% is if we knew it was actually a concat!
value_expression(V, T)---> numeric_value_expression(V, T), \+(@concatenation_operator), \+(@minus_sign), \+(@period) % Hints we might be barking up the wrong parse tree
        | string_value_expression(V, T), \+(@concatenation_operator), \+(@minus_sign), \+(@period)
        | datetime_value_expression(V, T)
        | interval_value_expression(V, T).

% TBD: This should ALSO be applied for subtract, since CURRENT_TIMESTAMP - 1 is not subtract but add_interval(CURRENT_TIMESTAMP, -1)

numeric_value_expression(V, T)--->
        numeric_value_expression_1(V, T1),
        qid(Qid),
        {left_factor_types(Qid, T1, T)}.

numeric_value_expression_1(V, T)--->
        get_source(LS), term(LHS, LT), qid(Qid),
        ( ( @plus_sign, {Op = add} | @minus_sign, {Op = subtract} ),
          get_source(RS),
          numeric_value_expression_1(RHS, RT),
          {T = node(LT, LS, Op, RT, RS),
           % T1 is not the type of the subexpression (since this sub-expression may not even exist in the final result)
           % but it IS needed to determine whether the operation is +(addition) or +(concatenation) since SQL Server
           % doesnt distinguish these with syntax
           % Similarly R1 is not necessarily needed for the SQL, but it IS needed here
           left_factor_types(Qid, RT, R1),
           left_factor_types(Qid, LT, L1),
           most_general_type(Qid, Source, Source, L1, R1, Op, T1),
           freeze(T1, determine_operation_from_types(T1, L1, R1, Op, LHS, RHS, V))}
        | {V = LHS, T = LT}
        ).


%TBD: This currently does not always work. See for example le_vw_negative_holdings
determine_operation_from_types(Type, LT, RT, Op, LHS, RHS, V):-
        %format(user_error, '--------------------------------- add_or_concat: ~w, ~w, ~w~n', [Type, LT, RT]),
        native_type_of_type(LT, LTT),
        native_type_of_type(RT, RTT),
        ( Op == add ->
            ( native_type_of_type(Type, native_type(varchar(_)))->
                V = concatenate(LHS, RHS)
            ; native_type_of_type(Type, native_type(nvarchar(_)))->
                V = concatenate(LHS, RHS)
            ; LTT = native_type(datetime),
              RTT = native_type(int(_))->
                V = add_interval(LHS, RHS)
            ; RTT = native_type(datetime),
              LTT = native_type(int(_))->
                V = add_interval(RHS, LHS)
            % Believe it or not, there are places where we add numerics to dates as well. I will only implement
            % the cases that exist as a workaround
            ; LTT = native_type(datetime),
              RTT = native_type(decimal(_,_))->
                V = add_interval(LHS, RHS)
            ; otherwise->
                V = add(LHS, RHS)
            )
        ; Op == subtract ->
            ( LTT = native_type(datetime),
              RTT = native_type(int(_))->
                V = add_interval(LHS, negative(RHS))
            ; RTT = native_type(datetime),
              LTT = native_type(int(_))->
                V = add_interval(RHS, negative(LHS))
            % Believe it or not, there are places where we subtract numerics from dates as well. I will only implement
            % the cases that exist as a workaround
            ; LTT = native_type(datetime),
              RTT = native_type(decimal(_,_))->
                V = add_interval(LHS, negative(RHS))
            ; otherwise->
                V = subtract(LHS, RHS)
            )
        ).


native_type_of_type(native_type(X), native_type(X)):- !.
native_type_of_type(domain(D), native_type(X)):-
        fetch_domain_data_type(D, X).

term(V, T)---> get_source(LS), factor(LHS, LT), ((@asterisk, {V = multiply(LHS, RHS), Op = multiply} | @solidus, {V = divide(LHS, RHS), Op = divide(RT)}), get_source(RS), term(RHS, RT), {T = node(LT, LS, Op, RT, RS)} | {V = LHS, T = LT}).
factor(V, T)---> ( (@plus_sign, {V = positive(N)} | @minus_sign, dbms(DBMS), {V = negative(N), (DBMS == 'Microsoft SQL Server' -> force_type_not_domain(T) ; true)}) | { V = N} ), numeric_primary(N, T). % Quirk. This is in contradiction to the T-SQL Reference, but confirmed.
numeric_primary(N, T)---> value_expression_primary(N, T) | numeric_value_function(N, T).
value_expression_primary(N, T)--->
        unsigned_value_specification(N, T) | parameter(N, T) | column_reference(N, T) | set_function_specification(N, T) | case_expression(N, T) | @left_paren, value_expression(N, T), @right_paren | cast_specification(N, T) | scalar_subquery(N, T) | routine_invocation(N, T).
parameter(parameter(N), native_type(int)) ---> @(?), get_parameter(N). % FIXME: Type of parameter
column_reference(column(Qualifier, Name), Type)---> ( qualifier(Qualifier), @period | {Qualifier = {no_qualifier}} ), column_name(Name), qid(Qid), get_source(Source), {strip_sql_comments(Qualifier, QS), strip_sql_comments(Name, NS), type_constraint(Qid, Source, Type, typeof(QS, NS)), type_constraint_ready(Qid, Type)}.
scalar_subquery(S, T)---> subquery(S, ST), qid(Qid), get_source(Source), {type_constraint(Qid, Source, T, scalar(ST)), type_constraint_ready(Qid, T)}.
having_clause(having(Having))---> @having, search_condition(Having).
group_by_clause(group_by(List))---> @group, @by, grouping_column_reference_list(List).
grouping_column_reference_list([Head|Tail])---> grouping_column_reference(Head), ( @comma, grouping_column_reference_list(Tail) | {Tail = []} ).
grouping_column_reference(group_column(Reference, Collate))---> column_reference(Reference, _), ( collate_clause(Collate) | {Collate = {no_collation}} ).
collate_clause(collate(Name))---> @collate, collation_name(Name).
collation_name(collation(Name))---> qualified_name(Name).
qualified_name(identifier(Qualifier, Name))---> ( schema_name(Qualifier), @period | {Qualifier = {no_schema}}), #Identifier, {name_from_identifier(Identifier, Name)}. % Quirk
schema_name(schema(Catalog, Schema))---> ( catalog_name(Catalog), @period | {Catalog = {no_catalog}}), #Schema.
catalog_name(Catalog)---> #Catalog.
set_function_specification(S, T)---> (@count, @left_paren, @asterisk, @right_paren, {S = count(all), T = native_type(int)} | general_set_function(S, T)).
set_function_specification(S, T)---> (@count_big, @left_paren, @asterisk, @right_paren, {S = count(all), T = native_type(bigint)} | general_set_function(S, T)).

general_set_function(set_function(S, Q, A), T)---> set_function_type(S), @left_paren, ( set_quantifier(Q) | {Q = {no_quantifier}} ), value_expression(A, AT), @right_paren, qid(Qid), get_source(Source),
        {S = _:count ->
           T = native_type(int)
        ; S = _:sum->
           type_merge_hint(T, sum),
           type_constraint(Qid, Source, T, AT),
           type_constraint_ready(Qid, T)
        ; S = _:avg->
           type_merge_hint(T, avg),
           type_constraint(Qid, Source, T, AT),
           type_constraint_ready(Qid, T)
        ; otherwise->
           T = AT
        }.
set_function_type(T)---> @avg, {T = avg} | @max, {T = max} | @min, {T = min} | @sum, {T = sum} | @count, {T = count}.
search_condition(C, _Types)---> search_condition(C).
search_condition(C)---> boolean_term(Head), (@or, search_condition(Tail), {C = or(Head, Tail)} | {C = Head}).
boolean_term(C)---> boolean_factor(Head), (@and, boolean_term(Tail), {C = and(Head, Tail)} | {C = Head}).
boolean_factor(C)---> ( @not, {C = not(X)} | {C = X} ), boolean_test(X).
boolean_test(Test)---> boolean_primary(X), ( @is_keyword, ( @not, {Test = isnot(X, T)} | {Test = is(X, T)}), truth_value(T) | {Test = X}).
truth_value(T)---> (@true, {T = true} | @false, {T = false} | @unknown, {T = unknown}).
boolean_primary(P)---> @left_paren, search_condition(Search), {P = search(Search)}, @right_paren | predicate(Pr), {P = predicate(Pr)}.
predicate(Predicate)---> comparison_predicate(Predicate) | between_predicate(Predicate) | in_predicate(Predicate) | like_predicate(Predicate) | null_predicate(Predicate) | quantified_comparison_predicate(Predicate) | exists_predicate(Predicate) | match_predicate(Predicate) | overlaps_predicate(Predicate).
comparison_predicate(comparison(CompOp, LHS, RHS))---> row_value_constructor(LHS, LT), comp_op(CompOp), row_value_constructor(RHS, RT), {check_types(LT, RT)}.
row_value_constructor(Row, Types)---> @left_paren, row_value_constructor_list(List, Types), {Row = list(List)}, @right_paren | row_value_constructor_element(Element, Types), {Row = element(Element)} | row_subquery(SubQuery, Types), {Row = query(SubQuery, Types)}.
row_value_constructor_element(Element, Type)---> value_expression(Element, Type) | null_specification(Element, Type) | default_specification(Element, Type).
comp_op(Op)---> @equals_operator, {Op = '='}
        | @not_equals_operator, {Op = '<>'}
        | @less_than_operator, {Op = '<'}
        | @greater_than_operator, {Op = '>'}
        | @less_than_or_equals_operator, {Op = '<='}
        | @greater_than_or_equals_operator, {Op = '>='}.
null_specification({null}, T)---> @null, qid(Qid), get_source(Source), {type_constraint(Qid, Source, T, {nulltype}), type_constraint_ready(Qid, T)}.
illegal_null_specification({null}, T)---> @null, qid(Qid), get_source(Source), {semantic_error(Source, null_value, 1), type_constraint(Qid, Source, T, {nulltype}), type_constraint_ready(Qid, T)}.
null_predicate(Predicate)---> row_value_constructor(LHS, _), @is_keyword, ( @not, {Predicate = is_not_null(LHS)} | {Predicate = is_null(LHS)} ), @null. % The spec is wrong here
default_specification({default}, {defaulttype})---> @default.
row_subquery(S, T)---> subquery(S, T).
row_value_constructor_list([Head|Tail], [Type|Types])---> row_value_constructor_element(Head, Type), (@comma, row_value_constructor_list(Tail, Types) | {Tail = [], Types = []}) .
between_predicate(Term)---> row_value_constructor(LHS, TA), ( @not, {Term = not_between(LHS, Min, Max)} | {Term = between(LHS, Min, Max)}), @between, row_value_constructor(Min, TB), @and, row_value_constructor(Max, TC), {check_types(TA, TB), check_types(TA, TC)}.
exists_predicate(exists(Query))---> @exists, table_subquery(Query, _).
in_predicate(P) ---> row_value_constructor(Value, T1), ( @not, {P = not_in(Value, List)} | {P = in(Value, List)} ), @in, in_predicate_value(List, T2), {forall(member(T, T2), check_types(T1, T))}.
in_predicate_value(In, Types)---> @left_paren, in_value_list(List, Types), {In = list(List)}, @right_paren | table_subquery(Query, Types), {In = query(Query)}.
in_value_list([Head|Tail], [Type|Types])---> value_expression(Head, Type), ( @comma, in_value_list(Tail, Types) | {Tail = [], Types = []} ).
like_predicate(P)---> match_value(LHS), ( @not, {P = not_like(LHS, Pattern, Escape)} | {P = like(LHS, Pattern, Escape)}), (@like | @ilike), pattern(Pattern), ( @escape, escape_character(Escape) | {Escape = {no_escape}}).
match_value(P)---> character_value_expression(P, _).
pattern(P)---> character_value_expression(P, _).
escape_character(P)---> character_value_expression(P, _).
character_value_expression(E, T)---> get_source(S1), character_factor(LHS, LT), (@concatenation_operator, get_source(S2), character_value_expression(RHS, RT), qid(Qid), {E = concatenate(LHS, RHS), concatenate_type(Qid, S1, S2, LT, RT, T)} | {E = LHS, T = LT}).
character_factor(Factor, T)---> character_primary(F, T), ( collate_clause(C), {Factor = collated_factor(F, C)} | {Factor = F} ).
character_primary(X, T)---> value_expression_primary(X, T) | string_value_function(X, T).
match_predicate(match(Unique, MatchLevel, LHS, RHS))---> row_value_constructor(LHS, TL), @match, [ @unique, {Unique = unique} ], [ (@partial, {MatchLevel = partial} | @full, {MatchLevel = full}) ], table_subquery(RHS, TR), {check_types(TL, TR)}.
overlaps_predicate(overlaps(LHS, RHS))---> row_value_constructor(LHS, TL), @overlaps, row_value_constructor(RHS, TR), {check_types(TL, TR)}.
quantified_comparison_predicate(quantified_comparison(Op, Quantifier, LHS, RHS))---> row_value_constructor(LHS, TL), comp_op(Op), quantifier(Quantifier), table_subquery(RHS, TR), {check_types(TL, TR)}.
quantifier(Quantifier)---> @all, {Quantifier = all} | @some, {Quantifier = some}.
table_value_constructor(N, T)---> @values, table_value_constructor_list(N, T).
table_value_constructor_list([Head|Tail], [Type|Types])---> row_value_constructor(Head, Type), (@comma, table_value_constructor_list(Tail, Types) | {Tail = [], Types = []}).
case_expression(N, T)---> case_abbreviation(N, T) | case_specification(N, T).
case_abbreviation(V, T)---> @nullif, @left_paren, value_expression(LHS, LT), @comma, value_expression(RHS, RT), @right_paren, qid(Qid), get_source(Source), {V = nullif(LHS, RHS), most_general_type(Qid, Source, Source, LT, RT, case, T)}
        | @coalesce, @left_paren, coalesce_list(List, Types, Sources), @right_paren, qid(Qid), {V = coalesce(List), coalesce_type(Qid, Types, Sources, T)}.
coalesce_list([Head|Tail], [Type|Types], [Source|Sources])---> get_source(Source), (null_specification(Head, Type), get_source(Source), {semantic_error(Source, coalesce(null_argument), 1)} | value_expression(Head, Type)), ( @comma, coalesce_list(Tail, Types, Sources) | {Tail = [], Types = [], Sources = []} ).
case_specification(N, T)---> searched_case(N, T) | simple_case(N, T).
simple_case(simple_case(Operand, List, Else), T)---> @case, qid(Qid), case_operand(Operand), simple_when_clause_list(List, Types, Sources), ( get_source(Source), else_clause(Else, ElseType), {coalesce_type(Qid, [ElseType|Types], [Source|Sources], T)} | {Else = {no_else}, coalesce_type(Qid, Types, Sources, T)} ), @end.
simple_when_clause_list([Head|Tail], [Type|Types], [Source|Sources])---> get_source(Source), simple_when_clause(Head, Type), (simple_when_clause_list(Tail, Types, Sources) | {Tail = [], Types = [], Sources = []}).
case_operand(X)---> value_expression(X, _).
simple_when_clause(when(When, Result), T)---> @when, when_operand(When), @then, result(Result, T).
when_operand(X)---> value_expression(X, _).
result(X, T)---> null_specification(X, T) | result_expression(X, T).
result_expression(X, T)---> value_expression(X, T).
else_clause(else(Else), T)---> @else, result(Else, T).
searched_case(case(Cases, Else), T)---> @case, qid(Qid), searched_when_clause_list(Cases, Types, Sources), ( get_source(Source), else_clause(Else, ElseT), {coalesce_type(Qid, [ElseT|Types], [Source|Sources], T)} | {Else = {no_else}, /* No domain if no else clause? */ force_type_not_domain(T), coalesce_type(Qid, Types, Sources, T)} ), (@end).
searched_when_clause_list([Head|Tail], [Type|Types], [Source|Sources])---> get_source(Source), searched_when_clause(Head, Type), (searched_when_clause_list(Tail, Types, Sources) | {Tail = [], Types = [], Sources = []}).
searched_when_clause(when(searched(Search), Result), T)---> @when, search_condition(Search), @then, result(Result, T).
numeric_value_function(N, native_type(int))---> position_expression(N) | extract_expression(N) | length_expression(N).
position_expression(position(A, B))---> @position, @left_paren, character_value_expression(A, _), @in, character_value_expression(B, _), @right_paren.
extract_expression(extract(Field, Source))---> @extract, @left_paren, extract_field(Field), @from, extract_source(Source), @right_paren.
length_expression(A)---> char_length_expression(A) | octet_length_expression(A) | bit_length_expression(A).
char_length_expression(char_length(A))---> ( @char_length | @character_length ), @left_paren, string_value_expression(A, _), @right_paren.
string_value_expression(X, T)---> character_value_expression(X, T) | bit_value_expression(X, T).
octet_length_expression(octet_length(A))---> @octet_length, @left_paren, string_value_expression(A, _), @right_paren.
bit_length_expression(bit_length(A))---> @bit_length, @left_paren, string_value_expression(A, _), @right_paren.
extract_field(Field)---> datetime_field(Field) | time_zone_field(Field).
extract_source(V)---> datetime_value_expression(V, _) | interval_value_expression(V, _).
unsigned_value_specification(Value, native_type(int(X)))---> #Value : int(X).
routine_invocation(routine(Name, Args), Type)---> qualified_name(Name), @left_paren, dbms(DBMS), {not_a_builtin_function(DBMS, Name)}, (sql_argument_list(Args) | {Args = []}), @right_paren, {routine_type(Name, Type)}.
sql_argument_list([Head|Tail])---> sql_argument(Head), (@comma, sql_argument_list(Tail) | {Tail = []}).
sql_argument(Arg)---> value_expression(Arg, _).
cast_specification(cast(Operand, Target), Type)---> @cast, @left_paren, cast_operand(Operand), @as, cast_target(Target), {strip_sql_comments(Target, Type)}, @right_paren.
cast_operand(Operand)---> @null, {Operand = {null}} | value_expression(Operand, _).
cast_target(Target)---> data_type(Type), {Target = native_type(Type)} | domain_name(Domain), {Target = domain(Domain)}.
domain_name(Name)---> qualified_name(Name).
data_type(Type)---> character_string_type(Type), ( @character, @set, character_set_specification(_) | {true}) | national_character_string_type(Type) | bit_string_type(Type) | numeric_type(Type) | datetime_type(Type) | interval_type(Type).
character_string_type(varchar(Length))---> @character, ( @left_paren, length(Length), @right_paren  | {Length = 30})
	|	@char, ( @left_paren, length(Length), @right_paren | {Length = 30})
	|	@character, @varying, ( @left_paren, length(Length), @right_paren | {Length = 30})
	|	@char, @varying, ( @left_paren, length(Length), @right_paren |  {Length = 30})
	|	@varchar, ( @left_paren, length(Length), @right_paren |  {Length = 30}).
length(Length)---> #Length : int(_) | (@max, {Length = max}).
national_character_string_type(nchar_type(Length))--->
		@national, @character, [ @left_paren, length(Length), @right_paren ]
	|	@national, @char, [ @left_paren, length(Length), @right_paren ]
	|	@nchar, [ @left_paren, length(Length), @right_paren ]
	|	@national, @character, @varying, [ @left_paren, length(Length), @right_paren ]
	|	@national, @char, @varying, [ @left_paren, length(Length), @right_paren ]
	|	@nchar, @varying, [ @left_paren, length(Length), @right_paren ].
bit_string_type(bit_type(Length))---> @bit, [ @left_paren, length(Length), @right_paren ]  | @bit, @varying, [ @left_paren, length(Length), @right_paren ].
numeric_type(Type)---> exact_numeric_type(Type) | approximate_numeric_type(Type).
exact_numeric_type(Type)---> @numeric, {Type = decimal(Precision, Scale)}, ( @left_paren, precision(Precision), ( @comma, scale(Scale) | {Scale = {no_scale}}), @right_paren | default_precision_and_scale(Precision, Scale))
	|	@decimal, {Type = decimal(Precision, Scale)}, ( @left_paren, precision(Precision), ( @comma, scale(Scale) | {Scale = {no_scale}}), @right_paren | default_precision_and_scale(Precision, Scale) )
	|	@dec, {Type = decimal(Precision, Scale)}, ( @left_paren, precision(Precision), ( @comma, scale(Scale) | {Scale = {no_scale}}), @right_paren | default_precision_and_scale(Precision, Scale) )
	|	@integer, {Type = int}
	|	@int, {Type = int}
	|	@smallint, {Type = smallint}
        |	dbms('Microsoft SQL Server'), @tinyint, {Type = tinyint}.
precision(Precision)---> #Precision : int(_).
scale(Scale)---> #Scale : int(_).
approximate_numeric_type(Type)---> @float, ( @left_paren, precision(Precision), @right_paren | {Precision = {no_precision}}), {Type = float(Precision)} | @real, {Type = real} | @double, precision(Precision), {Type = double(Precision)}.
datetime_value_expression(V, T)---> (datetime_term(LHS, LT) | interval_value_expression(LHS, LT)),
        ( ( @plus_sign, {V = add(LHS, RHS), Op = add} | @minus_sign, {V = subtract(LHS, RHS), Op = subtract}), datetime_value_expression(RHS, RT), qid(Qid), get_source(Source), {most_general_type(Qid, Source, Source, LT, RT, Op, T)} | {V = LHS, T = LT}).
order_by_clause(order_by(List))---> @order, @by, get_source(Source), check_order_by_is_in_top_query(Source), sort_specification_list(List).
sort_specification_list([Head|Tail])---> sort_specification(Head), ( @comma, sort_specification_list(Tail) | {Tail = []}).
sort_specification(sort_key(Key, Collate, Order))---> sort_key(Key), ( collate_clause(Collate) | {Collate = {no_collation}} ), ( ordering_specification(Order) | {Order = {no_order}} ).
% According to SQL92, a sort_key is a column_reference. In SQL99, however, it is a value_expression, which is quite a bit easier.
%sort_key(Key)---> column_reference(C, _), {Key = sort_column(C)} | #I : int(_), {Key = index(I)}.
sort_key(Key)---> value_expression(C, _), {Key = sort_column(C)} | #I : int(_), {Key = index(I)}.
ordering_specification(S)---> @asc, {S = asc} | @desc, {S = desc}.

interval_term(V, T)---> interval_factor(LHS, LT), (((@asterisk, {V = multiply(LHS, RHS), Op = multiply} | @solidus, {V = divide(LHS, RHS), Op = divide(RT)}), interval_term(RHS, RT), qid(Qid), get_source(Source), {most_general_type(Qid, Source, Source, LT, RT, Op, T)}) | {V = LHS, T = LT})
        | term(LHS, LT), @asterisk, interval_factor(RHS, RT), qid(Qid), get_source(Source), {T = multiply(LHS, RHS), most_general_type(Qid, Source, Source, LT, RT, multiply, T)}.
interval_factor(F, T)---> (@plus_sign, {F = positive(F1)} | @minus_sign, {F = negative(F1)} | {F = F1}), interval_primary(F1, T).
interval_primary(interval(P, Q), T)---> value_expression_primary(P, T), ( interval_qualifier(Q) | {Q = {no_qualifier}} ).
interval_value_expression(V, T)---> interval_term(LHS, LT), (((@plus_sign, {V = add(LHS, RHS), Op = add} | @minus_sign, {V = subtract(LHS, RHS), Op = subtract}), interval_term(RHS, RT), qid(Qid), get_source(Source), {most_general_type(Qid, Source, Source, LT, RT, Op, T)}) | {V = LHS, T = LT})
        | @left_paren, datetime_value_expression(LHS, LT), @minus_sign, datetime_term(RHS, RT), @right_paren, interval_qualifier(Q), qid(Qid), get_source(Source), {T = qualified_subtract(Q, LHS, RHS), most_general_type(Qid, Source, Source, LT, RT, subtract, T)}.
datetime_term(V, T)---> datetime_factor(V, T).
datetime_factor(V, T)---> datetime_primary(P, T1), ( time_zone(TZ), qid(Qid), get_source(Source), {V = date_with_timezone(P, TZ), most_general_type(Qid, Source, Source, datetime_with_timezone, T1, add, T)} | {V = P, T = T1}).
datetime_primary(V, T)---> value_expression_primary(V, T) | datetime_value_function(V, T).
time_zone(T)---> @at, time_zone_specifier(T).
time_zone_specifier(time_zone(T))---> @local, {T = local} | @time, @zone, interval_value_expression(T, _).
time_zone_field(T)---> (@timezone_hour, {T = timezone_hour} | @timezone_minute, {T = timezone_minute}).

datetime_type(T)---> @date, {T = date}
        | @time, ( @left_paren, time_precision(P), @right_paren | {P = {no_precision}}), [ @with, @time, @zone], {T = time(P)}
        | @timestamp, ( @left_paren, timestamp_precision(P), @right_paren | {P = {no_precision}}), [ @with, @time, @zone], {T = timestamp(P)}.
time_precision(P)---> precision(P).
timestamp_precision(P)---> precision(P).
character_set_specification(P) ---> #P.


% Not implemented
datetime_field(_)---> {fail}.
interval_qualifier(_)---> {fail}.
interval_type(_)---> {fail}.
string_value_function(_, _)---> {fail}.
bit_value_expression(_, _)---> {fail}.

% SQL Server 'features'
:-discontiguous(string_value_function/8).
string_value_function(ltrim(S), T)---> dbms('Microsoft SQL Server'), @ltrim, @left_paren, value_expression(S, T), @right_paren.
string_value_function(rtrim(S), T)---> dbms('Microsoft SQL Server'), @rtrim, @left_paren, value_expression(S, T), @right_paren.
string_value_function(left(S, N), T)---> dbms('Microsoft SQL Server'), @left, @left_paren, value_expression(S, ST), @comma, numeric_value_expression(N, _), @right_paren, get_source(Source), sized_varchar_type(N, Source, ST, T).
string_value_function(right(S, N), T)---> dbms('Microsoft SQL Server'), @right, @left_paren, value_expression(S, ST), @comma, numeric_value_expression(N, _), @right_paren, get_source(Source), sized_varchar_type(N, Source, ST, T).
string_value_function(isnull(P, C), T)---> dbms('Microsoft SQL Server'), @isnull(Source), {semantic_error(Source, deprecated(isnull, coalesce), 1)}, @left_paren, value_expression(P, T), @comma, value_expression(C, _), @right_paren. % According to the spec, the type of ISNULL is the first argument.
string_value_function(replace(S, M, R), T)---> dbms('Microsoft SQL Server'), @replace, @left_paren, string_value_expression(S, ST), @comma, string_value_expression(M, _), @comma, string_value_expression(R, _), @right_paren,
        get_source(Source),
        {type_merge_hint(T, max),
         type_constraint(Qid, Source, T, native_type(varchar(8000))),
         type_constraint(Qid, Source, T, ST)}.

        string_value_function(upper(S), T)---> dbms('Microsoft SQL Server'), @upper, @left_paren, value_expression(S, T), @right_paren.
string_value_function(lower(S), T)---> dbms('Microsoft SQL Server'), @lower, @left_paren, value_expression(S, T), @right_paren.
string_value_function(substring(Source, Start, Length), T)--->
        dbms('Microsoft SQL Server'), @substring, @left_paren, character_value_expression(Source, ST), @comma, numeric_value_expression(Start, _), @comma, numeric_value_expression(Length, _), @right_paren, get_source(Source), sized_varchar_type(Length, Source, ST, T).
string_value_function(datename(Type, Source), varchar) ---> dbms('Microsoft SQL Server'), @datename, @left_paren, sql_server_date_part(Type), @comma, datetime_value_expression(Source, _), @right_paren.
string_value_function(dbname({}), native_type(nvarchar(128)))---> dbms('Microsoft SQL Server'), @db_name, @left_paren, @right_paren.
string_value_function(permissions(S), varchar)---> dbms('Microsoft SQL Server'), @permissions, @left_paren, character_value_expression(S, _), @right_paren.
string_value_function(username(String), native_type(nvarchar(128)))---> dbms('Microsoft SQL Server'), @user_name, @left_paren, character_value_expression(String, _), @right_paren.
string_value_function(str(S), T)---> dbms('Microsoft SQL Server'), @str, @left_paren, qid(Qid), get_source(Source), {type_merge_hint(T, str), type_constraint(Qid, Source, T, native_type(varchar(1))), type_constraint(Qid, Source, T, ST)}, numeric_value_expression(S, ST), @right_paren.

:-discontiguous(datetime_value_function/8).
datetime_value_function(dateadd(Type, N, Source), native_type(datetime))---> dbms('Microsoft SQL Server'), @dateadd, @left_paren, sql_server_date_part(Type), @comma, numeric_value_expression(N, _), @comma, datetime_value_expression(Source, _), @right_paren.

:-discontiguous(numeric_value_function/8).
numeric_value_function(current_timestamp, native_type(datetime))---> @current_timestamp.
numeric_value_function(getdate({}), native_type(datetime))---> dbms('Microsoft SQL Server'), @getdate, @left_paren, @right_paren.
numeric_value_function(fn_now({}), native_type(datetime))---> dbms('Microsoft SQL Server'), @left_curly, @fn,  @now, @left_paren, @right_paren, @right_curly, get_source(Source), {semantic_error(sql_escape, Source, 1)}.
numeric_value_function(isnull(P, C), T)---> dbms('Microsoft SQL Server'), @isnull(Source), {semantic_error(Source, deprecated(isnull, coalesce), 1)}, @left_paren, value_expression(P, T), @comma, value_expression(C, _), @right_paren.
numeric_value_function(datediff(Type, LHS, RHS), native_type(int))---> dbms('Microsoft SQL Server'), @datediff, @left_paren, sql_server_date_part(Type), @comma, datetime_value_expression(LHS, _), @comma, numeric_value_expression(RHS, _), @right_paren.
numeric_value_function(day(S), native_type(int))---> dbms('Microsoft SQL Server'), @day, @left_paren, datetime_value_expression(S, _), @right_paren.
numeric_value_function(month(S), native_type(int))---> dbms('Microsoft SQL Server'), @month, @left_paren, datetime_value_expression(S, _), @right_paren.
numeric_value_function(year(S), native_type(int))---> dbms('Microsoft SQL Server'), @year, @left_paren, datetime_value_expression(S, _), @right_paren.

numeric_value_function(datepart(Type, S), native_type(int))---> dbms('Microsoft SQL Server'), @datepart, @left_paren, sql_server_date_part(Type), @comma, datetime_value_expression(S, _), @right_paren.
numeric_value_function(charindex(Source, Search, Start), native_type(int))---> dbms('Microsoft SQL Server'), @charindex, @left_paren, character_value_expression(Source, _), @comma, string_value_expression(Search, _), ( @comma, numeric_value_expression(Start, _) | {Start = {no_start}}), @right_paren.
numeric_value_function(len(Source), native_type(int))---> dbms('Microsoft SQL Server'), @len, @left_paren, character_value_expression(Source, _), @right_paren.
numeric_value_function(abs(S), T)---> dbms('Microsoft SQL Server'), @abs, @left_paren, {force_type_not_domain(T)}, numeric_value_expression(S, T), @right_paren.
numeric_value_function(round(S, P), T)---> dbms('Microsoft SQL Server'), @round, @left_paren, {force_type_not_domain(T)}, numeric_value_expression(S, T), @comma, numeric_value_expression(P, _), @right_paren.
numeric_value_function(floor(S), T)---> dbms('Microsoft SQL Server'), @floor, @left_paren, numeric_value_expression(S, ST), @right_paren, qid(Qid), get_source(Source), {type_merge_hint(T, round), type_constraint(Qid, Source, T, ST), type_constraint_ready(Qid, T)}.
numeric_value_function(ceiling(S), native_type(int))---> dbms('Microsoft SQL Server'), @ceiling, @left_paren, numeric_value_expression(S, _), @right_paren.
sql_server_date_part(T)---> ( @year | @yy | @yyyy), {T = year}
        | (@quarter | @qq | @q ), {T = quarter}
        | (@month | @mm | @m), {T = month}
        | (@dayofyear | @dy), {T = dayofyear}
        | (@day | @dd | @d | #literal(day, _), get_source(Source), {semantic_error(superfluous_quote(day), Source, 1)}), {T = day}
        | (@week | @wk | @ww), {T = week}
        | (@weekday | @dw | @w), {T = weekday}
        | (@hour | @hh), {T = hour}
        | (@minute | @mi | @n), {T = minute}
        | (@second | @ss | @s), {T = second}
        | (@millisecond | @ms), {T = millisecond}
        | (@microsecond | @mcs), {T = microsecond}
        | (@nanosecond | @ns), {T = nanosecond}.


:-discontiguous(cast_specification/8).
cast_specification(precision_cast(Target, Operand, P), Type)---> dbms('Microsoft SQL Server'), @convert, @left_paren, cast_target(Target), @comma, cast_operand(Operand), ( @comma, precision(P) | {P = {no_precision}} ), @right_paren, {strip_sql_comments(Target, Type)}.

top_clause(top(N))---> @top, numeric_value_expression(NN, _T), ( @percent, get_source(Source), {semantic_error(percent, Source, 1)}, {N = percent(NN)} | {N = NN}). % TBD: Check that T is integer!
limit_clause(top(N))---> @limit, numeric_value_expression(N, _T). % TBD: Check that T is integer!

with_attribute(With)---> @with, @schemabinding, {With = with(schemabinding)} % schema_bound_view(ViewName)
        | {With = {no_with}}. % view_attribute can also be ENCRYPTION or VIEW_METADATA, not used

with_clause(with(nolock))---> @with, @left_paren, @nolock, @right_paren.
with_clause(with(noexpand))---> @with, @left_paren, @noexpand, @right_paren. % eg force use of indexed views

for_clause(for(xml_path(I)))---> @for, @xml, @path, @left_paren, string_value_expression(I, _), @right_paren, qid(Q), {query_is_xml(Q)}.
:-discontiguous(grouping_column_reference/7).
grouping_column_reference(group_expression(Expression, Collate))---> dbms('Microsoft SQL Server'), value_expression(Expression, _), ( collate_clause(Collate) | {Collate = {no_collation}} ).
:-discontiguous(sort_key/7).
sort_key(sort_expression(Expression))---> dbms('Microsoft SQL Server'), value_expression(Expression, _).

:-discontiguous(datetime_type/7).
datetime_type(datetime)---> @datetime.


sized_varchar_type(L, L, O, O, P0, P0, Length, Source, SourceT, T):-
        memberchk(query_id(Qid), O),
        strip_sql_comments(Length, LS),
        ( integer(LS) ->
            % substring(foo, 5000) is the min of (5000, len(foo)). Create a new type resolution branch
            type_merge_hint(T, sized_varchar),
            type_constraint(Qid, Source, T, native_type(varchar(LS))),
            type_constraint(Qid, Source, T, SourceT)
        ; otherwise->
            force_type_not_domain(T),
            type_constraint(Qid, Source, T, SourceT),
            type_constraint_ready(Qid, T)
        ).

sql_explain(_).
user:goal_expansion(sql_explain(_), true).
%user:goal_expansion(sql_explain(A), (format(user_error, '*** ~w~n', [A]))).

remove_quoted_column @
        type_constraint(QueryId, Source, Type, typeof(identifier(A, B), literal(ColumnName, string)))
        <=>
        type_constraint(QueryId, Source, Type, typeof(identifier(A, B), ColumnName)).

define_type_from_subquery @
        derived_query_column(QueryId, TableAlias, Column, DerivedType)
        \
        type_constraint(QueryId, Source, Type, typeof(identifier(_, TableAlias), Column))
        <=>
        type_constraint(QueryId, Source, Type, DerivedType).

define_type_from_subquery_with_unspecified_column @
        derived_query_column(QueryId, _, Column, DerivedType)
        \
        type_constraint(QueryId, Source, Type, typeof({no_qualifier}, Column))
        <=>
        type_constraint(QueryId, Source, Type, DerivedType).


define_type_from_literal @
        type_constraint(QueryId, Source, Type, typeof({no_qualifier}, literal(Literal, Kind)))
        <=>
        ( Literal == '' ->
            ColumnType = native_type(varchar(1)) % Quirk?
        ; Kind == string->
            atom_length(Literal, L),
            ColumnType = native_type(varchar(L))
        ; otherwise->
            ColumnType = native_type(Kind)
        ),
        type_constraint(QueryId, Source, Type, ColumnType).

define_type_from_query @
        query_table(QueryId, TableAlias, identifier(_, TableName))
        \
        type_constraint(QueryId, Source, Type, typeof(identifier(_, TableAlias), SourceColumnName))
        <=>
        default_schema(Schema),
        fetch_database_attribute(_, Schema, TableName, SourceColumnName, ColumnType, _, _, _)
        |
        type_constraint(QueryId, Source, Type, ColumnType).

define_type_from_query_with_unnamed_table @
        query_table(QueryId, _, identifier(_, TableName))
        \
        type_constraint(QueryId, Source, Type, typeof({no_qualifier}, SourceColumnName))
        <=>
        % Have to search all tables :-(
        default_schema(Schema),
        %writeln(checking(Schema, TableName, SourceColumnName)),
        fetch_database_attribute(_, Schema, TableName, SourceColumnName, ColumnType, _, _, _)
        %writeln(found)
        |
        type_constraint(QueryId, Source, Type, ColumnType).

define_type_from_uncorrelated_table_with_explicit_reference @ % Yuck!
        query_table(QueryId, uncorrelated(TableName), _)
        \
        type_constraint(QueryId, Source, Type, typeof(identifier(_, TableName), SourceColumnName))
        <=>
        default_schema(Schema),
        fetch_database_attribute(_, Schema, TableName, SourceColumnName, ColumnType, _, _, _)
        |
        type_constraint(QueryId, Source, Type, ColumnType).

define_type_from_uncorrelated_table @
        query_table(QueryId, uncorrelated(TableName), _)
        \
        type_constraint(QueryId, Source, Type, typeof(X, SourceColumnName))
        <=>
        X \= identifier(_,_),
        % Have to search all tables here, too :-(
        default_schema(Schema),
        fetch_database_attribute(_, Schema, TableName, SourceColumnName, ColumnType, _, _, _)
        |
        type_constraint(QueryId, Source, Type, ColumnType).


crush_xml_subquery_into_scalar @
        query_is_xml(SubQueryId),
        subquery(QueryId, SubQueryId)
        \
        type_constraint(QueryId, Source, Type, scalar([merged(_, _, _Subtype)]))
        <=>
        sql_explain(crush_xml),
        type_constraint(QueryId, Source, Type, native_type(nvarchar(max))).


crush_subquery_into_scalar @
        type_constraint(QueryId, Source, Type, scalar([merged(_, _, Subtype)]))
        <=>
        sql_explain(crush_subquery),
        type_constraint(QueryId, Source, Type, Subtype).

concatenate_char @
        type_merge_hint(Type, Hint),
        type_constraint(QueryId, Source1, Type, native_type(varchar(N))),
        type_constraint(QueryId, Source2, Type, native_type(varchar(M)))
        <=>
        Hint == add ; Hint == concatenate
        |
        ( N == max ->
            Z = max
        ; M == max ->
            Z = max
        ; otherwise ->
            Z is min(N+M, 8000)
        ),
        sql_explain(concatenate_char),
        merge_sources(Source1, Source2, Source),
        type_constraint(QueryId, Source, Type, native_type(varchar(Z))),
        type_constraint_ready(QueryId, Type).

concatenate_nchar_and_varchar @
        type_merge_hint(Type, Hint),
        type_constraint(QueryId, Source1, Type, native_type(nvarchar(N))),
        type_constraint(QueryId, Source2, Type, native_type(varchar(M)))
        <=>
        Hint == add ; Hint == concatenate
        |
        ( N == max ->
            Z = max
        ; M == max ->
            Z = max
        ; otherwise->
            Z is min(N+M, 8000)
        ),
        sql_explain(concatenate_nchar_and_varchar),
        merge_sources(Source1, Source2, Source),
        type_constraint(QueryId, Source, Type, native_type(nvarchar(Z))),
        type_constraint_ready(QueryId, Type).

concatenate_nchar_and_nchar @
        type_merge_hint(Type, Hint),
        type_constraint(QueryId, Source1, Type, native_type(nvarchar(N))),
        type_constraint(QueryId, Source2, Type, native_type(nvarchar(M)))
        <=>
        Hint == add ; Hint == concatenate
        |
        ( N == max ->
            Z = max
        ; M == max ->
            Z = max
        ; otherwise->
            Z is min(N+M, 8000)
        ),
        sql_explain(concatenate_nchar_and_nchar),
        merge_sources(Source1, Source2, Source),
        type_constraint(QueryId, Source, Type, native_type(nvarchar(Z))),
        type_constraint_ready(QueryId, Type).


merge_sized_chars @
        type_constraint(QueryId, _, Type, native_type(varchar(N)))
        \
        type_merge_hint(Type, sized_varchar),
        type_constraint(QueryId, _, Type, native_type(varchar(M)))
        <=>
        ( integer(N), integer(M), N < M )
        |
        sql_explain(merge_sized_chars),
        type_constraint_ready(QueryId, Type).

union_chars @
        type_constraint(QueryId, _, Type, native_type(varchar(N)))
        \
        type_merge_hint(Type, Hint),
        type_constraint(QueryId, _, Type, native_type(varchar(M)))
        <=>
        Hint \== concatenate,
        ( N == max
        ; M == max
        ; N >= M
        )
        |
        sql_explain(union_chars(N, M)),
        type_constraint_ready(QueryId, Type).

union_nchars @
        type_constraint(QueryId, _, Type, native_type(nvarchar(N)))
        \
        type_merge_hint(Type, Hint),
        type_constraint(QueryId, _, Type, native_type(nvarchar(M)))
        <=>
        Hint \== concatenate,
        ( N == max
        ; M == max
        ; N >= M
        )
        |
        sql_explain(union_nchars),
        type_constraint_ready(QueryId, Type).

union_nchar_and_varchar @
        type_merge_hint(Type, Hint),
        type_constraint(QueryId, Source1, Type, native_type(nvarchar(N))),
        type_constraint(QueryId, Source2, Type, native_type(varchar(M)))
        <=>
        Hint \== concatenate
        |
        ( N == max->
            Z = max
        ; M == max->
            Z = max
        ; otherwise->
            Z is max(N, M)
        ),
        sql_explain(union_nchar_and_varchar),
        merge_sources(Source1, Source2, Source),
        type_constraint(QueryId, Source, Type, native_type(nvarchar(Z))),
        type_constraint_ready(QueryId, Type).



expand_precision_integer_to_decimal @ % These come from literals in the query like -1
        type_constraint(QueryId, Source1, Type, native_type(decimal(_, _)))
        \
        type_constraint(QueryId, Source2, Type, native_type(int(N)))
        <=>
        sql_explain(precision_integer_to_decimal(N,0)),
        merge_sources(Source1, Source2, Source),
        type_constraint(QueryId, Source, Type, native_type(decimal(N, 0))).

expand_precision_integer_to_general_integer @
        type_constraint(QueryId, _, Type, native_type(int))
        \
        type_merge_hint(Type, _),
        type_constraint(QueryId, _, Type, native_type(int(_)))
        <=>
        sql_explain(precision_integer_to_int),
        type_constraint_ready(QueryId, Type).


expand_integer_to_decimal @
        type_merge_hint(Type, union),
        type_constraint(QueryId, Source1, Type, native_type(decimal(_, _)))
        \
        type_constraint(QueryId, Source2, Type, native_type(int))
        <=>
        sql_explain(integer_to_decimal_for_union),
        merge_sources(Source1, Source2, Source),
        type_constraint(QueryId, Source, Type, native_type(decimal(10,0))).

expand_tinyint_to_int @
        type_constraint(QueryId, _, Type, native_type(int))
        \
        type_merge_hint(Type, _),
        type_constraint(QueryId, _, Type, native_type(tinyint))
        <=>
        sql_explain(tinyint_to_int),
        type_constraint_ready(QueryId, Type).

expand_tinyint_to_precision_int @
        type_constraint(QueryId, _, Type, native_type(int(_)))
        \
        type_merge_hint(Type, _),
        type_constraint(QueryId, _, Type, native_type(tinyint))
        <=>
        sql_explain(tinyint_to_precision_integer),
        type_constraint_ready(QueryId, Type).


expand_int_to_float @
        type_constraint(QueryId, _, Type, native_type(float(_)))
        \
        type_merge_hint(Type, _),
        type_constraint(QueryId, _, Type, native_type(int))
        <=>
        sql_explain(int_to_float),
        type_constraint_ready(QueryId, Type).

%Quirk. Note that if the varchar is anything BUT spaces, you get an error when selecting from the view!
quirk_tinyint_and_varchar_is_tinyint @
        type_constraint(QueryId, _, Type, native_type(tinyint))
        \
        type_merge_hint(Type, _),
        type_constraint(QueryId, _, Type, native_type(varchar(_)))
        <=>
        sql_explain(tinyint_and_varchar),
        type_constraint_ready(QueryId, Type).

max_varchars @
        type_constraint(QueryId, _, Type, native_type(varchar(A)))
        \
        type_merge_hint(Type, max),
        type_constraint(QueryId, _, Type, native_type(varchar(B)))
        <=>
        integer(A), integer(B), A >= B
        |
        sql_explain(max_varchars),
        type_constraint_ready(QueryId, Type).

max_nvarchar_with_varchar @
        type_merge_hint(Type, max),
        type_constraint(QueryId, Source1, Type, native_type(nvarchar(A))),
        type_constraint(QueryId, Source2, Type, native_type(varchar(B)))
        <=>
        ( ( A == max ; B == max)->
            C = max
        ; otherwise->
            C is max(A, B)
        ),
        sql_explain(max_vvarchar_with_varchar),
        merge_sources(Source1, Source2, Source),
        type_constraint(QueryId, Source, Type, native_type(nvarchar(C))),
        type_constraint_ready(QueryId, Type).


str_expression_with_int @
        type_merge_hint(Type, str),
        type_constraint(QueryId, Source1, Type, native_type(int)),
        type_constraint(QueryId, Source2, Type, native_type(varchar(A)))
        <=>
        ( A == max ->
            B = max
        ; otherwise ->
            B is max(A, 10)
        ),
        sql_explain(str_expression_with_int),
        merge_sources(Source1, Source2, Source),
        type_constraint(QueryId, Source, Type, native_type(varchar(B))),
        type_constraint_ready(QueryId, Type).

%Quirk. Note that if the varchar is anything BUT spaces, you get an error when selecting from the view!
quirk_int_and_varchar_is_int @
        type_constraint(QueryId, Source1, Type, native_type(int))
        \
        type_merge_hint(Type, _),
        type_constraint(QueryId, Source2, Type, native_type(varchar(_)))
        <=>
        sql_explain(int_and_varchar),
        type_mismatch(Source1, Source2, int, varchar),
        type_constraint_ready(QueryId, Type).

quirk_int_and_varchar_is_int @
        type_constraint(QueryId, Source1, Type, native_type(int(_)))
        \
        type_merge_hint(Type, _),
        type_constraint(QueryId, Source2, Type, native_type(varchar(_)))
        <=>
        sql_explain(int_and_varchar),
        type_mismatch(Source1, Source2, int, varchar),
        type_constraint_ready(QueryId, Type).


%Quirk
quirk_datetime_and_int_is_int @
        type_constraint(QueryId, _, Type, native_type(datetime))
        \
        type_merge_hint(Type, _),
        type_constraint(QueryId, _, Type, native_type(int))
        <=>
        sql_explain(datetime_and_int),
        type_constraint_ready(QueryId, Type).

%Quirk
quirk_datetime_and_precision_int_is_precision_int @
        type_constraint(QueryId, _, Type, native_type(datetime))
        \
        type_merge_hint(Type, _),
        type_constraint(QueryId, _, Type, native_type(int(_)))
        <=>
        sql_explain(datetime_and_precision_int),
        type_constraint_ready(QueryId, Type).


integer_addition @
        type_constraint(QueryId, _, Type, native_type(int))
        \
        type_merge_hint(Type, Hint),
        type_constraint(QueryId, _, Type, native_type(int))
        <=>
        memberchk(Hint, [add, subtract, concatenate])
        |
        sql_explain(integer_addition),
        type_constraint_ready(QueryId, Type).

integer_multiplication_requires_promotion @
        type_merge_hint(Type, Hint)
        \
        type_constraint(QueryId, Source, Type, native_type(int))
        <=>
        memberchk(Hint, [multiply, divide(_)])
        |
        sql_explain(promote_int_for_multiplication),
        type_constraint(QueryId, Source, Type, native_type(decimal(10,0))).


integer_and_decimal_arithmetic @
        type_merge_hint(Type, Hint),
        type_constraint(QueryId, Source1, Type, native_type(decimal(_, _)))
        \
        type_constraint(QueryId, Source2, Type, native_type(int))
        <=>
        Hint \== union
        |
        sql_explain(promote_int_to_decimal_for_arithmetic(Hint)),
        merge_sources(Source1, Source2, Source),
        type_constraint(QueryId, Source, Type, native_type(decimal(10,0))).


expand_type_scope_decimal_with_hint @
        type_merge_hint(Type, Hint),
        type_constraint(QueryId, Source1, Type, native_type(decimal(P1, S1))),
        type_constraint(QueryId, Source2, Type, native_type(decimal(P2, S2)))
        <=>
        ( Hint == multiply->
            P is P1 + P2 + 1,
            S is S1 + S2
        ; ( Hint == add ;  Hint == concatenate) -> % TBD: This indicates a parse failure, surely...
            P is max(S1, S2) + max(P1-S1, P2-S2) + 1,
            S is max(S1, S2)
        ; Hint == subtract ->
            P is max(S1, S2) + max(P1-S1, P2-S2) + 1,
            S is max(S1, S2)
        ; Hint = divide(DA)->
            ( DA = node(Divisor, _, _, _, _) ->
                true
            ; otherwise->
                Divisor = DA
            ),
            % We need to know which is the divisor and which is the quotient to
            % calculate this correctly
            ( Divisor = domain(D) ->
                fetch_domain_data_type(D, V)
            ; Divisor = native_type(V)->
                true
            ; otherwise->
                throw(bad_divisor(Divisor))
            ),
            ( ( ( V = int(P1), S1 == 0) % Promoted from int(P1) -> decimal(P1, 0)
              ;
                V == decimal(P1, S1))->

                % Oops, round the wrong way!
                P is P2 - S2 + S1 + max(6, S2 + P1 + 1),
                S is max(6, S2 + P1 + 1)
            ; otherwise->
                % Proceed as usual
                P is P1 - S1 + S2 + max(6, S1 + P2 + 1),
                S is max(6, S1 + P2 + 1)
            )
        ; Hint == union->
            P is max(S1, S2) + max(P1-S1, P2-S2),
            S is max(S1, S2)
        ; otherwise->
            throw(unhandled_scope(Hint))
        ),
        ( P > 38 ->
            Px = 38,
            % I determined this for addition by trial and error :-(
            % For multiplication, see http://blogs.msdn.com/b/sqlprogrammability/archive/2006/03/29/564110.aspx
            % eg SELECT (cast(1 as decimal(38, 6))) - (cast(1 as decimal(14, 2)))
            ( memberchk(Hint, [add, concatenate, subtract])->
                Sx is max(0, S - (P-39))
            ; Hint == union->
                Sx is max(0, S - (P-38))
            ; max(0, S-(P-38)) < 6->
                Sx is min(S, 6)
            ; otherwise->
                Sx is max(0, S - (P-38))
            )
        ; otherwise->
            Px = P,
            Sx = S
        ),
        sql_explain(decimal_arithmetic(Hint, Px, Sx)),
        merge_sources(Source1, Source2, Source),
        type_constraint(QueryId, Source, Type, native_type(decimal(Px, Sx))),
        type_constraint_ready(QueryId, Type).

resolve_types @
        resolve_types(QueryId)
        <=>
        commit(QueryId).

resolve_unions @
        commit(QueryId)
        \
        union_type(QueryId, L, R, T)
        <=>
        is_list(L),
        is_list(R)
        |
        ( resolve_union_type(QueryId, L, R, T)->
            true
        ; otherwise->
            format(atom(Message), 'Could not determine the type of the union of ~w and ~w', [L, R]),
            throw(cql_error(failed_to_resolve_union, Message))
        ).

resolve_derived_tables @
        commit(QueryId),
        derived_table(QueryId, Table, Constraint)
        <=>
        is_list(Constraint)
        |
        create_derived_table(QueryId, Table, Constraint),
        commit(QueryId).

union_of_type_decimal_domains_is_not_a_domain @
        commit(QueryId), % Have to suspend commits until all the new constraints are in
        type_merge_hint(Type, union),
        type_constraint(QueryId, Source1, Type, domain(D1)),
        type_constraint(QueryId, Source2, Type, domain(D2))
        <=>
        fetch_domain_data_type(D1, decimal(A1, A2)),
        fetch_domain_data_type(D2, decimal(B1, B2))
        |
        sql_explain(union_domains),
        most_general_type(QueryId, Source1, Source2, decimal(A1, A2), decimal(B1, B2), union, Type),
        commit(QueryId).

merge_domains @
        commit(QueryId), % Have to suspend commits until all the new constraints are in
        type_merge_hint(Type, Hint),
        type_constraint(QueryId, Source1, Type, domain(D1)),
        type_constraint(QueryId, Source2, Type, domain(D2))
        <=>
        fetch_domain_data_type(D1, T1),
        fetch_domain_data_type(D2, T2)
        |
        ( T1 = decimal(_, _),
          T2 = decimal(_, _)->
            most_general_type(QueryId, Source1, Source2, T1, T2, Hint, Type)
        ; T1 = datetime, T2 = datetime, memberchk(Hint, [add, subtract, concatenate])->
            type_constraint(QueryId, Source1, Type, native_type(datetime)),
            type_constraint_ready(QueryId, Type)
        ; D1 == D2, Hint \== concatenate->
            type_constraint(QueryId, Source1, Type, domain(D1)),
            type_constraint_ready(QueryId, Type)
        ; otherwise->
            most_general_type(QueryId, Source1, Source1, T1, T2, Hint, Type)
        ),
        sql_explain(join_domains_for(Hint, D1, D2, Type)),
        commit(QueryId).

merge_domain_to_native @
        commit(QueryId), % Have to suspend commits until all the new constraints are in
        type_merge_hint(Type, Hint),
        type_constraint(QueryId, Source1, Type, domain(D)),
        type_constraint(QueryId, Source2, Type, native_type(NT))
        <=>
        fetch_domain_data_type(D, T)
        |
        sql_explain(join_domain_to_native),
        most_general_type(QueryId, Source1, Source2, T, NT, Hint, Type),
        commit(QueryId).

drop_nulltype_in_favour_of_domain @
        % SQL Server drops the domain here too for some reason
        type_merge_hint(Type, _Anything),
        type_constraint(QueryId, Source1, Type, domain(D)),
        type_constraint(QueryId, Source2, Type, {nulltype})
        <=>
        sql_explain(drop_nulltype_for_domain),
        fetch_domain_data_type(D, T),
        merge_sources(Source1, Source2, Source),
        type_constraint(QueryId, Source, Type, native_type(T)),
        type_constraint_ready(QueryId, Type).

drop_nulltype_in_favour_of_native_type @
        type_constraint(QueryId, _, Type, native_type(T))
        \
        type_merge_hint(Type, Hint),
        type_constraint(QueryId, _, Type, {nulltype})
        <=>
        sql_explain(drop_nulltype_for_native(T, Hint)),
        type_constraint_ready(QueryId, Type).


% This is actually wrong. In fact, NULL is not a valid type in ANSI SQL.
% SQL Server appears to treat nulltypes as integers (ultimately) but delays resolving them
% until it has to. 'PostgreSQL' takes a very hard line on the standard, and says that the union
% of two nulltypes is a varchar. This means if you do this:
% SELECT NULL UNION SELECT NULL UNION SELECT 1
% you will get a type error, but if you do this:
% SELECT NULL UNION SELECT 1 UNION SELECT NULL
% you will not. In reality, the nicest thing to do is not to return an untyped NULL by changing
% SELECT NULL AS foo     -> SELECT CAST(NULL AS datetime) AS foo
% in the cases where NULL even makes sense.
drop_nulltype_in_favour_of_another_nulltype @
        type_constraint(QueryId, _, Type, {nulltype})
        \
        type_merge_hint(Type, Hint),
        type_constraint(QueryId, _, Type, {nulltype})
        <=>
        ( Hint == union ->
            ( prolog_load_context(file, _Filename)->
                true
                %format(user_error, '~w: Union of nulls is illegal. Use CAST() to make the type explicit~n', [Filename, Hint])
            ; otherwise->
                true
            )
        ; otherwise->
            true
        ),
        sql_explain(drop_nulltype_for_nulltype(Hint)),
        type_constraint_ready(QueryId, Type).

force_type_not_domain @
        % SQL Server seems to abandon domains very quickly in numeric expressions
        force_type_not_domain(Type),
        type_constraint(QueryId, Source, Type, domain(D))
        <=>
        fetch_domain_data_type(D, decimal(P, S))
        |
        sql_explain(forcing_decimal_not_domain(D)),
        type_constraint(QueryId, Source, Type, native_type(decimal(P, S))).

force_type_not_domain @
        % SQL Server seems to abandon domains in string operations too (eg substring(domain, 1) -> varchar)
        force_type_not_domain(Type),
        type_constraint(QueryId, Source, Type, domain(D))
        <=>
        fetch_domain_data_type(D, varchar(L))
        |
        sql_explain(forcing_varchar_not_domain(D)),
        type_constraint(QueryId, Source, Type, native_type(varchar(L))).

force_type_not_domain @
        force_type_not_domain(Type),
        type_constraint(QueryId, Source, Type, domain(D))
        <=>
        fetch_domain_data_type(D, datetime)
        |
        sql_explain(forcing_datetime_not_domain(D)),
        type_constraint(QueryId, Source, Type, native_type(datetime)).


rounded_int_is_decimal @ /* is it? */
        type_merge_hint(Type, round),
        type_constraint(QueryId, Source, Type, native_type(int))
        <=>
        sql_explain(rounded_int_is_decimal),
        type_constraint(QueryId, Source, Type, native_type(decimal(10,0))).

rounded_decimal_has_no_scale @
        type_merge_hint(Type, round),
        type_constraint(QueryId, Source, Type, native_type(decimal(P, _)))
        <=>
        sql_explain(rounded_decimal_has_no_scale),
        type_constraint(QueryId, Source, Type, native_type(decimal(P,0))).


force_domain_type_to_sum @
        % SQL Server always turns decimal(_, S) -> decimal(38, S) when adding
        type_merge_hint(Type, sum),
        type_constraint(QueryId, Source, Type, domain(D))
        <=>
        fetch_domain_data_type(D, decimal(_, S))
        |
        sql_explain(forcing_domain_to_sum(D)),
        type_constraint(QueryId, Source, Type, native_type(decimal(38, S))).

force_domain_type_to_avg @
        % SQL Server always turns decimal(_, S) -> decimal(38, 6) when averaging
        type_merge_hint(Type, avg),
        type_constraint(QueryId, Source, Type, domain(D))
        <=>
        sql_explain(forcing_domain_to_avg(D)),
        type_constraint(QueryId, Source, Type, native_type(decimal(38, 6))).

force_native_type_to_sum @
        % SQL Server always turns decimal(_, S) -> decimal(38, S) when adding
        type_merge_hint(Type, sum),
        type_constraint(QueryId, Source, Type, native_type(decimal(_, S)))
        <=>
        sql_explain(forcing_native_to_sum),
        type_constraint(QueryId, Source, Type, native_type(decimal(38, S))).

force_native_type_to_avg @
        % SQL Server always turns decimal(_, S) -> decimal(38, S) when averaging
        type_merge_hint(Type, avg),
        type_constraint(QueryId, Source, Type, native_type(decimal(_, _)))
        <=>
        sql_explain(forcing_native_to_avg),
        type_constraint(QueryId, Source, Type, native_type(decimal(38, 6))).

union_precision_ints @
        type_merge_hint(Type, union),
        type_constraint(QueryId, Source1, Type, native_type(int(_))),
        type_constraint(QueryId, Source2, Type, native_type(int(_)))
        <=>
        sql_explain(union_precision_ints),
        merge_sources(Source1, Source2, Source),
        type_constraint(QueryId, Source, Type, native_type(int)),
        type_constraint_ready(QueryId, Type).

union_identical_natives @
        type_constraint(QueryId, _, Type, native_type(T))
        \
        type_merge_hint(Type, union),
        type_constraint(QueryId, _, Type, native_type(T))
        <=>
        sql_explain(union_identical_natives(T)),
        type_constraint_ready(QueryId, Type).

merge_datetime_and_int @
        type_merge_hint(Type, _Hint),
        type_constraint(QueryId, Source1, Type, native_type(datetime)),
        type_constraint(QueryId, Source2, Type, native_type(decimal(_,_)))
        <=>
        sql_explain(operation_with_datetime_and_decimal),
        merge_sources(Source1, Source2, Source),
        type_constraint(QueryId, Source, Type, native_type(datetime)),
        type_constraint_ready(QueryId, Type).

union_of_datetime_and_date_is_datetime @
        type_merge_hint(Type, union),
        type_constraint(QueryId, Source, Type, native_type(datetime))
        \
        type_constraint(QueryId, Source, Type, native_type(date))
        <=>
        sql_explain(union_of_datetime_and_date),
        type_constraint_ready(QueryId, Type).


accept_domain @
        type_constraint(QueryId, _, Type, domain(Domain)),
        type_constraint_ready(QueryId, Type)
        <=>
        sql_explain(accepting(domain(Domain), Type)),
        Type = domain(Domain).

accept_native_type @
        type_constraint(QueryId, _, Type, native_type(Native)),
        type_constraint_ready(QueryId, Type)
        <=>
        sql_explain(accepting(native_type(Native), Type)),
        Type = native_type(Native).

accept_nulltype @
        type_constraint(QueryId, _, Type, {nulltype}),
        type_constraint_ready(QueryId, Type)
        <=>
        sql_explain(accepting_nulltype(Type)),
        Type = {nulltype}.


find_column_types @
        query_table(QueryId, _, identifier(_, TableName)),
        find_all_column_types(QueryId, Source, Tail)
        <=>
        default_schema(Schema),
        findall(merged(ColumnName, Source, Domain),
                fetch_database_attribute(_, Schema, TableName, ColumnName, Domain, _, _, _),
                Columns),
        create_column_constraints(QueryId, Source, Columns, Tail, NewTail),
        find_all_column_types(QueryId, Source, NewTail).

find_column_types @
        derived_query_column(QueryId, _, Column, ColumnType),
        find_all_column_types(QueryId, Source, Tail)
        <=>
        Tail = [merged(Column, Source, ColumnType)|NewTail],
        find_all_column_types(QueryId, Source, NewTail).

share_commit @
        commit(Parent),
        subquery(Parent, Child)
        ==>
        commit(Child).

commit_peers @
        commit(Qid),
        peer_query(Qid, Sibling)
        ==>
        commit(Sibling).


share_table @
        subquery(Parent, Child),
        query_table(Parent, A, B)
        ==>
        query_table(Child, A, B).

%share_derived_table @
%        subquery(Parent, Child),
%        derived_query_column(Parent, A, B, C)
%        ==>
%        derived_query_column(Child, A, B, C).

finished_finding @
        commit(QueryId)
        \
        find_all_column_types(QueryId, _, Tail)
        <=>
        Tail = [].

/*
 Unfortunately this isnt ALWAYS indicative of failure. The test case was
SELECT x FROM y WHERE NOT EXISTS (SELECT a FROM b UNION SELECT c FROM d WHERE y.z = 1)
fail_to_resolve @
        commit_phase(QueryId, _),
        type_constraint(QueryId, _, _, typeof(A, B))
        <=>
        throw(cannot_resolve_type(A, B)).
*/

frozen_reverse @
        frozen_reverse(_, A, B)
        <=>
        is_list(A)
        |
        A = B.
%        reverse(A, B).

cleanup @ cleanup(QueryId) \ frozen_reverse(QueryId, _, _) <=> true.
cleanup @ cleanup(QueryId) \ type_constraint(QueryId, _, _, _) <=> true. % This should be an error, I think?
cleanup @ cleanup(_QueryId) \ type_constraint_ready(_, _) <=> true.
cleanup @ cleanup(_QueryId) \ type_merge_hint(_, _) <=> true.
cleanup @ cleanup(QueryId) \ query_table(QueryId, _, _) <=> true.
cleanup @ cleanup(QueryId) \ derived_query_column(QueryId, _, _, _) <=> true.
cleanup @ cleanup(QueryId) \ subquery(QueryId, SubQueryId) <=> cleanup(SubQueryId).
cleanup @ cleanup(QueryId) \ peer_query(QueryId, PeerQueryId) <=> cleanup(PeerQueryId).
cleanup @ cleanup(QueryId) \ query_is_xml(QueryId) <=> true.
cleanup @ cleanup(QueryId) \ commit(QueryId) <=> true.
cleanup @ cleanup(QueryId) \ find_all_column_types(QueryId, _, _) <=> true.
cleanup @ cleanup(QueryId) \ subquery(SubQueryId, QueryId) <=> cleanup(SubQueryId).
%cleanup @ cleanup(QueryId) \ force_type_not_domain(_) <=> true.
cleanup @ cleanup(_) <=> true.

create_column_constraints(_QueryId, _Source, [], NewTail, NewTail):- !.
create_column_constraints(QueryId, Source, [merged(Name, Source, Domain)|Columns], [merged(Name, Source, Type)|T], NewTail):-
        type_constraint(QueryId, Source, Type, Domain),
        type_constraint_ready(QueryId, Type),
        create_column_constraints(QueryId, Source, Columns, T, NewTail).


resolve_union_type(_QueryId, [], [], []):- !.
resolve_union_type(_QueryId, A, [], []):- throw(union_mismatch(left(A))).
resolve_union_type(_QueryId, [], A, []):- throw(union_mismatch(right(A))).
resolve_union_type(QueryId, [merged(NameA, SourceA, TypeA)|As], [merged(NameB, SourceB, TypeB)|Bs], [merged(Name, SourceA, C)|Cs]):-
        % We can just pick either A or B for the source, I guess?
        % A and B are already CHR-resolved. Calling most_general_type/3 would add native_type/1 wrappers.
        % Quirk
        ( nonvar(TypeA),
          TypeA = domain(X),
          nonvar(TypeB),
          TypeB = domain(X),
          fetch_domain_data_type(X, decimal(P, S))->
            C = native_type(decimal(P,S))
        ; otherwise->
            type_merge_hint(C, union),
            type_constraint(QueryId, SourceA, C, TypeA),
            type_constraint(QueryId, SourceB, C, TypeB)
        ),
        ( NameA == {no_alias} ->
            name_from_identifier(NameB, Name)
        ; otherwise->
            name_from_identifier(NameA, Name)
        ),
        resolve_union_type(QueryId, As, Bs, Cs).


name_from_identifier(literal(NameMC, _), Name):- !,
        downcase_atom(NameMC, Name).
name_from_identifier({no_alias}, {no_alias}):- !.
name_from_identifier(Identifier, Name):-
        downcase_atom(Identifier, Name).




check_types(_, _).
most_general_type(QueryId, S1, S2, A, B, Op, C):-
        type_merge_hint(C, Op),
        ( var(A)->
            type_constraint(QueryId, S1, C, A)
        ; otherwise->
            strip_sql_comments(A, AA),
            ( AA = native_type(AAA)->
                type_constraint(QueryId, S1, C, native_type(AAA))
            ; AA = domain(AAA)->
                type_constraint(QueryId, S1, C, domain(AAA))
            ; AA == {nulltype}->
                type_constraint(QueryId, S1, C, {nulltype})
            ; otherwise->
                type_constraint(QueryId, S1, C, native_type(AA))
            )
        ),
        ( var(B)->
            type_constraint(QueryId, S2, C, B)
        ; otherwise->
            strip_sql_comments(B, BB),
            ( BB = native_type(BBB)->
                type_constraint(QueryId, S2, C, native_type(BBB))
            ; BB = domain(BBB)->
                type_constraint(QueryId, S2, C, domain(BBB))
            ; BB == {nulltype}->
                type_constraint(QueryId, S2, C, {nulltype})
            ; otherwise->
                type_constraint(QueryId, S2, C, native_type(BB))
            )
        ).

% The type system expects everything to have at most 2 parents. Make a tree.
coalesce_type(QueryId, [A, B], [S1, S2], T):- !,
        most_general_type(QueryId, S1, S2, A, B, union, T).
coalesce_type(QueryId, [A], [S1], T):- !,
        type_constraint(QueryId, S1, T, A),
        type_constraint_ready(QueryId, T).
coalesce_type(QueryId, [A, B|Xs], [S1, S2|Ss], T):-
        most_general_type(QueryId, S1, S2, A, B, union, AB),
        coalesce_type(QueryId, Xs, Ss, XT),
        most_general_type(QueryId, S2, S2, AB, XT, union, T).

concatenate_type(QueryId, S1, S2, A, B, C):-
        type_merge_hint(C, concatenate),
        type_constraint(QueryId, S1, C, A),
        type_constraint(QueryId, S2, C, B).


merge_types(all, _, Types, Types):- !.
merge_types([], [], [], []):- !.
merge_types([derived_column(From, Alias)|As], [S|Ss], [B|Bs], [merged(Name, S, B)|Cs]):-
        ( Alias == {no_alias},
          From = column(_Qualifier, Name)->
            true
        ; otherwise->
            name_from_identifier(Alias, Name)
        ),
        merge_types(As, Ss, Bs, Cs).


determine_tables(_, Var):- var(Var), !, throw(var).
determine_tables(_, []):- !.
determine_tables(QueryId, [A|B]):- !,
        determine_tables(QueryId, A),
        determine_tables(QueryId, B).

determine_tables(QueryId, join(LHS, RHS)):- !,
        determine_tables(QueryId, LHS),
        determine_tables(QueryId, RHS).
determine_tables(QueryId, correlated_table(table(Identifier), {no_correlation})):- !,
        Identifier = identifier(_, TableName), !,
        query_table(QueryId, uncorrelated(TableName), Identifier).
determine_tables(QueryId, correlated_table(table(Identifier), correlation(Correlation, _))):- !,
        query_table(QueryId, Correlation, Identifier).
determine_tables(QueryId, qualified_join(_Type, RHS, _On)):- !,
        determine_tables(QueryId, RHS).
determine_tables(QueryId, cross_join(RHS)):- !,
        determine_tables(QueryId, RHS).
determine_tables(QueryId, derived_table(_Derivation, correlation(DerivedTableName, _), T)):- !,
        derived_table(QueryId, DerivedTableName, T).

determine_tables(_, X):- throw(determine_tables(X)).


create_derived_table(_QueryId, _DerivedTableName, []):- !.
create_derived_table(QueryId, DerivedTableName, [merged(Name, _, Type)|Columns]):- !,
        derived_query_column(QueryId, DerivedTableName, Name, Type),
        create_derived_table(QueryId, DerivedTableName, Columns).

% Explicitly named domains in views MAY have a schema. Just ignore it
fetch_domain_data_type(identifier(_, Domain), Type):-
        !,
        fetch_domain_data_type(Domain, Type).
fetch_domain_data_type(Domain, Type):-
        domain_database_data_type(Domain, Type).

fetch_database_attribute(_, Schema, TableName, _, _, _, _, _):-
        \+system_table(TableName, _, _),
        \+database_attribute(_, Schema, TableName, _, _, _, _, _),
        !,
        format(atom(Message), 'View references entity ~w which does not exist', [TableName]),
        throw(cql_error(no_such_entity, Message)).

fetch_database_attribute(_, Schema, TableName, ColumnName, Domain, _, _, _):-
        system_table(TableName, ColumnName, Domain)
        ;
        database_attribute(_, Schema, TableName, ColumnName, Domain, _, _, _).


system_table(sysobjects, name, native_type(nvarchar(128))).
system_table(sysobjects, name, native_type(nvarchar(128))).
system_table(sysobjects, xtype, native_type(char(2))).
system_table(sysobjects, xtype, native_type(nvarchar(128))).
system_table(sysobjects, id, native_type(int)).
system_table(sysobjects, uid, native_type(smallint)).
system_table(sysobjects, parent_obj, native_type(int)).
system_table(syscolumns, name, native_type(nvarchar(128))).
system_table(syscolumns, colid, native_type(smallint)).
system_table(syscolumns, id, native_type(int)).
system_table(sysconstraints, colid, native_type(smallint)).
system_table(sysconstraints, constid, native_type(int)).
system_table(syscomments, id, native_type(int)).
system_table(syscomments, text, native_type(nvarchar(4000))).
system_table(syscomments, colid, native_type(smallint)).

system_table(table_constraints, constraint_catalog, native_type(nvarchar(128))).
system_table(table_constraints, constraint_schema, native_type(nvarchar(128))).
system_table(table_constraints, constraint_name, native_type(nvarchar(128))).
system_table(table_constraints, constraint_type, native_type(varchar(11))).
system_table(table_constraints, table_catalog, native_type(nvarchar(128))).
system_table(table_constraints, table_schema, native_type(nvarchar(128))).
system_table(table_constraints, table_name , native_type(nvarchar(128))).

system_table(key_column_usage, constraint_name, native_type(nvarchar(128))).
system_table(key_column_usage, table_catalog, native_type(nvarchar(128))).
system_table(key_column_usage, table_schema, native_type(nvarchar(128))).
system_table(key_column_usage, table_name, native_type(nvarchar(128))).
system_table(key_column_usage, column_name, native_type(nvarchar(128))).
system_table(key_column_usage, ordinal_position, native_type(int)).

system_table(referential_constraints, constraint_name, native_type(nvarchar(128))).
system_table(referential_constraints, constraint_catalog, native_type(nvarchar(128))).
system_table(referential_constraints, constraint_schema, native_type(nvarchar(128))).
system_table(referential_constraints, unique_constraint_name, native_type(nvarchar(128))).
system_table(referential_constraints, unique_constraint_catalog, native_type(nvarchar(128))).
system_table(referential_constraints, unique_constraint_schema, native_type(nvarchar(128))).


left_factor_types(Qid, In, T):-
        left_factor2(In, Left),
        resolve_factored_types(Qid, Left, T).

resolve_factored_types(_Qid, Var, Var):- var(Var), !.
resolve_factored_types(Qid, node(A, AS, Op, B, BS), T):- !,
        most_general_type(Qid, AS, BS, AT, B, Op, T),
        resolve_factored_types(Qid, A, AT).
resolve_factored_types(_Qid, T, T).

% Turn an LR node/3 tree into an LL node/3 tree
left_factor2(A, A):- var(A), !.
left_factor2(node(A, AS, Op, B, BS), Y):- !, left_factor_appending2(B, BS, A, AS, Op, Y).
left_factor2(A, A).
left_factor_appending2(Var, VarS, T, TS, Op, node(T, TS, Op, Var, VarS)):- var(Var), !.
left_factor_appending2(node(A, AS, Op1, B, BS), NS, T, TS, Op2, Z):- !, left_factor_appending2(B, BS, node(T, TS, Op2, A, AS), NS, Op1, Z).
left_factor_appending2(X, XS, Y, YS, Op, node(Y, YS, Op, X, XS)).



strip_sql_comments(Var, Var):- var(Var), !. % Frozen variables
strip_sql_comments(meta(_,_):A, B):- !,
        strip_sql_comments(A, B).

strip_sql_comments(A, B):-
        A =.. [Functor|Args], Args \== [], !,
        strip_sql_comments_list(Args, Args2),
        B =.. [Functor|Args2].

strip_sql_comments(A, A):- !.

strip_sql_comments_list([], []):- !.
strip_sql_comments_list([A|B], [C|D]):- !, strip_sql_comments(A, C), strip_sql_comments_list(B, D).

consolidate_errors(Var):-
        var(Var),
        throw(instantiation_error(Var)).
consolidate_errors(meta(_Comments, ErrorGroup):A):- !,
        ( ErrorGroup = {null} ->
            true
        ; otherwise->
            true %format(user_error, 'Found error: ~w~n', [ErrorGroup])
        ),
        consolidate_errors(A).
consolidate_errors(A):-
        A =.. [_|Args], Args \== [], !,
        consolidate_errors_list(Args).
consolidate_errors(_):- !.

consolidate_errors_list([]):- !.
consolidate_errors_list([A|B]):- !, consolidate_errors(A), consolidate_errors_list(B).


not_a_builtin_function(DBMS, Function):-
        strip_sql_comments(Function, identifier(Schema, Functor)),
        \+once(builtin_function(DBMS, Schema, Functor)).

builtin_function('Microsoft SQL Server', _, month).
builtin_function('Microsoft SQL Server', _, day).
builtin_function('Microsoft SQL Server', _, year).
builtin_function('Microsoft SQL Server', _, round).
builtin_function('Microsoft SQL Server', _, floor).
builtin_function('Microsoft SQL Server', _, ceiling).
builtin_function('Microsoft SQL Server', _, len).
builtin_function('Microsoft SQL Server', _, rtrim).
builtin_function('Microsoft SQL Server', _, str).
builtin_function('Microsoft SQL Server', _, datename).
builtin_function('Microsoft SQL Server', _, getdate).

builtin_function('Microsoft SQL Server', _, db_name).
builtin_function('Microsoft SQL Server', _, permissions).
builtin_function('Microsoft SQL Server', _, user_name).

routine_type(Name, Type):-
        % Note that this is not the DBMS we are reading IN, but the one we will eventually USE. This is why
        % I call default_schema here.
        default_schema(Schema),
        dbms(Schema, DBMS),
        strip_sql_comments(Name, identifier(_, Identifier)),
        ( cql_normalize_name(DBMS, Identifier, NormalizedName),
          routine_return_type(Schema, NormalizedName, Type),
          Type \== void->
            true
        ; otherwise->
            format(atom(Message), 'Could not determine the type of SQL function ~w', [Identifier]),
            throw(cql_error(cannot_determine_function_type, Message))
        ).

merge_sources(S, S, S):- !.
merge_sources(A, B, _):-
        format(atom(Message), '~w vs ~w', [A, B]),
        throw(cql_error(could_not_merge_sources, Message)).

type_mismatch(type_mismatch(Link, Type1, Type2), type_mismatch(Link, Type1, Type2), Type1, Type2):-
        % Note that just because the two unify does not mean that one of them is not already unified to a DIFFERENT mismatch of Type1 and Type2!
        % Therefore we have to do the flag/3 BEFORE the cut.
        flag(sql_error, Link, Link+1),
        !.

% This clause means that we will ignore subsequent errors which occur in the same place and just display the first one
type_mismatch(_, _, _, _):- !.

%type_mismatch(A, B, _, _):-
%        throw_exception(could_not_merge_type_errors, '~w vs ~w', [A, B]).


semantic_error(Error, Error, Level):-
        % Always add the error to the view tree, but only PRINT it if the level is lower than the gripe_level
        %sql_gripe_level(L),
        %Level =< L,
        !,
        format_sql_error(Error, _, Message),
        sql_gripe(Level, Message, []).

semantic_error(_, _, _).
