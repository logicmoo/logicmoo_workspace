/*  Part of SWI-Prolog

    Author:        Mike Elston
                   Matt Lilley
    E-mail:        matt.s.lilley@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2015, Mike Elston, Matt Lilley
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

:-module(cql,
         [
          cql_execute/1,
          cql_error/3,
          cql_data_type/10,
          cql_get_module_default_schema/2,
          cql_goal_expansion/3,
          cql_event_notification_table/2,
          cql_history_attribute/3,
          cql_identity/3,
          cql_odbc_select_statement/4,
          cql_odbc_state_change_statement/7,
          cql_portray/2,
          cql_var_check/1,
          cql_post_state_change_select_sql/4,
          cql_pre_state_change_select_sql/7,
          cql_runtime/7,
          cql_update_history_hook/14,
          cql_set_module_default_schema/1,
          cql_show/2,
          cql_state_change_statistics_sql/8,
          cql_statement_location/2,
          cql_temporary_column_name/4,
          cql_log/4,
          cql_normalize_name/3,
          cql_sql_clause/3,
          default_schema/1,
          odbc_execute_with_statistics/4,
          cql_access_token_to_user_id/2,
          dbms/2,
          odbc_data_type/4,
          attribute_domain/4,
          database_identity/3,
          database_key/5,
          primary_key_column_name/3,
          statistic_monitored_attribute/3,
          domain_database_data_type/2,
          database_attribute/8,
          database_domain/2,
          routine_return_type/3,
          database_constraint/4,
          in_line_format/4,
          row_count/2,
          sql_gripe/3,
          op(400, xfy, (::)),
          op(900, fy,  exists),
          op(750, yfx, *==),
          op(750, yfx, =*=),
          op(750, yfx, ==*),
          op(740, yfx, on),
          op(700, xfx, =~),
          op(700, xfx, \=~),
          op(200, fy, #),
          op(920, fy, ???),
          op(920, fy, ??),
          op(920, fy, ?)
	  ]).

:-license(swipl).

:-use_module(library(chr)).
:-use_module(library(dcg/basics)).
:-use_module(library(cql/sql_parser)).
:-use_module(library(cql/sql_tokenizer)).
:-use_module(library(cql/sql_write)).
:-use_module(library(cql/sql_keywords)).
:-use_module(library(cql/cql_database)).
:-reexport(cql_database, [register_database_connection_details/2,
                          cql_transaction/3]).


/** <module> CQL - Constraint Query Language

Note that CQL is currently in a state   of flux. Features may be dropped
in future releases, and the generated   SQL may change between releases.
In particular, _|runtime|_ mode is deprecated.

CQL is a Prolog interface to SQL databases. There are two modes: _|fully
compiled|_ and _|runtime|_. The _|fully compiled|_   mode should be used
if possible due to the far greater compile time checking it provides.

## Warnings from CQL {#cql-warnings}

### CQL Comparisons with NULL {#cql-compare-null}

CQLv2  correctly  compiles  equality  comparisons  with  NULL  into  the
appropriate expression at runtime. In CQLv1, executing

   ==
   A={null}, {[A], foo :: [a-A]}
   ==

would never succeed, regardless of the value of foo.a. This is no longer
the case: If A is `{null}` then this will execute as =|SELECT .... WHERE
a IS NULL|= and if A is  not   {null},  it will execute as =|SELECT ....
WHERE a = ?|=

See the section _|Removing null comparisions|_  for the dealing with the
common requirement to ignore comparisons with null.

### Avoid setof/3 and bagof/3 in CQL queries {#cql-and-setof}

It is generally not a good  idea  to   wrap  CQL  inside  a setof/3 or a
bagof/3 ... unless you are prepared  to   declare  all the CQL variables
that are neither bound nor mentioned in the setof/bagof template. If you
want to sort, use findall/3 followed by   sort/2. Note that sort/2 (like
setof/3) removes duplicates. If you don't want to remove duplicates, use
msort/2.

## CQL: Retrieved nulls have special logic to handle outer joins {#cql-null-outer-jpin}

In the course of executing a select query, the following rules are applied:

   1. Any selected attribute that is null does not bind its associated variable.
   2. Just before returning from the query any select variables that
      are still free are bound to {null}.

This is so we can handle outer joins.  Consider this:

  ==
  x :: [a-A] *== y :: [a-A]
  ==

Assume x.a binds A to a non-null value.   If there is no matching row in
`y`, then `y.a = null`. If variable `A` was truly shared the query could
never succeed. By not binding  the   variable  associated with `y.a` the
query can succeed ( rule 1) and `A` will be bound to the value in `x.a`.

## CQL: Getting Started Quickly {#cql-quickstart}

Here is a simple example of a SQL SELECT from the table `se_lt_x`

  ==
  test(A) :-
    format('About to call CQL with A=~w', [A]),
    {[],
    se_lt_x :: [a-A,
		b-B,
		c-C]},
    format('B=~w, C=~w', [B, C]).
  ==

   * The CQL is distinguished from the ordinary Prolog by appearing
     in curly brackets
   * Prolog variables which are ground when the CQL is executed will
     appear in the resulting SQL as part of the WHERE clause

Comparisons can be done in-line e.g.

  ==
  [a-'ELSTON_M']
  ==

or with the == operator e.g.

  ==
  [a-A], A == 'ELSTON_M'.
  ==

*|The single = operator means unify, not compare. Use = for unification,
not comparison|*

FIXME: Unification is deprecated.

The operators `=:=` and `\==`  are   also  available for numerical value
comparisons (they just translate to SQL  `=`   and  `<>`, so in fact you
could use them for string comparisons)


## Debugging CQL queries {#cql-debugging}

You can debug CQL using the meta-predicates ?/1, ??/2 and ???/3:

  ==
  ???{[], se_lt_x :: [a-A, b-_], A == 'ELSTON_M'}.
  ==

  $ ?/1 : Display a summary form of the generated SQL before and
  after the goal is called.

    ==
    [main]  CALL   SELECT slx_2.b, slx_2.a  FROM se_lt_x AS slx_2 WHERE slx_2.a = 'ELSTON_M'
    [main]  EXIT   SELECT slx_2.b, slx_2.a  FROM se_lt_x AS slx_2 WHERE slx_2.a = 'ELSTON_M' (0.006963s, 0.01cpu, 3,899 inferences)
    ==

  $ ??/1 : Display the exact query (and results) in a format which
  can be executed directly by the DBMS (In this case, SQL Server) The
  generated SQL may be significantly more complicated than expected, and
  this can be used to debug the CQL compiler itself

    ==
    [main]  CALL
    DECLARE @P0 VARCHAR(50);
    SET @P0 = 'ELSTON_M';
    SELECT slx_450.b,
	   slx_450.a
    FROM se_lt_x AS slx_450
    WHERE slx_450.a = @P0 AND slx_450.a COLLATE Latin1_General_CS_AS = @P0
    Result: se_lt_x.b = {null}
	    se_lt_x.a = 'ELSTON_M'
     (0.003304s, 0.00cpu, 359 inferences)
    ==


  $ ???/1 : Display simplified SQL before the goal is called and display
  the results afterwards

    ==
    [main]  CALL
    SELECT slx_450.b,
	   slx_450.a
    FROM se_lt_x AS slx_450
    WHERE slx_450.a = 'ELSTON_M'
    Result: se_lt_x.b = {null}
	    se_lt_x.a = 'ELSTON_M'
     (0.003304s, 0.00cpu, 359 inferences)
    ==


## Prolog Variables in CQL queries {#cql-prolog-vars}

A Prolog variable  can  be  simultaneously   a  _|SELECT|_  variable,  a
_|JOIN|_ variable and a _|WHERE|_ variable  as   A  is  in the following
example:

  ==
  {[],
   se_lt_x :: [a-A, c-C]
   =*=
   se_lt_y :: [d-A, f-F],
   A == 'A4'}
  ==

which generates the following SQL

  ==
  SELECT
    x_192.a, x_192.c, y_73.d, y_73.f
   FROM
    se_lt_x x_192 INNER JOIN se_lt_y y_73 ON y_73.d=x_192.a
   WHERE   x_192.a = ? and y_73.d = ?
  ==

Note how *|all the variables referenced in  the query|* are retrieved in
the SELECT. This is done to make   the query _|Prolog-like|_. This means
the retrieved row should behave like a Prolog  fact so that when a query
succeeds all the variables become instantiated.

There is one notable  exception  however:   *|WHERE  variables  and JOIN
variables are not bound in aggregation selections|*

FIXME: Is this still the case?

  ==
  sum_test :-
    {[],
     #se_lt_x :: [a-ValueA,
		  sum(b)-Summation]
     =*=
     #se_lt_y :: [e-ValueB],

     ValueA == ValueB,   % Explicit join point

     group_by([ValueA])},

    writeln(ValueA-ValueB-Summation).
  ==

  ==
  'ELSTON_M'-_G375971-99450
  true ;
  ==

## CQL Special Attributes {#cql-special-attrs}

The following attributes are automatically provided i.e if the attribute
is present in the table, CQL will automatically fill in the value:

   1. *|generation_|* Set to 0 on INSERT and incremented by 1 on each update

   2. *|inserted_|* Set to the current time at the time of the INSERT
      transaction

   3. *|inserted_by_|*  Set to the user ID corresponding to the access
      token supplied to the transaction

   4. *|updated_|* Set to the current time at the time of the UPDATE
      transaction. Note that updated_ is also set by an INSERT

   5. *|updated_by_|* Set to the user ID corresponding to the access
      token supplied to the transaction. Note that updated_by_ is also
      set by an INSERT

   6. *|transaction_id_|* Set to the transaction ID

All  the  special  attributes  can  be    overridden  by  supplying  the
attribute-value pair explicitly.

## CQL Examples {#cql-examples}

Rather than provide an abstract description of CQL syntax here is a set of
examples that show how to use it.

### CQL Simple INSERT {#cql-insert}

  ==
  {[],
   insert(se_lt_x, [a-'A', b-'B', c-100])}
  ==

### CQL Simple INSERT with retrieval of identity of the inserted {#cql-insert2}

==
{[],
 insert(se_lt_x, [a-'A', b-'B', c-100]),
 identity(I)}
==

### CQL Simple DELETE {#cql-delete}

  ==
  {[],
   delete(se_lt_x, [x_pk-I])}
  ==

Note that the WHERE clause is part  of the delete/2 term unlike _update_
where the WHERE clause is defined  outside   the  update/2 term. I could
have made delete consisent with update, but this would have required the
@ alias in the delete WHERE clause to  identify the table where the rows
are to be deleted). This seems  like   overkill  because a delete can in
fact refer to only one table  anyway   i.e.  you  can't identify rows to
delete via a JOIN.

### CQL Simple SELECT {#cql-select}

  ==
  {[],
   se_lt_x :: [a-A, b-B]}
  ==

This query will either:

   * If A is bound, and B are bound, fail if there are no such
     rows, or succeed (without binding anything) the same number of
     times as there are matching rows in se_lt_x.

   * If A is bound and B is unbound, bind B to each of the values in
     `se_lt_x.b` where `se_lt_x.a = A`

   * If B is bound and A is unbound, bind A to each of the values in
     `se_lt_x.a` where `se_lt_x.b = B`

   * If `A` and `B` are both unbound, bind `A` and `B` to each of
     the tuples in `se_lt_x`

### CQL Simple UPDATE {#cql-update}

  ==
  {[],
   update(se_lt_x, [c-100]),
   @ :: [a-'A1'],
   row_count(N)}
  ==

This corresponds to =|UPDATE se_lt_x   SET c=100 WHERE se_lt_x.a='A1'|=.
The '@' is a special alias referring to the table that is being updated.
The row_count/1 term gives the number or rows updated.

### CQL WHERE with arithmetic comparison {#cql-where-arith}

  ==
  {[],
   se_lt_x :: [a-A, c-C],
   C > 10}
  ==

### CQL Simple INNER JOIN {#cql-inner-join}

  ==
  {[],
   se_lt_x :: [a-J1, c-C]
    =*=
   se_lt_y :: [d-J1, f-F]}
  ==

The join is `se_lt_x.a = se_lt_y.d` because of the shared variable `J1`.
`se_lt_x.c` will be returned in `C` and  `se_lt_y.f` will be returned in
`F`

### CQL Arithmetic UPDATE with an INNER JOIN and a WHERE restriction {#cql-update-join-where}

==
{[],
 update(se_lt_x, [c-(C + 2 * F + 20)]),
 @ :: [a-A, c-C] =*= se_lt_y :: [d-A, f-F],
 C < 100}
==

This joins the table being updated  (table `se_lt_x`) on table `se_lt_y`
where `se_lt_x.a = se_lt_y.a` and where   `se_lt_x.c < 200` then updates
each identified row `se_lt_x.c` with the specified expression.

### CQL: Confirm row does not exist {#cql-not-exists}

  ==
  \+ exists {[], se_lt_x :: [a-'Z']}
  ==

### CQL: Aggregation - Count {#cql-count}

  ==
  {[],
   se_lt_x :: [count(c)-C]}
  ==

This will count the rows in table se_lt_x

### CQL: Aggregation - Sum {#cql-sum}

  ==
  {[],
   se_lt_x :: [sum(c)-C]}
  ==

Sum the values of attribute c in table se_lt_x

### CQL: Aggregation - Average {#cql-avg}

  ==
  {[],
   se_lt_x :: [avg(c)-C]}
  ==

Calculate the mean of the values of attribute c in table se_lt_x

### CQL: Maximum Value {#cql-max}

  ==
  {[],
   se_lt_x :: [max(c)-C]}
  ==

Calculate the maximum of the values of attribute c in table se_lt_x

### CQL: Minimum Value {#cql-min}

  ==
  {[],
   se_lt_x :: [min(c)-C]}
  ==

Calculate the minimum of the values of attribute c in table se_lt_x

### CQL: Aggregation requiring GROUP BY {#cql-group-by}

  ==
  {[],
   se_lt_z :: [g-G, sum(i)-I],
   group_by([G])}
  ==

This will generate the =|GROUP BY  SQL|=   and  sum `se_lt_z.i` for each
value of `se_lt_z.g`

### CQL: INNER JOIN with an aggregation sub-query where the sub-query is constrained by a shared variable from the main query {#cql-agg-subquery}

  ==
  {[],
   se_lt_x :: [b-J1, a-A]
     =*=
   se_lt_z :: [h-J1, i-I, g-Z],
   I > min(Y, se_lt_y :: [f-Y, d-Z])}
  ==

The main query and the sub-query share variable Z. The generated SQL is:

  ==
  SELECT
    x37.a, z4.i, z4.g
   FROM
    se_lt_x x37 INNER JOIN se_lt_z z4 ON x37.b=z4.h and z4.h=x37.b
   WHERE
    z4.i > (SELECT min(y11.f) FROM se_lt_y y11 WHERE z4.g=y11.d)
  ==

### CQL: INNER JOIN in an aggregation sub-query {#cql-join-subquery}

  ==
  {[],
   se_lt_y :: [d-D,f-F],
   F < sum(I,
	   se_lt_x :: [b-J1]
	     =*=
	   se_lt_z :: [h-J1, i-I])}
  ==

### CQL: Negation {#cql-negation}

  ==
  {[],
   se_lt_x :: [a-A, b-B],
   \+ exists se_lt_y :: [d-A]}
  ==

The generated SQL is:

  ==
  SELECT
    x39.a, x39.b
   FROM
    se_lt_x x39
   WHERE NOT EXISTS (SELECT * FROM se_lt_y y13 WHERE x39.a = y13.d)
  ==

### CQL: EXISTS {#cql-exists}

An exists restriction translates to a =|WHERE|= sub-query and is used to
say that "each row  returned  in  the   main  query  must  satisfy  some
condition expressed by another query".

*Example*

  ==
  {[],
   se_lt_x :: [a-A, b-B],
   exists se_lt_y :: [d-A]}
  ==

compiles to:

  ==
  SELECT
    x.b, x.a
  FROM
    se_lt_x x
  WHERE
    EXISTS (SELECT * FROM se_lt_y WHERE x.a = y.d)
  ==

### CQL: Left Outer Join {#cql-left-outer-join}

  ==
  se_lt_x :: [a-J1, b-B]
    *==
  se_lt_y :: [d-J1, e-E]}
  ==

### CQL: List-based Restrictions {#cql-list-restrictions}

CQL supports query restrictions based on lists.  Note that in both cases
*|\== [] and  ==  []  are   equivalent|*  despite  the  obvious  logical
inconsistency.

FIXME: Can we make this behaviour be controlled by a flag? It IS quite useful, even if it is completely illogical

  ==
  {[], se_lt_x :: [a-Bar], Bar == []}
  ==

and

  ==
  {[], se_lt_x :: [a-Bar], Bar \== []}
  ==

both do *exactly* the same thing  -   they  will  not restrict the query
based on Bar. The second case  seems   to  be logically consistent - all
things are not in the empty list.

### CQL: Compile time in-list constraint {#cql-list-compilation}

If your list is bound at compile-time,  you   can  simply  use it as the
attribute value in CQL, for example:

  ==
  {[], se_lt_x :: [a-['ELSTON_M', 'LILLEY_N']]}
  ==

This does not require the  list  to   be  *ground*,  merely *bound*. For
example, this is not precluded:

  ==
  foo(V1, V2):-
      {[], se_lt_x :: [a-[V1, V2]]}.
  ==

If, however, your list is not bound   at compile-time, you must wrap the
variable in list/1:

  ==
  Bar = [a,b,c],
  {[], se_lt_x :: [bar-list(Bar)]}
  ==

If you write

  ==
  foo(V1):-
      {[], se_lt_x :: [a-V1]}.
  ==

and at runtime call foo([value1]), you will get a type error.

_|Remember:|_ If the list of IN values is _empty_ then no restriction is
generated i.e.

  ==
  {[], se_lt_x :: [a-[], b-B}

  is the exactly the same as

  {[], se_lt_x :: [b-B}
  ==

### CQL: Disjunction resulting in OR in WHERE clause {#cql-disjunction}

  ==
  {[],
   se_lt_x :: [a-A, b-B, c-C],
   (C == 10 ; B == 'B2', C < 4)}
  ==

The generated SQL is:

  ==
  SELECT
    x.a, x.b, x.c
   FROM
    se_lt_x x
   WHERE
    ((x.b = ? AND x.c < ?) OR x.c = ?)
  ==

### CQL: Disjunction resulting in different joins (implemented as a SQL UNION) {#cql-disj-diff-joins}

  ==
  {[],
   se_lt_x :: [a-A, c-C]
   =*=
   (se_lt_y :: [d-A] ; se_lt_z :: [g-A])}
  ==

The generated SQL is:

  ==
  SELECT
    x43.c
   FROM
    (se_lt_x x43 INNER JOIN se_lt_z z6 ON x43.a=z6.g AND z6.g=x43.a)

  UNION

  SELECT
    x44.c
   FROM
    (se_lt_x x44 INNER JOIN se_lt_y y16 ON x44.a=y16.d AND y16.d=x44.a)
  ==

### CQL: Disjunction resulting in different SELECT attributes (implemented as separate ODBC queries) {#cql-disj-attrs}

  ==
  {[],
   (se_lt_x :: [a-A, c-10]
   ;
   se_lt_y :: [d-A, f-25])}
  ==

The output variable  A  is  bound  to   the  value  from  two  different
attributes and so the query is implemented as two separate ODBC queries

### CQL: ORDER BY {#cql-order-by}

==
{[],
 se_lt_z :: [g-G, h-H],
 order_by([-G])}
==

The order_by specification is a list  of "signed" variables. The example
above will order by se_lt_z.g descending

### CQL: DISTINCT {#cql-distinct}

Use distinct(ListOfVars) to specify which  attributes   you  want  to be
distinct:

  ==
  test_distinct :-
    findall(UserName,
	    {[],
	     se_lt_x :: [a-UserName,
			 c-Key],
	     Key >= 7,
	     distinct([UserName])},
	    L),
    length(L, N),
    format('~w solutions~n', [N]).

  CALL  : user:test_distinct/0
  26 solutions
  EXIT  : user:test_distinct/0 (0.098133s, 0.00cpu, 1,488 inferences)
  ==


### CQL: SELECT with NOT NULL restriction {#cql-select-not-null}

  ==
  {[],
   se_lt_z :: [i-I, j-J],
   J \== {null}}
  ==

### CQL: First N {#cql-top}

  ==
  {[],
   N = 3,
   se_lt_z :: [i-I],
   top(N),
   order_by([+I])}
  ==

This generates a TOP  clause  in  SQL   Server,  and  LIMIT  clauses for
PostgreSQL and SQLite


### CQL: Self JOIN {#cql-self-join}

  ==
  {[],
   se_lt_z :: [h-H, i-I1]
    =*=
   se_lt_z :: [h-H, i-I2],
   I1 \== I2}
  ==


### CQL: Removing null comparisions {#cql-no-null-compare}

Use the ignore_if_null wrapper in your CQL   to  'filter out' null input
values. This is a useful extension for creating user-designed searches.

  ==
  {[],
   se_lt_x :: [a-UserName,
	       b-ignore_if_null(SearchKey),
	       ...]}
  ==

At runtime, if SearchKey is bound to a  value other than {null} then the
query will contain =|WHERE ... b =  ?|=. If, however, SearchKey is bound
to `{null}`, then this comparison will be omitted.

*Disjunctions*

In general, don't use  ignore_if_null   in  disjunctions.  Consider this
query:

  ==
  SearchKey = '%ELSTON%',
  {[],
   se_lt_x :: [a-UserName,
	       b-RealName],
   ( RealName =~ SearchKey
   ; UserName =~ SearchKey)}
  ==

The query means "find a user where   the UserName contains ELSTON OR the
RealName contain ELSTON". If !SearchKey is {null} then RealName=~ {null}
will fail, which is correct. If ignore_if_null  was used, the test would
_succeed_, which means the disjunction  would   always  succeed i.e. the
query would contain no restriction, which   is  clearly not the intended
result. FIXME: Mike, what is this all about?

### CQL: Three table JOIN {#cql-three-table-join}

  ==
  {[],
   se_lt_x :: [a-A, c-C]
    =*=
   se_lt_y :: [d-A, f-F]
    =*=
   se_lt_z :: [i-F, g-G]}
  ==

The shared variable A joins `se_lt_x` and `se_lt_y`; the shared variable
`F` joins `se_lt_y` and `se_lt_z`

### CQL: Three table JOIN with NOLOCK locking hint {#cql-three-nolock}

  ==
  {[],
   se_lt_x :: [a-A, c-C]
    =*=
   #se_lt_y :: [d-A, f-F]
    =*=
   #se_lt_z :: [i-F, g-G]}
  ==

The hash operator indicates the table that should be accessed WITH (NOLOCK)

### CQL: SELECT with LIKE {#cql-like}

  ==
  {[],
   se_lt_z :: [g-G, i-I],
   G =~ 'A_'}
  ==

The operator =~ means LIKE. If you are using PostgreSQL, it means ILIKE.

### CQL: Writing exceptions directly to the database {#cql-write}

You can write an exception term directly to a varchar-type column in the
database. Note that it will be rendered  as text using ~p, and truncated
if necessary - so you certainly can't read   it  out again and expect to
get an exception! Example code:

  ==
  catch(process_message(Message),
	Exception,
	{[],
	update(some_table, [status-'ERROR',
			    status_comment-Exception]),
	@ :: [some_table_primary_key-PrimaryKey]}).
  ==

FIXME: This code is specific to my usage of CQL

### CQL: TOP N is Parametric {#cql-top-n-parametric}

You can pass the  "N"  is  TOP  N   as  a  parameter  (Subject  to  DBMS
compatibility. This works in SQL Server 2005 and later, and PostgreSQL 9
(possibly earlier versions) and SQLite3.

  ==
  N = 3,
  findall(I,
	  {[],
	   se_lt_z :: [i-I], top(N), order_by([+I])},
	  L)
  ==


### CQL: Using compile_time_goal/1 {#cql-compile-time-goal}

You can include compile_time_goal(Goal) in your   CQL.  If you specify a
module, it will be used,  otherwise  the   goal  will  be  called in the
current module. Note that the goal is executed in-order - if you want to
use the bindings in your CQL, you  must put the compile_time_goal before
them.

*|Example 1|*

  ==
  {[],
   se_lt_x :: [a-UserName,
	       b-RealName,
	       d-FavouriteColour],
     compile_time_goal(standard_batch_size_for_search(StandardBatchSize)),
     top(StandardBatchSize),
     order_by([+UserName]}
  ==

*|Example 2|*

  ==
  excellent_colours(['RED', 'BLUE']).

  {[],
   se_lt_x :: [a-UserName,
	       b-RealName,
	       d-FavouriteColour],
   compile_time_goal(excellent_colours(Colours)),
   FavouriteColour == Colours}
  ==

### CQL: ON {#cql-on}

CQL supports both constant and shared variable join specifications. This
is particularly useful when specifying outer joins.

*Example*

  ==
  {[],
   se_lt_x :: [a-UserNameA,
	       b-RealName,
	       d-FavouriteColour]
   *==
   se_lt_x :: [a-UserNameB,
	       e-FavouriteFood] on( UserNameA == UserNameB,
				    FavouriteColour == FavouriteFood,
				    FavouriteFood == 'ORANGE')}
  ==

All the CQL comparison operators, =|<, =<, ==, =~, \=~, \==, >=, >|= can
be used in ON specifications.

For example:

  ==
  {[],
   se_lt_z :: [i-J1, k-K]
   *==
   se_lt_x :: [c-J1, a-A, b-B] on A \== 'A1'},
  ==

### CQL: Expressions In Where Restrictions {#cql-expr-in-where}

Expressions in WHERE restrictions are supported, for example:

  ==
  {[],
   se_lt_n :: [i-I, j-J, k-K],
   J > 10 * (K / I) + 15},
  ==


### CQL: Explicitly avoid the "No WHERE restriction" message {#cql-no-where}

To avoid accidentally deleting or  updating  all   rows  in  a table CQL
raises an exception if there is no WHERE restriction.

Sometimes however you really do need to delete   or update all rows in a
table.

To support this requirement in  a  disciplined   way  (and  to avoid the
creation    of    "dummy"    WHERE      restrictions)     the    keyword
*absence_of_where_restriction_is_deliberate*   has   been   added.   For
example:

  ==
  {[],
   update(se_lt_x, [c-10]),
	  @ :: [],
	  absence_of_where_restriction_is_deliberate}
  ==

### CQL: HAVING {#cql-having}

HAVING restrictions can be specified.  For example:

  ==
  {[],
   se_lt_z :: [sum(i)-I,
	       g-G],
   group_by([G]),
   having(I > 30)}
  ==

For         a         description         of          HAVING         see
http://en.wikipedia.org/wiki/Having_(SQL)

There is one important  difference  between   SQL  HAVING  and SQL WHERE
clauses. The SQL WHERE clause  condition   is  tested against *|each and
every|* row of data, while the  SQL   HAVING  clause condition is tested
against the _|groups and/or aggregates  specified   in  the SQL GROUP BY
clause and/or the SQL SELECT column list|_.


### CQL: INSERT and UPDATE value in-line formatting {#cql-inst-update-value}

INSERT and UPDATE values can be formatted in-line at runtime.  For example:

  ==
  Suffix = 'NOGG',
  cql_transaction(Schema, UserId,
		  {[],
		  insert(se_lt_x, [a-'A', b-'B', c-100, d-format('EGG_~w', [Suffix])])}),
  ==

will insert 'EGG_NOGG' into attribute 'd'.

### CQL: Negations in WHERE Clauses {#cql-neg-where}

You can specify negations in CQL WHERE clauses e.g.

  ==
  {[],
   se_lt_z :: [g-G, h-H, i-I],
   \+((G == 'A1', H == 'B1' ; G == 'D1', H == 'B3'))},
  ==

Note that, just like  in  Prolog,  \+   is  a  unary  operator hence the
"double" brackets in the example above.

### CQL: Predicate-generated Attribute Values {#cql-gen-att-values}

It  is  possible  to  generate  *|compile  time|*  attribute  values  by
specifying a _predicate_ which is  executed   when  the CQL statement is
compiled.

The predicate must return the value you   want as its last argument. You
specify the predicate where you would  normally put the attribute value.
The predicate is specified _with its output argument missing_.

*Example* - Using domain allowed values in a query.

In the following CQL statement  the predicate cql_domain_allowed_value/3
is called within findall/3 *|at compile  time|*   to  generate a list of
domain values that restrict favourite_colour to be 'ORANGE' or 'PINK' or
'BLUE', or 'GREEN'.

  ==
  colour('ORANGE').
  colour('PINK').
  colour('BLUE').
  colour('GREEN').

  {[],
   se_lt_x :: [d-findall(Value,
			 permissible_colour(Value)),
	       a-UserName]}
  ==

Note how findall/3 is actually called by specifying findall/2.

There is not much point using   predicate-generated  attribute values in
compile-at-runtime CQL as you can always  call the predicate to generate
the required values _outside_ the CQL statement.

### CQL: INSERT from SELECT {#cql-insert-from-select}

INSERT from SELECT is supported:

  ==
  Constant = 'MIKE',
  {[],
   insert(se_lt_x1, [x_pk-Pk, a-A, b-B, c-C, d-Constant]),
   se_lt_x :: [x_pk-Pk, a-A, b-B, c-C, as(d)-Constant]}
  ==

which generates the following SQL:

  ==
  INSERT INTO se_lt_x1 (x_pk, a, b, c, d)
  SELECT se_lt_x_955.x_pk, se_lt_x_955.a, se_lt_x_955.b, se_lt_x_955.c, ? AS d
    FROM se_lt_x lt_x_955
  ==

Note the use of the `as(d)` construct in   the SELECT part of the CQL to
make the constant *|'MIKE'|* appear to come from the SELECT thus setting
`lt_x1.d` to *|'MIKE'|* in every row inserted.

## CQL: Hooks {#cql-hooks}

CQL provides a large number of hooks   to  fine-tune behaviour and allow
for customization. These are:

### CQL: Generated Code Hooks {#cql-code-hooks}

   * cql:cql_dependency_hook(+EntitySet, +Module) can be defined to be
     notified when a given Module references a list of database
     entities. This can be used to manage metadata/code dependency

   * cql:cql_generated_sql_hook(+Filename, +LineNumber, +Goals) can be
     defined to examine generated SQL. Use cql_sql_clause(+Goals, -SQL,
     -Parameters) to examine the goals

   * cql:cql_index_suggestion_hook(+Index) can be defined if you are
     interested in proposed indices for your schema. Note that this is
     not very mature (yet)

### CQL: Data Representation Hooks {#cql-represent-hooks}

   * cql:cql_atomic_value_check_hook(+Value) can be defined to declare
     new 'atomic' types (That is, types which can be written directly to
     the database), such as a representation like boolean(true) for 1.

   * cql:cql_check_value_hook(+Value) can be used to check that a value
     is legal

   * cql:application_value_to_odbc_value_hook(+OdbcDataType, +Schema,
     +TableName, +ColumnName, +Qualifiers, +ApplicationValue,
     -OdbcValue).

   * cql:odbc_value_to_application_value_hook(+OdbcDataType, +Schema,
     +TableName, +ColumnName, +Domain, +OdbcValue, -ApplicationValue).

### CQL: Application Integration {#cql-appl-hooks}

   * cql:cql_access_token_hook(+AccessToken, -UserId) can be defined to
     map the generic 'AccessToken' passed to cql_transaction/3 to a user
     ID. If not defined, the AccessToken is assumed to be the user ID.
     This UserID is used in logging.

   * cql:log_selects can be defined if you want to receive logging
     information about selects. By default only update, delete and
     insert are logged

   * cql:cql_execution_hook(+Statement, +OdbcParameters,
     +OdbcParameterDataTypes, -Row) can be defined if you want to
     implement the exeuction yourself (for example, to add extra
     debugging)

   * cql:cql_log_hook(+Topics, +Level, +Format, +Args) can be defined
     to redirect CQL logging.
      * Levels is one of informational, warning, or error
      * Topics is a list of topics. Currently the only lists possible
        are [] and [debug(deadlocks)]

   * cql:sql_gripe_hook(+Level, +Format, +Args) is called when
     suspect SQL is found by the SQL parser

   * cql:cql_normalize_atom_hook(+DBMS, +ApplciationAtom, -DBMSAtom)
     can be used to create a map for atoms in a specific DBMS. For
     example, your schema may have arbitrarily long table names, but
     your DBMS may only allow names up to 64 bytes long. In this case,
     you can create a scheme for mapping the application-level atom to
     the DBMS. Other uses include deleting or normalizing illegal
     characters in names

   * cql:cql_error_hook(+ErrorId, +Format, +Args) can be defined to
     generate a specific exception term from the given arguments. If not
     defined (or if it does not throw an exception, or fails), you will
     get cql_error(ErrorId, FormattedMessage).

   * cql:cql_max_db_connections_hook(-Max)
     can be defined to limit the number of simultaneous connections each
     thread will attempt to have

   * cql:odbc_connection_complete_hook(+Schema, +Details, +Connection)
     can be hooked if you want to know every time a connection is made

   * cql:cql_transaction_info_hook(+AccessToken, +Connection, +DBMS, +Goal, -Info)
     can be defined if you want to define any application-defined
     information on a per-transaction level. This can be recovered via
     database_transaction_query_info(?ThreadId, ?Goal, ?Info).

### CQL: Inline values {#cql-inline-hooks}

   * cql:cql_inline_domain_value_hook(+DomainName, +Value)
     can be defined if you want the given value to be 'inlined' into the
     CQL (ie not supplied as a parameter). Great care must be taken to
     avoid SQL injection attacks if this is used.

### CQL: Schema {#cql-shema-hook}

These define the  schema.  You  MUST   either  define  them,  or include
library(cql/cql_autoschema) and add two directives   to build the schema
automatically:

   * :-register_database_connection_details(+Schema, +ConnectionInfo).
   * :-build_schema(+Schema).

Otherwise,  you  need  to  define   at  least  cql:default_schema/1  and
cql:dbms/2, and then as many  of  the   other  facts  as needed for your
schema.

   * cql:default_schema(-Schema)
     MUST be defined. CQL autoschema will define this for you if you use
     it.

   * cql:dbms(+Schema, -DBMS)
   MUST be defined for every schema you use. CQL autoschema will define
   this for you if you use it. DBMS must be one of the following:

      * 'Microsoft SQL Server'
      * 'PostgreSQL'
      * 'SQLite'

   * cql:odbc_data_type(+Schema, +TableName, +ColumnName, +OdbcDataType).
   * cql:primary_column_name(+Schema, +Tablename, +ColumnName).
   * cql:database_attribute(+EntityType:table/view, +Schema:atom, +EntityName:atom, +ColumnName:atom, +DomainOrNativeType:atom, +AllowsNulls:allows_nulls(true/false), +IsIdentity:is_identity(true/false), +ColumnDefault).
   * cql:database_domain(+DomainName, +OdbcDataType).
   * cql:routine_return_type(+Schema, +RoutineName, +OdbcDataType).
   * cql:database_constraint(+Schema, +EntityName, +ConstraintName, +Constraint).

### CQL: Event Processing and History {#cql-events-and-history}

CQL provides hooks for  maintaining  detailed   history  of  data in the
database.

The hook predicates are:

   * cql:cql_event_notification_table(+Schema, +TableName)
   * cql:cql_history_attribute(+Schema, +TableName, +ColumnName)
   * cql:cql_update_history_hook(+Schema, +TableName, +ColumnName, +PrimaryKeyColumnName, +PrimaryKeyValue, +ApplicationValueBefore, +ApplicationValueAfter, +AccessToken, +TransactionId, +TransactionTimestamp, +ThreadId, +Connection, +Goal).
   * cql:process_database_events(+Events)

Event  Processing  and  History  recording  can   be  suppressed  for  a
particular   update/insert/delete   statement    by     including    the
_no_state_change_actions_9 directive.

For example

  ==
  {[],
   update(se_lt_x, [f-'LILAC']
   @ :: [a-'ELSTON_M'],
   no_state_change_actions,   % Don't want history to record this change
   row_count(RowCount)}
  ==

### CQL: Statistical Hooks {#cql-statistics-hooks}

CQL has hooks to enable in-memory statistics  to be tracked for database
tables. Using this hook, it's possible to  monitor the number of rows in
a table with a particular value in a particular column.

Often the kind of statistics of  interest   are  'how  many rows in this
table are in ERROR' or 'how many in this table are at NEW'? While it may
be possible to maintain these directly in any code which updates tables,
it can be difficult to ensure all  cases are accounted for, and requires
developers to remember which attributes are tracked.

To ensure that all (CQL-originated) updates   to  statuses are captured,
it's possible to use the CQL hook   system to update them automatically.
Define add a fact like:

  ==
  cql_statistic_monitored_attribute_hook(my_schema, my_table,
					 my_table_status_column).
  ==

This will examine the domain   for  the column 'my_table_status_column',
and       generate       a       statistic       for       each       of
my_table::my_table_status_column(xxx),  where  xxx  is    each  possible
allowed value for the domain. Code   will  be automatically generated to
trap updates to this specific column, and  maintain the state. This way,
if you are interested in the number  of   rows  in my_table which have a
status       of       'NEW',        you         can        look       at
my_table::my_table_status_column('NEW'), without having to   manage  the
state directly. CQL update  statements  which   affect  the  status will
automatically maintain the statistics.

The calculations are vastly simpler than the history mechanism, so as to
keep performance as high as possible. For   inserts, there is no cost to
monitoring the table (the insert simply  increments the statistic if the
transaction completes). For deletes, the delete query  is first run as a
select, aggregating on the  monitored  columns   to  find  the number of
deletes for each domain allowed  value.  This   means  that  a delete of
millions of rows might requires a select returning only a single row for
statistics purposes. For updates,  the  delete   code  is  run, then the
insert calculation is done, multiplied by the number of rows affected by
the update.

In       all       cases,       CQL         ends        up       calling
cql_statistic_monitored_attribute_change_hook/5, where the last argument
is a signed value indicating the number   of  changes to that particular
statistic.
*/

:-chr_option(line_numbers, on).
:-chr_option(check_guard_bindings, error).
:-chr_option(debug, off).
:-chr_option(optimize, full).
:-chr_option(guard_simplification, off). % Added to stop trail overflowing

:-chr_type list(T) ---> [] ; [T|list(T)].

:-chr_type 'AggregationOperator' ---> count ; max ; min ; avg ; sum.
:-chr_type 'AggregationVariable' == any.
:-chr_type 'ApplicationValue' == any.
:-chr_type 'DebugMode' ---> explicit.
:-chr_type 'DictinctionType' ---> no_distinction ; distinct_on_specified_attributes ; distinct_on_all_select_attributes.
:-chr_type 'Dsn' == any.
:-chr_type 'Attribute' ---> attribute('Schema', 'TableAlias', 'AttributeName').
:-chr_type 'AttributeName' == any.
:-chr_type 'AttributeNameValuePair' ---> 'AttributeName'-'ApplicationValue'.
:-chr_type 'BooleanOperator' ---> and ; or.
:-chr_type 'ComparisonOperator' ---> < ; =< ; == ; \== ; >= ; > ; (=~) ; (=\=) ; (=:=).
:-chr_type 'CompilationInstruction' ---> if_var('Variable') ; if_not_var('Variable') ; if_null('Variable') ; if_not_null('Variable') ; list('Variable') ; empty('Variable') ; not_empty('Variable') ; compile ; and('CompilationInstruction', 'CompilationInstruction').
:-chr_type 'CompileMode' ---> runtime ; compiletime.
:-chr_type 'ConjunctionGoal' == any.
:-chr_type 'ConjunctionVariable' == any.
:-chr_type 'Connection' == any.
:-chr_type 'Constraints' == any.
:-chr_type 'ClockTime' == any.
:-chr_type 'CpuTime' == any.
:-chr_type 'Cql' == any.
:-chr_type 'Disposition' ---> top ; where ; having ; join.
:-chr_type 'EqualityRestrictionVariableUsed' ---> equality_restriction_variable_used.
:-chr_type 'Expression' == any.
:-chr_type 'ExternalVariable' == any.
:-chr_type 'FileName' == any.
:-chr_type 'Format' == any.
:-chr_type 'FormatArg' == any.
:-chr_type 'Goal' == any.
:-chr_type 'Having' == any.
:-chr_type 'Identity' == int.
:-chr_type 'Inferences' == any.
:-chr_type 'InputVariable' == 'Variable'.
:-chr_type 'Join' == any.
:-chr_type 'JoinTreeNode' == any.
:-chr_type 'JoinType' ---> 'INNER JOIN' ; 'LEFT OUTER JOIN' ; 'RIGHT OUTER JOIN'.
:-chr_type 'Keep' ---> 1.
:-chr_type 'LineNumber' == int.
:-chr_type 'N' == int.
:-chr_type 'OdbcCachingOption' ---> do_not_cache_odbc_statement ; cache_odbc_statement.
:-chr_type 'OdbcDataType' ---> varchar(int) ; decimal(int, int) ; timestamp ; integer ; bit.
:-chr_type 'LogicalType' ---> varchar ; decimal ; timestamp ; integer ; boolean.
:-chr_type 'OdbcInput' == any.
:-chr_type 'OdbcOutput' == any.
:-chr_type 'OdbcParameter' ---> odbc_parameter('Schema', 'TableName', 'AttributeName', 'ApplicationValue', 'OdbcParameterUse', 'OdbcDataType')   % OdbcDataType unbound if no override required
                              ; odbc_explicit_type_parameter('OdbcDataType', 'ApplicationValue', 'OdbcParameterUse').
:-chr_type 'OdbcParameterUse' ---> insert_value ; update_value ; evaluated_update_attribute ; evaluated_update_parameter ; where_value ; top_value.
:-chr_type 'On' == any.
:-chr_type 'OrderBy' ---> +('Variable') ; -('Variable').
:-chr_type 'Output' ---> output('Schema', 'TableName', 'AttributeName', 'Variable') ;
                         ignore_output ;
                         count('Variable') ;
                         avg('Variable') ;  % PostgreSQL
                         selection_constant('Schema', 'TableName', 'AttributeName', 'Variable').
:-chr_type 'Phase' ---> initial ; distinct ; top ; select_attributes ; from ; where ; group_by ; having ; order_by ; union ; limit.
:-chr_type 'PreparedStatement' == any.
:-chr_type 'PrimaryKeyAttributeName' == 'AttributeName'.
:-chr_type 'PrimaryKeyValue' == any.
:-chr_type 'QueryId' == any.
:-chr_type 'QueryLevel' ---> top_level_query ; sub_query.
:-chr_type 'Reason' == any.
:-chr_type 'ResultSpec' == any.
:-chr_type 'Resolved' ---> resolved.
:-chr_type 'RestrictionExpression' == any.
:-chr_type 'RestrictionType' ---> where ; having ; join.
:-chr_type 'RestrictionTree' ---> true ;
                                  comparison('ApplicationValue', 'ComparisonOperator', 'ApplicationValue') ;
                                  sub_query('SubQueryType', list('SqlToken'), 'Tail', list('OdbcParameter')) ;
                                  and('RestrictionTree', 'RestrictionTree') ;
                                  or('RestrictionTree', 'RestrictionTree').
:-chr_type 'Row' == any.
:-chr_type 'StateChangeType' ---> insert ; update ; delete.
:-chr_type 'QueryType' ---> insert ; update ; delete ; select.
:-chr_type 'Schema' == any.
:-chr_type 'SelectAttribute' ---> select_attribute('SelectBindingType', 'Schema', 'TableName', 'TableAlias', 'AttributeName').
:-chr_type 'SelectAttributeWithSize' ---> 'Size'-'SelectAttributeInfo'.
:-chr_type 'SelectAttributeInfo' ---> select_info('CompilationInstruction', list('SqlToken'), 'Tail', 'Output').
:-chr_type 'SelectAttributeVariableUsed' ---> select_attribute_variable_used.
:-chr_type 'SelectBindingType' ---> plain ; aggregation('AggregationOperator').
:-chr_type 'SelectionType' ---> aggregation_selection ; non_aggregation_selection.
:-chr_type 'Side' ---> lhs ; rhs.
:-chr_type 'Size' == any.
:-chr_type 'SqlToken' == any.
:-chr_type 'SqlComparisonOperator' ---> < ; <= ; = ; <> ; >= ; > .
:-chr_type 'SubQueryType' ---> exists ; \+ .
:-chr_type 'Tail' == any.
:-chr_type 'TableAlias' == any.
:-chr_type 'TableName' == any.
:-chr_type 'Variable' == any.
:-chr_type 'VariablePair' ---> 'Variable'-'Variable'.
:-chr_type 'When' ---> pre_state_change ; post_state_change.



:-chr_constraint absence_of_where_restriction_is_deliberate.
:-chr_constraint add_on(-'Join', ?'On').
:-chr_constraint aggregation_sub_query(-'QueryId', ?'TableName', ?'TableAlias', ?list('SqlToken'), ?'Tail', ?list('ApplicationValue')).
:-chr_constraint aggregation_variable(-'QueryId', +'AggregationOperator', ?'AggregationVariable').
:-chr_constraint attribute_binding(-'QueryId', ?'Attribute', ?'ApplicationValue').
:-chr_constraint attribute_for_group_by(-'QueryId', ?'TableAlias', +'AttributeName', ?'Variable').
:-chr_constraint attribute_for_order_by(-'QueryId', ?'TableAlias', +'AttributeName', ?'Variable').
:-chr_constraint attribute_to_check(+'Schema', +'TableName', ?'AttributeNameValuePair').
:-chr_constraint attributes_to_check(-'QueryId', +'Schema', +'TableName', ?list('AttributeNameValuePair')).
:-chr_constraint call_history_hook(-'QueryId', +'Connection').
:-chr_constraint call_row_change_hooks(-'QueryId', +'Connection').
:-chr_constraint check_for_orphan_distincts.
:-chr_constraint check_for_orphan_group_bys.
:-chr_constraint check_for_orphan_order_bys.
:-chr_constraint check_for_orphan_select_attributes_in_aggregations.
:-chr_constraint check_for_orphan_select_variables_in_updates.
:-chr_constraint check_for_top_without_order_by.
:-chr_constraint check_for_unjoined_tables.
:-chr_constraint check_query.
:-chr_constraint cleanup_compile.
:-chr_constraint cleanup_cql_post_state_change_select_sql(-'QueryId').
:-chr_constraint collect_indices(-'QueryId').
:-chr_constraint collect_runtime_constraints(?'Constraints').
:-chr_constraint collect_select_attributes(-'QueryId', +list('SelectAttributeWithSize')).
:-chr_constraint comparison(-'QueryId', ?'ApplicationValue', +'ComparisonOperator', ?'ApplicationValue').
:-chr_constraint compile_mode(+'CompileMode').
:-chr_constraint conjunction_constraints(?'Constraints').
:-chr_constraint conjunction_goal(?'ConjunctionGoal').
:-chr_constraint conjunction_variable(-'QueryId', ?'ExternalVariable', ?'ConjunctionVariable').
:-chr_constraint copy_of_from(-'QueryId', ?list('SqlToken'), ?'Tail', ?list('OdbcParameter')).
:-chr_constraint find_copy_of_from(-'QueryId', ?list('SqlToken'), ?'Tail', ?list('OdbcParameter')).
:-chr_constraint cql2_variable(-'QueryId', -'Variable', ?'RestrictionTree').
:-chr_constraint cql_execute(+'OdbcCachingOption').
:-chr_constraint cql_fully_compiled.
:-chr_constraint cql_identity(-'QueryId', +'Schema', ?'Identity').
:-chr_constraint cql_odbc_select_statement(+'Schema', +'SqlToken', ?list('OdbcParameter'), ?list('Output')).
:-chr_constraint cql_odbc_state_change_statement(-'QueryId', +'StateChangeType', +'Schema', +'TableName', +'SqlToken', ?list('OdbcParameter'), ?list('Output')).
:-chr_constraint cql_post_state_change_select_sql(-'QueryId', +list('AttributeName'), +'OdbcDataType', +'SqlToken').
:-chr_constraint cql_pre_state_change_select_sql(-'QueryId', +'Schema', +'TableName', +'AttributeName', +'SqlToken', +list('AttributeName'), +list('OdbcParameter')).
:-chr_constraint cql_state_change_statistics_sql(-'QueryId', +'Schema', +'TableName',  +'StateChangeType', +'SqlToken', +list('AttributeName'), ?list('OdbcParameter'), ?list('OdbcParameter')).
:-chr_constraint cql_statement_location(+'FileName', +'LineNumber').
:-chr_constraint create_cql_pre_state_change_select_sql(-'QueryId', +'StateChangeType', +list('SqlToken'), +'TableName', ?list('OdbcParameter')).
:-chr_constraint create_cql_state_change_statistics_sql(-'QueryId', +'StateChangeType', +list('SqlToken'), +'TableName', ?list('OdbcParameter')).
:-chr_constraint create_in_line_joins.
:-chr_constraint create_join_points.
:-chr_constraint create_restrictions.
:-chr_constraint create_select_bindings.
:-chr_constraint debug_after(+'Reason', ?'ResultSpec').
:-chr_constraint debug_before(+'Format', +'Schema', +'FormatArg').
:-chr_constraint debug_statistics(+'CpuTime', +'ClockTime', +'Inferences').
:-chr_constraint delete_row(-'QueryId', +'TableName', ?'TableAlias').
:-chr_constraint determine_select_distinction(-'QueryId').
:-chr_constraint determine_select_distinctions.
:-chr_constraint determine_selection_type.
:-chr_constraint dictionary_addendum(-'QueryId', ?'ExternalVariable', ?'ConjunctionVariable').
:-chr_constraint dictionary_lookup(-'QueryId', ?'ExternalVariable', ?'ConjunctionVariable').
:-chr_constraint distinct(-'QueryId', -'Variable').
:-chr_constraint distincts(-'QueryId', ?list('Variable')).
:-chr_constraint equality_restriction_variable(?'ApplicationValue', ?'EqualityRestrictionVariableUsed').
:-chr_constraint event(-'QueryId').
:-chr_constraint expression_where_restriction_variable(?'Variable').
:-chr_constraint fully_compile.
:-chr_constraint generate_sub_query_sql.
:-chr_constraint get_conjunction_constraints(?'Constraints').
:-chr_constraint group_by(-'QueryId', -'Variable').
:-chr_constraint group_bys(-'QueryId', ?list('Variable')).
:-chr_constraint identify_insert_row(+'StateChangeType', -'QueryId', +'Schema', +'TableName', +'Connection', ?'Identity').
:-chr_constraint identify_post_state_change_values(-'QueryId', +'Connection').
:-chr_constraint identify_pre_state_change_values(-'QueryId', +'StateChangeType', +'Connection').
:-chr_constraint ignore_if_null(?'Variable', ?'Variable').
:-chr_constraint implicit_join(-'QueryId', +'TableAlias', -'QueryId'). % PostgreSQL only
:-chr_constraint implicit_join_link(-'QueryId', -'QueryId'). % PostgreSQL only
:-chr_constraint implicit_join_sql(-'QueryId', ?list('SqlToken'), ?'Tail'). % PostgreSQL only
:-chr_constraint fetch_implicit_join_sql(-'QueryId', ?list('SqlToken'), ?'Tail'). % PostgreSQL only
:-chr_constraint in_line_format(-'QueryId', +'Format', ?list('FormatArg'), ?'ApplicationValue').
:-chr_constraint include_select_attribute(-'QueryId', ?'CompilationInstruction', +'Size', +list('SqlToken'), ?'Tail', ?'Output').
:-chr_constraint insert(-'QueryId', +'Schema', +'TableName', +list('AttributeNameValuePair')).
:-chr_constraint instantiate_table_aliases.
:-chr_constraint join(-'QueryId', -'Join', -'Join', +'JoinType', -'Join').
:-chr_constraint join_alias(-'Join', +'Side', ?'TableAlias').
:-chr_constraint join_leaf(-'Join', ?'TableAlias').
:-chr_constraint join_on(?'TableAlias', +'AttributeName', ?'TableAlias', +'AttributeName').
:-chr_constraint join_pointer(-'QueryId', -'Join').
:-chr_constraint join_tree_node(-'QueryId', -'Join', +'JoinTreeNode').
:-chr_constraint join_tree_nodes(-'QueryId', +list('JoinTreeNode')).
:-chr_constraint join_variable(?'Variable').
:-chr_constraint limit(-'QueryId', +'Schema', +'N').
:-chr_constraint log_select(+'SqlToken', +list('OdbcInput')).
:-chr_constraint log_state_change(+'SqlToken', +'StateChangeType', +list('OdbcInput')).
:-chr_constraint next_group_by_attribute_needs_comma(-'QueryId').
:-chr_constraint next_in_list_value_needs_comma(-'QueryId').
:-chr_constraint next_order_by_attribute_needs_comma(-'QueryId').
:-chr_constraint no_debug.
:-chr_constraint no_sql_statement_generated.
:-chr_constraint no_state_change_actions(-'QueryId').
:-chr_constraint no_where_restriction(+'StateChangeType').
:-chr_constraint not_a_singleton(+'Variable').
:-chr_constraint nolock(-'QueryId', ?'TableAlias').
:-chr_constraint number_of_rows_affected(-'QueryId', +'Connection', ?'N').

:-chr_constraint odbc_select_disjunction(?'Goal').
%:-chr_meta_predicate(odbc_select_disjunction(0)).

:-chr_constraint odbc_select_statement(+'Schema', +'SqlToken', ?list('OdbcParameter'), ?list('Output')).
:-chr_constraint on(-'Join', ?'Resolved', ?'On').
:-chr_constraint order_bys(-'QueryId', ?list('OrderBy')).
:-chr_constraint original_cql(?'Cql').
:-chr_constraint original_human_query(?'Cql').
:-chr_constraint outer_side_join(-'Join').
:-chr_constraint phase(-'QueryId', +'Phase').
:-chr_constraint post_execute_cleanup.
:-chr_constraint post_state_change_select_statement(-'QueryId', +list('AttributeName'), +'OdbcDataType', +'PreparedStatement').
:-chr_constraint postgres_identity(-'QueryId', ?'Identity').
:-chr_constraint prepare_odbc_statements.
:-chr_constraint prior_to_execution.
:-chr_constraint query(-'QueryId', +'Schema', +'QueryLevel').
:-chr_constraint query_table_alias(-'QueryId', +'Schema', +'TableName', ?'TableAlias').
:-chr_constraint query_type(-'QueryId', +'QueryType').
:-chr_constraint remove_query(-'QueryId', -'QueryId').
:-chr_constraint resolve_join_points(-'Join', ?'On', ?'On').
:-chr_constraint resolve_join_points.
:-chr_constraint restriction_leaf(-'QueryId', +'Disposition', ?'RestrictionTree').
:-chr_constraint restriction_tree(-'QueryId', +'Disposition', ?'RestrictionTree').
:-chr_constraint row_count(-'QueryId', ?'N').
:-chr_constraint runtime_constraints(?'Constraints').
:-chr_constraint search_for_join_aliases(-'Join', +'Side', -'Join').
:-chr_constraint select_attribute(-'QueryId', ?'SelectAttribute', ?'Keep', ?'SelectAttributeVariableUsed', ?'Variable').
:-chr_constraint select_attribute_for_disjunction_comparison(-'QueryId', ?'SelectAttribute').
:-chr_constraint select_attribute_written(-'QueryId').
:-chr_constraint select_attributes_for_disjunction_comparison(-'QueryId', ?list('SelectAttribute')).
:-chr_constraint select_binding(-'QueryId', ?'SelectBindingType', ?'Attribute', ?'ApplicationValue').
:-chr_constraint select_distinction(-'QueryId', +'DictinctionType').
:-chr_constraint select_for_insert_variable(-'QueryId', ?'Variable', +'TableName').
:-chr_constraint select_for_insert_variables(?list('Variable'), +'TableName').
:-chr_constraint selection_type(-'QueryId', +'SelectionType').
:-chr_constraint show_debug(+'DebugMode').
:-chr_constraint simplify.
:-chr_constraint solve.
:-chr_constraint sql_not(-'QueryId', -'QueryId').
:-chr_constraint sql_statement(-'QueryId', +list('SqlToken'), ?'Tail', +list('SqlToken'), ?'Tail', +list('SqlToken'), ?'Tail', ?list('OdbcParameter'), ?list('OdbcParameter'), ?list('Output')).
:-chr_constraint state_change_query(-'QueryId', +'StateChangeType', +'Schema', +'TableName').
:-chr_constraint state_change_value(-'QueryId', +'StateChangeType', +'When', +'Schema', +'TableName', +'PrimaryKeyAttributeName', +'PrimaryKeyValue', +'AttributeName', +'OdbcOutput').
:-chr_constraint store_equality_restriction_variables(?list('Variable')).
:-chr_constraint store_ignore_if_null_variables(?list('VariablePair')).
:-chr_constraint sub_query(-'QueryId', ?list('SqlToken'), ?'Tail', ?list('OdbcParameter')).
:-chr_constraint sub_query_join_variable(?'Variable').
:-chr_constraint sub_query_restriction(-'QueryId', +'SubQueryType', ?list('SqlToken'), ?'Tail', ?list('OdbcParameter')).
:-chr_constraint sub_query_select(-'QueryId').
:-chr_constraint referenced_table(+'TableName').
:-chr_constraint referenced_tables(?list('TableName')).
:-chr_constraint representative_attribute(?'Expression', +'Schema', -'TableName', -'AttributeName').
:-chr_constraint runtime_instantiation_check(-'QueryId', -'Variable').
:-chr_constraint tables_to_remove(+'Schema', +list('Identity')).
:-chr_constraint temporary_table(+'Schema', +'Identity').
:-chr_constraint temporary_tables(+'Schema', +list('Identity')).
:-chr_constraint top(-'QueryId', +'Schema', +'N').
:-chr_constraint unify(?'Variable', ?'Variable').
:-chr_constraint unify_ignore_if_null_variables.
:-chr_constraint union_outputs(-'QueryId', ?list('Output'), ?list('Variable')).
:-chr_constraint update(-'QueryId', +'Schema', +'TableName', ?'TableAlias', ?list('AttributeNameValuePair')).
:-chr_constraint update_table_alias(-'QueryId', +'Schema', -'Join', ?'TableAlias').
:-chr_constraint update_table_key(-'QueryId', +'Schema', ?list('AttributeNameValuePair')).
:-chr_constraint updated_row(-'QueryId', +'StateChangeType', +'When', +'Schema', +'TableName', +'PrimaryKeyAttributeName', +'PrimaryKeyValue', +list('AttributeNameValuePair')).
:-chr_constraint updated_row_primary_key(-'QueryId', +'StateChangeType', +'Schema', +'TableName', +'PrimaryKeyAttributeName', +'PrimaryKeyValue').
:-chr_constraint updated_rows(-'QueryId', +'StateChangeType', +'When', +'Schema', +'TableName', +'AttributeName', +list('AttributeName'), +list('Row')).
:-chr_constraint variables_to_attributes(?'Expression', ?'Expression').
:-chr_constraint where_restriction_variable(?'Variable').
:-chr_constraint write_expression(-'QueryId', +'Schema', +'TableName', +'AttributeName', ?'TableAlias', ?'Expression').
:-chr_constraint write_group_by_attribute(-'QueryId', ?list('SqlToken'), ?'Tail').
:-chr_constraint write_group_bys(-'QueryId').
:-chr_constraint write_in_list(-'QueryId', +'Disposition', +'Schema', +'TableName', +'AttributeName', +list('ApplicationValue')).
:-chr_constraint write_insert_attribute_name(-'QueryId', +'AttributeName').
:-chr_constraint write_insert_attribute_names(-'QueryId', +list('AttributeNameValuePair')).
:-chr_constraint write_insert_value(-'QueryId', +'Schema', +'TableName', +'AttributeName', ?'ApplicationValue').
:-chr_constraint write_insert_values(-'QueryId', +'Schema', +'TableName', ?list('AttributeNameValuePair')).
:-chr_constraint write_join(-'QueryId', -'Join').
:-chr_constraint write_join_ons(-'QueryId', ?'On').
:-chr_constraint write_limit.
:-chr_constraint write_lock_hint(-'QueryId', +'Schema', ?'TableAlias').
:-chr_constraint write_order_by(-'QueryId', ?'OrderBy').
:-chr_constraint write_order_by_attribute(-'QueryId', ?list('SqlToken'), ?'Tail').
:-chr_constraint write_order_bys(-'QueryId', ?list('OrderBy')).
:-chr_constraint write_query_sql.
:-chr_constraint write_restriction(-'QueryId', ?'CompilationInstruction', +'Disposition', ?'ApplicationValue', +'ComparisonOperator', ?'ApplicationValue').
:-chr_constraint write_restriction_1(-'QueryId', ?'CompilationInstruction', +'Disposition', +'OdbcDataType', ?'OdbcDataType', +'Schema', +'TableName', +'AttributeName', ?'RestrictionExpression', +'ComparisonOperator', ?'RestrictionExpression').
:-chr_constraint write_restriction_expression(-'QueryId', ?'CompilationInstruction', +'Disposition', ?'OdbcDataType', +'OdbcDataType', +'Schema', +'TableName', +'AttributeName', ?'RestrictionExpression').
:-chr_constraint write_restriction_tree(-'QueryId', +'Disposition', ?'RestrictionTree').
:-chr_constraint write_select_attribute(-'QueryId', ?'CompilationInstruction', ?list('SqlToken'), ?'Tail', ?'Output').
:-chr_constraint write_select_attribute_1(-'QueryId', ?'CompilationInstruction', ?list('SqlToken'), ?'Tail', ?'Output').
:-chr_constraint write_select_attributes(-'QueryId').
:-chr_constraint write_sql(-'QueryId', ?'CompilationInstruction',  +'Disposition', +list('SqlToken'), ?'Tail', ?list('OdbcParameter'), ?list('Output')).
:-chr_constraint write_update_attribute(-'QueryId', ?'TableAlias', +'AttributeName', ?'ApplicationValue').
:-chr_constraint write_update_attributes(-'QueryId', ?'TableAlias', ?list('AttributeNameValuePair')).


:-op(400, xfy, (::)).            % CQL
:-op(900, fy,  exists).          % CQL
:-op(750, yfx, *==).             % CQL
:-op(750, yfx, =*=).             % CQL
:-op(750, yfx, ==*).             % CQL
:-op(740, yfx, on).              % CQL
:-op(700, xfx, =~).              % CQL (LIKE)
:-op(700, xfx, \=~).             % CQL (NOT LIKE)
:-op(200, fy, #).                % CQL (nolock)
:-op(920, fy, ???).              % Debugging
:-op(920, fy, ??).               % Debugging
:-op(920, fy, ?).                % Debugging



%%      cql_set_module_default_schema(+Schema).
%
%       Set the Schema for a module

:-dynamic
        module_default_schema/2.

cql_set_module_default_schema(Schema) :-                       % +
        prolog_load_context(module, Module),
        set_module_default_schema(Module, Schema).



set_module_default_schema(Module,      % +
                          Schema) :-   % +
        retractall(module_default_schema(Module, _)),
        assert(module_default_schema(Module, Schema)).


%%      cql_get_module_default_schema(+Module, ?ModuleDefaultSchema).

cql_get_module_default_schema(Module,                          % +
                              ModuleDefaultSchema) :-          % ?

        ( module_default_schema(Module, Schema) ->
            ModuleDefaultSchema = Schema
        ;
            default_schema(ModuleDefaultSchema)
        ).

% This lets me control the compiletime checks via an environment variable
:-dynamic(do_cql_compiletime_checks/1).
do_cql_compiletime_checks:-
        ( do_cql_compiletime_checks(Status)->
            Status == true
        ; getenv('CQL_COMPILETIME_CHECKS', Atom)->
            assert(do_cql_compiletime_checks(Atom)),
            Atom == true
        ; otherwise->
            assert(do_cql_compiletime_checks(false)),
            fail
        ).

cql_compiletime_checks(Schema, Goals):-
        forall(cql_sql_clause(Goals, SQL, Parameters),
               check_decompilation(Schema, SQL, Parameters)).

check_decompilation(Schema, HalfCompiledSql, HalfCompiledOdbcParameters):-
        dbms(Schema, DBMS),
        ( fully_compile_sql(HalfCompiledSql, HalfCompiledOdbcParameters, [], Sql, OdbcParameters, _),
          atom_codes(Sql, SqlCodes),
          sql_tokens(Tokens, SqlCodes, []),
          findall(test,
                    ( member(odbc_parameter(_, _, _, _, _, _), OdbcParameters)
                    ; member(odbc_explicit_type_parameter(_, _, _), OdbcParameters)
                    ),
                  Bindings),
          sql_parse(action(Expression, _Types), _, [dbms(DBMS)], Tokens),
          with_output_to(atom(Atom), sql_write(current_output, Expression, [dbms(DBMS), parameter_bindings(Bindings), suppress_collations]))->
            % Make sure that the COLLATEs are all removed
            \+sub_atom(Atom, _, _, _, 'COLLATE')
        ; otherwise->
            prolog_load_context(source, FileName),
            prolog_load_context(term_position, TermPosition),
            stream_position_data(line_count, TermPosition, LineNumber),
            format(user_error, 'Could not decompile generated CQL: ~w~n~q~n', [FileName:LineNumber, HalfCompiledSql])
        ).





%%      cql_goal_expansion(?Schema,
%%                         ?Cql,
%%                         ?GoalExpansion).
%
%       Expand at compile time if the first term is a list of unbound input variables
%
%       Expand at runtime if the first term is compile_at_runtime

cql_goal_expansion(Schema, Cql, GoalExpansion) :-
        % {} is also used by clp(r,q) so make sure the CQL looks like CQL
        nonvar(Cql),
        Cql = (Arg, _),
        ( is_list(Arg)
        ; nonvar(Arg),
          Arg = compile_at_runtime(_)
        ),

        \+current_prolog_flag(xref, true), % Prevent expansion when used by pldoc to prevent spurious CQL compile errors
        atom(Schema),
        ( cql_goal_expansion_1(Schema, Cql, GoalExpansion_) ->
            GoalExpansion = GoalExpansion_
        ;
            throw(format('Cannot expand CQL: Schema = ~w, Cql=(~w)', [Schema, Cql]))
        ),
        ( do_cql_compiletime_checks ->
            setup_call_cleanup(assert(skip_cql_instantiation_check),
                               cql_compiletime_checks(Schema, GoalExpansion),
                               retract(skip_cql_instantiation_check))
        ; otherwise->
            true
        ).


cql_sql_clause(cql_odbc_state_change_statement(_, _, _, _, SQL, Parameters, _), SQL, Parameters).
cql_sql_clause(cql_pre_state_change_select_sql(_, _, _, _, SQL, _, Parameters), SQL, Parameters).
cql_sql_clause(cql_post_state_change_select_sql(_, _, Parameter, SQL), SQL, [odbc_explicit_type_parameter(Parameter, _, where_value)]).
cql_sql_clause(cql_odbc_select_statement(_, SQL, Parameters, _), SQL, Parameters).
cql_sql_clause((A, B), SQL, Parameters):-
        ( cql_sql_clause(A, SQL, Parameters)
        ; cql_sql_clause(B, SQL, Parameters)
        ).

:-multifile(cql_dependency_hook/2).
:-multifile(cql_generated_sql_hook/3).
cql_goal_expansion_1(Schema, (CompilationDirective, CqlA), GoalExpansion) :-
        ( prolog_load_context(source, FileName),
          prolog_load_context(term_position, TermPosition),
          stream_position_data(line_count, TermPosition, LineNumber)->
            DynamicallyCreatedCql = boolean(false)

        ; otherwise ->
            DynamicallyCreatedCql = boolean(true),
            FileName = '<Dynamically created CQL - no source file>',
            LineNumber = 0
        ),

        ( is_list(CompilationDirective),
          EqualityRestrictionVariables = CompilationDirective,
          forall(member(EqualityRestrictionVariable, EqualityRestrictionVariables), var(EqualityRestrictionVariable)) ->
            CqlB = (store_equality_restriction_variables(EqualityRestrictionVariables),
                    original_cql(CqlA),
                    cql_statement_location(FileName, LineNumber),
                    CqlA),
            translate_to_constraints(Schema, CqlB, InitialConstraints),
            call(InitialConstraints),
            compile_mode(compiletime),
            fully_compile,
            runtime_constraints(cql_execute(cache_odbc_statement)),
            collect_runtime_constraints(GoalExpansion),
            referenced_tables(ReferencedTables),
            ( ReferencedTables \== [],
              DynamicallyCreatedCql == boolean(false) ->
                sort(ReferencedTables, ReferencedTableSet),  % Remove duplicates
                file_base_name(FileName, FileBaseName),
                file_name_extension(Module, _, FileBaseName),
                ignore(cql_dependency_hook(ReferencedTableSet, Module))

            ; otherwise ->
                true
            ),
            ignore(cql_generated_sql_hook(FileName, LineNumber, GoalExpansion))
        ; nonvar(CompilationDirective),
          CompilationDirective = compile_at_runtime(IgnoreIfNullVariables) ->
            % Should this be a compile warning? runtime-compilation should now be officially deprecated
            ( nonvar(IgnoreIfNullVariables) ->
                variable_map(IgnoreIfNullVariables, CqlA, CqlB, VariableMap)

            ; otherwise ->
                true
            ),
            GoalExpansion = cql_runtime(Schema, IgnoreIfNullVariables, CqlA, CqlB, VariableMap, FileName, LineNumber)
        ; otherwise ->
            throw(error(domain_error(cql_compilation_directive, CompilationDirective), _))
        ).



%%      cql_runtime(+Schema, +IgnoreIfNullVariables, +CqlA, +CqlB, +VariableMap, +FileName, +LineNumber)

cql_runtime(Schema, IgnoreIfNullVariables, CqlA, CqlB, VariableMap, FileName, LineNumber) :-
        catch(cql_runtime_1(Schema, IgnoreIfNullVariables, CqlA, CqlB, VariableMap, FileName, LineNumber),
              format(Format, Arguments),  % Want a backtrace for compile errors in compile_at_runtime statements at runtime
              cql_error(cql, Format, Arguments)).


cql_runtime_1(Schema, IgnoreIfNullVariables, CqlA, CqlB, VariableMap, FileName, LineNumber) :-
        ( var(VariableMap) ->
            % Handle the case where IgnoreIfNullVariables is dynamically generated
            variable_map(IgnoreIfNullVariables, CqlA, CqlB, VariableMap)

        ; otherwise ->
            true
        ),
        CqlC = (store_ignore_if_null_variables(VariableMap),
                original_cql(CqlA),
                cql_statement_location(FileName, LineNumber),
                CqlB),
        translate_to_constraints(Schema, CqlC, InitialConstraints),
        call(InitialConstraints),
        fully_compile,
        cql_execute(do_not_cache_odbc_statement),
        referenced_tables(_).    % Clean up


variable_map(IgnoreIfNullVariables, CqlA, CqlB, VariableMap) :-
        copy_term(CqlA, CqlB),
        term_variables(CqlA, ExternalVariables),
        term_variables(CqlB, InternalVariables),
        variable_map_1(ExternalVariables, InternalVariables, IgnoreIfNullVariables, VariableMap).



variable_map_1([], [], _, []).

variable_map_1([A|As], [B|Bs], IgnoreIfNullVariables, [A-B|VariableMap]) :-
        select(I, IgnoreIfNullVariables, Rest),
        I == A, !,
        variable_map_1(As, Bs, Rest, VariableMap).

variable_map_1([V|As], [V|Bs], IgnoreIfNullVariables, VariableMap) :-
        variable_map_1(As, Bs, IgnoreIfNullVariables, VariableMap).



translate_to_constraints(Schema,                      % +
                         Cql,                         % +
                         InitialConstraints) :-       % ?
        create_variable_dictionary(Cql, [], CqlGround, Dictionary),
        findall(QueryId-Conjunction,
                translate_to_constraints_1(Schema, top_level_query, CqlGround, QueryId, Conjunction),
                Conjunctions),
        store_conjunctions(Conjunctions, Dictionary),
        conjunction_constraints(true),
        get_conjunction_constraints(InitialConstraints).



store_conjunctions([], _).

store_conjunctions([QueryId-Conjunction|Conjunctions], Dictionary) :-
        store_conjunction(Conjunction, QueryId, Dictionary),
        store_conjunctions(Conjunctions, Dictionary).



store_conjunction([], _, _).

store_conjunction([Goal|Goals], QueryId, Dictionary) :-
        create_conjunction_variables(Goal, QueryId, Dictionary, ConjunctionGoal),
        conjunction_goal(ConjunctionGoal),
        store_conjunction(Goals, QueryId, Dictionary).


collect_conjunction_variables @
        conjunction_constraints(Constraints),
        conjunction_variable(QueryId, ExternalVariable, ConjunctionVariable)
        <=>
        conjunction_constraints((conjunction_variable(QueryId, ExternalVariable, ConjunctionVariable), Constraints)).


collect_conjunction_goals @
        conjunction_constraints(Constraints),
        conjunction_goal(ConjunctionGoal)
        <=>
        conjunction_constraints((ConjunctionGoal, Constraints)).


get_conjunction_constraints @
        get_conjunction_constraints(Constraints),
        conjunction_constraints(C)
        <=>
        C = Constraints.



create_variable_dictionary(Term,                     % +
                           Dictionary,               % +
                           GroundTerm,               % ?
                           NewDictionary) :-         % ?
        ( ground(Term) ->
            GroundTerm = Term,
            NewDictionary = Dictionary

        ; var(Term) ->
            ( member(Variable-GroundTerm, Dictionary),
              Variable == Term ->
                NewDictionary = Dictionary
            ; var_property(Term, fresh(false))->
                gensym(cql_stale_var_, UniqueAtom),
                GroundTerm = '$VAR'(UniqueAtom),
                NewDictionary = [Term-GroundTerm|Dictionary]
            ; otherwise->
                gensym(cql_var_, UniqueAtom),
                GroundTerm = '$VAR'(UniqueAtom),
                NewDictionary = [Term-GroundTerm|Dictionary]
            )

        ; otherwise ->
            functor(Term, Name, Arity),
            functor(GroundTerm, Name, Arity),
            add_args_to_dictionary(Term, Dictionary, 1, Arity, GroundTerm, NewDictionary)
        ).



add_args_to_dictionary(Term, Dictionary, N, Arity, GroundTerm, NewDictionary) :-
        ( N > Arity ->
            NewDictionary = Dictionary
        ;
            arg(N, Term, Arg),
            create_variable_dictionary(Arg, Dictionary, GroundArg, DictionaryA),
            arg(N, GroundTerm, GroundArg),
            NextN is N + 1,
            add_args_to_dictionary(Term, DictionaryA, NextN, Arity, GroundTerm, NewDictionary)
        ).


dictionary_lookup @
        % This allows us to define new join points and tables as we compile (eep?)
        dictionary_addendum(QueryId, A, C)
        \
        dictionary_lookup(QueryId, A, B)
        <=>
        B = C.

failed_to_get_dictionary_addendum @
        dictionary_lookup(_,_,_)
        <=>
        fail.

cql_stale:attr_unify_hook(_,_).
create_conjunction_variables(Term, QueryId, Dictionary, TermWithVariables) :-
        ( var(Term) ->
            TermWithVariables = Term
        ; memberchk(ExternalVariable-Term, Dictionary) ->
            conjunction_variable(QueryId, ExternalVariable, ConjunctionVariable),
            ( Term = '$VAR'(Key),
              atom_prefix(Key, cql_stale_var_)->
                put_attr(ConjunctionVariable, cql_stale, 1)
            ; otherwise->
                true
            ),
            TermWithVariables = ConjunctionVariable
        ; dictionary_lookup(QueryId, Term, Var)->
            TermWithVariables = Var
        ; atomic(Term) ->
           TermWithVariables = Term

        ; otherwise ->
           functor(Term, Name, Arity),
           functor(TermWithVariables, Name, Arity),
           add_arg_variables(Term, QueryId, Dictionary, 1, Arity, TermWithVariables)
        ).



add_arg_variables(Term, QueryId, Dictionary, N, Arity, TermWithVariables) :-
        ( N > Arity ->
            true
        ;
            arg(N, Term, Arg),
            create_conjunction_variables(Arg, QueryId, Dictionary, NewArg),
            arg(N, TermWithVariables, NewArg),
            NextN is N + 1,
            add_arg_variables(Term, QueryId, Dictionary, NextN, Arity, TermWithVariables)
        ).



share_conjunction_variable @
        conjunction_variable(QueryId, ExternalVariable, ConjunctionVariableA)
        \
        conjunction_variable(QueryId, ExternalVariable, ConjunctionVariableB)
        <=>
        ConjunctionVariableA = ConjunctionVariableB.


bind_conjunction_variable_if_external_variable_gets_bound @
        conjunction_variable(_, ExternalVariable, ConjunctionVariable)
        <=>
        nonvar(ExternalVariable)
        |
        ConjunctionVariable = ExternalVariable.


bind_external_variables_once_fully_compiled @
        cql_fully_compiled
        \
        conjunction_variable(_, ExternalVariable, ConjunctionVariable)
        <=>
        ExternalVariable = ConjunctionVariable.



translate_to_constraints_1(Schema, QueryLevel, Cql, QueryId, CqlConstraints) :-
        ( translate_to_constraints_2(Schema, QueryLevel, Cql, QueryId, CqlConstraints, []) *->

            ( msort(CqlConstraints, SortedCqlConstraints),
              nextto(state_change_query(_, _, _, _), state_change_query(_, _, _, _), SortedCqlConstraints) ->
                throw(format('Cannot mix state change queries in a single CQL statement', []))
            ;
                true
            )
        ;
           throw(format('Cannot translate CQL: ~w~n', [Cql]))
        ).


%       translate_to_constraints_2//4
%
%       QueryId is a variable identifying a query (or sub-query).  The
%       top of the JOIN and WHERE trees is the QueryId

translate_to_constraints_2(Schema, QueryLevel, Cql, QueryId) -->
        translate(Schema, QueryId, QueryId, Cql),
        [query(QueryId, Schema, QueryLevel),
         restriction_tree(QueryId, where, true),
         sql_statement(QueryId, A, A, B, B, C, C, [], [], [])].


%       translate//4

translate(_, _, _, compile_time_goal(Goal)) --> !,
        {(Goal = Module:Goal1 ->
             true
         ; otherwise->
             prolog_load_context(module, Module),
             Goal1 = Goal
         )},
        [Module:Goal1].

translate(_, _, _, original_cql(Cql)) --> !,
        [original_cql(Cql)].

translate(_, _, _, cql_statement_location(FileName, LineNumber)) --> !,
        [cql_statement_location(FileName, LineNumber)].

translate(_, _, _, store_equality_restriction_variables(EqualityRestrictionVariables)) --> !,
        [store_equality_restriction_variables(EqualityRestrictionVariables)].

translate(_, _, _, store_ignore_if_null_variables(VariableMap)) --> !,
        [store_ignore_if_null_variables(VariableMap)].

translate(_, _, _, Term) -->
        {functor(Term, \+, Arity),
         Arity \== 1,
        throw(format('Negation (\\+) is arity one ... add some parentheses: ~w', [Term]))}.

translate(Schema, QueryId, ParentJoin, (Lhs ; Rhs)) --> !,     % BTP (compile time!)
        (translate(Schema, QueryId, ParentJoin, Lhs)
        ;
        translate(Schema, QueryId, ParentJoin, Rhs)).

translate(Schema, QueryId, ParentJoin, (Lhs, Rhs)) --> !,
        translate(Schema, QueryId, ParentJoin, Lhs),
        translate(Schema, QueryId, ParentJoin, Rhs).


% if we update FROM in postgres, we get an automatic join to the table we're updating
% We need to push things from the @ :: [...] into the where clause, and not have them in the join
% in fact, we shouldn't even list the table in the join unless we're doing a self-join
% (or I guess an outer join).

% I think that a right outer join in an update is the same as in inner join for all intents and purposes
translate(Schema, QueryId, ParentJoin, JoinTerm) -->
        {dbms(Schema, 'PostgreSQL')},
        { JoinTerm =.. [JoinOperator, Lhs, on(Rhs, On)],
          (join(JoinOperator, 'INNER JOIN') ; join(JoinOperator, 'RIGHT OUTER JOIN'))
        },
        {Lhs = (@ :: _) ; Rhs = (@ :: _)},
        !,
        translate(Schema, QueryId, ParentJoin, Lhs),
        translate(Schema, QueryId, ParentJoin, Rhs),
        translate(Schema, QueryId, ParentJoin, On),
        [implicit_join(QueryId, @, SubQueryId),
         implicit_join_link(QueryId, SubQueryId),
         on(SubQueryId, _, On)].

translate(Schema, QueryId, ParentJoin, JoinTerm) -->
        {dbms(Schema, 'PostgreSQL')},
        { JoinTerm =.. [JoinOperator, Lhs, Rhs],
          (join(JoinOperator, 'INNER JOIN') ; join(JoinOperator, 'RIGHT OUTER JOIN'))
        },
        {Lhs = (@ :: _) ; Rhs = (@ :: _)},
        !,
        translate(Schema, QueryId, ParentJoin, Lhs),
        translate(Schema, QueryId, ParentJoin, Rhs),
        [implicit_join(QueryId, @, _)].


% This gets very very unpleasant. To do a left outer join in the update, we have to first do an inner join the target
% and then outer join from THERE to complete. Postgres does NOT support left outer join in the from clause otherwise.
% note that FROM in an update is not standard SQL anyway.
translate(Schema, QueryId, ParentJoin, JoinTerm) -->
        {dbms(Schema, 'PostgreSQL')},
        { JoinTerm =.. [JoinOperator, Lhs, Rhs],
          join(JoinOperator, 'LEFT OUTER JOIN')
        },
        {Lhs = (@ :: Conditions)}, % don't allow for the target to be on the right. That doesn't really make a lot of sense anyway
        !,
        % Effectively we translate
        %    @ :: [a-A, ...] ==* z :: [z-A, ...]
        % into
        %    (@ :: [a-A, ..., pk-Pk] =*= @@ :: [pk-Pk]) ==* z :: [z-A, ...]
        % Where @@ is a symbol to mean 'the same table as the target but a different alias'
        % The first part of this is dropped as an implicit join

        % First off, we need to save space for the key. If we don't add this cql_var_X to the
        % dictionary somehow, it will be translated as if cql_var_X were a literal for the WHERE clause
         {gensym(cql_var_, KeyInfo),
          dictionary_addendum(QueryId, KeyInfo, Variable),
          append(Conditions, KeyInfo, NewConditions)},
         [update_table_key(QueryId, Schema, Variable)],
         translate(Schema, QueryId, ParentJoin, (@ :: KeyInfo =*= ((@@) :: NewConditions)) *== Rhs).


translate(Schema, QueryId, ParentJoin, JoinTerm) -->
        { JoinTerm =.. [JoinOperator, Lhs, on(Rhs, On)],
          join(JoinOperator, JoinType)
        }, !,
        translate(Schema, QueryId, LhsJoin, Lhs),
        translate(Schema, QueryId, RhsJoin, Rhs),

        [on(ParentJoin, _, On),
         join(QueryId, ParentJoin, LhsJoin, JoinType, RhsJoin)].

translate(Schema, QueryId, ParentJoin, JoinTerm) -->
        { JoinTerm =.. [JoinOperator, Lhs, Rhs],
          join(JoinOperator, JoinType)
        },
        !,
        translate(Schema, QueryId, LhsJoin, Lhs),
        translate(Schema, QueryId, RhsJoin, Rhs),

        [join(QueryId, ParentJoin, LhsJoin, JoinType, RhsJoin)].



translate(Schema,
          QueryId,
          ParentJoin,
          @ :: AttributeNameValuePairs) --> !,   % '@' means the update table
        [store_attribute_bindings(Schema, QueryId, TableAlias, AttributeNameValuePairs),
         attributes_to_check(QueryId, Schema, @, AttributeNameValuePairs),
         update_table_alias(QueryId, Schema, ParentJoin, TableAlias)].

translate(Schema,
          QueryId,
          ParentJoin,
          (@@) :: AttributeNameValuePairs) --> !,   % '@@' means a copy of update table
        [store_attribute_bindings(Schema, QueryId, TableAlias, AttributeNameValuePairs),
         attributes_to_check(QueryId, Schema, @, AttributeNameValuePairs),
         query_table_alias(QueryId, Schema, (@@), TableAlias),
         join_leaf(ParentJoin, TableAlias)].

translate(Schema,
          QueryId,
          ParentJoin,
          #TableName :: AttributeNameValuePairs) --> !,   % '#' means nolock
        translate_select(Schema,
                         QueryId,
                         ParentJoin,
                         TableName,
                         AttributeNameValuePairs, QueryTableAlias),
        [nolock(QueryId, QueryTableAlias)].

translate(Schema,
          QueryId,
          ParentJoin,
          TableName :: AttributeNameValuePairs) --> !,
        translate_select(Schema,
                         QueryId,
                         ParentJoin,
                         TableName,
                         AttributeNameValuePairs,
                         _).

translate(Schema, QueryId, _, insert(TableName, AttributeNameValuePairs)) --> !,
        {\+ duplicate_attributes(insert, Schema, TableName, AttributeNameValuePairs)},
        [insert(QueryId, Schema, TableName, AttributeNameValuePairs),
         query_type(QueryId, insert),
         attributes_to_check(QueryId, Schema, TableName, AttributeNameValuePairs),
         state_change_query(QueryId, insert, Schema, TableName)].

translate(Schema, QueryId, _, update(TableName, UpdateAttributeNameValuePairs)) --> !,
        {\+ duplicate_attributes(update, Schema, TableName, UpdateAttributeNameValuePairs)},
        [update(QueryId, Schema, TableName, _, UpdateAttributeNameValuePairs),
         query_type(QueryId, update),
         attributes_to_check(QueryId, Schema, TableName, UpdateAttributeNameValuePairs),
         state_change_query(QueryId, update, Schema, TableName)].

translate(Schema, QueryId, ParentJoin, delete(TableName, AttributeNameValuePairs)) --> !,
        {\+ duplicate_attributes(delete, Schema, TableName, AttributeNameValuePairs)},
        [delete_row(QueryId, TableName, TableAlias),
         query_type(QueryId, delete),
         attributes_to_check(QueryId, Schema, TableName, AttributeNameValuePairs),
         store_attribute_bindings(Schema, QueryId, TableAlias, AttributeNameValuePairs),
         join_leaf(ParentJoin, TableAlias),
         query_table_alias(QueryId, Schema, TableName, TableAlias),
         state_change_query(QueryId, delete, Schema, TableName)].

translate(Schema, QueryId, ParentJoin, \+((Lhs, Rhs))) --> !,     % De Morgan
        translate(Schema, QueryId, ParentJoin, (\+Lhs ; \+Rhs)).

translate(Schema, QueryId, ParentJoin, \+((Lhs ; Rhs))) --> !,    % De Morgan
        translate(Schema, QueryId, ParentJoin, (\+Lhs, \+Rhs)).

translate(Schema, QueryId, _, \+Comparison) -->
        {simple_comparison(Schema, Comparison, _, InverseOperator, Lhs, Rhs)}, !,
        translate_comparison(QueryId, Schema, Lhs, InverseOperator, Rhs).

translate(Schema, QueryId, _, Comparison) -->
        {simple_comparison(Schema, Comparison, Operator, _, Lhs, Rhs)}, !,
        translate_comparison(QueryId, Schema, Lhs, Operator, Rhs).

translate(Schema, QueryId, _, \+ exists(Goals)) --> !,
        translate_sub_query(Schema, QueryId, \+ exists, Goals).

translate(Schema, QueryId, _, exists(Goals)) --> !,
        translate_sub_query(Schema, QueryId, exists, Goals).

translate(_, QueryId, _, group_by(GroupBys)) --> !,
        [group_bys(QueryId, GroupBys)].

translate(_, QueryId, _, order_by(OrderBys)) --> !,
        [order_bys(QueryId, OrderBys)].

translate(Schema, QueryId, _, having(Having)) --> !,
        {prolog_term_to_restriction_tree(Schema, Having, RestrictionTree)},
        [restriction_tree(QueryId, having, RestrictionTree)].

translate(_, QueryId, _, distinct) --> !,
        [select_distinction(QueryId, distinct_on_all_select_attributes)].

translate(_, QueryId, _, distinct(Distincts)) --> !,
        [distincts(QueryId, Distincts)].

translate(Schema, QueryId, _, top(N)) --> !,
        [top(QueryId, Schema, N)].

translate(Schema, QueryId, _, identity(I)) --> !,
        [cql_identity(QueryId, Schema, I)].

translate(_, QueryId, _, row_count(N)) --> !,
        [row_count(QueryId, N)].

translate(_, _, _, absence_of_where_restriction_is_deliberate) --> !,
        [absence_of_where_restriction_is_deliberate].

translate(_, _, _, A=B) --> !,
        {(prolog_load_context(source, FileName),
          prolog_load_context(term_position, TermPosition),
          stream_position_data(line_count, TermPosition, LineNumber)->
            true
         ; otherwise->
            FileName = '<Dynamically created CQL - no source file>',
            LineNumber = 0
         ),
        print_message(warning, format('Unification in CQL is DEPRECATED (~w:~w)~n', [FileName, LineNumber]))},
        [unify(A, B)].

translate(_, _, _, true) --> !,
        [].

translate(_, QueryId, _, no_state_change_actions) --> !,
        [no_state_change_actions(QueryId)].

translate(_, _, _, Term) -->
        {throw(format('Cannot translate CQL term: ~w~n', [Term]))}.


join(*==, 'LEFT OUTER JOIN').
join(=*=, 'INNER JOIN').
join(==*, 'RIGHT OUTER JOIN').


translate_sub_query(Schema, QueryId, SubQueryType, Goals) -->
        translate_to_constraints_2(Schema, sub_query, Goals, SubQueryId), !,
        [sub_query_select(SubQueryId),
         sub_query_restriction(QueryId, SubQueryType, SubQuerySqlTokens, SubQueryTail, SubQueryInputs),
         sub_query(SubQueryId, SubQuerySqlTokens, SubQueryTail, SubQueryInputs)].


translate_select(Schema,                     % +
                 QueryId,                    % +
                 ParentJoin,                 % +
                 TableName,                  % +
                 AttributeNameValuePairs,    % +
                 QueryTableAlias) -->        % ?
        [store_attribute_bindings(Schema, QueryId, QueryTableAlias, AttributeNameValuePairs),
         query_type(QueryId, select),
         attributes_to_check(QueryId, Schema, TableName, AttributeNameValuePairs),
         join_leaf(ParentJoin, QueryTableAlias),
         query_table_alias(QueryId, Schema, TableName, QueryTableAlias)].


simple_comparison(Schema,                    % +
                  Comparison,                % +
                  Operator,                  % ?
                  InverseOperator,           % ?
                  Lhs,                       % ?
                  Rhs) :-                    % ?
        functor(Comparison, Operator, 2),
        prolog_to_sql_comparison_operator(Schema, Operator, _, InverseOperator),
        arg(1, Comparison, Lhs),
        arg(2, Comparison, Rhs).


translate_comparison(QueryId, Schema, Lhs, Operator, Rhs) -->
        translate_expression(Schema, Lhs, LhsResult),
        translate_expression(Schema, Rhs, RhsResult),

        [comparison(QueryId, LhsResult, Operator, RhsResult)].


translate_expression(Schema,
                     Goal,
                     aggregation_sub_query_sql(AggregationTableName, AggregationAttributeName, SubQuerySqlTokens, Tail, SubQueryInputs)) -->
        {functor(Goal, AggregationOperator, 2),
         aggregation_operator(AggregationOperator), !,
         arg(1, Goal, AggregationVariable),
         arg(2, Goal, Goals)},

        translate_to_constraints_2(Schema, sub_query, Goals, SubQueryId),

        [aggregation_variable(SubQueryId,
                              AggregationOperator,
                              AggregationVariable),
         aggregation_sub_query(SubQueryId, AggregationTableName, AggregationAttributeName, SubQuerySqlTokens, Tail, SubQueryInputs)].


translate_expression(_, Variable, Variable) -->
        [true].


aggregation_operator(count).
aggregation_operator(max).
aggregation_operator(min).
aggregation_operator(avg).
aggregation_operator(sum).


prolog_term_to_restriction_tree(Schema, \+(Lhs, Rhs), RestrictionTree) :- !,
        prolog_term_to_restriction_tree(Schema, (\+Lhs ; \+Rhs), RestrictionTree).

prolog_term_to_restriction_tree(Schema, \+(Lhs ; Rhs), RestrictionTree) :- !,
        prolog_term_to_restriction_tree(Schema, (\+Lhs, \+Rhs), RestrictionTree).

prolog_term_to_restriction_tree(Schema, (Lhs, Rhs), and(RestrictionLhs, RestrictionRhs)) :- !,
        prolog_term_to_restriction_tree(Schema, Lhs, RestrictionLhs),
        prolog_term_to_restriction_tree(Schema, Rhs, RestrictionRhs).

prolog_term_to_restriction_tree(Schema, (Lhs ; Rhs), or(RestrictionLhs, RestrictionRhs)) :- !,
        prolog_term_to_restriction_tree(Schema, Lhs, RestrictionLhs),
        prolog_term_to_restriction_tree(Schema, Rhs, RestrictionRhs).

prolog_term_to_restriction_tree(Schema, \+Comparison, comparison(RestrictionLhs, InverseOperator, RestrictionRhs)) :- !,
        simple_comparison(Schema, Comparison, _, InverseOperator, RestrictionLhs, RestrictionRhs).

prolog_term_to_restriction_tree(Schema, Comparison, comparison(Lhs, Operator, Rhs)) :-
        ( simple_comparison(Schema, Comparison, Operator, _, Lhs, Rhs) ->
            true
        ;
            throw(format('Cannot translate restriction term: ~w', [Comparison]))
        ).


equality_restriction_variables_are_unique @
        equality_restriction_variable(Variable, _)
        \
        equality_restriction_variable(Variable, _)
        <=>
        true.


store_equality_restriction_variables @
        store_equality_restriction_variables([InputVariable|InputVariables])
        <=>
        equality_restriction_variable(InputVariable, _),
        store_equality_restriction_variables(InputVariables).


cleanup_store_equality_restriction_variables @
        store_equality_restriction_variables([])
        <=>
        true.


store_ignore_if_null_variables @
        store_ignore_if_null_variables([ExternalVariable-InternalVariable|VariableMap])
        <=>
        ignore_if_null(ExternalVariable, InternalVariable),
        store_ignore_if_null_variables(VariableMap).


cleanup_store_ignore_if_null_variables @
        store_ignore_if_null_variables([])
        <=>
        true.


store_attribute_bindings(Schema, QueryId, TableAlias, AttributeNameValuePairs) :-
        ( store_attribute_bindings_1(Schema, QueryId, TableAlias, AttributeNameValuePairs) ->
            true
        ;
            throw(format('Bad attribute bindings: ~w', [AttributeNameValuePairs]))
        ).


store_attribute_bindings_1(_, _, _, []).

store_attribute_bindings_1(Schema, QueryId, TableAlias, [AttributeNameValuePair|AttributeNameValuePairs]) :-
        % For INSERT from SELECTs
        ( AttributeNameValuePair = as(AttributeName)-ApplicationValue ->
            attribute_binding(QueryId, attribute(Schema, TableAlias, AttributeName), selection_constant(ApplicationValue))

        % Normal Name-Value specification
        ; AttributeNameValuePair = AttributeName-ApplicationValue,
          atomic_application_value(ApplicationValue) ->
            attribute_binding(QueryId, attribute(Schema, TableAlias, AttributeName), ApplicationValue)

        % ignore_if_null
        ; AttributeNameValuePair = (AttributeName-ignore_if_null(ApplicationValue)),
          var(ApplicationValue)->
            attribute_binding(QueryId, attribute(Schema, TableAlias, AttributeName), AttributeValue),
            comparison(QueryId, AttributeValue, ==, ignore_if_null(ApplicationValue))

        % runtime list
        ; AttributeNameValuePair = (AttributeName-list(ApplicationValue))->
            attribute_binding(QueryId, attribute(Schema, TableAlias, AttributeName), AttributeValue),
            comparison(QueryId, AttributeValue, ==, list(ApplicationValue))

        % Compile-time attribute value
        ; AttributeNameValuePair = (AttributeName-CompileTimeGoal),
          callable(CompileTimeGoal),
          functor(CompileTimeGoal, PredicateName, ArityMinusOne),
          Arity is ArityMinusOne + 1,
          current_predicate(user:PredicateName/Arity),
          user:call(CompileTimeGoal, ApplicationValue) ->
            attribute_binding(QueryId, attribute(Schema, TableAlias, AttributeName), ApplicationValue)
        ),
        store_attribute_bindings_1(Schema, QueryId, TableAlias, AttributeNameValuePairs).


atomic_application_value(ApplicationValue) :-  % +
        ( var(ApplicationValue)
        ; atom(ApplicationValue)
        ; integer(ApplicationValue)
        ; rational(ApplicationValue)
	; timestamp(ApplicationValue)
        ; cql_atomic_value_check_hook(ApplicationValue)
        ; is_list(ApplicationValue)
        ; ApplicationValue == {null}
        ; ApplicationValue == {timestamp}
        ; ApplicationValue == {user_id}
        ; ApplicationValue == {transaction_id}
        ),
        !.

timestamp(TS) :-
	compound(TS),
	functor(TS, timestamp, 7).


ensure_binding_is_on_the_external_variable_so_that_ignore_if_null_works_properly_1 @
        ignore_if_null(ExternalVariable, InternalVariable)
        \
        unify(InternalVariable, X)
        <=>
        ExternalVariable = X.


ensure_binding_is_on_the_external_variable_so_that_ignore_if_null_works_properly_2 @
        ignore_if_null(ExternalVariable, InternalVariable)
        \
        unify(X, InternalVariable)
        <=>
        ExternalVariable = X.


make_unify_unify @
        unify(X, Y)
        <=>
        X = Y.


remove_comparison_involving_ignored_variable @
        ignore_if_null(ExternalVariable, InternalVariable)
        \
        comparison(_, Lhs, _, Rhs)
        <=>
        ( ExternalVariable == {null},
          InternalVariable == Lhs

        ; ExternalVariable == {null},
          InternalVariable == Rhs

        ; InternalVariable == Lhs,
          Rhs == {null}

        ; InternalVariable == Rhs,
          Lhs == {null}
        )
        |
        true.


fully_compile @
        fully_compile
        <=>
        no_sql_statement_generated,
        unify_ignore_if_null_variables,
        create_in_line_joins,
        create_join_points,
        resolve_join_points,
        determine_select_distinctions,
        create_select_bindings,
        determine_selection_type,
        create_restrictions,
        generate_sub_query_sql,
        simplify,
        ( debugging(cql(compile)) ->
            with_output_to(codes(Codes), chr_show_store(cql)),
            debug(cql(compile), '==========~n~s^^^^^^^^^^~n~n', [Codes])
        ;
            true
        ),
        check_for_top_without_order_by,
        write_query_sql,
        instantiate_table_aliases,
        write_limit,
        check_for_orphan_select_attributes_in_aggregations,
        check_query,
        check_for_unjoined_tables,
        check_for_orphan_group_bys,
        check_for_orphan_order_bys,
        check_for_orphan_distincts,
        check_for_orphan_select_variables_in_updates,
        prepare_odbc_statements,
        cleanup_compile,
        cql_fully_compiled.



unify_ignore_if_null_variables @
        unify_ignore_if_null_variables
        \
        ignore_if_null(ExternalVariable, InternalVariable)
        <=>
        nonvar(ExternalVariable),
        ExternalVariable \== {null}
        |
        InternalVariable = ExternalVariable.


cleanup_unify_ignore_if_null_variables @
        unify_ignore_if_null_variables
        <=>
        true.


update_atat_table_name @
        update(QueryId, Schema, TableName, _, _)
        \
        query_table_alias(QueryId, Schema, (@@), Alias)
        <=>
        query_table_alias(QueryId, Schema, TableName, Alias).


resolve_update_table_alias @
        create_join_points,
        update(QueryId, Schema, TableName, UpdateTableAlias, _),
        update_table_alias(QueryId, _, ParentJoin, TableAlias)
        ==>
        join_leaf(ParentJoin, TableAlias),
        query_table_alias(QueryId, Schema, TableName, TableAlias),
        UpdateTableAlias = TableAlias.


where_restriction_variable_not_allowed_to_be_an_outer_join_point @
        outer_side_join(Join),
        join_leaf(Join, TableAlias),
        attribute_binding(_, attribute(_, TableAlias, _), JoinVariable),
        join_variable(JoinVariable),
        where_restriction_variable(JoinVariable)
        <=>
        throw(format('A variable used in a WHERE RESTRICTION must not also be a SHARED VARIABLE defining an OUTER JOIN point', [])).


deprecated_group_by @
        attribute_binding(_, attribute(_, _, group_by(_)), _)
        <=>
        throw(format('OBSOLETE group_by format.  Use separate group_by([V1, V2, ...])', [])).


bad_group_by_specification @
        group_bys(_, GroupBys)
        <=>
        ( \+ is_list(GroupBys)
        ; is_list(GroupBys),
          member(GroupBy, GroupBys),
          nonvar(GroupBy)
        )
        |
        throw(format('GROUP BY must specify a LIST of variables', [group_by(GroupBys)])).


individual_group_by @
        group_bys(QueryId, [GroupBy|GroupBys])
        <=>
        group_by(QueryId, GroupBy),
        group_bys(QueryId, GroupBys).


no_more_group_bys @
        group_bys(_, [])
        <=>
        true.


copy_attribute_for_group_by @
        attribute_binding(QueryId, attribute(_, TableAlias, AttributeName), Variable)
        ==>
        attribute_for_group_by(QueryId, TableAlias, AttributeName, Variable).


ambiguous_group_by_attribute @
        group_by(QueryId, GroupBy),
        attribute_for_group_by(QueryId, TableAliasA, AttributeNameA, GroupBy),
        attribute_for_group_by(QueryId, TableAliasB, AttributeNameB, GroupBy),
        query_table_alias(_, _, TableNameA, TableAliasA),
        query_table_alias(_, _, TableNameB, TableAliasB)
        <=>
        throw(format('GROUP BY variable is AMBIGUOUS.  It identifies both ~w.~w AND ~w.~w.  Use == to specify the join point?',
                     [TableNameA, AttributeNameA, TableNameB, AttributeNameB])).


bad_distinct_specification @
        distincts(_, Distincts)
        <=>
        ( \+ is_list(Distincts)
        ; is_list(Distincts),
          member(Distinct, Distincts),
          nonvar(Distinct)
        )
        |
        throw(format('DISTINCT must specify a LIST of variables', [distinct(Distincts)])).


individual_distinct @
        distincts(QueryId, [Distinct|Distincts])
        <=>
        distinct(QueryId, Distinct),
        distincts(QueryId, Distincts).


no_more_distincts @
        distincts(QueryId, [])
        <=>
        select_distinction(QueryId, distinct_on_specified_attributes).


determine_select_distinctions @
        query(QueryId, _, _),
        determine_select_distinctions
        ==>
        determine_select_distinction(QueryId).


select_distinction_exists @
        select_distinction(QueryId, _)
        \
        determine_select_distinction(QueryId)
        <=>
        true.


no_select_distinction @
        determine_select_distinction(QueryId)
        <=>
        select_distinction(QueryId, no_distinction).


select_binding @
        create_select_bindings,
        attribute_binding(QueryId, attribute(Schema, TableAlias, AttributeName), Variable)
        ==>
        selection_variable(Variable)
        |
        Attribute = attribute(Schema, TableAlias, AttributeName),
        select_binding(QueryId, plain, Attribute, Variable).

cleanup_create_select_bindings @
        create_select_bindings
        <=>
        true.


selection_variable(V) :-
        var(V).
selection_variable(V) :-
        \+ ground(V),
        cql_atomic_value_check_hook(V).
selection_variable(selection_constant(_)).


select_binding_aggregation @
        select_binding(QueryId, plain, attribute(Schema, TableAlias, AggregationTerm), Variable)
        <=>
        AggregationTerm =.. [AggregationOperator, AttributeName],
        aggregation_operator(AggregationOperator)
        |
        select_binding(QueryId, aggregation(AggregationOperator), attribute(Schema, TableAlias, AttributeName), Variable).


sub_select_binding_aggregation @
        aggregation_variable(_, AggregationOperator, AggregationVariable),
        select_binding(QueryId, plain, Attribute, Variable)
        <=>
        AggregationVariable == Variable
        |
        select_binding(QueryId, aggregation(AggregationOperator), Attribute, Variable).


instantiate_table_aliases @
        instantiate_table_aliases,
        query_table_alias(_, _, TableName, TableAlias)
        ==>
        var(TableAlias)
        |
        table_alias_crunch(TableName, Crunched),
        atom_concat(Crunched, '_', Stem),
        gensym(Stem, Symbol),
        map_database_atom(Symbol, TableAlias).


table_alias_crunch(TableName, Crunched):-
        atom_codes(TableName, Codes),
        extract_abbreviation(AbbreviationCodes, [95|Codes], []),
        atom_codes(Crunched, AbbreviationCodes).

extract_abbreviation([])-->
        [].

extract_abbreviation([Code|Codes])-->
        "_",
        !,
        [Code],
        extract_abbreviation(Codes).

extract_abbreviation(Codes)-->
        [_],
        extract_abbreviation(Codes).

cleanup_instantiate_table_aliases @
        instantiate_table_aliases
        <=>
        true.


check_aggregation_attribute @
        select_binding(QueryId, aggregation(_), attribute(Schema, TableAlias, AttributeName), _),
        query_table_alias(QueryId, _, TableName, TableAlias)
        <=>
        \+ cql_data_type(Schema, TableName, AttributeName, _, _, _, _, _, _, _)
        |
        throw(format('Unknown SELECT attribute in CQL: ~w', [Schema:TableName:AttributeName])).


aggregation_selection @
        select_binding(QueryId, aggregation(_), _, _),
        determine_selection_type
        ==>
        selection_type(QueryId, aggregation_selection).


non_aggregation_selection @
        select_binding(QueryId, SelectBindingType, _, _),
        determine_selection_type
        ==>
        SelectBindingType \= aggregation(_)
        |
        selection_type(QueryId, non_aggregation_selection).


cleanup_determine_selection_type @
        determine_selection_type
        <=>
        true.


priority @
        selection_type(QueryId, aggregation_selection)
        \
        selection_type(QueryId, non_aggregation_selection)
        <=>
        true.


uniqueness @
        selection_type(QueryId, SelectionType)
        \
        selection_type(QueryId, SelectionType)
        <=>
        true.

get_data_size(Schema, TableName, AttributeName, Size):-
        cql_data_type(Schema, TableName, AttributeName, _, CharacterMaximumLength, _, _, _, _, _),
        ( CharacterMaximumLength == max ->
            % This should be close enough. It just has to be larger than any declared column length, and the max there is 8192 for SQL Server.
            % For all other DBMS it doesnt matter
            Size = 65535
        ; integer(CharacterMaximumLength) ->
            Size = CharacterMaximumLength
        ; otherwise->
            Size = 0
        ).

aggregation_selection @
        selection_type(QueryId, aggregation_selection),
        query_table_alias(QueryId, _, TableName, TableAlias)
        \
        select_binding(QueryId, SelectBindingType, attribute(Schema, TableAlias, AttributeName), Variable)
        <=>
        SelectBindingType = aggregation(_)
        |
        select_attribute(QueryId, select_attribute(SelectBindingType, Schema, TableName, TableAlias, AttributeName), _, _, Variable).


aggregation_group_by_selection @
        selection_type(QueryId, aggregation_selection),
        query_table_alias(QueryId, _, TableName, TableAlias),
        group_by(QueryId, GroupBy)
        \
        select_binding(QueryId, SelectBindingType, attribute(Schema, TableAlias, AttributeName), GroupBy)
        <=>
        select_attribute(QueryId, select_attribute(SelectBindingType, Schema, TableName, TableAlias, AttributeName), _, _, GroupBy).


ignore_aggregation_select_binding @
        selection_type(QueryId, aggregation_selection), where_restriction_variable(Variable) \ select_binding(QueryId, _, _, Variable) <=> true.
        selection_type(QueryId, aggregation_selection), join_variable(Variable) \ select_binding(QueryId, _, _, Variable) <=> true.
        selection_type(QueryId, aggregation_selection), sub_query_join_variable(Variable) \ select_binding(QueryId, _, _, Variable) <=> true.
        selection_type(QueryId, aggregation_selection), ignore_if_null(_, Variable) \ select_binding(QueryId, _, _, Variable) <=> true.


aggregation_select_binding_error @
        check_for_orphan_select_attributes_in_aggregations,
        selection_type(QueryId, aggregation_selection)
        \
        select_binding(QueryId, _, Attribute, _)
        <=>
        throw(format('Aggregation refers to an attribute which is not aggregated, not grouped by, not a restriction and not a join point: ~w', [Attribute])).


non_aggregation_select_binding @
        selection_type(QueryId, non_aggregation_selection),
        query_table_alias(QueryId, _, TableName, TableAlias)
        \
        select_binding(QueryId, SelectBindingType, attribute(Schema, TableAlias, AttributeName), Variable)
        <=>
        select_attribute(QueryId, select_attribute(SelectBindingType, Schema, TableName, TableAlias, AttributeName), _, _, Variable).


keep_if_distinct_on_specified_attributes @
        select_distinction(QueryId, distinct_on_specified_attributes),
        select_attribute(QueryId, _, Keep, _, Variable)
        \
        distinct(QueryId, Variable)
        <=>
        Keep = 1.


keep_if_distinct_on_all_attributes_or_if_there_is_no_distinction @
        select_distinction(QueryId, Distinction),
        select_attribute(QueryId, _, Keep, _, _)
        ==>
        ( Distinction == distinct_on_all_select_attributes
        ; Distinction == no_distinction
        )
        |
        Keep = 1.


copy_select_attribute_for_disjunction_comparison @
        select_attribute(QueryId, select_attribute(SelectBindingType, Schema, TableName, TableAlias, AttributeName), _, _, _)
        ==>
        select_attribute_for_disjunction_comparison(QueryId, select_attribute(SelectBindingType, Schema, TableName, TableAlias, AttributeName)).


bad_order_by_specification @
        order_bys(_, OrderBys)
        <=>
        ( \+ is_list(OrderBys)
        ; is_list(OrderBys),
          member(OrderBy, OrderBys),
          nonvar(OrderBy),
          OrderBy \= +(_),
          OrderBy \= -(_)
        )
        |
        throw(format('ORDER BY must specify a LIST of +/1 and -/1 terms but found ~w', [order_by(OrderBys)])).


copy_attribute_for_order_by @
        attribute_binding(QueryId, attribute(_, TableAlias, AttributeName), Variable)
        ==>
        attribute_for_order_by(QueryId, TableAlias, AttributeName, Variable).


ambiguous_order_by_attribute @
        write_order_by(QueryId, OrderBy)
        \
        attribute_for_order_by(QueryId, TableAliasA, AttributeNameA, Variable),
        attribute_for_order_by(QueryId, TableAliasB, AttributeNameB, Variable),
        query_table_alias(_, _, TableNameA, TableAliasA),
        query_table_alias(_, _, TableNameB, TableAliasB)
        <=>
        ( OrderBy == +Variable
        ; OrderBy == -Variable
        )
        |
        throw(format('ORDER BY variable is AMBIGUOUS.  It identifies both ~w.~w AND ~w.~w.  Use == to specify the join point?',
                     [TableNameA, AttributeNameA, TableNameB, AttributeNameB])).

select_attributes_1 @
        selection_type(QueryId, _)
        ==>
        select_attributes_for_disjunction_comparison(QueryId, []).


select_attributes_2 @
        select_attribute_for_disjunction_comparison(QueryId, SelectAttribute),
        select_attributes_for_disjunction_comparison(QueryId, SelectAttributes)
        <=>
        merge_set([SelectAttribute], SelectAttributes, SortedSelectAttributes),
        select_attributes_for_disjunction_comparison(QueryId, SortedSelectAttributes).


check_for_top_without_order_by @
        top(QueryId, _, _), order_bys(QueryId, _) \ check_for_top_without_order_by <=> true.
        top(_, _, _), check_for_top_without_order_by, original_cql(Cql) <=> throw(format('top without order_by in CQL: ~w', [Cql])).
        check_for_top_without_order_by <=> true.


join_tree_nodes @
        simplify,
        query(Join, _, top_level_query)       % Only bother doing join_tree for top_level_queries
        % Solitary join leaf has the QueryId as its parent i.e. Join == QueryId
        ==>
        join_pointer(Join, Join),
        join_tree_nodes(Join, []).


identify_join_type_1 @
        join(_, _, _, 'LEFT OUTER JOIN', RhsJoin)
        ==>
        outer_side_join(RhsJoin).


identify_join_type_2 @
        join(_, _, LhsJoin, 'RIGHT OUTER JOIN', _)
        ==>
        outer_side_join(LhsJoin).


every_join_below_an_outer_side_join_is_an_outer_side_join @
        outer_side_join(ParentJoin),
        join(_, ParentJoin, LhsJoin, _, RhsJoin)
        ==>
        outer_side_join(LhsJoin),
        outer_side_join(RhsJoin).


walk_join_tree_branch @
        join(QueryId, Join, LhsJoin, JoinType, RhsJoin)
        \
        join_pointer(QueryId, Join)
        <=>
        join_tree_node(QueryId, Join, branch(JoinType)),
        join_pointer(QueryId, LhsJoin),
        join_pointer(QueryId, RhsJoin).


walk_join_tree_on_clause @
        join_tree_node(QueryId, Join, branch(_)),
        on(Join, _, On)
        ==>
        join_tree_node(QueryId, Join, On).


cleanup_join_tree_node_branch @
        join_tree_node(_, _, branch(_))
        <=>
        true.


walk_join_tree_leaf @
        join_leaf(Join, TableAlias),
        query_table_alias(QueryId, _, _, TableAlias)
        \
        join_pointer(QueryId, Join)
        <=>
        join_tree_node(QueryId, Join, table_alias(TableAlias)).


accumulate_join_tree_nodes @
        join_tree_nodes(QueryId, JoinTreeNodes),
        join_tree_node(QueryId, _, JoinTreeNode)
        <=>
        join_tree_nodes(QueryId, [JoinTreeNode|JoinTreeNodes]).


amalgamate_restrictions_if_join_tree_is_the_same @
        select_attributes_for_disjunction_comparison(QueryIdA, SelectAttributesA),
        join_tree_nodes(QueryIdA, JoinTreeNodesA)
        \
        select_attributes_for_disjunction_comparison(QueryIdB, SelectAttributesB),
        join_tree_nodes(QueryIdB, JoinTreeNodesB),
        restriction_tree(QueryIdA, RestrictionType, RestrictionTreeA),
        restriction_tree(QueryIdB, RestrictionType, RestrictionTreeB)
        <=>
        unifiable(SelectAttributesA, SelectAttributesB, _),
        unifiable(JoinTreeNodesA, JoinTreeNodesB, Unifiers)
        |
        restriction_tree(QueryIdA, RestrictionType, or(RestrictionTreeA, RestrictionTreeB)),
        remove_query(QueryIdB, QueryIdA),
        unify_table_aliases(Unifiers).


% We can do this because the ONLY unbound variables in the join tree are table aliases
unify_table_aliases([]).
unify_table_aliases([TableAliasA=TableAliasB|Unifiers]) :-
        TableAliasA = TableAliasB,
        unify_table_aliases(Unifiers).


solve_sub_query @
        generate_sub_query_sql,
        query(SubQueryId, _, sub_query)
        ==>
        phase(SubQueryId, initial).


solve_top_level_query @
        write_query_sql,
        query(QueryId, _, top_level_query)
        ==>
        phase(QueryId, initial).


ignore_equality_restriction_variable_if_it_becomes_bound @
        equality_restriction_variable(Variable, _)
        <=>
        nonvar(Variable)
        |
        true.


add_sub_query_join_where @
        query(TopLevelQueryId, _, top_level_query),
        query(SubQueryId, _, sub_query),
        attribute_binding(TopLevelQueryId, attribute(_, TopLevelTableAlias, TopLevelAttributeName), Variable),
        attribute_binding(SubQueryId, attribute(_, SubQueryTableAlias, SubQueryAttributeName), Variable)
        \
        % No longer a SELECT binding in the sub-query; its now part of the sub query WHERE
        select_binding(SubQueryId, _, _, Variable)
        <=>
        not_a_singleton(Variable),
        sub_query_join_variable(Variable),
        restriction_leaf(SubQueryId,
                         where,
                         comparison(attribute(_, TopLevelTableAlias, TopLevelAttributeName),
                                    ==,
                                    attribute(_, SubQueryTableAlias, SubQueryAttributeName))).


restriction_from_comparison_of_top_level_query_attribute_to_sub_query_attribute @
        %
        % x :: [a-A, b-B], \+ y :: [b-B, c-C], C > A
        %
        query(QueryId, _, top_level_query),
        query(SubQueryId, _, sub_query),
        attribute_binding(QueryId, Attribute_1, V1),
        attribute_binding(SubQueryId, Attribute_2, V2)
        \
        comparison(QueryId, Lhs, Operator, Rhs)
        <=>
        ( Lhs == V1,
          Rhs == V2 ->
            Comparison = comparison(Attribute_1, Operator, Attribute_2)

        ; Lhs == V2,
          Rhs == V1 ->
            Comparison = comparison(Attribute_2, Operator, Attribute_1)
        )
        |
        restriction_leaf(SubQueryId, where, Comparison).


add_sub_query_restriction @
        create_restrictions
        \
        sub_query_restriction(QueryId, SubQueryType, SubQuerySql, SubQueryTail, SubQueryInputs)
        <=>
        restriction_leaf(QueryId, where, sub_query(SubQueryType, SubQuerySql, SubQueryTail, SubQueryInputs)).


restriction_from_bound_attribute @
        %
        % x :: a-'A1'
        % x :: a-['A1', 'A2']
        %
        create_restrictions,
        attribute_binding(QueryId, Attribute, ApplicationValue)
        ==>
        ( ground(ApplicationValue)
        ; is_list(ApplicationValue)
        ),
        ApplicationValue \= selection_constant(_)
        |
        restriction_leaf(QueryId, where, comparison(Attribute, ==, ApplicationValue)).


restriction_from_equality_restriction_variable @
        %
        % {[A], x :: [a-A]}
        %
        create_restrictions,
        attribute_binding(QueryId, Attribute, Variable),
        equality_restriction_variable(Variable, EqualityRestrictionVariableUsed)
        ==>
        EqualityRestrictionVariableUsed = equality_restriction_variable_used,
        where_restriction_variable(Variable),
        restriction_leaf(QueryId, where, comparison(Attribute, ==, equality_restriction(Variable))).


restriction_from_comparison @
        create_restrictions
        \
        comparison(QueryId, Lhs, Operator, Rhs)
        <=>
        not_a_singleton(Lhs),
        not_a_singleton(Rhs),
        variables_to_attributes(Lhs, MappedLhs),
        variables_to_attributes(Rhs, MappedRhs),
        restriction_leaf(QueryId, where, comparison(MappedLhs, Operator, MappedRhs)).

cqlv2_1_restriction_from_any_non_fresh_variable @
        query_type(QueryId, QueryType),
        compile_mode(compiletime),
        attribute_binding(QueryId, attribute(Schema, Alias, AttributeName), Variable)
        ==>
        atom(AttributeName), % Exclude aggregates. Is there a more elegant way?
        var(Variable),
        get_attr(Variable, cql_stale, 1)
        |
        ( ( QueryType == select ; QueryType == insert) ->
            Comparison = if_not_var(Variable)
        ; otherwise->
            Comparison = equality_restriction(Variable)
        ),
        cql2_variable(QueryId, Variable, comparison(attribute(Schema, Alias, AttributeName), ==, Comparison)).

/*
cqlv2_variable_unique @
        cql2_variable(QueryId, _, Variable)
        \
        cql2_variable(QueryId, _, Variable)
        <=>
        true.
*/

cqlv2_1_except_when_restriction_already_exists_1 @
        equality_restriction_variable(Variable, _)
        \
        cql2_variable(_, Variable, _)
        <=>
        true.

cqlv2_1_except_when_restriction_already_exists_2 @
        expression_where_restriction_variable(Variable)
        \
        cql2_variable(_, Variable, _)
        <=>
        true.

cqlv2_1_except_when_restriction_already_exists_3 @
        sub_query_join_variable(Variable)
        \
        cql2_variable(_, Variable, _)
        <=>
        true.

cqlv2_1_except_when_restriction_already_exists_4 @
        expression_where_restriction_variable(Variable)
        \
        cql2_variable(_, Variable, _)
        <=>
        true.

cqlv2_1_except_when_restriction_already_exists_5 @
        outer_side_join(Join),
        join_leaf(Join, TableAlias),
        attribute_binding(_, attribute(_, TableAlias, _), JoinVariable),
        join_variable(JoinVariable)
        \
        cql2_variable(_, JoinVariable, _)
        <=>
        true.


cqlv2_1_except_comparisons @
        query_type(QueryId, Type),
        comparison(QueryId, Lhs, _Operator, Rhs)
        \
        cql2_variable(QueryId, Variable, _)
        <=>
        % If you write something like
        % foo(X):- {[], some_table :: [column-X], X == 'VALUE'}
        % Then, first of all, you probably made a coding error, since this is unlikely what you meant.
        % X is not actually selected here, it MUST be part of the where clause. Further, it MUST contain 'VALUE'.
        % For now, unify X with the target (Value). However, we must do it after the select. Consider:
        % foo(X):- {[], some_table :: [integer_value-X], X > 500}
        % This should generate
        %   SELECT .... FROM some_table WHERE integer_value > 500
        % What should we actually do with X? If it is bound, then we can make it
        %    SELECT .... FROM some_table WHERE integer_value > 500 AND integer_value = ?
        % If it is NOT bound, then I think this is actually an instantiation error?
        % The trouble comes when we have
        % {[], update(some_table, [...]), @ :: [integer_value-X], X == Y}.
        %   If Y is unbound here, we want a runtime error, but if X is bound, we want an error as well!
        ( Type == update ; Type == delete ),
        ( Lhs == Variable ; Rhs == Variable )
        |
        runtime_instantiation_check(QueryId, Variable).


cqlv2_variable_is_ok @
        create_restrictions
        \
        cql2_variable(QueryId, Variable, Comparison)
        <=>
        where_restriction_variable(Variable),
        restriction_leaf(QueryId, where, Comparison).






variables_to_attributes @
        attribute_binding(_, A, Variable) \ variables_to_attributes(Variable, Attribute) <=> var(Variable) | Attribute = A, where_restriction_variable(Variable).
        variables_to_attributes(T1, T2) <=> var(T1) ; ground(T1) | T2 = T1.
        variables_to_attributes([H1|T1], T3) <=> variables_to_attributes(H1, H2), variables_to_attributes(T1, T2), T3 = [H2|T2].
        variables_to_attributes(T1, T4) <=> T1 =.. T2, variables_to_attributes(T2, T3), T4 =.. T3.


cleanup_create_restrictions @
        create_restrictions
        <=>
        true.


ignore_comparison_to_empty_list @
        restriction_leaf(_, _, comparison(Lhs, ==, Rhs))
        <=>
        ( Lhs == []
        ; Rhs == []
        )
        |
        true.


top_level_restriction_cannot_refer_to_a_sub_query_attribute @
        query(TopLevelQueryId, _, top_level_query),
        query(SubQueryId, _, sub_query),
        attribute_binding(SubQueryId, attribute(Schema, TableAlias, AttributeName), _)
        \
        restriction_leaf(TopLevelQueryId, _, comparison(Lhs, _, Rhs))
        <=>
        ( cql_path_arg(_, Lhs, SubTerm)
        ; cql_path_arg(_, Rhs, SubTerm)),
        SubTerm == attribute(Schema, TableAlias, AttributeName)
        |
        throw(format('Top level restriction cannot refer to a sub-query attribute : ~w', [AttributeName])).


simplify_or_restriction @
        % Trivial "or" conditions can arise out from the simplification of disjunctions
        restriction_tree(QueryId, RestrictionType, or(Lhs, Rhs))
        <=>
        ( Lhs == true
        ; Rhs == true
        )
        |
        restriction_tree(QueryId, RestrictionType, true).


simplify_and_restriction @
        restriction_tree(QueryId, RestrictionType, and(Lhs, Rhs))
        <=>
        ( Lhs == true ->
            RestrictionTree = Rhs

        ; Rhs == true ->
            RestrictionTree = Lhs
        )
        |
        restriction_tree(QueryId, RestrictionType, RestrictionTree).


add_to_restriction_tree @
        restriction_tree(QueryId, RestrictionType, ExistingRestrictionTree),
        restriction_leaf(QueryId, RestrictionType, Restriction)
        <=>
        restriction_tree(QueryId, RestrictionType, and(ExistingRestrictionTree, Restriction)).


insert_inserted_ @
        insert(QueryId, Schema, TableName, AttributeNameValuePairs)
        <=>
        cql_data_type(Schema, TableName, inserted_, _, _, _, _, _, _, _),
        \+ memberchk(inserted_-_, AttributeNameValuePairs)
        |
        insert(QueryId, Schema, TableName, [inserted_-{timestamp}|AttributeNameValuePairs]).


insert_inserted_by_ @
        insert(QueryId, Schema, TableName, AttributeNameValuePairs)
        <=>
        cql_data_type(Schema, TableName, inserted_by_, _, _, _, _, _, _, _),
        \+ memberchk(inserted_by_-_, AttributeNameValuePairs)
        |
        insert(QueryId, Schema, TableName, [inserted_by_-{user_id}|AttributeNameValuePairs]).


insert_updated_ @
        insert(QueryId, Schema, TableName, AttributeNameValuePairs)
        <=>
        cql_data_type(Schema, TableName, updated_, _, _, _, _, _, _, _),
        \+ memberchk(updated_-_, AttributeNameValuePairs)
        |
        insert(QueryId, Schema, TableName, [updated_-{timestamp}|AttributeNameValuePairs]).


insert_updated_by_ @
        insert(QueryId, Schema, TableName, AttributeNameValuePairs)
        <=>
        cql_data_type(Schema, TableName, updated_by_, _, _, _, _, _, _, _),
        \+ memberchk(updated_by_-_, AttributeNameValuePairs)
        |
        insert(QueryId, Schema, TableName, [updated_by_-{user_id}|AttributeNameValuePairs]).


update_updated_ @
        update(QueryId, Schema, TableName, TableAlias, AttributeNameValuePairs)
        <=>
        cql_data_type(Schema, TableName, updated_, _, _, _, _, _, _, _),
        \+ memberchk(updated_-_, AttributeNameValuePairs)
        |
        update(QueryId, Schema, TableName, TableAlias, [updated_-{timestamp}|AttributeNameValuePairs]).


update_updated_by_ @
        update(QueryId, Schema, TableName, TableAlias, AttributeNameValuePairs)
        <=>
        cql_data_type(Schema, TableName, updated_by_, _, _, _, _, _, _, _),
        \+ memberchk(updated_by_-_, AttributeNameValuePairs)
        |
        update(QueryId, Schema, TableName, TableAlias, [updated_by_-{user_id}|AttributeNameValuePairs]).


insert_transaction_id_ @
        insert(QueryId, Schema, TableName, AttributeNameValuePairs)
        <=>
        cql_data_type(Schema, TableName, transaction_id_, _, _, _, _, _, _, _),
        \+ memberchk(transaction_id_-_, AttributeNameValuePairs)
        |
        insert(QueryId, Schema, TableName, [transaction_id_-{transaction_id}|AttributeNameValuePairs]).


update_transaction_id_ @
        update(QueryId, Schema, TableName, TableAlias, AttributeNameValuePairs)
        <=>
        cql_data_type(Schema, TableName, transaction_id_, _, _, _, _, _, _, _),
        \+ memberchk(transaction_id_-_, AttributeNameValuePairs)
        |
        update(QueryId, Schema, TableName, TableAlias, [transaction_id_-{transaction_id}|AttributeNameValuePairs]).


insert_generation_ @
        insert(QueryId, Schema, TableName, AttributeNameValuePairs)
        <=>
        cql_data_type(Schema, TableName, generation_, _, _, _, _, _, _, _),
        \+ memberchk(generation_-_, AttributeNameValuePairs)
        |
        insert(QueryId, Schema, TableName, [generation_-0|AttributeNameValuePairs]).


update_generation_ @
        update(QueryId, Schema, TableName, TableAlias, AttributeNameValuePairs)
        <=>
        cql_data_type(Schema, TableName, generation_, _, _, _, _, _, _, _),
        \+ memberchk(generation_-_, AttributeNameValuePairs)
        |
        update(QueryId, Schema, TableName, TableAlias, [generation_-{increment}|AttributeNameValuePairs]).


write_insert_based_on_select @
        query_table_alias(QueryId, _, _, _),
        phase(QueryId, initial)
        \
        insert(QueryId, _, TableName, AttributeNameValuePairs)
        <=>
        extract_variables(AttributeNameValuePairs, InsertVariables),
        select_for_insert_variables(InsertVariables, TableName),
        write_sql(QueryId, compile, top, ['INSERT INTO ', table_name(TableName), ' ('|T1], T1, [], []),
        write_insert_attribute_names(QueryId, AttributeNameValuePairs),
        write_sql(QueryId, compile, top, [') '|T2], T2, [], []).



extract_variables([], []).

extract_variables([_-V|AttributeNameValuePairs], [V|InsertVariables]) :-
        var(V), !,
        extract_variables(AttributeNameValuePairs, InsertVariables).

extract_variables([_|AttributeNameValuePairs], InsertVariables) :-
        extract_variables(AttributeNameValuePairs, InsertVariables).


write_insert @
        phase(QueryId, initial),
        insert(QueryId, Schema, TableName, AttributeNameValuePairs)
        <=>
        ( AttributeNameValuePairs == []->
            write_sql(QueryId, compile, top, ['INSERT INTO ', table_name(TableName), ' DEFAULT VALUES'|T1], T1, [], [])
        ;
            write_sql(QueryId, compile, top, ['INSERT INTO ', table_name(TableName), ' ('|T2], T2, [], []),
            write_insert_attribute_names(QueryId, AttributeNameValuePairs),
            write_sql(QueryId, compile, top, [') VALUES ('|T3], T3, [], []),
            write_insert_values(QueryId, Schema, TableName, AttributeNameValuePairs),
            ( dbms(Schema, 'PostgreSQL'),
              database_identity(Schema, TableName, PrimaryKey) ->
                write_sql(QueryId, compile, top, [') RETURNING ', PrimaryKey|T4], T4, [], [])
            ; otherwise->
                write_sql(QueryId, compile, top, [')'|T4], T4, [], [])
            )
        ).


write_insert_attribute_names_1 @
        write_insert_attribute_names(QueryId, [AttributeName-_])
        <=>
        write_insert_attribute_name(QueryId, AttributeName).


write_insert_attribute_names_2 @
        write_insert_attribute_names(QueryId, [AttributeName-_|AttributeNameValuePairs])
        <=>
        write_insert_attribute_name(QueryId, AttributeName),
        write_sql(QueryId, compile, top, [', '|T], T, [], []),
        write_insert_attribute_names(QueryId, AttributeNameValuePairs).


write_insert_attribute_name @
        write_insert_attribute_name(QueryId, AttributeName)
        <=>
        write_sql(QueryId, compile, top, [attribute_name(AttributeName)|T], T, [], []).


write_insert_values_1 @
        write_insert_values(QueryId, Schema, TableName, [AttributeName-ApplicationValue])
        <=>
        write_insert_value(QueryId, Schema, TableName, AttributeName, ApplicationValue).


write_insert_values_2 @
        write_insert_values(QueryId, Schema, TableName, [AttributeName-ApplicationValue|AttributeNameValuePairs])
        <=>
        write_insert_value(QueryId, Schema, TableName, AttributeName, ApplicationValue),
        write_sql(QueryId, compile, top, [', '|T], T, [], []),
        write_insert_values(QueryId, Schema, TableName, AttributeNameValuePairs).


write_in_line_formatted_insert_value @
        write_insert_value(QueryId, Schema, TableName, AttributeName, format(Format, FormatArgs))
        <=>
        in_line_format(QueryId, Format, FormatArgs, ApplicationValue),
        write_insert_value(QueryId, Schema, TableName, AttributeName, ApplicationValue).


write_insert_value @
        write_insert_value(QueryId, Schema, TableName, AttributeName, ApplicationValue)
        <=>
        write_sql(QueryId, compile, top, [?|T], T, [odbc_parameter(Schema, TableName, AttributeName, ApplicationValue, insert_value, _)], []).


write_update @
        update(QueryId, Schema, TableName, TableAlias, AttributeNameValuePairs)
        \
        phase(QueryId, initial)
        <=>
        ( dbms(Schema, 'Microsoft SQL Server') ->
            write_sql(QueryId, compile, top, ['UPDATE ', TableAlias, ' SET '|T], T, [], [])

        ; dbms(Schema, 'PostgreSQL') ->
            write_sql(QueryId, compile, top, ['UPDATE ', TableName, ' ', TableAlias, ' SET '|T], T, [], [])
        ; dbms(Schema, 'SQLite') ->
            % SQLite does not support joins in updates. However, it has an ID for each row, meaning we can put this:
            % UPDATE <tablename> SET <columns without aliases> WHERE rowid IN (SELECT <tablename>.rowid FROM <rest of the query>)
            write_sql(QueryId, compile, top, ['UPDATE ', TableName, ' SET '|T], T, [], [])
        ),
        write_update_attributes(QueryId, TableAlias, AttributeNameValuePairs),
        phase(QueryId, from).


write_update_attributes_1 @
        write_update_attributes(QueryId, TableAlias, [AttributeName-ApplicationValue])
        <=>
        write_update_attribute(QueryId, TableAlias, AttributeName, ApplicationValue).


write_update_attributes_2 @
        write_update_attributes(QueryId, TableAlias, [AttributeName-ApplicationValue|AttributeNameValuePairs])
        <=>
        write_update_attribute(QueryId, TableAlias, AttributeName, ApplicationValue),
        write_sql(QueryId, compile, top, [', '|T], T, [], []),
        write_update_attributes(QueryId, TableAlias, AttributeNameValuePairs).


write_update_attributes_3 @
        write_update_attributes(_, _, AttributeNameValuePairs)
        <=>
        throw(format('Bad UPDATE attributes: ~w',  [AttributeNameValuePairs])).


write_generation_attribute @
        update_table_alias(QueryId, Schema, _, TableAlias)
        \
        write_update_attribute(QueryId, TableAlias, AttributeName, {increment})
        <=>
        AttributeName == generation_
        |
        ( dbms(Schema, 'Microsoft SQL Server') ->
            write_sql(QueryId,
                      compile,
                      top,
                      [TableAlias, '.', attribute_name(AttributeName), =, TableAlias, '.', attribute_name(AttributeName), +, '1'|T],
                      T,
                      [],
                      [])
        ; dbms(Schema, 'PostgreSQL') ->
            write_sql(QueryId, compile,
                      top,
                      [attribute_name(AttributeName), =, TableAlias, '.', attribute_name(AttributeName), +, '1'|T],
                      T,
                      [],
                      [])
        ; dbms(Schema, 'SQLite') ->
            write_sql(QueryId, compile,
                      top,
                      [attribute_name(AttributeName), =, attribute_name(AttributeName), +, '1'|T],
                      T,
                      [],
                      [])
        ).


write_update_attribute_copy_sql_server @
        query_table_alias(QueryId, Schema, _, TableAlias),
        select_attribute(QueryId, select_attribute(_, _, _, SelectTableAlias, SelectAttributeName), 1, SelectAttributeVariableUsed, SelectVariable)
        \
        write_update_attribute(QueryId, TableAlias, AttributeName, UpdateVariable)
        <=>
        dbms(Schema, 'Microsoft SQL Server'),
        var(SelectVariable),
        SelectVariable == UpdateVariable
        |
        SelectAttributeVariableUsed = select_attribute_variable_used,
        write_sql(QueryId,
                  compile,
                  top,
                  [TableAlias, '.', attribute_name(AttributeName), =, SelectTableAlias, '.', attribute_name(SelectAttributeName)|T],
                  T,
                  [],
                  []).

write_update_attribute_copy_postgres @
        update_table_alias(QueryId, Schema, _, TargetAlias),
        query_table_alias(QueryId, _, _, TableAlias),
        select_attribute(QueryId, select_attribute(_, _, _, SelectTableAlias, SelectAttributeName), 1, SelectAttributeVariableUsed, SelectVariable)
        \
        write_update_attribute(QueryId, TableAlias, AttributeName, UpdateVariable)
        <=>
        dbms(Schema, 'PostgreSQL'),
        var(SelectVariable),
        SelectVariable == UpdateVariable
        |
        SelectAttributeVariableUsed = select_attribute_variable_used,

        ( TargetAlias == TableAlias ->
            write_sql(QueryId,
                      compile,
                      top,
                      [attribute_name(AttributeName), =, SelectTableAlias, '.', attribute_name(SelectAttributeName)|T],
                      T,
                      [],
                      [])
        ; otherwise->
            write_sql(QueryId,
                      compile,
                      top,
                      [TableAlias, '.', attribute_name(AttributeName), =, SelectTableAlias, '.', attribute_name(SelectAttributeName)|T],
                      T,
                      [],
                      [])

        ).


write_update_attribute_copy_sqlite @
        update_table_alias(QueryId, Schema, _, TargetAlias),
        query_table_alias(QueryId, Schema, TableName, TableAlias),
        select_attribute(QueryId, select_attribute(_, _, _, SelectTableAlias, SelectAttributeName), 1, SelectAttributeVariableUsed, SelectVariable)
        \
        write_update_attribute(QueryId, TableAlias, AttributeName, UpdateVariable)
        <=>
        dbms(Schema, 'SQLite'),
        var(SelectVariable),
        SelectVariable == UpdateVariable
        |
        SelectAttributeVariableUsed = select_attribute_variable_used,
        ( TargetAlias == SelectTableAlias->
            % This is the simplest case. Otherwise we must write a subquery
            write_sql(QueryId,
                      compile,
                      top,
                      [attribute_name(AttributeName), =, attribute_name(SelectAttributeName)|T],
                      T,
                      [],
                      [])
        ; otherwise->
            write_sql(QueryId,
                      compile,
                      top,
                      [attribute_name(AttributeName), =, '(SELECT ', SelectTableAlias, '.', attribute_name(SelectAttributeName), ' '|T],
                      T,
                      [],
                      []),
          Tail = [' AND ', TableAlias, '.rowid = ', TableName, '.rowid)'|T3],
          write_sql(QueryId,
                    compile,
                    top,
                    X,
                    T3,
                    Parameters,
                    []),
          find_copy_of_from(QueryId, X, Tail, Parameters)
        ).

write_in_line_formatted_update_attribute @
        query_table_alias(QueryId, _, _, TableAlias)
        \
        write_update_attribute(QueryId, TableAlias, AttributeName, format(Format, FormatArgs))
        <=>
        in_line_format(QueryId, Format, FormatArgs, ApplicationValue),
        write_update_attribute(QueryId, TableAlias, AttributeName, ApplicationValue).

:-multifile(cql_atomic_value_check_hook/1).
write_atomic_update_attribute @
        query_table_alias(QueryId, Schema, TableName, TableAlias)
        \
        write_update_attribute(QueryId, TableAlias, AttributeName, ApplicationValue)
        <=>
        ( var(ApplicationValue)
        ; atom(ApplicationValue)
        ; integer(ApplicationValue)
        ; rational(ApplicationValue)
        ; cql_atomic_value_check_hook(ApplicationValue)
        ; ApplicationValue == {null}
        ; ApplicationValue == {timestamp}
        ; ApplicationValue == {user_id}
        ; ApplicationValue == {transaction_id}
        )
        |
        ( dbms(Schema, 'Microsoft SQL Server') ->
            write_sql(QueryId,
                      compile,
                      top,
                      [TableAlias, '.', attribute_name(AttributeName), =, ?|T],
                      T,
                      [odbc_parameter(Schema, TableName, AttributeName, ApplicationValue, update_value, _)],
                      [])
        ; dbms(Schema, 'PostgreSQL') ->
            write_sql(QueryId,
                      compile,
                      top,
                      [attribute_name(AttributeName), =, ?|T],
                      T,
                      [odbc_parameter(Schema, TableName, AttributeName, ApplicationValue, update_value, _)],
                      [])
        ; dbms(Schema, 'SQLite') ->
            write_sql(QueryId,
                      compile,
                      top,
                      [attribute_name(AttributeName), =, ?|T],
                      T,
                      [odbc_parameter(Schema, TableName, AttributeName, ApplicationValue, update_value, _)],
                      [])

        ).

write_evaluated_update_attribute @
        query_table_alias(QueryId, Schema, TableName, TableAlias)
        \
        write_update_attribute(QueryId, TableAlias, AttributeName, Expression)
        <=>
        ( dbms(Schema, 'Microsoft SQL Server') ; dbms(Schema, 'PostgreSQL') )
        |
        ( dbms(Schema, 'Microsoft SQL Server') ->
            write_sql(QueryId,
                      compile,
                      top,
                      [TableAlias, '.', attribute_name(AttributeName), =|T],
                      T,
                      [odbc_parameter(Schema, TableName, AttributeName, {null}, evaluated_update_attribute, _)],
                      [])
        ; dbms(Schema, 'PostgreSQL') ->
            write_sql(QueryId,
                      compile,
                      top,
                      [attribute_name(AttributeName), =|T],
                      T,
                      [odbc_parameter(Schema, TableName, AttributeName, {null}, evaluated_update_attribute, _)],
                      [])
        ),
        write_expression(QueryId, Schema, TableName, AttributeName, TableAlias, Expression).

write_evaluated_update_attribute_sqlite @
        query_table_alias(QueryId, Schema, TableName, TableAlias)
        \
        write_update_attribute(QueryId, TableAlias, AttributeName, Expression)
        <=>
        dbms(Schema, 'SQLite')
        |
        % SQLite does not support the above method. Instead of Expression, we must actually put the entire FROM/WHERE clause here in a subquery like
        % attribute_name(AttributeName) = SELECT(Expression FROM .......)
        write_sql(QueryId,
                  compile,
                  top,
                  [attribute_name(AttributeName), =, '(SELECT '|T],
                  T,
                  [odbc_parameter(Schema, TableName, AttributeName, {null}, evaluated_update_attribute, _)],
                  []),
        write_expression(QueryId, Schema, TableName, AttributeName, TableAlias, Expression),
        Tail = [' AND ', TableAlias, '.rowid = ', TableName, '.rowid)'|T3],
        write_sql(QueryId,
                    compile,
                    top,
                    X,
                    T3,
                    Parameters,
                    []),
        find_copy_of_from(QueryId, X, Tail, Parameters).


write_evaluated_update_binary_expression @
        write_expression(QueryId, Schema, TableName, AttributeName, TableAlias, Expression)
        <=>
        nonvar(Expression),
        functor(Expression, Operator, 2),
        memberchk(Operator, [+, -, *, /])
        |
        arg(1, Expression, Lhs),
        arg(2, Expression, Rhs),
        ( dbms(Schema, 'SQLite') ->
            % SQLite defaults to integer arithmetic. Fix this here
            write_sql(QueryId, compile, top, ['(1.0*'|T1], T1, [], [])
        ; otherwise->
            write_sql(QueryId, compile, top, ['('|T1], T1, [], [])
        ),
        write_expression(QueryId, Schema, TableName, AttributeName, TableAlias, Lhs),
        write_sql(QueryId, compile, top, [' ', Operator, ' '|T2], T2, [], []),
        write_expression(QueryId, Schema, TableName, AttributeName, TableAlias, Rhs),
        ( dbms(Schema, 'SQLite') ->
            % SQLite defaults to integer arithmetic.
            write_sql(QueryId, compile, top, ['*1.0)'|T3], T3, [], [])
        ; otherwise->
            write_sql(QueryId, compile, top, [')'|T3], T3, [], [])
        ).

write_evaluated_update_expression_attribute @
        select_attribute(QueryId, select_attribute(_, _, _, TableAlias, AttributeName), 1, SelectAttributeVariableUsed, Variable)
        \
        write_expression(QueryId, _, _, _, _, Variable)
        <=>
        SelectAttributeVariableUsed = select_attribute_variable_used,
        write_sql(QueryId, compile, top, [TableAlias, '.', attribute_name(AttributeName)|T], T, [], []).

write_evaluated_update_expression_constant @
        write_expression(QueryId, _, _, _, _, Constant)
        <=>
        integer(Constant)
        |
        atom_number(ConstantAtom, Constant),
        write_sql(QueryId, compile, top, [ConstantAtom|T], T, [], []).

write_evaluated_update_expression_parameter @
        write_expression(QueryId, Schema, TableName, AttributeName, _, Variable)
        <=>
        var(Variable)
        |
        write_sql(QueryId,
                  compile,
                  top,
                  [?|T],
                  T,
                  [odbc_parameter(Schema, TableName, AttributeName, Variable, evaluated_update_parameter, _)],
                  []).


write_evaluated_update_table_expression_attribute @
        write_expression(QueryId, _, _, _, TableAlias, AttributeName)
        <=>
        write_sql(QueryId, compile, top, [TableAlias, '.', attribute_name(AttributeName)|T], T, [], []).


write_delete_with_no_where_clause @
        restriction_tree(QueryId, where, true)
        \
        phase(QueryId, initial),
        delete_row(QueryId, TableName, _)
        <=>
        write_sql(QueryId, compile, top, ['DELETE'|T1], T1, [], []),
        write_sql(QueryId, compile, join, [' FROM ', table_name(TableName)|T2], T2, [], []),
        phase(QueryId, where).


write_delete_with_where_clause @
        phase(QueryId, initial),
        delete_row(QueryId, TableName, TableAlias)
        <=>
        TableAlias = TableName,
        write_sql(QueryId, compile, top, ['DELETE'|T1], T1, [], []),
        write_sql(QueryId, compile, join, [' FROM ', TableAlias|T2], T2, [], []),
        phase(QueryId, where).


write_select_keyword @
        phase(QueryId, initial)
        <=>
        write_sql(QueryId, compile, top, ['SELECT '|T], T, [], []),
        phase(QueryId, distinct).


write_select_distinct @
        select_distinction(QueryId, DistinctionType)
        \
        phase(QueryId, distinct)
        <=>
        DistinctionType \== no_distinction
        |
        write_sql(QueryId, compile, top, ['DISTINCT '|T], T, [], []),
        phase(QueryId, top).


no_distinct @
        phase(QueryId, distinct)
        <=>
        phase(QueryId, top).


should_not_be_any_distinct_constraints_left_over @
        check_for_orphan_distincts,
        distinct(_, Distinct),
        original_cql(Cql)
        <=>
        throw(format('Unused DISTINCT ~w in CQL: ~w', [Distinct, Cql])).

select_variable_is_used_if_its_a_where_variable @
        select_attribute(_, _, 1, SelectAttributeVariableUsed, Variable),
        where_restriction_variable(Variable)
        ==>
        SelectAttributeVariableUsed = select_attribute_variable_used.

select_variable_is_used_if_its_a_join_variable @
        select_attribute(_, _, 1, SelectAttributeVariableUsed, Variable),
        join_variable(Variable)
        ==>
        SelectAttributeVariableUsed = select_attribute_variable_used.


select_variable_is_used_if_its_a_sub_query_join_variable @
        select_attribute(_, _, 1, SelectAttributeVariableUsed, Variable),
        sub_query_join_variable(Variable)
        ==>
        SelectAttributeVariableUsed = select_attribute_variable_used.

check_for_orphan_select_variables_in_updates @
        check_for_orphan_select_variables_in_updates,
        query_type(QueryId, update),
        select_attribute(QueryId, _, 1, SelectAttributeVariableUsed, Variable),
        original_cql(Cql)
        <=>
        var(SelectAttributeVariableUsed)
        |
        throw(format('Unused SELECT variable ~w in UPDATE in CQL: ~w', [Variable, Cql])).

original_cql_uniqueness @
        original_cql(Cql)
        \
        original_cql(Cql)
        <=>
        true.


statement_location_uniqueness @
        cql_statement_location(FileName, LineNumber)
        \
        cql_statement_location(FileName, LineNumber)
        <=>
        true.


top_n_error @
        top(_, _, N),
        original_cql(Cql)
        ==>
        \+ var(N),
        \+ (integer(N), N >= 0)
        |
        throw(format('The N in top(N) must be an integer and not less than zero but found ~w in CQL: ~w', [N, Cql])).


write_select_top_n @
        phase(QueryId, top),
        top(QueryId, Schema, N)
        <=>
        ( dbms(Schema, 'Microsoft SQL Server') ->
            ( var(N) ->
                write_sql(QueryId,
                          compile,
                          top,
                          ['TOP (?) '|T],
                          T,
                          [odbc_explicit_type_parameter(integer, N, top_value)],
                          [])
            ; otherwise->
                write_sql(QueryId, compile, top, ['TOP ', N, ' '|T], T, [], [])
            )
        ; dbms(Schema, 'PostgreSQL') ->
            limit(QueryId, Schema, N)
        ; dbms(Schema, 'SQLite') ->
            limit(QueryId, Schema, N)
        ),
        phase(QueryId, select_attributes).


no_top @
        phase(QueryId, top)
        <=>
        phase(QueryId, select_attributes).


write_limit @
        write_limit,
        limit(QueryId, Schema, N)
        <=>
        ( ( dbms(Schema, 'PostgreSQL') ; dbms(Schema, 'SQLite') )->
            (var(N)->
               write_sql(QueryId,
                         compile,
                          where,
                         [' LIMIT (?) '|T],
                         T,
                         [odbc_explicit_type_parameter(integer, N, top_value)],
                         [])
            ; otherwise->
                write_sql(QueryId, compile, where, [' LIMIT ', N, ' '|T], T, [], [])
            )
        ; otherwise->
           true).

no_limit @
        write_limit
        <=>
        true.

write_sub_query_select @
        sub_query_select(QueryId)
        \
        phase(QueryId, select_attributes)
        <=>
        write_sql(QueryId, compile, top, [*|T], T, [], []),
        phase(QueryId, from).


write_select_for_insert_attributes @
        phase(QueryId, select_attributes),
        query_table_alias(QueryId, _, _, _)
        \
        select_for_insert_variables([Variable|InsertVariables], InsertTableName)
        <=>
        select_for_insert_variable(QueryId, Variable, InsertTableName),
        select_for_insert_variables(InsertVariables, InsertTableName).


write_select_for_insert_attributes_complete @
        phase(QueryId, select_attributes),
        select_for_insert_variables([], _)
        <=>
        phase(QueryId, from).


write_select_for_insert_count @
        select_for_insert_variable(QueryId, Variable, _),
        select_attribute(QueryId, select_attribute(aggregation(count), _, _, TableAlias, AttributeName), 1, _, Variable)
        <=>
        write_select_attribute(QueryId,
                               compile,
                               ['count(', TableAlias, '.', attribute_name(AttributeName), ')'|T],
                               T,
                               ignore_output).


write_select_for_insert_aggregation @
        select_for_insert_variable(QueryId, Variable, _),
        select_attribute(QueryId, select_attribute(aggregation(AggregationOperator), _, _, TableAlias, AttributeName), 1, _, Variable)
        <=>
        % Why did someone try and do this?!
        %map_database_atom(AggregationOperator, AggregationOperatorUc),
        write_select_attribute(QueryId,
                               compile,
                               [AggregationOperator, '(', TableAlias, '.', attribute_name(AttributeName), ')'|T],
                               T,
                               ignore_output).


write_select_for_insert_select_constant @
        select_for_insert_variable(QueryId, Variable, InsertTableName),
        select_attribute(QueryId, select_attribute(plain, Schema, _, _, AttributeName), 1, _, selection_constant(Variable))
        <=>
        write_select_attribute(QueryId,
                               compile,
                               ['? AS ', attribute_name(AttributeName)|T],
                               T,
                               selection_constant(Schema, InsertTableName, AttributeName, Variable)).


write_select_for_insert_plain_attribute @
        select_for_insert_variable(QueryId, Variable, _),
        select_attribute(QueryId, select_attribute(plain, _, _, TableAlias, AttributeName), 1, _, Variable)
        <=>
        write_select_attribute(QueryId,
                               compile,
                               [TableAlias, '.', attribute_name(AttributeName)|T],
                               T,
                               ignore_output).


write_select_attributes @
        select_attribute(QueryId, _, _, _, _)
        \
        phase(QueryId, select_attributes)
        <=>
        write_select_attributes(QueryId),
        phase(QueryId, from).


write_do_nothing_select @
        phase(QueryId, select_attributes)
        <=>
        write_sql(QueryId, compile, top, [0|T], T, [], [ignore_output]),
        % no human
        phase(QueryId, from).


write_select_count_attribute @
        write_select_attributes(QueryId),
        query_table_alias(QueryId, Schema, TableName, TableAlias)
        \
        select_attribute(QueryId, select_attribute(aggregation(count), Schema, _, TableAlias, AttributeName), 1, _, Variable)
        <=>
        get_data_size(Schema, TableName, AttributeName, Size),
        include_select_attribute(QueryId,
                                 compile,
                                 Size,
                                 ['count(', TableAlias, '.', attribute_name(AttributeName), ')'|T],
                                 T,
                                 count(Variable)).

write_select_avg_attribute_for_postgres @
        write_select_attributes(QueryId),
        query_table_alias(QueryId, Schema, TableName, TableAlias)
        \
        select_attribute(QueryId, select_attribute(aggregation(avg), Schema, _, TableAlias, AttributeName), 1, _, Variable)
        <=>
        dbms(Schema, 'PostgreSQL')
        |
        %map_database_atom(avg, AggregationOperatorUc),
        upcase_atom(avg, AggregationOperatorUc),
        get_data_size(Schema, TableName, AttributeName, Size),
        include_select_attribute(QueryId,
                                 compile,
                                 Size,
                                 [AggregationOperatorUc, '(', TableAlias, '.', attribute_name(AttributeName), ')'|T],
                                 T,
                                 avg(Variable)).

write_select_aggregation_attribute @
        write_select_attributes(QueryId),
        query_table_alias(QueryId, Schema, TableName, TableAlias)
        \
        select_attribute(QueryId, select_attribute(aggregation(AggregationOperator), Schema, _, TableAlias, AttributeName), 1, _, Variable)
        <=>
        get_data_size(Schema, TableName, AttributeName, Size),
        %map_database_atom(AggregationOperator, AggregationOperatorUc),
        upcase_atom(AggregationOperator, AggregationOperatorUc),
        include_select_attribute(QueryId,
                                 compile,
                                 Size,
                                 [AggregationOperatorUc, '(', TableAlias, '.', attribute_name(AttributeName), ')'|T],
                                 T,
                                 output(Schema, TableName, AttributeName, Variable)).


write_top_level_select_attribute @
        write_select_attributes(QueryId),
        query(QueryId, _, top_level_query),
        query_table_alias(QueryId, Schema, TableName, TableAlias)
        \
        select_attribute(QueryId, select_attribute(plain, Schema, _, TableAlias, AttributeName), 1, _, Variable)
        <=>
        get_data_size(Schema, TableName, AttributeName, Size),
        include_select_attribute(QueryId,
                                 if_var(Variable),
                                 Size,
                                 [TableAlias, '.', attribute_name(AttributeName)|T],
                                 T,
                                 output(Schema, TableName, AttributeName, Variable)).



flush_select_attributes @
        write_select_attributes(QueryId)
        ==>
        collect_select_attributes(QueryId, Unsorted),
        keysort(Unsorted, SortedWithKeys),
        cql_strip_sort_keys(SortedWithKeys, Sorted),
        actually_write_select_attributes(QueryId, compile, Sorted).

collect_select_attributes @
        collect_select_attributes(QueryId, Tail),
        include_select_attribute(QueryId, CompileInstruction, Size, Tokens, TokenTail, Attribute)
        <=>
        Tail = [Size-select_info(CompileInstruction, Tokens, TokenTail, Attribute)|NewTail],
        collect_select_attributes(QueryId, NewTail).

finished_collecting_select_attributes @
        collect_select_attributes(_, Tail)
        <=>
        Tail = [].

actually_write_select_attributes(_, _, []).
actually_write_select_attributes(QueryId, _PreviousCompileInstruction, [select_info(CompileInstruction, Tokens, Tail, Attribute)|More]):-
        %instruction_conjunction(PreviousCompileInstruction, CompileInstruction, Conjunction),
        write_select_attribute(QueryId, CompileInstruction, Tokens, Tail, Attribute),
        actually_write_select_attributes(QueryId, CompileInstruction, More).



write_select_attribute_with_leading_comma @
        select_attribute_written(QueryId)
        \
        write_select_attribute(QueryId, CompileInstruction, SqlTokens, Tail, Output)
        <=>
        write_sql(QueryId, CompileInstruction, top, [', '|T], T, [], []),
        write_select_attribute_1(QueryId, CompileInstruction, SqlTokens, Tail, Output).


write_select_attribute_without_leading_comma @
        write_select_attribute(QueryId, _CompileInstruction, SqlTokens, Tail, Output)
        <=>
        write_select_attribute_1(QueryId, compile, SqlTokens, Tail, Output),
        select_attribute_written(QueryId).


write_select_attribute_1a @
        write_select_attribute_1(QueryId, CompileInstruction, SqlTokens, Tail, selection_constant(Schema, TableName, AttributeName, ApplicationValue))
        <=>
        write_sql(QueryId,
                  CompileInstruction,
                  top,
                  SqlTokens,
                  Tail,
                  [odbc_parameter(Schema, TableName, AttributeName, ApplicationValue, insert_value, _)],
                  [ignore_output]).

write_select_attribute_1c @
        write_select_attribute_1(QueryId, CompileInstruction, SqlTokens, Tail, Output)
        <=>
        write_sql(QueryId, CompileInstruction, top, SqlTokens, Tail, [], [Output]).

in_line_join_on @
        create_in_line_joins,
        attribute_binding(QueryId, attribute(Schema, TableAliasA, AttributeNameA), JoinVariableA),
        attribute_binding(QueryId, attribute(Schema, TableAliasB, AttributeNameB), JoinVariableB)
        ==>
        dbms(Schema, 'Microsoft SQL Server'),
        var(JoinVariableA),
        JoinVariableA == JoinVariableB,
        TableAliasA \== TableAliasB
        |
        join_variable(JoinVariableA),
        join_variable(JoinVariableB),
        join_on(TableAliasA, AttributeNameA, TableAliasB, AttributeNameB).


in_line_join_on @
        create_in_line_joins,
        attribute_binding(QueryId, attribute(Schema, TableAliasA, AttributeNameA), JoinVariableA),
        attribute_binding(QueryId, attribute(Schema, TableAliasB, AttributeNameB), JoinVariableB)
        ==>
        dbms(Schema, 'SQLite'),
        var(JoinVariableA),
        JoinVariableA == JoinVariableB,
        TableAliasA \== TableAliasB
        |
        join_variable(JoinVariableA),
        join_variable(JoinVariableB),
        join_on(TableAliasA, AttributeNameA, TableAliasB, AttributeNameB).


/* We can end up with selects mixed in with other types because of how
   translate_select ends up getting called to parse the where clauses of
   other types of query. In this case, we can simply delete all the selects.
*/

a_query_cannot_be_select_and_something_else @
        query_type(QueryId, _)
        \
        query_type(QueryId, select)
        <=>
        true.


in_line_join_on_postgres_select_insert_or_delete @
        create_in_line_joins,
        query_type(QueryId, QueryType),
        attribute_binding(QueryId, attribute(Schema, TableAliasA, AttributeNameA), JoinVariableA),
        attribute_binding(QueryId, attribute(Schema, TableAliasB, AttributeNameB), JoinVariableB)
        ==>
        dbms(Schema, 'PostgreSQL'),
        memberchk(QueryType, [select, insert, delete]),
        var(JoinVariableA),
        JoinVariableA == JoinVariableB,
        TableAliasA \== TableAliasB
        |
        join_variable(JoinVariableA),
        join_variable(JoinVariableB),
        join_on(TableAliasA, AttributeNameA, TableAliasB, AttributeNameB).

in_line_join_on_postgres_update_but_not_target @
        create_in_line_joins,
        attribute_binding(QueryId, attribute(Schema, TableAliasA, AttributeNameA), JoinVariableA),
        attribute_binding(QueryId, attribute(Schema, TableAliasB, AttributeNameB), JoinVariableB),
        update_table_alias(QueryId, _, _, TargetAlias)
        ==>
        dbms(Schema, 'PostgreSQL'),
        var(JoinVariableA),
        JoinVariableA == JoinVariableB,
        TableAliasA \== TableAliasB,
        TableAliasA \== TargetAlias,
        TableAliasB \== TargetAlias
        |
        join_variable(JoinVariableA),
        join_variable(JoinVariableB),
        join_on(TableAliasA, AttributeNameA, TableAliasB, AttributeNameB).

in_line_join_on_postgres_and_is_target @
        create_in_line_joins,
        attribute_binding(QueryId, attribute(Schema, TableAliasA, AttributeNameA), JoinVariableA),
        attribute_binding(QueryId, attribute(Schema, TableAliasB, AttributeNameB), JoinVariableB),
        update_table_alias(QueryId, _, _, TargetAlias)
        ==>
        dbms(Schema, 'PostgreSQL'),
        var(JoinVariableA),
        JoinVariableA == JoinVariableB,
        TableAliasA \== TableAliasB,
        ( TableAliasA == TargetAlias ; TableAliasB == TargetAlias)
        |
        join_variable(JoinVariableA),
        join_variable(JoinVariableB),
        comparison(QueryId, attribute(Schema, TableAliasA, AttributeNameA), ==, attribute(Schema, TableAliasB, AttributeNameB)),
        join_on(TableAliasA, AttributeNameA, TableAliasB, AttributeNameB).

cleanup_create_in_line_joins @
        create_in_line_joins
        <=>
        true.

explicit_join_on_postgres_update @
        create_join_points,
        update_table_alias(QueryId, _, _, TargetAlias),
        attribute_binding(QueryId, attribute(Schema, TableAliasA, AttributeNameA), JoinVariableA),
        attribute_binding(QueryId, attribute(Schema, TableAliasB, AttributeNameB), JoinVariableB)
        \
        % Use up the comparison/4 as we don't want it in the WHERE clause
        comparison(QueryId, JoinVariableA, ==, JoinVariableB)
        <=>
        dbms(Schema, 'PostgreSQL'),
        var(JoinVariableA),
        var(JoinVariableB),
        TableAliasA \== TableAliasB, TableAliasA \== TargetAlias, TableAliasB \== TargetAlias
        |
        join_variable(JoinVariableA),
        join_variable(JoinVariableB),
        join_on(TableAliasA, AttributeNameA, TableAliasB, AttributeNameB).

explicit_join_on_postgres_other @
        create_join_points,
        query_type(QueryId, Type),
        attribute_binding(QueryId, attribute(Schema, TableAliasA, AttributeNameA), JoinVariableA),
        attribute_binding(QueryId, attribute(Schema, TableAliasB, AttributeNameB), JoinVariableB)
        \
        % Use up the comparison/4 as we don't want it in the WHERE clause
        comparison(QueryId, JoinVariableA, ==, JoinVariableB)
        <=>
        dbms(Schema, 'PostgreSQL'),
        memberchk(Type, [select, insert, delete]),
        var(JoinVariableA),
        var(JoinVariableB),
        TableAliasA \== TableAliasB
        |
        join_variable(JoinVariableA),
        join_variable(JoinVariableB),
        join_on(TableAliasA, AttributeNameA, TableAliasB, AttributeNameB).



explicit_join_on @
        create_join_points,
        attribute_binding(QueryId, attribute(Schema, TableAliasA, AttributeNameA), JoinVariableA),
        attribute_binding(QueryId, attribute(Schema, TableAliasB, AttributeNameB), JoinVariableB)
        \
        % Use up the comparison/4 as we don't want it in the WHERE clause
        comparison(QueryId, JoinVariableA, ==, JoinVariableB)
        <=>
        dbms(Schema, 'Microsoft SQL Server'),
        var(JoinVariableA),
        var(JoinVariableB),
        TableAliasA \== TableAliasB
        |
        join_variable(JoinVariableA),
        join_variable(JoinVariableB),
        join_on(TableAliasA, AttributeNameA, TableAliasB, AttributeNameB).

explicit_join_on @
        create_join_points,
        attribute_binding(QueryId, attribute(Schema, TableAliasA, AttributeNameA), JoinVariableA),
        attribute_binding(QueryId, attribute(Schema, TableAliasB, AttributeNameB), JoinVariableB)
        \
        % Use up the comparison/4 as we don't want it in the WHERE clause
        comparison(QueryId, JoinVariableA, ==, JoinVariableB)
        <=>
        dbms(Schema, 'SQLite'),
        var(JoinVariableA),
        var(JoinVariableB),
        TableAliasA \== TableAliasB
        |
        join_variable(JoinVariableA),
        join_variable(JoinVariableB),
        join_on(TableAliasA, AttributeNameA, TableAliasB, AttributeNameB).


avoid_duplicate_join_ons @
        join_on(TableAliasA, AttributeNameA, TableAliasB, AttributeNameB)
        \
        join_on(TableAliasB, AttributeNameB, TableAliasA, AttributeNameA)
        <=>
        true.


search_for_join_aliases_0 @
        join(_, Join, LhsJoin, _, RhsJoin)
        ==>
        search_for_join_aliases(Join, lhs, LhsJoin),
        search_for_join_aliases(Join, rhs, RhsJoin).


search_for_join_aliases_1 @
        join_leaf(Join, TableAlias),
        search_for_join_aliases(JoinParent, Side, Join)
        ==>
        join_alias(JoinParent, Side, TableAlias).


search_for_join_aliases_2 @
        join(_, Join, LhsJoin, _, _),
        search_for_join_aliases(JoinParent, Side, Join)
        ==>
        search_for_join_aliases(JoinParent, Side, LhsJoin).


search_for_join_aliases_3 @
        join(_, Join, _, _, RhsJoin),
        search_for_join_aliases(JoinParent, Side, Join)
        ==>
        search_for_join_aliases(JoinParent, Side, RhsJoin).

% This is for PostgreSQL
% If we have an implicit join in some query QueryId, we get the join for free.
% However, to do the pre-state-change stuff, we still have to build the join in memory
% and capture the ON clause. This formula creates the two join points in SubQueryId
% which is where the on/4 term goes and implicit_join_sql/3 looks.
% Note that this is only used for explicit on clauses.
search_for_join_aliases_4 @
        implicit_join(QueryId, _, SubQueryId),
        update_table_alias(QueryId, _Schema, _TableName, TableAlias)
        ==>
        join_alias(SubQueryId, lhs, TableAlias),
        search_for_join_aliases(SubQueryId, rhs, QueryId).



add_on_from_join_on @
        resolve_join_points,
        join_alias(Join, lhs, TableAliasLhs),
        join_alias(Join, rhs, TableAliasRhs)
        \
        join_on(TableAliasA, AttributeNameA, TableAliasB, AttributeNameB)
        <=>
        ( TableAliasLhs == TableAliasA,
          TableAliasRhs == TableAliasB
        ; TableAliasLhs == TableAliasB,
          TableAliasRhs == TableAliasA
        )
        |
        add_on(Join, TableAliasA-AttributeNameA==TableAliasB-AttributeNameB).


add_ons @
        on(Join, Resolved, On),
        add_on(Join, ExistingOn)
        <=>
        on(Join, Resolved, (ExistingOn, On)).


add_on @
        add_on(Join, ExistingOn)
        <=>
        on(Join, _, ExistingOn).


resolve_join_points @
        resolve_join_points
        \
        on(Join, Resolved, On)
        <=>
        var(Resolved)
        |
        resolve_join_points(Join, On, NewOn),
        on(Join, resolved, NewOn).


cleanup_resolve_join_points @
        resolve_join_points
        <=>
        true.


resolve_join_points_1 @
        resolve_join_points(Join, (Lhs, Rhs), NewOn)
        <=>
        resolve_join_points(Join, Lhs, NewLhs),
        resolve_join_points(Join, Rhs, NewRhs),
        NewOn = (NewLhs, NewRhs).


resolve_join_points_2 @
        resolve_join_points(Join, (Lhs ; Rhs), NewOn)
        <=>
        resolve_join_points(Join, Lhs, NewLhs),
        resolve_join_points(Join, Rhs, NewRhs),
        NewOn = (NewLhs ; NewRhs).


resolve_join_points_3 @
        % Ihese come from in-line (shared varoabel) joins and explicit where-style restrictions
        resolve_join_points(_, TableAliasLhs-AttributeNameLhs==TableAliasRhs-AttributeNameRhs, NewOn)
        <=>
        NewOn = (TableAliasLhs-AttributeNameLhs==TableAliasRhs-AttributeNameRhs).


resolve_join_points_4 @
        % These come from on clauses
        attribute_binding(QueryId, attribute(Schema, TableAliasLhs, AttributeNameLhs), JoinVariableA),
        attribute_binding(QueryId, attribute(Schema, TableAliasRhs, AttributeNameRhs), JoinVariableB),
        join_alias(Join, lhs, TableAliasLhs),
        join_alias(Join, rhs, TableAliasRhs)
        \
        resolve_join_points(Join, A==B, NewOn)
        <=>
        var(A),
        var(B),
        ( (A==B) == (JoinVariableA==JoinVariableB)
        ; (B==A) == (JoinVariableA==JoinVariableB)
        )
        |
        not_a_singleton(JoinVariableA),
        not_a_singleton(JoinVariableB),
        join_variable(JoinVariableA),
        join_variable(JoinVariableB),
        NewOn = (TableAliasLhs-AttributeNameLhs==TableAliasRhs-AttributeNameRhs).


resolve_join_points_5 @
        attribute_binding(_, attribute(_, TableAlias, _), JoinVariable),
        join_alias(Join, _, TableAlias)
        \
        resolve_join_points(Join, On, NewOn)
        <=>
        On =.. [Operator, V, Rhs],
        JoinVariable == V
        |
        not_a_singleton(JoinVariable),
        join_variable(JoinVariable),
        NewOn =.. [Operator, TableAlias-JoinVariable, Rhs].


resolve_join_points_6 @
        attribute_binding(_, attribute(_, TableAlias, _), JoinVariable),
        join_alias(Join, _, TableAlias)
        \
        resolve_join_points(Join, On, NewOn)
        <=>
        On =.. [Operator, Lhs, V],
        JoinVariable == V
        |
        not_a_singleton(JoinVariable),
        join_variable(JoinVariable),
        NewOn =.. [Operator, Lhs, TableAlias-JoinVariable].


resolve_join_points_7 @
        resolve_join_points(_, On, _)
        <=>
        throw(format('Cannot translate ON specification: ~w', [On])).


join_variable_must_be_a_variable @
        join_variable(Variable)
        <=>
        nonvar(Variable)
        |
        true.


join_variable_is_unique @
        join_variable(Variable)
        \
        join_variable(Variable)
        <=>
        true.


update_not_from_source @
        query_table_alias(QueryId, _, _, TableAlias),
        update_table_alias(QueryId, Schema, _, TargetAlias)
        \
        phase(QueryId, Phase)
        <=>
        dbms(Schema, 'PostgreSQL'),
        Phase == from,
        TableAlias \== TargetAlias
        |
        write_sql(QueryId, compile, top, [' FROM '|T], T, [], []),
        write_join(QueryId, QueryId),
        phase(QueryId, where).

update_from_source @
        query_table_alias(QueryId, _, _, _),
        update_table_alias(QueryId, Schema, _, _)
        \
        phase(QueryId, from)
        <=>
        dbms(Schema, 'PostgreSQL')
        |
        phase(QueryId, where).


select_from @
        query_table_alias(QueryId, _, _, _)
        \
        phase(QueryId, from)
        <=>
        write_sql(QueryId, compile, join, [' FROM '|T], T, [], []),
        write_join(QueryId, QueryId),
        phase(QueryId, where).


no_from_statement @
        phase(QueryId, from)
        <=>
        phase(QueryId, where).


% The inclusion of the target alias is implied in Postgres UPDATE ... FROM
write_join_leaf_unless_target @
        join_leaf(Join, TableAlias),
        query_table_alias(QueryId, _, TableName, TableAlias),
        update_table_alias(QueryId, Schema, _, TargetAlias)
        \
        write_join(QueryId, Join)
        <=>
        dbms(Schema, 'PostgreSQL'),
        TargetAlias \== TableAlias
        |
        write_sql(QueryId, compile, join, [table_name(TableName), ' ', TableAlias|T], T, [], []).

write_join_leaf @
        join_leaf(Join, TableAlias),
        query_table_alias(QueryId, Schema, TableName, TableAlias)
        \
        write_join(QueryId, Join)
        <=>
        write_sql(QueryId, compile, join, [table_name(TableName), ' ', TableAlias|T], T, [], []),
        write_lock_hint(QueryId, Schema, TableAlias).


write_lock_hint @
        write_lock_hint(QueryId, Schema, TableAlias),
        nolock(QueryId, TableAlias)
        <=>
        dbms(Schema, 'Microsoft SQL Server')
        |
        write_sql(QueryId, compile, join, [' WITH (NOLOCK)'|T], T, [], []).

% Ignored for PostgreSQL and SQLite
cleanup_write_lock_hint @
        write_lock_hint(_, _, _)
        <=>
        true.



write_join_clause @
        query(QueryId, Schema, _)
        \
        join(QueryId, Join, LhsJoin, JoinType, RhsJoin),
        write_join(QueryId, Join),
        on(Join, _, On)
        <=>
        write_sql(QueryId, compile, join, ['('|T1], T1, [], []),
        write_join(QueryId, LhsJoin),
        write_sql(QueryId, compile, join, [' ', JoinType, ' '|T2], T2, [], []),
        write_join(QueryId, RhsJoin),
        write_sql(QueryId, compile, join, [' ON '|T3], T3, [], []),
        write_join_ons(QueryId, On),
        write_sql(QueryId, compile, join, [')'|T4], T4, [], []),

        ( debugging(index_suggestions) ->
            on_to_where(Schema, On, RestrictionTree),
            cql_suggest_indices(RestrictionTree, QueryId)

        ; otherwise ->
            true
        ).


write_join_on_conjunction @
        write_join_ons(QueryId, (Lhs, Rhs))
        <=>
        write_join_ons(QueryId, Lhs),
        write_sql(QueryId, compile, join, [' AND '|T], T, [], []),
        write_join_ons(QueryId, Rhs).


write_join_on_disjunction @
        write_join_ons(QueryId, (Lhs ; Rhs))
        <=>
        write_sql(QueryId, compile, join, ['('|T1], T1, [], []),
        write_join_ons(QueryId, Lhs),
        write_sql(QueryId, compile, join, [' OR '|T2], T2, [], []),
        write_join_ons(QueryId, Rhs),
        write_sql(QueryId, compile, join, [')'|T3], T3, [], []).


write_join_on_attributes @
        write_join_ons(QueryId, TableAliasLhs-AttributeNameLhs==TableAliasRhs-AttributeNameRhs)
        <=>
        write_sql(QueryId,
                  compile,
                  join,
                  [TableAliasLhs, '.', attribute_name(AttributeNameLhs), =, TableAliasRhs, '.', attribute_name(AttributeNameRhs)|T],
                  T,
                  [],
                  []).

write_join_on_lhs @
        attribute_binding(_, attribute(Schema, TableAlias, AttributeName), Variable)
        \
        write_join_ons(QueryId, On)
        <=>
        var(Variable),
        On =.. [ComparisonOperator, ApplicationValue, Rhs],
        error_on_comparison_operator(Schema, ComparisonOperator),
        Rhs == TableAlias-Variable
        |
        write_restriction(QueryId, compile, join, ApplicationValue, ComparisonOperator, attribute(Schema, TableAlias, AttributeName)).


write_join_on_rhs @
        attribute_binding(_, attribute(Schema, TableAlias, AttributeName), Variable)
        \
        write_join_ons(QueryId, On)
        <=>
        var(Variable),
        On =.. [ComparisonOperator, Lhs, ApplicationValue],
        error_on_comparison_operator(Schema, ComparisonOperator),
        Lhs == TableAlias-Variable
        |
        write_restriction(QueryId, compile, join, attribute(Schema, TableAlias, AttributeName), ComparisonOperator, ApplicationValue).

error_on_comparison_operator(Schema, ComparisonOperator) :-
        ( ground(ComparisonOperator),
          prolog_to_sql_comparison_operator(Schema, ComparisonOperator, _, _) ->
            true
        ;
            throw(format('Invalid comparison operator in JOIN ON: ~w', [ComparisonOperator]))
        ).

query_table_alias_no_duplicates @
        query_table_alias(QueryId, Schema, TableName, TableAlias)
        \
        query_table_alias(QueryId, Schema, TableName, TableAlias)
        <=>
        true.


check_update_where_restriction @
        phase(QueryId, where),
        restriction_tree(QueryId, where, true),
        state_change_query(QueryId, StateChangeType, _, _)
        ==>
        ( StateChangeType == delete
        ; StateChangeType == update
        )
        |
        no_where_restriction(StateChangeType).


no_where_restriction_permitted @
        no_where_restriction(_),
        absence_of_where_restriction_is_deliberate
        <=>
        true.


deletes_and_updates_must_have_a_where_restriction @
        no_where_restriction(StateChangeType)
        <=>
        upcase_atom(StateChangeType, StateChangeTypeUc),
        throw(format('~w has no WHERE restriction - check RESTRICTION variables declared', [StateChangeTypeUc])).


remove_empty_restriction_tree @
        phase(QueryId, where)
        \
        restriction_tree(QueryId, where, true)
        <=>
        true.


where_restriction_clause @
        phase(QueryId, where),
        restriction_tree(QueryId, where, RestrictionTree)
        <=>
        collect_indices(QueryId),
        write_sql(QueryId, compile, where, [' WHERE '|T], T, [], []),
        write_restriction_tree(QueryId, where, RestrictionTree),
        phase(QueryId, group_by).

sqlite_must_have_where @
        % This is because we add to the WHERE clause programmatically to compute subqueries for update expressions
        query(QueryId, Schema, _)
        \
        phase(QueryId, where)
        <=>
        dbms(Schema, 'SQLite')
        |
        write_sql(QueryId, compile, where, [' WHERE 1=1 '|T], T, [], []),
        phase(QueryId, group_by).


no_where_restriction_clause @
        phase(QueryId, where)
        <=>
        phase(QueryId, group_by).


having_restriction_clause @
        phase(QueryId, having),
        restriction_tree(QueryId, having, RestrictionTree)
        <=>
        write_sql(QueryId, compile, having, [' HAVING '|T], T, [], []),
        write_restriction_tree(QueryId, having, RestrictionTree),
        phase(QueryId, order_by).


no_having_restriction_clause @
        phase(QueryId, having)
        <=>
        phase(QueryId, order_by).

suggest_indices @
        write_restriction_tree(QueryId, _, RestrictionTree)
        \
        collect_indices(QueryId)
        <=>
        ( debugging(index_suggestions) ->
            cql_suggest_indices(RestrictionTree, QueryId)

        ; otherwise ->
            true
        ).

write_restriction_clause @
        write_restriction_tree(QueryId, RestrictionType, RestrictionTree)
        <=>
        ( RestrictionTree = or(Lhs, Rhs) ->
            Operator = 'OR'

        ; RestrictionTree = and(Lhs, Rhs) ->
            Operator = 'AND'
        )
        |
        write_sql(QueryId, compile, where, ['('|T1], T1, [], []),
        write_restriction_tree(QueryId, RestrictionType, Lhs),
        write_sql(QueryId, compile, where, [' ', Operator, ' '|T2], T2, [], []),
        write_restriction_tree(QueryId, RestrictionType, Rhs),
        write_sql(QueryId, compile, where, [')'|T3], T3, [], []).

write_restriction_leaf_for_ignore_if_null_on_rhs @
        query_table_alias(_, Schema, _TableName, TableAlias)
        \
        write_restriction_tree(QueryId, RestrictionType, comparison(attribute(Schema, TableAlias, AttributeName), Operator, ignore_if_null(Variable)))
        <=>
        write_restriction(QueryId, if_not_null(Variable), RestrictionType, attribute(Schema, TableAlias, AttributeName), Operator, Variable),
        write_sql(QueryId, if_null(Variable), RestrictionType, ['1 = 1'|T], T, [], []).


write_restriction_leaf_for_ignore_if_null_on_lhs @
        query_table_alias(_, Schema, _TableName, TableAlias)
        \
        write_restriction_tree(QueryId, RestrictionType, comparison(ignore_if_null(Variable), Operator, attribute(Schema, TableAlias, AttributeName)))
        <=>
        write_restriction(QueryId, if_not_null(Variable), RestrictionType, Variable, Operator, attribute(Schema, TableAlias, AttributeName)),
        write_sql(QueryId, if_null(Variable), RestrictionType, ['1 = 1'|T], T, [], []).

write_restriction_leaf_for_if_not_var_on_rhs @
        write_restriction_tree(QueryId, RestrictionType, comparison(Attribute, ==, if_not_var(Variable)))
        <=>
        write_restriction(QueryId, if_not_var(Variable), RestrictionType, Attribute, ==, if_not_var(Variable)),
        write_sql(QueryId, if_var(Variable), RestrictionType, ['1 = 1'|T], T, [], []).

write_restriction_leaf @
        write_restriction_tree(QueryId, RestrictionType, comparison(Lhs, Operator, Rhs))
        <=>
        write_restriction(QueryId, compile, RestrictionType, Lhs, Operator, Rhs).


write_restriction_between_attribute_and_ignore_if_null @
        query_table_alias(_, Schema, TableName, TableAlias)
        \
        write_restriction(QueryId,
                          CompileInstruction,
                          RestrictionType,
                          attribute(Schema, TableAlias, AttributeName),
                          Operator,
                          ignore_if_null(ApplicationValue))
        <=>
        odbc_data_type(Schema, TableName, AttributeName, OdbcDataType)
        |
        write_restriction_1(QueryId,
                            CompileInstruction,
                            RestrictionType,
                            OdbcDataType,
                            _,
                            Schema,
                            TableName,
                            AttributeName,
                            attribute(Schema, TableAlias, AttributeName),
                            Operator,
                            ignore_if_null(TableAlias, ApplicationValue)).

write_restriction_between_attribute_and_if_not_var @
        query_table_alias(_, Schema, TableName, TableAlias)
        \
        write_restriction(QueryId,
                          CompileInstruction,
                          RestrictionType,
                          attribute(Schema, TableAlias, AttributeName),
                          Operator,
                          if_not_var(Variable))
        <=>
        atom(AttributeName),
        odbc_data_type(Schema, TableName, AttributeName, OdbcDataType)
        |
        write_restriction_1(QueryId,
                            CompileInstruction,
                            RestrictionType,
                            OdbcDataType,
                            _,
                            Schema,
                            TableName,
                            AttributeName,
                            attribute(Schema, TableAlias, AttributeName),
                            Operator,
                            if_not_var(Variable)).


not_in_list_comparison_with_no_elements @
        write_restriction(QueryId, CompileInstruction, RestrictionType, _, (\==), [])
        <=>
        true
        |
        write_sql(QueryId, CompileInstruction, RestrictionType, ['1 = 1'|T], T, [], []).

write_list_comparison_with_collation @
        query_table_alias(_, Schema, TableName, TableAlias)
        \
        write_restriction(QueryId, _CompileInstruction, RestrictionType, attribute(Schema, TableAlias, AttributeName), Operator, List)
        <=>
        is_list(List),
        odbc_data_type(Schema, TableName, AttributeName, OdbcDataType),
        collatable_odbc_data_type(OdbcDataType),

        ( Operator == (==) ->
            ListOperator = 'IN'

        ; Operator == (\==) ->
            ListOperator = 'NOT IN'
        )
        |
        % Duplicate the restriction to avoid the index scan that would result from
        % using the collation comparison alone
        % but only for SQL Server
        ( collation(Schema, Collation)->
           write_sql(QueryId, compile, RestrictionType, [TableAlias, '.', attribute_name(AttributeName)|T1], T1, [], []),
           write_sql(QueryId, compile, RestrictionType, [' ', Collation, ' ', ListOperator, ' ('|T2], T2, [], []),
           write_in_list(QueryId, RestrictionType, Schema, TableName, AttributeName, List),
           write_sql(QueryId, compile, RestrictionType, [') AND '|T3], T3, [], [])
        ; otherwise->
           true
        ),
        write_sql(QueryId, compile, RestrictionType, [TableAlias, '.', attribute_name(AttributeName)|T4], T4, [], []),
        write_sql(QueryId, compile, RestrictionType, [' ', ListOperator, ' ('|T5], T5, [], []),
        write_in_list(QueryId, RestrictionType, Schema, TableName, AttributeName, List),
        write_sql(QueryId, compile, RestrictionType, [')'|T6], T6, [], []).

write_list_comparison_without_collation @
        query_table_alias(_, Schema, TableName, TableAlias)
        \
        write_restriction(QueryId, _CompileInstruction, RestrictionType, attribute(Schema, TableAlias, AttributeName), Operator, List)
        <=>
        is_list(List),
        ( Operator == (==) ->
            ListOperator = 'IN'

        ; Operator == (\==) ->
            ListOperator = 'NOT IN'
        )
        |
        write_sql(QueryId, compile, RestrictionType, [TableAlias, '.', attribute_name(AttributeName)|T1], T1, [], []),
        write_sql(QueryId, compile, RestrictionType, [' ', ListOperator, ' ('|T2], T2, [], []),
        write_in_list(QueryId, RestrictionType, Schema, TableName, AttributeName, List),
        write_sql(QueryId, compile, RestrictionType, [')'|T3], T3, [], []).

write_runtime_list @
        query_table_alias(_, Schema, TableName, TableAlias)
        \
        write_restriction(QueryId, _CompileInstruction, RestrictionType, attribute(Schema, TableAlias, AttributeName), Operator, list(List))
        <=>
        SubInstruction = not_empty(List),
        OtherInstruction = empty(List),

        ( Operator == (==) ->
            ListOperator = 'IN'

        ; Operator == (\==) ->
            ListOperator = 'NOT IN'
        ),
        odbc_data_type(Schema, TableName, AttributeName, OdbcDataType),

        ( collatable_odbc_data_type(OdbcDataType), collation(Schema, Collation)->
            true
        ;
            Collation = ''
        ),
        write_sql(QueryId, SubInstruction, RestrictionType, [TableAlias, '.', attribute_name(AttributeName)|T1], T1, [], []),

        write_sql(QueryId,
                  SubInstruction,
                  RestrictionType,
                  [' ', Collation, ' ', ListOperator, ' ('|T2],
                  T2,
                  [],
                  []),
        write_sql(QueryId,
                  list(List),
                  RestrictionType,
                  [?|T3],
                  T3,
                  [odbc_parameter(Schema, TableName, AttributeName, _, where_value, _)],
                  []),
        write_sql(QueryId,
                  SubInstruction,
                  RestrictionType,
                  [')'|T4],
                  T4,
                  [],
                  []),
        write_sql(QueryId,
                  OtherInstruction,
                  RestrictionType,
                  ['1=1'|T5],
                  T5,
                  [],
                  []).




write_rhs_null_comparison @
        write_restriction(QueryId, _CompileInstruction, RestrictionType, attribute(_, TableAlias, AttributeName), Operator, {null})
        <=>
        null_comparison_keywords(Operator, Keywords, Tail)
        |
        write_sql(QueryId, compile, RestrictionType, [TableAlias, '.', attribute_name(AttributeName)|Keywords], Tail, [], []).


write_lhs_null_comparison @
        write_restriction(QueryId, _CompileInstruction, RestrictionType, {null}, Operator, attribute(_, TableAlias, AttributeName))
        <=>
        null_comparison_keywords(Operator, Keywords, Tail)
        |
        write_sql(QueryId, compile, RestrictionType, [TableAlias, '.', attribute_name(AttributeName)|Keywords], Tail, [], []).


null_comparison_keywords(==, [' IS NULL'|T], T).
null_comparison_keywords(\==, [' IS NOT NULL'|T], T).


write_restriction_between_attributes @
        query_table_alias(_, _, TableNameLhs, TableAliasLhs)
        \
        write_restriction(QueryId,
                          CompileInstruction,
                          RestrictionType,
                          attribute(_, TableAliasLhs, AttributeNameLhs),
                          Operator,
                          attribute(_, TableAliasRhs, AttributeNameRhs))
        <=>
        odbc_data_type(Schema, TableNameLhs, AttributeNameLhs, OdbcDataType)
        |
        write_restriction_1(QueryId,
                            CompileInstruction,
                            RestrictionType,
                            OdbcDataType,
                            _,
                            Schema,
                            TableNameLhs,
                            AttributeNameLhs,
                            attribute(_, TableAliasLhs, AttributeNameLhs),
                            Operator,
                            attribute(_, TableAliasRhs, AttributeNameRhs)).


write_restriction_between_attribute_and_aggregation_sub_select @
        query_table_alias(_, _, TableName, TableAlias)
        \
        write_restriction(QueryId,
                          CompileInstruction,
                          RestrictionType,
                          attribute(Schema, TableAlias, AttributeName),
                          Operator,
                          aggregation_sub_query_sql(AggregationTableName, AggregationAttributeName, Sql, Tail, Inputs))
        <=>
        odbc_data_type(Schema, AggregationTableName, AggregationAttributeName, OdbcDataType)
        |
        Tail = [')'|NewTail],
        write_restriction_1(QueryId,
                            CompileInstruction,
                            RestrictionType,
                            OdbcDataType,
                            _,
                            Schema,
                            TableName,
                            AttributeName,
                            attribute(Schema, TableAlias, AttributeName),
                            Operator,
                            tokens_and_parameters(['('|Sql], NewTail, Inputs)).


write_restriction_between_aggregation_sub_select_and_attribute @
        query_table_alias(_, _, TableName, TableAlias)
        \
        write_restriction(QueryId,
                          CompileInstruction,
                          RestrictionType,
                          aggregation_sub_query_sql(AggregationTableName, AggregationAttributeName, Sql, Tail, Inputs),
                          Operator,
                          attribute(Schema, TableAlias, AttributeName))
        <=>
        odbc_data_type(Schema, AggregationTableName, AggregationAttributeName, OdbcDataType)
        |
        Tail = [')'],
        write_restriction_1(QueryId,
                            CompileInstruction,
                            RestrictionType,
                            OdbcDataType,
                            _,
                            Schema,
                            TableName,
                            AttributeName,
                            ['('|Sql]-Inputs,
                            Operator,
                            attribute(Schema, TableAlias, AttributeName)).


write_restriction_between_expression_and_aggregation_sub_select @
        write_restriction(QueryId,
                          CompileInstruction,
                          RestrictionType,
                          Expression,
                          Operator,
                          aggregation_sub_query_sql(AggregationTableName, AggregationAttributeName, Sql, Tail, Inputs))
        <=>
        odbc_data_type(Schema, AggregationTableName, AggregationAttributeName, OdbcDataType)
        |
        Tail = [')'|NewTail],
        write_restriction_1(QueryId,
                            CompileInstruction,
                            RestrictionType,
                            OdbcDataType,
                            _,
                            Schema,
                            AggregationTableName,
                            AggregationAttributeName,
                            Expression,
                            Operator,
                            tokens_and_parameters(['('|Sql], NewTail, Inputs)).


write_restriction_between_aggregation_sub_select_and_expression @
        write_restriction(QueryId,
                          CompileInstruction,
                          RestrictionType,
                          aggregation_sub_query_sql(AggregationTableName, AggregationAttributeName, Sql, Tail, Inputs),
                          Operator,
                          Expression)
        <=>
        odbc_data_type(Schema, AggregationTableName, AggregationAttributeName, OdbcDataType)
        |
        Tail = [')'|NewTail],
        write_restriction_1(QueryId,
                            CompileInstruction,
                            RestrictionType,
                            OdbcDataType,
                            _,
                            Schema,
                            AggregationTableName,
                            AggregationAttributeName,
                            tokens_and_parameters(['('|Sql], NewTail, Inputs),
                            Operator,
                            Expression).


write_restriction_between_expressions @
        write_restriction(QueryId, CompileInstruction, RestrictionType, LhsExpression, Operator, RhsExpression)
        <=>
        RestrictionType \== having   % Could be an aggregation e.g. sum(x) - leave those for the HAVING phase
        |
        ( representative_attribute(LhsExpression+RhsExpression, Schema, TableName, AttributeName) ->
            true
        ; otherwise ->
            throw(format('Cannot find attribute to determine expression data type in ~w or in ~w', [LhsExpression, RhsExpression]))
        ),
        ( odbc_data_type(Schema, TableName, AttributeName, OdbcDataType)->
            true
        ; otherwise->
            throw(format('Could not determine the type of ~w.~w', [TableName, AttributeName]))
        ),
        write_restriction_1(QueryId,
                            CompileInstruction,
                            RestrictionType,
                            OdbcDataType,
                            _,
                            Schema,
                            TableName,
                            AttributeName,
                            LhsExpression,
                            Operator,
                            RhsExpression).


representative_attribute_1 @
        % Find an attribute to determine the data type of Expression
        query_table_alias(_, Schema, TableName, TableAlias)
        \
        representative_attribute(attribute(Schema, TableAlias, AttributeName), RepresentativeSchema, RepresentativeTableName, RepresentativeAttributeName)
        <=>
        RepresentativeSchema = Schema,
        RepresentativeTableName = TableName,
        RepresentativeAttributeName = AttributeName.

representative_attribute_2 @
        representative_attribute(Expression, Schema, TableName, AttributeName)
        <=>
        nonvar(Expression),
        Expression =.. [_|L],

        ( L = [Lhs, Rhs] ->
            ( representative_attribute(Lhs, Schema, TableName, AttributeName)
            ; representative_attribute(Rhs, Schema, TableName, AttributeName)
            )
        ; L = [Expr] ->
            representative_attribute(Expr, Schema, TableName, AttributeName)
        ).


write_having_comparison_between_aggregation_and_null @
        query_table_alias(QueryId, Schema, _, TableAlias),
        attribute_binding(QueryId, attribute(Schema, TableAlias, Aggregation), Variable)
        \
        write_restriction(QueryId, CompileInstruction, RestrictionType, Variable, Operator, {null})
        <=>
        not_a_singleton(Variable),
        null_comparison_keywords(Operator, Keywords, Tail),
        functor(Aggregation, Functor, 1),
        aggregation_operator(Functor),
        arg(1, Aggregation, AttributeName)
        |
        write_sql(QueryId, CompileInstruction, RestrictionType, [Functor, '(', TableAlias, '.', attribute_name(AttributeName), ')'|Keywords], Tail, [], []).

write_having_comparison_between_null_and_aggregation @
        query_table_alias(QueryId, Schema, _, TableAlias),
        attribute_binding(QueryId, attribute(Schema, TableAlias, Aggregation), Variable)
        \
        write_restriction(QueryId, CompileInstruction, RestrictionType, {null}, Operator, Variable)
        <=>
        not_a_singleton(Variable),
        null_comparison_keywords(Operator, Keywords, Tail),
        functor(Aggregation, Functor, 1),
        aggregation_operator(Functor),
        arg(1, Aggregation, AttributeName)
        |
        write_sql(QueryId, CompileInstruction, RestrictionType, [Functor, '(', TableAlias, '.', attribute_name(AttributeName), ')'|Keywords], Tail, [], []).

write_having_comparison_between_aggregation_and_expression @
        query_table_alias(QueryId, Schema, TableName, TableAlias),
        attribute_binding(QueryId, attribute(Schema, TableAlias, Aggregation), Variable)
        \
        write_restriction(QueryId, CompileInstruction, RestrictionType, Variable, Operator, Expression)
        <=>
        not_a_singleton(Variable),
        functor(Aggregation, Functor, 1),
        aggregation_operator(Functor),
        arg(1, Aggregation, AttributeName),
        ( nonvar(Expression),
          Expression = ignore_if_null(ApplicationValue) ->
            IgnoreExpression =.. [Functor, TableAlias, AttributeName],
            E = ignore_if_null(IgnoreExpression, ApplicationValue)

        ; otherwise ->
            E = Expression
        ),
        odbc_data_type(Schema, TableName, AttributeName, OdbcDataType),

        ( Functor == count ->
            OdbcDataTypeOverride = integer

        ; otherwise ->
            true
        )
        |
        write_restriction_1(QueryId,
                            CompileInstruction,
                            RestrictionType,
                            OdbcDataType,
                            OdbcDataTypeOverride,
                            Schema,
                            TableName,
                            AttributeName,
                            tokens_and_parameters([Functor, '(', TableAlias, '.', attribute_name(AttributeName), ')'|Tail], Tail, []),
                            Operator,
                            E).

write_having_comparison_between_expression_and_aggregation @
        query_table_alias(QueryId, Schema, TableName, TableAlias),
        attribute_binding(QueryId, attribute(Schema, TableAlias, Aggregation), Variable)
        \
        write_restriction(QueryId, CompileInstruction, RestrictionType, Expression, Operator, Variable)
        <=>
        functor(Aggregation, Functor, 1),
        aggregation_operator(Functor),
        arg(1, Aggregation, AttributeName),
        ( nonvar(Expression),
          Expression = ignore_if_null(ApplicationValue) ->
            IgnoreExpression =.. [Functor, TableAlias, AttributeName],
            E = ignore_if_null(IgnoreExpression, ApplicationValue)

        ; otherwise ->
            E = Expression
        ),
        odbc_data_type(Schema, TableName, AttributeName, OdbcDataType),

        ( Functor == count ->
            OdbcDataTypeOverride = integer

        ; otherwise ->
            true
        )
        |
        write_restriction_1(QueryId,
                            CompileInstruction,
                            RestrictionType,
                            OdbcDataType,
                            OdbcDataTypeOverride,
                            Schema,
                            TableName,
                            AttributeName,
                            E,
                            Operator,
                            tokens_and_parameters([Functor, '(', TableAlias, '.', attribute_name(AttributeName), ')'|Tail], Tail, [])).

write_restriction_with_collation @  % Prolog-style atom matching i.e. case-sensitive
        write_restriction_1(QueryId, CompileInstruction, RestrictionType, OdbcDataType, OdbcDataTypeOverride, Schema, TableName, AttributeName, Lhs, ComparisonOperator, Rhs)
        <=>
        ( nonvar(OdbcDataTypeOverride) ->
            collatable_odbc_data_type(OdbcDataTypeOverride)

        ; collatable_odbc_data_type(OdbcDataType) ->
            true
        ),

        collatable_operator(ComparisonOperator),
        prolog_to_sql_comparison_operator(Schema, ComparisonOperator, BaseSqlOperator, _)
        |
        % Need to duplicate the restriction to avoid the index scan that would result from
        % using the collation comparison alone
        % But only in SQL Server
        ( RestrictionType == where ->
            instruction_conjunction(CompileInstruction, if_not_null(Variable), C1)
        ; otherwise->
            C1 = CompileInstruction
        ),
        instruction_conjunction(CompileInstruction, if_null(Variable), C2),
        ( collation(Schema, Collation)->
            atomic_list_concat([Collation, ' ', BaseSqlOperator], SqlOperator),
            write_restriction_expression(QueryId, CompileInstruction, RestrictionType, OdbcDataType, OdbcDataTypeOverride, Schema, TableName, AttributeName, Lhs),
            ( ( var(Rhs)->
                  Variable = Rhs
              ; Rhs = if_not_var(Variable)->
                  true
              ; Rhs = equality_restriction(Variable) ->
                  true
              )->
                % Either "= RHS" or " IS NULL" if RHS is {null} at runtime
                write_sql(QueryId, C1, RestrictionType, [' ', SqlOperator, ' '|T1], T1, [], []),
                write_restriction_expression(QueryId, C1, RestrictionType, OdbcDataType, OdbcDataTypeOverride, Schema, TableName, AttributeName, Rhs),
                ( RestrictionType == where ->
                    write_sql(QueryId, C2, RestrictionType, [' IS NULL'|T2], T2, [], [])
                ; otherwise->
                    true
                )
            ; otherwise->
                write_sql(QueryId, CompileInstruction, RestrictionType, [' ', SqlOperator, ' '|T3], T3, [], []),
                write_restriction_expression(QueryId, CompileInstruction, RestrictionType, OdbcDataType, OdbcDataTypeOverride, Schema, TableName, AttributeName, Rhs)
            ),
            write_sql(QueryId, CompileInstruction, RestrictionType, [' AND '|T4], T4, [], [])
        ; otherwise->
            true
        ),
        write_restriction_expression(QueryId, CompileInstruction, RestrictionType, OdbcDataType, OdbcDataTypeOverride, Schema, TableName, AttributeName, Lhs),
        ( ( var(Rhs)->
              Variable = Rhs
           ; Rhs = if_not_var(Variable)->
                  true
          ; Rhs = equality_restriction(Variable) ->
              true
          )->
            % Either "= RHS" or " IS NULL" if RHS is {null} at runtime
            write_sql(QueryId, C1, RestrictionType, [' ', BaseSqlOperator, ' '|T5], T5, [], []),
            write_restriction_expression(QueryId, C1, RestrictionType, OdbcDataType, OdbcDataTypeOverride, Schema, TableName, AttributeName, Rhs),
            ( RestrictionType == where ->
                write_sql(QueryId, C2, RestrictionType, [' IS NULL'|T6], T6, [], [])
            ; otherwise->
                true
            )
        ; otherwise->
            write_sql(QueryId, CompileInstruction, RestrictionType, [' ', BaseSqlOperator, ' '|T7], T7, [], []),
            write_restriction_expression(QueryId, CompileInstruction, RestrictionType, OdbcDataType, OdbcDataTypeOverride, Schema, TableName, AttributeName, Rhs)
        ).


write_restriction_without_collation @
        write_restriction_1(QueryId, CompileInstruction, RestrictionType, OdbcDataType, OdbcDataTypeOverride, Schema, TableName, AttributeName, Lhs, ComparisonOperator, Rhs)
        <=>
        prolog_to_sql_comparison_operator(Schema, ComparisonOperator, SqlOperator, _)
        |
        write_restriction_expression(QueryId, CompileInstruction, RestrictionType, OdbcDataType, OdbcDataTypeOverride, Schema, TableName, AttributeName, Lhs),

        % This sets the RHS to be a 'LikeParameter'. We can undo this later if we discover the RHS is an expression
        ( is_like_operator(ComparisonOperator, LikeOdbcDataType) ->
            OdbcDataTypeOverride = LikeOdbcDataType
        ; otherwise ->
            true
        ),

        ( ( var(Rhs)->
              Variable = Rhs
           ; Rhs = if_not_var(Variable)->
              true
          ; Rhs = equality_restriction(Variable) ->
              true
          )->
            ( RestrictionType == where ->
                instruction_conjunction(CompileInstruction, if_not_null(Variable), C1)
            ; otherwise->
                C1 = CompileInstruction
            ),
            instruction_conjunction(CompileInstruction, if_null(Variable), C2),
            write_sql(QueryId, C1, RestrictionType, [' ', SqlOperator, ' '|T1], T1, [], []),
            write_restriction_expression(QueryId, C1, RestrictionType, OdbcDataType, OdbcDataTypeOverride, Schema, TableName, AttributeName, Rhs),
            ( RestrictionType == where ->
                write_sql(QueryId, C2, RestrictionType, [' IS NULL'|T2], T2, [], [])
            ; otherwise->
                true
            )
        ; otherwise->
            write_sql(QueryId, CompileInstruction, RestrictionType, [' ', SqlOperator, ' '|T3], T3, [], []),
            write_restriction_expression(QueryId, CompileInstruction, RestrictionType, OdbcDataType, OdbcDataTypeOverride, Schema, TableName, AttributeName, Rhs)
        ).


% prolog_to_sql_comparison_operator(Schema,
%                                   ComparisonOperator,
%                                   SqlOperator,
%                                   InverseComparisonOperator)

prolog_to_sql_comparison_operator(_, <, <, >=).
prolog_to_sql_comparison_operator(_, =<, <=, >).
prolog_to_sql_comparison_operator(_, ==, =, \==).
prolog_to_sql_comparison_operator(_, =:=, =, =\=).
prolog_to_sql_comparison_operator(Schema, =~, Operator, \=~):-
        (dbms(Schema, 'Microsoft SQL Server')->
           Operator = 'LIKE'
        ; dbms(Schema, 'SQLite')->
           Operator = 'LIKE'
        ; dbms(Schema, 'PostgreSQL')->
           Operator = 'ILIKE'
        ).
prolog_to_sql_comparison_operator(Schema, \=~, Operator, =~):-
        ( dbms(Schema, 'Microsoft SQL Server')->
            Operator = 'NOT LIKE'
        ; dbms(Schema, 'SQLite')->
            Operator = 'NOT LIKE'
        ; dbms(Schema, 'PostgreSQL')->
            Operator = 'NOT ILIKE'
        ).
prolog_to_sql_comparison_operator(_, \==, <>, ==).
prolog_to_sql_comparison_operator(_, =\=, <>, =:=).
prolog_to_sql_comparison_operator(_, >=, >=, <).
prolog_to_sql_comparison_operator(_, >, >, =<).


collatable_odbc_data_type(char(_)).
collatable_odbc_data_type(varchar(_)).
collatable_odbc_data_type(longvarchar).


collatable_operator(Operator):-
        Operator \== (=~).


is_like_operator((=~), varchar(128)).


collation(Schema, 'COLLATE Latin1_General_CS_AS'):-  % Make CQL case-sensitive
        dbms(Schema, 'Microsoft SQL Server').

collation(Schema, _):-
        ( dbms(Schema, 'PostgreSQL')->
            fail
        ; dbms(Schema, 'SQLite')->
            fail
        ).

write_restriction_expression_1 @
        write_restriction_expression(QueryId, CompileInstruction, RestrictionType, _, _, _, _, _, tokens_and_parameters(Tokens, Tail, OdbcParameters))
        <=>
        write_sql(QueryId,
                  CompileInstruction,
                  RestrictionType,
                  Tokens,
                  Tail,
                  OdbcParameters,
                  []).

write_restriction_expression_2 @
        write_restriction_expression(QueryId, CompileInstruction, RestrictionType, _, _, _, _, _, attribute(_, TableAlias, AttributeName))
        <=>
        write_sql(QueryId,
                  CompileInstruction,
                  RestrictionType,
                  [TableAlias, '.', attribute_name(AttributeName)|T],
                  T,
                  [],
                  []).

write_restriction_expression_3 @
        write_restriction_expression(QueryId, CompileInstruction, RestrictionType, _, OdbcDataTypeOverride, Schema, TableName, AttributeName, equality_restriction(Variable))
        <=>
        write_sql(QueryId,
                  CompileInstruction,
                  RestrictionType,
                  [?|T],
                  T,
                  [odbc_parameter(Schema, TableName, AttributeName, Variable, where_value, OdbcDataTypeOverride)],
                  []).

% This is a ignore_if_null against an aggregation
write_restriction_expression_4a @
        write_restriction_expression(QueryId, CompileInstruction, RestrictionType, _, OdbcDataTypeOverride, Schema, TableName, AttributeName, ignore_if_null(Aggregation, Variable))
        <=>
        functor(Aggregation, Functor, 2),
        aggregation_operator(Functor)
        |
        arg(1, Aggregation, TableAlias),
        arg(2, Aggregation, AttributeName),
        write_sql(QueryId,
                  CompileInstruction,
                  RestrictionType,
                  ['COALESCE(?, ', Functor, '(', TableAlias, '.', attribute_name(AttributeName), '))'|T],
                  T,
                  [odbc_parameter(Schema, TableName, AttributeName, Variable, where_value, OdbcDataTypeOverride)],
                  []).

write_restriction_expression_4 @
        write_restriction_expression(QueryId, CompileInstruction, RestrictionType, _, OdbcDataTypeOverride, Schema, TableName, AttributeName, ignore_if_null(TableAlias, Variable))
        <=>
        write_sql(QueryId,
                  CompileInstruction,
                  RestrictionType,
                  ['COALESCE(?, ', TableAlias, '.', attribute_name(AttributeName), ')'|T],
                  T,
                  [odbc_parameter(Schema, TableName, AttributeName, Variable, where_value, OdbcDataTypeOverride)],
                  []).

write_restriction_expression_5 @
        attribute_binding(QueryId, Attribute, Variable)
        \
        write_restriction_expression(QueryId, CompileInstruction, RestrictionType, OdbcDataType, OdbcDataTypeOverride, Schema, TableName, AttributeName, Variable)
        <=>
        var(Variable)
        |
        where_restriction_variable(Variable),
        write_restriction_expression(QueryId, CompileInstruction, RestrictionType, OdbcDataType, OdbcDataTypeOverride, Schema, TableName, AttributeName, Attribute).

write_restriction_expression_6 @
        write_restriction_expression(QueryId, CompileInstruction, RestrictionType, _, OdbcDataTypeOverride, Schema, TableName, AttributeName, Variable)
        <=>
        atomic_application_value(Variable)
        |
        write_sql(QueryId,
                  CompileInstruction,
                  RestrictionType,
                  [?|T],
                  T,
                  [odbc_parameter(Schema, TableName, AttributeName, Variable, where_value, OdbcDataTypeOverride)],
                  []).

write_restriction_expression_7 @
        write_restriction_expression(QueryId, CompileInstruction, RestrictionType, OdbcDataType, OdbcDataTypeOverride, Schema, TableName, AttributeName, Expression)
        <=>
        Expression =.. [Operator, Rhs],
        restriction_prefix_operator(Operator)
        |
        write_sql(QueryId, CompileInstruction,  RestrictionType, [Operator|T], T, [], []),
        % We unset the OdbcDataTypeOverride here
        write_restriction_expression(QueryId, CompileInstruction, RestrictionType, OdbcDataType, OdbcDataTypeOverride, Schema, TableName, AttributeName, Rhs).

write_restriction_expression_8 @
        write_restriction_expression(QueryId, CompileInstruction, RestrictionType, OdbcDataType, OdbcDataTypeOverride, Schema, TableName, AttributeName, Expression)
        <=>
        Expression =.. [Operator, Lhs, Rhs],
        restriction_expression_operator(Operator)
        |
        ( dbms(Schema, 'SQLite') ->
            write_sql(QueryId, CompileInstruction, RestrictionType, ['(1.0*'|T1], T1, [], [])
        ; otherwise->
            write_sql(QueryId, CompileInstruction, RestrictionType, ['('|T1], T1, [], [])
        ),
        % We unset the OdbcDataTypeOverride here
        write_restriction_expression(QueryId, CompileInstruction, RestrictionType, OdbcDataType, OdbcDataTypeOverride, Schema, TableName, AttributeName, Lhs),
        write_sql(QueryId, CompileInstruction, RestrictionType, [')'|T2], T2, [], []),
        write_sql(QueryId, CompileInstruction, RestrictionType, [Operator|T3], T3, [], []),
        ( dbms(Schema, 'SQLite') ->
            write_sql(QueryId, CompileInstruction, RestrictionType, ['(1.0*'|T4], T4, [], [])
        ; otherwise->
            write_sql(QueryId, CompileInstruction, RestrictionType, ['('|T4], T4, [], [])
        ),
        % We unset the OdbcDataTypeOverride here
        write_restriction_expression(QueryId, CompileInstruction, RestrictionType, OdbcDataType, OdbcDataTypeOverride, Schema, TableName, AttributeName, Rhs),
        write_sql(QueryId, CompileInstruction, RestrictionType, [')'|T5], T5, [], []).

write_restriction_expression_9 @
        write_restriction_expression(QueryId, CompileInstruction, RestrictionType, _OdbcDataType, OdbcDataTypeOverride, Schema, TableName, AttributeName, if_not_var(Var))
        <=>
        instruction_conjunction(CompileInstruction, if_not_var(Var), NewInstruction),
        write_sql(QueryId,
                  NewInstruction,
                  RestrictionType,
                  [?|T],
                  T,
                  [odbc_parameter(Schema, TableName, AttributeName, Var, where_value, OdbcDataTypeOverride)],
                  []).


write_restriction_expression_10 @
        write_restriction_expression(_, _, _, _, _, _, _, _, Expression)
        <=>
        throw(format('Bad restriction expression: ~w', [Expression])).


restriction_prefix_operator(+).
restriction_prefix_operator(-).

restriction_expression_operator(+).
restriction_expression_operator(-).
restriction_expression_operator(/).
restriction_expression_operator(*).


write_sub_query @
        write_restriction_tree(QueryId, RestrictionType, sub_query(SubQueryType, Sql, SqlTail, SubQueryInputs))
        <=>
        ( SubQueryType == exists ->
            Token = 'EXISTS'

        ; SubQueryType == (\+ exists) ->
            Token = 'NOT EXISTS'
        )
        |
        SqlTail = [')'|T],
        write_sql(QueryId, compile, RestrictionType, [Token,  ' ('|Sql], T, SubQueryInputs, []).


write_in_list_1 @
        next_in_list_value_needs_comma(QueryId)
        \
        write_in_list(QueryId, RestrictionType, Schema, TableName, AttributeName, [ApplicationValue|ApplicationValues])
        <=>
        write_sql(QueryId, compile, RestrictionType, [', ?'|T], T, [odbc_parameter(Schema, TableName, AttributeName, ApplicationValue, where_value, _)], []),
        write_in_list(QueryId, RestrictionType, Schema, TableName, AttributeName, ApplicationValues).


write_in_list_2 @
        write_in_list(QueryId, RestrictionType, Schema, TableName, AttributeName, [ApplicationValue|ApplicationValues])
        <=>
        write_sql(QueryId, compile, RestrictionType, [?|T], T, [odbc_parameter(Schema, TableName, AttributeName, ApplicationValue, where_value, _)], []),
        next_in_list_value_needs_comma(QueryId),
        write_in_list(QueryId, RestrictionType, Schema, TableName, AttributeName, ApplicationValues).


write_in_list_3 @
        write_in_list(QueryId, _, _, _, _, []),
        next_in_list_value_needs_comma(QueryId)
        <=>
        true.


write_group_by @
        group_by(QueryId, _)
        \
        phase(QueryId, group_by)
        <=>
        write_sql(QueryId, compile, having, [' GROUP BY '|T], T, [], []),
        write_group_bys(QueryId),
        phase(QueryId, having).


no_group_by @
        phase(QueryId, group_by)
        <=>
        phase(QueryId, having).


write_group_bys @
        write_group_bys(QueryId)
        \
        attribute_for_group_by(QueryId, TableAlias, AttributeName, GroupBy),
        group_by(QueryId, GroupBy)
        <=>
        write_group_by_attribute(QueryId, [TableAlias, '.', AttributeName|T], T).


write_group_by_attribute_with_trailing_comma @
        next_group_by_attribute_needs_comma(QueryId)
        \
        write_group_by_attribute(QueryId, SqlTokens, Tail)
        <=>
        write_sql(QueryId, compile, having, [', '|SqlTokens], Tail, [], []).


write_group_by_attribute_without_trailing_comma @
        write_group_by_attribute(QueryId, SqlTokens, Tail)
        <=>
        write_sql(QueryId, compile, having, SqlTokens, Tail, [], []),
        next_group_by_attribute_needs_comma(QueryId).


should_not_be_any_group_by_constraints_left_over @
        check_for_orphan_group_bys,
        group_by(_, GroupBy),
        original_cql(Cql)
        <=>
        throw(format('Unused GROUP BY ~w in CQL: ~w', [GroupBy, Cql])).


write_order_by @
        order_bys(QueryId, OrderBys)
        \
        phase(QueryId, order_by)
        <=>
        OrderBys \== []
        |
        write_sql(QueryId, compile, having, [' ORDER BY '|T], T, [], []),
        write_order_bys(QueryId, OrderBys),
        phase(QueryId, union).


no_order_by @
        phase(QueryId, order_by)
        <=>
        phase(QueryId, union).


write_order_bys @
        write_order_bys(QueryId, [OrderBy|OrderBys])
        <=>
        write_order_by(QueryId, OrderBy),
        write_order_bys(QueryId, OrderBys).


clean_up_write_order_bys @
        write_order_bys(_, [])
        <=>
        true.


write_order_by_aggregate @
        write_order_by(QueryId, OrderBy),
        attribute_for_order_by(QueryId, TableAlias, AggregationTerm, Variable)
        <=>
        AggregationTerm =.. [AggregationOperator, AttributeName],
        aggregation_operator(AggregationOperator),

        ( OrderBy == +(Variable) ->
            Direction = 'ASC'

        ; OrderBy == -(Variable) ->
            Direction = 'DESC'
        ),
        %map_database_atom(AggregationOperator, AggregationOperatorUc)
        upcase_atom(AggregationOperator, AggregationOperatorUc)
        |
        not_a_singleton(Variable),
        write_order_by_attribute(QueryId, [AggregationOperatorUc, '(', TableAlias, '.', attribute_name(AttributeName), ') ', Direction|T], T).


write_order_by @
        write_order_by(QueryId, OrderBy),
        attribute_for_order_by(QueryId, TableAlias, AttributeName, Variable)
        <=>
        ( OrderBy == +(Variable) ->
            Direction = 'ASC'

        ; OrderBy == -(Variable) ->
            Direction = 'DESC'
        )
        |
        not_a_singleton(Variable),
        write_order_by_attribute(QueryId, [TableAlias, '.', attribute_name(AttributeName), ' ', Direction|T], T).



write_order_by_attribute_with_trailing_comma @
        next_order_by_attribute_needs_comma(QueryId)
        \
        write_order_by_attribute(QueryId, SqlTokens, Tail)
        <=>
        write_sql(QueryId, compile, having, [', '|SqlTokens], Tail, [], []).


write_order_by_attribute_without_trailing_comma @
        write_order_by_attribute(QueryId, SqlTokens, Tail)
        <=>
        write_sql(QueryId, compile, having, SqlTokens, Tail, [], []),
        next_order_by_attribute_needs_comma(QueryId).


should_not_be_any_write_order_by_constraints_left_over @
        check_for_orphan_order_bys,
        write_order_by(_, OrderBy),
        original_cql(Cql)
        <=>
        throw(format('Unused ORDER BY ~w in CQL: ~w', [OrderBy, Cql])).


in_line_formats @
        prior_to_execution \ in_line_format(_, Format, FormatArgs, ApplicationValue) <=>  format(atom(ApplicationValue), Format, FormatArgs).
        prior_to_execution <=> true.


postgres_identity @
        number_of_rows_affected(QueryId, _, _)
        \
        postgres_identity(QueryId, ReturnedIdentity),
        cql_identity(QueryId, _, Identity)
        <=>
        Identity = ReturnedIdentity.

ignored_postgres_identity @
        number_of_rows_affected(QueryId, _, _)
        \
        postgres_identity(QueryId, _)
        <=>
        true.

identity_sql_server @
        number_of_rows_affected(QueryId, Connection, _),
        cql_statement_location(FileName, LineNumber)
        \
        cql_identity(QueryId, Schema, Identity)
        <=>
        dbms(Schema, 'Microsoft SQL Server'),
        odbc_query(Connection, 'SELECT CAST(@@IDENTITY AS INTEGER)', row(ScopeIdentity))
        |
        ( integer(ScopeIdentity) ->
            Identity = ScopeIdentity,
            get_transaction_context(TransactionId, _, AccessToken, _, _),
            cql_access_token_to_user_id(AccessToken, UserId),
            cql_log([], informational, 'CQL\t~w\t~w\t   Inserted row has identity ~w\t(~w:~w)', [UserId, TransactionId, Identity, FileName, LineNumber])
        ; otherwise ->
            cql_error(bad_identity, 'Integer identity value expected but got ~q', [ScopeIdentity])
        ).

identity_sqlite @
        number_of_rows_affected(QueryId, Connection, _),
        cql_statement_location(FileName, LineNumber)
        \
        cql_identity(QueryId, Schema, Identity)
        <=>
        dbms(Schema, 'SQLite'),
        odbc_query(Connection, 'SELECT CAST(last_insert_rowid() AS INTEGER)', row(ScopeIdentity))
        |
        ( integer(ScopeIdentity) ->
            Identity = ScopeIdentity,
            get_transaction_context(TransactionId, _, AccessToken, _, _),
            cql_access_token_to_user_id(AccessToken, UserId),
            cql_log([], informational, 'CQL\t~w\t~w\t   Inserted row has identity ~w\t(~w:~w)', [UserId, TransactionId, Identity, FileName, LineNumber])
        ; otherwise ->
            cql_error(bad_identity, 'Integer identity value expected but got ~q', [ScopeIdentity])
        ).

rows_affected @
        number_of_rows_affected(QueryId, _, N)
        \
        row_count(QueryId, RowCount)
        <=>
        RowCount = N.

post_state_change_statistics @
        number_of_rows_affected(QueryId, _, N)
        \
        cql_state_change_statistics_sql(QueryId, Schema, TableName, update, _, StateChangeAttributeNames, OdbcParameters, _)
        <=>
        process_statistics_post_state_changes(Schema, TableName, StateChangeAttributeNames, OdbcParameters, N).

number_of_rows_affected_cleanup @
        number_of_rows_affected(QueryId, _, _),  cql_state_change_statistics_sql(QueryId, _, _, _, _, _, _, _) <=> true.
        number_of_rows_affected(_, _, _) <=> true.


write_sql @
        sql_statement(QueryId, TokensSoFar, TokensTail, FromTokensSoFar, FromTail, RestrictionTokensSoFar, RestrictionTail, ExistingOdbcParameters, ExistingFromParameters, ExistingOutputs),
        write_sql(QueryId, CompileInstruction, Position, AddTokens, AddTail, OdbcParameters, Outputs)
        <=>
        ( CompileInstruction == compile->
            Addition = AddTokens,
            AdditionTail = AddTail,
            ( Position \== top->
                append(ExistingFromParameters, OdbcParameters, NewFromParameters),
                NewOdbcParameters = ExistingOdbcParameters
            ; otherwise->
                ( var(OdbcParameters)->
                    append(ExistingOdbcParameters, [compile:OdbcParameters], NewOdbcParameters)
                ; otherwise->
                    append(ExistingOdbcParameters, OdbcParameters, NewOdbcParameters)
                ),
                NewFromParameters = ExistingFromParameters
            )
        ; otherwise->
            % FIXME: This is where to keep the tail around so we can flatten quickly at runtime
            Addition = [CompileInstruction:AddTokens|AdditionTail],
            AddTail = [],
            ( OdbcParameters == []->
                NewOdbcParameters = ExistingOdbcParameters,
                NewFromParameters = ExistingFromParameters
            ; otherwise->
                ( Position \== top->
                    append(ExistingFromParameters, [CompileInstruction:OdbcParameters], NewFromParameters),
                    NewOdbcParameters = ExistingOdbcParameters
                ; otherwise->
                    append(ExistingOdbcParameters, [CompileInstruction:OdbcParameters], NewOdbcParameters),
                    NewFromParameters = ExistingFromParameters
                )
            )
        ),
        ( Position == join ->
            FromTail = Addition,
            NewFromTail = AdditionTail,
            NewTokensTail = TokensTail,
            NewRestrictionTail = RestrictionTail
        ; Position == top ->
            TokensTail = Addition,
            NewTokensTail = AdditionTail,
            NewFromTail = FromTail,
            NewRestrictionTail = RestrictionTail
        ; otherwise->
            NewRestrictionTail = AdditionTail,
            RestrictionTail = Addition,
            NewTokensTail = TokensTail,
            NewFromTail = FromTail
        ),
        ( CompileInstruction == compile ->
            append(ExistingOutputs, Outputs, NewOutputs)
        ; Outputs == []->
            NewOutputs = ExistingOutputs
        ; otherwise->
            append(ExistingOutputs, [CompileInstruction:Outputs], NewOutputs)
        ),
        sql_statement(QueryId, TokensSoFar, NewTokensTail, FromTokensSoFar, NewFromTail, RestrictionTokensSoFar, NewRestrictionTail, NewOdbcParameters, NewFromParameters, NewOutputs).


instantiate_aggregation_sub_query @
        query(SubQueryId, _, sub_query)
        \
        phase(SubQueryId, union),
        sql_statement(SubQueryId, SqlTokens, TokensTail, SqlFromTokens, FromTail, SqlRestrictionTokens, RestrictionTail, OdbcParameters, FromParameters, [output(_, TableName, AttributeName, _)]),
        aggregation_sub_query(SubQueryId, AggregationTableName, AggregationAttributeName, SubQuerySqlTokens, SubQueryTail, SubQueryOdbcParameters)
        <=>
        AggregationTableName = TableName,
        AggregationAttributeName = AttributeName,
        TokensTail = SqlFromTokens,
        FromTail = SqlRestrictionTokens,
        SubQueryTail = RestrictionTail,
        SubQuerySqlTokens = SqlTokens,
        append(OdbcParameters, FromParameters, SubQueryOdbcParameters).

instantiate_sub_query @
        query(QueryId, _, sub_query)
        \
        phase(QueryId, union),
        sql_statement(QueryId, SqlTokens, TokensTail, SqlFromTokens, FromTail, SqlRestrictionTokens, RestrictionTail, OdbcParameters, FromParameters, _),
        sub_query(QueryId, SubQuerySqlTokens, SubQuerySqlTail, SubQueryOdbcParameters)
        <=>
        TokensTail = SqlFromTokens,
        FromTail = SqlRestrictionTokens,
        SubQuerySqlTail = RestrictionTail,
        SubQuerySqlTokens = SqlTokens,
        append(OdbcParameters, FromParameters, SubQueryOdbcParameters).

orphan_write_restriction @
        check_query,
        write_restriction(_, _, _, ApplicationValueLhs, Operator, ApplicationValueRhs),
        original_cql(Cql)
        <=>
        throw(format('Unused restriction: ~w ~w ~w in CQL: ~w', [ApplicationValueLhs, Operator, ApplicationValueRhs, Cql])).

% Ignore any join_on which comes for free in PostgreSQL
% (if we do an UPDATE ... FROM) we get a free join to the target
% NB: We can only get implicit_join/2 if dbms is PostgreSQL
ignore_implicit_joins @
        implicit_join(QueryId, Ignore, SubQueryId)
        \
        join_on(TableAliasA, AttributeNameA, TableAliasB, AttributeNameB)
        <=>
        ( Ignore == TableAliasA ; Ignore == TableAliasB)
        |
        add_on(SubQueryId, TableAliasA-AttributeNameA==TableAliasB-AttributeNameB),
        implicit_join_link(QueryId, SubQueryId).

remove_duplicate_implicit_join_links @
        implicit_join_link(QueryId, SubQueryId)
        \
        implicit_join_link(QueryId, SubQueryId)
        <=>
        true.

recover_implicit_join_for_update @
        implicit_join_sql(QueryId, Sql, Tail),
        implicit_join_link(QueryId, SubQueryId),
        on(SubQueryId, _, On)
        <=>
        sql_statement(SubQueryId, A, A, B, B, C, C, [], [], []),
        write_join_ons(SubQueryId, On),
        fetch_implicit_join_sql(SubQueryId, Sql, Tail).

fetch_implicit_join_sql @
        fetch_implicit_join_sql(SubQueryId, Sql, Tail),
        sql_statement(SubQueryId, SqlTokens, TokensTail, SqlFromTokens, FromTail, SqlRestrictionTokens, RestrictionTail, _, _, _)
        <=>
        TokensTail = SqlFromTokens,
        FromTail = SqlRestrictionTokens,
        Tail = RestrictionTail,
        Sql = SqlTokens.

check_for_joins @
        check_query,
        join_on(TableAliasA, AttributeNameA, TableAliasB, AttributeNameB),
        original_cql(Cql)
        <=>
        throw(format('Unused JOIN point ~w (check join operator present) in CQL: ~w',
                     [join_on(TableAliasA, AttributeNameA, TableAliasB, AttributeNameB), Cql])).


check_for_unused_select_bindings @
        % Select bindings should have either been translated into select attributes (i.e. will appear in
        % the SELECT clause) or been explicitly discarded.  Any left behind indicate a problem.
        check_query,
        select_binding(_, X, Attribute, _),
        original_cql(Cql)
        <=>
        throw(format('Unused SELECT binding (missing GROUP BY?): ~n~w ~n~n~w~n~nin CQL: ~w', [X, Attribute, Cql])).


check_for_unused_join_on_comparisons @
        check_query,
        write_join_ons(_, On),
        original_cql(Cql)
        <=>
        throw(format('Unused join ON comparison <~w> in CQL: ~w', [On, Cql])).


check_for_unused_comparisons @
        check_query,
        comparison(_, Lhs, ComparisonOperator, Rhs),
        original_cql(Cql)
        <=>
        throw(format('Unused comparison: ~w ~w ~w in CQL: ~w', [Lhs, ComparisonOperator, Rhs, Cql])).


cleanup_check_query @
        check_query
        <=>
        true.


odbc_state_change_statement_update @
        prepare_odbc_statements,
        sql_statement(QueryId, SqlTokens, TokensTail, SqlFromTokens, FromTail, SqlRestrictionTokens, RestrictionTail, SelectOdbcParameters, FromParameters, Outputs),
        state_change_query(QueryId, StateChangeType, Schema, TableName)
        <=>
        dbms(Schema, 'Microsoft SQL Server'),
        StateChangeType == update
        |
        TokensTail = SqlFromTokens,
        FromTail = SqlRestrictionTokens,
        RestrictionTail = [],
        AllSqlTokens = SqlTokens,
        append(SelectOdbcParameters, FromParameters, OdbcParameters),
        create_cql_pre_state_change_select_sql(QueryId, StateChangeType, SqlFromTokens, TableName, OdbcParameters),
        create_cql_state_change_statistics_sql(QueryId, StateChangeType, SqlFromTokens, TableName, OdbcParameters),
        compile_tokens(AllSqlTokens, Sql),
        cql_odbc_state_change_statement(QueryId, StateChangeType, Schema, TableName, Sql, OdbcParameters, Outputs).


duplicate_from_clause @
        copy_of_from(QueryId, Tokens, Tail, Parameters)
        \
        find_copy_of_from(QueryId, NewTokens, NewTail, NewParameters)
        <=>
        Parameters = NewParameters,
        swap_tail(Tokens, Tail, NewTail, NewTokens).

remove_cycles([], []):- !.
remove_cycles([compile:_|As], [Bs]):- !, remove_cycles(As, Bs).
remove_cycles([A|As], [A|Bs]):- !, remove_cycles(As, Bs).
remove_cycles([_|As], Bs):- remove_cycles(As, Bs).

odbc_state_change_statement_update_sqlite_1 @
        update_table_alias(QueryId, _, _, TargetAlias)
        \
        prepare_odbc_statements,
        sql_statement(QueryId, SqlTokens, TokensTail, SqlFromTokens, FromTail, SqlRestrictionTokens, RestrictionTail, SelectOdbcParameters, FromParameters, Outputs),
        state_change_query(QueryId, StateChangeType, Schema, TableName)
        <=>
        dbms(Schema, 'SQLite'),
        StateChangeType == update
        |
        TokensTail = [' WHERE rowid IN (SELECT ', TargetAlias, '.rowid '|SqlFromTokens],
        FromTail = SqlRestrictionTokens,
        AllSqlTokens = SqlTokens,
        % Yikes. Initially I used copy_term here but the variables need to be shared (except the tail, of course)
        swap_tail(SqlFromTokens, RestrictionTail, [], StateChangeSelectTokens),
        swap_tail(SqlFromTokens, RestrictionTail, CopyFromTail, CopyFrom),
        copy_of_from(QueryId, CopyFrom, CopyFromTail, FromParameters),
        RestrictionTail = [')'],
        append(SelectOdbcParameters, FromParameters, OdbcParameters),
        create_cql_pre_state_change_select_sql(QueryId, StateChangeType, StateChangeSelectTokens, TableName, OdbcParameters),
        create_cql_state_change_statistics_sql(QueryId, StateChangeType, StateChangeSelectTokens, TableName, OdbcParameters),
        compile_tokens(AllSqlTokens, Sql),
        cql_odbc_state_change_statement(QueryId, StateChangeType, Schema, TableName, Sql, OdbcParameters, Outputs).


swap_tail(Var, RestrictionTail, Tail, Tail):-
        RestrictionTail == Var, !.

swap_tail([A|As], Tail, X, [A|Bs]):-
        swap_tail(As, Tail, X, Bs).

odbc_state_change_statement_update @
        query_table_alias(QueryId, _, _, TableAlias),
        update_table_alias(QueryId, _, _, TargetAlias)
        \
        prepare_odbc_statements,
        sql_statement(QueryId, SqlTokens, TokensTail, SqlFromTokens, FromTail, SqlRestrictionTokens, RestrictionTail, SelectOdbcParameters, FromParameters, Outputs),
        state_change_query(QueryId, StateChangeType, Schema, UpdateTableName)
        <=>
        dbms(Schema, 'PostgreSQL'),
        implicit_join_sql(QueryId, ImplicitJoinSQL, [')'|ImplicitJoinTail]),
        % If there is any query_table_alias which is NOT the same as the update_table_alias then the
        % SqlRestrictionTokens will already contain a FROM
        TableAlias \== TargetAlias,
        StateChangeType == update
        |
        append(SelectOdbcParameters, FromParameters, OdbcParameters),
        % This is full of nasty traps. We must build up two very similar but not identical queries. Take care not to instantiate too much before the copy_term/2!
        RestrictionTail = [],
        ImplicitJoinTail = SqlRestrictionTokens,
        copy_term([' FROM '|SqlFromTokens]:FromTail, SqlPreFromTokens:[' INNER JOIN ', UpdateTableName, ' ', TargetAlias, ' ON ('|ImplicitJoinSQL]),
        TokensTail = SqlFromTokens,
        FromTail = SqlRestrictionTokens,
        AllSqlTokens = SqlTokens,

        create_cql_pre_state_change_select_sql(QueryId, StateChangeType, SqlPreFromTokens, UpdateTableName, OdbcParameters),
        create_cql_state_change_statistics_sql(QueryId, StateChangeType, SqlPreFromTokens, UpdateTableName, OdbcParameters),
        % Inject the implicit join here
        % ImplicitJoin = [' INNER JOIN ', TableName, ' ', TableAlias, ' ON ', '(1=1)'],
        compile_tokens(AllSqlTokens, Sql),
        cql_odbc_state_change_statement(QueryId, StateChangeType, Schema, UpdateTableName, Sql, OdbcParameters, Outputs).


odbc_state_change_statement_update @
        query_table_alias(QueryId, _, TableName, TableAlias)
        \
        prepare_odbc_statements,
        sql_statement(QueryId, SqlTokens, TokensTail, SqlFromTokens, FromTail, SqlRestrictionTokens, RestrictionTail, SelectOdbcParameters, FromParameters, Outputs),
        state_change_query(QueryId, StateChangeType, Schema, TableName)
        <=>
        dbms(Schema, 'PostgreSQL'),
        StateChangeType == update
        |
        append(SelectOdbcParameters, FromParameters, OdbcParameters),
        % If there is no query_table_alias which is NOT the same as the update_table_alias then the
        % SqlRestrictionTokens will NOT contain a FROM, so we must add one
        TokensTail = SqlFromTokens,
        FromTail = SqlRestrictionTokens,
        RestrictionTail = [],
        AllSqlTokens = SqlTokens,
        % This case is much simpler than the above
        create_cql_pre_state_change_select_sql(QueryId, StateChangeType, [' FROM ', TableName, ' ', TableAlias, ' '|SqlRestrictionTokens], TableName, OdbcParameters),
        create_cql_state_change_statistics_sql(QueryId, StateChangeType, [' FROM ', TableName, ' ', TableAlias, ' '|SqlRestrictionTokens], TableName, OdbcParameters),
        compile_tokens(AllSqlTokens, Sql),
        cql_odbc_state_change_statement(QueryId, StateChangeType, Schema, TableName, Sql, OdbcParameters, Outputs).


odbc_state_change_statement_not_update @
        prepare_odbc_statements,
        sql_statement(QueryId, SqlTokens, TokensTail, SqlFromTokens, FromTail, SqlRestrictionTokens, RestrictionTail, SelectOdbcParameters, FromParameters, Outputs),
        state_change_query(QueryId, StateChangeType, Schema, TableName)
        <=>
        TokensTail = SqlFromTokens,
        FromTail = SqlRestrictionTokens,
        RestrictionTail = [],
        AllSqlTokens = SqlTokens,
        append(SelectOdbcParameters, FromParameters, OdbcParameters),
        create_cql_pre_state_change_select_sql(QueryId, StateChangeType, SqlFromTokens, TableName, OdbcParameters),
        ( StateChangeType == delete ->
            create_cql_state_change_statistics_sql(QueryId, StateChangeType, SqlFromTokens, TableName, OdbcParameters)
        ; otherwise->
            true
        ),
        compile_tokens(AllSqlTokens, Sql),
        cql_odbc_state_change_statement(QueryId, StateChangeType, Schema, TableName, Sql, OdbcParameters, Outputs).


execute_state_change_query @
        cql_statement_location(FileName, LineNumber)
        \
        cql_execute(OdbcCachingOption),
        cql_odbc_state_change_statement(QueryId, StateChangeType, Schema, TableName, HalfCompiledSql, HalfCompiledOdbcParameters, _)
        <=>
        fully_compile_sql(HalfCompiledSql, HalfCompiledOdbcParameters, [], Sql, OdbcParameters, _),
        get_transaction_context(TransactionId, _, AccessToken, _, Connection),
        execute_on_connection(Schema,
                              Connection,
                              ( debug_before(Sql, Schema, OdbcParameters),
                                identify_pre_state_change_values(QueryId, StateChangeType, Connection),
                                cql_access_token_to_user_id(AccessToken, UserId),
                                ( StateChangeType == insert,
                                  statistic_monitored_attribute(Schema, TableName, _) ->
                                    forall((statistic_monitored_attribute(Schema, TableName, MonitoredAttribute),
                                            memberchk(odbc_parameter(Schema, TableName, MonitoredAttribute, ApplicationValue, insert_value, _), OdbcParameters)),
                                           statistic_monitored_attribute_change(Schema, TableName, MonitoredAttribute, ApplicationValue, 1))
                                ; otherwise ->
                                    true
                                ),
                                odbc_data_types_and_inputs(OdbcParameters, OdbcDataTypes, OdbcInputs),
                                log_state_change(Sql, StateChangeType, OdbcInputs),
                                ( odbc_prepare_and_execute(OdbcCachingOption, Connection, FileName, LineNumber, Sql, OdbcDataTypes, OdbcInputs, Result)->
                                    true
                                ; otherwise->
                                    % Some drivers may return SQL_NO_DATA_FOUND if an UPDATE or DELETE affects no rows. In fact,
                                    % even Microsoft says they are supposed to do this:
                                    % "If SQLExecute executes a searched update, insert, or delete statement that does not
                                    %  affect any rows at the data source, the call to SQLExecute returns SQL_NO_DATA."
                                    %   from http://msdn.microsoft.com/en-us/library/windows/desktop/ms713584(v=vs.85).aspx
                                    Result = affected(0)
                                ),
                                ( Result = affected(N) ->
                                    number_of_rows_affected(QueryId, Connection, N),
                                    cql_log([], informational, 'CQL\t~w\t~w\t   ~w row(s) affected\t(~w:~w)', [UserId, TransactionId, N, FileName, LineNumber]),
                                    ( N > 0 ->
                                        identify_insert_row(StateChangeType, QueryId, Schema, TableName, Connection, Identity),
                                        ( StateChangeType == insert->
                                            DebugResult = identity(Identity)
                                        ; otherwise->
                                            DebugResult = Result
                                        ),
                                        identify_post_state_change_values(QueryId, Connection),
                                        call_row_change_hooks(QueryId, Connection)
                                    ; otherwise->
                                        DebugResult = Result,
                                        cleanup_cql_post_state_change_select_sql(QueryId)
                                    )
                                ; Result = row(Identity), dbms(Schema, 'PostgreSQL')->
                                    postgres_identity(QueryId, Identity),
                                    identify_insert_row(StateChangeType, QueryId, Schema, TableName, Connection, _),
                                    number_of_rows_affected(QueryId, Connection, 1),
                                    cql_log([], informational, 'CQL\t~w\t~w\t   Row inserted. Identity ~w\t(~w:~w)', [UserId, TransactionId, Identity, FileName, LineNumber]),
                                    identify_post_state_change_values(QueryId, Connection),
                                    call_row_change_hooks(QueryId, Connection),
                                    DebugResult = identity(Identity)
                                ),
                                debug_after(exit, DebugResult))).



collect_external_variables @
        phase(QueryId, union),
        sql_statement(QueryId, _, _, _, _, _, _, _, _, HalfCompiledOutputs)
        ==>
        strip_compile_instructions(HalfCompiledOutputs, Outputs),
        union_outputs(QueryId, Outputs, []).

strip_compile_instructions([], []):-!.
strip_compile_instructions([_:Outputs|More], Result):-
        !,
        append(Outputs, O1, Result),
        strip_compile_instructions(More, O1).
strip_compile_instructions([Output|O1], [Output|O2]):-
        strip_compile_instructions(O1, O2).


collect_external_variables_1 @
        conjunction_variable(_, ExternalVariable, ConjunctionVariable)
        \
        union_outputs(QueryId, [output(_, _, _, ConjunctionVariable)|Outputs], ExternalVariables)
        <=>
        union_outputs(QueryId, Outputs, [ExternalVariable|ExternalVariables]).


do_not_create_a_union_if_there_is_an_order_by @
        phase(QueryId, union),
        order_bys(QueryId, _)
        <=>
        true.


union_if_external_variables_the_same_and_there_is_no_order_by @
        phase(QueryIdA, union)
        \
        sql_statement(QueryIdA, SqlTokensA, TokensTailA, SqlFromTokensA, FromTailA, SqlRestrictionTokensA, RestrictionTailA, SelectOdbcParametersA, FromOdbcParametersA, Outputs),
        union_outputs(QueryIdA, [], ExternalVariables),
        phase(QueryIdB, union),
        sql_statement(QueryIdB, SqlTokensB, TokensTailB, SqlFromTokensB, FromTailB, SqlRestrictionTokensB, RestrictionTailB, SelectOdbcParametersB, FromOdbcParametersB, _),
        union_outputs(QueryIdB, [], ExternalVariables)
        <=>
        append(SelectOdbcParametersA, SelectOdbcParametersB, SelectOdbcParameters),
        append(FromOdbcParametersA, FromOdbcParametersB, FromOdbcParameters),
        TokensTailA = SqlFromTokensA,
        FromTailA = SqlRestrictionTokensA,
        RestrictionTailA = [' UNION '|SqlTokensB],
        TokensTailB = SqlFromTokensB,
        FromTailB = SqlRestrictionTokensB,
        RestrictionTailB = NewTail,
        UnionSqlTokens = SqlTokensA,
        sql_statement(QueryIdA, UnionSqlTokens, NewTail, A, A, B, B, SelectOdbcParameters, FromOdbcParameters, Outputs),
        remove_query(QueryIdB, QueryIdA),

        ( debugging(cql(union)) ->
            prolog_load_context(source, FileName),
            prolog_load_context(term_position, TermPosition),
            stream_position_data(line_count, TermPosition, LineNumber),
            debug(cql(union), 'UNION created ~w:~w~n', [FileName, LineNumber])
        ;
            true
        ).


join_has_no_on_clause @
        check_for_unjoined_tables,
        write_join(_, _),
        original_cql(Cql)
        <=>
        throw(format('Generated SQL has a JOIN with no ON clause.  Ensure each table shares a variable with a preceding table: ~w',
                     [Cql])).


update_join_has_no_source_table @
        check_for_unjoined_tables,
        update(_, _, _, TableAlias, _)
        <=>
        var(TableAlias)
        |
        throw(format('UPDATE join has no source table', [])).


odbc_select_statement @
        prepare_odbc_statements
        \
        query(QueryId, Schema, top_level_query),
        sql_statement(QueryId, SqlTokens, TokensTail, SqlFromTokens, FromTail, SqlRestrictionTokens, RestrictionTail, SelectOdbcParameters, FromOdbcParameters, Outputs)
        <=>
        TokensTail = SqlFromTokens,
        FromTail = SqlRestrictionTokens,
        RestrictionTail = [],
        AllSqlTokens = SqlTokens,
        compile_tokens(AllSqlTokens, Sql),
        append(SelectOdbcParameters, FromOdbcParameters, OdbcParameters),
        odbc_select_statement(Schema, Sql, OdbcParameters, Outputs).


odbc_select_disjunction_1 @
        prepare_odbc_statements
        \
        odbc_select_disjunction(Goals),
        odbc_select_statement(Schema, Sql, OdbcParameters, Outputs)
        <=>
        odbc_select_disjunction(cql_odbc_select_statement(Schema, Sql, OdbcParameters, Outputs) ; Goals).


odbc_select_disjunction_2 @
        prepare_odbc_statements
        \
        odbc_select_statement(Schema, Sql, OdbcParameters, Outputs)
        <=>
        odbc_select_disjunction(cql_odbc_select_statement(Schema, Sql, OdbcParameters, Outputs)).


cleanup_prepare_odbc_statements @
        prepare_odbc_statements
        <=>
        true.


select_statements_from_disjunctions @      % CQL runtime needs this.  In compile-time CQL the disjunction is expanded at compile time
        cql_execute(_)
        \
        odbc_select_disjunction(Goals)     % Goals is a disjunction of odbc_select_statement/4's and cql_human_readable_sql/4's
        <=>
        call(Goals).


execute_select_queries @
        cql_statement_location(FileName, LineNumber)
        \
        cql_execute(OdbcCachingOption),
        cql_odbc_select_statement(Schema, HalfCompiledSql, HalfCompiledOdbcParameters, HalfCompiledOutputs)
        <=>
        fully_compile_sql(HalfCompiledSql, HalfCompiledOdbcParameters, HalfCompiledOutputs, Sql, OdbcParameters, Outputs),
        ( transaction_active ->
            get_transaction_context(_, _, _, _, Connection),
            execute_on_connection(Schema,
                                  Connection,
                                  execute_select(Schema, Connection, OdbcCachingOption, FileName, LineNumber, Sql, OdbcParameters, Outputs))

        ; otherwise ->
            odbc_connection_call(Schema,
                                 Connection,
                                 execute_on_connection(Schema,
                                                       Connection,
                                                       execute_select(Schema, Connection, OdbcCachingOption, FileName, LineNumber, Sql, OdbcParameters, Outputs)))
        ).



execute_select(Schema, Connection, OdbcCachingOption, FileName, LineNumber, Sql, OdbcParameters, Outputs) :-
        inline(Sql, OdbcParameters, FinalSql, FinalOdbcParameters),
        %Sql = FinalSql,
        %OdbcParameters = FinalOdbcParameters,
        debug_before(Sql, Schema, OdbcParameters),
        odbc_data_types_and_inputs(FinalOdbcParameters, OdbcDataTypes, OdbcInputs),
        log_select(FinalSql, OdbcInputs),
        setup_call_catcher_cleanup(true,
                                   complete_execution(OdbcCachingOption, Connection, FileName, LineNumber, FinalSql, OdbcDataTypes, OdbcInputs, Outputs),
                                   Reason,
                                   debug_after(Reason, Outputs)),  % This handles fail ok, but not exceptions
        ( var(Reason) ->
            debug_after(exit, Outputs)

        ; otherwise ->
            true
        ).


complete_execution(OdbcCachingOption, Connection, FileName, LineNumber, Sql, OdbcDataTypes, OdbcInputs, Outputs) :-
        odbc_prepare_and_execute(OdbcCachingOption, Connection, FileName, LineNumber, Sql, OdbcDataTypes, OdbcInputs, Result),    % BTP
        Result =.. [row|OdbcOutputs],
        bind_application_values(Outputs, OdbcOutputs),
        bind_variables_to_nulls(Outputs).



%%      execute_on_connection(+Schema, +Connection, +Goal)
%
%       Execute Goal on Connection
%
%       Use odbc_connection_call/3 when you want a connection _|allocated|_.

:-meta_predicate
        execute_on_connection(+, +, 0).

execute_on_connection(_Schema, _Connection, Goal) :-
        prior_to_execution,
        Goal,
        post_execute_cleanup.


%%      cql_temporary_column_name(?Schema, ?DataType, ?ColumnName, ?Type)

cql_temporary_column_name(_, integer, value_integer, int).
cql_temporary_column_name(_, decimal(_, _), value_decimal, decimal(30,30)).
cql_temporary_column_name(_, varchar(_), value_varchar, varchar(8000)).
cql_temporary_column_name(_, timestamp, value_time, timestamp).
cql_temporary_column_name(Schema, bit, value_bit, bit):-dbms(Schema, 'Microsoft SQL Server').
cql_temporary_column_name(Schema, bit, value_bit, smallint):-dbms(Schema, 'PostgreSQL').


post_execute_cleanup @
        post_execute_cleanup, cql_statement_location(_, _) <=> true.


% Convert any remaining variables to nulls e.g. in
%
% x :: [a-A] *== (y :: [a-A, b-B] =*= z :: [b-B])
%
% B will still be free if x and y don't join
bind_variables_to_nulls([]).

bind_variables_to_nulls([Output|Outputs]) :-
        ( Output = output(_, _, _, Variable),
          var(Variable) ->
            Variable = {null}
        ;
            true
        ),
        bind_variables_to_nulls(Outputs).


sanity_check_to_confirm_something_got_generated @
        cql_odbc_state_change_statement(_, _, _, _, _, _, _) \ no_sql_statement_generated <=> true.
        odbc_select_disjunction(_) \ no_sql_statement_generated <=> true.
        no_sql_statement_generated, cql_fully_compiled, original_cql(Cql) <=> throw(format('No SQL statement generated:  {~w}', [Cql])).


error_on_unused_equality_restriction_variable @
        cql_fully_compiled,
        equality_restriction_variable(Variable, EqualityRestrictionVariableUsed),
        original_cql(Cql)
        <=>
        var(EqualityRestrictionVariableUsed)
        |
        throw(format('Unused EQUALITY RESTRICTION variable ~w in CQL: ~w', [Variable, Cql])).


cleanup_original_cql @
        cql_fully_compiled \ original_cql(_) <=> true.
        cql_fully_compiled <=> true.


%!      odbc_prepare_and_execute(+OdbcCachingOption, +Connection, +FileName, +LineNumber, +Sql, +OdbcDataTypes, +OdbcInputs, -Result) is det.

odbc_prepare_and_execute(do_not_cache_odbc_statement, Connection, _, _, Sql, OdbcDataTypes, OdbcInputs, Result) :-
        setup_call_cleanup(odbc_prepare(Connection, Sql, OdbcDataTypes, Statement),
                           odbc_execute_with_statistics(Statement, OdbcInputs, OdbcDataTypes, Result),
                           odbc_free_statement(Statement)).

odbc_prepare_and_execute(cache_odbc_statement, Connection, FileName, LineNumber, Sql, OdbcDataTypes, OdbcInputs, Result) :-
        odbc_execute_with_statement_cache(Connection, FileName, LineNumber, Sql, OdbcInputs, OdbcDataTypes, Result).


%%      inline(+Sql:atom, +OdbcParameters:list, -FinalSql:atom, -FinalOdbcParameters:list)
%
%       SQL Server optimises queries when odbc_prepare/4 is called, not when the query is executed.  This is
%       fine for restrictions where the optimiser can determine an invariant good strategy e.g. select on
%       a primary key.  However it can be sub-optimal for restrictions based on columns which have a small number
%       of allowed values e.g. status values like NEW, PROCESSING, PROCESSED as the query plan relies on knowing
%       the statistical distribution of these values.  If the specific restriction is not known because the
%       restriction value is a parameter, performance-destroying, table-locking table scans can be the result.  To avoid
%       this we move allowed value restictions into the SQL statement and drop the parameter.  This will result in more
%       compiled statements, but hopefully better overall performance.

inline(Sql, OdbcParameters, FinalSql, FinalOdbcParameters) :-
        ( atom_codes(Sql, SqlCodes),
          inline_1(SqlCodes, OdbcParameters, FinalOdbcParameters, FinalSqlCodes, []),
          OdbcParameters \== FinalOdbcParameters -> % to hide debug for queries where nothing changed
            atom_codes(FinalSql, FinalSqlCodes),
            debug(cql(inline), '~w (~w)', [FinalSql, FinalOdbcParameters])

        ; otherwise ->
            FinalSql = Sql,
            FinalOdbcParameters = OdbcParameters
        ).

inline_1([], _, []) -->
        !,
        [].

inline_1([0'?|Codes], [OdbcParameter|OdbcParameters], FinalOdbcParameters) -->  % '
        {( OdbcParameter = odbc_explicit_type_parameter(_, ApplicationValue, where_value)
         ; OdbcParameter = odbc_parameter(_, _, _, ApplicationValue, where_value, _)
         ),
         atom(ApplicationValue),
         cql_domain_allowed_value(_, ApplicationValue),
         !,
         atom_codes(ApplicationValue, ApplicationValueCodes)},

        "'", sql_constant(ApplicationValueCodes), "'",
        inline_1(Codes, OdbcParameters, FinalOdbcParameters).

inline_1([0'?|Codes], [OdbcParameter|OdbcParameters], [OdbcParameter|FinalOdbcParameters]) -->  % '
        !,
        [0'?],   %'
        inline_1(Codes, OdbcParameters, FinalOdbcParameters).

inline_1([Code|Codes], OdbcParameters, FinalOdbcParameters) -->
        [Code],
        inline_1(Codes, OdbcParameters, FinalOdbcParameters).

sql_constant([]) --> [].
sql_constant([0''|Codes]) --> !, "''", sql_constant(Codes).
sql_constant([Code|Codes]) --> [Code], sql_constant(Codes).



odbc_data_types_and_inputs(OdbcParameters, OdbcDataTypes, OdbcInputs) :-
        ( transaction_active ->
            get_transaction_context(TransactionId, _, AccessToken, TransactionTimestamp, _),
            cql_access_token_to_user_id(AccessToken, UserId)

        ; otherwise ->
            UserId = {null},
            TransactionId = {null},
            TransactionTimestamp = {null}
        ),
        odbc_data_types_and_inputs_1(OdbcParameters,
                                     UserId,
                                     TransactionId,
                                     TransactionTimestamp,
                                     OdbcDataTypes,
                                     OdbcInputs).


odbc_data_types_and_inputs_1([], _, _, _, [], []).
odbc_data_types_and_inputs_1([OdbcParameter|OdbcParameters],
                             UserId,
                             TransactionId,
                             TransactionTimestamp,
                             [OdbcDataType|OdbcDataTypes],
                             [OdbcInput|OdbcInputs]) :-

        odbc_data_type_and_input(OdbcParameter,
                                 UserId,
                                 TransactionId,
                                 TransactionTimestamp,
                                 OdbcDataType,
                                 OdbcInput),
        !,
        odbc_data_types_and_inputs_1(OdbcParameters,
                                     UserId,
                                     TransactionId,
                                     TransactionTimestamp,
                                     OdbcDataTypes,
                                     OdbcInputs).

odbc_data_types_and_inputs_1([OdbcParameter|OdbcParameters],
                             UserId,
                             TransactionId,
                             TransactionTimestamp,
                             OdbcDataTypes,
                             OdbcInputs) :-

        ( OdbcParameter = odbc_parameter(_, _, _, _, OdbcParameterUse, _),
          OdbcParameterUse == evaluated_update_attribute ->
            true

        ; OdbcParameter = odbc_explicit_type_parameter(_, _, OdbcParameterUse),
          OdbcParameterUse == evaluated_update_attribute ->
            true

        ; otherwise ->
            throw(error(domain_error(odbc_parameter, OdbcParameter), _))
        ),
        odbc_data_types_and_inputs_1(OdbcParameters,
                                     UserId,
                                     TransactionId,
                                     TransactionTimestamp,
                                     OdbcDataTypes,
                                     OdbcInputs).


%%      odbc_data_type_and_input(+OdbcParameter, +UserId, +TransactionId, +TransactionTimestamp, -OdbcDataType, -OdbcInput)
%
%       This predicate is called at *runtime*
odbc_data_type_and_input(OdbcParameter, UserId, TransactionId, TransactionTimestamp, OdbcDataType, OdbcInput) :-
        ( OdbcParameter = odbc_parameter(Schema, TableName, AttributeName, ApplicationValue, OdbcParameterUse, OdbcDataTypeOverride) ->
            ( nonvar(OdbcDataTypeOverride) ->
                OdbcDataType_1 = OdbcDataTypeOverride   % Used for count(a.x) : the type of count is integer, not the type of the attribute a.x being counted

            ; otherwise ->
                odbc_data_type(Schema, TableName, AttributeName, OdbcDataType_0),
	        sql_odbc_datatype(OdbcDataType_0, OdbcDataType_1)
            )

        ; OdbcParameter = odbc_explicit_type_parameter(ExplicitOdbcDataType, ApplicationValue, OdbcParameterUse) ->
            OdbcDataType_1 = ExplicitOdbcDataType,
            Schema = {null},
            TableName = {null},
            AttributeName = {null}
        ),

        ( OdbcDataType_1 == timestamp ->
            % t7/7's are sent to the database as text to allow millisecond time resolution.  Such resolution
            % is not quite achievable with SQL Server data type datetime (see http://msdn.microsoft.com/en-us/library/ms187819.aspx)
            % but is with datetime2
            OdbcDataType = varchar(128)

        ; OdbcParameterUse == evaluated_update_parameter ->
            % Accordining to http://msdn.microsoft.com/en-us/library/aa258832(SQL.80).aspx maximum precision in SQL Server is 38.
            % Allowing 20 decimal places means numbers up to 10**18 (a million trillion) can be handled.
            OdbcDataType = decimal(38, 20)

        ; otherwise ->
             OdbcDataType = OdbcDataType_1
        ),

        ( OdbcParameterUse == insert_value
        ; OdbcParameterUse == update_value
        ; OdbcParameterUse == where_value
        ; OdbcParameterUse == top_value
        ; OdbcParameterUse == evaluated_update_parameter
        ),

        ( atom(ApplicationValue) ->
            Qualifiers = [check_length_if_atom]

        ; otherwise ->
            Qualifiers = []
        ),

        ( ( AttributeName == inserted_
          ; AttributeName == updated_
          ),
          ApplicationValue == {timestamp} ->
            % FIXME: This discards a lot of possibly useful information
            stamp_date_time(TransactionTimestamp, date(Year, Month, Day, Hour, Minute, Seconds, _Offset, _Timezone, _DST), local),
            Sec is integer(float_integer_part(Seconds)),
            MilliSec is integer(float_fractional_part(Seconds)*1000),
            timestamp_to_unambiguous_atom(timestamp(Year, Month, Day, Hour, Minute, Sec, MilliSec), OdbcInput)

        ; AttributeName == transaction_id_,
          ApplicationValue == {transaction_id} ->
            OdbcInput = TransactionId

        ; ( AttributeName == inserted_by_
          ; AttributeName == updated_by_
          ),
          ApplicationValue == {user_id} ->
            OdbcInput = UserId

        ; ground(ApplicationValue) ->
            application_value_to_odbc_value(ApplicationValue,
                                            OdbcDataType,
                                            Schema,
                                            TableName,
                                            AttributeName,
                                            Qualifiers,
                                            OdbcInput)

        ; nonvar(ApplicationValue),
          ApplicationValue = error(_, _) ->
            application_value_to_odbc_value(ApplicationValue,
                                            OdbcDataType,
                                            Schema,
                                            TableName,
                                            AttributeName,
                                            Qualifiers,
                                            OdbcInput)

        ; nonvar(ApplicationValue),
          ApplicationValue = application_error(_, _, _) ->
            application_value_to_odbc_value(ApplicationValue,
                                            OdbcDataType,
                                            Schema,
                                            TableName,
                                            AttributeName,
                                            Qualifiers,
                                            OdbcInput)

        ; otherwise ->
            % Definitely a runtime exception (so use throw_exception to get full backtrace)
            cql_error(application_value_unmappable, 'The value /~w/ being written to ~w.~w.~w cannot be mapped to a term compatible with the ODBC interface', [ApplicationValue, Schema, TableName, AttributeName])
        ).

sql_odbc_datatype('timestamp without time zone', timestamp) :- !.
sql_odbc_datatype(Type, Type).

bind_application_values([], []).
bind_application_values([ignore_output|Outputs], [_|OdbcOutputs]) :- !,
        bind_application_values(Outputs, OdbcOutputs).
bind_application_values([count(Count)|Outputs], [Count|OdbcOutputs]) :- !,
        bind_application_values(Outputs, OdbcOutputs).
bind_application_values([avg(Avg)|Outputs], [AvgAtom|OdbcOutputs]) :- !,
        atom_to_term(AvgAtom, AvgFloat, _),
        rationalize(AvgFloat, Avg),
        bind_application_values(Outputs, OdbcOutputs).
bind_application_values([Output|Outputs],
                        [OdbcOutput|OdbcOutputs]) :- !,
        ( OdbcOutput == {null} ->
            % Treat nulls as variables so that outer joins like:
            %
            % x :: [a-A] *== y :: [a-A]
            %
            % can succeed.  If there was no matching row in y, then SQL would return y.a = null which would bind A
            % to null ... so the query could never succeed unless a.a was null as well.
            true
        ; Output = output(Schema, TableName, AttributeName, ApplicationValue)->
            odbc_value_to_application_value(Schema, TableName, AttributeName, OdbcOutput, V),
            ( rational(V),
              rational(ApplicationValue) ->
                ApplicationValue =:= V
            ;
                ApplicationValue = V
            )
        ),
        bind_application_values(Outputs, OdbcOutputs).

:- if(current_prolog_flag(bounded, false)).
rationalize(Float, Rat) :-
        Rat is rationalize(Float).
:- else.
rationalize(Float, Float).
:- endif.


determine_update_table_name_in_implicit_join @
        update_table_alias(QueryId, Schema, _, SourceAlias)
        \
        implicit_join(QueryId, @, SubQueryId)
        <=>
        dbms(Schema, 'PostgreSQL'),
        % Don't try and translate it until we can!
        SourceAlias \== (@)
        |
        implicit_join(QueryId, SourceAlias, SubQueryId).

determine_update_table_name @
        update(QueryId, Schema, TableName, _, _)
        \
        attributes_to_check(QueryId, Schema, @, AttributeNameValuePairs)
        <=>
        attributes_to_check(QueryId, Schema, TableName, AttributeNameValuePairs).


determine_update_table_name_again @
        update(QueryId, Schema, TableName, _, _)
        \
        attributes_to_check(QueryId, Schema, (@@), AttributeNameValuePairs)
        <=>
        attributes_to_check(QueryId, Schema, TableName, AttributeNameValuePairs).


determine_update_table_key @
        update(QueryId, Schema, TableName, _, _)
        \
        update_table_key(QueryId, Schema, Key)
        <=>
        % Try and locate a key. First, use the primary key, if possible
        % Failing that, try any unique key
        % Failing that, try an identity
        % Failing that, throw an exception
        ( database_key(Schema, TableName, _, InvolvedColumns, 'primary key')->
            true
        ; database_key(Schema, TableName, _, InvolvedColumns, unique)->
            true
        ; database_key(Schema, TableName, _, InvolvedColumns, identity)->
            true
        ; database_identity(Schema, TableName, InvolvedColumn)->
            InvolvedColumns = [InvolvedColumn]
        ; otherwise->
            cql_error(cannot_join, 'Table ~w does not contain any keys. To do a LEFT OUTER JOIN in an update in PostgreSQL, you must add a key to the table you are trying to update', [TableName])
        ),
        findall(InvolvedColumn-_, member(InvolvedColumn, InvolvedColumns), Key).


duplicate_attributes(StateChangeType,               % +
                     Schema,                        % +
                     TableName,                     % +
                     AttributeNameValuePairs) :-    % +
        attribute_names(AttributeNameValuePairs, AttributeNames),
        cql_duplicates(AttributeNames, Duplicates),
        Duplicates \== [],
        throw(format('Duplicate attributes in CQL ~w: ~w', [StateChangeType, Schema:TableName:Duplicates])).


attribute_names([], []).
attribute_names([AttributeName-_|AttributeNameValuePairs], [AttributeName|AttributeNames]) :-
        attribute_names(AttributeNameValuePairs, AttributeNames).


check_attributes_1 @
        attributes_to_check(_, Schema, TableName, AttributeNameValuePairs)
        <=>
        \+ is_list(AttributeNameValuePairs)
        |
        throw(format('Attribute LIST expected in CQL: ~w', [Schema:TableName:AttributeNameValuePairs])).

check_attributes_2 @
        attributes_to_check(QueryId, Schema, TableName, [AttributeNameValuePair|AttributeNameValuePairs])
        <=>
        attribute_to_check(Schema, TableName, AttributeNameValuePair),
        attributes_to_check(QueryId, Schema, TableName, AttributeNameValuePairs).

check_attributes_3 @
        attributes_to_check(_, _, _, [])
        <=>
        true.

check_attribute_1 @
       attribute_to_check(Schema, TableName, AttributeNameValuePair)
       <=>
       var(AttributeNameValuePair)
       |
       throw(format('Variable found in CQL attribute list: ~w', [Schema:TableName:AttributeNameValuePair])).

check_attribute_2 @
       attribute_to_check(Schema, TableName, AttributeNameValuePair)
       <=>
       AttributeNameValuePair \= _-_
       |
       throw(format('Name-value pair expected in CQL attribute list: ~w', [Schema:TableName:AttributeNameValuePair])).

check_attribute_3 @
       attribute_to_check(Schema, TableName, AttributeName-_)
       <=>
       atom(AttributeName),   % Don't care about aggregations for this test
       \+ cql_data_type(Schema, TableName, AttributeName, _, _, _, _, _, _, _)
       |
       throw(format('Unknown attribute in CQL attribute list: ~w', [Schema:TableName:AttributeName])).

:-multifile(cql_check_value_hook/1).
check_attribute_4 @
       attribute_to_check(_, _, _-Value)
       <=>
       ground(Value),
       cql_check_value_hook(Value)
       |
       throw(format('Invalid constant: ~w', [Value])).

check_attribute_5 @
       attribute_to_check(_, _, _)
       <=>
       true.

log_select @
        cql_statement_location(FileName, LineNumber)
        \
        log_select(Sql, OdbcInputs)
        <=>
        log_selects,
        atomic_list_concat(CqlAtoms, ?, Sql),
        embed_odbc_inputs(CqlAtoms, OdbcInputs, SqlAtoms, []),
        atomic_list_concat(SqlAtoms, SqlWithEmbeddedInputs)
        |
        cql_log([],
               informational,
               'CQL\t\t~w\t(~w:~w)',
               [SqlWithEmbeddedInputs, FileName, LineNumber]).


cleanup_log_select @
        log_select(_, _)
        <=>
        true.


embed_odbc_inputs([Atom], []) --> !,
        [Atom].

embed_odbc_inputs([Atom|Atoms], [OdbcInput|OdbcInputs]) -->
        [Atom],
        odbc_input_atom(OdbcInput),
        embed_odbc_inputs(Atoms, OdbcInputs).


% FIXME: This obviously just discards a lot of information!
odbc_input_atom(date(Year, Month, Day, Hour, Minute, Seconds, _Offset, _Timezone, _DST)) --> !,
        {Sec is integer(float_integer_part(Seconds)),
         MilliSec is integer(float_fractional_part(Seconds)*1000),
         timestamp_to_unambiguous_atom(timestamp(Year, Month, Day, Hour, Minute, Sec, MilliSec), TimeStampAtom)},
        ['\'', TimeStampAtom, '\''].

odbc_input_atom(Atom) -->  % This handle decimals as well, which at this point are atoms e.g. '10.000'
        {atom(Atom), !,
         atomic_list_concat(SubAtoms, '\'', Atom),
         atomic_list_concat(SubAtoms, '\'\'', EscapedAtom)},
         ['\'', EscapedAtom, '\''].

odbc_input_atom(boolean(true)) --> !,
        ['1'].

odbc_input_atom(boolean(false)) --> !,
        ['0'].

odbc_input_atom({null}) --> !,
        [null].

odbc_input_atom(Integer) -->
        [Integer].


log_state_change @
        cql_statement_location(FileName, LineNumber)
        \
        log_state_change(Sql, _, OdbcInputs)
        <=>
        get_transaction_context(TransactionId, _, AccessToken, _, _),
        cql_access_token_to_user_id(AccessToken, UserId),
        cql_log([],
               informational,
               'CQL\t~w\t~w\t   ~w\t~q\t(~w:~w)',
               [UserId, TransactionId, Sql, OdbcInputs, FileName, LineNumber]).


no_state_change_actions @
        no_state_change_actions(QueryId)
        \
        create_cql_pre_state_change_select_sql(QueryId, _, _, _, _)
        <=>
        true.

no_statistics_change_actions @
        no_state_change_actions(QueryId)
        \
        create_cql_state_change_statistics_sql(QueryId, _, _, _, _)
        <=>
        true.

update_attributes([], []).
update_attributes([odbc_parameter(Schema, TableName, AttributeName, _, update_value, _)|In], [[Schema, TableName, AttributeName]|Out]):-
        !,
        update_attributes(In, Out).
update_attributes([_|In], Out):-
        update_attributes(In, Out).

create_cql_state_change_statistics_sql @
        query_table_alias(QueryId, Schema, TableName, TableAlias)
        \
        create_cql_state_change_statistics_sql(QueryId, StateChangeType, SqlFromTokens, TableName, OdbcParameters)
        <=>
        % For an update, the value is only changed if it is included in the update list
        ( StateChangeType == update ->
            update_attributes(OdbcParameters, UpdateAttributes),
            findall(AttributeName,
                    ( member([Schema, TableName, AttributeName], UpdateAttributes),
                      statistic_monitored_attribute(Schema, TableName, AttributeName)),
                    Attributes)
        ; otherwise->
            % For deletes and inserts, it is ALWAYS changed!
            findall(AttributeName,
                    statistic_monitored_attribute(Schema, TableName, AttributeName),
                    Attributes)
        ),
        Attributes \== []
        |
        sql_to_statistic_values_pre_state_change(StateChangeType, Attributes, TableAlias, SelectSql, SqlFromTokens),
        sql_restriction_parameters(OdbcParameters, OdbcRestrictionParameters),
        cql_state_change_statistics_sql(QueryId, Schema, TableName, StateChangeType, SelectSql, Attributes, OdbcParameters, OdbcRestrictionParameters).

sql_restriction_parameters([], []).
sql_restriction_parameters([CompileInstruction:List|T1], [CompileInstruction:L1|T2]):-
        !,
        sql_restriction_parameters(List, L1),
        sql_restriction_parameters(T1, T2).
sql_restriction_parameters([H|T1], [H|T2]) :-
        ( H = odbc_parameter(_, _, _, _, where_value, _)             % See CHR type 'OdbcParameter'
        ; H = odbc_explicit_type_parameter(_, _, where_value)        % See CHR type 'OdbcParameter'
        ),
        !,
        sql_restriction_parameters(T1, T2).

sql_restriction_parameters([_|T1], T2) :-
        sql_restriction_parameters(T1, T2).




no_statistics_changes @
        create_cql_state_change_statistics_sql(_, _, _, _, _)
        <=>
        true.



create_cql_pre_state_change_select_sql_for_deletes @
        query_table_alias(QueryId, Schema, TableName, TableAlias)
        \
        create_cql_pre_state_change_select_sql(QueryId, StateChangeType, SqlFromTokens, TableName, OdbcParametersA)
        <=>
        StateChangeType == delete,
        cql_event_notification_table(Schema, TableName),
        get_primary_key_attribute_name(Schema, TableName, PrimaryKeyAttributeName)
        |
        OdbcParametersB = [odbc_parameter(Schema, TableName, PrimaryKeyAttributeName, _, update_value, _)|OdbcParametersA],
        sql_to_select_values_pre_state_change(StateChangeType, OdbcParametersB, StateChangeAttributeNames, TableAlias, OdbcRestrictionParameters, SelectSql, SqlFromTokens),

        cql_pre_state_change_select_sql(QueryId, Schema, TableName, PrimaryKeyAttributeName, SelectSql, StateChangeAttributeNames, OdbcRestrictionParameters).


create_state_change_select_sql_for_updates @
        update_table_alias(QueryId, Schema, _, TableAlias)
        \
        create_cql_pre_state_change_select_sql(QueryId, StateChangeType, SqlFromTokens, TableName, OdbcParametersA)
        <=>
        StateChangeType == update,
        is_state_change_attribute(StateChangeType, Schema, TableName, _),
        get_primary_key_attribute_name(Schema, TableName, PrimaryKeyAttributeName)
        |
        % Add primary key to list of attributes to be selected if its not already there
        ( memberchk(odbc_parameter(Schema, TableName, PrimaryKeyAttributeName, _, update_value, _), OdbcParametersA) ->
            OdbcParametersB = OdbcParametersA
        ; memberchk(_:odbc_parameter(Schema, TableName, PrimaryKeyAttributeName, _, update_value, _), OdbcParametersA) ->
            OdbcParametersB = OdbcParametersA
        ;
            OdbcParametersB = [odbc_parameter(Schema, TableName, PrimaryKeyAttributeName, _, update_value, _)|OdbcParametersA]
        ),

        sql_to_select_values_pre_state_change(StateChangeType, OdbcParametersB, StateChangeAttributeNames, TableAlias, OdbcRestrictionParameters, SelectSql, SqlFromTokens),
        sql_to_select_values_post_state_change(TableName, StateChangeAttributeNames, PrimaryKeyAttributeName, PostUpdateSelectSql),
        odbc_data_type(Schema, TableName, PrimaryKeyAttributeName, PrimaryKeyOdbcDataType),

        cql_pre_state_change_select_sql(QueryId, Schema, TableName, PrimaryKeyAttributeName, SelectSql, StateChangeAttributeNames, OdbcRestrictionParameters),
        cql_post_state_change_select_sql(QueryId, StateChangeAttributeNames, PrimaryKeyOdbcDataType, PostUpdateSelectSql).

identify_row_arising_from_insert_postgres @
        postgres_identity(QueryId, Identity)
        \
        identify_insert_row(StateChangeType, QueryId, Schema, TableName, _, Value)
        <=>
        StateChangeType == insert,
        cql_event_notification_table(Schema, TableName),
        get_primary_key_attribute_name(Schema, TableName, PrimaryKeyAttributeName)
        |
        Value = Identity,
        updated_row_primary_key(QueryId, StateChangeType, Schema, TableName, PrimaryKeyAttributeName, Identity).


identify_row_arising_from_insert_mssql @
        identify_insert_row(StateChangeType, QueryId, Schema, TableName, Connection, Identity)
        <=>
        dbms(Schema, 'Microsoft SQL Server'),
        StateChangeType == insert,
        cql_event_notification_table(Schema, TableName),
        get_primary_key_attribute_name(Schema, TableName, PrimaryKeyAttributeName)
        |
        compile_tokens(['SELECT CAST(@@IDENTITY AS INTEGER), COLUMNPROPERTY(OBJECT_ID(\'', table_name(TableName), '\'),\'', attribute_name(PrimaryKeyAttributeName), '\',\'IsIdentity\')'], HalfCompiledSql),
        fully_compile_sql(HalfCompiledSql, [], [], Sql, _, _),
        odbc_query(Connection, Sql, row(Identity, IsIdentity)),

        ( IsIdentity == 1 ->
            true
        ;
            throw(format('~w.~w.~w is not an IDENTITY attribute', [Schema, TableName, PrimaryKeyAttributeName]))
        ),

        updated_row_primary_key(QueryId, StateChangeType, Schema, TableName, PrimaryKeyAttributeName, Identity).

% FIXME: SQLite

cleanup_identify_insert_row @
        identify_insert_row(_, _, _, _, _, _)
        <=>
        true.


pre_state_change_select_statement @
        identify_pre_state_change_values(QueryId, StateChangeType, Connection),
        cql_pre_state_change_select_sql(QueryId, Schema, TableName, PrimaryKeyAttributeName, HalfCompiledSelectSql, StateChangeAttributeNames, HalfCompiledOdbcRestrictionParameters)
        <=>
        fully_compile_sql(HalfCompiledSelectSql, HalfCompiledOdbcRestrictionParameters, [], SelectSql, OdbcRestrictionParameters, _),
        odbc_data_types_and_inputs(OdbcRestrictionParameters, OdbcDataTypes, OdbcInputs),
        findall(Row,
                setup_call_cleanup(odbc_prepare(Connection, SelectSql, OdbcDataTypes, PreparedStatement),
                                   odbc_execute_with_statistics(PreparedStatement, OdbcInputs, OdbcDataTypes, Row),
                                   odbc_free_statement(PreparedStatement)),
                Rows),

        updated_rows(QueryId,
                     StateChangeType,
                     pre_state_change,
                     Schema,
                     TableName,
                     PrimaryKeyAttributeName,
                     StateChangeAttributeNames,
                     Rows).


pre_state_change_statistics_statement @
        identify_pre_state_change_values(QueryId, _, Connection),
        cql_state_change_statistics_sql(QueryId, Schema, TableName, _, HalfCompiledSelectSql, StateChangeAttributeNames, _, HalfCompiledOdbcRestrictionParameters)
        ==>
        % In case statistics are not enabled, do not run this query. This is the only really expensive step.
        flag(cql_statistics, 1, 1)
        |
        fully_compile_sql(HalfCompiledSelectSql, HalfCompiledOdbcRestrictionParameters, [], SelectSql, OdbcRestrictionParameters, _),
        odbc_data_types_and_inputs(OdbcRestrictionParameters, OdbcDataTypes, OdbcInputs),
        forall(setup_call_cleanup(odbc_prepare(Connection, SelectSql, OdbcDataTypes, PreparedStatement),
                                  odbc_execute_with_statistics(PreparedStatement, OdbcInputs, OdbcDataTypes, Row),
                                  odbc_free_statement(PreparedStatement)),
               process_statistics_pre_state_changes(StateChangeAttributeNames, Schema, TableName, Row)).

process_statistics_post_state_changes(_, _, [], _, _).
process_statistics_post_state_changes(Schema, TableName, [AttributeName|AttributeNames], Values, N):-
        memberchk(odbc_parameter(Schema, TableName, AttributeName, Value, update_value, _), Values),
        statistic_monitored_attribute_change(Schema, TableName, AttributeName, Value, N),
        process_statistics_post_state_changes(Schema, TableName, AttributeNames, Values, N).

process_statistics_pre_state_changes(StateChangeAttributeNames, Schema, TableName, Row):-
        Row =.. [row|Args],
        process_statistics_pre_state_changes_1(StateChangeAttributeNames, Schema, TableName, Args).

process_statistics_pre_state_changes_1([], _, _, []):- !.
process_statistics_pre_state_changes_1([MonitoredAttribute|Attributes], Schema, TableName, [Status, Count|Args]):-
        statistic_monitored_attribute_change(Schema, TableName, MonitoredAttribute, Status, -Count),
        process_statistics_pre_state_changes_1(Attributes, Schema, TableName, Args).


cleanup_identify_pre_state_change_values @
        identify_pre_state_change_values(_, _, _)
        <=>
        true.


updated_rows @
        updated_rows(QueryId,
                     StateChangeType,   % delete ; update
                     When,
                     Schema,
                     TableName,
                     PrimaryKeyAttributeName,
                     StateChangeAttributeNames,
                     [Row|Rows])
        <=>
        Row =.. [row|OdbcOutputs],

        label_odbc_outputs(StateChangeAttributeNames, OdbcOutputs, Nvps),
        select(PrimaryKeyAttributeName-PrimaryKeyValue, Nvps, LabelledOdbcOutputs),

        ( When == pre_state_change ->
            updated_row_primary_key(QueryId, StateChangeType, Schema, TableName, PrimaryKeyAttributeName, PrimaryKeyValue)
        ;
            true
        ),

        updated_row(QueryId,
                    StateChangeType,
                    When,
                    Schema,
                    TableName,
                    PrimaryKeyAttributeName,
                    PrimaryKeyValue,
                    LabelledOdbcOutputs),

        updated_rows(QueryId,
                     StateChangeType,
                     When,
                     Schema,
                     TableName,
                     PrimaryKeyAttributeName,
                     StateChangeAttributeNames,
                     Rows).

cleanup_updated_rows @
        updated_rows(_, _, _, _, _, _, _, [])
        <=>
        true.


state_change_value_from_updated_row @
        updated_row(QueryId,
                    StateChangeType,
                    When,
                    Schema,
                    TableName,
                    PrimaryKeyAttributeName,
                    PrimaryKeyValue,
                    [AttributeName-OdbcOutput|LabelledOdbcOutputs])
        <=>
        state_change_value(QueryId,
                           StateChangeType,
                           When,
                           Schema,
                           TableName,
                           PrimaryKeyAttributeName,
                           PrimaryKeyValue,
                           AttributeName,
                           OdbcOutput),
        updated_row(QueryId,
                    StateChangeType,
                    When,
                    Schema,
                    TableName,
                    PrimaryKeyAttributeName,
                    PrimaryKeyValue,
                    LabelledOdbcOutputs).

cleanup_updated_row @
        % Should get here only for deletes (because no attributes are changed in a delete)
        updated_row(_, _, _, _, _, _, _, _)
        <=>
        true.

sql_to_statistic_values_pre_state_change(StateChangeType, StateChangeAttributeNames, TableAlias, SelectSql, SqlTokens):-
        sql_to_statistic_values_pre_state_change_1(StateChangeType, StateChangeAttributeNames, StateChangeAttributeNames, TableAlias, SelectSqlTokens, SqlTokens),
        group_by_clause(StateChangeType, StateChangeAttributeNames, TableAlias, GroupSqlTokens, []),
        append([SelectSqlTokens, GroupSqlTokens], QueryTokens),
        compile_tokens(QueryTokens, SelectSql).

group_by_clause(StateChangeType, StateChangeAttributeNames, TableAlias)-->
        [' GROUP BY '],
        state_change_select_statistic_sql_group(StateChangeType, StateChangeAttributeNames, TableAlias).

sql_to_statistic_values_pre_state_change_1(StateChangeType, StateChangeAttributeNames, StateChangeAttributeNames, TableAlias) -->
        ['SELECT '],
        state_change_select_statistic_sql(StateChangeType, StateChangeAttributeNames, TableAlias).


sql_to_select_values_pre_state_change(StateChangeType, OdbcParameters, StateChangeAttributeNames, TableAlias, OdbcRestrictionParameters, SelectSql, SqlTokens) :-
        sql_to_select_values_pre_state_change_1(StateChangeType, OdbcParameters, StateChangeAttributeNames, TableAlias, OdbcRestrictionParameters, SelectSqlTokens, SqlTokens),
        compile_tokens(SelectSqlTokens, SelectSql).


sql_to_select_values_pre_state_change_1(delete, OdbcParameters, [PrimaryKeyAttributeName], _, OdbcRestrictionParameters) --> !,
        {odbc_parameters_for_state_change(OdbcParameters, delete, [_-PrimaryKeyAttributeName], OdbcRestrictionParameters, [])},
        ['SELECT ', attribute_name(PrimaryKeyAttributeName)].

sql_to_select_values_pre_state_change_1(update, OdbcParameters, StateChangeAttributeNames, TableAlias, OdbcRestrictionParameters) -->
        {odbc_parameters_for_state_change(OdbcParameters, update, KeyedStateChangeAttributeNames, OdbcRestrictionParameters, []),
        keysort(KeyedStateChangeAttributeNames, SortedKeyedStateChangeAttributeNames),
        cql_strip_sort_keys(SortedKeyedStateChangeAttributeNames, StateChangeAttributeNames)},
        ['SELECT '],
        state_change_select_select_sql(StateChangeAttributeNames, TableAlias).


odbc_parameters_for_state_change([], _, [], Tail, Tail).
odbc_parameters_for_state_change([CompileInstruction:ListOfInstructions|Parameters],
                                 StateChangeType,
                                 AttributeNames,
                                 Output,
                                 Tail):-
        % Preserve the compile instructions
        !,
        odbc_parameters_for_state_change(ListOfInstructions, StateChangeType, AttributeNames, NewList, []),
        ( NewList == [] ->
            Output = T1
        ; otherwise->
            Output = [CompileInstruction:NewList|T1]
        ),
        odbc_parameters_for_state_change(Parameters, StateChangeType, AttributeNames, T1, Tail).


odbc_parameters_for_state_change([odbc_parameter(Schema, TableName, AttributeName, _, OdbcParameterUse, _)|OdbcParameters],  % +
                                 StateChangeType,                                                                            % +
                                 [SortKey-AttributeName|AttributeNames],                                                     % ?
                                 OdbcRestrictionParameters,                                                                  % ?
                                 Tail) :-                                                                                    % ?
        ( OdbcParameterUse == update_value
        ; OdbcParameterUse == evaluated_update_attribute
        ),

        ( is_state_change_attribute(StateChangeType, Schema, TableName, AttributeName)
        ; primary_key_column_name(Schema, TableName, AttributeName)
        ), !,

        cql_data_type(Schema, TableName, AttributeName, _, CharacterMaximumLength, _, _, _, _, _),

        % Get this so we can select longest fields last and so avoid the INVALID DESCRIPTOR INDEX bug
        ( integer(CharacterMaximumLength) ->
            SortKey = CharacterMaximumLength
        ; CharacterMaximumLength == max->
            SortKey = 65535
        ; otherwise->
            SortKey = 0
        ),

        odbc_parameters_for_state_change(OdbcParameters, StateChangeType, AttributeNames, OdbcRestrictionParameters, Tail).


odbc_parameters_for_state_change([odbc_parameter(Schema, TableName, AttributeName, ApplicationValue, where_value, OdbcDataTypeOverride)|OdbcParameters],
                                 StateChangeType,
                                 StateChangeAttributeNames,
                                 [odbc_parameter(Schema, TableName, AttributeName, ApplicationValue, where_value, OdbcDataTypeOverride)|OdbcRestrictionParameters],
                                 Tail) :- !,
        odbc_parameters_for_state_change(OdbcParameters, StateChangeType, StateChangeAttributeNames, OdbcRestrictionParameters, Tail).

odbc_parameters_for_state_change([odbc_explicit_type_parameter(Type, ApplicationValue, where_value)|OdbcParameters],
                                 StateChangeType,
                                 StateChangeAttributeNames,
                                 [odbc_explicit_type_parameter(Type, ApplicationValue, where_value)|OdbcRestrictionParameters],
                                 Tail) :- !,
        odbc_parameters_for_state_change(OdbcParameters, StateChangeType, StateChangeAttributeNames, OdbcRestrictionParameters, Tail).


odbc_parameters_for_state_change([_|OdbcParameters],
                                 StateChangeType,
                                 StateChangeAttributeNames,
                                 OdbcRestrictionParameters,
                                 Tail) :-
        odbc_parameters_for_state_change(OdbcParameters,
                                         StateChangeType,
                                         StateChangeAttributeNames,
                                         OdbcRestrictionParameters,
                                         Tail).


state_change_select_select_sql([AttributeName], TableAlias) --> !,
        [TableAlias, '.', attribute_name(AttributeName)].

state_change_select_select_sql([AttributeName|Outputs], TableAlias) -->
        [TableAlias, '.', attribute_name(AttributeName), ', '],
        state_change_select_select_sql(Outputs, TableAlias).


state_change_select_statistic_sql(update, [AttributeName], TableAlias) --> !,
        [TableAlias, '.', attribute_name(AttributeName), ', count(', TableAlias, '.', attribute_name(AttributeName), ')'].

state_change_select_statistic_sql(update, [AttributeName|Outputs], TableAlias) -->
        [TableAlias, '.', attribute_name(AttributeName), ', count(', TableAlias, '.', attribute_name(AttributeName), '), '],
        state_change_select_statistic_sql(update, Outputs, TableAlias).

state_change_select_statistic_sql(delete, [AttributeName], _) --> !,
        [attribute_name(AttributeName), ', count(', attribute_name(AttributeName), ')'].

state_change_select_statistic_sql(delete, [AttributeName|Outputs], TableAlias) -->
        [attribute_name(AttributeName), ', count(', attribute_name(AttributeName), '), '],
        state_change_select_statistic_sql(delete, Outputs, TableAlias).


state_change_select_statistic_sql_group(update, [AttributeName], TableAlias) --> !,
        [TableAlias, '.', attribute_name(AttributeName)].

state_change_select_statistic_sql_group(update, [AttributeName|Outputs], TableAlias) -->
        [TableAlias, '.', attribute_name(AttributeName), ', '],
        state_change_select_statistic_sql_group(update, Outputs, TableAlias).

state_change_select_statistic_sql_group(delete, [AttributeName], _) --> !,
        [attribute_name(AttributeName)].

state_change_select_statistic_sql_group(delete, [AttributeName|Outputs], TableAlias) -->
        [attribute_name(AttributeName), ', '],
        state_change_select_statistic_sql_group(delete, Outputs, TableAlias).



get_primary_key_attribute_name(Schema, TableName, PrimaryKeyAttributeName) :-
        ( primary_key_column_name(Schema, TableName, PrimaryKeyAttributeName) ->
            true
        ;
            throw(format('No primary key attribute defined for ~w', [Schema:TableName]))
        ).


label_odbc_outputs([], [], []).

label_odbc_outputs([AttributeName|AttributeNames], [OdbcOutput|OdbcOutputs], [AttributeName-OdbcOutput|LabelledOdbcOutputs]) :-
        label_odbc_outputs(AttributeNames, OdbcOutputs, LabelledOdbcOutputs).



sql_to_select_values_post_state_change(TableName, StateChangeAttributeNames, PrimaryKeyAttributeName, PostUpdateSelectSql) :-
        sql_to_select_values_post_state_change_1(TableName, StateChangeAttributeNames, PrimaryKeyAttributeName, PostUpdateSelectSqlTokens, []),
        compile_tokens(PostUpdateSelectSqlTokens, PostUpdateSelectSql).


sql_to_select_values_post_state_change_1(TableName, StateChangeAttributeNames, PrimaryKeyAttributeName) -->
        ['SELECT '],
        state_change_select_attributes(StateChangeAttributeNames),
        [' FROM ', table_name(TableName)],
        [' WHERE '],
        [attribute_name(PrimaryKeyAttributeName), '= ?'].


state_change_select_attributes([AttributeName]) --> !,
        state_change_select_attribute(AttributeName).

state_change_select_attributes([AttributeName|AttributeNames]) -->
        state_change_select_attribute(AttributeName),
        [', '],
        state_change_select_attributes(AttributeNames).


state_change_select_attribute(AttributeName) -->
        [attribute_name(AttributeName)].


post_state_change_select_statement @
        identify_post_state_change_values(QueryId, Connection),
        cql_post_state_change_select_sql(QueryId, StateChangeAttributeNames, PrimaryKeyOdbcDataType, HalfCompiledPostUpdateSelectSql)
        <=>
        fully_compile_sql(HalfCompiledPostUpdateSelectSql, [], [], PostUpdateSelectSql, _, _),

        odbc_prepare(Connection, PostUpdateSelectSql, [PrimaryKeyOdbcDataType], PreparedStatement),
        post_state_change_select_statement(QueryId, StateChangeAttributeNames, PrimaryKeyOdbcDataType, PreparedStatement).


cleanup_cql_post_state_change_select_sql @
        cleanup_cql_post_state_change_select_sql(QueryId),
        cql_post_state_change_select_sql(QueryId, _, _, _)
        <=>
        true.


cleanup_identify_post_state_change_values @
        identify_post_state_change_values(_, _)
        <=>
        true.


cleanup_cleanup_cql_post_state_change_select_sql @
        cleanup_cql_post_state_change_select_sql(_)
        <=>
        true.


post_state_change_values @
        post_state_change_select_statement(QueryId, StateChangeAttributeNames, PrimaryKeyOdbcDataType, PreparedStatement),
        updated_row_primary_key(QueryId, StateChangeType, Schema, TableName, PrimaryKeyAttributeName, PrimaryKeyValue)
        ==>
        odbc_execute_with_statistics(PreparedStatement, [PrimaryKeyValue], [PrimaryKeyOdbcDataType], Row),
        updated_rows(QueryId,
                     StateChangeType,
                     post_state_change,
                     Schema,
                     TableName,
                     PrimaryKeyAttributeName,
                     StateChangeAttributeNames,
                     [Row]).


cleanup_post_state_change_select_statement @
        post_state_change_select_statement(_, _, _, PreparedStatement)
        <=>
        odbc_free_statement(PreparedStatement).


is_state_change_attribute(StateChangeType,    % +
                          Schema,             % +
                          TableName,          % +
                          AttributeName) :-   % +

        once(( cql_event_notification_table(Schema, TableName)
             ; StateChangeType == update,
               cql_history_attribute(Schema, TableName, AttributeName)
             )).


call_row_change_hooks @
        call_row_change_hooks(QueryId, Connection)
        <=>
        call_history_hook(QueryId, Connection),
        event(QueryId).


debug_state_change_value @
        state_change_value(QueryId, StateChangeType, When, Schema, TableName, PrimaryKeyAttributeName, PrimaryKeyValue, AttributeName, OdbcOutput)
        ==>
        debug(cql(history),
              '~w',
              [state_change_value(QueryId, StateChangeType, When, Schema, TableName, PrimaryKeyAttributeName, PrimaryKeyValue, AttributeName, OdbcOutput)]).

call_history_hook @
        call_history_hook(QueryId, Connection)
        \
        state_change_value(QueryId,
                           update,
                           pre_state_change,
                           Schema,
                           TableName,
                           PrimaryKeyAttributeName,
                           PrimaryKeyValue,
                           AttributeName,
                           OdbcValueBefore),
        state_change_value(QueryId,
                           update,
                           post_state_change,
                           Schema,
                           TableName,
                           PrimaryKeyAttributeName,
                           PrimaryKeyValue,
                           AttributeName,
                           OdbcValueAfter)
        <=>
        debug(cql(history), '(1): ~w, ~w, ~w, ~w, ~w', [Schema, TableName, PrimaryKeyAttributeName, PrimaryKeyValue, AttributeName]),

        \+ memberchk(AttributeName, [PrimaryKeyAttributeName, inserted_, updated_, transaction_id_, generation_]),

        debug(cql(history), '(2): ~w, ~w, ~w, ~w, ~w', [Schema, TableName, PrimaryKeyAttributeName, PrimaryKeyValue, AttributeName]),

        get_transaction_context(TransactionId, _, AccessToken, TransactionTimestamp, _),
        thread_self(ThreadId),
        database_transaction_query_info(ThreadId, Goal, Info),

        debug(cql(history), '(3): ~w, ~w, ~w, ~w, ~w', [Schema, TableName, PrimaryKeyAttributeName, PrimaryKeyValue, AttributeName]),

        odbc_value_to_application_value(Schema, TableName, AttributeName, OdbcValueBefore, ApplicationValueBefore),
        odbc_value_to_application_value(Schema, TableName, AttributeName, OdbcValueAfter, ApplicationValueAfter),

        debug(cql(history), '(4): ~w, ~w, ~w, ~w, ~w', [Schema, TableName, PrimaryKeyAttributeName, PrimaryKeyValue, AttributeName]),

        ignore(catch(history_hook(Schema,
                                  TableName,
                                  AttributeName,
                                  PrimaryKeyAttributeName,
                                  PrimaryKeyValue,
                                  ApplicationValueBefore,
                                  ApplicationValueAfter,
                                  AccessToken,
                                  Info,
                                  TransactionId,
                                  TransactionTimestamp,
                                  ThreadId,
                                  Connection,
                                  Goal),
                     E,
                     cql_log([], error, 'Error in history hook: ~p', [E]))),

        fail
        ;
        true.


history_hook(Schema,                      % +
             TableName,                   % +
             AttributeName,               % +
             PrimaryKeyAttributeName,     % +
             PrimaryKeyValue,             % +
             ApplicationValueBefore,      % +
             ApplicationValueAfter,       % +
             AccessToken,                 % +
             Info,                        % +
             TransactionId,               % +
             TransactionTimestamp,        % +
             ThreadId,                    % +
             Connection,                  % +
             Goal) :-                     % +

        % Need this because CHR rule call_history_hook is also called for attributes
        % identified in pql_event_notification_table/2.  We don't necessarily want these
        % turning up in history
        cql_history_attribute(Schema,
                              TableName,
                              AttributeName),

        % Call the actual hook
        update_history(Schema,
                       TableName,
                       AttributeName,
                       PrimaryKeyAttributeName,
                       PrimaryKeyValue,
                       ApplicationValueBefore,
                       ApplicationValueAfter,
                       AccessToken,
                       Info,
                       TransactionId,
                       TransactionTimestamp,
                       ThreadId,
                       Connection,
                       Goal).


cleanup_call_history_hook @
        call_history_hook(_, _)
        <=>
        true.


call_event_hook @
        event(QueryId)
        \
        updated_row_primary_key(QueryId, StateChangeType, Schema, TableName, PrimaryKeyAttributeName, PrimaryKeyValue)
        <=>
        get_transaction_context(_, _, AccessToken, _, _),

        save_database_event(AccessToken,
                            StateChangeType,      % insert ; update ; delete
                            Schema,
                            TableName,
                            PrimaryKeyAttributeName,
                            PrimaryKeyValue),
        fail
        ;
        true.


cleanup_event @
        event(_)
        <=>
        true.


%tokens_to_atom(Tokens, Sql) :-
%        tokens_to_atoms(Tokens, Atoms),
%        atomic_list_concat(Atoms, Sql).

compile_tokens(Tokens, CompiledTokens):-
        tokens_to_atoms(Tokens, CompiledTokens).


tokens_to_atoms([], []).

tokens_to_atoms([Condition:List|Tokens], [Condition:CompiledList|Atoms]) :- !,
        tokens_to_atoms(List, CompiledList),
        tokens_to_atoms(Tokens, Atoms).

tokens_to_atoms([attribute_name(AttributeName)|Tokens], [AttributeNameUc|Atoms]) :- !,
        map_database_atom(AttributeName, AttributeNameUc),
        tokens_to_atoms(Tokens, Atoms).

tokens_to_atoms([table_name(TableName)|Tokens], [TableNameUc|Atoms]) :- !,
        map_database_atom(TableName, TableNameUc),
        tokens_to_atoms(Tokens, Atoms).

tokens_to_atoms([Atom|Tokens], [Atom|Atoms]) :-
        tokens_to_atoms(Tokens, Atoms).


%       The formatting of the output needs to be kept in sync with std:show_result/9
debug_before @
        show_debug(Mode)
        \
        debug_before(Sql, Schema, Parameters)
        <=>
        dbms(Schema, DBMS),
        ( atom_codes(Sql, SqlCodes),
          sql_tokens(Tokens, SqlCodes, []),
          sql_parse(action(Expression, _Types), _, [dbms(DBMS)], Tokens)->
            ( ( Mode == full ;  Mode == minimal ;  DBMS == 'PostgreSQL')-> % Postgresql does not support the DECLARE .... stuff, so just print it out using full mode
                findall(Binding,
                        ( member(odbc_parameter(_, _, _, Binding, Disposition, _), Parameters),
                          Disposition \== evaluated_update_attribute
                        ),
                        Bindings),
                ( with_output_to(atom(HSql), sql_write(current_output, Expression, [dbms(DBMS), parameter_bindings(Bindings), suppress_collations, suppress_trivial_conditions]))->
                    true
                ; otherwise->
                    HSql = '<could not parse>'
                )
            ; Mode == explicit ->
                Counter = counter(0),
                aggregate_all(r(bag(Declaration),
                                bag(Assignment),
                                bag(ParameterName)),
                              ( member(odbc_parameter(Schema, TableName, AttributeName, Binding, Disposition, _), Parameters),
                                arg(1, Counter, I),
                                II is I + 1,
                                nb_setarg(1, Counter, II),
                                Disposition \== evaluated_update_attribute,
                                once(declaration_sql(Schema, TableName, AttributeName, Binding, I, Declaration, Assignment, ParameterName))
                        ),
                        r(Declarations, Assignments, Bindings)),
                atomic_list_concat(Declarations, ', ', H1),
                atomic_list_concat(Assignments, '\n', H2),
                ( with_output_to(atom(H3), sql_write(current_output, Expression, [dbms(DBMS), parameter_bindings(Bindings), suppress_collations, suppress_trivial_conditions]))->
                    atomic_list_concat([H1, H2, H3], '\n', HSql)
                ; otherwise->
                    HSql = '<could not parse>'
                )
            )->
            true
        ; otherwise->
            HSql = '<could not parse>'
        ),

        HBindings = [],

        thread_self(ThreadId),
        cql_port_label(call, PortName, Colour),

        ( Mode == full ->
            format(atom(HumanSql), HSql, HBindings),
            ansi_format([], '[~w]  ~|', [ThreadId]),
            ansi_format([fg(Colour), bold], '~w', [PortName]),
            ansi_format([], '~n~w~n', [HumanSql])
        ; Mode == minimal ->
            format(atom(VerboseHumanSql), HSql, HBindings),
            remove_newlines_and_truncate(VerboseHumanSql, HumanSql),
            ansi_format([], '[~w]  ~|', [ThreadId]),
            ansi_format([fg(Colour), bold], '~w', [PortName]),
            ansi_format([], '~w~n', [HumanSql])
        ; Mode == explicit ->
            format(atom(HumanSql), HSql, HBindings),
            ansi_format([], '[~w]  ~|', [ThreadId]),
            ansi_format([fg(Colour), bold], '~w', [PortName]),
            ansi_format([], '~n~w~n', [HumanSql])
        ),
        original_human_query(HumanSql).

declaration_sql(Schema, TableName, AttributeName, Binding, I, Declaration, Assignment, Parameter):-
        format(atom(ParameterName), '@P~w', [I]),
        attribute_domain(Schema, TableName, AttributeName, Domain),
        domain_database_data_type(Domain, Type),
        format(atom(Declaration), 'DECLARE ~w ~w', [ParameterName, Type]),
        format(atom(Assignment), 'SET ~w = ~C', [ParameterName, Binding]),
        Parameter = parameter(ParameterName).


debug_after @
        show_debug(Mode),
        debug_statistics(C1, T1, I1),
        original_human_query(HumanSql)
        \
        debug_after(Reason, Outputs)
        <=>
        statistics(inferences, I2),
        cql_perf_time(T2),
        statistics(cputime, C2),
        ElapsedTime is T2 - T1,
        CpuTime is C2 - C1,
        Inferences is I2 - I1 - 5,   % ? How many to subtract for CHR?!
        functor(Reason, Port, _),
        thread_self(ThreadId),
        cql_port_label(Port, PortName, Colour),
        ( memberchk(Port, [!, exit]) ->
            ( Outputs == [ignore_output] ->
                ansi_format([], '[~w]  ~|', [ThreadId]),
                ansi_format([fg(Colour), bold], '~w', [PortName]),
                ansi_format([], '~w', [HumanSql]),
                ansi_format([], ' (~6fs, ~2fcpu, ~D inferences)~n', [ElapsedTime, CpuTime, Inferences])

            ; otherwise ->
                ( Mode == minimal ->
                    ansi_format([], '[~w]  ~|', [ThreadId]),
                    ansi_format([fg(Colour), bold], '~w', [PortName]),
                    ansi_format([], '~w', [HumanSql]),
                    ansi_format([], ' (~6fs, ~2fcpu, ~D inferences)~n', [ElapsedTime, CpuTime, Inferences])
                ; Outputs = affected(N)->
                    ansi_format([], '[~w]  ~|', [ThreadId]),
                    ansi_format([fg(Colour), bold], 'Number of rows affected: ', []),
                    ansi_format([], '~w~n', [N]),
                    ansi_format([], ' (~6fs, ~2fcpu, ~D inferences)~n', [ElapsedTime, CpuTime, Inferences])
                ; Outputs = identity(N)->
                    ansi_format([], '[~w]  ~|', [ThreadId]),
                    ansi_format([fg(Colour), bold], 'Identity of inserted row: ', []),
                    ansi_format([], '~w~n', [N]),
                    ansi_format([], ' (~6fs, ~2fcpu, ~D inferences)~n', [ElapsedTime, CpuTime, Inferences])
                ; otherwise ->
                    selection_results(Outputs, Results),
                    atomic_list_concat(Results, '\n        ', Debug),
                    ansi_format([], '[~w]  ~|', [ThreadId]),
                    ansi_format([fg(Colour), bold], 'Result: ', []),
                    ansi_format([], '~w~n', [Debug]),
                    ansi_format([], ' (~6fs, ~2fcpu, ~D inferences)~n', [ElapsedTime, CpuTime, Inferences])
                )
            )

        ; Port == fail ->
            ( Mode == minimal ->
                ansi_format([], '[~w]  ~|', [ThreadId]),
                ansi_format([fg(Colour), bold], '~w', [PortName]),
                ansi_format([], '~w', [HumanSql])
            ; otherwise ->
                ansi_format([], '[~w]  ~|', [ThreadId]),
                ansi_format([fg(Colour), bold], '~w', [PortName])
            ),
            ansi_format([], ' (~6fs, ~2fcpu, ~D inferences)~n', [ElapsedTime, CpuTime, Inferences])

        ).

no_debug_before @
        debug_before(_, _, _)
        <=>
        true.

no_debug_after @
        debug_after(_, _)
        <=>
        true.


cleanup_compile \ absence_of_where_restriction_is_deliberate <=> true.
cleanup_compile \ attribute_binding(_, _, _) <=> true.
cleanup_compile \ attribute_for_group_by(_, _, _, _) <=> true.
cleanup_compile \ attribute_for_order_by(_, _, _, _) <=> true.
cleanup_compile \ attributes_to_check(_, _, _, _) <=> true.
cleanup_compile \ check_for_orphan_distincts <=> true.
cleanup_compile \ check_for_orphan_group_bys <=> true.
cleanup_compile \ check_for_orphan_order_bys <=> true.
cleanup_compile \ check_for_orphan_select_attributes_in_aggregations <=> true.
cleanup_compile \ check_for_orphan_select_variables_in_updates <=> true.
cleanup_compile \ check_for_unjoined_tables <=> true.
cleanup_compile \ comparison(_, _, _, _) <=> true.
cleanup_compile \ compile_mode(_) <=> true.
cleanup_compile \ copy_of_from(_, _, _, _) <=> true.
cleanup_compile \ create_join_points <=> true.
cleanup_compile \ create_cql_pre_state_change_select_sql(_, _, _, _, _) <=> true.
cleanup_compile \ determine_select_distinctions <=> true.
cleanup_compile \ generate_sub_query_sql <=> true.
cleanup_compile \ ignore_if_null(_, _) <=> true.
cleanup_compile \ implicit_join(_, _, _) <=> true.
cleanup_compile \ join_alias(_, _, _) <=> true.
cleanup_compile \ join_leaf(_, _) <=> true.
cleanup_compile \ join_pointer(_, _) <=> true.
cleanup_compile \ join_tree_nodes(_, _) <=> true.
cleanup_compile \ join_variable(_) <=> true.
cleanup_compile \ limit(_, _, _) <=> true.
cleanup_compile \ next_group_by_attribute_needs_comma(_) <=> true.
cleanup_compile \ next_order_by_attribute_needs_comma(_) <=> true.
cleanup_compile \ next_in_list_value_needs_comma(_) <=> true.
cleanup_compile \ no_state_change_actions(_) <=> true.
cleanup_compile \ nolock(_, _) <=> true.
cleanup_compile \ on(_, _, _) <=> true.
cleanup_compile \ order_bys(_, _) <=> true.
cleanup_compile \ outer_side_join(_) <=> true.
cleanup_compile \ phase(_, _) <=> true.
cleanup_compile \ query(_, _, _) <=> true.
cleanup_compile \ query_table_alias(_, _, _, _) <=> true.
cleanup_compile \ query_type(_, _) <=> true.
cleanup_compile \ restriction_tree(_, _, _) <=> true.
cleanup_compile \ equality_restriction_variable(_, equality_restriction_variable_used) <=> true.
cleanup_compile \ search_for_join_aliases(_, _, _) <=> true.
cleanup_compile \ select_attribute(_, _, _, _, _) <=> true.
cleanup_compile \ select_attribute_written(_) <=> true.
cleanup_compile \ select_attributes_for_disjunction_comparison(_, _) <=> true.
cleanup_compile \ select_binding(_, _, _, _) <=> true.
cleanup_compile \ select_distinction(_, _) <=> true.
cleanup_compile \ selection_type(_, _) <=> true.
cleanup_compile \ simplify <=> true.
cleanup_compile \ state_change_query(_, _, _, _) <=> true.
cleanup_compile \ sql_statement(_, _, _, _, _, _, _, _, _, _) <=> true.
cleanup_compile \ sub_query_join_variable(_) <=> true.
cleanup_compile \ sub_query_select(_) <=> true.
cleanup_compile \ top(_, _, _) <=> true.
cleanup_compile \ union_outputs(_, _, _) <=> true.
cleanup_compile \ update(_, _, _, _, _) <=> true.
cleanup_compile \ update_table_alias(_, _, _, _) <=> true.
cleanup_compile \ where_restriction_variable(_) <=> true.
cleanup_compile \ write_group_bys(_) <=> true.
cleanup_compile \ write_in_list(_, _, _, _, _, _) <=> true.
cleanup_compile \ write_join_ons(_, _) <=> true.
cleanup_compile \ write_query_sql <=> true.
cleanup_compile \ write_restriction_tree(_, _, _) <=> true.
cleanup_compile \ write_select_attributes(_) <=> true.
cleanup_compile \ write_sql(_, _, _, _, _, _, _) <=> true.
cleanup_compile <=> true.


no_duplicate_where_restriction_variable @
        where_restriction_variable(Variable) \ where_restriction_variable(Variable) <=> true.


share_variables_with_removed_query @
        remove_query(RemovedQueryId, ReplacementQueryId),
        conjunction_variable(RemovedQueryId, ExternalVariable, OldWhereRestrictionVariable),
        conjunction_variable(ReplacementQueryId, ExternalVariable, NewWhereRestrictionVariable)
        ==>
        OldWhereRestrictionVariable = NewWhereRestrictionVariable.

remove_query(QueryId, _) \ aggregation_sub_query(QueryId, _, _, _, _, _) <=> true.
remove_query(QueryId, _) \ aggregation_variable(QueryId, _, _) <=> true.
remove_query(QueryId, _) \ attribute_binding(QueryId, _, _) <=> true.
remove_query(QueryId, _) \ attribute_for_group_by(QueryId, _, _, _) <=> true.
remove_query(QueryId, _) \ attribute_for_order_by(QueryId, _, _, _) <=> true.
remove_query(QueryId, _) \ attributes_to_check(QueryId, _, _, _) <=> true.
remove_query(QueryId, _) \ comparison(QueryId, _, _, _) <=> true.
remove_query(QueryId, _) \ create_cql_pre_state_change_select_sql(QueryId, _, _, _, _) <=> true.
remove_query(QueryId, _) \ distinct(QueryId, _) <=> true.
remove_query(QueryId, _) \ group_by(QueryId, _) <=> true.
remove_query(QueryId, _) \ join(QueryId, _, _, _, _) <=> true.
remove_query(QueryId, _) \ join_alias(QueryId, _, _) <=> true.
remove_query(QueryId, _) \ join_leaf(QueryId, _) <=> true.
remove_query(QueryId, _) \ join_pointer(QueryId, _) <=> true.
remove_query(QueryId, _) \ join_tree_node(QueryId, _, _) <=> true.
remove_query(QueryId, _) \ join_tree_nodes(QueryId, _) <=> true.
remove_query(QueryId, _) \ next_group_by_attribute_needs_comma(QueryId) <=> true.
remove_query(QueryId, _) \ phase(QueryId, _) <=> true.
remove_query(QueryId, _) \ query(QueryId, _, _) <=> true.
remove_query(QueryId, _) \ query_type(QueryId, _) <=> true.
remove_query(QueryId, _) \ restriction_tree(QueryId, _, _) <=> true.
remove_query(QueryId, _) \ search_for_join_aliases(QueryId, _, _) <=> true.
remove_query(QueryId, _) \ select_attribute(QueryId, _, _, _, _) <=> true.
remove_query(QueryId, _) \ select_attribute_written(QueryId) <=> true.
remove_query(QueryId, _) \ select_attributes_for_disjunction_comparison(QueryId, _) <=> true.
remove_query(QueryId, _) \ select_binding(QueryId, _, _, _) <=> true.
remove_query(QueryId, _) \ select_distinction(QueryId, _) <=> true.
remove_query(QueryId, _) \ selection_type(QueryId, _) <=> true.
remove_query(QueryId, _) \ sql_not(QueryId, _) <=> true.
remove_query(QueryId, _) \ sql_statement(QueryId, _, _, _, _, _, _, _, _, _) <=> true.
remove_query(QueryId, _) \ sub_query(QueryId, _, _, _) <=> true.
remove_query(QueryId, _) \ sub_query_select(QueryId) <=> true.
remove_query(QueryId, _) \ union_outputs(QueryId, _, _) <=> true.
remove_query(QueryId, _) \ update(QueryId, _, _, _, _) <=> true.
remove_query(QueryId, _) \ update_table_alias(QueryId, _, _, _) <=> true.
remove_query(QueryId, _) \ write_group_bys(QueryId) <=> true.
remove_query(QueryId, _) \ write_join(QueryId, _) <=> true.
remove_query(QueryId, _) \ write_join_ons(QueryId, _) <=> true.
remove_query(QueryId, _) \ write_restriction_tree(QueryId, _, _) <=> true.
remove_query(QueryId, _) \ write_select_attributes(QueryId) <=> true.
remove_query(QueryId, _) \ write_sql(QueryId, _, _, _, _, _, _) <=> true.
remove_query(_, _) <=> true.

nonvar_is_never_singleton @
        not_a_singleton(Value)
        <=>
        nonvar(Value)
        |
        true.

collect_constraints_that_must_be_recreated_for_use_at_runtime @
        collect_runtime_constraints(RuntimeConstraints),
        runtime_constraints(Constraints)
        <=>
        RuntimeConstraints = Constraints.

cn01 @
        conjunction_variable(A, B, C),
        runtime_constraints(Constraints)
        <=>
        runtime_constraints((conjunction_variable(A, B, C), Constraints)).

cn02 @
        odbc_select_disjunction(A),
        runtime_constraints(Constraints)
        <=>
        runtime_constraints((A, Constraints)).   % Make the disjunction visible to the Prolog compiler (compile-time CQL)

cn03 @
        cql_odbc_state_change_statement(A, B, C, D, E, F, G),
        runtime_constraints(Constraints)
        <=>
        runtime_constraints((cql_odbc_state_change_statement(A, B, C, D, E, F, G), Constraints)).

cn04 @
        cql_identity(A, B, C),
        runtime_constraints(Constraints)
        <=>
        runtime_constraints((cql_identity(A, B, C), Constraints)).

cn05 @
        cql_pre_state_change_select_sql(A, B, C, D, E, F, G),
        runtime_constraints(Constraints)
        <=>
        runtime_constraints((cql_pre_state_change_select_sql(A, B, C, D, E, F, G), Constraints)).

cn06 @
        cql_post_state_change_select_sql(A, B, C, D),
        runtime_constraints(Constraints)
        <=>
        runtime_constraints((cql_post_state_change_select_sql(A, B, C, D), Constraints)).

cn07 @
        row_count(A, B),
        runtime_constraints(Constraints)
        <=>
        runtime_constraints((row_count(A, B), Constraints)).

cn08 @
        cql_statement_location(A, B),
        runtime_constraints(Constraints)
        <=>
        runtime_constraints((cql_statement_location(A, B), Constraints)).

cn09 @
        in_line_format(A, B, C, D),
        runtime_constraints(Constraints)
        <=>
        runtime_constraints((in_line_format(A, B, C, D), Constraints)).


cn12 @
        cql_state_change_statistics_sql(A, B, C, D, E, F, G, H),
        runtime_constraints(Constraints)
        <=>
        runtime_constraints((cql_state_change_statistics_sql(A, B, C, D, E, F, G, H), Constraints)).

cn13 @
        runtime_instantiation_check(_, Variable),
        runtime_constraints(Constraints)
        <=>
        runtime_constraints((cql_var_check(Variable), Constraints)).

cn14 @
        not_a_singleton(Variable),
        runtime_constraints(Constraints)
        <=>
        runtime_constraints((Variable=Variable, Constraints)).


clean_up_statistics @
        no_debug
        \
        debug_statistics(_,_,_)
        <=>
        true.

clean_up_hsql @
        no_debug
        \
        original_human_query(_)
        <=>
        true.


finish_debug @
        show_debug(_),
        no_debug
        <=>
        true.



%%      cql_show(:Goal, +Mode)
%
%       Called when ?/1, ??/1, and ???/1 applied to CQL
%
%       @param Goal goal term
%       @param Mode minimal ; explicit ; full

:-meta_predicate
        cql_show(0, +).

cql_show(Goal, Mode):-
        statistics(cputime, C1),
        cql_perf_time(T1),
        statistics(inferences, I1),
        debug_statistics(C1, T1, I1),
        show_debug(Mode),
        Goal,
        no_debug.

selection_results([], []):- !.
selection_results([output(_Schema, TableName, ColumnName, Value)|Selections], [Result|Results]):-
        format(atom(Result), '~w.~w = ~q', [TableName, ColumnName, Value]),
        selection_results(Selections, Results).

generic_type(decimal(_,_), decimal):-!.
generic_type(varchar(_), varchar):-!.
generic_type(X, X):-!.

remove_newlines_and_truncate(VerboseHumanSql, HumanSql):-
        atom_codes(VerboseHumanSql, Codes),
        replace_newlines_with_spaces(Codes, Codes1),
        length(Codes2, 77),
        (append(Codes2, _, Codes1)->
            format(atom(HumanSql), '~s ...', [Codes2])
        ; otherwise->
            format(atom(HumanSql), '~s', [Codes1])
        ).


replace_newlines_with_spaces([], []).
replace_newlines_with_spaces([32, 32|X], Y):-
        !,
        replace_newlines_with_spaces([32|X], Y).
replace_newlines_with_spaces([13, 10|X], Y):-
        !,
        replace_newlines_with_spaces([32|X], Y).
replace_newlines_with_spaces([13|X], Y):-
        !,
        replace_newlines_with_spaces([32|X], Y).
replace_newlines_with_spaces([10|X], Y):-
        !,
        replace_newlines_with_spaces([32|X], Y).

replace_newlines_with_spaces([X|Xs], [X|Y]):-
        !,
        replace_newlines_with_spaces(Xs, Y).


record_referenced_tables @
        query_table_alias(_, _, TableName, _) ==> referenced_table(TableName).

no_duplicate_referenced_table @
        referenced_table(TableName) \ referenced_table(TableName) <=> true.

get_referenced_tables @
        referenced_tables(ReferencedTables), referenced_table(TableName) <=> ReferencedTables = [TableName|T], referenced_tables(T).
        referenced_tables(Parameters) <=> Parameters = [].

% FreeTDS does not like decimal for types; it really wants decimal(x,y). If you specify decimal, you get decimal(0) which is
% impossible. According to http://msdn.microsoft.com/en-us/library/ms187746.aspx:
%   * The default precision is 18
%   * The default scale is 0
max_decimal(decimal(18, 0)).


%!      on_to_where(+Schema, +On, -Where)
%
%       Convert ON terms to WHERE terms so we can use cql_suggest_indices/2 for joins

on_to_where(Schema, (A, B), and(C, D)) :-
        !,
        on_to_where(Schema, A, C),
        on_to_where(Schema, B, D).
on_to_where(Schema, (A ; B), or(C, D)) :-
        !,
        on_to_where(Schema, A, C),
        on_to_where(Schema, B, D).
on_to_where(Schema, On, comparison(C, Op, D)) :-
        On =.. [Op, A, B],
        prolog_to_sql_comparison_operator(Schema, Op, _, _),
        on_to_where_1(Schema, A, C),
        on_to_where_1(Schema, B, D),
        !.
on_to_where(_, On, _) :-
        throw(format('Unexpected ON : ~q', [On])).


on_to_where_1(_Schema, AttributeVar, Attribute) :-
        var(AttributeVar),
        !,
        ( attribute(AttributeVar, Attribute) ->
            true
        ; otherwise ->
            Attribute = AttributeVar
        ).
on_to_where_1(Schema, TableAlias-AttributeName, attribute(Schema, TableAlias, AttributeName)) :-
        atom(AttributeName),
        !.
on_to_where_1(_, _-AttributeVar, Attribute) :-
        attribute(AttributeVar, Attribute),
        !.
on_to_where_1(_, Expr, Expr).


:-chr_constraint
        attribute(-'ApplicationValue', ?'Attribute').

attribute @
        attribute_binding(_, Attribute_1, AttributeVar) \ attribute(AttributeVar, Attribute) <=> Attribute = Attribute_1.
        attribute(_, _) <=> fail.


%!      cql_suggest_indices(+RestrictionTree, +QueryId) is det.
%
%       Warn of any restrictions without corresponding indexes on the query identified by QueryId.
:-multifile(cql_index_suggestion_hook/1).
cql_suggest_indices(RestrictionTree, QueryId) :-
        phrase(conjunction(RestrictionTree, QueryId), Conjunction),   % Each solution is a different conjunction
        ignore(cql_index_suggestion_hook(Conjunction)).

conjunction(or(LHS, RHS), QueryId) -->
        !,
        ( conjunction(LHS, QueryId)
        ; conjunction(RHS, QueryId)
        ).
conjunction(and(LHS, RHS), QueryId) -->
        !,
        conjunction(LHS, QueryId),
        conjunction(RHS, QueryId).
conjunction(comparison(LHS, Op, RHS), QueryId) -->
        {( Op == ==
         ; Op == =:=
         )},
        !,
        where_attribute(LHS, QueryId),
        where_attribute(RHS, QueryId).
conjunction(_, _) --> [].

where_attribute(Attribute, QueryId) -->
        {nonvar(Attribute),
         Attribute = attribute(_, TableAlias, AttributeName),
         table_name(QueryId, TableAlias, TableName)},
        !,
        [TableName-AttributeName].
where_attribute(_, _) --> [].

:-chr_constraint
        table_name(-'QueryId', -'TableAlias', ?'TableName').

table_name @
        query_table_alias(QueryId, _, TableName_1, TableAlias) \ table_name(QueryId, TableAlias, TableName) <=> TableName = TableName_1.
        table_name(_, _, _) <=> fail.



fully_compile_sql(HalfCompiledSql, HalfCompiledOdbcParameters, HalfCompiledOutputs, Sql, OdbcParameters, Outputs):-
        fully_compile_list(HalfCompiledSql, SqlTokens),
        atomic_list_concat(SqlTokens, Sql),
        fully_compile_list(HalfCompiledOdbcParameters, OdbcParameters),
        fully_compile_list(HalfCompiledOutputs, Outputs),
        %writeln(sql=Sql),
        %writeln(params=OdbcParameters),
        true.

:-thread_local
        skip_cql_instantiation_check/0.

% We are decompiling for a test. To be consistent, instantiate:
%   All if_null or if_not_null variables to {null}
%   All if_empty or not_empty variables to []
instantiate_test_values(if_null({null})):-!.
instantiate_test_values(if_not_null({null})):-!.
instantiate_test_values(if_empty([])):-!.
instantiate_test_values(not_empty([])):-!.
instantiate_test_values(list(_)):- !.
instantiate_test_values(if_var({null})):- !.
instantiate_test_values(if_not_var({null})):- !.
instantiate_test_values(and(A, B)):- !,
        instantiate_test_values(A),
        instantiate_test_values(B).
% Also, the test values may already be (partly) instantiated. In this case, just accept anything.
instantiate_test_values(_T):- !.
%        ground(T).


fully_compile_list([], []):- !.
fully_compile_list([Condition:List|More], Sql):-
        % Note that this will instantiate condition if unbound. This likely indicates a mistake: If the list is not bound at runtime,
        % the developer has forgotten something. list(X) is instantiated with X = [], ie the restriction is ignored if unbound.
        % Note that the CQL decompiler tester can get into this situation, because it does not /execute/ the CQL, just tries to parse the generated
        % output.
        !,
        ( skip_cql_instantiation_check->
            instantiate_test_values(Condition)
        ; Condition = if_null(Var), var(Var)->
            throw(instantiation_error(Var))
        ; Condition = if_not_null(Var), var(Var)->
            throw(instantiation_error(Var))
        ; Condition = empty(Var), var(Var)->
            throw(instantiation_error(Var))
        ; Condition = not_empty(Var), var(Var)->
            throw(instantiation_error(Var))
        ; otherwise->
            true
        ),
        ( Condition = list(Items)->
            fully_compile_list_expansion(List, Items, X),
            append(X, Tail, Sql),
            fully_compile_list(More, Tail)
        ; check_compile_condition(Condition)->
            append(List, Tail, Sql),
            fully_compile_list(More, Tail)
        ; otherwise->
            fully_compile_list(More, Sql)
        ).
fully_compile_list([Atom|More], [Atom|Sql]):-
        fully_compile_list(More, Sql).


instruction_conjunction(compile, X, X):- !.
instruction_conjunction(X, compile, X):- !.
instruction_conjunction(Existing, New, Existing):-
        instruction_is_subsumed(Existing, New), !.
instruction_conjunction(A, B, and(A,B)):- !.

instruction_is_subsumed(A, B):- A == B, !.
instruction_is_subsumed(and(A, C), B):-
        ( instruction_is_subsumed(A, B)->
            true
        ; instruction_is_subsumed(C, B)->
            true
        ).

check_compile_condition(and(A, B)):-
        !,
        check_compile_condition(A),
        check_compile_condition(B).

check_compile_condition(Condition):-
        ( Condition = if_null({null})->
            true
        ; Condition = if_null(_)->
            fail
        ; Condition = if_var(V)->
            var(V)
        ; Condition = if_not_var(V)->
          \+var(V)
        ; Condition = if_not_null({null})->
            fail
        ; Condition = if_not_null(_)->
            true
        ; Condition = not_empty([]) ->
            fail
        ; Condition = not_empty(_) ->
            true
        ; Condition = empty([]) ->
            true
        ; Condition = empty(_) ->
            fail
        ; Condition == compile ->
            true
        ; otherwise->
            cql_error(bad_cql_compile_instruction, '~w', [Condition])
        ).


% Handle case where list is empty
fully_compile_list_expansion([_], [], []):- !.

fully_compile_list_expansion([?], [_], [?]):- !.
fully_compile_list_expansion([?], [_|Items], ['?, '|More]):- !,
        fully_compile_list_expansion([?], Items, More).

fully_compile_list_expansion([odbc_parameter(Schema, TableName, AttributeName, _, Disposition, Override)],
                             [Value],
                             [odbc_parameter(Schema, TableName, AttributeName, Value, Disposition, Override)]):- !.
fully_compile_list_expansion([odbc_parameter(Schema, TableName, AttributeName, _, Disposition, Override)],
                             [Value|Values],
                             [odbc_parameter(Schema, TableName, AttributeName, Value, Disposition, Override)|More]):- !,
        fully_compile_list_expansion([odbc_parameter(Schema, TableName, AttributeName, _, Disposition, Override)], Values, More).

% This is a much better place for this than portray, since it would otherwise make portray depend on sql_write now
:-format_predicate('C', cql_portray(_, _Term)).
cql_portray(_, Term):-
        (nonvar(Term)->
           ( Term = t7(_,_,_,_,_,_,_)->
               format(current_output, '\'~p\'', [Term])
           ; Term == boolean(true)->
               format(current_output, '1', [])
           ; Term == boolean(false)->
               format(current_output, '0', [])
            ; atom(Term)->
               atom_codes(Term, Codes),
               sql_quote_codes(Quoted, Codes, []),
               format(current_output, '\'~s\'', [Quoted])
           ; otherwise->
               format(current_output, '~q', [Term])
           )
        ; otherwise->
           format(current_output, '~q', [Term])
        ).


cql_var_check(Var):-
        ( var(Var)->
            true
        ; otherwise->
            throw(uninstantiation_error(Var))
        ).



cql_duplicates(List, SortedDuplicates):-
        msort(List, SortedList),
        cql_duplicates(SortedList, [], Duplicates),
        sort(Duplicates, SortedDuplicates).

cql_duplicates([], Duplicates, Duplicates) :- !.
cql_duplicates([_], Duplicates, Duplicates) :- !.
cql_duplicates([A, B|T1], Duplicates, T2) :-
        A == B,
        !,
        ( memberchk(A, Duplicates) ->
            cql_duplicates(T1, Duplicates, T2)
        ; otherwise->
            cql_duplicates(T1, [A|Duplicates], T2)
        ).
cql_duplicates([_|T1], Duplicates, T2) :-
        cql_duplicates(T1, Duplicates, T2).


map_database_atom(Keyword, Mapped):-
        reserved_sql_keyword(Keyword), !,
        format(atom(Mapped), '"~w"', [Keyword]).
map_database_atom(Atom, Atom).

cql_strip_sort_keys([], []).
cql_strip_sort_keys([_-Detail|T1], [Detail|T2]) :-
        cql_strip_sort_keys(T1, T2).

cql_path_arg([], Term, Term).
cql_path_arg([Index|Indices], Term, SubTerm) :-
        compound(Term),
	arg(Index, Term, Arg),
	cql_path_arg(Indices, Arg, SubTerm).



:-multifile(cql_access_token_hook/2).
cql_access_token_to_user_id(X, Y):-
        ( cql_access_token_hook(X, Y)->
            true
        ; otherwise->
            Y = X
        ).

% If log_selects succeeds, selects will be logged via cql_log/4.
:-multifile(log_selects/0).


% default_schema/1 needs to be defined by the application
:-multifile(cql:default_schema/1).
user:term_expansion(:-cql_option(default_schema(Schema)), cql:default_schema(Schema)).

user:term_expansion(:-cql_option(max_db_connections(N)), cql_database:max_db_connections_hook(N)).

%%      statistic_monitored_attribute(+Schema, +TableName, +ColumnName).
:-multifile(cql_statistic_monitored_attribute_hook/3).
statistic_monitored_attribute(Schema, TableName, ColumnName):-
        cql_statistic_monitored_attribute_hook(Schema, TableName, ColumnName).

%%      statistic_monitored_attribute_change(+Schema, +TableName, +ColumnName, +Value, +Delta).
%       Called when a statistic_monitored_attribute changes.
%       Delta is the (signed) change in the number of rows in the table where TableName.ColumnName == Value
:-multifile(cql_statistic_monitored_attribute_change_hook/5).
statistic_monitored_attribute_change(Schema, TableName, ColumnName, Value, Delta):-
        ignore(cql_statistic_monitored_attribute_change_hook(Schema, TableName, ColumnName, Value, Delta)).

%%      dbms(+Schema, -DBMSName).
%       Determine the DBMS for a given Schema.
%       Can be autoconfigured.
:-multifile(dbms/2).

%%      odbc_data_type(+Schema, +TableSpec, +ColumnName, ?OdbcDataType)
%       OdbcDataType must be a native SQL datatype, such as varchar(30) or decimal(10, 5)
%       Can be autoconfigured.
:-multifile(odbc_data_type/4).

%%      primary_key_column_name(+Schema, +TableName, -PrimaryKeyAttributeName).
%       Can be autoconfigured.
:-multifile(primary_key_column_name/3).

%%      database_attribute(?EntityType:table/view, ?Schema:atom, ?EntityName:atom, ?ColumnName:atom, ?DomainOrNativeType:atom, ?AllowsNulls:allows_nulls(true/false), ?IsIdentity:is_identity(true/false), ?ColumnDefault) is nondet.
%       Can be autoconfigured.
:-multifile(database_attribute/8).

%%      database_attribute(?DomainName:atom, ?OdbcDataType) is nondet.
%       Can be autoconfigured.
:-multifile(database_domain/2).

%%      routine_return_type(?Schema:atom, ?EntityName:atom, ?OdbcType).
%       Can be autoconfigured
:-multifile(routine_return_type/3).

%%      database_constraint(?Schema:atom, ?EntityName:atom, ?ConstraintName:atom, ?Constraint) is nondet.
%       Constraint is one of:
%          * primary_key(ColumnNames:list)
%          * foreign_key(ForeignTableName:atom, ForeignColumnNames:list, ColumnNames:list)
%          * unique(ColumnNames:list)
%          * check(CheckClause)
%       In theory this can be autoconfigured too, but I have not written the code for it yet
:-multifile(cql:database_constraint/4).

user:goal_expansion(Schema:{Cql}, GoalExpansion) :-
        atom(Schema),
        !,
        cql_goal_expansion(Schema, Cql, GoalExpansion).

user:goal_expansion({Cql}, GoalExpansion) :- !,
        default_schema(Schema),
        cql_goal_expansion(Schema, Cql, GoalExpansion).

user:goal_expansion(?(Goal), cql_show(Goal, minimal)):-
        nonvar(Goal),
        ( Goal = {_} ; Goal = _:{_} ).
user:goal_expansion(??(Goal), cql_show(Goal, explicit)):-
        nonvar(Goal),
        ( Goal = {_} ; Goal = _:{_} ).
user:goal_expansion(???(Goal), cql_show(Goal, full)):-
        nonvar(Goal),
        ( Goal = {_} ; Goal = _:{_} ).

:-multifile(cql_execution_hook/4).
odbc_execute_with_statistics(Statement, OdbcParameters, OdbcParameterDataTypes, Row):-
        ( cql_execution_hook(Statement, OdbcParameters, OdbcParameterDataTypes, Row) *->
            true
        ; otherwise->
            odbc_execute(Statement, OdbcParameters, Row)
        ).

:-multifile(cql_log_hook/4).
cql_log(Targets, Level, Format, Args):-
        ( cql_log_hook(Targets, Level, Format, Args)->
            true
        ; otherwise->
            debug(cql(logging), Format, Args)
        ).


%%      attribute_domain(+Schema, +TableName, +ColumnName, -Domain).
attribute_domain(Schema, TableName, ColumnName, Domain):-
        %odbc_data_type(Schema, TableName, ColumnName, Domain).
        % CHECKME: Above or below?
        database_attribute(_, Schema, TableName, ColumnName, domain(Domain), _, _, _).

%%      database_identity(?Schema:atom, ?EntityName:atom, ?ColumnName:atom)
database_identity(Schema, EntityName, ColumnName) :-
        database_attribute(_, Schema, EntityName, ColumnName, _, _, is_identity(true), _).

domain_database_data_type(Domain, Type) :-
        checked_domain(Domain, _, _, _, _, _, Type).


%%      database_key(?Schema:atom, ?EntityName:atom, ?ConstraintName:atom, ?KeyColumnNames:list, ?KeyType)
%
%       @param KeyColumnNames list of _|atom|_ in database-supplied order
%       @param KeyType _|identity|_ ; _|'primary key'|_ ; _|unique|_

database_key(Schema, EntityName, ConstraintName, KeyColumnNames, 'primary key') :-
        database_constraint(Schema, EntityName, ConstraintName, primary_key(KeyColumnNames)).

database_key(Schema, EntityName, ConstraintName, KeyColumnNames, unique) :-
        database_constraint(Schema, EntityName, ConstraintName, unique(KeyColumnNames)).

database_key(Schema, EntityName, identity, [ColumnName], identity) :-
        database_identity(Schema, EntityName, ColumnName).

checked_domain(Domain, _, _, _, _, _, DataType):-
        database_domain(Domain, DataType).



cql_data_type(Schema,                                          % +
              TableSpec,                                       % +
              ColumnName,                                      % ?
              DatabaseDataType,                                % ?
              CharacterMaximumLength,                          % ?
              NumericPrecision,                                % ?
              NumericScale,                                    % ?
              Domain,                                          % ?
              OrdinalPosition,                                 % ?
              Nullability) :-
        cached_data_type(Schema,
                           TableSpec,
                           ColumnName,
                           DatabaseDataType,
                           CharacterMaximumLength,
                           NumericPrecision,
                           NumericScale,
                           Domain,
                           OrdinalPosition,
                           Nullability,
                           _,    % IsIdentity
                           _).

cached_data_type(Schema, EntityName, ColumnName, DatabaseDataType, MaximumLength, NumericPrecision, NumericScale, Domain, _, boolean(AllowsNullsTrueFalse), boolean(IsIdentityTrueFalse), ColumnDefault) :-
        database_attribute(_, Schema, EntityName, ColumnName, domain(Domain), allows_nulls(AllowsNullsTrueFalse), is_identity(IsIdentityTrueFalse), ColumnDefault),
        checked_domain(Domain, _, _, _, _, _, DataType),
        data_type_length_precision_scale(DataType, DatabaseDataType, MaximumLength, NumericPrecision, NumericScale).

cached_data_type(Schema, EntityName, ColumnName, DatabaseDataType, MaximumLength, NumericPrecision, NumericScale, {null}, _, boolean(AllowsNullsTrueFalse), boolean(IsIdentityTrueFalse), ColumnDefault) :-
        database_attribute(_, Schema, EntityName, ColumnName, native_type(DataType), allows_nulls(AllowsNullsTrueFalse), is_identity(IsIdentityTrueFalse), ColumnDefault),
        data_type_length_precision_scale(DataType, DatabaseDataType, MaximumLength, NumericPrecision, NumericScale).

data_type_length_precision_scale(DataType, DatabaseDataType, MaximumLength, NumericPrecision, NumericScale) :-
        once(data_type_length_precision_scale_1(DataType, DatabaseDataType, MaximumLength, NumericPrecision, NumericScale)).

data_type_length_precision_scale_1(varchar(MaximumLength), varchar, MaximumLength, {null}, {null}).
data_type_length_precision_scale_1(nvarchar(MaximumLength), nvarchar, MaximumLength, {null}, {null}).
data_type_length_precision_scale_1(varbinary(MaximumLength), varbinary, MaximumLength, {null}, {null}).
data_type_length_precision_scale_1(decimal(NumericPrecision, NumericScale), decimal, {null}, NumericPrecision, NumericScale).
data_type_length_precision_scale_1(int, integer, 10, {null}, {null}).
data_type_length_precision_scale_1(DataType, DataType, {null}, {null}, {null}).


cql_port_label(unify,              'CALL  ', green).
cql_port_label(call,               'CALL  ', cyan).
cql_port_label(pending,            'EXIT  ', yellow).
cql_port_label(exit,               'EXIT  ', white).
cql_port_label(!,                  'EXIT !', white).
cql_port_label(fail,               'FAIL  ', magenta).
cql_port_label(exception,          'ERROR ', red).
cql_port_label(external_exception, 'ERROR ', red).


%%      cql_update_history_hook(+Schema,
%%                              +TableName,
%%                              +AttributeName,
%%                              +PrimaryKeyAttributeName,
%%                              +PrimaryKeyValue,
%%                              +ApplicationValueBefore,
%%                              +ApplicationValueAfter,
%%                              +AccessToken,
%%                              +TransactionId,
%%                              +TransactionTimestamp,
%%                              +ThreadId,
%%                              +Connection,
%%                              +Goal).
%
%       Use this hook predicate to actually record database attribute value changes.
%
%       You are free to let this predicate fail or raise an exception - the
%       database layer will ignore both of these eventualities.
%
%       @param Schema <atom>
%       @param TableName <atom> (lower case)
%       @param AttributeName <atom> (lower case)
%       @param PrimaryKeyAttributeName <atom> (lower case)
%       @param PrimaryKeyValue <int>
%       @param ApplicationValueBefore <domain dependent>
%       @param ApplicationValueAfter <domain dependent>
%       @param AccessToken <atom>
%       @param Info <term> (Arbitrary information determined by cql:cql_transaction_info_hook/5)
%       @param TransactionId <atom>
%       @param TransactionTimestamp <t7/7>
%       @param ThreadId <atom>
%       @param Connection <opaque>
%       @param Goal <goal term> The goal passed to pri_db_trans
:-multifile(cql_update_history_hook/14).


%%      cql_event_notification_table(+Schema,
%%                               +TableName).
:-multifile(cql_event_notification_table/2).

%%      cql_history_attribute(+Schema,
%%                            +TableName,
%%                            +ColumnName).
:-multifile(cql_history_attribute/3).

%%      sql_gripe_hook(+Level,
%%                     +Format,
%%                     +Args).
%       Called when something dubious is found by the SQL parser.

:-multifile(sql_gripe_hook/3).
sql_gripe(Level, Format, Args):-
        ignore(sql_gripe_hook(Level, Format, Args)).

timestamp_to_unambiguous_atom(timestamp(Y, M, D, H, Min, S, Ms), Atom):-
        format(atom(Atom), '~`0t~w~4+-~`0t~w~3+-~`0t~w~3+ ~`0t~w~3+:~`0t~w~3+:~`0t~w~3+.~`0t~w~4+', [Y, M, D, H, Min, S, Ms]).


:-multifile(cql_inline_domain_value_hook/2).
cql_domain_allowed_value(Domain, Value):-
        cql_inline_domain_value_hook(Domain, Value).


% FIXME: Clean up everything below this line

cql_perf_time(T):- get_time(T). % This needs to do something else for Windows because get_time/1 has insufficient granularity. Jan suggests a windows_perf_counter/1 predicate

% Tests to add:
% Can I do anything about the ugly compile: list of attributes in sqlite?
% Sqlite seems to handle longvarchar and varchar(max) and text as different types to varchar(N). The type conversion fails and we end up inserting NULLs

trime(Goal):-
        setup_call_catcher_cleanup(format('CALL  ~q~n', [Goal]),
                                   Goal,
                                   Catcher,
                                   ( Catcher == ! ->
                                       format('CUT   ~q~n', [Goal])
                                   ; Catcher == fail->
                                       format('FAIL  ~q~n', [Goal])
                                   ; Catcher == exit->
                                       format('EXIT  ~q~n', [Goal])
                                   ; Catcher = error(E)->
                                       format('ERROR  ~q~n~w~n', [Goal, E])
                                   )),
        ( var(Catcher)->
            format('PEND  ~q~n', [Goal])
        ; otherwise->
            true
        ).


%%      cql_normalize_name(+DBMS, +Name, -NormalizedName).
%       Normalize a name which is potentially longer than the DBMS allows to a unique truncation
:-multifile(cql_normalize_atom_hook/3).
cql_normalize_name(DBMS, CQLName, DBMSName):-
        ( cql_normalize_atom_hook(DBMS, CQLName, DBMSName)->
            true
        ; CQLName = DBMSName
        ).

:-multifile(cql_error_hook/3).
cql_error(ErrorId, Format, Arguments):-
        cql_error_hook(ErrorId, Format, Arguments).
cql_error(ErrorId, Format, Arguments):-
        format(atom(Message), Format, Arguments),
        throw(cql_error(ErrorId, Message)).
