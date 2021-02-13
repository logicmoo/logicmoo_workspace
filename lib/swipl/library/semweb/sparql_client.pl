/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2020, University of Amsterdam
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

:- module(sparql_client,
          [ sparql_query/3,             % +Query, -Row, +Options
            sparql_set_server/1,        % +Options
            sparql_read_xml_result/2,   % +Stream, -Result
            sparql_read_json_result/2   % +Input, -Result
          ]).
:- autoload(library(apply), [maplist/3, maplist/4, partition/4]).
:- autoload(library(gensym), [gensym/2]).
:- autoload(library(lists), [member/2]).
:- autoload(library(option), [select_option/3, select_option/4, merge_options/3]).
:- autoload(library(rdf), [load_rdf/2]).
:- autoload(library(readutil), [read_stream_to_codes/2]).
:- autoload(library(sgml), [load_structure/3]).
:- autoload(library(uri),
            [ uri_components/2,
              uri_data/3,
              uri_authority_components/2,
              uri_authority_data/3
            ]).
:- autoload(library(http/http_open), [http_open/3]).
:- autoload(library(http/json), [json_read/2]).
:- autoload(library(semweb/turtle), [rdf_read_turtle/3]).

/** <module> SPARQL client library

This module provides a SPARQL client.  For example:

```
?- sparql_query('select * where { ?x rdfs:label "Amsterdam" }', Row,
                [ host('dbpedia.org'), path('/sparql/')]).

Row = row('http://www.ontologyportal.org/WordNet#WN30-108949737') ;
false.
```

Or, querying a local server using an ``ASK`` query:

```
?- sparql_query('ask { owl:Class rdfs:label "Class" }', Row,
                [ host('localhost'), port(3020), path('/sparql/')]).
Row = true.
```

HTTPS servers are supported using the scheme(https) option:

```
?- sparql_query('select * where { ?x rdfs:label "Amsterdam"@nl }',
                Row,
                [ scheme(https),
                  host('query.wikidata.org'),
                  path('/sparql')
                ]).
```
*/


%!  sparql_query(+Query, -Result, +Options) is nondet.
%
%   Execute a SPARQL query on an HTTP  SPARQL endpoint. Query is an atom
%   that denotes the query. Result is unified   to a term rdf(S,P,O) for
%   ``CONSTRUCT`` and ``DESCRIBE``  queries,   row(...)  for  ``SELECT``
%   queries and `true` or `false` for ``ASK`` queries. Options are
%
%   Variables that are unbound in SPARQL   (e.g., due to SPARQL optional
%   clauses), are bound in Prolog to the atom `'$null$'`.
%
%	- endpoint(+URL)
%	  May be used as alternative to Scheme, Host, Port and Path
%	  to specify the endpoint in a single option.
%       - host(+Host)
%       - port(+Port)
%       - path(+Path)
%       - scheme(+Scheme)
%         The above four options set the location of the server.
%       - search(+ListOfParams)
%         Provide additional query parameters, such as the graph.
%       - variable_names(-ListOfNames)
%         Unifies ListOfNames with a list of atoms that describe the
%         names of the variables in a =SELECT= query.
%
%   Remaining options are passed to http_open/3.  The defaults for Host,
%   Port and Path can be  set   using  sparql_set_server/1.  The initial
%   default for port is 80 and path is `/sparql/`.
%
%   For  example,  the  ClioPatria  server   understands  the  parameter
%   `entailment`.  The  code  below  queries    for  all  triples  using
%   _rdfs_entailment.
%
%   ```
%   ?- sparql_query('select * where { ?s ?p ?o }',
%                   Row,
%                   [ search([entailment=rdfs])
%                   ]).
%   ```
%
%   Another useful option is the   `request_header`  which, for example,
%   may be used to trick force  a   server  to  reply using a particular
%   document format:
%
%   ```
%   ?- sparql_query(
%          'select * where { ?s ?p ?o }',
%           Row,
%           [ host('integbio.jp'),
%             path('/rdf/sparql'),
%             request_header('Accept' =
%                            'application/sparql-results+xml')
%           ]).
%   ```

sparql_query(Query, Row, Options) :-
    (   select_option(endpoint(URL), Options, Options5)
    ->  uri_components(URL, Components),
        uri_data(scheme, Components, Scheme),
        uri_data(authority, Components, Auth),
        uri_data(path, Components, Path),
        uri_data(search, Components, Extra),
        ignore(Extra = []),
        uri_authority_components(Auth, AComp),
        uri_authority_data(host, AComp, Host),
        uri_authority_data(port, AComp, Port),
        (   var(Port)
        ->  sparql_port(Scheme, Port, _, _)
        ;   true
        )
    ;   sparql_param(scheme(Scheme), Options,  Options1),
        sparql_port(Scheme, Port,    Options1, Options2),
        sparql_param(host(Host),     Options2, Options3),
        sparql_param(path(Path),     Options3, Options4),
        select_option(search(Extra), Options4, Options5, [])
    ),
    select_option(variable_names(VarNames), Options5, Options6, _),
    partition(is_url_option, Options6, UrlOptions, HTTPOptions),
    sparql_extra_headers(HTTPOptions0),
    merge_options(HTTPOptions, HTTPOptions0, HTTPOptions1),
    http_open([ scheme(Scheme),
                host(Host),
                port(Port),
                path(Path),
                search([ query = Query
                       | Extra
                       ])
              | UrlOptions
              ], In,
              [ header(content_type, ContentType),
                status_code(Status)
              | HTTPOptions1
              ]),
    plain_content_type(ContentType, CleanType),
    read_reply(Status, CleanType, In, VarNames, Row).

url_option(scheme).
url_option(user).
url_option(password).
url_option(host).
url_option(port).
url_option(path).
url_option(query_string).
url_option(search).

is_url_option(Name = _Value) :-
    url_option(Name),
    !.
is_url_option(Opt) :-
    compound(Opt),
    functor(Opt, Name, 1),
    url_option(Name).

%!  sparql_extra_headers(-List)
%
%   Send extra headers with the request. Note that, although we also
%   process RDF embedded in HTML, we do  not explicitely ask for it.
%   Doing so causes some   (e.g., http://w3.org/2004/02/skos/core to
%   reply with the HTML description rather than the RDF).

sparql_extra_headers(
        [ request_header('Accept' = 'application/sparql-results+xml, \c
                                     application/n-triples, \c
                                     application/x-turtle; q=0.9, \c
                                     application/turtle; q=0.9, \c
                                     text/turtle, \c
                                     application/sparql-results+json, \c
                                     application/rdf+xml, \c
                                     text/rdf+xml; q=0.8, \c
                                     */*; q=0.1')
        ]).

%!  read_reply(+Status, +ContentType, +In, -Close, -Row)

read_reply(200, ContentType, In, Close, Row) :-
    !,
    read_reply(ContentType, In, Close, Row).
read_reply(Status, _ContentType, In, _Close, _Row) :-
    call_cleanup(read_string(In, _, Reply),
                 close(In, [force(true)])),
    throw(error(sparql_error(Status, Reply), _)).

read_reply('application/rdf+xml', In, _, Row) :-
    !,
    call_cleanup(load_rdf(stream(In), RDF), close(In)),
    member(Row, RDF).
read_reply(MIME, In, _, Row) :-
    turtle_media_type(MIME),
    !,
    call_cleanup(rdf_read_turtle(stream(In), RDF, []), close(In)),
    member(Row, RDF).
read_reply(MIME, In, VarNames, Row) :-
    sparql_result_mime(MIME),
    !,
    call_cleanup(sparql_read_xml_result(stream(In), Result),
                 close(In)),
    varnames(Result, VarNames),
    xml_result(Result, Row).
read_reply(MIME, In, VarNames, Row) :-
    json_result_mime(MIME),
    !,
    call_cleanup(sparql_read_json_result(stream(In), Result),
                 close(In)),
    (   Result = select(VarNames, Rows)
    ->  member(Row, Rows)
    ;   Result = ask(True)
    ->  Row = True,
        VarNames = []
    ).
read_reply(Type, In, _, _) :-
    read_stream_to_codes(In, Codes),
    string_codes(Reply, Codes),
    close(In),
    throw(error(domain_error(sparql_result_document, Type),
                context(_, Reply))).

turtle_media_type('application/x-turtle').
turtle_media_type('application/turtle').
turtle_media_type('application/n-triples').
turtle_media_type('text/rdf+n3').
turtle_media_type('text/turtle').

sparql_result_mime('application/sparql-results+xml'). % official
sparql_result_mime('application/sparql-result+xml').

json_result_mime('application/sparql-results+json').


plain_content_type(Type, Plain) :-
    sub_atom(Type, B, _, _, (;)),
    !,
    sub_string(Type, 0, B, _, Main),
    normalize_space(atom(Plain), Main).
plain_content_type(Type, Type).

xml_result(ask(Bool), Result) :-
    !,
    Result = Bool.
xml_result(select(_VarNames, Rows), Result) :-
    member(Result, Rows).

varnames(ask(_), _).
varnames(select(VarTerm, _Rows), VarNames) :-
    VarTerm =.. [_|VarNames].


                 /*******************************
                 *            SETTINGS          *
                 *******************************/

:- dynamic
    sparql_setting/1.

sparql_setting(scheme(http)).
sparql_setting(path('/sparql/')).

sparql_param(Param, Options0, Options) :-
    select_option(Param, Options0, Options),
    !.
sparql_param(Param, Options, Options) :-
    sparql_setting(Param),
    !.
sparql_param(Param, Options, Options) :-
    functor(Param, Name, _),
    throw(error(existence_error(option, Name), _)).

sparql_port(_Scheme, Port, Options0, Options) :-
    select_option(port(Port), Options0, Options),
    !.
sparql_port(_Scheme, Port, Options, Options) :-
    sparql_setting(port(Port)),
    !.
sparql_port(http, 80, Options, Options) :-
    !.
sparql_port(https, 443, Options, Options) :-
    !.


%!  sparql_set_server(+OptionOrList)
%
%   Set sparql server default options.  Provided defaults are:
%   host, port and repository.  For example:
%
%       ==
%           sparql_set_server([ host(localhost),
%                               port(8080)
%                               path(world)
%                             ])
%       ==
%
%   The default for port is 80 and path is =|/sparql/|=.

sparql_set_server([]) :- !.
sparql_set_server([H|T]) :-
    !,
    sparql_set_server(H),
    sparql_set_server(T).
sparql_set_server(Term) :-
    functor(Term, Name, Arity),
    functor(Unbound, Name, Arity),
    retractall(sparql_setting(Unbound)),
    assert(sparql_setting(Term)).


                 /*******************************
                 *             RESULT           *
                 *******************************/

ns(sparql, 'http://www.w3.org/2005/sparql-results#').

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Read    the    SPARQL    XML    result     format    as    defined    in
http://www.w3.org/TR/rdf-sparql-XMLres/, version 6 April 2006.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

                 /*******************************
                 *        MACRO HANDLING        *
                 *******************************/

%       substitute 'sparql' by the namespace   defined  above for better
%       readability of the remainder of the code.

term_subst(V, _, _, V) :-
    var(V),
    !.
term_subst(F, F, T, T) :- !.
term_subst(C, F, T, C2) :-
    compound(C),
    !,
    functor(C, Name, Arity),
    functor(C2, Name, Arity),
    term_subst(0, Arity, C, F, T, C2).
term_subst(T, _, _, T).

term_subst(A, A, _, _, _, _) :- !.
term_subst(I0, Arity, C0, F, T, C) :-
    I is I0 + 1,
    arg(I, C0, A0),
    term_subst(A0, F, T, A),
    arg(I, C, A),
    term_subst(I, Arity, C0, F, T, C).

term_expansion(T0, T) :-
    ns(sparql, NS),
    term_subst(T0, sparql, NS, T).


                 /*******************************
                 *           READING            *
                 *******************************/

%!  sparql_read_xml_result(+Input, -Result)
%
%   Specs from http://www.w3.org/TR/rdf-sparql-XMLres/.  The returned
%   Result term is of the format:
%
%           * select(VarNames, Rows)
%           Where VarNames is a term v(Name, ...) and Rows is a
%           list of row(....) containing the column values in the
%           same order as the variable names.
%
%           * ask(Bool)
%           Where Bool is either =true= or =false=

:- thread_local
    bnode_map/2.

sparql_read_xml_result(Input, Result) :-
    load_structure(Input, DOM,
                   [ dialect(xmlns)
                   ]),
    call_cleanup(dom_to_result(DOM, Result),
                 retractall(bnode_map(_,_))).

dom_to_result(DOM, Result) :-
    (   sub_element(DOM, sparql:head, _HAtt, Content)
    ->  variables(Content, Vars)
    ;   Vars = []
    ),
    (   Vars == [],
        sub_element(DOM, sparql:boolean, _, [TrueFalse])
    ->  Result = ask(TrueFalse)
    ;   VarTerm =.. [v|Vars],
        Result = select(VarTerm, Rows),
        sub_element(DOM, sparql:results, _RAtt, RContent)
    ->  rows(RContent, Vars, Rows)
    ),
    !.                                   % Guarantee finalization

%!  variables(+DOM, -Varnames)
%
%   Deals with <variable name=Name>.  Head   also  may contain <link
%   href="..."/>. This points to additional   meta-data.  Not really
%   clear what we can do with that.

variables([], []).
variables([element(sparql:variable, Att, [])|T0], [Name|T]) :-
    !,
    memberchk(name=Name, Att),
    variables(T0, T).
variables([element(sparql:link, _, _)|T0], T) :-
    !,
    variables(T0, T).
variables([CDATA|T0], T) :-
    atomic(CDATA),
    variables(T0, T).


rows([], _, []).
rows([R|T0], Vars, [Row|T]) :-
    R = element(sparql:result, _, _),
    !,
    row_values(Vars, R, Values),
    Row =.. [row|Values],
    rows(T0, Vars, T).
rows([CDATA|T0], Vars, T) :-
    atomic(CDATA),
    rows(T0, Vars, T).

row_values([], _, []).
row_values([Var|VarT], DOM, [Value|ValueT]) :-
    (   sub_element(DOM, sparql:binding, Att, Content),
        memberchk(name=Var, Att)
    ->  value(Content, Value)
    ;   Value = '$null$'
    ),
    row_values(VarT, DOM, ValueT).

value([element(sparql:literal, Att, Content)|Rest], literal(Lit)) :-
    !,
    white(Rest),
    lit_value(Content, Value),
    (   memberchk(datatype=Type, Att)
    ->  Lit = type(Type, Value)
    ;   memberchk(xml:lang=Lang, Att)
    ->  Lit = lang(Lang, Value)
    ;   Lit = Value
    ).
value([element(sparql:uri, [], [URI])|Rest], URI) :- !,
    white(Rest).
value([element(sparql:bnode, [], [NodeID])|Rest], URI) :-
    !,
    white(Rest),
    bnode(NodeID, URI).
value([element(sparql:unbound, [], [])|Rest], '$null$') :-
    !,
    white(Rest).
value([CDATA|Rest], Value) :-
    atomic(CDATA),
    value(Rest, Value).


white([]).
white([CDATA|T]) :-
    atomic(CDATA),
    white(T).

lit_value([], '').
lit_value([Value], Value).


%!  sub_element(+DOM, +Name, -Atttribs, -Content)

sub_element(element(Name, Att, Content), Name, Att, Content).
sub_element(element(_, _, List), Name, Att, Content) :-
    sub_element(List, Name, Att, Content).
sub_element([H|T], Name, Att, Content) :-
    (   sub_element(H, Name, Att, Content)
    ;   sub_element(T, Name, Att, Content)
    ).


bnode(Name, URI) :-
    bnode_map(Name, URI),
    !.
bnode(Name, URI) :-
    gensym('__bnode', URI0),
    assertz(bnode_map(Name, URI0)),
    URI = URI0.


%!  sparql_read_json_result(+Input, -Result) is det.
%
%   The returned Result term is of the format:
%
%           * select(VarNames, Rows)
%           Where VarNames is a term v(Name, ...) and Rows is a
%           list of row(....) containing the column values in the
%           same order as the variable names.
%
%           * ask(Bool)
%           Where Bool is either =true= or =false=
%
%   @see http://www.w3.org/TR/rdf-sparql-json-res/

sparql_read_json_result(Input, Result) :-
    setup_call_cleanup(
        open_input(Input, In, Close),
        read_json_result(In, Result),
        close_input(Close)).

open_input(stream(In), In, Close) :-
    !,
    encoding(In, utf8, Close).
open_input(In, In, Close) :-
    is_stream(In),
    !,
    encoding(In, utf8, Close).
open_input(File, In, close(In)) :-
    open(File, read, In, [encoding(utf8)]).

encoding(In, Encoding, Close) :-
    stream_property(In, encoding(Old)),
    (   Encoding == Old
    ->  Close = true
    ;   set_stream(In, encoding(Encoding)),
        Close = set_stream(In, Encoding, Old)
    ).

close_input(close(In)) :-
    !,
    retractall(bnode_map(_,_)),
    close(In).
close_input(_) :-
    retractall(bnode_map(_,_)).

read_json_result(In, Result) :-
    json_read(In, JSON),
    json_to_result(JSON, Result).

json_to_result(json([ head    = json(Head),
                      results = json(Body)
                    ]),
               select(Vars, Rows)) :-
    memberchk(vars=VarList, Head),
    Vars =.. [v|VarList],
    memberchk(bindings=Bindings, Body),
    !,
    maplist(json_row(VarList), Bindings, Rows).
json_to_result(json(JSon), ask(Boolean)) :-
    memberchk(boolean = @(Boolean), JSon).


json_row(Vars, json(Columns), Row) :-
    maplist(json_cell, Vars, Columns, Values),
    !,
    Row =.. [row|Values].
json_row(Vars, json(Columns), Row) :-
    maplist(json_cell_or_null(Columns), Vars, Values),
    Row =.. [row|Values].

json_cell(Var, Var=json(JValue), Value) :-
    memberchk(type=Type, JValue),
    jvalue(Type, JValue, Value).

json_cell_or_null(Columns, Var, Value) :-
    memberchk(Var=json(JValue), Columns),
    !,
    memberchk(type=Type, JValue),
    jvalue(Type, JValue, Value).
json_cell_or_null(_, _, '$null$').

jvalue(uri, JValue, URI) :-
    memberchk(value=URI, JValue).
jvalue(literal, JValue, literal(Literal)) :-
    memberchk(value=Value, JValue),
    (   memberchk('xml:lang'=Lang, JValue)
    ->  Literal = lang(Lang, Value)
    ;   memberchk('datatype'=Type, JValue)
    ->  Literal = type(Type, Value)
    ;   Literal = Value
    ).
jvalue('typed-literal', JValue, literal(type(Type, Value))) :-
    memberchk(value=Value, JValue),
    memberchk('datatype'=Type, JValue).
jvalue(bnode, JValue, URI) :-
    memberchk(value=NodeID, JValue),
    bnode(NodeID, URI).
