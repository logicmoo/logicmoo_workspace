/*  Part of SWI-Prolog

    Author:        Eshel Yaron
    E-mail:        eshel@swi-prolog.org
    Copyright (c)  2022, SWI-Prolog Solutions B.V.
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

:- module(graphql,
          [ graphql_read_document/3,      % +Source, -Document, +Options
            graphql_execute_document/4,   % +URI, +Document, -Result, +Options
            graphql_document_to_string/3, % +Document, -String, +Options
            graphql_document_to_codes/3,  % +Document, -Codes, +Options
            graphql/4                     % Quasi-quotation syntax
          ]).

/** <module> GraphQL interface

This module provides predicates for working with GraphQL, a query
language for HTTP-based APIs.

*/

:- autoload(library(quasi_quotations),
            [phrase_from_quasi_quotation/2, quasi_quotation_syntax/1]).
:- autoload(library(dcg/basics),
            [ prolog_var_name//1,
              digit//1,
              digits//1,
              xdigit//1,
              xinteger//1
            ]).
:- autoload(library(dcg/high_order), [optional//2, sequence//2]).
:- autoload(library(http/json), [atom_json_dict/3]).
:- autoload(library(http/http_client), [http_post/4]).
:- autoload(library(apply), [include/3]).
:- autoload(library(lists), [member/2, append/3]).
:- autoload(library(option), [option/3, option/2]).
:- autoload(library(pure_input), [phrase_from_stream/2]).
:- use_module(library(http/http_json), []).


%! graphql_execute_document(+URI, +Document, -Result, +Options) is det.
%
%  Send GraphQL document Document to be executed by the GraphQL
%  endpoint at URI.
%
%  Document is a Prolog term representing the abstract syntax tree of
%  the GraphQL document, as obtained from e.g. graphql_read_document/3
%  or graphql/4 quasi-quotation.  Result is unified with a dict
%  representing the JSON formatted response received from the server.
%
%  The following example shows how graphql_execute_document/4 can be
%  used to expose a simple GraphQL interface to Prolog:
%
%  ```prolog
%  sourcehut_repository_description(Owner, Repo, Desc) :-
%      graphql_execute_document("https://git.sr.ht/query",
%                               {| graphql(Owner, Repo) ||
%                                  { user(username: <Owner>)
%                                    { repository(name: <Repo>)
%                                      { description } } } |},
%                               Dict,
%                               [token(...)]),
%      Desc = Dict.get(data/user/repository/description).
%
%
%  ?- sourcehut_repository_description("eshel", "sourcehut.pl", Desc).
%  Desc = "SWI-Prolog package implementing a SourceHut GraphQL API client.".
%  ```
%
%  Options is a list whose elemenets are one of the following:
%  - token(+Token)
%    Token is string that will be used as an OAuth2.0 token. See also
%    graphql_auth_token_hook/2.
%  - variables(+Variables)
%    Variables is a dict with keys corresponding to GraphQL
%    variables that occur in Document as `$key`.
%    Variables is sent to the remote to the GraphQL endpoint in
%    JSON format for server-side interpolation.
%    For more information about GraphQL variables, see
%    https://spec.graphql.org/draft/#sec-Language.


:- predicate_options(graphql_execute_document/4, 4,
                     [variables(list),
                      data(list),
                      pass_to(graphql_auth_token/3, 3),
                      pass_to(graphql_document_to_string/3, 3)]).


graphql_execute_document(URI,
                         Document,
                         Result,
                         Options) :-
    option(variables(Variables), Options, null),
    option(data(Data), Options, [map=null]),
    graphql_auth_token(URI, Token, Options),
    graphql_document_to_string(Document, Text, Options),
    atom_json_dict(Operations,
                   _{query: Text, variables: Variables},
                   []),
    http_post(URI,
              form_data([operations=Operations|Data]),
              Result,
              [json_object(dict), authorization(bearer(Token))]).


:- predicate_options(graphql_auth_token/3, 3,
                     [token(string)]).


graphql_auth_token(_URI, Token, Options) :-
    option(token(Token), Options),
    !.
graphql_auth_token(URI, Token, _Options) :-
    graphql_auth_token_hook(URI, Token),
    !.


%! graphql_auth_token_hook(+URI, -Token) is nondet.
%
%  Multifile, dynamic hook. graphql_execute_document/4 consults this
%  hook to set authorization token for the GraphQL endpoint at URI.
:- multifile graphql_auth_token_hook/2.
:- dynamic   graphql_auth_token_hook/2.




%! graphql(+Content, +Args, -VariableNames, -Result) is det.
%
%  Quasi-quotation syntax for embedding GraphQL in Prolog text.
%  Prolog variables can be incorporated in the quasi-quotation using
%  the special lexical construct "<VarName>" where VarName is the name
%  of a variable that is an element of Args. See also option
%  variable_names(+VarNames) in graphql_read_document/3.
%
%  Result is a term representing the given GraphQL document in the
%  same format as used by graphql_read_document/3.

:- quasi_quotation_syntax(graphql).

graphql(Content, Args, VariableNames0, Result) :-
    include(qq_var(Args), VariableNames0, VariableNames),
    phrase_from_quasi_quotation(graphql_tokens(Tokens,
                                               [variable_names(VariableNames)]),
                                Content),
    phrase(graphql_executable_document(Result), Tokens).


qq_var(Vars, _=Var) :- member(V, Vars), V == Var, !.


%! graphql_read_document(+Source, -Document, +Options) is semidet.
%
%  True when Document is a term representing the abstract syntax tree
%  obtained from parsing Source as a GraphQL executable document.
%
%  Document is a list of terms representing _GraphQL executable
%  definitions_, each being one of:
%
%    - operation(Type, Name, VariableDefinitions, Directives, SelectionSet)
%      Type is one of the atoms _query_, _mutation_ or _subscription_.
%      Name is either the atom _null_ or a string denoting the
%      name given to this specific operation.
%
%      VariableDefinitions is a list of terms of the form
%      variable_definition(VarName, VarType, VarDefault, VarDirs)
%      where _VarName_ is a string denoting the name of the variable,
%      _VarType_ is a term denoting the _GraphQL type_ of the defined
%      variable in the format described below, _VarDefault_ is a term
%      denoting a default _GraphQL value_ associated with the defined
%      variable, and _VarDirs_ is a possibly empty list of _GraphQL
%      directives_, each of which a term DirName-DirArgs where DirName
%      is the string name of the directive and DirArgs is a Prolog
%      dict denoting the directive arguments.
%
%      Directives is a possibly empty list of _GraphQL
%      directives_ associated with the given GraphQL operation.
%
%      SelectionSet is a list of _GraphQL selections_, each selection
%      is one of field(FieldAlias, FieldName, FieldArgs, FieldDirs,
%      FieldSelection), in which case _FieldAlias_ is either _null_ or
%      a string denoting the alias of the field, _FieldName_ is a
%      string denoting the name of the field, _FieldArgs_ is a dict
%      denoting the field arguments, _FieldDirs_ is a list of _GraphQL
%      directives_ and _FieldSelection_ is a list of _GraphQL
%      selections_ nested below the given field. Otherwise, each
%      selection can have the from fragment_spread(FragName,
%      FragDirs) where _FragName_ is a string denoting the name of
%      the fragment and FragDirs is a possibly empty list of _GraphQL
%      directives_.  Lastly, each selection can have the form
%      inline_fragment(IFragTypeCondition, IFragDirs,
%      IFragSelectionSet) where _IFragTypeCondition_ is a string
%      denoting a type condition associated with the inline fragment,
%      _IFragDirs_ denotes _GraphQL directives_ associated with it,
%      and _IFragSelectionSet_ is a list of _GraphQL selections_
%      specified by the fragment.
%
%    - fragment(Name, Type, Directives, SelectionSet)
%      Where Name is a string denoting the name of the fragment, Type
%      is a _GraphQL type_ term, directives is possibly empty list
%      of _GraphQL directives_, and SelectionSet is a list of
%      _GraphQL selections_ associated with the fragment.
%
%  A _GraphQL type_ is represented as one of:
%
%    - named_type(Name)
%      Where Name is a string denoting the name of the type.
%    - list_type(Type)
%      Where Type is a _GraphQL type_.
%    - non_null_type(Type)
%      Where Type is a _GraphQL type_.
%
%  A _GraphQL value_ is represented as one of:
%
%    - true
%      Represents the GraphQL `true` value.
%    - false
%      Represents the GraphQL `false` value.
%    - null
%      Represents the GraphQL `null` value.
%    - enum(E)
%      Represents the GraphQL enum corresponding to the string E.
%    - String
%      A Prolog string String represents the same GraphQL string.
%    - Integer
%      A Prolog integer Integer represents the same GraphQL integer.
%    - Float
%      A Prolog float Float represents the same GraphQL float.
%    - ListOfValues
%      A Prolog list ListOfValues whose elements are _GraphQL values_
%      represents the GraphQL list value whose elements are
%      represented by the elements of ListOfValues.
%    - DictOfValues
%      A Prolog dict DictOfValues with _GraphQL values_ for values
%      represents the GraphQL object value whose fields are represented
%      by the key-value pairs of DictOfValues.
%
%  Source can be one of:
%
%    - codes(+Codes, -Rest)
%      Codes is a list of codes representing the source GraphQL text.
%      Rest is unified with the list of trailing codes starting from
%      the first code that was not recognized as part of a valid
%      GraphQL token.
%    - codes(+Codes)
%      Same as codes(Codes, []).
%    - string(+String)
%      String is a string representing the source GraphQL text.
%    - Stream
%      Stream is a stream from which the source GraphQL text
%      is read with phrase_from_stream/2.
%
%  Options is a list whose elements can be one of:
%
%    - variable_names([Name=Value|...])
%      A list of associations of Prolog variable names, given as atoms,
%      to _GraphQL values_.
%
%      Occurences of the special lexical construct "<Name>" (that is,
%      ASCII 60, then the codes of the atom Name, then ASCII 62) in
%      Source are expanded in Document to the _GraphQL value_ Value.
%      This option can be used to interpolate GraphQL documents with
%      values given in Prolog representation.

:- predicate_options(graphql_read_document/3, 3,
                     [variable_names(list)]).

graphql_read_document(codes(Codes, Rest), Document, Options) =>
    phrase(graphql_tokens(Tokens, Options), Codes, Rest),
    phrase(graphql_executable_document(Document), Tokens).
graphql_read_document(codes(Codes), Document, Options) =>
    phrase(graphql_tokens(Tokens, Options), Codes),
    phrase(graphql_executable_document(Document), Tokens).
graphql_read_document(string(String), Document, Options) =>
    string_codes(String, Codes),
    phrase(graphql_tokens(Tokens, Options), Codes),
    phrase(graphql_executable_document(Document), Tokens).
graphql_read_document(Stream, Document, Options) =>
    phrase_from_stream(graphql_tokens(Tokens, Options), Stream),
    phrase(graphql_executable_document(Document), Tokens).


graphql_executable_document([H|T]) -->
    graphql_executable_definition(H),
    graphql_executable_definitions(T).


graphql_executable_definitions([H|T]) -->
    graphql_executable_definition(H),
    !,
    graphql_executable_definitions(T).
graphql_executable_definitions([]) --> [].


graphql_executable_definition(operation(Type,
                                        Name,
                                        VariableDefinitions,
                                        Directives,
                                        SelectionSet)) -->
    graphql_operation_definition(Type,
                                 Name,
                                 VariableDefinitions,
                                 Directives,
                                 SelectionSet),
    !.
graphql_executable_definition(fragment(Name,
                                       Type,
                                       Directives,
                                       SelectionSet)) -->
    [name("fragment"), name(Name), name("on"), name(Type)],
    graphql_inline_fragment(Directives, SelectionSet).


graphql_operation_definition(T, N, V, D, S) -->
    graphql_operation_type(T),
    !,
    graphql_query(N, V, D, S).
graphql_operation_definition(query, null, [], [], S) -->
    graphql_selection_set(S).


graphql_operation_type(query) -->
    [name("query")],
    !.
graphql_operation_type(mutation) -->
    [name("mutation")],
    !.
graphql_operation_type(subscription) -->
    [name("subscription")],
    !.


graphql_query(N, V, D, S) -->
    optional([name(N)],
             {N=null}),
    optional(graphql_variables_definition(V),
             {V=[]}),
    optional(graphql_directives(D),
             {D=[]}),
    graphql_selection_set(S).


graphql_variables_definition([H|T]) -->
    ['('],
    graphql_variable_definition(H),
    sequence(graphql_variable_definition, T),
    [')'].


graphql_variable_definition(variable_definition(Var, Type, Def, Dirs)) -->
    graphql_variable(Var),
    [':'],
    graphql_type(Type),
    optional(graphql_default_value(Def),
             {Def=null}),
    optional(graphql_directives(Dirs),
             {Dirs=[]}).


graphql_type(T) -->
    graphql_type_(T0),
    graphql_type_nullable(T0, T).


graphql_type_(named_type(N)) -->
    [name(N)],
    !.
graphql_type_(list_type(T)) -->
    ['['],
    !,
    graphql_type(T),
    [']'].


graphql_type_nullable(T, non_null_type(T)) -->
    ['!'],
    !.
graphql_type_nullable(T, T) -->
    [].

graphql_variable(V) -->
    ['$', name(V)].


graphql_default_value(V) -->
    graphql_value([const(true)], V).


graphql_value(_, V) -->
    [prolog(V)],
    !.
graphql_value(Options, variable(V)) -->
    {   \+ option(const(true), Options)   },
    graphql_variable(V),
    !.
graphql_value(_, N) -->
    [integer(N)],
    !.
graphql_value(_, F) -->
    [float(F)],
    !.
graphql_value(_, S) -->
    [string(S)],
    !.
graphql_value(_, V) -->
    [name(N)],
    !,
    {   graphql_name_value(N, V)   }.
graphql_value(Options, L) -->
    graphql_list_value(Options, L),
    !.
graphql_value(Options, O) -->
    graphql_object_value(Options, O),
    !.


graphql_name_value("true" , true   ) :- !.
graphql_name_value("false", false  ) :- !.
graphql_name_value("null" , null   ) :- !.
graphql_name_value(N      , enum(N)).


graphql_list_value(Options, L) -->
    ['['],
    sequence(graphql_value(Options), L),
    [']'].


graphql_object_value(Options, O) -->
    ['{'],
    sequence(graphql_object_field(Options), O0),
    ['}'],
    {   dict_pairs(O, _, O0)   }.


graphql_object_field(Options, Name-Value) -->
    [name(Name0), ':'],
    {   atom_string(Name, Name0)   },
    graphql_value(Options, Value).


graphql_directives([H|T]) -->
    graphql_directive(H),
    graphql_directives_(T).


graphql_directives_([H|T]) -->
    graphql_directive(H),
    !,
    graphql_directives_(T).
graphql_directives_([]) --> [].


graphql_directive(N-A) -->
    ['@', name(N)],
    optional(graphql_arguments(A),
             {A=_{}}).


graphql_arguments(A) -->
    ['('],
    graphql_argument(H),
    sequence(graphql_argument, T),
    [')'],
    {   dict_pairs(A, _, [H|T])   }.


graphql_argument(N-V) -->
    [name(N0), ':'],
    {   atom_string(N, N0)   },
    graphql_value([], V).


graphql_selection_set([H|T]) -->
    ['{'],
    graphql_selection(H),
    sequence(graphql_selection, T),
    ['}'].

graphql_selection(field(A, N, R, D, S)) -->
    graphql_field(A, N, R, D, S),
    !.
graphql_selection(F) -->
    ['...'],
    graphql_selection_(F).


graphql_selection_(F) -->
    [name(N)],
    !,
    graphql_selection__(N, F).
graphql_selection_(inline_fragment(null, D, S)) -->
    graphql_inline_fragment(D, S).


graphql_selection__("on", inline_fragment(T, D, S)) -->
    !,
    [name(T)],
    graphql_inline_fragment(D, S).
graphql_selection__(N, fragment_spread(N, D)) -->
    optional(graphql_directives(D),
             {D=[]}).


graphql_inline_fragment(D, S) -->
    optional(graphql_directives(D),
             {D=[]}),
    graphql_selection_set(S).


graphql_field(Alias, Name, Args, Directives, SelectionSet) -->
    [name(Name0)],
    graphql_field_(Name0, Alias, Name, Args, Directives, SelectionSet).


graphql_field_(Alias, Alias, Name, Args, Directives, SelectionSet) -->
    [':'],
    !,
    [name(Name)],
    graphql_field__(Args, Directives, SelectionSet).
graphql_field_(Name, null, Name, Args, Directives, SelectionSet) -->
    graphql_field__(Args, Directives, SelectionSet).


graphql_field__(Args, Directives, SelectionSet) -->
    optional(graphql_arguments(Args),
             {Args=[]}),
    optional(graphql_directives(Directives),
             {Directives=[]}),
    optional(graphql_selection_set(SelectionSet),
             {SelectionSet=[]}).


%! graphql_tokens(-Ts, +Options)// is det.
graphql_tokens(Ts, Options) -->
    graphql_ignored,
    graphql_tokens_(Ts, Options).


graphql_tokens_([H|T], Options) -->
    graphql_token(H, Options),
    !,
    graphql_tokens(T, Options).
graphql_tokens_([   ], _Options) --> [].


%! graphql_token(-T)// is semidet.
%
%  https://spec.graphql.org/draft/#Token
graphql_token(P, _Options)         --> graphql_punctuator(P).
graphql_token(name(N), _Options)   --> graphql_name(N).
graphql_token(N, _Options)         --> graphql_numeric_value(N).
graphql_token(string(S), _Options) --> graphql_string_value(S).
graphql_token(prolog(E), Options)  --> graphql_prolog(E, Options).


graphql_prolog(V, Options) -->
    "<",
    prolog_var_name(N),
    ">",
    {   option(variable_names(VarNames), Options, []),
        memberchk(N=V, VarNames)
    }.



%! graphql_ignored// is semidet.
%
%  https://spec.graphql.org/draft/#Ignored
graphql_ignored --> graphql_white_space    , !, graphql_ignored.
graphql_ignored --> graphql_line_terminator, !, graphql_ignored.
graphql_ignored --> graphql_comment        , !, graphql_ignored.
graphql_ignored --> graphql_comma          , !, graphql_ignored.
graphql_ignored --> [].


%! graphql_white_space// is semidet.
%
%  https://spec.graphql.org/draft/#WhiteSpace
graphql_white_space --> graphql_white_space(_).


graphql_white_space(0'  ) --> " ", !.
graphql_white_space(0'\t) --> "\t".


%! graphql_line_terminator// is semidet.
%
%  https://spec.graphql.org/draft/#LineTerminator
graphql_line_terminator --> "\n".
graphql_line_terminator --> "\r".


%! graphql_comment// is semidet.
%
%  https://spec.graphql.org/draft/#Comment
graphql_comment --> "#", graphql_comment_chars.


%! graphql_comment_chars// is semidet.
graphql_comment_chars --> graphql_comment_char, !, graphql_comment_chars.
graphql_comment_chars --> [].


%! graphql_comment_char// is semidet.
%
%  https://spec.graphql.org/draft/#CommentChar
graphql_comment_char --> graphql_line_terminator, !, { false }.
graphql_comment_char --> [_], !.


%! graphql_commma// is semidet.
%
%  https://spec.graphql.org/draft/#Comma
graphql_comma --> ",".


%! graphql_punctuator(-P)// is semidet.
%
%  https://spec.graphql.org/draft/#Punctuator
graphql_punctuator('!') --> "!", !.
graphql_punctuator('$') --> "$", !.
graphql_punctuator('&') --> "&", !.
graphql_punctuator('(') --> "(", !.
graphql_punctuator(')') --> ")", !.
graphql_punctuator('...') --> "...", !.
graphql_punctuator(':') --> ":", !.
graphql_punctuator('=') --> "=", !.
graphql_punctuator('@') --> "@", !.
graphql_punctuator('[') --> "[", !.
graphql_punctuator(']') --> "]", !.
graphql_punctuator('{') --> "{", !.
graphql_punctuator('}') --> "}", !.
graphql_punctuator('|') --> "|", !.


%! graphql_name(-N)// is semidet.
%
%  https://spec.graphql.org/draft/#Name
graphql_name(N) -->
    graphql_name_start(H),
    graphql_name_(T),
    {   string_codes(N, [H|T])   }.


graphql_name_([H|T]) -->
    graphql_name_continue(H),
    !,
    graphql_name_(T).
graphql_name_([]) --> [].


%! graphql_name_start(-S)// is semidet.
%
%  https://spec.graphql.org/draft/#NameStart
graphql_name_start(L)  --> graphql_letter(L).
graphql_name_start(0'_) --> "_".


%! graphql_name_continue(-C)// is semidet.
%
%  https://spec.graphql.org/draft/#NameContinue
graphql_name_continue(L)   --> graphql_letter(L).
graphql_name_continue(D)   --> digit(D).
graphql_name_continue(0'_) --> "_".


%! graphql_letter(-L)// is semidet.
%
%  https://spec.graphql.org/draft/#Letter
graphql_letter(L) -->
    [L],
    {   (   0'A =< L, L =< 0'Z
        ->  true
        ;   0'a =< L, L =< 0'z
        )
    }.


graphql_numeric_value(N) -->
    graphql_integer_part(I),
    graphql_numeric_value_(I, N).


graphql_numeric_value_(I, N) -->
    graphql_fractional_part(F),
    !,
    graphql_numeric_value__(I, F, N).
graphql_numeric_value_(I, N) -->
    graphql_numeric_value__(I, [], N).


graphql_fractional_part([0'., H|T]) -->
    ".",
    !,
    digits([H|T]).


graphql_exponent_part([E|T]) -->
    graphql_exponent_indicator(E),
    !,
    graphql_exponent_part_(T).


graphql_exponent_part_([S,H|T]) -->
    graphql_sign(S),
    digits([H|T]).
graphql_exponent_part_([H|T]) -->
    digits([H|T]).


graphql_exponent_indicator(0'e) --> "e", !.
graphql_exponent_indicator(0'E) --> "E".


graphql_sign(0'-) --> "-", !.
graphql_sign(0'+) --> "+".


graphql_numeric_value__(I, F, float(N)) -->
    graphql_exponent_part(E),
    !,
    {   append(I, F, H),
        append(H, E, C),
        number_codes(N, C)
    }.
graphql_numeric_value__(I, [], integer(N)) -->
    !,
    {    number_codes(N, I)
    }.
graphql_numeric_value__(I, F, float(N)) -->
    {   append(I, F, C),
        number_codes(N, C)
    }.


graphql_integer_part([0'-|T]) -->
    "-",
    !,
    graphql_natural_part(T).
graphql_integer_part(T) -->
    graphql_natural_part(T).

graphql_natural_part([0'0]) -->
    "0",
    !.
graphql_natural_part([H|T]) -->
    graphql_non_zero_digit(H),
    digits(T).


graphql_non_zero_digit(D) -->
    [D],
    {   0'1 =< D, D =< 0'9   }.


graphql_string_value(S) -->
    "\"",
    graphql_string_value_(S).


graphql_string_value_(S) -->
    "\"",
    !,
    graphql_string_value__(S).
graphql_string_value_(S) -->
    graphql_string_body(S).


graphql_string_value__(S) -->
    "\"",
    !,
    graphql_block_string(S).
graphql_string_value__("") --> [].


graphql_string_body(S) -->
    graphql_string_character(H),
    graphql_string_body_(H, S).

graphql_string_body_(H, S) -->
    graphql_string_characters(T),
    {   string_codes(S, [H|T])   }.


graphql_string_characters([]) --> "\"", !.
graphql_string_characters([H|T]) -->
    graphql_string_character(H),
    graphql_string_characters(T).


graphql_string_character(C) -->
    "\\",
    !,
    graphql_string_escape_sequence(C).
graphql_string_character(C) -->
    [C].


graphql_string_escape_sequence(U) -->
    "u",
    !,
    graphql_string_escape_hex(U).
graphql_string_escape_sequence(C) -->
    [C],
    {   memberchk(C, `\"\\/bfnrt`)   }.

graphql_string_escape_hex(U) -->
    "{",
    !,
    xinteger(U),
    "}".
graphql_string_escape_hex(U) -->
    xdigit(A),
    xdigit(B),
    xdigit(C),
    xdigit(D),
    {   U is (A << 12) + (B << 8) + (C << 4) + D   }.


graphql_block_string("") -->
    graphql_block_string_quote,
    !.
graphql_block_string(S) -->
    graphql_line_terminator,
    !,
    graphql_block_string(S).
graphql_block_string(S) -->
    graphql_white_space(C),
    !,
    graphql_block_string_empty_initial_line([C|T]-T, 1, S).
graphql_block_string(S) -->
    graphql_block_string_characters(C),
    {   append(C, T, H)   },
    graphql_block_string_first_line(H-T, S).


graphql_block_string_empty_initial_line(_, _, "") -->
    graphql_block_string_quote,
    !.
graphql_block_string_empty_initial_line(_, _, S) -->
    graphql_line_terminator,
    !,
    graphql_block_string(S).
graphql_block_string_empty_initial_line(H-[C|T], I0, S) -->
    graphql_white_space(C),
    !,
    {   I is I0 + 1   },
    graphql_block_string_empty_initial_line(H-T, I, S).
graphql_block_string_empty_initial_line(H-T0, I, S) -->
    graphql_block_string_characters(C),
    {   append(C, T, T0),
        length(C, N0),
        N is N0 + I
    },
    graphql_block_string_initial_line(H-T, N, I, S).


graphql_block_string_first_line(L, S) -->
    graphql_block_string_quote,
    !,
    {   graphql_block_string_close(L, [], 0, S)  }.
graphql_block_string_first_line(L, S) -->
    graphql_line_terminator,
    !,
    graphql_block_string_line_indent(L, M-M, C-C, 0, 1Inf, S).
graphql_block_string_first_line(H-T0, S) -->
    graphql_block_string_characters(C),
    {   append(C, T, T0)   },
    graphql_block_string_first_line(H-T, S).


graphql_block_string_initial_line(CH-CT, N, I, S) -->
    graphql_block_string_quote,
    !,
    {   graphql_block_string_close(F-F, [line(CH, CT, N)], I, S)  }.
graphql_block_string_initial_line(CH-CT, N, I, S) -->
    graphql_line_terminator,
    !,
    graphql_block_string_line_indent(F-F, [line(CH,CT,N)|MoreLines]-MoreLines, L-L, 0, I, S).
graphql_block_string_initial_line(H-T0, N0, I, S) -->
    graphql_block_string_characters(C),
    {   append(C, T, T0),
        length(C, N1),
        N is N0 + N1
    },
    graphql_block_string_initial_line(H-T, N, I, S).


graphql_block_string_characters([34,34,34]) -->
    "\\",
    graphql_block_string_quote,
    !.
graphql_block_string_characters([C]) -->
    [C].


graphql_block_string_line_indent(F, MH-[], _, _, I, S) -->
    graphql_block_string_quote,
    !,
    {   graphql_block_string_close(F, MH, I, S)  }.
graphql_block_string_line_indent(F, M, LH-LT, N, I, S) -->
    graphql_line_terminator,
    !,
    graphql_block_string_maybe_trailing_empty_line(F, M, [line(LH, LT, N)|T]-T, C-C, 0, I, S).
graphql_block_string_line_indent(F, M, H-[C|T], N0, I, S) -->
    graphql_white_space(C),
    !,
    {   N is N0 + 1   },
    graphql_block_string_line_indent(F, M, H-T, N, I, S).
graphql_block_string_line_indent(F, M, H-T0, N0, I0, S) -->
    graphql_block_string_characters(C),
    {   append(C, T, T0),
        I is min(N0, I0),
        length(C, N1),
        N is N0 + N1
    },
    graphql_block_string_line(F, M, H-T, N, I, S).


graphql_block_string_maybe_trailing_empty_line(F, MH-[], _W, _C, _N, I, S) -->
    graphql_block_string_quote,
    !,
    {   graphql_block_string_close(F, MH, I, S)  }.
graphql_block_string_maybe_trailing_empty_line(F, M, WH-[line(CH0,CT0,N)|WT], CH0-CT0, N, I, S) -->
    graphql_line_terminator,
    !,
    graphql_block_string_maybe_trailing_empty_line(F, M, WH-WT, C-C, 0, I, S).
graphql_block_string_maybe_trailing_empty_line(F, M, W, CH-[C|CT], N0, I, S) -->
    graphql_white_space(C),
    !,
    {   N is N0 + 1   },
    graphql_block_string_maybe_trailing_empty_line(F, M, W, CH-CT, N, I, S).
graphql_block_string_maybe_trailing_empty_line(F, MH-WH, WH-WT, H-T0, N0, I0, S) -->
    graphql_block_string_characters(C),
    {   append(C, T, T0),
        I is min(N0, I0),
        length(C, N1),
        N is N0 + N1
    },
    graphql_block_string_line(F, MH-WT, H-T, N, I, S).


graphql_block_string_line(F, MH-[line(CH, CT, N)], CH-CT, N, I, S) -->
    graphql_block_string_quote,
    !,
    {   graphql_block_string_close(F, MH, I, S)  }.
graphql_block_string_line(F, MH-[line(CH, CT, N)|MT], CH-CT, N, I, S) -->
    graphql_line_terminator,
    !,
    graphql_block_string_maybe_trailing_empty_line(F, MH-MT, W-W, C-C, 0, I, S).
graphql_block_string_line(F, M, H-T0, N0, I, S) -->
    graphql_block_string_characters(C),
    {   append(C, T, T0),
        length(C, N1),
        N is N0 + N1
    },
    graphql_block_string_line(F, M, H-T, N, I, S).


graphql_block_string_close(FirstLineH-FirstLineT, [line(H0, T, L)|MoreLines], Indent, String) :-
    FirstLineH == FirstLineT,
    !,
    graphql_block_string_dedent_line(H0, L, Indent, H),
    graphql_block_string_combine_more_lines(MoreLines, Indent, T),
    string_codes(String, H).
graphql_block_string_close(FirstLineH-FirstLineT, MoreLines, Indent, String) :-
    graphql_block_string_combine_more_lines(MoreLines, Indent, FirstLineT),
    string_codes(String, FirstLineH).

graphql_block_string_combine_more_lines([], _, []) :-
    !.
graphql_block_string_combine_more_lines([line(H0, T, L)|MoreLines],
                                        Indent,
                                        [0'\n|H]) :-
    graphql_block_string_dedent_line(H0, L, Indent, H),
    graphql_block_string_combine_more_lines(MoreLines, Indent, T).


graphql_block_string_dedent_line(Line0, Length, Indent, Line) :-
    PrefixLength is min(Length, Indent),
    length(Prefix, PrefixLength),
    append(Prefix, Line, Line0).


graphql_block_string_quote --> "\"\"\"".




%! graphql_document_to_string(+Document, -String, +Options) is det.
%
%  Serialize the GraphQL document Document and unify String with the
%  resulting string.
%
%  Options are passed on to graphql_document_to_codes/3.

:- predicate_options(graphql_document_to_string/3, 3,
                     [pass_to(graphql_document_to_codes/3, 3)]).

graphql_document_to_string(Document, String, Options) :-
    graphql_document_to_codes(Document, Codes, Options),
    string_codes(String, Codes).


%! graphql_document_to_codes(+Document, -Codes, +Options) is det.
%
%  Serialize Document, a Prolog term representing a GraphQL document
%  as obtained from graphql_read_document/3 or the graphql/4
%  quasi-quotation, and unify Codes with the resulting list of
%  character codes.
%
%  Options are a list whose elements are one of:
%    - separator(+Sep)
%      Sep is a list of codes to be used for separating adjancent
%      GraphQL values in Codes. Defaults to a single space.
%      This option can be used to separate values with commas
%      (which are optional throughout GraphQL) by passing
%      e.g. separator(`, `).

:- predicate_options(graphql_document_to_codes/3, 3,
                     [separator(list)]).

graphql_document_to_codes(Document, Codes, Options) :-
    phrase(graphql_write_document(Document, Options), Codes).


graphql_write_document([H|T], Options) -->
    graphql_write_definition(H, Options),
    graphql_write_document(T, Options).
graphql_write_document([], _Options) --> [], !.


graphql_write_definition(operation(Type,
                                   Name,
                                   VariableDefinitions,
                                   Directives,
                                   SelectionSet), Options) -->
    graphql_write_name(Type, Options),
    graphql_write_name_maybe(Name, Options),
    graphql_write_variable_definitions(VariableDefinitions, Options),
    graphql_write_directives_and_selection_set(Directives,
                                               SelectionSet,
                                               Options).


graphql_write_name(Name, _Options) -->
    {   string_codes(Name, Codes)   },
    Codes.


graphql_write_name_maybe(null, _Options) --> [], !.
graphql_write_name_maybe(Name, Options) -->
    graphql_write_separator(Options),
    graphql_write_name(Name, Options).


graphql_write_variable_definitions([   ], _Options) --> [], !.
graphql_write_variable_definitions([H|T], Options) -->
    "(",
    graphql_write_variable_definition(H, Options),
    graphql_write_variable_definitions_(T, Options),
    ")".


graphql_write_variable_definitions_([   ], _Options) --> [], !.
graphql_write_variable_definitions_([H|T], Options) -->
    graphql_write_separator(Options),
    graphql_write_variable_definition(H, Options),
    graphql_write_variable_definitions_(T, Options).


graphql_write_variable_definition(variable_definition(Name,
                                                      Type,
                                                      Default,
                                                      Directives),
                                  Options) -->
    `$`,
    graphql_write_name(Name, Options),
    `:`,
    graphql_write_type(Type, Options),
    graphql_write_value_maybe(Default, Options),
    graphql_write_directives(Directives, Options).


graphql_write_value_maybe(null, _Options) -->
    !,
    [].
graphql_write_value_maybe(Value, Options) -->
    graphql_write_separator(Options),
    graphql_write_value(Value, Options).

graphql_write_value(enum(N), _Options) -->
    !,
    {   string_codes(N, Codes)    },
    Codes.
graphql_write_value(variable(V), _Options) -->
    !,
    {   string_codes(V, Codes)    },
    [0'$|Codes].
graphql_write_value(Atom, _Options) -->
    {   atom(Atom),
        !,
        atom_codes(Atom, Codes)
    },
    Codes.
graphql_write_value(String, Options) -->
    {   string(String),
        !,
        string_codes(String, Codes)
    },
    "\"",
    graphql_write_string(Codes, Options),
    "\"".
graphql_write_value(Number, _Options) -->
    {   number(Number),
        !,
        number_codes(Number, Codes)
    },
    Codes.
graphql_write_value(List, Options) -->
    {   is_list(List)   },
    !,
    "[",
    graphql_write_list_value(List, Options),
    "]".
graphql_write_value(Dict, Options) -->
    {   is_dict(Dict),
        !,
        dict_pairs(Dict, _, Object)
    },
    "{",
    graphql_write_pairs(Object, Options),
    "}".


graphql_write_pairs([H|T], Options) -->
    !,
    graphql_write_pair(H, Options),
    graphql_write_pairs_(T, Options).
graphql_write_pairs([], _) --> [].


graphql_write_pairs_([H|T], Options) -->
    graphql_write_separator(Options),
    graphql_write_pair(H, Options),
    graphql_write_pairs_(T, Options).
graphql_write_pairs_([], _) --> [].


graphql_write_pair(N-V, Options) -->
    graphql_write_name(N, Options),
    ":",
    graphql_write_value(V, Options).


%! graphql_write_string(+Codes, +Options)// is det.
%
%  Generates Codes, except that codes in Codes that are not allowed in
%  GraphQL string values are replaced by their escape sequences.
graphql_write_string([], _Options) --> !, [].
graphql_write_string([0'\"|T], Options) -->
    !,
    "\\\"",
    graphql_write_string(T, Options).
graphql_write_string([0'\\|T], Options) -->
    !,
    "\\\\",
    graphql_write_string(T, Options).
graphql_write_string([0'\n|T], Options) -->
    !,
    "\\n",
    graphql_write_string(T, Options).
graphql_write_string([0'\r|T], Options) -->
    !,
    "\\r",
    graphql_write_string(T, Options).
graphql_write_string([H|T], Options) -->
    [H],
    graphql_write_string(T, Options).


graphql_write_list_value([], _Options) --> !, [].
graphql_write_list_value([H|T], Options) -->
    graphql_write_value(H, Options),
    graphql_write_list_value_(T, Options).


graphql_write_list_value_([], _Options) --> !, [].
graphql_write_list_value_([H|T], Options) -->
    graphql_write_separator(Options),
    graphql_write_value(H, Options),
    graphql_write_list_value_(T, Options).


graphql_write_type(non_null_type(Type), Options) -->
    !,
    graphql_write_type(Type, Options),
    "!".
graphql_write_type(named_type(Name), Options) -->
    !,
    graphql_write_name(Name, Options).
graphql_write_type(list_type(Type), Options) -->
    "[",
    graphql_write_type(Type, Options),
    "]".


graphql_write_directives([   ], _Options) --> [], !.
graphql_write_directives([H|T], Options) -->
    graphql_write_separator(Options),
    graphql_write_directive(H, Options),
    graphql_write_directives(T, Options).


graphql_write_directive(Name-Arguments, Options) -->
    graphql_write_name(Name, Options),
    graphql_write_arguments(Arguments, Options).


graphql_write_arguments(_{}, _Options) --> !, [].
graphql_write_arguments(Args, Options) -->
    {   dict_pairs(Args, _, Pairs)   },
    "(",
    graphql_write_pairs(Pairs, Options),
    ")".


graphql_write_selection_set([   ], _Options) --> [], !.
graphql_write_selection_set([H|T], Options) -->
    "{",
    graphql_write_selection(H, Options),
    graphql_write_selection_set_(T, Options),
    "}".


graphql_write_selection_set_([   ], _Options) --> [], !.
graphql_write_selection_set_([H|T], Options) -->
    graphql_write_separator(Options),
    graphql_write_selection(H, Options),
    graphql_write_selection_set_(T, Options).


graphql_write_selection(field(Alias,
                              Name,
                              Args,
                              Directives,
                              SelectionSet),
                       Options) -->
    graphql_write_field(Alias,
                        Name,
                        Args,
                        Directives,
                        SelectionSet,
                        Options).
graphql_write_selection(fragment_spread(Name, Directives),
                        Options) -->
    graphql_write_fragment_spread(Name, Directives, Options).
graphql_write_selection(inline_fragment(Type,
                                        Directives,
                                        SelectionSet),
                        Options) -->
    graphql_write_inline_fragment(Type,
                                  Directives,
                                  SelectionSet,
                                  Options).


graphql_write_field(null,
                    Name,
                    Arguments,
                    Directives,
                    SelectionSet,
                    Options) -->
    !,
    graphql_write_field_(Name,
                         Arguments,
                         Directives,
                         SelectionSet,
                         Options).
graphql_write_field(Alias,
                    Name,
                    Arguments,
                    Directives,
                    SelectionSet,
                    Options) -->
    graphql_write_name(Alias, Options),
    ": ",
    graphql_write_field_(Name,
                         Arguments,
                         Directives,
                         SelectionSet,
                         Options).


graphql_write_field_(Name,
                     Arguments,
                     Directives,
                     SelectionSet,
                     Options) -->
    graphql_write_name(Name, Options),
    graphql_write_arguments(Arguments, Options),
    graphql_write_directives_and_selection_set(Directives,
                                               SelectionSet,
                                               Options).


graphql_write_directives_and_selection_set(Directives,
                                           SelectionSet,
                                           Options) -->
    graphql_write_directives(Directives, Options),
    graphql_write_selection_set(SelectionSet, Options).


graphql_write_separator(Options) -->
    {   option(separator(Sep), Options, [0' ])   },
    Sep.


graphql_write_fragment_spread(Name, Directives, Options) -->
    "...",
    graphql_write_separator(Options),
    graphql_write_name(Name, Options),
    graphql_write_directives(Directives, Options).


graphql_write_inline_fragment(TypeCondition,
                              Directives,
                              SelectionSet,
                              Options) -->
    "...",
    graphql_write_separator(Options),
    graphql_write_type_condition(TypeCondition, Options),
    graphql_write_directives_and_selection_set(Directives,
                                               SelectionSet,
                                               Options).


graphql_write_type_condition(TypeCondition, Options) -->
    "on ", graphql_write_name(TypeCondition, Options).
