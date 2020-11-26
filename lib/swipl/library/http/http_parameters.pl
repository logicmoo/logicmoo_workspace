/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2015, University of Amsterdam
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

:- module(http_parameters,
          [ http_parameters/2,          % +Request, -Params
            http_parameters/3,          % +Request, -Params, +TypeG

            http_convert_parameter/4,   % +Options, +FieldName, +ValIn, -ValOut
            http_convert_parameters/2,  % +Data, +Params
            http_convert_parameters/3   % +Data, +Params, :DeclGoal
          ]).
:- use_module(http_client).
:- use_module(http_multipart_plugin).
:- use_module(http_hook).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(broadcast)).

:- multifile
    http:convert_parameter/3.

:- predicate_options(http_parameters/3, 3,
                     [ form_data(-list),
                       attribute_declarations(callable)
                     ]).

/** <module> Extract parameters (GET and POST) from HTTP requests

Most   code   doesn't   need  to   use  this   directly;  instead   use
library(http/http_server),  which  combines   this  library  with   the
typical HTTP libraries that most servers need.

This module is used to extract the value  of GET or POST parameters from
an HTTP request. The typical usage is e.g.,

    ==
    :- http_handler('/register_user', register_user, []).

    register_user(Request) :-
        http_parameters(Request,
                        [ name(Name, []),
                          sex(Sex, [oneof([male,female])]),
                          birth_year(BY, [between(1850,10000)])
                        ]),
        register_user(Name, Sex, BY),
        html_reply_page(title('New user added'),
                        ...).
    ==

@see http_dispatch.pl dispatches requests to predicates.
*/

:- meta_predicate
    http_parameters(+, ?, :),
    http_convert_parameters(+, ?, 2).

%!  http_parameters(+Request, ?Parms) is det.
%!  http_parameters(+Request, ?Parms, :Options) is det.
%
%   Get HTTP GET  or  POST   form-data,  applying  type  validation,
%   default values, etc.  Provided options are:
%
%           * attribute_declarations(:Goal)
%           Causes the declarations for an attributed named A to be
%           fetched using call(Goal, A, Declarations).
%
%           * form_data(-Data)
%           Return the data read from the GET por POST request as a
%           list Name = Value.  All data, including name/value pairs
%           used for Parms, is unified with Data.
%
%   The attribute_declarations hook allows   sharing the declaration
%   of attribute-properties between many http_parameters/3 calls. In
%   this form, the requested attribute takes   only one argument and
%   the options are acquired by calling the hook. For example:
%
%       ==
%           ...,
%           http_parameters(Request,
%                           [ sex(Sex)
%                           ],
%                           [ attribute_declarations(http_param)
%                           ]),
%           ...
%
%       http_param(sex, [ oneof(male, female),
%                         description('Sex of the person')
%                       ]).
%       ==
%
%   @bug If both request parameters  (?name=value&...)   and  a POST are
%   present the parameters are extracted   from  the request parameters.
%   Still, as it is valid to have   request parameters in a POST request
%   this predicate should not process POST   requests.  We will keep the
%   current behaviour as the it is not common for a request to have both
%   request   parameters   and    a    POST     data    of    the   type
%   =|application/x-www-form-urlencoded|=.
%
%   In the unlikely event this  poses  a   problem  the  request  may be
%   specified as [method(get)|Request].

http_parameters(Request, Params) :-
    http_parameters(Request, Params, []).

http_parameters(Request, Params, Options) :-
    must_be(list, Params),
    meta_options(is_meta, Options, QOptions),
    option(attribute_declarations(DeclGoal), QOptions, no_decl_goal),
    http_parms(Request, Params, DeclGoal, Form),
    (   memberchk(form_data(RForm), QOptions)
    ->  RForm = Form
    ;   true
    ).

is_meta(attribute_declarations).


http_parms(Request, Params, DeclGoal, Search) :-
    memberchk(search(Search), Request),
    !,
    fill_parameters(Params, Search, DeclGoal).
http_parms(Request, Params, DeclGoal, Data) :-
    memberchk(method(Method), Request),
    Method == post,
    memberchk(content_type(Content), Request),
    form_data_content_type(Content),
    !,
    debug(post_request, 'POST Request: ~p', [Request]),
    posted_form(Request, Data),
    fill_parameters(Params, Data, DeclGoal).
http_parms(_Request, Params, DeclGoal, []) :-
    fill_parameters(Params, [], DeclGoal).

:- multifile
    form_data_content_type/1.

form_data_content_type('application/x-www-form-urlencoded') :- !.
form_data_content_type(ContentType) :-
    sub_atom(ContentType, 0, _, _, 'application/x-www-form-urlencoded;').

%!  posted_form(+Request, -Data) is det.
%
%   True when Data is list  of   Name=Value  pairs  representing the
%   posted data.

posted_form(Request, _Data) :-
    nb_current(http_post_data, read),
    !,
    option(request_uri(URI), Request),
    throw(error(permission_error('re-read', 'POST data', URI),
                context(_, 'Attempt to re-read POST data'))).
posted_form(Request, Data) :-
    http_read_data(Request, Data, []),
    nb_setval(http_post_data, read),
    debug(post, 'POST Data: ~p', [Data]).

wipe_posted_data :-
    debug(post, 'Wiping posted data', []),
    nb_delete(http_post_data).

:- listen(http(request_finished(_Id, _Code, _Status, _CPU, _Bytes)),
          wipe_posted_data).


%!  fill_parameters(+ParamDecls, +FormData, +DeclGoal)
%
%   Fill values from the parameter list

:- meta_predicate fill_parameters(+, +, 2).

fill_parameters([], _, _).
fill_parameters([H|T], FormData, DeclGoal) :-
    fill_parameter(H, FormData, DeclGoal),
    fill_parameters(T, FormData, DeclGoal).

fill_parameter(H, _, _) :-
    var(H),
    !,
    instantiation_error(H).
fill_parameter(group(Members, _Options), FormData, DeclGoal) :-
    is_list(Members),
    !,
    fill_parameters(Members, FormData, DeclGoal).
fill_parameter(H, FormData, _) :-
    H =.. [Name,Value,Options],
    !,
    fill_param(Name, Value, Options, FormData).
fill_parameter(H, FormData, DeclGoal) :-
    H =.. [Name,Value],
    (   DeclGoal \== (-),
        call(DeclGoal, Name, Options)
    ->  true
    ;   throw(error(existence_error(attribute_declaration, Name), _))
    ),
    fill_param(Name, Value, Options, FormData).

fill_param(Name, Values, Options, FormData) :-
    memberchk(zero_or_more, Options),
    !,
    fill_param_list(FormData, Name, Values, Options).
fill_param(Name, Values, Options, FormData) :-
    memberchk(list(Type), Options),
    !,
    fill_param_list(FormData, Name, Values, [Type|Options]).
fill_param(Name, Value, Options, FormData) :-
    (   memberchk(Name=Value0, FormData),
        Value0 \== ''               % Not sure
    ->  http_convert_parameter(Options, Name, Value0, Value)
    ;   memberchk(default(Value), Options)
    ->  true
    ;   memberchk(optional(true), Options)
    ->  true
    ;   throw(error(existence_error(http_parameter, Name), _))
    ).


fill_param_list([], _, [], _).
fill_param_list([Name=Value0|Form], Name, [Value|VT], Options) :-
    !,
    http_convert_parameter(Options, Name, Value0, Value),
    fill_param_list(Form, Name, VT, Options).
fill_param_list([_|Form], Name, VT, Options) :-
    fill_param_list(Form, Name, VT, Options).


%!  http_convert_parameters(+Data, ?Params) is det.
%!  http_convert_parameters(+Data, ?Params, :AttrDecl) is det.
%
%   Implements the parameter  translation   of  http_parameters/2 or
%   http_parameters/3. I.e., http_parameters/2 for   a  POST request
%   can be implemented as:
%
%     ==
%     http_parameters(Request, Params) :-
%         http_read_data(Request, Data, []),
%         http_convert_parameters(Data, Params).
%     ==

http_convert_parameters(Data, ParamDecls) :-
    fill_parameters(ParamDecls, Data, no_decl_goal).
http_convert_parameters(Data, ParamDecls, DeclGoal) :-
    fill_parameters(ParamDecls, Data, DeclGoal).

no_decl_goal(_,_) :- fail.

%!  http_convert_parameter(+Options, +FieldName, +ValueIn, -ValueOut) is det.
%
%   Conversion of an HTTP form value. First tries the multifile hook
%   http:convert_parameter/3 and next the built-in checks.
%
%   @param Option           List as provided with the parameter
%   @param FieldName        Name of the HTTP field (for better message)
%   @param ValueIn          Atom value as received from HTTP layer
%   @param ValueOut         Possibly converted final value
%   @error type_error(Type, Value)

http_convert_parameter([], _, Value, Value).
http_convert_parameter([H|T], Field, Value0, Value) :-
    (   check_type_no_error(H, Value0, Value1)
    ->  catch(http_convert_parameter(T, Field, Value1, Value),
              error(Formal, _),
              throw(error(Formal, context(_, http_parameter(Field)))))
    ;   throw(error(type_error(H, Value0),
                    context(_, http_parameter(Field))))
    ).

check_type_no_error(Type, In, Out) :-
    http:convert_parameter(Type, In, Out),
    !.
check_type_no_error(Type, In, Out) :-
    check_type3(Type, In, Out).

%!  check_type3(+Type, +ValueIn, -ValueOut) is semidet.
%
%   HTTP parameter type-check for types that need converting.

check_type3((T1;T2), In, Out) :-
    !,
    (   check_type_no_error(T1, In, Out)
    ->  true
    ;   check_type_no_error(T2, In, Out)
    ).
check_type3(string, Atom, String) :-
    !,
    to_string(Atom, String).
check_type3(number, Atom, Number) :-
    !,
    to_number(Atom, Number).
check_type3(integer, Atom, Integer) :-
    !,
    to_number(Atom, Integer),
    integer(Integer).
check_type3(nonneg, Atom, Integer) :-
    !,
    to_number(Atom, Integer),
    integer(Integer),
    Integer >= 0.
check_type3(float, Atom, Float) :-
    !,
    to_number(Atom, Number),
    Float is float(Number).
check_type3(between(Low, High), Atom, Value) :-
    !,
    to_number(Atom, Number),
    (   (float(Low) ; float(High))
    ->  Value is float(Number)
    ;   Value = Number
    ),
    is_of_type(between(Low, High), Value).
check_type3(boolean, Atom, Bool) :-
    !,
    truth(Atom, Bool).
check_type3(Type, Atom, Atom) :-
    check_type2(Type, Atom).

to_number(In, Number) :-
    number(In), !, Number = In.
to_number(In, Number) :-
    atom(In),
    atom_number(In, Number).

to_string(In, String) :- string(In), !, String = In.
to_string(In, String) :- atom(In),   !, atom_string(In, String).
to_string(In, String) :- number(In), !, number_string(In, String).

%!  check_type2(+Type, +ValueIn) is semidet.
%
%   HTTP parameter type-check for types that need no conversion.

check_type2(oneof(Set), Value) :-
    !,
    memberchk(Value, Set).
check_type2(length > N, Value) :-
    !,
    atom_length(Value, Len),
    Len > N.
check_type2(length >= N, Value) :-
    !,
    atom_length(Value, Len),
    Len >= N.
check_type2(length < N, Value) :-
    !,
    atom_length(Value, Len),
    Len < N.
check_type2(length =< N, Value) :-
    !,
    atom_length(Value, Len),
    Len =< N.
check_type2(_, _).

%!  truth(+In, -Boolean) is semidet.
%
%   Translate some commonly used textual   representations  for true
%   and false into their canonical representation.

truth(true,    true).
truth('TRUE',  true).
truth(yes,     true).
truth('YES',   true).
truth(on,      true).
truth('ON',    true).                   % IE7
truth('1',     true).

truth(false,   false).
truth('FALSE', false).
truth(no,      false).
truth('NO',    false).
truth(off,     false).
truth('OFF',   false).
truth('0',     false).


                 /*******************************
                 *         XREF SUPPORT         *
                 *******************************/

:- multifile
    prolog:called_by/2,
    emacs_prolog_colours:goal_colours/2.

prolog:called_by(http_parameters(_,_,Options), [G+2]) :-
    option(attribute_declarations(G), Options, _),
    callable(G),
    !.

emacs_prolog_colours:goal_colours(http_parameters(_,_,Options),
                                  built_in-[classify, classify, Colours]) :-
    option_list_colours(Options, Colours).

option_list_colours(Var, error) :-
    var(Var),
    !.
option_list_colours([], classify) :- !.
option_list_colours(Term, list-Elements) :-
    Term = [_|_],
    !,
    option_list_colours_2(Term, Elements).
option_list_colours(_, error).

option_list_colours_2(Var, classify) :-
    var(Var).
option_list_colours_2([], []).
option_list_colours_2([H0|T0], [H|T]) :-
    option_colours(H0, H),
    option_list_colours_2(T0, T).

option_colours(Var,  classify) :-
    var(Var),
    !.
option_colours(_=_,  built_in-[classify,classify]) :- !.
option_colours(attribute_declarations(_),               % DCG = is a hack!
               option(attribute_declarations)-[dcg]) :- !.
option_colours(Term, option(Name)-[classify]) :-
    compound(Term),
    Term =.. [Name,_Value],
    !.
option_colours(_, error).

                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile prolog:error_message//1.
:- multifile prolog:message//1.

prolog:error_message(existence_error(http_parameter, Name)) -->
    [ 'Missing value for parameter "~w".'-[Name] ].
prolog:message(error(type_error(Type, Term), context(_, http_parameter(Param)))) -->
    { atom(Param) },
    [ 'Parameter "~w" must be '-[Param] ],
    param_type(Type),
    ['.  Found "~w".'-[Term] ].

param_type(length>N) -->
    !,
    ['longer than ~D characters'-[N]].
param_type(length>=N) -->
    !,
    ['at least ~D characters'-[N]].
param_type(length<N) -->
    !,
    ['shorter than ~D characters'-[N]].
param_type(length=<N) -->
    !,
    ['at most ~D characters'-[N]].
param_type(between(Low,High)) -->
    !,
    (   {float(Low);float(High)}
    ->  ['a number between ~w and ~w'-[Low,High]]
    ;   ['an integer between ~w and ~w'-[Low,High]]
    ).
param_type(oneof([Only])) -->
    !,
    ['"~w"'-[Only]].
param_type(oneof(List)) -->
    !,
    ['one of '-[]], oneof(List).
param_type(T) -->
    ['of type ~p'-[T]].


oneof([]) --> [].
oneof([H|T]) -->
    ['"~w"'-[H]],
    (   {T == []}
    ->  []
    ;   {T = [Last]}
    ->  [' or "~w"'-[Last] ]
    ;   [', '-[]],
        oneof(T)
    ).
