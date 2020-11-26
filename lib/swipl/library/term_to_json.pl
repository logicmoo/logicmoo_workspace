:- encoding(utf8).
/*  Part of SWI-Prolog

    Author:        Torbjörn Lager and Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2015, Torbjörn Lager,
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

:- module(term_to_json,
          [ term_to_json/3,                     % +Term, +Bindings, -Json
            term_to_json/2                      % +Term, -Json
          ]).
:- autoload(library(apply),[maplist/2,maplist/3]).
:- autoload(library(error),[must_be/2,domain_error/2]).

%!  term_to_json(+Term, +Bindings, -JsonTerm) is det.
%!  term_to_json(+Term, -JsonTerm) is det.
%
%   Convert any general Prolog term into   a JSON term. Prolog lists
%   are  treated  in  a  special  way.  Also,  JSON  terms  are  not
%   converted. Mapping:
%
%     * Variable: =|{"type":"var", "name":<string>}|=
%     * Atom: =|{"type":"atom", "value":<string>}|=
%     * Integer: =|{"type":"integer", "value":<integer>}|=
%     * Float: =|{"type":"float", "value":<float>}|=
%     * List: JSON array
%     * Dict: a JSON object. Values are processed recursively.
%       (the tag is ignored)
%     * json([Key=Value, ...]): a JSON object Values are processed
%       recursively.
%     * compound: =|{"type":"compound", "functor":<string>, "args":<array>}|=
%
%   @param  Bindings is a list of Name=Var terms for variables that
%           get their name from the environment.

term_to_json(Term, JSON) :-
    term_to_json(Term, [], JSON).
term_to_json(Term, Bindings, JSON) :-
    findall(X,
            (   maplist(bind_var, Bindings),
                numbervars(Term, 0, _, [singletons(true)]),
                to_json(Term, X)
            ),
            [JSON]).

bind_var(Name=Var) :-
    (   var(Var)
    ->  Var = '$VAR'(Name)
    ;   true
    ).

to_json(Term, '_') :-
    var(Term),
    !.
to_json(@(Symbol), Symbol) :-                   % compatibility
    atom(Symbol),
    json_symbol(Symbol),
    !.
to_json(Term, Term) :-
    atom(Term),
    !.                          % interpreted as a string
to_json(Term, Term) :-
    string(Term),
    !.
to_json(Term, Value) :-
    integer(Term),
    !,
    (   Term >= -(2**31), Term < 2**31
    ->  Value = Term
    ;   atom_number(Value, Term)
    ).
to_json(Term, Term) :-
    float(Term),
    !.
to_json(Term, JsonList) :-
    is_list(Term),
    !,
    maplist(to_json, Term, JsonList).
to_json(json(Pairs0), Term) :-
    must_be(list, Pairs0),
    maplist(pair_value_to_json_ex, Pairs0, Pairs),
    dict_pairs(Term, json, Pairs).
to_json(Term0, Term) :-
    is_dict(Term0),
    !,
    dict_pairs(Term0, Tag, Pairs0),
    maplist(pair_value_to_json, Pairs0, Pairs),
    dict_pairs(Term, Tag, Pairs).
to_json('$VAR'(Name), VarName) :-
    !,
    format(string(VarName), '~W', ['$VAR'(Name), [numbervars(true)]]).
to_json(Term, json{functor:F, args:JsonArgs}) :-
    Term =.. [F|Args],
    maplist(to_json, Args, JsonArgs).

json_symbol(null).
json_symbol(true).
json_symbol(false).

pair_value_to_json(Key-Value0, Key-Value) :-
    to_json(Value0, Value).

pair_value_to_json_ex(Key=Value0, Key-Value) :-
    (atom(Key) ; integer(Key)),
    !,
    to_json(Value0, Value).
pair_value_to_json_ex(Elem, _) :-
    domain_error(json_key_value, Elem).

