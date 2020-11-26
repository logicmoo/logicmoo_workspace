/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2020, University of Amsterdam
                              VU University Amsterdam
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

:- module(json,
          [ json_read/2,                % +Stream, -JSONTerm
            json_read/3,                % +Stream, -JSONTerm, +Options
            atom_json_term/3,           % ?Atom, ?JSONTerm, +Options
            json_write/2,               % +Stream, +Term
            json_write/3,               % +Stream, +Term, +Options
            is_json_term/1,             % @Term
            is_json_term/2,             % @Term, +Options
                                        % Version 7 dict support
            json_read_dict/2,           % +Stream, -Dict
            json_read_dict/3,           % +Stream, -Dict, +Options
            json_write_dict/2,          % +Stream, +Dict
            json_write_dict/3,          % +Stream, +Dict, +Options
            atom_json_dict/3            % ?Atom, ?JSONDict, +Options
          ]).
:- use_module(library(record)).
:- use_module(library(memfile)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(lists)).

:- use_foreign_library(foreign(json)).

:- multifile
    json_write_hook/4.                          % +Term, +Stream, +State, +Options

:- predicate_options(json_read/3, 3,
                     [ null(ground),
                       true(ground),
                       false(ground),
                       value_string_as(oneof([atom,string]))
                     ]).
:- predicate_options(json_write/3, 3,
                     [ indent(nonneg),
                       step(positive_integer),
                       tab(positive_integer),
                       width(nonneg),
                       null(ground),
                       true(ground),
                       false(ground),
                       serialize_unknown(boolean)
                     ]).
:- predicate_options(json_read_dict/3, 3,
                     [ tag(atom),
                       default_tag(atom),
                       pass_to(json_read/3, 3)
                     ]).
:- predicate_options(json_write_dict/3, 3,
                     [ tag(atom),
                       pass_to(json_write/3, 3)
                     ]).
:- predicate_options(is_json_term/2, 2,
                     [ null(ground),
                       true(ground),
                       false(ground)
                     ]).
:- predicate_options(atom_json_term/3, 3,
                     [ as(oneof([atom,string,codes])),
                       pass_to(json_read/3, 3),
                       pass_to(json_write/3, 3)
                     ]).

/** <module> Reading and writing JSON serialization

This module supports reading and  writing   JSON  objects.  This library
supports two Prolog representations (the   _new_  representation is only
supported in SWI-Prolog version 7 and later):

  - The *classical* representation is provided by json_read/3 and
    json_write/3.  This represents a JSON object as json(NameValueList),
    a JSON string as an atom and the JSON constants =null=, =true= and
    =false= as @(null), @(true) and @false.

  - The *new* representation is provided by json_read_dict/3 and
    json_write_dict/3. This represents a JSON object as a dict, a JSON
    string as a Prolog string and the JSON constants using the Prolog
    atoms =null=, =true= and =false=.

@author Jan Wielemaker
@see    http_json.pl links JSON to the HTTP client and server modules.
@see    json_convert.pl converts JSON Prolog terms to more comfortable
terms.
*/

:- record json_options(
              null:ground = @(null),
              true:ground = @(true),
              false:ground = @(false),
              end_of_file:ground = error,
              value_string_as:oneof([atom,string]) = atom,
              tag:atom = '',
              default_tag:atom).

default_json_dict_options(
    json_options(null, true, false, error, string, '', _)).


                 /*******************************
                 *       MAP TO/FROM TEXT       *
                 *******************************/

%!  atom_json_term(?Atom, ?JSONTerm, +Options) is det.
%
%   Convert between textual  representation  and   a  JSON  term. In
%   _write_ mode (JSONTerm to Atom), the option
%
%       * as(Type)
%       defines the output type, which is one of =atom= (default),
%       =string=, =codes= or =chars=.

atom_json_term(Atom, Term, Options) :-
    ground(Atom),
    !,
    setup_call_cleanup(
        ( atom_to_memory_file(Atom, MF),
          open_memory_file(MF, read, In, [free_on_close(true)])
        ),
        json_read(In, Term, Options),
        close(In)).
atom_json_term(Result, Term, Options) :-
    select_option(as(Type), Options, Options1, atom),
    (   type_term(Type, Result, Out)
    ->  true
    ;   must_be(oneof([atom,string,codes,chars]), Type)
    ),
    with_output_to(Out,
                   json_write(current_output, Term, Options1)).

type_term(atom,   Result, atom(Result)).
type_term(string, Result, string(Result)).
type_term(codes,  Result, codes(Result)).
type_term(chars,  Result, chars(Result)).


                 /*******************************
                 *           READING            *
                 *******************************/

%!  json_read(+Stream, -Term) is det.
%!  json_read(+Stream, -Term, +Options) is det.
%
%   Read next JSON value from Stream into a Prolog term. The
%   canonical representation for Term is:
%
%     * A JSON object is mapped to a term json(NameValueList), where
%       NameValueList is a list of Name=Value. Name is an atom
%       created from the JSON string.
%
%     * A JSON array is mapped to a Prolog list of JSON values.
%
%     * A JSON string is mapped to a Prolog atom
%
%     * A JSON number is mapped to a Prolog number
%
%     * The JSON constants =true= and =false= are mapped -like JPL-
%       to @(true) and @(false).
%
%     * The JSON constant =null= is mapped to the Prolog term
%       @(null)
%
%   Here is a complete example in  JSON and its corresponding Prolog
%   term.
%
%     ```
%     { "name":"Demo term",
%       "created": {
%         "day":null,
%         "month":"December",
%         "year":2007
%       },
%       "confirmed":true,
%       "members":[1,2,3]
%     }
%     ```
%
%     ```
%     json([ name='Demo term',
%            created=json([day= @null, month='December', year=2007]),
%            confirmed= @true,
%            members=[1, 2, 3]
%          ])
%     ```
%
%   The following options are processed:
%
%     - null(+NullTerm)
%       Term used to represent JSON =null=.  Default @(null)
%     - true(+TrueTerm)
%       Term used to represent JSON =true=.  Default @(true)
%     - false(+FalseTerm)
%       Term used to represent JSON =false=.  Default @(false)
%     - end_of_file(+ErrorOrTerm)
%       If end of file is reached after skipping white space
%       but before any input is processed take the following
%       action (default `error`):
%         - If ErrorOrTerm == `error`, throw an unexpected
%           end of file syntax error
%         - Otherwise return ErrorOrTerm.
%       Returning an status term is required to process
%       [Concatenated
%       JSON](https://en.wikipedia.org/wiki/JSON_streaming#Concatenated_JSON).
%       Suggested values are `@(eof)` or `end_of_file`.
%     - value_string_as(+Type)
%       Prolog type used for strings used as value. Default is `atom`.
%       The alternative is `string`, producing a packed string object.
%       Please note that `codes` or `chars` would produce ambiguous
%       output and are therefore not supported.
%
%   @see    json_read_dict/3 to read a JSON term using the version 7
%           extended data types.

json_read(Stream, Term) :-
    default_json_options(Options),
    (   json_value_top(Stream, Term, Options)
    ->  true
    ;   syntax_error(illegal_json, Stream)
    ).
json_read(Stream, Term, Options) :-
    make_json_options(Options, OptionTerm, _RestOptions),
    (   json_value_top(Stream, Term, OptionTerm)
    ->  true
    ;   syntax_error(illegal_json, Stream)
    ).

json_value_top(Stream, Term, Options) :-
    stream_property(Stream, type(binary)),
    !,
    setup_call_cleanup(
        set_stream(Stream, encoding(utf8)),
        json_value_top_(Stream, Term, Options),
        set_stream(Stream, type(binary))).
json_value_top(Stream, Term, Options) :-
    json_value_top_(Stream, Term, Options).

json_value_top_(Stream, Term, Options) :-
    get_code(Stream, C0),
    ws(C0, Stream, C1),
    (   C1 == -1
    ->  json_options_end_of_file(Options, Action),
        (   Action == error
        ->  syntax_error(unexpected_end_of_file, Stream)
        ;   Term = Action
        )
    ;   json_term_top(C1, Stream, Term, Options)
    ).

json_value(Stream, Term, Next, Options) :-
    get_code(Stream, C0),
    ws(C0, Stream, C1),
    (   C1 == -1
    ->  syntax_error(unexpected_end_of_file, Stream)
    ;   json_term(C1, Stream, Term, Next, Options)
    ).

json_term(C0, Stream, JSON, Next, Options) :-
    json_term_top(C0, Stream, JSON, Options),
    get_code(Stream, Next).

json_term_top(0'{, Stream, json(Pairs), Options) :-
    !,
    ws(Stream, C),
    json_pairs(C, Stream, Pairs, Options).
json_term_top(0'[, Stream, Array, Options) :-
    !,
    ws(Stream, C),
    json_array(C, Stream, Array, Options).
json_term_top(0'", Stream, String, Options) :-
    !,
    get_code(Stream, C1),
    json_string_codes(C1, Stream, Codes),
    json_options_value_string_as(Options, Type),
    codes_to_type(Type, Codes, String).
json_term_top(0'-, Stream, Number, _Options) :-
    !,
    json_read_number(Stream, 0'-, Number).
json_term_top(D, Stream, Number, _Options) :-
    between(0'0, 0'9, D),
    !,
    json_read_number(Stream, D, Number).
json_term_top(C, Stream, Constant, Options) :-
    json_read_constant(C, Stream, ID),
    json_constant(ID, Constant, Options).

json_pairs(0'}, _, [], _) :- !.
json_pairs(C0, Stream, [Pair|Tail], Options) :-
    json_pair(C0, Stream, Pair, C, Options),
    ws(C, Stream, Next),
    (   Next == 0',
    ->  ws(Stream, C2),
        json_pairs(C2, Stream, Tail, Options)
    ;   Next == 0'}
    ->  Tail = []
    ;   syntax_error(illegal_object, Stream)
    ).

json_pair(C0, Stream, Name=Value, Next, Options) :-
    json_string_as_atom(C0, Stream, Name),
    ws(Stream, C),
    C == 0':,
    json_value(Stream, Value, Next, Options).


json_array(0'], _, [], _) :- !.
json_array(C0, Stream, [Value|Tail], Options) :-
    json_term(C0, Stream, Value, C, Options),
    ws(C, Stream, Next),
    (   Next == 0',
    ->  ws(Stream, C1),
        json_array(C1, Stream, Tail, Options)
    ;   Next == 0']
    ->  Tail = []
    ;   syntax_error(illegal_array, Stream)
    ).

codes_to_type(atom, Codes, Atom) :-
    atom_codes(Atom, Codes).
codes_to_type(string, Codes, Atom) :-
    string_codes(Atom, Codes).
codes_to_type(codes, Codes, Codes).

json_string_as_atom(0'", Stream, Atom) :-
    get_code(Stream, C1),
    json_string_codes(C1, Stream, Codes),
    atom_codes(Atom, Codes).

json_string_codes(0'", _, []) :- !.
json_string_codes(0'\\, Stream, [H|T]) :-
    !,
    get_code(Stream, C0),
    (   escape(C0, Stream, H)
    ->  true
    ;   syntax_error(illegal_string_escape, Stream)
    ),
    get_code(Stream, C1),
    json_string_codes(C1, Stream, T).
json_string_codes(-1, Stream, _) :-
    !,
    syntax_error(eof_in_string, Stream).
json_string_codes(C, Stream, [C|T]) :-
    get_code(Stream, C1),
    json_string_codes(C1, Stream, T).

escape(0'", _, 0'") :- !.
escape(0'\\, _, 0'\\) :- !.
escape(0'/, _, 0'/) :- !.
escape(0'b, _, 0'\b) :- !.
escape(0'f, _, 0'\f) :- !.
escape(0'n, _, 0'\n) :- !.
escape(0'r, _, 0'\r) :- !.
escape(0't, _, 0'\t) :- !.
escape(0'u, Stream, C) :-
    !,
    get_code(Stream, C1),
    get_code(Stream, C2),
    get_code(Stream, C3),
    get_code(Stream, C4),
    code_type(C1, xdigit(D1)),
    code_type(C2, xdigit(D2)),
    code_type(C3, xdigit(D3)),
    code_type(C4, xdigit(D4)),
    C is D1<<12+D2<<8+D3<<4+D4.

json_read_constant(0't, Stream, true) :-
    !,
    must_see(`rue`, Stream, true).
json_read_constant(0'f, Stream, false) :-
    !,
    must_see(`alse`, Stream, false).
json_read_constant(0'n, Stream, null) :-
    !,
    must_see(`ull`, Stream, null).

must_see([], _Stream, _).
must_see([H|T], Stream, Name) :-
    get_code(Stream, C),
    (   C == H
    ->  true
    ;   syntax_error(json_expected(Name), Stream)
    ),
    must_see(T, Stream, Name).

json_constant(true, Constant, Options) :-
    !,
    json_options_true(Options, Constant).
json_constant(false, Constant, Options) :-
    !,
    json_options_false(Options, Constant).
json_constant(null, Constant, Options) :-
    !,
    json_options_null(Options, Constant).

%!  ws(+Stream, -Next) is det.
%!  ws(+C0, +Stream, -Next)
%
%   Skip white space on the Stream, returning the first non-ws
%   character.  Also skips =|//|= ... comments.

ws(Stream, Next) :-
    get_code(Stream, C0),
    json_skip_ws(Stream, C0, Next).

ws(C0, Stream, Next) :-
    json_skip_ws(Stream, C0, Next).

syntax_error(Message, Stream) :-
    stream_error_context(Stream, Context),
    throw(error(syntax_error(json(Message)), Context)).

stream_error_context(Stream, stream(Stream, Line, LinePos, CharNo)) :-
    stream_pair(Stream, Read, _),
    character_count(Read, CharNo),
    line_position(Read, LinePos),
    line_count(Read, Line).


                 /*******************************
                 *          JSON OUTPUT         *
                 *******************************/

%!  json_write_string(+Stream, +Text) is det.
%
%   Write a JSON string to  Stream.  Stream   must  be  opened  in a
%   Unicode capable encoding, typically UTF-8.

% foreign json_write_string/2.

%!  json_write_indent(+Stream, +Indent, +TabDistance) is det.
%
%   Newline and indent to  Indent.  A   Newline  is  only written if
%   line_position(Stream, Pos) is not 0. Then   it  writes Indent //
%   TabDistance tab characters and Indent mode TabDistance spaces.

% foreign json_write_indent/3.

%!  json_write(+Stream, +Term) is det.
%!  json_write(+Stream, +Term, +Options) is det.
%
%   Write a JSON term to Stream. The JSON   object is of the same format
%   as  produced  by  json_read/2,  though  we    allow  for  some  more
%   flexibility with regard to pairs  in   objects.  All  of Name=Value,
%   Name-Value and Name(Value) produce the same output.
%
%   Values can be of  the  form  #(Term),   which  causes  `Term`  to be
%   _stringified_ if it is not  an   atom  or string. Stringification is
%   based on term_string/2.
%
%   Rational numbers are emitted as  floating   point  numbers. The hook
%   json_write_hook/4  can  be  used   to    realize   domain   specific
%   alternatives.
%
%   The version 7 _dict_ type is supported   as well. Optionally, if the
%   dict has a _tag_, a  property  "type":"tag"   can  be  added  to the
%   object. This behaviour can be controlled using the =tag= option (see
%   below). For example:
%
%     ==
%     ?- json_write(current_output, point{x:1,y:2}).
%     {
%       "x":1,
%       "y":2
%     }
%     ==
%
%     ==
%     ?- json_write(current_output, point{x:1,y:2}, [tag(type)]).
%     {
%       "type":"point",
%       "x":1,
%       "y":2
%     }
%     ==
%
%   In addition to the options recognised by json_read/3, we process
%   the following options are recognised:
%
%       * width(+Width)
%       Width in which we try to format the result.  Too long lines
%       switch from _horizontal_ to _vertical_ layout for better
%       readability. If performance is critical and human
%       readability is not an issue use Width = 0, which causes a
%       single-line output.
%
%       * step(+Step)
%       Indentation increnment for next level.  Default is 2.
%
%       * tab(+TabDistance)
%       Distance between tab-stops.  If equal to Step, layout
%       is generated with one tab per level.
%
%       * serialize_unknown(+Boolean)
%       If =true= (default =false=), serialize unknown terms and
%       print them as a JSON string.  The default raises a type
%       error.  Note that this option only makes sense if you can
%       guarantee that the passed value is not an otherwise valid
%       Prolog reporesentation of a Prolog term.
%
%   If a string is  emitted,  the   sequence  =|</|=  is  emitted as
%   =|<\/|=. This is valid  JSON  syntax   which  ensures  that JSON
%   objects  can  be  safely  embedded  into  an  HTML  =|<script>|=
%   element.

%!  json_write_hook(+Term, +Stream, +State, +Options) is semidet.
%
%   Hook that can be used to  emit   a  JSON  representation for Term to
%   Stream. If the  predicate  succeeds  it   __must__  have  written  a
%   __valid__ JSON data element and if it fails it may not have produced
%   any output. This facility may be used  to map arbitrary Prolog terms
%   to JSON. It was added to manage   the  precision with which floating
%   point numbers are emitted.
%
%   Note that this hook is shared by all   users  of this library. It is
%   generally  adviced  to  map  a  unique    compound   term  to  avoid
%   interference with normal output.
%
%   @arg State and Options are opaque handles to the current output
%   state and settings.  Future versions may provide documented access
%   to these terms.  Currently it is adviced to ignore these arguments.



:- record json_write_state(indent:nonneg = 0,
                       step:positive_integer = 2,
                       tab:positive_integer = 8,
                       width:nonneg = 72,
                       serialize_unknown:boolean = false
                      ).

json_write(Stream, Term) :-
    json_write(Stream, Term, []).
json_write(Stream, Term, Options) :-
    make_json_write_state(Options, State, Options1),
    make_json_options(Options1, OptionTerm, _RestOptions),
    json_write_term(Term, Stream, State, OptionTerm).

json_write_term(Var, _, _, _) :-
    var(Var),
    !,
    instantiation_error(Var).
json_write_term(json(Pairs), Stream, State, Options) :-
    !,
    json_write_object(Pairs, Stream, State, Options).
json_write_term(Dict, Stream, State, Options) :-
    is_dict(Dict),
    !,
    dict_pairs(Dict, Tag, Pairs0),
    (   nonvar(Tag),
        json_options_tag(Options, Name),
        Name \== ''
    ->  Pairs = [Name-Tag|Pairs0]
    ;   Pairs = Pairs0
    ),
    json_write_object(Pairs, Stream, State, Options).
json_write_term(List, Stream, State, Options) :-
    is_list(List),
    !,
    space_if_not_at_left_margin(Stream, State),
    write(Stream, '['),
    (   json_write_state_width(State, Width),
        (   Width == 0
        ->  true
        ;   json_write_state_indent(State, Indent),
            json_print_length(List, Options, Width, Indent, _)
        )
    ->  set_width_of_json_write_state(0, State, State2),
        write_array_hor(List, Stream, State2, Options),
        write(Stream, ']')
    ;   step_indent(State, State2),
        write_array_ver(List, Stream, State2, Options),
        indent(Stream, State),
        write(Stream, ']')
    ).

json_write_term(Term, Stream, State, Options) :-
    json_write_hook(Term, Stream, State, Options),
    !.
json_write_term(Number, Stream, _State, _Options) :-
    number(Number),
    !,
    (   float(Number)
    ->  write(Stream, Number)
    ;   integer(Number)
    ->  write(Stream, Number)
    ;   Float is float(Number)              % rational number
    ->  write(Stream, Float)
    ).
json_write_term(True, Stream, _State, Options) :-
    json_options_true(Options, True),
    !,
    write(Stream, true).
json_write_term(False, Stream, _State, Options) :-
    json_options_false(Options, False),
    !,
    write(Stream, false).
json_write_term(Null, Stream, _State, Options) :-
    json_options_null(Options, Null),
    !,
    write(Stream, null).
json_write_term(#(Text), Stream, _State, _Options) :-
    !,
    (   (   atom(Text)
        ;   string(Text)
        )
    ->  json_write_string(Stream, Text)
    ;   term_string(Text, String),
        json_write_string(Stream, String)
    ).
json_write_term(String, Stream, _State, _Options) :-
    atom(String),
    !,
    json_write_string(Stream, String).
json_write_term(String, Stream, _State, _Options) :-
    string(String),
    !,
    json_write_string(Stream, String).
json_write_term(AnyTerm, Stream, State, _Options) :-
    (   json_write_state_serialize_unknown(State, true)
    ->  term_string(AnyTerm, String),
        json_write_string(Stream, String)
    ;   type_error(json_term, AnyTerm)
    ).

json_write_object(Pairs, Stream, State, Options) :-
    space_if_not_at_left_margin(Stream, State),
    write(Stream, '{'),
    (   json_write_state_width(State, Width),
        (   Width == 0
        ->  true
        ;   json_write_state_indent(State, Indent),
            json_print_length(json(Pairs), Options, Width, Indent, _)
        )
    ->  set_width_of_json_write_state(0, State, State2),
        write_pairs_hor(Pairs, Stream, State2, Options),
        write(Stream, '}')
    ;   step_indent(State, State2),
        write_pairs_ver(Pairs, Stream, State2, Options),
        indent(Stream, State),
        write(Stream, '}')
    ).


write_pairs_hor([], _, _, _).
write_pairs_hor([H|T], Stream, State, Options) :-
    json_pair(H, Name, Value),
    json_write_string(Stream, Name),
    write(Stream, ':'),
    json_write_term(Value, Stream, State, Options),
    (   T == []
    ->  true
    ;   write(Stream, ', '),
        write_pairs_hor(T, Stream, State, Options)
    ).

write_pairs_ver([], _, _, _).
write_pairs_ver([H|T], Stream, State, Options) :-
    indent(Stream, State),
    json_pair(H, Name, Value),
    json_write_string(Stream, Name),
    write(Stream, ':'),
    json_write_term(Value, Stream, State, Options),
    (   T == []
    ->  true
    ;   write(Stream, ','),
        write_pairs_ver(T, Stream, State, Options)
    ).


json_pair(Var, _, _) :-
    var(Var),
    !,
    instantiation_error(Var).
json_pair(Name=Value, Name, Value) :- !.
json_pair(Name-Value, Name, Value) :- !.
json_pair(NameValue, Name, Value) :-
    compound(NameValue),
    NameValue =.. [Name, Value],
    !.
json_pair(Pair, _, _) :-
    type_error(json_pair, Pair).


write_array_hor([], _, _, _).
write_array_hor([H|T], Stream, State, Options) :-
    json_write_term(H, Stream, State, Options),
    (   T == []
    ->  write(Stream, ' ')
    ;   write(Stream, ', '),
        write_array_hor(T, Stream, State, Options)
    ).

write_array_ver([], _, _, _).
write_array_ver([H|T], Stream, State, Options) :-
    indent(Stream, State),
    json_write_term(H, Stream, State, Options),
    (   T == []
    ->  true
    ;   write(Stream, ','),
        write_array_ver(T, Stream, State, Options)
    ).


indent(Stream, State) :-
    json_write_state_indent(State, Indent),
    json_write_state_tab(State, Tab),
    json_write_indent(Stream, Indent, Tab).

step_indent(State0, State) :-
    json_write_state_indent(State0, Indent),
    json_write_state_step(State0, Step),
    NewIndent is Indent+Step,
    set_indent_of_json_write_state(NewIndent, State0, State).

space_if_not_at_left_margin(Stream, State) :-
    stream_pair(Stream, _, Write),
    line_position(Write, LinePos),
    (   LinePos == 0
    ;   json_write_state_indent(State, LinePos)
    ),
    !.
space_if_not_at_left_margin(Stream, _) :-
    put_char(Stream, ' ').


%!  json_print_length(+Value, +Options, +Max, +Len0, +Len) is semidet.
%
%   True if Len-Len0 is the print-length of Value on a single line
%   and Len-Len0 =< Max.
%
%   @tbd    Escape sequences in strings are not considered.

json_print_length(Var, _, _, _, _) :-
    var(Var),
    !,
    instantiation_error(Var).
json_print_length(json(Pairs), Options, Max, Len0, Len) :-
    !,
    Len1 is Len0 + 2,
    Len1 =< Max,
    must_be(list, Pairs),
    pairs_print_length(Pairs, Options, Max, Len1, Len).
json_print_length(Dict, Options, Max, Len0, Len) :-
    is_dict(Dict),
    !,
    dict_pairs(Dict, _Tag, Pairs),
    Len1 is Len0 + 2,
    Len1 =< Max,
    pairs_print_length(Pairs, Options, Max, Len1, Len).
json_print_length(Array, Options, Max, Len0, Len) :-
    is_list(Array),
    !,
    Len1 is Len0 + 2,
    Len1 =< Max,
    array_print_length(Array, Options, Max, Len1, Len).
json_print_length(Null, Options, Max, Len0, Len) :-
    json_options_null(Options, Null),
    !,
    Len is Len0 + 4,
    Len =< Max.
json_print_length(False, Options, Max, Len0, Len) :-
    json_options_false(Options, False),
    !,
    Len is Len0 + 5,
    Len =< Max.
json_print_length(True, Options, Max, Len0, Len) :-
    json_options_true(Options, True),
    !,
    Len is Len0 + 4,
    Len =< Max.
json_print_length(Number, _Options, Max, Len0, Len) :-
    number(Number),
    !,
    write_length(Number, AL, []),
    Len is Len0 + AL,
    Len =< Max.
json_print_length(@(Id), _Options, Max, Len0, Len) :-
    atom(Id),
    !,
    atom_length(Id, IdLen),
    Len is Len0+IdLen,
    Len =< Max.
json_print_length(String, _Options, Max, Len0, Len) :-
    string_len(String, Len0, Len),
    !,
    Len =< Max.
json_print_length(AnyTerm, _Options, Max, Len0, Len) :-
    write_length(AnyTerm, AL, []),          % will be serialized
    Len is Len0 + AL+2,
    Len =< Max.

pairs_print_length([], _, _, Len, Len).
pairs_print_length([H|T], Options, Max, Len0, Len) :-
    pair_len(H, Options, Max, Len0, Len1),
    (   T == []
    ->  Len = Len1
    ;   Len2 is Len1 + 2,
        Len2 =< Max,
        pairs_print_length(T, Options, Max, Len2, Len)
    ).

pair_len(Pair, Options, Max, Len0, Len) :-
    compound(Pair),
    pair_nv(Pair, Name, Value),
    !,
    string_len(Name, Len0, Len1),
    Len2 is Len1+2,
    Len2 =< Max,
    json_print_length(Value, Options, Max, Len2, Len).
pair_len(Pair, _Options, _Max, _Len0, _Len) :-
    type_error(pair, Pair).

pair_nv(Name=Value, Name, Value) :- !.
pair_nv(Name-Value, Name, Value) :- !.
pair_nv(Term, Name, Value) :-
    compound_name_arguments(Term, Name, [Value]).

array_print_length([], _, _, Len, Len).
array_print_length([H|T], Options, Max, Len0, Len) :-
    json_print_length(H, Options, Max, Len0, Len1),
    (   T == []
    ->  Len = Len1
    ;   Len2 is Len1+2,
        Len2 =< Max,
        array_print_length(T, Options, Max, Len2, Len)
    ).

string_len(String, Len0, Len) :-
    atom(String),
    !,
    atom_length(String, AL),
    Len is Len0 + AL + 2.
string_len(String, Len0, Len) :-
    string(String),
    !,
    string_length(String, AL),
    Len is Len0 + AL + 2.


                 /*******************************
                 *             TEST             *
                 *******************************/

%!  is_json_term(@Term) is semidet.
%!  is_json_term(@Term, +Options) is semidet.
%
%   True if Term is  a  json  term.   Options  are  the  same as for
%   json_read/2, defining the Prolog  representation   for  the JSON
%   =true=, =false= and =null= constants.

is_json_term(Term) :-
    default_json_options(Options),
    is_json_term2(Options, Term).

is_json_term(Term, Options) :-
    make_json_options(Options, OptionTerm, _RestOptions),
    is_json_term2(OptionTerm, Term).

is_json_term2(_, Var) :-
    var(Var), !, fail.
is_json_term2(Options, json(Pairs)) :-
    !,
    is_list(Pairs),
    maplist(is_json_pair(Options), Pairs).
is_json_term2(Options, List) :-
    is_list(List),
    !,
    maplist(is_json_term2(Options), List).
is_json_term2(_, Primitive) :-
    atomic(Primitive),
    !.           % atom, string or number
is_json_term2(Options, True) :-
    json_options_true(Options, True).
is_json_term2(Options, False) :-
    json_options_false(Options, False).
is_json_term2(Options, Null) :-
    json_options_null(Options, Null).

is_json_pair(_, Var) :-
    var(Var), !, fail.
is_json_pair(Options, Name=Value) :-
    atom(Name),
    is_json_term2(Options, Value).

                 /*******************************
                 *         DICT SUPPORT         *
                 *******************************/

%!  json_read_dict(+Stream, -Dict) is det.
%!  json_read_dict(+Stream, -Dict, +Options) is det.
%
%   Read  a  JSON  object,  returning  objects    as  a  dicts.  The
%   representation depends on the options, where the default is:
%
%     * String values are mapped to Prolog strings
%     * JSON =true=, =false= and =null= are represented using these
%       Prolog atoms.
%     * JSON objects are mapped to dicts.
%     * Optionally, a =type= field in an object assigns a tag for
%       the dict.
%
%   The predicate json_read_dict/3 processes  the   same  options as
%   json_read/3,  but  with  different  defaults.  In  addition,  it
%   processes the `tag` option. See   json_read/3  for details about
%   the shared options.
%
%     * tag(+Name)
%       When converting to/from a dict, map the indicated JSON
%       attribute to the dict _tag_. No mapping is performed if Name
%       is the empty atom ('', default). See json_read_dict/2 and
%       json_write_dict/2.
%     * default_tag(+Tag)
%       Provide the default tag if the above `tag` option does not
%       apply.
%     * null(+NullTerm)
%       Default the atom `null`.
%     * true(+TrueTerm)
%       Default the atom `true`.
%     * false(+FalseTerm)
%       Default the atom `false`
%     * end_of_file(+ErrorOrTerm)
%       Action on reading end-of-file. See json_read/3 for details.
%     * value_string_as(+Type)
%       Prolog type used for strings used as value.  Default
%       is `string`.  The alternative is `atom`, producing a
%       packed string object.

json_read_dict(Stream, Dict) :-
    json_read_dict(Stream, Dict, []).

json_read_dict(Stream, Dict, Options) :-
    make_json_dict_options(Options, OptionTerm, _RestOptions),
    (   json_value_top(Stream, Term, OptionTerm)
    ->  true
    ;   syntax_error(illegal_json, Stream)
    ),
    term_to_dict(Term, Dict, OptionTerm).

term_to_dict(json(Pairs), Dict, Options) :-
    !,
    (   json_options_tag(Options, TagName),
        Tag \== '',
        select(TagName = Tag0, Pairs, NVPairs),
        to_atom(Tag0, Tag)
    ->  json_dict_pairs(NVPairs, DictPairs, Options)
    ;   json_options_default_tag(Options, DefTag),
        (   var(DefTag)
        ->  true
        ;   Tag = DefTag
        ),
        json_dict_pairs(Pairs, DictPairs, Options)
    ),
    dict_create(Dict, Tag, DictPairs).
term_to_dict(Value0, Value, _Options) :-
    atomic(Value0), Value0 \== [],
    !,
    Value = Value0.
term_to_dict(List0, List, Options) :-
    is_list(List0),
    !,
    terms_to_dicts(List0, List, Options).
term_to_dict(Special, Special, Options) :-
    (   json_options_true(Options, Special)
    ;   json_options_false(Options, Special)
    ;   json_options_null(Options, Special)
    ;   json_options_end_of_file(Options, Special)
    ),
    !.

json_dict_pairs([], [], _).
json_dict_pairs([Name=Value0|T0], [Name=Value|T], Options) :-
    term_to_dict(Value0, Value, Options),
    json_dict_pairs(T0, T, Options).

terms_to_dicts([], [], _).
terms_to_dicts([Value0|T0], [Value|T], Options) :-
    term_to_dict(Value0, Value, Options),
    terms_to_dicts(T0, T, Options).

to_atom(Tag, Atom) :-
    string(Tag),
    !,
    atom_string(Atom, Tag).
to_atom(Atom, Atom) :-
    atom(Atom).

%!  json_write_dict(+Stream, +Dict) is det.
%!  json_write_dict(+Stream, +Dict, +Options) is det.
%
%   Write a JSON term, represented using dicts.  This is the same as
%   json_write/3, but assuming the default   representation  of JSON
%   objects as dicts.

json_write_dict(Stream, Dict) :-
    json_write_dict(Stream, Dict, []).

json_write_dict(Stream, Dict, Options) :-
    make_json_write_state(Options, State, Options1),
    make_json_dict_options(Options1, OptionTerm, _RestOptions),
    json_write_term(Dict, Stream, State, OptionTerm).


make_json_dict_options(Options, Record, RestOptions) :-
    default_json_dict_options(Record0),
    set_json_options_fields(Options, Record0, Record, RestOptions).

%!  atom_json_dict(+Atom, -JSONDict, +Options) is det.
%!  atom_json_dict(-Text, +JSONDict, +Options) is det.
%
%   Convert  between  textual  representation  and    a   JSON  term
%   represented as a dict. Options are as for json_read/3.
%   In _write_ mode, the addtional option
%
%       * as(Type)
%       defines the output type, which is one of =atom=,
%       =string= or =codes=.

atom_json_dict(Atom, Term, Options) :-
    ground(Atom),
    !,
    setup_call_cleanup(
        open_string(Atom, In),
        json_read_dict(In, Term, Options),
        close(In)).
atom_json_dict(Result, Term, Options) :-
    select_option(as(Type), Options, Options1, atom),
    (   type_term(Type, Result, Out)
    ->  true
    ;   must_be(oneof([atom,string,codes]), Type)
    ),
    with_output_to(Out,
                   json_write_dict(current_output, Term, Options1)).


                 /*******************************
                 *           MESSAGES           *
                 *******************************/

:- multifile
    prolog:error_message/3.

prolog:error_message(syntax_error(json(Id))) -->
    [ 'JSON syntax error: ' ],
    json_syntax_error(Id).

json_syntax_error(illegal_comment) -->
    [ 'Illegal comment' ].
json_syntax_error(illegal_string_escape) -->
    [ 'Illegal escape sequence in string' ].
