/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, VU University Amsterdam
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

:- module(yaml,
          [ yaml_read/2,                        % +Input, -DOM
            yaml_write/2,                       % +Output, +DOM
            yaml_write/3                        % +Output, +DOM, +Options
          ]).
:- autoload(library(apply),[maplist/3,exclude/3]).
:- autoload(library(base64),[base64/3]).
:- autoload(library(debug),[debug/3]).
:- autoload(library(error),[instantiation_error/1]).
:- autoload(library(option),[option/2,option/3]).
:- autoload(library(terms),[term_factorized/3]).

:- use_foreign_library(foreign(yaml4pl)).

/** <module> Process YAML data

This module parses  YAML  serialized  data   into  a  Prolog  term  with
structure that is compatible with the JSON   library.  This library is a
wrapper around the C library `libyaml`. This  library forms the basis of
the YAML support in several languages  and thus guarantees compatibility
of our YAML support with other languages.
*/

:- multifile
    tagged/3.                         % +Tag, ?String, ?Value

:- predicate_options(yaml_write/3, 3,
                     [ canonical(boolean),
                       unicode(boolean),
                       implicit(boolean),
                       factorize(boolean)
                     ]).

%!  yaml_read(+Input, -DOM) is det.
%
%   Parse Input to a YALM DOM. The DOM representation uses the following
%   mapping:
%
%     - A YAML sequence is mapped to a Prolog List.
%     - A YAML mapping is mapped to a Prolog dict.
%     - Untagged _scalars_ follow the implicit tag rules defined by
%       YAML, providing numbers (int, float and special floats),
%       `null` and the booleans `true` and `false`.  Other untagged
%       values are returned as a Prolog string.  Tagged values are
%       returned as tag(Tag, String) which is processed by
%       yalm_tagged/3.  This internal predicate calls the user hook
%       yaml:tagged/3 with the same arguments and, if the hook fails,
%       provides the following defaults:
%
%         - =|!!binary|= converts the Base64 to a string of bytes.
%         - =|!!str|= explicitly keeps a string
%         - =|!!null|= translates "null" to `null`
%         - =|!!bool|= translates to `true` and `false`
%         - =|!!int|= translates to an integer
%         - =|!!float|= translates to a float
%         - Anything else is returned as tag(Tag, String)
%
%   @arg Input is one of (1) a stream, (2) a term string(Data) or
%   (3) a file name.

yaml_read(In, DOM) :-
    setup_call_cleanup(
        yaml_open(In, Stream, Close),
        yaml_parse_stream(Stream, DOM0),
        Close),
    finalize_dom(DOM0, DOM).

yaml_open(Stream, Stream, Close) :-
    is_stream(Stream),
    !,
    stream_property(Stream, eof_action(EOF0)),
    (   EOF0 == eof_code
    ->  Close = true
    ;   set_stream(Stream, eof_action(eof_code)),
        Close = set_stream(Stream, eof_action(EOF0))
    ).
yaml_open(string(Data), Stream, close(Stream)) :-
    open_string(Data, Stream),
    set_stream(Stream, eof_action(eof_code)).
yaml_open(File, Stream, close(Stream)) :-
    open(File, read, Stream,
         [ eof_action(eof_code)
         ]).

finalize_dom(Var, _) :-
    var(Var),                                   % node in progress
    !.
finalize_dom(sequence(Elems0, Done, Elems), Elems) :-
    !,
    (   var(Done)
    ->  Done = true,
        maplist(finalize_dom, Elems0, Elems)
    ;   true
    ).
finalize_dom(mapping(Attrs0, Done, Dict), Dict) :-
    !,
    (   var(Done)
    ->  Done = true,
        maplist(mapping_pair, Attrs0, Pairs),
        dict_pairs(Dict, yaml, Pairs)
    ;   true
    ).
finalize_dom(tag(Tag, ValueIn), Value) :-
    !,
    (   string(ValueIn)
    ->  (   yalm_tagged(Tag, ValueIn, Value0)
        ->  Value = Value0
        ;   debug(yaml(tag), 'Ignored tag ~p for ~p', [Tag, ValueIn]),
            Value = tag(Tag, ValueIn)
        )
    ;   finalize_dom(ValueIn, ValueOut),
        Value = tag(Tag, ValueOut)
    ).
finalize_dom(Value, Value).

mapping_pair(Name=Value0, Name-Value) :-
    finalize_dom(Value0, Value).

yalm_tagged(Tag, String, Value) :-
    tagged(Tag, String, Value), !.
yalm_tagged('tag:yaml.org,2002:binary', Base64, Data) :-
    string_codes(Base64, EncCodes0),
    exclude(whitespace, EncCodes0, EncCodes),
    phrase(base64(PlainCodes), EncCodes),
    string_codes(Data, PlainCodes).
yalm_tagged('tag:yaml.org,2002:str', String, String).
yalm_tagged('tag:yaml.org,2002:null', "null", null).
yalm_tagged('tag:yaml.org,2002:bool', "true", true).
yalm_tagged('tag:yaml.org,2002:bool', "false", false).
yalm_tagged('tag:yaml.org,2002:int',  String, Int) :-
    number_string(Int, String).
yalm_tagged('tag:yaml.org,2002:float', String, Float) :-
    (   special_float(String, Float)
    ->  true
    ;   number_string(Float0, String),
        Float is float(Float0)
    ).

special_float(".nan", NaN) :- NaN is nan.
special_float(".NaN", NaN) :- NaN is nan.
special_float(".NAN", NaN) :- NaN is nan.
special_float(".inf", Inf) :- Inf is inf.
special_float(".Inf", Inf) :- Inf is inf.
special_float(".INF", Inf) :- Inf is inf.
special_float("-.inf", Inf) :- Inf is -inf.
special_float("-.Inf", Inf) :- Inf is -inf.
special_float("-.INF", Inf) :- Inf is -inf.

whitespace(0'\s).
whitespace(0'\t).
whitespace(0'\r).
whitespace(0'\n).

		 /*******************************
		 *             EMITTER		*
		 *******************************/

%!  yaml_write(+Out:stream, +DOM) is det.
%!  yaml_write(+Out:stream, +DOM, +Options) is det.
%
%   Emit a YAML DOM object as a   serialized YAML document to the stream
%   Out.  Options processed are:
%
%     - canonical(+Boolean)
%       Use canonical representation.  Default is `false`.
%     - unicode(+Boolean)
%       Use unicode Default is `true`.
%     - implicit(+Boolean)
%       Use implicit or explicit representation.  Currently only
%       affects the opening and closing the document.  Default is
%       `true`.  Use `false` for embedded documents.
%     - factorize(+Boolean)
%       If `true`, minimize the term by factoring out common
%       structures and use =|&anchor|= and =|*anchor|=.  Factorization
%       is always used if DOM is a cyclic term.

yaml_write(To, DOM) :-
    yaml_write(To, DOM, []).

yaml_write(To, DOM, Options) :-
    (   option(factorize(true), Options)
    ->  true
    ;   cyclic_term(DOM)
    ),
    !,
    term_factorized(DOM, Skeleton, Substitutions),
    assign_anchors(Substitutions, 1),
    yaml_write2(To, Skeleton, Options).
yaml_write(To, DOM, Options) :-
    yaml_write2(To, DOM, Options).

assign_anchors([], _).
assign_anchors([anchored(Anchor,_Done,Term)=Term|T], I) :-
    string_concat("a", I, Anchor),
    I2 is I + 1,
    assign_anchors(T, I2).

yaml_write2(To, DOM, Options) :-
    option(implicit(Implicit), Options, true),
    yaml_emitter_create(Emitter, To, Options),
    yaml_emit_event(Emitter, stream_start),
    yaml_emit_event(Emitter, document_start(Implicit)),
    yaml_emit(DOM, Emitter, Options),
    yaml_emit_event(Emitter, document_end(Implicit)),
    yaml_emit_event(Emitter, stream_end).

yaml_emit(Var, _, _) :-
    var(Var),
    !,
    instantiation_error(Var).
yaml_emit(anchored(Anchor, Done, Term), Emitter, Options) :-
    !,
    (   var(Done)
    ->  Done = true,
        yaml_emit(Term, Emitter, Anchor, Options)
    ;   yaml_emit_event(Emitter, alias(Anchor))
    ).
yaml_emit(Term, Emitter, Options) :-
    yaml_emit(Term, Emitter, _Anchor, Options).

yaml_emit(List, Emitter, Anchor, Options) :-
    is_list(List),
    !,
    yaml_emit_event(Emitter, sequence_start(Anchor, _Tag)),
    yaml_emit_list_elements(List, Emitter, Options),
    yaml_emit_event(Emitter, sequence_end).
yaml_emit(Dict, Emitter, Anchor, Options) :-
    is_dict(Dict, _),
    !,
    dict_pairs(Dict, _, Pairs),
    emit_mapping(Pairs, Emitter, Anchor, Options).
yaml_emit(json(Pairs), Emitter, Anchor, Options) :-
    !,
    emit_mapping(Pairs, Emitter, Anchor, Options).
yaml_emit(yaml(Pairs), Emitter, Anchor, Options) :-
    !,
    emit_mapping(Pairs, Emitter, Anchor, Options).
yaml_emit(Scalar, Emitter, Anchor, _Options) :-
    yaml_emit_event(Emitter, scalar(Scalar, _Tag, Anchor, plain)).

yaml_emit_list_elements([], _, _).
yaml_emit_list_elements([H|T], Emitter, Options) :-
    yaml_emit(H, Emitter, Options),
    yaml_emit_list_elements(T, Emitter, Options).

emit_mapping(Pairs, Emitter, Anchor, Options) :-
    yaml_emit_event(Emitter, mapping_start(Anchor, _Tag)),
    yaml_emit_mapping_elements(Pairs, Emitter, Options),
    yaml_emit_event(Emitter, mapping_end).

yaml_emit_mapping_elements([], _, _).
yaml_emit_mapping_elements([H|T], Emitter, Options) :-
    name_value(H, Name, Value),
    yaml_emit(Name, Emitter, Options),
    yaml_emit(Value, Emitter, Options),
    yaml_emit_mapping_elements(T, Emitter, Options).

name_value(Name-Value, Name, Value) :- !.
name_value(Name=Value, Name, Value) :- !.
name_value(NameValue, Name, Value) :-
    NameValue =.. [Name,Value].


		 /*******************************
		 *            HOOKS		*
		 *******************************/

%!  tagged(+Tag, ?String, ?Value) is semidet.
%
%   Hook that allows  convering  =|!!tag|=  values   to  be  decoded  or
%   encoded.
