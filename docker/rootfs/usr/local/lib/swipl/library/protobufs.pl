/*  Part of SWI-Prolog

    Author:        Jeffrey Rosenwald, extended by Peter Ludemann
    E-mail:        jeffrose@acm.org, peter.ludemann@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2013, Jeffrey Rosenwald
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

:- module(protobufs,
          [ protobuf_message/2,   % ?Template ?Codes
            protobuf_message/3,   % ?Template ?Codes ?Rest
            protobuf_parse_from_codes/3, % +WireCodes, +MessageType, -Term
            protobuf_serialize_to_codes/3,  % +Term, +MessageType, -WireCodes
            protobuf_field_is_map/2, % +MessageType, +FieldName
            protobuf_map_pairs/3 % ?ProtobufTermList, ?DictTag, ?Pairs

            % TODO: Restore the following to the public interface, if
            %       someone needs them.  For now, the tests directly specify
            %       them using, e.g. protobufs:uint32_codes(..., ...).
            %
            % protobuf_segment_message/2,  % ?Segments ?Codes
            % protobuf_segment_convert/2,  % +Form1 ?Form2
            % uint32_codes/2,
            % int32_codes/2,
            % float32_codes/2,
            % uint64_codes/2,
            % int64_codes/2,
            % float64_codes/2,
            % int64_zigzag/2,
            % uint32_int32/2,
            % uint64_int64/2,
            % uint32_codes_when/2,
            % int32_codes_when/2,  % TODO: unused
            % float32_codes_when/2,
            % uint64_codes_when/2,
            % int64_codes_when/2,  % TODO: unused
            % float64_codes_when/2,
            % int64_zigzag_when/2,
            % uint32_int32_when/2,
            % uint64_int64_when/2,
            % int64_float64_when/2,
            % int32_float32_when/2,
            % protobuf_var_int//1,
            % protobuf_tag_type//2
          ]).

:- use_module(library(apply_macros)).  % autoload(library(apply), [maplist/3, foldl/4]).
:- autoload(library(error), [must_be/2, domain_error/2, existence_error/2]).
:- autoload(library(lists), [append/3]).
:- autoload(library(utf8), [utf8_codes//1]).
:- autoload(library(dif), [dif/2]).
:- autoload(library(dcg/high_order), [sequence//2]).
:- autoload(library(when), [when/2]).
:- autoload(library(debug), [assertion/1]). % TODO: remove

:- set_prolog_flag(optimise, true). % For arithmetic using is/2.

/** <module> Google's Protocol Buffers ("protobufs")

Protocol  buffers  are  Google's    language-neutral,  platform-neutral,
extensible mechanism for serializing structured data  --  think XML, but
smaller, faster, and simpler. You define how   you  want your data to be
structured once. This takes the form of   a  template that describes the
data structure. You use this template  to   encode  and decode your data
structure into wire-streams that may be sent-to or read-from your peers.
The underlying wire stream is platform independent, lossless, and may be
used to interwork with a variety of  languages and systems regardless of
word size or endianness. Techniques  exist   to  safely extend your data
structure without breaking deployed programs   that are compiled against
the "old" format.

The idea behind Google's Protocol Buffers is that you define your
structured messages using a domain-specific language and tool
set. Further documentation on this is at
[https://developers.google.com/protocol-buffers](https://developers.google.com/protocol-buffers).

There are two ways you can use protobufs in Prolog:
  * with a compiled =|.proto|= file: protobuf_parse_from_codes/3 and
    protobuf_serialize_to_codes/3.
  * with a lower-level interface protobuf_message/2, which allows you
    to define your own domain-specific language for parsing and
    serializing protobufs.

The protobuf_parse_from_codes/3 and protobuf_serialize_to_codes/3
interface translates between a "wire stream" and a Prolog term. This
interface takes advantage of SWI-Prolog's
[dict](</pldoc/man?section=bidicts>).
There is a =protoc= plugin (=protoc-gen-swipl=) that generates a
Prolog file of meta-information that captures the =|.proto|= file's
definition in the =protobufs= module:
   * =|proto_meta_normalize(Unnormalized, Normalized)|=
   * =|proto_meta_package(Package, FileName, Options)|=
   * =|proto_meta_message_type(                    Fqn,     Package, Name)|=
   * =|proto_meta_message_type_map_entry(          Fqn)|=
   * =|proto_meta_field_name(                      Fqn,     FieldNumber, FieldName, FqnName)|=
   * =|proto_meta_field_json_name(                 FqnName, JsonName)|=
   * =|proto_meta_field_label(                     FqnName, LabelRepeatOptional) % 'LABEL_OPTIONAL', 'LABEL_REQUIRED', 'LABEL_REPEATED'|=
   * =|proto_meta_field_type(                      FqnName, Type) % 'TYPE_INT32', 'TYPE_MESSAGE', etc|=
   * =|proto_meta_field_type_name(                 FqnName, TypeName)|=
   * =|proto_meta_field_default_value(             FqnName, DefaultValue)|=
   * =|proto_meta_field_option_packed(             FqnName)|=
   * =|proto_meta_enum_type(                       FqnName, Fqn, Name)|=
   * =|proto_meta_enum_value(                      FqnName, Name, Number)|=
   * =|proto_meta_field_oneof_index(               FqnName, Index)|=
   * =|proto_meta_oneof(                           FqnName, Index, Name)|=

The protobuf_message/2 interface allows you to define your message
template as a list of predefined
Prolog terms that correspond to production  rules in the Definite Clause
Grammar (DCG) that realizes the interpreter. Each production rule has an
equivalent rule in the  protobuf  grammar.   The  process  is not unlike
specifiying the format of a regular  expression. To encode a template to
a wire-stream, you pass a grounded template, =X=, and  variable, =Y=, to
protobuf_message/2. To decode a wire-stream, =Y=, you pass an ungrounded
template, =X=,  along  with  a   grounded    wire-stream,   =Y=,  to
protobuf_message/2. The interpreter will unify  the unbound variables in
the template with values decoded from the wire-stream.

For an overview and tutorial with examples, see
[library(protobufs): Google's Protocol Buffers](#protobufs-main)
Examples of usage may also be found by inspecting
[[test_protobufs.pl][https://github.com/SWI-Prolog/contrib-protobufs/blob/master/test_protobufs.pl]]
and the
[[demo][https://github.com/SWI-Prolog/contrib-protobufs/tree/master/demo]]
directory, or by looking at the "addressbook" example that is typically
installed at
/usr/lib/swi-prolog/doc/packages/examples/protobufs/interop/addressbook.pl

@see https://developers.google.com/protocol-buffers
@see https://developers.google.com/protocol-buffers/docs/encoding
@author Jeffrey Rosenwald (JeffRose@acm.org)
@author Peter Ludemann (peter.ludemann@gmail.org)
@compat SWI-Prolog
*/

:- use_foreign_library(foreign(protobufs)).

%! protobuf_parse_from_codes(+WireCodes:list(int), +MessageType:atom, -Term) is semidet.
% Process bytes (list of int) that is the serialized form of a message (designated
% by =MessageType=), creating a Prolog term.
%
% =Protoc= must have been run (with the =|--swipl_out=|= option and the resulting
% top-level _pb.pl file loaded. For more details, see the "protoc" section of the
% overview documentation.
%
% Fails if the message can't be parsed or if the appropriate meta-data from =protoc=
% hasn't been loaded.
%
% All fields that are omitted from the =WireCodes= are set to their
% default values (typically the empty string or 0, depending on the
% type; or =|[]|= for repeated groups). There is no way of testing
% whether a value was specified in =WireCodes= or given its default
% value (that is, there is no equivalent of the Python
% implementation's =HasField`). Optional embedded messages and groups
% do not have any default value -- you must check their existence by
% using get_dict/3 or similar. If a field is part of a "oneof" set,
% then none of the other fields is set. You can determine which field
% had a value by using get_dict/3.
%
% @tbd document the generated terms (see library(http/json) and json_read_dict/3)
% @tbd add options such as =true= and =value_string_as= (similar to json_read_dict/3)
% @tbd add option for form of the [dict](</pldoc/man?section=bidicts>) tags (fully qualified or not)
% @tbd add option for outputting fields in the C++/Python/Java order
%       (by field number rather than by field name).
%
% @bug Ignores =|.proto|= [extensions](https://developers.google.com/protocol-buffers/docs/proto#extensions).
% @bug =map= fields don't get special treatment (but see protobuf_map_pairs/3).
% @bug Generates fields in a different order from the C++, Python,
%      Java implementations, which use the field number to determine
%      field order whereas currently this implementation uses field
%      name.  (This isn't stricly speaking a bug, because it's allowed
%      by the specification; but it might cause some surprise.)
%
% @param WireCodes Wire format of the message from e.g., read_stream_to_codes/2.
%          (The stream should have options `encoding(octet)` and `type(binary)`,
%          either as options to read_file_to_codes/3 or by calling set_stream/2
%          on the stream to read_stream_to_codes/2.)
% @param MessageType Fully qualified message name (from the =|.proto|= file's =package= and =message=).
%        For example, if the =package= is =google.protobuf= and the
%        message is =FileDescriptorSet=, then you would use
%        =|'.google.protobuf.FileDescriptorSet'|= or =|'google.protobuf.FileDescriptorSet'|=.
%        If there's no package name, use e.g.: =|'MyMessage|= or =|'.MyMessage'|=.
%        You can see the packages by looking at
%        =|protobufs:proto_meta_package(Pkg,File,_)|=
%        and the message names and fields by
%        =|protobufs:proto_meta_field_name('.google.protobuf.FileDescriptorSet',
%        FieldNumber, FieldName, FqnName)|= (the initial '.' is not optional for these facts,
%        only for the top-level name given to protobuf_serialize_to_codes/3).
% @param Term The generated term, as nested [dict](</pldoc/man?section=bidicts>)s.
% @see  [library(protobufs): Google's Protocol Buffers](#protobufs-serialize-to-codes)
% @error version_error(Module-Version) you need to recompile the =Module=
%        with a newer version of =|protoc|=.
protobuf_parse_from_codes(WireCodes, MessageType0, Term) :-
    verify_version,
    must_be(ground, MessageType0),
    (   proto_meta_normalize(MessageType0, MessageType)
    ->  true
    ;   existence_error(protobuf_package, MessageType0)
    ),
    protobuf_segment_message(Segments, WireCodes),
    % protobuf_segment_message/2 can leave choicepoints, backtracking
    % through all the possibilities would have combinatoric explosion;
    % instead use segment_to_term/3 call protobuf_segment_convert/2 to
    % change segments that were guessed incorrectly.
    !,
    maplist(segment_to_term(MessageType), Segments, MsgFields),
    !, % TODO: remove
    combine_fields(MsgFields, MessageType{}, Term),
    !. % TODO: remove? - but proto_meta might have left choicepoints if loaded twice

verify_version :-
    (   protoc_gen_swipl_version(Module, Version),
        Version @< [0,9,1] % This must be sync-ed with changes to protoc-gen-swipl
    ->  throw(error(version_error(Module-Version), _))
    ;   true
    ).

%! protobuf_serialize_to_codes(+Term:dict, -MessageType:atom, -WireCodes:list(int)) is det.
% Process a Prolog term into bytes (list of int) that is the serialized form of a
% message (designated by =MessageType=).
%
% =Protoc= must have been run (with the =|--swipl_out=|= option and the resulting
% top-level _pb.pl file loaded. For more details, see the "protoc" section of the
% overview documentation.
%
% Fails if the term isn't of an appropriate form or if the appropriate
% meta-data from =protoc= hasn't been loaded, or if a field name is incorrect
% (and therefore nothing in the meta-data matches it).
%
% @bug =map= fields don't get special treatment (but see protobuf_map_pairs/3).
% @bug =oneof= is not checked for validity.
%
% @param Term The Prolog form of the data, as nested [dict](</pldoc/man?section=bidicts>)s.
% @param MessageType Fully qualified message name (from the =|.proto|= file's =package= and =message=).
%        For example, if the =package= is =google.protobuf= and the
%        message is =FileDescriptorSet=, then you would use
%        =|'.google.protobuf.FileDescriptorSet'|= or =|'google.protobuf.FileDescriptorSet'|=.
%        If there's no package name, use e.g.: =|'MyMessage|= or =|'.MyMessage'|=.
%        You can see the packages by looking at
%        =|protobufs:proto_meta_package(Pkg,File,_)|=
%        and the message names and fields by
%        =|protobufs:proto_meta_field_name('.google.protobuf.FileDescriptorSet',
%        FieldNumber, FieldName, FqnName)|= (the initial '.' is not optional for these facts,
%        only for the top-level name given to protobuf_serialize_to_codes/3).
% @param WireCodes Wire format of the message, which can be output using
%        =|format('~s', [WireCodes])|=.
% @see [library(protobufs): Google's Protocol Buffers](#protobufs-serialize-to-codes)
% @error version_error(Module-Version) you need to recompile the =Module=
%        with a newer version of =|protoc|=.
% @error existence_error if a field can't be found in the meta-data
protobuf_serialize_to_codes(Term, MessageType0, WireCodes) :-
    verify_version,
    must_be(ground, MessageType0),
    (   proto_meta_normalize(MessageType0, MessageType)
    ->  true
    ;   existence_error(protobuf_package, MessageType0)
    ),
    term_to_segments(Term, MessageType, Segments),
    !, % TODO: remove
    protobuf_segment_message(Segments, WireCodes),
    !. % TODO: remove? - but proto_meta might have left choicepoints if loaded twice

%
% Map wire type (atom) to its encoding (an int)
%
wire_type(varint,            0). % for int32, int64, uint32, uint64, sint32, sint64, bool, enum
wire_type(fixed64,           1). % for fixed64, sfixed64, double
wire_type(length_delimited,  2). % for string, bytes, embedded messages, packed repeated fields
wire_type(start_group,       3). % for groups (deprecated)
wire_type(end_group,         4). % for groups (deprecated)
wire_type(fixed32,           5). % for fixed32, sfixed32, float

%
%  basic wire-type processing handled by C-support code in DCG-form
%

fixed_uint32(X, [A0, A1, A2, A3 | Rest], Rest) :-
    uint32_codes_when(X, [A0, A1, A2, A3]).
/* equivalent to:
fixed_uint32_(X) -->
  [ A0,A1,A2,A3 ],
  { uint32_codes_when(X, [A0,A1,A2,A3]) }.
*/

fixed_uint64(X, [A0, A1, A2, A3, A4, A5, A6, A7 | Rest], Rest) :-
    uint64_codes_when(X, [A0, A1, A2, A3, A4, A5, A6, A7]).

fixed_float64(X, [A0, A1, A2, A3, A4, A5, A6, A7 | Rest], Rest) :-
    float64_codes_when(X, [A0, A1, A2, A3, A4, A5, A6, A7]).

fixed_float32(X, [A0, A1, A2, A3 | Rest], Rest) :-
    float32_codes_when(X, [A0, A1, A2, A3]).

%
%   Start of the DCG
%

code_string(N, Codes, Rest, Rest1) :-
    length(Codes, N),
    append(Codes, Rest1, Rest),
    !.
/*
code_string(N, Codes) -->
        { length(Codes, N) },
        Codes, !.
*/

%
% deal with Google's method of packing unsigned integers in variable
% length, modulo 128 strings.
%
% protobuf_var_int//1 and protobuf_tag_type//2 productions were rewritten in straight
% Prolog for speed's sake.
%

%! protobuf_var_int(?A:int)// is det.
% Conversion between an int A and a list of codes, using the
% "varint" encoding.
% The behvior is undefined if =A= is negative.
% This is a low-level predicate; normally, you should use
% template_message/2 and the appropriate template term.
% e.g. phrase(protobuf_var_int(300), S) => S = [172,2]
%      phrase(protobuf_var_int(A), [172,2]) -> A = 300
protobuf_var_int(A, [A | Rest], Rest) :-
    A < 128,
    !.
protobuf_var_int(X, [A | Rest], Rest1) :-
    nonvar(X),
    X1 is X >> 7,
    A is 128 + (X /\ 0x7f),
    protobuf_var_int(X1, Rest, Rest1),
    !.
protobuf_var_int(X, [A | Rest], Rest1) :-
    protobuf_var_int(X1, Rest, Rest1),
    X is (X1 << 7) + A - 128,
    !.

%! protobuf_tag_type(?Tag:int, ?WireType:atom)// is det.
% Conversion between Tag (number) + WireType and wirestream codes.
% This is a low-level predicate; normally, you should use
% template_message/2 and the appropriate template term.
% @arg Tag The item's tag (field number)
% @arg WireType The item's wire type (see prolog_type//2 for how to
%               convert this to a Prolog type)
protobuf_tag_type(Tag, WireType, Rest, Rest1) :-
    nonvar(Tag), nonvar(WireType),
    wire_type(WireType, WireTypeEncoding),
    A is Tag << 3 \/ WireTypeEncoding,
    protobuf_var_int(A, Rest, Rest1),
    !.
protobuf_tag_type(Tag, WireType, Rest, Rest1) :-
    protobuf_var_int(A, Rest, Rest1),
    WireTypeEncoding is A /\ 0x07,
    wire_type(WireType, WireTypeEncoding),
    Tag is A >> 3.

%! prolog_type(?Tag:int, ?PrologType:atom)// is semidet.
% Match Tag (field number) + PrologType.
% When Type is a variable, backtracks through all the possibilities
% for a given wire encoding.
% Note that 'repeated' isn't here because it's handled by single_message//3.
% See also segment_type_tag/3.
prolog_type(Tag, double) -->     protobuf_tag_type(Tag, fixed64).
prolog_type(Tag, integer64) -->  protobuf_tag_type(Tag, fixed64).
prolog_type(Tag, unsigned64) --> protobuf_tag_type(Tag, fixed64).
prolog_type(Tag, float) -->      protobuf_tag_type(Tag, fixed32).
prolog_type(Tag, integer32) -->  protobuf_tag_type(Tag, fixed32).
prolog_type(Tag, unsigned32) --> protobuf_tag_type(Tag, fixed32).
prolog_type(Tag, integer) -->    protobuf_tag_type(Tag, varint).
prolog_type(Tag, unsigned) -->   protobuf_tag_type(Tag, varint).
prolog_type(Tag, signed32) -->   protobuf_tag_type(Tag, varint).
prolog_type(Tag, signed64) -->   protobuf_tag_type(Tag, varint).
prolog_type(Tag, boolean) -->    protobuf_tag_type(Tag, varint).
prolog_type(Tag, enum) -->       protobuf_tag_type(Tag, varint).
prolog_type(Tag, atom) -->       protobuf_tag_type(Tag, length_delimited).
prolog_type(Tag, codes) -->      protobuf_tag_type(Tag, length_delimited).
prolog_type(Tag, utf8_codes) --> protobuf_tag_type(Tag, length_delimited).
prolog_type(Tag, string) -->     protobuf_tag_type(Tag, length_delimited).
prolog_type(Tag, embedded) -->   protobuf_tag_type(Tag, length_delimited).
prolog_type(Tag, packed) -->     protobuf_tag_type(Tag, length_delimited).

%
%   The protobuf-2.1.0 grammar allows negative values in enums.
%   But they are encoded as unsigned in the  golden message.
%   As such, they use the maximum length of a varint, so it is
%   recommended that they be non-negative. However, that's controlled
%   by the =|.proto|= file.
%
:- meta_predicate enumeration(1,?,?).

enumeration(Type) -->
    { call(Type, Value) },
    payload(signed64, Value).

%! payload(?PrologType, ?Payload) is det.
% Process the codes into =Payload=, according to =PrologType=
% TODO: payload//2 "mode" is sometimes module-sensitive, sometimes not.
%       payload(enum, A)// has A as a callable
%       all other uses of payload//2, the 2nd arg is not callable.
%     - This confuses check/0; it also makes defining an enumeration
%       more difficult because it has to be defined in module protobufs
%       (see vector_demo.pl, which defines protobufs:commands/2)
payload(enum, Payload) -->
    enumeration(Payload).
payload(double, Payload) -->
    fixed_float64(Payload).
payload(integer64, Payload) -->
    { uint64_int64_when(Payload0, Payload) },
    fixed_uint64(Payload0).
payload(unsigned64, Payload) -->
    fixed_uint64(Payload).
payload(float, Payload) -->
    fixed_float32(Payload).
payload(integer32, Payload) -->
    { uint32_int32_when(Payload0, Payload) },
    fixed_uint32(Payload0).
payload(unsigned32, Payload) -->
    fixed_uint32(Payload).
payload(integer, Payload) -->
    { nonvar(Payload), int64_zigzag(Payload, X) }, % TODO: int64_zigzag_when/2
    !,
    protobuf_var_int(X).
payload(integer, Payload) -->
    protobuf_var_int(X),
    { int64_zigzag(Payload, X) }. % TODO: int64_zigzag_when/2
payload(unsigned, Payload) -->
    protobuf_var_int(Payload),
    { Payload >= 0 }.
payload(signed32, Payload) --> % signed32 is not defined by prolog_type//2
                               % for wire-stream compatibility reasons.
    % signed32 ought to write 5 bytes for negative numbers, but both
    % the C++ and Python implementations write 10 bytes. For
    % wire-stream compatibility, we follow C++ and Python, even though
    % protoc decode appears to work just fine with 5 bytes --
    % presumably there are some issues with decoding 5 bytes and
    % getting the sign extension correct with some 32/64-bit integer
    % models.  See CodedOutputStream::WriteVarint32SignExtended(int32
    % value) in google/protobuf/io/coded_stream.h.
    payload(signed64, Payload).
payload(signed64, Payload) -->
    % protobuf_var_int//1 cannot handle negative numbers (note that
    % zig-zag encoding always results in a positive number), so
    % compute the 64-bit 2s complement, which is what is produced
    % form C++ and Python.
    { nonvar(Payload) },
    !,
    { uint64_int64(X, Payload) }, % TODO: uint64_int64_when
    protobuf_var_int(X).
payload(signed64, Payload) -->
    % See comment in previous clause about negative numbers.
    protobuf_var_int(X),
    { uint64_int64(X, Payload) }. % TODO: uint64_int64_when
payload(codes, Payload) -->
    { nonvar(Payload),
      !,
      length(Payload, Len)
    },
    protobuf_var_int(Len),
    code_string(Len, Payload).
payload(codes, Payload) -->
    protobuf_var_int(Len),
    code_string(Len, Payload).
payload(utf8_codes, Payload) -->
    { nonvar(Payload), % TODO: use freeze/2 or when/2
      !,
      phrase(utf8_codes(Payload), B)
    },
    payload(codes, B).
payload(utf8_codes, Payload) -->
    payload(codes, B),
    { phrase(utf8_codes(Payload), B) }.
payload(atom, Payload) -->
    { nonvar(Payload),
      atom_codes(Payload, Codes)
    },
    payload(utf8_codes, Codes),
    !.
payload(atom, Payload) -->
    payload(utf8_codes, Codes),
    { atom_codes(Payload, Codes) }.
payload(boolean, true) -->
    payload(unsigned, 1).
payload(boolean, false) -->
    payload(unsigned, 0).
payload(string, Payload) -->
    {   nonvar(Payload)
    ->  string_codes(Payload, Codes)
    ;   true
    },
    % string_codes produces a list of unicode, not bytes
    payload(utf8_codes, Codes),
    { string_codes(Payload, Codes) }.
payload(embedded, protobuf(PayloadSeq)) -->
    { ground(PayloadSeq),
      phrase(protobuf(PayloadSeq), Codes)
    },
    payload(codes, Codes),
    !.
payload(embedded, protobuf(PayloadSeq)) -->
    payload(codes, Codes),
    { phrase(protobuf(PayloadSeq), Codes) }.
payload(packed, TypedPayloadSeq) -->
    { TypedPayloadSeq =.. [PrologType, PayloadSeq],  % TypedPayloadSeq = PrologType(PayloadSeq)
      ground(PayloadSeq),
      phrase(packed_payload(PrologType, PayloadSeq), Codes)
    },
    payload(codes, Codes),
    !.
payload(packed, enum(EnumSeq)) -->
    !,
    % TODO: combine with next clause
    % TODO: replace =.. with a predicate that gives all the possibilities - see detag/6.
    { EnumSeq =.. [ Enum, Values ] }, % EnumSeq = Enum(Values)
    payload(codes, Codes),
    { phrase(packed_enum(Enum, Values), Codes) }.
payload(packed, TypedPayloadSeq) -->
    payload(codes, Codes),
    % TODO: replace =.. with a predicate that gives all the possibilities - see detag/6.
    { TypedPayloadSeq =.. [PrologType, PayloadSeq] },  % TypedPayloadSeq = PrologType(PayloadSeq)
    { phrase(packed_payload(PrologType, PayloadSeq), Codes) }.

packed_payload(enum, EnumSeq) -->
    { ground(EnumSeq) }, !,
    { EnumSeq =.. [EnumType, Values] }, % EnumSeq = EnumType(Values)
    packed_enum(EnumType, Values).
packed_payload(PrologType, PayloadSeq) -->
    sequence_payload(PrologType, PayloadSeq).

% sequence_payload//2 (because sequence//2 isn't compile-time expanded)
sequence_payload(PrologType, PayloadSeq) -->
    sequence_payload_(PayloadSeq, PrologType).

sequence_payload_([], _PrologType) --> [ ].
sequence_payload_([Payload|PayloadSeq], PrologType) -->
        payload(PrologType, Payload),
        sequence_payload_(PayloadSeq, PrologType).

packed_enum(Enum, [ A | As ]) -->
    % TODO: replace =.. with a predicate that gives all the possibilities - see detag/6.
    { E =.. [Enum, A] },
    payload(enum, E),
    packed_enum(Enum, As).
packed_enum(_, []) --> [ ].

start_group(Tag) --> protobuf_tag_type(Tag, start_group).

end_group(Tag) -->   protobuf_tag_type(Tag, end_group).
%
%
nothing([]) --> [], !.

protobuf([Field | Fields]) -->
    % TODO: don't use =.. -- move logic to single_message
    (   { Field = repeated_embedded(Tag, protobuf(EmbeddedFields), Items) }
    ->  repeated_embedded_messages(Tag, EmbeddedFields, Items)
    ;   { Field =.. [ PrologType, Tag, Payload] },  % Field = PrologType(Tag, Payload)
        single_message(PrologType, Tag, Payload),
        (   protobuf(Fields)
        ;   nothing(Fields)
        )
    ),
    !.

repeated_message(repeated_enum, Tag, Type, [A | B]) -->
    % TODO: replace =.. with a predicate that gives all the possibilities - see detag/6.
    { TypedPayload =.. [Type, A] },  % TypedPayload = Type(A)
    single_message(enum, Tag, TypedPayload),
    (   repeated_message(repeated_enum, Tag, Type, B)
    ;   nothing(B)
    ).
repeated_message(Type, Tag, [A | B]) -->
    { Type \= repeated_enum },
    single_message(Type, Tag, A),
    repeated_message(Type, Tag, B).
repeated_message(_Type, _Tag, A) -->
    nothing(A).

repeated_embedded_messages(Tag, EmbeddedFields, [protobuf(A) | B]) -->
    { copy_term(EmbeddedFields, A) },
    single_message(embedded, Tag, protobuf(A)), !,
    repeated_embedded_messages(Tag, EmbeddedFields, B).
repeated_embedded_messages(_Tag, _EmbeddedFields, []) -->
    [ ].

%! single_message(+PrologType:atom, ?Tag, ?Payload)// is det.
% Processes a single messages (e.g., one item in the list in protobuf([...]).
% The PrologType, Tag, Payload are from Field =.. [PrologType, Tag, Payload]
% in the caller
single_message(repeated, Tag, enum(EnumSeq)) -->
    !,
    { EnumSeq =.. [EnumType, Values] },  % EnumSeq = EnumType(Values)
    repeated_message(repeated_enum, Tag, EnumType, Values).
single_message(repeated, Tag, Payload) -->
    !,
    % TODO: replace =.. with a predicate that gives all the possibilities - see detag/6.
    { Payload =.. [PrologType, A] },  % Payload = PrologType(A)
    { PrologType \= enum },
    repeated_message(PrologType, Tag, A).
single_message(group, Tag, A) -->
    !,
    start_group(Tag),
    protobuf(A),
    end_group(Tag).
single_message(PrologType, Tag, Payload) -->
    { PrologType \= repeated, PrologType \= group },
    prolog_type(Tag, PrologType),
    payload(PrologType, Payload).

%!  protobuf_message(?Template, ?WireStream) is semidet.
%!  protobuf_message(?Template, ?WireStream, ?Rest) is nondet.
%
%   Marshals  and  unmarshals   byte  streams  encoded  using   Google's
%   Protobuf  grammars.  protobuf_message/2  provides  a  bi-directional
%   parser that marshals a Prolog   structure to WireStream,  according
%   to rules specified by Template. It   can also unmarshal  WireStream
%   into  a  Prolog   structure   according    to   the   same  grammar.
%   protobuf_message/3 provides a difference list version.
%
%   @bug The protobuf specification states that the wire-stream can have
%   the fields in any order and that unknown fields are to be ignored.
%   This implementation assumes that the fields are in the exact order
%   of the definition and match exactly. If you use
%   protobuf_parse_from_codes/3, you can avoid this problem.o
%
%   @param Template is a  protobuf   grammar  specification.  On decode,
%   unbound variables in the Template are  unified with their respective
%   values in the WireStream. On encode, Template must be ground.
%
%   @param WireStream is a code list that   was generated by a protobuf
%   encoder using an equivalent template.

protobuf_message(protobuf(TemplateList), WireStream) :-
    must_be(list, TemplateList),
    phrase(protobuf(TemplateList), WireStream),
    !.

protobuf_message(protobuf(TemplateList), WireStream, Residue) :-
    must_be(list, TemplateList),
    phrase(protobuf(TemplateList), WireStream, Residue).

%! protobuf_segment_message(+Segments:list, -WireStream:list(int)) is det.
%! protobuf_segment_message(-Segments:list, +WireStream:list(int)) is det.
%
%  Low level marshalling and unmarshalling of byte streams. The
%  processing is independent of the =|.proto|= description, similar to
%  the processing done by =|protoc --decode_raw|=. This means that
%  field names aren't shown, only field numbers.
%
%  For unmarshalling, a simple heuristic is used on length-delimited
%  segments: first interpret it as a message; if that fails, try to
%  interpret as a UTF8 string; otherwise, leave it as a "blob" (if the
%  heuristic was wrong, you can convert to a string or a blob by using
%  protobuf_segment_convert/2).  32-bit and 64-bit numbers are left as
%  codes because they could be either integers or floating point (use
%  int32_codes_when/2, float32_codes_when/2, int64_codes_when/2,
%  uint32_codes_when/2, uint64_codes_when/2, float64_codes_when/2 as
%  appropriate); variable-length numbers ("varint" in the [[Protocol
%  Buffers encoding
%  documentation][https://developers.google.com/protocol-buffers/docs/encoding#varints]]),
%  might require "zigzag" conversion, int64_zigzag_when/2.
%
%  For marshalling, use the predicates int32_codes_when/2,
%  float32_codes_when/2, int64_codes_when/2, uint32_codes_when/2,
%  uint64_codes_when/2, float64_codes_when/2, int64_zigzag_when/2 to
%  put integer and floating point values into the appropriate form.
%
%  @bug This predicate is preliminary and may change as additional
%       functionality is added.
%
%  @param Segments a list containing terms of the following form (=Tag= is
%  the field number; =Codes= is a list of integers):
%    * varint(Tag,Varint) - =Varint= may need int64_zigzag_when/2
%    * fixed64(Tag,Int) - =Int= signed, derived from the 8 codes
%    * fixed32(Tag,Codes) - =Int= is signed, derived from the 4 codes
%    * message(Tag,Segments)
%    * group(Tag,Segments)
%    * string(Tag,String) - =String= is a SWI-Prolog string
%    * packed(Tag,Type(Scalars)) - =Type= is one of
%             =varint=, =fixed64=, =fixed32=; =Scalars=
%             is a list of =Varint= or =Codes=, which should
%             be interpreted as described under those items.
%             Note that the protobuf specification does not
%             allow packed repeated string.
%    * length_delimited(Tag,Codes)
%    * repeated(List) - =List= of segments
%  Of these, =group= is deprecated in the protobuf documentation and
%  shouldn't appear in modern code, having been superseded by nested
%  message types.
%
%  For deciding how to interpret a length-delimited item (when
%  =Segments= is a variable), an attempt is made to parse the item in
%  the following order (although code should not rely on this order):
%    * message
%    * string (it must be in the form of a UTF string)
%    * packed (which can backtrack through the various =Type=s)
%    * length_delimited - which always is possible.
%
%  The interpretation of length-delimited items can sometimes guess
%  wrong; the interpretation can be undone by either backtracking or
%  by using protobuf_segment_convert/2 to convert the incorrect
%  segment to a string or a list of codes. Backtracking through all
%  the possibilities is not recommended, because of combinatoric
%  explosion (there is an example in the unit tests); instead, it is
%  suggested that you take the first result and iterate through its
%  items, calling protobuf_segment_convert/2 as needed to reinterpret
%  incorrectly guessed segments.
%
%  @param WireStream a code list that was generated by a protobuf
%  endoder.
%
%  @see https://developers.google.com/protocol-buffers/docs/encoding
protobuf_segment_message(Segments, WireStream) :-
    phrase(segment_message(Segments), WireStream).

segment_message(Segments) -->
    sequence_segment(Segments).

% sequence_segment//1 (because sequence//2 isn't compile-time expanded)
sequence_segment([]) --> [ ].
sequence_segment([Segment|Segments]) -->
    segment(Segment),
    sequence_segment(Segments).

segment(Segment) -->
    { nonvar(Segment) },
    !,
    % repeated(List) can be created by field_segment_scalar_or_repeated/7
    (   { Segment = repeated(Segments) }
    ->  sequence_segment(Segments)
    ;   { segment_type_tag(Segment, Type, Tag) },
        protobuf_tag_type(Tag, Type),
        segment(Type, Tag, Segment)
    ).
segment(Segment) -->
    % { var(Segment) },
    protobuf_tag_type(Tag, Type),
    segment(Type, Tag, Segment).

segment(varint, Tag, varint(Tag,Value)) -->
    protobuf_var_int(Value).
segment(fixed64, Tag, fixed64(Tag, Int64)) -->
    payload(integer64, Int64).
segment(fixed32, Tag, fixed32(Tag, Int32)) -->
    payload(integer32, Int32).
segment(start_group, Tag, group(Tag, Segments)) -->
    segment_message(Segments),
    protobuf_tag_type(Tag, end_group).
segment(length_delimited, Tag, Result) -->
    segment_length_delimited(Tag, Result).

segment_length_delimited(Tag, Result) -->
    { nonvar(Result) },
    !,
    { length_delimited_segment(Result, Tag, Codes) },
    { length(Codes, CodesLen) },
    protobuf_var_int(CodesLen),
    code_string(CodesLen, Codes).
segment_length_delimited(Tag, Result) -->
    % { var(Result) },
    protobuf_var_int(CodesLen),
    code_string(CodesLen, Codes),
    { length_delimited_segment(Result, Tag, Codes) }.

length_delimited_segment(message(Tag,Segments), Tag, Codes) :-
    protobuf_segment_message(Segments, Codes).
length_delimited_segment(group(Tag,Segments), Tag, Codes) :-
    phrase(segment_group(Tag, Segments), Codes).
length_delimited_segment(string(Tag,String), Tag, Codes) :-
    (   nonvar(String)
    ->  string_codes(String, StringCodes),
        phrase(utf8_codes(StringCodes), Codes)
    ;   phrase(utf8_codes(StringCodes), Codes),
        string_codes(String, StringCodes)
    ).
length_delimited_segment(packed(Tag,Payload), Tag, Codes) :-
    % We don't know the type of the fields, so we try the 3
    % possibilities.  This has a problem: an even number of fixed32
    % items can't be distinguished from half the number of fixed64
    % items; but it's all we can do. The good news is that usually
    % varint (possibly with zig-zag encoding) is more common because
    % it's more compact (I don't know whether 32-bit or 64-bit is more
    % common for floating point).
    packed_option(Type, Items, Payload),
    phrase(sequence_payload(Type, Items), Codes).
length_delimited_segment(length_delimited(Tag,Codes), Tag, Codes).

segment_group(Tag, Segments) -->
    start_group(Tag),
    segment_message(Segments),
    end_group(Tag).

% See also prolog_type//2. Note that this doesn't handle repeated(List),
% which is used internally (see field_segment_scalar_or_repeated/7).
segment_type_tag(varint(Tag,_Value),           varint,           Tag).
segment_type_tag(fixed64(Tag,_Value),          fixed64,          Tag).
segment_type_tag(group(Tag,_Segments),         start_group,      Tag).
segment_type_tag(fixed32(Tag,_Value),          fixed32,          Tag).
segment_type_tag(length_delimited(Tag,_Codes), length_delimited, Tag).
segment_type_tag(message(Tag,_Segments),       length_delimited, Tag).
segment_type_tag(packed(Tag,_Payload),         length_delimited, Tag).
segment_type_tag(string(Tag,_String),          length_delimited, Tag).

%! detag(+Compound, -Name, -Tag, -Value, List, -CompoundWithList) is semidet.
% Deconstruct =Compound= or the form =|Name(Tag,Value)|= and create a
% new =CompoundWithList= that replaces =Value= with =List=. This is
% used by packed_list/2 to transform =|[varint(1,0),varint(1,1)]|= to
% =|varint(1,[0,1])|=.
%
% Some of =Compound= items are impossible for =packed= with the
% current protobuf spec, but they don't do any harm.
detag(varint(Tag,Value),           varint,            Tag, Value,     List, varint(List)).
detag(fixed64(Tag,Value),          fixed64,           Tag, Value,     List, fixed64(List)).
detag(fixed32(Tag,Value),          fixed32,           Tag, Value,     List, fixed32(List)).
detag(length_delimited(Tag,Codes), length_delimited,  Tag, Codes,     List, length_delimited(List)).
detag(message(Tag,Segments),       message,           Tag, Segments,  List, message(List)).
detag(packed(Tag,Payload),         packed,            Tag, Payload,   List, packed(List)). % TODO: delete?
detag(string(Tag,String),          string,            Tag, String,    List, string(List)).

% See also prolog_type//2, but pick only one for each wirestream type
% For varint(Items), use one that doesn't do zigzag
packed_option(integer64, Items, fixed64(Items)).
packed_option(integer32, Items, fixed32(Items)).
packed_option(unsigned,  Items, varint(Items)).
% packed_option(integer,   Items, varint(Items)).
% packed_option(double,    Items, fixed64(Items)).
% packed_option(float,     Items, fixed32(Items)).
% packed_option(signed64,  Items, varint(Items)).
% packed_option(boolean,   Items, varint(Items)).
% packed_option(enum,      Items, varint(Items)).

%! protobuf_segment_convert(+Form1, ?Form2) is multi.
% A convenience predicate for dealing with the situation where
% protobuf_segment_message/2 interprets a segment of the wire stream
% as a form that you don't want (e.g., as a message but it should have
% been a UTF8 string).
%
% =Form1= is converted back to the original wire stream, then the
% predicate non-deterimisticly attempts to convert the wire stream to
% a =|string|= or =|length_delimited|= term (or both: the lattter
% always succeeds).
%
% The possible conversions are:
%   message(Tag,Segments) => string(Tag,String)
%   message(Tag,Segments) => length_delimited(Tag,Codes)
%   string(Tag,String) => length_delimited(Tag,Codes)
%   length_delimited(Tag,Codes) => length_delimited(Tag,Codes)
%
% Note that for fixed32, fixed64, only the signed integer forms are
% given; if you want the floating point forms, then you need to do use
% int64_float64_when/2 and int32_float32_when/2.
%
% For example:
% ~~~{.pl}
% ?- protobuf_segment_convert(
%        message(10,[fixed64(13,7309475598860382318)]),
%        string(10,"inputType")).
% ?- protobuf_segment_convert(
%        message(10,[fixed64(13,7309475598860382318)]),
%        length_delimited(10,[105,110,112,117,116,84,121,112,101])).
% ?- protobuf_segment_convert(
%        string(10, "inputType"),
%        length_delimited(10,[105,110,112,117,116,84,121,112,101])).
% ?- forall(protobuf_segment_convert(string(1999,"\x1\\x0\\x0\\x0\\x2\\x0\\x0\\x0\"),Z), writeln(Z)).
%       string(1999,      )
%       packed(1999,fixed64([8589934593]))
%       packed(1999,fixed32([1,2]))
%       packed(1999,varint([1,0,0,0,2,0,0,0]))
%       length_delimited(1999,[1,0,0,0,2,0,0,0])
% ~~~
% These come from:
% ~~~{.pl}
% Codes = [82,9,105,110,112,117,116,84,121,112,101],
% protobuf_message(protobuf([embedded(T1, protobuf([integer64(T2, I)]))]), Codes),
% protobuf_message(protobuf([string(T,S)]), Codes).
%    T = 10, T1 = 10, T2 = 13,
%    I = 7309475598860382318,
%    S = "inputType".
% ~~~
%
%  @bug This predicate is preliminary and may change as additional
%       functionality is added.
%  @bug This predicate will sometimes generate unexpected choice points,
%       Such as from =|protobuf_segment_convert(message(10,...), string(10,...))|=
%
% @param Form1 =|message(Tag,Pieces)|=, =|string(Tag,String)|=, =|length_delimited(Tag,Codes)|=,
%        =|varint(Tag,Value)|=, =|fixed64(Tag,Value)|=, =|fixed32(Tag,Value)|=.
% @param Form2 similar to =Form1=.
protobuf_segment_convert(Form, Form). % for efficiency, don't generate codes
protobuf_segment_convert(Form1, Form2) :-
    dif(Form1, Form2),          % Form1=Form2 handled by first clause
    protobuf_segment_message([Form1], WireCodes),
    phrase(tag_and_codes(Tag, Codes), WireCodes),
    length_delimited_segment(Form2, Tag, Codes).

tag_and_codes(Tag, Codes) -->
    protobuf_tag_type(Tag, length_delimited),
    payload(codes, Codes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Documention of the foreign predicates, which are wrapped and exported.

%! uint32_codes_when(?Uint32, ?Codes) is det.
% Convert between a 32-bit unsigned integer value and its wirestream codes.
% This is a low-level predicate; normally, you should use
% template_message/2 and the appropriate template term.
%
% This predicate delays until either =Uint32= or =Codes= is
% sufficiently instantiated.
%
% There is also a non-delayed protobufs:uint32_codes/2
%
% SWI-Prolog doesn't have a 32-bit integer type, so 32-bit integer
% is simulated by doing a range check.
%
% @param Uint32 an unsigned integer that's in the 32-bit range
% @param Codes a list of 4 integers (codes)
%
% @error Type,Domain if =Value= or =Codes= are of the wrong
%                    type or out of range.
uint32_codes_when(Uint32, Codes) :-
    when((nonvar(Uint32) ; ground(Codes)), uint32_codes(Uint32, Codes)).

%! int32_codes_when(?Int32, ?Codes) is det.
% Convert between a 32-bit signed integer value and its wirestream codes.
% This is a low-level predicate; normally, you should use
% template_message/2 and the appropriate template term.
%
% This predicate delays until either =Int32= or =Codes= is
% sufficiently instantiated.
%
% There is also a non-delayed protobufs:int32_codes/2
%
% SWI-Prolog doesn't have a 32-bit integer type, so 32-bit integer
% is simulated by doing a range check.
%
% @param Int32 an unsigned integer that's in the 32-bit range
% @param Codes a list of 4 integers (codes)
%
% @error Type,Domain if =Value= or =Codes= are of the wrong
%                    type or out of range.
int32_codes_when(Int32, Codes) :- % TODO: unused
    when((nonvar(Int32) ; ground(Codes)), int32_codes(Int32, Codes)).

%! float32_codes_when(?Value, ?Codes) is det.
% Convert between a 32-bit floating point value and its wirestream codes.
% This is a low-level predicate; normally, you should use
% template_message/2 and the appropriate template term.
%
% This predicate delays until either =Value= or =Codes= is
% sufficiently instantiated.
%
% There is also a non-delayed protobufs:float32_codes/2
%
% @param Value a floating point number
% @param Codes a list of 4 integers (codes)
float32_codes_when(Value, Codes) :-
    when((nonvar(Value) ; ground(Codes)), float32_codes(Value, Codes)).

%! uint64_codes_when(?Uint64, ?Codes) is det.
% Convert between a 64-bit unsigned integer value and its wirestream codes.
% This is a low-level predicate; normally, you should use
% template_message/2 and the appropriate template term.
%
% SWI-Prolog allows integer values greater than 64 bits, so
% a range check is done.
%
% This predicate delays until either =Uint64= or =Codes= is
% sufficiently instantiated.
%
% There is also a non-delayed protobufs:uint64_codes/2

%
% @param Uint64 an unsigned integer
% @param Codes a list of 8 integers (codes)
%
% @error Type,Domain if =Uint64= or =Codes= are of the wrong
%                    type or out of range.
uint64_codes_when(Uint64, Codes) :-
    when((nonvar(Uint64) ; ground(Codes)), uint64_codes(Uint64, Codes)).

%! int64_codes_when(?Int64, ?Codes) is det.
% Convert between a 64-bit signed integer value and its wirestream codes.
% This is a low-level predicate; normally, you should use
% template_message/2 and the appropriate template term.
%
% SWI-Prolog allows integer values greater than 64 bits, so
% a range check is done.
%
% This predicate delays until either =Int64= or =Codes= is
% sufficiently instantiated.
%
% There is also a non-delayed protobufs:int64_codes/2

%
% @param Int64 an unsigned integer
% @param Codes a list of 8 integers (codes)
%
% @error Type,Domain if =Int64= or =Codes= are of the wrong
%                    type or out of range.
int64_codes_when(Int64, Codes) :-  % TODO: unused
    when((nonvar(Int64) ; ground(Codes)), int64_codes(Int64, Codes)).

%! float64_codes_when(?Value, ?Codes) is det.
% Convert between a 64-bit floating point value and its wirestream codes.
% This is a low-level predicate; normally, you should use
% template_message/2 and the appropriate template term.
%
% This predicate delays until either =Value= or =Codes= is
% sufficiently instantiated.
%
% There is also a non-delayed protobufs:float64_codes/2
%
% @param Value a floating point number
% @param Codes a list of 8 integers (codes)
%
% @error instantiation error if both =Value= and =Codes= are uninstantiated.
%
% @bug May give misleading exception under some circumstances
%      (e.g., float64_codes(_, [_,_,_,_,_,_,_,_]).
float64_codes_when(Value, Codes) :-
    when((nonvar(Value) ; ground(Codes)), float64_codes(Value, Codes)).

%! int64_zigzag_when(?Original, ?Encoded) is det.
% Convert between a signed integer value and its zigzag encoding,
% used for the protobuf =sint32= and =sint64= types. This is a
% low-level predicate; normally, you should use template_message/2 and
% the appropriate template term.
%
% SWI-Prolog allows integer values greater than 64 bits, so
% a range check is done.
%
% This predicate delays until either =Original= or =Encoded= is
% sufficiently instantiated.
%
% There is also a non-delayed protobufs:int64_zigzag/2
%
% @see https://developers.google.com/protocol-buffers/docs/encoding#types
%
% @param Original an integer in the original form
% @param Encoded the zigzag encoding of =Original=
%
% @error Type,Domain if =Original= or =Encoded= are of the wrong
%                    type or out of range.
%
% @error instantiation error if both =Original= and =Encoded= are uninstantiated.
int64_zigzag_when(Original, Encoded) :-
    when((nonvar(Original) ; nonvar(Encoded)), int64_zigzag(Original, Encoded)).

%! uint64_int64_when(?Uint64:integer, ?Int64:integer) is det.
% Reinterpret-cast between uint64 and int64. For example,
% =|uint64_int64(0xffffffffffffffff,-1)|=.
%
% This predicate delays until either =Uint64= or =Int64= is
% sufficiently instantiated.
%
% There is also a non-delayed protobufs:uint64_int64/2
%
% @param Uint64 64-bit unsigned integer
% @param Int64 64-bit signed integer
%
% @error Type,Domain if =Value= or =Codes= are of the wrong
%                    type or out of range.
%
% @error instantiation error if both =Value= and =Codes= are uninstantiated.
uint64_int64_when(Uint64, Int64) :-
    when((nonvar(Uint64) ; nonvar(Int64)), uint64_int64(Uint64, Int64)).

% Reversed argument ordering for maplist/3
int64_uint64_when(Int64, Uint64) :-
    uint64_int64_when(Uint64, Int64).

%! uint32_int32_when(?Uint32, ?Int32) is det.
% Reinterpret-case between uint32 and int32.
%
% This predicate delays until either =Uint32= or =Int32= is
% sufficiently instantiated.
%
% There is also a non-delayed protobufs:uint32_int32/2
%
% @param Uint32 32-bit unsigned integer (range between 0 and 4294967295).
% @param Int32 32-bit signed integer (range between -2147483648 and 2147483647).
%
% @error Type,Domain if =Int32= or =Uint32= are of the wrong
%                    type or out of range.
%
% @error instantiation error if both =UInt32= and =Int32= are uninstantiated.
uint32_int32_when(Uint32, Int32) :-
    when((nonvar(Uint32) ; nonvar(Int32)), uint32_int32(Uint32, Int32)).

% Reversed argument ordering for maplist/3
int32_uint32_when(Int32, Uint32) :-

    uint32_int32_when(Uint32, Int32).

%! int64_float64_when(?Int64:integer, ?Float64:float) is det.
% Reinterpret-cast between uint64 and float64. For example,
% =|int64_float64(3ff0000000000000,1.0)|=.
%
% This predicate delays until either =Int64= or =Float64= is
% sufficiently instantiated.
%
% There is also a non-delayed protobufs:uint64_int64/2
%
% @param Int64 64-bit unsigned integer
% @param Float64 64-bit float
%
% @error Type,Domain if =Value= or =Codes= are of the wrong
%                    type or out of range.
%
% @error instantiation error if both =Value= and =Codes= are uninstantiated.
int64_float64_when(Int64, Float64) :-
    when((nonvar(Int64) ; nonvar(Float64)), int64_float64(Int64, Float64)).

%! int32_float32_when(?Int32:integer, ?Float32:float) is det.
% Reinterpret-cast between uint32 and float32. For example,
% =|int32_float32(0x3f800000,1.0)|=.
%
% This predicate delays until either =Int32= or =Float32= is
% sufficiently instantiated.
%
% There is also a non-delayed protobufs:uint32_int32/2
%
% @param Int32 32-bit unsigned integer
% @param Float32 32-bit float
%
% @error Type,Domain if =Value= or =Codes= are of the wrong
%                    type or out of range.
%
% @error instantiation error if both =Value= and =Codes= are uninstantiated.
int32_float32_when(Int32, Float32) :-
    when((nonvar(Int32) ; nonvar(Float32)), int32_float32(Int32, Float32)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Use protobufs meta-data (see the section on protoc in the "overview" documentation).

% The protoc plugin generates the following facts (all starting with "proto_meta_").
% The are documented in protoc-gen-swipl and in the overview section.

:- multifile
     proto_meta_normalize/2,              % (Unnormalized, Normalized)
     proto_meta_package/3,                % (Package, FileName, Options)
     proto_meta_message_type/3,           % (Fqn, Package, Name)
     proto_meta_message_type_map_entry/1, % (Fqn)
     proto_meta_field_name/4,             % (Fqn, FieldNumber, FieldName, FqnName)
     proto_meta_field_json_name/2,        % (FqnName, JsonName)
     proto_meta_field_label/2,            % (FqnName, LabelRepeatOptional) % LABEL_OPTIONAL, LABEL_REQUIRED, LABEL_REPEATED
     proto_meta_field_type/2,             % (FqnName, Type) % TYPE_INT32, TYPE_MESSAGE, etc
     proto_meta_field_type_name/2,        % (FqnName, TypeName)
     proto_meta_field_default_value/2,    % (FqnName, DefaultValue)
     proto_meta_field_option_packed/1,    % (FqnName)
     proto_meta_enum_type/3,              % (FqnName, Fqn, Name)
     proto_meta_enum_value/3,             % (FqnName, Name, Number)
     proto_meta_field_oneof_index/2,      % (FqnName, Index)
     proto_meta_oneof/3.                  % (FqnName, Index, Name)

proto_meta_enum_value_when(ContextType, EnumValue, IntValue) :-
    when((nonvar(EnumValue) ; nonvar(IntValue)),
         proto_meta_enum_value_(ContextType, EnumValue, IntValue)).

proto_meta_enum_value_(ContextType, EnumValue, IntValue) :-
    (   proto_meta_enum_value(ContextType, EnumValue, IntValue)
    ->  true
    ;   existence_error(ContextType, EnumValue-IntValue)
    ).

:- det(segment_to_term/3).
%! segment_to_term(+ContextType:atom, +Segment, -FieldAndValue) is det.
% ContextType is the type (name) of the containing message
% Segment is a segment from protobuf_segment_message/2
% TODO: if performance is an issue, this code can be combined with
%       protobuf_segment_message/2 (and thereby avoid the use of protobuf_segment_convert/2)
segment_to_term(ContextType0, Segment, FieldAndValue) =>
    segment_type_tag(Segment, _, Tag),
    field_and_type(ContextType0, Tag, FieldName, _FqnName, ContextType, RepeatOptional, Type),
    (   RepeatOptional = repeat_packed
    ->  convert_segment_packed(Type, ContextType, Tag, Segment, Value)
    ;   convert_segment(Type, ContextType, Tag, Segment, Value)
    ),
    !, % TODO: get rid of this?
    FieldAndValue = field_and_value(FieldName,RepeatOptional,Value).

% :- det(convert_segment_packed/5). % TODO: "succeeded with a choicepoint"
%! convert_segment_packed(+Type:atom, +ContextType:atom, +Tag:atom, ?Segment, ?Values) is det.
% Reversible on =Segment=, =Values=.
%
% TODO: these are very similar to convert_segment - can they be combined?

convert_segment_packed('TYPE_DOUBLE', _ContextType, Tag, Segment0, Values) =>
    freeze(Segment0, protobuf_segment_convert(Segment0, packed(Tag, fixed64(Values0)))),
    maplist(int64_float64_when, Values0, Values), !.
convert_segment_packed('TYPE_FLOAT', _ContextType, Tag, Segment0, Values) =>
    freeze(Segment0, protobuf_segment_convert(Segment0, packed(Tag, fixed32(Values0)))),
    maplist(int32_float32_when, Values0, Values), !.
convert_segment_packed('TYPE_INT64', _ContextType, Tag, Segment0, Values) =>
    freeze(Segment0, protobuf_segment_convert(Segment0, packed(Tag, varint(Values0)))),
    maplist(uint64_int64_when, Values0, Values).
convert_segment_packed('TYPE_UINT64', _ContextType, Tag, Segment0, Values) =>
    protobuf_segment_convert(Segment0, packed(Tag, varint(Values))), !.
convert_segment_packed('TYPE_INT32', _ContextType, Tag, Segment0, Values) =>
    freeze(Segment0, protobuf_segment_convert(Segment0, packed(Tag, varint(Values0)))),
    maplist(uint32_int32_when, Values0, Values).
convert_segment_packed('TYPE_FIXED64', _ContextType, Tag, Segment0, Values) =>
    freeze(Segment0, protobuf_segment_convert(Segment0, packed(Tag, fixed64(Values0)))),
    maplist(int64_uint64_when, Values0, Values).
convert_segment_packed('TYPE_FIXED32', _ContextType, Tag, Segment0, Values) =>
    freeze(Segment0, protobuf_segment_convert(Segment0, packed(Tag, fixed32(Values0)))),
    maplist(int32_uint32_when, Values0, Values).
convert_segment_packed('TYPE_BOOL', _ContextType, Tag, Segment0, Values) =>
    freeze(Segment0, protobuf_segment_convert(Segment0, packed(Tag, varint(Values0)))),
    maplist(int_bool_when, Values0, Values).
% TYPE_STRING  isn't allowed TODO: add it anyway?
% TYPE_GROUP   isn't allowed
% TYPE_MESSAGE isn't allowed
% TYPE_BYTES   isn't allowed TODO: add it anyway?
convert_segment_packed('TYPE_UINT32', _ContextType, Tag, Segment0, Values) =>
    protobuf_segment_convert(Segment0, packed(Tag, varint(Values))), !.
convert_segment_packed('TYPE_ENUM', ContextType, Tag, Segment0, Values) =>
    % uint64_int64_when(...), % TODO! https://github.com/SWI-Prolog/contrib-protobufs/issues/9
    freeze(Segment0, protobuf_segment_convert(Segment0, packed(Tag, varint(Values0)))),
    maplist(convert_enum(ContextType), Values, Values0).
convert_segment_packed('TYPE_SFIXED32', _ContextType, Tag, Segment0, Values) =>
    protobuf_segment_convert(Segment0, packed(Tag, fixed32(Values))).
convert_segment_packed('TYPE_SFIXED64', _ContextType, Tag, Segment0, Values) =>
    protobuf_segment_convert(Segment0, packed(Tag, fixed64(Values))).
convert_segment_packed('TYPE_SINT32', _ContextType, Tag, Segment0, Values) =>
    freeze(Segment0, protobuf_segment_convert(Segment0, packed(Tag, varint(Values0)))),
    maplist(int64_zigzag_when, Values, Values0).
convert_segment_packed('TYPE_SINT64', _ContextType, Tag, Segment0, Values) =>
    freeze(Segment0, protobuf_segment_convert(Segment0, packed(Tag, varint(Values0)))),
    maplist(int64_zigzag_when, Values, Values0).
% convert_segment_packed(Type, ContextType, Tag, Segment, Values) => % TODO: delete this clause
%     domain_error(type(type=Type, % TODO: this is a bit funky
%                       context_type=ContextType),
%                  value(segment=Segment,
%                        tag=Tag,
%                        values=Values)).

:- det(convert_segment/5).
%! convert_segment(+Type:atom, +ContextType:atom, Tag:atom, ?Segment, ?Value) is det.
% Compute an appropriate =Value= from the combination of descriptor
% "type" (in =Type=) and a =Segment=.
% Reversible on =Segment=, =Values=.
convert_segment('TYPE_DOUBLE', _ContextType, Tag, Segment0, Value) =>
    Segment = fixed64(Tag,Int64),
    int64_float64_when(Int64, Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_FLOAT', _ContextType, Tag, Segment0, Value) =>
    Segment = fixed32(Tag,Int32),
    int32_float32_when(Int32, Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_INT64', _ContextType, Tag, Segment0, Value) =>
    Segment = varint(Tag,Value0),
    uint64_int64_when(Value0, Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_UINT64', _ContextType, Tag, Segment0, Value) =>
    Segment = varint(Tag,Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_INT32', _ContextType, Tag, Segment0, Value) =>
    Segment = varint(Tag,Value0),
    uint32_int32_when(Value0, Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_FIXED64', _ContextType, Tag, Segment0, Value) =>
    Segment = fixed64(Tag,Value0),
    uint64_int64_when(Value, Value0),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_FIXED32', _ContextType, Tag, Segment0, Value) =>
    Segment = fixed32(Tag,Value0),
    uint32_int32_when(Value, Value0),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_BOOL', _ContextType, Tag, Segment0, Value) =>
    Segment = varint(Tag,Value0),
    int_bool_when(Value0, Value),
    protobuf_segment_convert(Segment0, Segment), !.
% convert_segment('TYPE_STRING', _ContextType, Tag, Segment0, Value) =>
%     Segment = string(Tag,ValueStr),
%     protobuf_segment_convert(Segment0, Segment), !,
%     (   false    % TODO: control whether atom or string with an option
%     ->  atom_string(Value, ValueStr)
%     ;   Value = ValueStr
%     ).
convert_segment('TYPE_STRING', _ContextType, Tag, Segment0, Value) =>
    % TODO: option to control whether to use atom_string(Value,ValueStr)
    Segment = string(Tag,Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_GROUP', ContextType, Tag, Segment0, Value) =>
    Segment = group(Tag,MsgSegments),
    % TODO: combine with TYPE_MESSAGE code:
    (   nonvar(Value)
    ->  dict_pairs(Value, _, FieldValues),
        maplist(field_segment(ContextType), FieldValues, MsgSegments),
        protobuf_segment_convert(Segment0, Segment)
    ;   protobuf_segment_convert(Segment0, Segment),
        maplist(segment_to_term(ContextType), MsgSegments, MsgFields),
        combine_fields(MsgFields, ContextType{}, Value)
    ), !.
convert_segment('TYPE_MESSAGE', ContextType, Tag, Segment0, Value) =>
    Segment = message(Tag,MsgSegments),
    (   nonvar(Value)
    ->  dict_pairs(Value, _, FieldValues),
        maplist(field_segment(ContextType), FieldValues, MsgSegments),
        protobuf_segment_convert(Segment0, Segment)
    ;   protobuf_segment_convert(Segment0, Segment),
        maplist(segment_to_term(ContextType), MsgSegments, MsgFields),
        combine_fields(MsgFields, ContextType{}, Value)
    ), !.
convert_segment('TYPE_BYTES', _ContextType, Tag, Segment0, Value) =>
    Segment = length_delimited(Tag,Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_UINT32', _ContextType, Tag, Segment0, Value) =>
    Segment = varint(Tag,Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_ENUM', ContextType, Tag, Segment0, Value) =>
    Segment = varint(Tag,Value0),
    convert_enum(ContextType, Value, Value0), % TODO: negative values: https://github.com/SWI-Prolog/contrib-protobufs/issues/9
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_SFIXED32', _ContextType, Tag, Segment0, Value) =>
    Segment = fixed32(Tag,Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_SFIXED64', _ContextType, Tag, Segment0, Value) =>
    Segment = fixed64(Tag,Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_SINT32', _ContextType, Tag, Segment0, Value) =>
    Segment = varint(Tag,Value0),
    int64_zigzag_when(Value, Value0),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_SINT64', _ContextType, Tag, Segment0, Value) =>
    Segment = varint(Tag,Value0),
    int64_zigzag_when(Value, Value0),
    protobuf_segment_convert(Segment0, Segment), !.

convert_enum(ContextType, Enum, Uint) :-
    uint64_int64_when(Uint, Int),
    proto_meta_enum_value_when(ContextType, Enum, Int).

% TODO: use options to translate to/from false, true (see json_read/3)
int_bool(0, false).
int_bool(1, true).

int_bool_when(Int, Bool) :-
    when((nonvar(Int) ; nonvar(Bool)), int_bool(Int, Bool)).

%! add_defaulted_fields(+Value0:dict, ContextType:atom, -Value:dict) is det.
add_defaulted_fields(Value0, ContextType, Value) :-
    % Can use bagof or findall if we know that there aren't any
    % duplicated proto_meta_field_name/4 rules, although this isn't
    % strictly necessary (just avoids processing a field twice).
    ( setof(Name-DefaultValue, message_field_default(ContextType, Name, DefaultValue), DefaultValues)
    ->  true
    ;   DefaultValues = []
    ),
    foldl(add_empty_field_if_missing, DefaultValues, Value0, Value).

%! message_field_default(+ContextType:atom, Name:atom, -DefaultValue) is semidet.
message_field_default(ContextType, Name, DefaultValue) :-
    proto_meta_field_name(ContextType, _FieldNumber, Name, Fqn),
    proto_meta_field_default_value(Fqn, DefaultValue),
    % If the field is part of a "oneof" group, then there will be a
    % proto_meta_oneof entry for it (using the oneof_index). All
    % fields have a oneof_index, but our code doesn't depend on that.
    \+ (proto_meta_field_oneof_index(Fqn, OneofIndex),
        proto_meta_oneof(ContextType, OneofIndex, _)).

add_empty_field_if_missing(FieldName-DefaultValue, Dict0, Dict) :-
    (   get_dict(FieldName, Dict0, _)
    ->  Dict = Dict0
    ;   put_dict(FieldName, Dict0, DefaultValue, Dict)
    ).

:- det(combine_fields/3).
%! combine_fields(+Fields:list, +MsgDict0, -MsgDict) is det.
% Combines the fields into a dict and sets missing fields to their default values.
% If the field is marked as 'norepeat' (optional/required), then the last
%    occurrence is kept (as per the protobuf wire spec)
% If the field is marked as 'repeat', then all the occurrences
%    are put into a list, in order.
% This code assumes that fields normally occur all together, but can handle
% (less efficiently) fields not occurring together, as is allowed
% by the protobuf spec.
combine_fields([], MsgDict0, MsgDict) =>
    is_dict(MsgDict0, ContextType),
    add_defaulted_fields(MsgDict0, ContextType, MsgDict).
combine_fields([field_and_value(Field,norepeat,Value)|Fields], MsgDict0, MsgDict) =>
    put_dict(Field, MsgDict0, Value, MsgDict1),
    combine_fields(Fields, MsgDict1, MsgDict).
combine_fields([field_and_value(Field,repeat_packed,Values0)|Fields], MsgDict0, MsgDict) =>
    (   get_dict(Field, MsgDict0, ExistingValues)
    ->  append(ExistingValues, Values0, Values)
    ;   Values = Values0
    ),
    put_dict(Field, MsgDict0, Values, MsgDict1),
    combine_fields(Fields, MsgDict1, MsgDict).
combine_fields([field_and_value(Field,repeat,Value)|Fields], MsgDict0, MsgDict) =>
    combine_fields_repeat(Fields, Field, NewValues, RestFields),
    (   get_dict(Field, MsgDict0, ExistingValues)
    ->  append(ExistingValues, [Value|NewValues], Values)
    ;   Values = [Value|NewValues]
    ),
    put_dict(Field, MsgDict0, Values, MsgDict1),
    combine_fields(RestFields, MsgDict1, MsgDict).

:- det(combine_fields_repeat/4).
%! combine_fields_repeat(+Fields:list, Field:atom, -Values:list, RestFields:list) is det.
% Helper for combine_fields/3
% Stops at the first item that doesn't match =Field= - the assumption
% is that all the items for a field will be together and if they're
% not, they would be combined outside this predicate.
%
% @param Fields a list of fields (Field-Repeat-Value)
% @param Field the name of the field that is being combined
% @param Values gets the Value items that match Field
% @param RestFields gets any left-over fields
combine_fields_repeat([], _Field, Values, RestFields) => Values = [], RestFields = [].
combine_fields_repeat([Field-repeat-Value|Fields], Field, Values, RestFields) =>
    Values = [Value|Values2],
    combine_fields_repeat(Fields, Field, Values2, RestFields).
combine_fields_repeat(Fields, _Field, Values, RestFields) => Values = [], RestFields = Fields.

:- det(field_and_type/7).
%! field_and_type(+ContextType:atom, +Tag:int, -FieldName:atom, -FqnName:atom, -ContextType2:atom, -RepeatOptional:atom, -Type:atom) is det.
% Lookup a =ContextType= and =Tag= to get the field name, type, etc.
field_and_type(ContextType, Tag, FieldName, FqnName, ContextType2, RepeatOptional, Type) =>
    assertion(ground(ContextType)), % TODO: remove
    assertion(ground(Tag)), % TODO: remove
    (   proto_meta_field_name(ContextType, Tag, FieldName, FqnName),
        proto_meta_field_type_name(FqnName, ContextType2),
        fqn_repeat_optional(FqnName, RepeatOptional),
        proto_meta_field_type(FqnName, Type)
    ->  true % Remove choicepoint, if JITI didn't do the right thing.
    ;   existence_error(ContextType, Tag)
    ).

%! fqn_repeat_optional(+FqnName:atom, -RepeatOptional:atom) is det.
% Lookup up proto_meta_field_label(FqnName, _), proto_meta_field_option_packed(FqnName)
% and set RepeatOptional to one of
% =norepeat=, =repeat=, =repeat_packed=.
fqn_repeat_optional(FqnName, RepeatOptional) =>
    % TODO: existence_error if \+ proto_meta_field_label
    proto_meta_field_label(FqnName, LabelRepeatOptional),
    (   LabelRepeatOptional = 'LABEL_REPEATED',
        proto_meta_field_option_packed(FqnName)
    ->  RepeatOptional = repeat_packed
    ;   \+ proto_meta_field_option_packed(FqnName), % validity check
        fqn_repeat_optional_2(LabelRepeatOptional, RepeatOptional)
    ).

:- det(fqn_repeat_optional_2/2).
%! fqn_repeat_optional_2(+DescriptorLabelEnum:atom, -RepeatOrEmpty:atom) is det.
% Map the descriptor "label" to 'repeat' or 'norepeat'.
% From proto_meta_enum_value('.google.protobuf.FieldDescriptorProto.Label', Label, _).
fqn_repeat_optional_2('LABEL_OPTIONAL', norepeat).
fqn_repeat_optional_2('LABEL_REQUIRED', norepeat).
fqn_repeat_optional_2('LABEL_REPEATED', repeat).

%! field_descriptor_label_repeated(+Label:atom) is semidet.
% From proto_meta_enum_value('.google.protobuf.FieldDescriptorProto.Label', 'LABEL_REPEATED', _).
% TODO: unused
field_descriptor_label_repeated('LABEL_REPEATED').

%! field_descriptor_label_single(+Label:atom) is semidet.
% From proto_meta_enum_value('.google.protobuf.FieldDescriptorProto.Label', Label, _).
field_descriptor_label_single('LABEL_OPTIONAL').
field_descriptor_label_single('LABEL_REQUIRED').

:- det(term_to_segments/3).
%! term_to_segments(+Term:dict, +MessageType:atom, Segments) is det.
% Recursively traverse a =Term=, generating message segments
term_to_segments(Term, MessageType, Segments) :-
    dict_pairs(Term, _, FieldValues),
    maplist(field_segment(MessageType), FieldValues, Segments).

:- det(field_segment/3).
% MessageType is the FQN of the field type (e.g., '.test.Scalars1')
% FieldName-Value is from the dict_pairs of the term.
% TODO: Throw an error if proto_meta_field_name/4 fails (need to make
%       sure of all the possible uses of field_segment/3 and that
%       nothing depends on it being able to fail without an error).
field_segment(MessageType, FieldName-Value, Segment) :-
    (   proto_meta_field_name(MessageType, Tag, FieldName, FieldFqn),
        proto_meta_field_type(FieldFqn, FieldType),
        proto_meta_field_type_name(FieldFqn, FieldTypeName),
        proto_meta_field_label(FieldFqn, Label)
    ->  true  % Remove choicepoint, if JITI didn't do the right thing.
    ;   existence_error(MessageType, FieldName-Value)
    ),
    (   proto_meta_field_option_packed(FieldFqn)
    ->  Packed = packed
    ;   Packed = not_packed
    ),
    field_segment_scalar_or_repeated(Label, Packed, FieldType, Tag, FieldTypeName, Value, Segment),
    !. % TODO: remove

:- det(field_segment_scalar_or_repeated/7).
%! field_segment_scalar_or_repeated(+Label, +Packed, +FieldType, +Tag, +FieldTypeName, ?Value, Segment) is det.
% =FieldType= is from the =|.proto|= meta information ('TYPE_SINT32', etc.)
field_segment_scalar_or_repeated('LABEL_OPTIONAL', not_packed, FieldType, Tag, FieldTypeName, Value, Segment) =>
    convert_segment(FieldType, FieldTypeName, Tag, Segment, Value).
field_segment_scalar_or_repeated('LABEL_REQUIRED', not_packed, FieldType, Tag, FieldTypeName, Value, Segment) =>  % same as LABEL_OPTIONAL
    convert_segment(FieldType, FieldTypeName, Tag, Segment, Value).
field_segment_scalar_or_repeated('LABEL_REPEATED', packed, FieldType, Tag, FieldTypeName, Values, Segment) =>
    Segment = packed(Tag,FieldValues),
    maplist(convert_segment_v_s(FieldType, FieldTypeName, Tag), Values, Segments0),
    packed_list(Segments0, FieldValues).
field_segment_scalar_or_repeated('LABEL_REPEATED', not_packed, FieldType, Tag, FieldTypeName, Values, Segment) =>
    Segment = repeated(Segments),
    maplist(convert_segment_v_s(FieldType, FieldTypeName, Tag), Values, Segments).
% field_segment_scalar_or_repeated(Label, Packed, FieldType, Tag, FieldTypeName, Value, Segment) :- % TODO: delete this clause
%     domain_error(type(field_type=FieldType,     % TODO: this is a bit funky
%                       label=Label,
%                       packed=Packed),
%                  value(tag=Tag, field_type_name=FieldTypeName, value=Value, segment=Segment)).

convert_segment_v_s(FieldType, FieldTypeName, Tag, Value, Segment) :-
    convert_segment(FieldType, FieldTypeName, Tag, Segment, Value).

% Convert [varint(1,10),varint(1,20)] to varint(1,[10,20]).
packed_list([], []).
packed_list([T1|Ts], PackedList) :-
    detag(T1, Functor, Tag, _V1, List, PackedList),
    packed_list_([T1|Ts], Functor, Tag, List).

% Functor and Tag are only for verifying that the terms are of the
% expected form.
packed_list_([], _, _, []).
packed_list_([T1|Ts], Functor, Tag, [X1|Xs]) :-
    detag(T1, Functor, Tag, X1, _, _),
    packed_list_(Ts, Functor, Tag, Xs).

%! protobuf_field_is_map(+MessageType, +FieldName) is semidet.
% Succeeds if =MessageType='s =FieldName= is defined as a map<...> in
% the .proto file.
protobuf_field_is_map(MessageType0, FieldName) :-
    proto_meta_normalize(MessageType0, MessageType),
    proto_meta_field_name(MessageType, _, FieldName, FieldFqn),
    proto_meta_field_type(FieldFqn, 'TYPE_MESSAGE'),
    proto_meta_field_label(FieldFqn, 'LABEL_REPEATED'),
    proto_meta_field_type_name(FieldFqn, FieldTypeName),
    proto_meta_message_type_map_entry(FieldTypeName),
    assertion(proto_meta_field_name(FieldTypeName, 1, key, _)),
    assertion(proto_meta_field_name(FieldTypeName, 2, value, _)),
    !.

%! protobuf_map_pairs(+ProtobufTermList:list, ?DictTag:atom, ?Pairs) is det.
% Convert between a list of protobuf map entries (in the form
% =|DictTag{key:Key, value:Value}|= and a key-value list as described
% in library(pairs). At least one of =ProtobufTermList= and =Pairs=
% must be instantiated; =DictTag= can be uninstantiated. If
% =ProtobufTermList= is from a term created by
% protobuf_parse_from_codes/3, the ordering of the items is undefined;
% you can order them by using keysort/2 (or by a predicate such as
% dict_pairs/3, list_to_assoc/2, or list_to_rbtree/2.
protobuf_map_pairs(ProtobufTermList, DictTag, Pairs) :-
    maplist(protobuf_dict_map_pairs(DictTag), ProtobufTermList, Pairs).

protobuf_dict_map_pairs(DictTag, DictTag{key:Key,value:Value}, Key-Value).


end_of_file.
