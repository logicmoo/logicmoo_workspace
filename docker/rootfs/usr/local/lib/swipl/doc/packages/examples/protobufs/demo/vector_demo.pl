% Sample code.
% For more explanation of this code, see ../protobufs_overview.md

% TODO: separate out some tests and use plunit for them.

:- module(vector_demo,
          [
           write_as_proto/1,
           read_from_proto/1,
           vector/2,
           send_command/3,
           send_precompiled_command/3,
           protobuf_bag/2,
           make_tmp99/0,
           test_basic_usage/0,
           test_basic_usage/1,
           test_send_command/0,
           test_send_command/1,
           test_send_precompiled_command/0,
           test_send_precompiled_command/1,
           test_xml/0,
           test_xml/1,
           test_xml/2
          ]).

:- use_module(library(protobufs)).
:- use_module(library(error)).
:- use_module('../eventually_implies'). % For ~>

:- use_module(library(debug)).
:- set_prolog_flag(optimise_debug, false). % assertion/1 always on


% Example: "Basic Usage"
%  (see "Basic Usage" section in ../protobufs_overview.md)

% TODO: move this to ../test_protobufs.pl
% TODO: Create an associated .proto for this example

%! command(+Term, -Proto) is det.
% Map a Prolog term to a corresponding protobuf term.
command(add(X,Y), Proto) :-
    freeze(X, must_be(integer, X)), % for debugging
    freeze(Y, must_be(integer, Y)), % for debugging
    Proto = protobuf([atom(1, command),
                      atom(2, add),
                      integer(3, X),
                      integer(4, Y)
                     ]).
command2(Command, Op, X, Y, Extra, Proto) :-
    command2_item(Command, atom, Commands),
    command2_item(Op, atom, Ops),
    command2_item(X, integer, Xs),
    command2_item(Y, integer, Ys),
    command2_item(Extra, atom, Extras),
    Proto = protobuf([repeated(1, atom(Commands)),
                      repeated(2, atom(Ops)),
                      repeated(3, integer(Xs)),
                      repeated(4, integer(Ys)),
                      repeated(5, atom(Extras))
                     ]).

command2_item(Item, MustBe, Items) :-
    freeze(Item, must_be(MustBe, Item)),
    (   var(Item)
    ->  freeze(Items, ( Items = [] ; Items = [Item] ) ) % or: last(Items, Item)
    ;   Items = [Item]
    ).

test_basic_usage :-
    forall(test_basic_usage(Term),
           (   print_term(Term, [right_margin(150)]), nl )).

test_basic_usage(['X'=X,
                  'X2'=X2,
                  'Y'=Y,
                  'Y2'=Y2,
                  'Command2'=Command2,
                  'Op2'=Op2,
                  'Extra2'=Extra2,
                  'Proto'=Proto,
                  'WireCodes'=WireCodes,
                  'Segments-raw'=Segments,
                  'CommandCode'=CommandCode,
                  'OpCode'=OpCode,
                  'Xseg'=Xseg,
                  'Yseg'=Yseg]) :-
    X = 666, Y = 123,
    command(add(X,Y), Proto),
    protobuf_message(Proto, WireCodes),
    % and read it back again:
    command(add(X2,Y2), Proto2),
    protobuf_message(Proto2, WireCodes),
    assertion(Proto == Proto2),
    command2(Command2, Op2, X2, Y2, Extra2, Proto3),
    protobuf_message(Proto3, WireCodes),
    protobufs:protobuf_segment_message(Segments, WireCodes),
    Segments = [string(1,CommandCode),
                string(2,OpCode),
                varint(3,Xzig),
                varint(4,Yzig)],
    % The following conversions are based on our knowledge
    % of the Proto template:
    protobufs:int64_zigzag(Xseg, Xzig),
    protobufs:int64_zigzag(Yseg, Yzig),
    protobufs:protobuf_segment_message(Segments, WireCodes4),
    assertion(WireCodes == WireCodes4).

% ======================

% vector_type/2 corresponds to pb_vector.proto enum VectorType
vector_type(double(_List),    2).
vector_type(float(_List),     3).
vector_type(integer(_List),   4).
vector_type(integer64(_List), 5).
vector_type(integer32(_List), 6).
vector_type(unsigned(_List),  7).
vector_type(codes(_List),     8).
vector_type(atom(_List),      9).
vector_type(string(_List),    10).

% basic_vector/2 corresponds to pb_vector.proto message Vector
basic_vector(TypedList, Template) :-
    vector_type(TypedList, Tag),
    Template = protobuf([ repeated(Tag, TypedList) ]).

%! vector(+TypedList, -WireCodes:list(int)) is det.
% TypedList is of the form Type(List) - see vector_type/2
% WireCodes is a list of codes to be output
vector(TypedList, WireCodes):-
    basic_vector(TypedList, Proto),
    protobuf_message(Proto, WireCodes).

%! write_as_proto(+TypedList) is det.
% TypedList is of the form Type(List) - see vector_type/2
write_as_proto(TypedList) :-
    vector(TypedList, Z),
    open('tmp99.tmp', write, S, [encoding(octet),type(binary)])
      ~> close(S),
    format(S, '~s', [Z]), % ~s: list of character codes
    !.

read_from_proto(V) :-
    read_file_to_codes('tmp99.tmp', Codes, [encoding(octet),type(binary)]),
    vector(V, Codes).

protobufs:commands(Key, Value) :-
    nth1(Value,
            [ square,
              decimate,
              transform,
              inverse_transform
            ],
         Key).

send_command(Command, Vector, WireCodes) :-
    basic_vector(Vector, Proto1),
    Proto = protobuf([enum(1, commands(Command)), embedded(2, Proto1)]),
    % e.g., if Command=square, Proto1=protobuf([repeated(2,double([1,22,3,4]))])
    %       Proto=protobuf([ enum(1,commands(square)),
    %                        embedded(2,protobuf([repeated(2,double([1,22,3,4]))]))
    %                      ])
    % protobuf:commands/2 is used to expand an enum: the code in
    % library(protobufs) expands an` enum(Tag,Type)` by calling `Type`,
    % so enum(1,commands(square)) gets turned into enum(1,1) by calling
    % protobufs:commands(square,Value) => Value=1
    protobuf_message(Proto, WireCodes).

test_send_command :-
    test_send_command(WireCodes),
    protobufs:protobuf_segment_message(Seg, WireCodes),
    print_term(Seg, [right_margin(80)]), nl.

test_send_command(WireCodes) :-
    send_command(square, double([1,22,3,4]), WireCodes).

test_send_precompiled_command :-
    test_send_precompiled_command(WireCodes),
    protobufs:protobuf_segment_message(Seg, WireCodes),
    print_term(Seg, [right_margin(80)]), nl.

test_send_precompiled_command(WireCodes) :-
    send_precompiled_command(square, double([1,22,3,4]), WireCodes).

send_precompiled_command(Command, Vector, WireCodes) :-
    basic_vector(Vector, Proto1),
    precompiled_message(commands(Command), WireCodes, WireCodes1),
    protobuf_message(protobuf([embedded(3, Proto1)]), WireCodes1),

    % Do it again, but without the precompiled message.
    % Above, precompile_commands added
    % [atom(1,command), enum(2,commands(Command)].
    Proto2 = protobuf([atom(1, command),
                       enum(2, commands(Command)),
                       embedded(3, Proto1)]),
    protobuf_message(Proto2, WireCodes2),
    assertion(WireCodes2 == WireCodes).

term_expansion(precompile_commands, Clauses) :-
    findall(precompiled_message(commands(Key), WireCodes, Tail),
            (   protobufs:commands(Key, _),
                Proto = protobuf([atom(1, command),
                                  enum(2, commands(Key))]),
                protobuf_message(Proto, WireCodes, Tail)
            ),
            Clauses).

%
%

compound_protobuf(complex(Real, Img),
                  group(12, [
                             double(1, Real),
                             double(2, Img)])).
compound_protobuf(float(Val),
                  float(13, Val)).
compound_protobuf(double(Val),
                  double(14, Val)).
compound_protobuf((Num rdiv Den),
                  group(15, [
                             integer(1, Num),
                             integer(2, Den)])).
compound_protobuf(integer(Val),
                  integer(16, Val)).

protobuf_bag([], []).

protobuf_bag([Type|More], WireCodes) :-
    compound_protobuf(Type, X),
    Proto = protobuf([embedded(1, protobuf([X]))]),
    protobuf_message(Proto, WireCodes, WireCodes1),
    protobuf_bag(More, WireCodes1),
    !.

make_tmp99 :-
    X is pi,
    write_as_proto(double([-2.2212, -7.6675, X, 0, 1.77e-9, 2.54e222])),
    halt(0).

%
%  Example of adding ornamental message sequences to the parser.
%  In this example we demonstrate managing a recursive structure like
%  XML. The structure shown in xml_proto/1 below, is similar to the
%  structure returned by load_xml_file/2, which is part of the SGML
%  library. We supply three message_sequence decorators: kv_pair,
%  xml_element, and aux_xml_element. These are treated as first class
%  host types.
%
:- multifile protobufs:message_sequence//3.

protobufs:message_sequence(Type, Tag, Value) -->
    { my_message_sequence(Type, Value, Proto) },
    protobufs:message_sequence(embedded, Tag, Proto),
    !.
%
% On encode, the value type determines the tag. And on decode
% the tag to determines the value type.
%
guard(Type, Value) :-
    (nonvar(Value) -> is_of_type(Type, Value); true).

my_message_sequence(kv_pair, Key=Value, Proto) :-
    Proto = protobuf([ atom(30, Key), X]),
    (   (   guard(integer, Value), X = integer(31, Value));
        (   guard(float, Value),   X = double(32, Value));
        (   guard(atom, Value),    X = atom(33, Value))).

%
%
my_message_sequence(xml_element, element(Name, Attributes, Contents), Proto) :-
    Proto = protobuf([ atom(21, Name),
                       repeated(22, kv_pair(Attributes)),
                       repeated(23, aux_xml_element(Contents))]).
%
%
my_message_sequence(aux_xml_element, Contents, Proto) :-
    Contents = element(_Name, _Attributes, _ElementContents),
    Proto = protobuf([xml_element(40, Contents)]).

my_message_sequence(aux_xml_element, Contents, Proto) :-
    Proto = protobuf([atom(43, Contents)]).


% This is embedded in protobuf([repeated(20, xml_element(...))])
% - see test_xml/2.
xml_proto([element(space1,
                   [foo='1',
                    bar='2'],
                   [fum,
                    bar,
                    element(space2,
                            [fum= 3.1415,
                             bum= -14],
                            ['more stuff for you']),
                    element(space2b,
                            [],
                            [this,
                             is,
                             embedded,
                             also]),
                    to,
                    you])]).

xml_protobuf(X) :-
    X = protobuf([
          repeated(20,
              embedded([protobuf([string(21,"space1"),
                                  repeated(22,
                                      embedded([protobuf([string(30,"foo"),string(33,"1")]),protobuf([string(30,"bar"),string(33,"2")])])),
                                  repeated(23,
                                      embedded([protobuf([string(43,"fum")]),
                                                protobuf([string(43,"bar")]),
                                                protobuf([embedded(40,
                                                                   protobuf([string(21,"space2"),
                                                                             repeated(22,
                                                                                 embedded([protobuf([string(30,"fum"),double(32,3.1415)]),
                                                                                           protobuf([string(30,"bum"),
                                                                                                     integer(31,-14)
                                                                                                    ])
                                                                                          ])),
                                                                             embedded(23,protobuf([string(43,"more stuff for you")]))
                                                                            ]))
                                                         ]),
                                                protobuf([embedded(40,
                                                                   protobuf([string(21,"space2b"),
                                                                             repeated(23,
                                                                                 embedded([protobuf([string(43,"this")]),
                                                                                           protobuf([string(43,"is")]),
                                                                                           protobuf([string(43,"embedded")]),
                                                                                           protobuf([string(43,"also")])
                                                                                          ]))
                                                                            ]))
                                                         ]),
                                                protobuf([string(43,"to")]),
                                                protobuf([string(43,"you")])
                                               ]))
                                 ])
                       ]))
                 ]).

test_xml(XmlProto, WireCodes) :-
    Proto = protobuf([repeated(20, xml_element(XmlProto))]),
    protobuf_message(Proto, WireCodes).

%! test_xml(-WireCodes:list(int)) is det.
% Tests outputting the data defined by xml_proto/1.
% =WireCodes= is a list of codes to be output
test_xml(['XmlProto'=XmlProto, 'WireCodes'=WireCodes, 'Segments'=Segments, 'Template'=Template]) :-
    xml_proto(XmlProto),
    test_xml(XmlProto, WireCodes),
    test_xml(XmlProto2, WireCodes),
    XmlProto == XmlProto2,
    protobufs:protobuf_segment_message(Segments, WireCodes),
    % segments_to_template(Segments, T0), print_term(T0, [right_margin(160)]), nl,
    xml_protobuf(Template),
    protobuf_message(Template, WireCodes),
    % Verify same WireCodes as produced by xml_example.py:
    assertion(WireCodes ==
       [162, 1, 198, 1, 170, 1, 6, 115, 112, 97, 99, 101, 49, 178, 1,
        10, 242, 1, 3, 102, 111, 111, 138, 2, 1, 49, 178, 1, 10, 242,
        1, 3, 98, 97, 114, 138, 2, 1, 50, 186, 1, 6, 218, 2, 3, 102,
        117, 109, 186, 1, 6, 218, 2, 3, 98, 97, 114, 186, 1, 67, 194,
        2, 64, 170, 1, 6, 115, 112, 97, 99, 101, 50, 178, 1, 16, 242,
        1, 3, 102, 117, 109, 129, 2, 111, 18, 131, 192, 202, 33, 9,
        64, 178, 1, 9, 242, 1, 3, 98, 117, 109, 248, 1, 27, 186, 1,
        21, 218, 2, 18, 109, 111, 114, 101, 32, 115, 116, 117, 102,
        102, 32, 102, 111, 114, 32, 121, 111, 117, 186, 1, 55, 194, 2,
        52, 170, 1, 7, 115, 112, 97, 99, 101, 50, 98, 186, 1, 7, 218,
        2, 4, 116, 104, 105, 115, 186, 1, 5, 218, 2, 2, 105, 115, 186,
        1, 11, 218, 2, 8, 101, 109, 98, 101, 100, 100, 101, 100, 186,
        1, 7, 218, 2, 4, 97, 108, 115, 111, 186, 1, 5, 218, 2, 2, 116,
        111, 186, 1, 6, 218, 2, 3, 121, 111, 117]).

test_xml :-
    test_xml(['XmlProto'=XmlProto, 'WireCodes'=WireCodes, 'Segments'=Segments, 'Template'=Template]),
    print_term('XmlProto'=XmlProto, [right_margin(160)]), nl,
    print_term('Segments'=Segments, [right_margin(160)]), nl,
    format('WireCodes: ~q~n', [WireCodes]),
    print_term('Template'=Template, [right_margin(160)]), nl.


% A simple predicate to help in converting segments to a template.
segments_to_template(Segments, protobuf(Templates)) :-
    maplist(segment_to_template, Segments, Templates).

segment_to_template(message(Tag, Fields), embedded(Tag, protobuf(Templates))) :-
    maplist(segment_to_template, Fields, Templates).
segment_to_template(string(Tag, String), string(Tag, String)).
segment_to_template(fixed64(Tag, Codes), fixed64(Tag, Codes)).
segment_to_template(fixed32(Tag, Codes), fixed32(Tag, Codes)).
segment_to_template(varint(Tag, Int), varint(Tag, Int)).

% TODO: Convert xml_proto to dict form

xml_xlate(element(Name,Attributes0,Contents0),
          xml_element{name:Name,
                      attributes:Attributes,
                      contents:Contents}) :-
    maplist(kv_pair, Attributes0, Attributes),
    maplist(aux_xml_element, Contents0, Contents).

kv_pair(Key=Value, Dict) :-
    (   nonvar(Dict)
    ->  Dict >:< kv_pair{key:Key, int_value:Value, float_value:Value, atom_value:Value}
    ;   integer(Value) -> Dict = kv_pair{key:Key, int_value:Value}
    ;   float(  Value) -> Dict = kv_pair{key:Key, float_value:Value}
    ;   atom(   Value) -> Dict = kv_pair{key:Key, atom_value:Value}
    ).

aux_xml_element(element(Name, Attributes, Contents),
                aux_xml_element{element:Element}) :-
    xml_xlate(element(Name, Attributes, Contents), Element).
aux_xml_element(Atom, aux_xml_element{atom:Atom}).

test_xml_xlate :-
    test_xml_xlate(D),
    print_term(D, [right_margin(140)]).

test_xml_xlate(D) :-
    xml_proto(X),
    maplist(xml_xlate, X, D),
    vector_demo:maplist(xml_xlate, Y, D),
    X == Y.

precompile_commands.  % Trigger the term-expansion precompilation
