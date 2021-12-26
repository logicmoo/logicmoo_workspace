% -*- mode: Prolog; coding:utf-8 -*-

% Common stuff for test_read.pl, test_write.pl
% (Originally this just had templates, but other common predicates
% have been added)

:- module(test_templates, [scalars1_template/2,
                           repeated1a_template/2,
                           packed1a_template/2,
                           string_values/8,
                           assertion_eq_dict/2,
                           message_file/2,
                           read_message_codes/2,
                           write_message_codes/2,
                           test_write_template/2,
                           print_term_stderr/1,
                           print_term_stderr/2]).

:- encoding(utf8).

:- autoload(library(pprint)).
:- autoload(library(debug), [assertion/1]).
:- autoload(library(readutil), [read_file_to_codes/3]).
:- autoload(library(protobufs)).
:- autoload(library(lists), [nth0/3]).

%! protobufs:my_enum(?Key:atom, ?Value:atom) is semidet.
% Define the my_enum callback.
protobufs:my_enum(Key, Value) :-
    nth0(Value,
         ['E1',
          'Enum2',
          'AnotherEnum'],
         Key).

%! write_message_codes(+LocalPath:atom, +WireCodes:list(int)) is det.
% Write a "wire stream" to a file with =LocalPath= relative to this
% module, into =WireCodes=.
write_message_codes(LocalPath, WireCodes) :-
    message_file(LocalPath, Path),
    open(Path, write, Stream, [encoding(octet),type(binary)]),
    format(Stream, '~s', [WireCodes]),
    close(Stream).

%! read_message_codes(+LocalPath:atom, -WireCodes:list(int)) is det.
% Read a "wire stream" file with =LocalPath= relative to this
% module, into =WireCodes=.
read_message_codes(LocalPath, WireCodes) :-
    message_file(LocalPath, Path),
    read_file_to_codes(Path, WireCodes, [encoding(octet),type(binary)]),
    assertion(ground(WireCodes)). % TODO: not needed

test_write_template(Path, Template) :-
    protobuf_message(Template, WireCodes),
    write_message_codes(Path, WireCodes).

%! message_file(+LocalPath:atom, -Path:atom) is det.
% Given a =LocalPath=, determine its path relative to the current module.
message_file(LocalPath, Path) :-
    source_file(test_templates:message_file(_,_), MyFile),
    file_directory_name(MyFile, MyDir),
    atomic_list_concat([MyDir, LocalPath], /, Path).

%! assertion_eq_dict(+D1:dict, +D2:dict) is det.
% Convenience predicate - does assertion/2 check for the two dicts, then
% repeats field by field. This produces nicer error messages.
assertion_eq_dict(D1, D2) :-
    ( assertion(D1 == D2) -> true ; true ),
    dict_pairs(D1, _, D1pairs),
    dict_pairs(D2, _, D2pairs),
    assertion_eq(D1pairs, D2pairs).

%! assertion_eq(+D1Fields, +D2Fields) is det.
% Helper for assertion_eq_dict/2 - pair-wise comparison of fields.
assertion_eq([], []).
assertion_eq([V|Vs], []) :-
    assertion(V == '*not-exist*'),
    assertion_eq(Vs, []).
assertion_eq([], [V|Vs]) :-
    assertion('*not-exist*' == V),
    assertion_eq([], Vs).
assertion_eq([K1-V1|Xs], [K2-V2|Ys]) :-
    (   K1 @< K2
    ->  assertion(K1-V1 == '*not-exist*'),
        assertion_eq(Xs, [K2-V2|Ys])
    ;   K1 == K2
    ->  assertion(K1-V1 == K2-V2),
        assertion_eq(Xs, Ys)
    ;   assertion('*not-exist*' == K2-V2),
        assertion_eq([K1-V1|Xs], Ys)
    ).

print_term_stderr(Term) :-
    print_term_stderr('', Term).

print_term_stderr(Msg, Term) :-
    format(user_error, '~n~w', [Msg]),
    print_term(Term, [output(user_error)]),
    nl(user_error).

%! string_values(-S1, -S2, -S3, -S4, -C1, -C2, -C3, -C4) is det.
% Get some test strings.
% Note: Windows version of swipl doesn't support > 0xffff, and surrogate pairs are poorly supported.
string_values(S1, S2, S3, S4, C1, C2, C3, C4) :-
    C1 = [0xe9, 0x63, 0x72, 0x61, 0x6e, 0x20, 0x7db2, 0x76ee, 0x9326, 0x86c7], % "écran 網目錦蛇"
    C2 = [0x7db2, 0x76ee, 0x9326, 0x86c7],  % "網目錦蛇"
    C3 = [0x5b, 0xe0, 0x6d, 0xed, 0x6d, 0xe9, 0x20, 0x6e, 0xed, 0x73, 0x68, 0xed, 0x6b, 0xed, 0x68, 0xe9, 0xa71c, 0x62, 0xec, 0x5d, 0x20, 0x72, 0x65, 0x74, 0x69, 0x63, 0x75, 0x6c, 0x61, 0x74, 0x65, 0x64, 0x20, 0x70, 0x79, 0x74, 0x68, 0x6f, 0x6e], % [àmímé níshíkíhéꜜbì] reticulated python"
    C4 = [0xe0, 0x6d, 0xed, 0x6d, 0xe9, 0x20, 0x6e, 0xed, 0x73, 0x68, 0xed, 0x6b, 0xed, 0x68, 0xe9, 0xa71c, 0x62, 0xec], % àmímé níshíkíhéꜜbì"
    string_codes(S1, C1),
    string_codes(S2, C2),
    string_codes(S3, C3),
    string_codes(S4, C4).

%! scalars1_template(-Template, -Vars:list) is det.
% A protobufs template and the variables in it.
scalars1_template(Template, Vars) :-
    % See test.Scalars1
    Template = protobuf([
                         double(       1, V_double),
                         float(        2, V_float),
                         signed32(   103, V_int32),
                         signed64(   127, V_int64),
                         unsigned(   128, V_uint32),
                         unsigned(   666, V_uint64),
                         integer(    777, V_sint32),
                         integer(    888, V_sint64),
                         integer32(  999, V_fixed32),
                         integer64( 1010, V_fixed64),
                         integer32( 1011, V_sfixed32),
                         integer64( 1012, V_sfixed64),
                         boolean(   1013, V_bool),
                         string(    1014, V_string),
                         codes(     1015, V_bytes),
                         enum(      1016, my_enum(V_enum)),
                         utf8_codes(1017, V_utf8_codes),
                         embedded(  9999, protobuf([string(15,  V_key),
                                                    string(128, V_value)]))
                        ]),
    Vars = [                             V_double,
                                         V_float,
                                         V_int32,
                                         V_int64,
                                         V_uint32,
                                         V_uint64,
                                         V_sint32,
                                         V_sint64,
                                         V_fixed32,
                                         V_fixed64,
                                         V_sfixed32,
                                         V_sfixed64,
                                         V_bool,
                                         V_string,
                                         V_bytes,
                                         V_enum,
                                         V_utf8_codes,
                                         V_key,
                                         V_value
           ].

repeated1a_template(Template, Vars) :-
    % See test.Scalars1, test_write.py - repeated1a
    Template = protobuf([
                         repeated(    1, double(V_double)),
                         repeated(   12, float(V_float)),
                         repeated( 1103, signed32(V_int32)),
                         repeated( 1127, signed64(V_int64)),
                         repeated( 1128, unsigned(V_uint32)),
                         repeated( 1666, unsigned(V_uint64)),
                         repeated( 1777, integer(V_sint32)),
                         repeated( 1888, integer(V_sint64)),
                         repeated( 1999, unsigned32(V_fixed32)),
                         repeated(11010, unsigned64(V_fixed64)),
                         repeated(11011, integer32(V_sfixed32)),
                         repeated(11012, integer64(V_sfixed64)),
                         repeated(11013, boolean(V_bool)),
                         repeated(11014, string(V_string)),
                         repeated(11015, codes(V_bytes)),
                         repeated(11016, enum(my_enum(V_enum))),
                         repeated(11017, utf8_codes(V_utf8_codes)),
                         repeated_embedded(99999,
                                           protobuf([string(15, _Key),
                                                     string(128, _Value)]),
                                           V_key_values)
                        ]),

    Vars = [                             V_double,
                                         V_float,
                                         V_int32,
                                         V_int64,
                                         V_uint32,
                                         V_uint64,
                                         V_sint32,
                                         V_sint64,
                                         V_fixed32,
                                         V_fixed64,
                                         V_sfixed32,
                                         V_sfixed64,
                                         V_bool,
                                         V_string,
                                         V_bytes,
                                         V_enum,
                                         V_utf8_codes,
                                         V_key_values
           ].

packed1a_template(Template, Vars) :-
    % repeated1a_template/2, using packed where possible
    Template = protobuf([
                         packed(      1, double(V_double)),
                         packed(     12, float(V_float)),
                         packed(   1103, signed32(V_int32)),
                         packed(   1127, signed64(V_int64)),
                         packed(   1128, unsigned(V_uint32)),
                         packed(   1666, unsigned(V_uint64)),
                         packed(   1777, integer(V_sint32)),
                         packed(   1888, integer(V_sint64)),
                         packed(   1999, unsigned32(V_fixed32)),
                         packed(  11010, unsigned64(V_fixed64)),
                         packed(  11011, integer32(V_sfixed32)),
                         packed(  11012, integer64(V_sfixed64)),
                         packed(  11013, boolean(V_bool)),
                         repeated(11014, string(V_string)),
                         repeated(11015, codes(V_bytes)),
                         packed(  11016, enum(my_enum(V_enum))),
                         repeated(11017, utf8_codes(V_utf8_codes)),
                         repeated_embedded(99999,
                                           protobuf([string(15, _Key),
                                                     string(128, _Value)]),
                                           V_key_values)
                        ]),

    Vars = [                           V_double,
                                       V_float,
                                       V_int32,
                                       V_int64,
                                       V_uint32,
                                       V_uint64,
                                       V_sint32,
                                       V_sint64,
                                       V_fixed32,
                                       V_fixed64,
                                       V_sfixed32,
                                       V_sfixed64,
                                       V_bool,
                                       V_string,
                                       V_bytes,
                                       V_enum,
                                       V_utf8_codes,
                                       V_key_values
           ].

