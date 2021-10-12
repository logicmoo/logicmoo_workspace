% -*- mode: Prolog; coding:utf-8 -*-

% The "addressbook" example, derived from
% https://developers.google.com/protocol-buffers/docs/pythontutorial

% To use this, you must first install Google's Protocol Buffer
% compiler and use it to create the "_pb.pl" files. Instructions for
% installing the compiler are at
% https://developers.google.com/protocol-buffers/docs/downloads
%
% You can also install the compiler on Ubuntu and similar systems by:
%    sudo apt install protobuf-compiler
%
% (This version typically isn't the latest, but it should suffice.)
%
% If you wish to try interopability with other languages, there are
% additional things to install; for example, for Python you should do
% something like "python3 -m pip install protobuf"
%
% Once you've installed the protobuf compiler, you need to make
% the SWI-Prolog compiler "plugin" available. The easiest way
% is to add it to your path:
%   PATH="$PATH:/usr/lib/swi-prolog/library/protobufs"
% If you don't want to change your PATH, then you can specify the
% plugin by using the "--plugin" option to protoc, e.g.:
%    protoc ... --plugin=protoc-gen-swipl=/usr/lib/swi-prolog/library/protobufs/protoc-gen-swipl
%
% You can then run the compiler (eventually this loop won't be necessary;
%    protoc -I. -I /usr/include --swipl_out=. addressbook.proto addressbook2.proto google/protobufs/timestamp.proto
% This will create the files addressbook_pb.pl, addressbook2_pb.pl, google/protobufs/timestamp_pb.pl
%
% You can then run this example:
%   swipl -g test_write -g test_write -g test_read -t halt addressbook.pl
%
% This will create a file addressbook.wire that contains the "wire
% format" of an AddressBook message. If you wish to see this file
% using the Google tools:
%   protoc --decode=tutorial.AddressBook addressbook.proto <addressbook.msg
% or you could try the tutorial in another programming language for
% comparison (it swill be able to process the addressbook.wire file
% created by addressbook.pl).

:- module(addressbook, [test_write/0, test_read/0]).

:- use_module(library(protobufs)).
:- use_module(addressbook_pb). % loads addressbook2_pb, google/protobuf/timestamp_pb.pl

write_message(Path, Person) :-
    message_file(Path, FullPath),
    (   exists_file(FullPath)
    ->  read_file_to_codes(FullPath, WireCodes, [encoding(octet),type(binary)]),
        protobuf_parse_from_codes(WireCodes, 'tutorial.AddressBook', AddressBook)
    ;   AddressBook = 'tutorial.AddressBook'{people: []}
    ),
    get_time(TimeStamp),
    TimeSec is integer(floor(TimeStamp)),
    TimeNano is integer((TimeStamp-TimeSec)*1000000000),
    put_dict(timestamps, Person,
             'Timestamp'{last_updated:u{seconds:TimeSec, nanos:TimeNano}}, % 'Timestamp', 'u' are comments
             Person2),
    % The Python example simply appends to the new entry: the following
    % code replaces the entry if it's there:
    (   select(APerson, AddressBook.people, OtherPersons),
        APerson.id == Person2.id
    ->  append(OtherPersons, [Person2], People2)
    ;   append(AddressBook.people, [Person2], People2)
    ),
    protobuf_serialize_to_codes('tutorial.AddressBook'{people: People2},
                                'tutorial.AddressBook', WireCodesOut),
    open(FullPath, write, OutStream, [encoding(octet),type(binary)]),
    format(OutStream, '~s', [WireCodesOut]),
    close(OutStream).

test_write :-
    test_write('addressbook.wire').

test_write(Path) :-
    write_message(Path,
                  _{id:1234,
                    name:"John Doe",
                    email:"jdoe@example.com",
                    phones:[_{number:"555-4321", type:'HOME'}]}),
    write_message(Path,
                  'tutorial.Person'{id:666,
                                    name:"Satan",
                                    email:"satan@fb.com",
                                    phones:['tutorial.Person.PhoneNumber'{number:"555-1212", type:'WORK'},
                                            'tutorial.Person.PhoneNumber'{number:"555-1234", type:'HOME'}]}),
    write_message(Path,
                  'tutorial.Person'{id:999,
                                    name:"Crowley"
                                   % no email
                                   % no phones
                                   }).

test_read :-
    test_read('addressbook.wire').

test_read(Path) :-
    message_file(Path, FullPath),
    read_file_to_codes(FullPath, WireCodes, [encoding(octet),type(binary)]),
    protobuf_parse_from_codes(WireCodes, 'tutorial.AddressBook', AddressBook),
    % format('=== address book ===~n', []),
    % maplist(print_entry, AddressBook.people),
    % format('=== address book (end) ===~n', []).
    % Note the use of =/2 in the following and not ==/2, because of the "_"s:
    assertion(ground(AddressBook.people)),
    assertion(AddressBook.people =
             ['.tutorial.Person'{email:"jdoe@example.com",
                                 id:1234,
                                 name:"John Doe",
                                 phones:['.tutorial.Person.PhoneNumber'{number:"555-4321", type:'HOME'}],
                                 timestamps:'.tutorial.TimeStamps'{last_updated:'.google.protobuf.Timestamp'{nanos:_,seconds:_},
                                                                   updates:[]}},
              '.tutorial.Person'{email:"satan@fb.com",
                                 id:666,
                                 name:"Satan",
                                 phones:['.tutorial.Person.PhoneNumber'{number:"555-1212", type:'WORK'},
                                         '.tutorial.Person.PhoneNumber'{number:"555-1234", type:'HOME'}],
                                 timestamps:'.tutorial.TimeStamps'{last_updated:'.google.protobuf.Timestamp'{nanos:_,seconds:_},
                                                                   updates:[]}},
              '.tutorial.Person'{email:"",
                                 id:999,
                                 name:"Crowley",
                                 phones:[],
                                 timestamps:'.tutorial.TimeStamps'{last_updated:'.google.protobuf.Timestamp'{nanos:_,seconds:_},
                                                                   updates:[]}}
             ]).

print_entry(Person) :-
    % In Python, you test for the email existing by
    % person.HasField('email'); but in SWI-Prolog, it's a regular
    % dict, so use get_dict(email, Person, Email)

    % You can print the entire term for debugging by:
    %    print_term(Person, [indent_arguments(4), right_margin(80), output(current_output)]),

    format('Person ID: ~w~n', [Person.id]),
    format('  Name: ~w~n', [Person.name]),
    (   Person.email \= "" % default value: ""
    ->  format('  Email: ~w~n', [Person.email])
    ;   true
    ),
    maplist(print_phone_number, Person.phones),
    % TODO: print last update timestamp
    nl.

print_phone_number(Phone) :-
    phone_type(Phone.type, PhoneType),
    format('  ~w #: ~w~n', [PhoneType, Phone.number]).

phone_type('MOBILE', 'Mobile phone').
phone_type('HOME', 'Home phone').
phone_type('WORK', 'Work phone').

%! message_file(+LocalPath:atom, -Path:atom) is det.
% Given a =LocalPath=, determine its path relative to the current module.
message_file(LocalPath, Path) :-
    source_file(addressbook:message_file(_,_), MyFile),
    file_directory_name(MyFile, MyDir),
    atomic_list_concat([MyDir, LocalPath], /, Path).

end_of_file.
