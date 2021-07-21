/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2015, VU University Amsterdam
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

:- module(websocket,
          [ http_open_websocket/3,      % +URL, -WebSocket, +Options
            http_upgrade_to_websocket/3, % :Goal, +Options, +Request
            ws_send/2,                  % +WebSocket, +Message
            ws_receive/2,               % +WebSocket, -Message
            ws_receive/3,               % +WebSocket, -Message, +Options
            ws_close/3,                 % +WebSocket, +Code, +Message
                                        % Low level interface
            ws_open/3,                  % +Stream, -WebSocket, +Options
            ws_property/2               % +WebSocket, ?Property
          ]).
:- autoload(library(base64),[base64//1]).
:- autoload(library(debug),[debug/3]).
:- autoload(library(error),
	    [permission_error/3,must_be/2,type_error/2,domain_error/2]).
:- autoload(library(lists),[member/2]).
:- autoload(library(option),[select_option/3,option/2,option/3]).
:- autoload(library(sha),[sha_hash/3]).
:- autoload(library(http/http_dispatch),[http_switch_protocol/2]).
:- autoload(library(http/http_open),[http_open/3]).
:- autoload(library(http/json),[json_write_dict/2,json_read_dict/3]).

:- meta_predicate
    http_upgrade_to_websocket(1, +, +).

:- predicate_options(http_open_websocket/3, 3,
                     [ subprotocols(list(atom)),
                       pass_to(http_open:http_open/3, 3)
                     ]).
:- predicate_options(http_upgrade_to_websocket/3, 2,
                     [ guarded(boolean),
                       subprotocols(list(atom))
                     ]).

:- use_foreign_library(foreign(websocket)).

/** <module> WebSocket support

WebSocket is a lightweight message oriented   protocol  on top of TCP/IP
streams. It is typically used as an   _upgrade_ of an HTTP connection to
provide bi-directional communication, but can also  be used in isolation
over arbitrary (Prolog) streams.

The SWI-Prolog interface is based on _streams_ and provides ws_open/3 to
create a _websocket stream_ from any   Prolog stream. Typically, both an
input and output stream are wrapped  and   then  combined  into a single
object using stream_pair/3.

The high-level interface provides http_upgrade_to_websocket/3 to realise
a   websocket   inside   the    HTTP     server    infrastructure    and
http_open_websocket/3 as a layer over http_open/3   to  realise a client
connection. After establishing a connection,  ws_send/2 and ws_receive/2
can be used to send and receive   messages.  The predicate ws_close/3 is
provided to perform the closing  handshake   and  dispose  of the stream
objects.

@see    RFC 6455, http://tools.ietf.org/html/rfc6455
@tbd    Deal with protocol extensions.
*/



                 /*******************************
                 *         HTTP SUPPORT         *
                 *******************************/

%!  http_open_websocket(+URL, -WebSocket, +Options) is det.
%
%   Establish  a  client  websocket  connection.  This  predicate  calls
%   http_open/3  with  additional  headers  to   negotiate  a  websocket
%   connection. In addition to the options processed by http_open/3, the
%   following options are recognised:
%
%     - subprotocols(+List)
%     List of subprotocols that are acceptable. The selected
%     protocol is available as ws_property(WebSocket,
%     subprotocol(Protocol).
%
%   Note that clients often provide an  `Origin` header and some servers
%   require this field. See  RFC  6455   for  details.  By  default this
%   predicate  does  not  set  `Origin`.  It    may  be  set  using  the
%   `request_header` option of http_open/3, e.g. by  passing this in the
%   Options list:
%
%       request_header('Origin' = 'https://www.swi-prolog.org')
%
%   The   following   example   exchanges    a     message    with   the
%   html5rocks.websocket.org echo service:
%
%     ```
%     ?- URL = 'ws://html5rocks.websocket.org/echo',
%        http_open_websocket(URL, WS, []),
%        ws_send(WS, text('Hello World!')),
%        ws_receive(WS, Reply),
%        ws_close(WS, 1000, "Goodbye").
%     URL = 'ws://html5rocks.websocket.org/echo',
%     WS = <stream>(0xe4a440,0xe4a610),
%     Reply = websocket{data:"Hello World!", opcode:text}.
%     ```
%
%   @arg WebSocket is a stream pair (see stream_pair/3)

http_open_websocket(URL, WebSocket, Options) :-
    phrase(base64(`___SWI-Prolog___`), Bytes),
    string_codes(Key, Bytes),
    add_subprotocols(Options, Options1),
    http_open(URL, In,
              [ status_code(Status),
                output(Out),
                header(sec_websocket_protocol, Selected),
                header(sec_websocket_accept, AcceptedKey),
                connection('Keep-alive, Upgrade'),
                request_header('Upgrade' = websocket),
                request_header('Sec-WebSocket-Key' = Key),
                request_header('Sec-WebSocket-Version' = 13)
              | Options1
              ]),
    (   Status == 101,
        sec_websocket_accept(_{key:Key}, AcceptedKey)
    ->  ws_client_options(Selected, WsOptions),
        stream_pair(In,  Read, Write),      % Old API: In and Out
        stream_pair(Out, Read, Write),      % New API: In == Out (= pair)
        ws_open(Read,  WsIn,  WsOptions),
        ws_open(Write, WsOut, WsOptions),
        stream_pair(WebSocket, WsIn, WsOut)
    ;   close(Out),
        close(In),
        permission_error(open, websocket, URL)
    ).

ws_client_options('',          [mode(client)]) :- !.
ws_client_options(null,        [mode(client)]) :- !.
ws_client_options(Subprotocol, [mode(client), subprotocol(Subprotocol)]).

add_subprotocols(OptionsIn, OptionsOut) :-
    select_option(subprotocols(Subprotocols), OptionsIn, Options1),
    !,
    must_be(list(atom), Subprotocols),
    atomic_list_concat(Subprotocols, ', ', Value),
    OptionsOut = [ request_header('Sec-WebSocket-Protocol' = Value)
                 | Options1
                 ].
add_subprotocols(Options, Options).


%!  http_upgrade_to_websocket(:Goal, +Options, +Request)
%
%   Create a websocket connection running call(Goal, WebSocket),
%   where WebSocket is a socket-pair.  Options:
%
%     * guarded(+Boolean)
%     If =true= (default), guard the execution of Goal and close
%     the websocket on both normal and abnormal termination of Goal.
%     If =false=, Goal itself is responsible for the created
%     websocket.  This can be used to create a single thread that
%     manages multiple websockets using I/O multiplexing.
%
%     * subprotocols(+List)
%     List of acceptable subprotocols.
%
%     * timeout(+TimeOut)
%     Timeout to apply to the input stream.  Default is =infinite=.
%
%   Note that the Request argument is  the last for cooperation with
%   http_handler/3. A simple _echo_ server that   can be accessed at
%   =/ws/= can be implemented as:
%
%     ==
%     :- use_module(library(http/websocket)).
%     :- use_module(library(http/thread_httpd)).
%     :- use_module(library(http/http_dispatch)).
%
%     :- http_handler(root(ws),
%                     http_upgrade_to_websocket(echo, []),
%                     [spawn([])]).
%
%     echo(WebSocket) :-
%         ws_receive(WebSocket, Message),
%         (   Message.opcode == close
%         ->  true
%         ;   ws_send(WebSocket, Message),
%             echo(WebSocket)
%         ).
%     ==
%
%   @see http_switch_protocol/2.
%   @throws switching_protocols(Goal, Options).  The recovery from
%           this exception causes the HTTP infrastructure to call
%           call(Goal, WebSocket).

http_upgrade_to_websocket(Goal, Options, Request) :-
    request_websocket_info(Request, Info),
    debug(websocket(open), 'Websocket request: ~p', [Info]),
    sec_websocket_accept(Info, AcceptKey),
    choose_subprotocol(Info, Options, SubProtocol, ExtraHeaders),
    debug(websocket(open), 'Subprotocol: ~p', [SubProtocol]),
    http_switch_protocol(
        open_websocket(Goal, SubProtocol, Options),
        [ header([ upgrade(websocket),
                   connection('Upgrade'),
                   sec_websocket_accept(AcceptKey)
                 | ExtraHeaders
                 ])
        ]).

choose_subprotocol(Info, Options, SubProtocol, ExtraHeaders) :-
    HdrValue = Info.get(subprotocols),
    option(subprotocols(ServerProtocols), Options),
    split_string(HdrValue, ",", " ", RequestProtocols),
    member(Protocol, RequestProtocols),
    member(SubProtocol, ServerProtocols),
    atom_string(SubProtocol, Protocol),
    !,
    ExtraHeaders = [ 'Sec-WebSocket-Protocol'(SubProtocol) ].
choose_subprotocol(_, _, null, []).

open_websocket(Goal, SubProtocol, Options, HTTPIn, HTTPOut) :-
    option(timeout(TimeOut), Options, infinite),
    set_stream(HTTPIn, timeout(TimeOut)),
    WsOptions = [mode(server), subprotocol(SubProtocol)],
    ws_open(HTTPIn, WsIn, WsOptions),
    ws_open(HTTPOut, WsOut, WsOptions),
    stream_pair(WebSocket, WsIn, WsOut),
    (   option(guarded(true), Options, true)
    ->  guard_websocket_server(Goal, WebSocket)
    ;   call(Goal, WebSocket)
    ).

guard_websocket_server(Goal, WebSocket) :-
    (   catch(call(Goal, WebSocket), E, true)
    ->  (   var(E)
        ->  Msg = bye, Code = 1000
        ;   message_to_string(E, Msg),
            Code = 1011
        )
    ;   Msg = "goal failed", Code = 1011
    ),
    catch(ws_close(WebSocket, Code, Msg), Error,
          print_message(error, Error)).


request_websocket_info(Request, Info) :-
    option(upgrade(Websocket), Request),
    downcase_atom(Websocket, websocket),
    option(connection(Connection), Request),
    connection_contains_upgrade(Connection),
    option(sec_websocket_key(ClientKey), Request),
    option(sec_websocket_version(Version), Request),
    Info0 = _{key:ClientKey, version:Version},
    add_option(origin,                   Request, origin,       Info0, Info1),
    add_option(sec_websocket_protocol,   Request, subprotocols, Info1, Info2),
    add_option(sec_websocket_extensions, Request, extensions,   Info2, Info).

connection_contains_upgrade(Connection) :-
    split_string(Connection, ",", " ", Tokens),
    member(Token, Tokens),
    string_lower(Token, "upgrade"),
    !.

add_option(OptionName, Request, Key, Dict0, Dict) :-
    Option =.. [OptionName,Value],
    option(Option, Request),
    !,
    Dict = Dict0.put(Key,Value).
add_option(_, _, _, Dict, Dict).

%!  sec_websocket_accept(+Info, -AcceptKey) is det.
%
%   Compute the accept key as per 4.2.2., point 5.4

sec_websocket_accept(Info, AcceptKey) :-
    string_concat(Info.key, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11", Str),
    sha_hash(Str, Hash, [ algorithm(sha1) ]),
    phrase(base64(Hash), Encoded),
    string_codes(AcceptKey, Encoded).


                 /*******************************
                 *     HIGH LEVEL INTERFACE     *
                 *******************************/

%!  ws_send(+WebSocket, +Message) is det.
%
%   Send a message over a websocket. The following terms are allowed
%   for Message:
%
%     - text(+Text)
%       Send a text message.  Text is serialized using write/1.
%     - binary(+Content)
%       As text(+Text), but all character codes produced by Content
%       must be in the range [0..255].  Typically, Content will be
%       an atom or string holding binary data.
%     - prolog(+Term)
%       Send a Prolog term as a text message. Text is serialized
%       using write_canonical/1.
%     - json(+JSON)
%       Send the Prolog representation of a JSON term using
%       json_write_dict/2.
%     - string(+Text)
%       Same as text(+Text), provided for consistency.
%     - close(+Code, +Text)
%       Send a close message.  Code is 1000 for normal close.  See
%       websocket documentation for other values.
%     - Dict
%       A dict that minimally contains an =opcode= key.  Other keys
%       used are:
%
%       - format:Format
%         Serialization format used for Message.data. Format is
%         one of =string=, =prolog= or =json=.  See ws_receive/3.
%
%       - data:Term
%         If this key is present, it is serialized according
%         to Message.format.  Otherwise it is serialized using
%         write/1, which implies that string and atoms are just
%         sent verbatim.
%
%   Note that ws_start_message/3 does not unlock the stream. This is
%   done by ws_send/1. This implies that   multiple  threads can use
%   ws_send/2 and the messages are properly serialized.
%
%   @tbd    Provide serialization details using options.

ws_send(WsStream, Message) :-
    message_opcode(Message, OpCode),
    setup_call_cleanup(
        ws_start_message(WsStream, OpCode, 0),
        write_message_data(WsStream, Message),
        ws_send(WsStream)).

message_opcode(Message, OpCode) :-
    is_dict(Message),
    !,
    to_opcode(Message.opcode, OpCode).
message_opcode(Message, OpCode) :-
    functor(Message, Name, _),
    (   text_functor(Name)
    ->  to_opcode(text, OpCode)
    ;   to_opcode(Name, OpCode)
    ).

text_functor(json).
text_functor(string).
text_functor(prolog).

write_message_data(Stream, Message) :-
    is_dict(Message),
    !,
    (   _{code:Code, data:Data} :< Message
    ->  write_message_data(Stream, close(Code, Data))
    ;   _{format:prolog, data:Data} :< Message
    ->  format(Stream, '~k .~n', [Data])
    ;   _{format:json, data:Data} :< Message
    ->  json_write_dict(Stream, Data)
    ;   _{data:Data} :< Message
    ->  format(Stream, '~w', Data)
    ;   true
    ).
write_message_data(Stream, Message) :-
    functor(Message, Format, 1),
    !,
    arg(1, Message, Data),
    (   text_functor(Format)
    ->  write_text_message(Format, Stream, Data)
    ;   format(Stream, '~w', [Data])
    ).
write_message_data(_, Message) :-
    atom(Message),
    !.
write_message_data(Stream, close(Code, Data)) :-
    !,
    High is (Code >> 8) /\ 0xff,
    Low  is Code /\ 0xff,
    put_byte(Stream, High),
    put_byte(Stream, Low),
    stream_pair(Stream, _, Out),
    set_stream(Out, encoding(utf8)),
    format(Stream, '~w', [Data]).
write_message_data(_, Message) :-
    type_error(websocket_message, Message).

write_text_message(json, Stream, Data) :-
    !,
    json_write_dict(Stream, Data).
write_text_message(prolog, Stream, Data) :-
    !,
    format(Stream, '~k .', [Data]).
write_text_message(_, Stream, Data) :-
    format(Stream, '~w', [Data]).



%!  ws_receive(+WebSocket, -Message:dict) is det.
%!  ws_receive(+WebSocket, -Message:dict, +Options) is det.
%
%   Receive the next message  from  WebSocket.   Message  is  a dict
%   containing the following keys:
%
%     - opcode:OpCode
%       OpCode of the message.  This is an atom for known opcodes
%       and an integer for unknown ones.  If the peer closed the
%       stream, OpCode is bound to =close= and data to the atom
%       =end_of_file=.
%     - data:String
%       The data, represented as a string.  This field is always
%       present.  String is the empty string if there is no data
%       in the message.
%     - rsv:RSV
%       Present if the WebSocket RSV header is not 0. RSV is an
%       integer in the range [1..7].
%
%   If =ping= message is received and   WebSocket  is a stream pair,
%   ws_receive/1 replies with a  =pong=  and   waits  for  the  next
%   message.
%
%   The predicate ws_receive/3 processes the following options:
%
%     - format(+Format)
%     Defines how _text_ messages are parsed.  Format is one of
%       - string
%       Data is returned as a Prolog string (default)
%       - json
%       Data is parsed using json_read_dict/3, which also receives
%       Options.
%       - prolog
%       Data is parsed using read_term/3, which also receives
%       Options.
%
%   @tbd    Add a hook to allow for more data formats?

ws_receive(WsStream, Message) :-
    ws_receive(WsStream, Message, []).

ws_receive(WsStream, Message, Options) :-
    ws_read_header(WsStream, Code, RSV),
    debug(websocket, 'ws_receive(~p): OpCode=~w, RSV=~w',
          [WsStream, Code, RSV]),
    (   Code == end_of_file
    ->  Message = websocket{opcode:close, data:end_of_file}
    ;   (   ws_opcode(OpCode, Code)
        ->  true
        ;   OpCode = Code
        ),
        read_data(OpCode, WsStream, Data, Options),
        (   OpCode == ping,
            reply_pong(WsStream, Data.data)
        ->  ws_receive(WsStream, Message, Options)
        ;   (   RSV == 0
            ->  Message = Data
            ;   Message = Data.put(rsv, RSV)
            )
        )
    ),
    debug(websocket, 'ws_receive(~p) --> ~p', [WsStream, Message]).

read_data(close, WsStream,
          websocket{opcode:close, code:Code, format:string, data:Data}, _Options) :-
    !,
    get_byte(WsStream, High),
    (   High == -1
    ->  Code = 1000,
        Data = ""
    ;   get_byte(WsStream, Low),
        Code is High<<8 \/ Low,
        stream_pair(WsStream, In, _),
        set_stream(In, encoding(utf8)),
        read_string(WsStream, _Len, Data)
    ).
read_data(text, WsStream, Data, Options) :-
    !,
    option(format(Format), Options, string),
    read_text_data(Format, WsStream, Data, Options).
read_data(OpCode, WsStream, websocket{opcode:OpCode, format:string, data:Data}, _Options) :-
    read_string(WsStream, _Len, Data).

%!  read_text_data(+Format, +WsStream, -Dict, +Options) is det.
%
%   Read a websocket message into   a  dict websocket{opcode:OpCode,
%   data:Data}, where Data is parsed according to Format.

read_text_data(string, WsStream,
          websocket{opcode:text, format:string, data:Data}, _Options) :-
    !,
    read_string(WsStream, _Len, Data).
read_text_data(json, WsStream,
          websocket{opcode:text, format:json,   data:Data}, Options) :-
    !,
    json_read_dict(WsStream, Data, Options).
read_text_data(prolog, WsStream,
          websocket{opcode:text, format:prolog, data:Data}, Options) :-
    !,
    read_term(WsStream, Data, Options).
read_text_data(Format, _, _, _) :-
    domain_error(format, Format).

reply_pong(WebSocket, Data) :-
    stream_pair(WebSocket, _In, Out),
    is_stream(Out),
    ws_send(Out, pong(Data)).


%!  ws_close(+WebSocket:stream_pair, +Code, +Data) is det.
%
%   Close a WebSocket connection by sending a =close= message if
%   this was not already sent and wait for the close reply.
%
%   @arg    Code is the numerical code indicating the close status.
%           This is 16-bit integer.  The codes are defined in
%           section _|7.4.1. Defined Status Codes|_ of RFC6455.
%           Notably, 1000 indicates a normal closure.
%   @arg    Data is currently interpreted as text.
%   @error  websocket_error(unexpected_message, Reply) if
%           the other side did not send a close message in reply.

ws_close(WebSocket, Code, Data) :-
    setup_call_cleanup(
        true,
        ws_close_(WebSocket, Code, Data),
        close(WebSocket)).

ws_close_(WebSocket, Code, Data) :-
    stream_pair(WebSocket, In, Out),
    (   (   var(Out)
        ;   ws_property(Out, status, closed)
        )
    ->  debug(websocket(close),
              'Output stream of ~p already closed', [WebSocket])
    ;   ws_send(WebSocket, close(Code, Data)),
        close(Out),
        debug(websocket(close), '~p: closed output', [WebSocket]),
        (   (   var(In)
            ;   ws_property(In, status, closed)
            )
        ->  debug(websocket(close),
                  'Input stream of ~p already closed', [WebSocket])
        ;   ws_receive(WebSocket, Reply),
            (   Reply.opcode == close
            ->  debug(websocket(close), '~p: close confirmed', [WebSocket])
            ;   throw(error(websocket_error(unexpected_message, Reply), _))
            )
        )
    ).


%!  ws_open(+Stream, -WSStream, +Options) is det.
%
%   Turn a raw TCP/IP (or any other  binary stream) into a websocket
%   stream. Stream can be an input stream, output stream or a stream
%   pair. Options includes
%
%     * mode(+Mode)
%     One of =server= or =client=.  If =client=, messages are sent
%     as _masked_.
%
%     * buffer_size(+Count)
%     Send partial messages for each Count bytes or when flushing
%     the output. The default is to buffer the entire message before
%     it is sent.
%
%     * close_parent(+Boolean)
%     If =true= (default), closing WSStream also closes Stream.
%
%     * subprotocol(+Protocol)
%     Set the subprotocol property of WsStream.  This value can be
%     retrieved using ws_property/2.  Protocol is an atom.  See
%     also the =subprotocols= option of http_open_websocket/3 and
%     http_upgrade_to_websocket/3.
%
%   A typical sequence to turn a pair of streams into a WebSocket is
%   here:
%
%     ==
%         ...,
%         Options = [mode(server), subprotocol(chat)],
%         ws_open(Input, WsInput, Options),
%         ws_open(Output, WsOutput, Options),
%         stream_pair(WebSocket, WsInput, WsOutput).
%     ==

%!  ws_start_message(+WSStream, +OpCode) is det.
%!  ws_start_message(+WSStream, +OpCode, +RSV) is det.
%
%   Prepare for sending a new  message.   OpCode  is  one of =text=,
%   =binary=,  =close=,  =ping=  or  =pong=.  RSV  is  reserved  for
%   extensions. After this call, the application usually writes data
%   to  WSStream  and  uses  ws_send/1   to  complete  the  message.
%   Depending on OpCode, the stream  is   switched  to _binary_ (for
%   OpCode is =binary=) or _text_ using   =utf8= encoding (all other
%   OpCode values). For example,  to  a   JSON  message  can be send
%   using:
%
%     ==
%     ws_send_json(WSStream, JSON) :-
%        ws_start_message(WSStream, text),
%        json_write(WSStream, JSON),
%        ws_send(WSStream).
%     ==

%!  ws_send(+WSStream) is det.
%
%   Complete and send the WebSocket message.   If  the OpCode of the
%   message is =close=, close the stream.

%!  ws_read_header(+WSStream, -OpCode, -RSV) is det.
%
%   Read the header of the WebSocket  next message. After this call,
%   WSStream is switched to  the   appropriate  encoding and reading
%   from the stream will  signal  end-of-file   at  the  end  of the
%   message.  Note  that  this  end-of-file  does  *not*  invalidate
%   WSStream.  Reading may perform various tasks on the background:
%
%     - If the message has _Fin_ is =false=, it will wait for an
%       additional message.
%     - If a =ping= is received, it will reply with a =pong= on the
%       matching output stream.
%     - If a =pong= is received, it will be ignored.
%     - If a =close= is received and a partial message is read,
%       it generates an exception (TBD: which?).  If no partial
%       message is received, it unified OpCode with =close= and
%       replies with a =close= message.
%
%   If not all data has been read  for the previous message, it will
%   first read the remainder of the  message. This input is silently
%   discarded. This allows for  trailing   white  space after proper
%   text messages such as JSON, Prolog or XML terms. For example, to
%   read a JSON message, use:
%
%     ==
%     ws_read_json(WSStream, JSON) :-
%         ws_read_header(WSStream, OpCode, RSV),
%         (   OpCode == text,
%             RSV == 0
%         ->  json_read(WSStream, JSON)
%         ;   OpCode == close
%         ->  JSON = end_of_file
%         ).
%     ==

%!  ws_property(+WebSocket, ?Property) is nondet.
%
%   True if Property is  a   property  WebSocket. Defined properties
%   are:
%
%     * subprotocol(Protocol)
%     Protocol is the negotiated subprotocol. This is typically set
%     as a property of the websocket by ws_open/3.

ws_property(WebSocket, Property) :-
    ws_property_(Property, WebSocket).

ws_property_(subprotocol(Protocol), WebSocket) :-
    ws_property(WebSocket, subprotocol, Protocol).

%!  to_opcode(+Spec, -OpCode:int) is det.
%
%   Convert a specification of an opcode into the numeric opcode.

to_opcode(In, Code) :-
    integer(In),
    !,
    must_be(between(0, 15), In),
    Code = In.
to_opcode(Name, Code) :-
    must_be(atom, Name),
    (   ws_opcode(Name, Code)
    ->  true
    ;   domain_error(ws_opcode, Name)
    ).

%!  ws_opcode(?Name, ?Code)
%
%   Define symbolic names for the WebSocket opcodes.

ws_opcode(continuation, 0).
ws_opcode(text,         1).
ws_opcode(binary,       2).
ws_opcode(close,        8).
ws_opcode(ping,         9).
ws_opcode(pong,         10).


%!  ws_mask(-Mask)
%
%   Produce a good random number of the mask of a client message.

:- public ws_mask/1.

ws_mask(Mask) :-
    Mask is 1+random(1<<32-1).
