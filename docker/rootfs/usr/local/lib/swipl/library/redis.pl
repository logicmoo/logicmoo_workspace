/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Sean Charles
    E-mail:        jan@swi-prolog.org and <sean at objitsu dot com>
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013-2022, Sean Charles
                              SWI-Prolog Solutions b.v.
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

    NOTE

    The original code was subject to the MIT licence and written by
    Sean Charles.  Re-licenced to standard SWI-Prolog BSD-2 with
    permission from Sean Charles.
*/

:- module(redis,
          [ redis_server/3,             % +Alias, +Address, +Options
            redis_connect/1,            % -Connection
            redis_connect/3,            % -Connection, +Host, +Port
            redis_disconnect/1,         % +Connection
            redis_disconnect/2,         % +Connection, +Options
                                        % Queries
            redis/1,                    % +Request
            redis/2,                    % +Connection, +Request
            redis/3,                    % +Connection, +Request, -Reply
                                        % High level queries
            redis_get_list/3,           % +Redis, +Key, -List
            redis_get_list/4,           % +Redis, +Key, +ChunkSize, -List
            redis_set_list/3,           % +Redis, +Key, +List
            redis_get_hash/3,           % +Redis, +Key, -Data:dict
            redis_set_hash/3,           % +Redis, +Key, +Data:dict
            redis_scan/3,               % +Redis, -LazyList, +Options
            redis_sscan/4,              % +Redis, +Set, -LazyList, +Options
            redis_hscan/4,              % +Redis, +Hash, -LazyList, +Options
            redis_zscan/4,              % +Redis, +Set, -LazyList, +Options
                                        % Publish/Subscribe
            redis_subscribe/4,          % +Redis, +Channels, -Id, +Options
            redis_subscribe/2,          % +Id, +Channels
            redis_unsubscribe/2,        % +Id, +Channels
            redis_current_subscription/2, % ?Id,?Channels
            redis_write/2,              % +Redis, +Command
            redis_read/2,               % +Redis, -Reply
                                        % Building blocks
            redis_array_dict/3,         % ?Array, ?Tag, ?Dict
                                        % Admin stuff
            redis_property/2,           % +Reply, ?Property
            redis_current_command/2,    % +Redis,?Command
            redis_current_command/3     % +Redis, +Command, -Properties
          ]).
:- autoload(library(socket), [tcp_connect/3]).
:- autoload(library(apply), [maplist/2, convlist/3, maplist/3, maplist/5]).
:- autoload(library(broadcast), [broadcast/1]).
:- autoload(library(error),
            [ must_be/2,
	      type_error/2,
              instantiation_error/1,
              uninstantiation_error/1,
              existence_error/2,
              existence_error/3
            ]).
:- autoload(library(lazy_lists), [lazy_list/2]).
:- autoload(library(lists), [append/3, member/2]).
:- autoload(library(option), [merge_options/3, option/2,
			      option/3, select_option/4]).
:- autoload(library(pairs), [group_pairs_by_key/2]).
:- autoload(library(time), [call_with_time_limit/2]).
:- use_module(library(debug), [debug/3, assertion/1]).
:- use_module(library(settings), [setting/4, setting/2]).
:- if(exists_source(library(ssl))).
:- autoload(library(ssl), [ssl_context/3, ssl_negotiate/5]).
:- endif.

:- use_foreign_library(foreign(redis4pl)).

:- setting(max_retry_count, nonneg, 8640, % one day
           "Max number of retries").
:- setting(max_retry_wait, number, 10,
           "Max time to wait between recovery attempts").
:- setting(sentinel_timeout, number, 0.2,
	   "Time to wait for a sentinel").

:- predicate_options(redis_server/3, 3,
                     [ pass_to(redis:redis_connect/3, 3)
                     ]).
:- predicate_options(redis_connect/3, 3,
                     [ reconnect(boolean),
                       user(atom),
                       password(atomic),
                       version(between(2,3))
                     ]).
:- predicate_options(redis_disconnect/2, 2,
                     [ force(boolean)
                     ]).
:- predicate_options(redis_scan/3, 3,
                     [ match(atomic),
                       count(nonneg),
                       type(atom)
                     ]).
% Actually not passing, but the same
:- predicate_options(redis_sscan/4, 4, [pass_to(redis:redis_scan/3, 3)]).
:- predicate_options(redis_hscan/4, 4, [pass_to(redis:redis_scan/3, 3)]).
:- predicate_options(redis_zscan/4, 4, [pass_to(redis:redis_scan/3, 3)]).


/** <module> Redis client

This library is a client  to   [Redis](https://redis.io),  a popular key
value store to  deal  with  caching   and  communication  between  micro
services.

In the typical use case we register  the   details  of one or more Redis
servers using redis_server/3. Subsequenly, redis/2-3   is  used to issue
commands on the server.  For example:

```
?- redis_server(default, redis:6379, [password("secret")]).
?- redis(default, set(user, "Bob")).
?- redis(default, get(user), User).
User = "Bob"
```
*/

:- dynamic server/3.

:- dynamic ( connection/2,              % ServerName, Stream
	     sentinel/2			% Pool, Address
           ) as volatile.

%!  redis_server(+ServerName, +Address, +Options) is det.
%
%   Register a redis server without  connecting   to  it. The ServerName
%   acts as a lazy connection alias.  Initially the ServerName `default`
%   points at `localhost:6379` with no   connect  options. The `default`
%   server is used for redis/1 and redis/2 and may be changed using this
%   predicate.  Options are described with redis_connect/3.
%
%   Connections  established  this  way  are  by  default  automatically
%   reconnected if the connection  is  lost   for  some  reason unless a
%   reconnect(false) option is specified.

redis_server(Alias, Address, Options) :-
    must_be(ground, Alias),
    retractall(server(Alias, _, _)),
    asserta(server(Alias, Address, Options)).

server(default, localhost:6379, []).

%!  redis_connect(-Connection) is det.
%!  redis_connect(+Address, -Connection, +Options) is det.
%!  redis_connect(-Connection, +Host, +Port) is det.
%
%   Connect to a redis server. The  main mode is redis_connect(+Address,
%   -Connection,   +Options).   redis_connect/1   is     equivalent   to
%   redis_connect(localhost:6379, Connection, []).  Options:
%
%     - reconnect(+Boolean)
%       If `true`, try to reconnect to the service when the connection
%       seems lost.  Default is `true` for connections specified using
%       redis_server/3 and `false` for explictly opened connections.
%     - user(+User)
%       If version(3) and password(Password) are specified, these
%       are used to authenticate using the `HELLO` command.
%     - password(+Password)
%       Authenticate using Password
%     - version(+Version)
%       Specify the connection protocol version.  Initially this is
%       version 2.  Redis 6 also supports version 3.  When specified
%       as `3`, the `HELLO` command is used to upgrade the protocol.
%     - tls(true)
%       When specified, initiate a TLS connection.  If this option is
%       specified we must also specify the `cacert`, `key` and `cert`
%       options.
%     - cacert(+File)
%       CA Certificate file to verify with.
%     - cert(+File)
%       Client certificate to authenticate with.
%     - key(+File)
%       Private key file to authenticate with.
%     - sentinels(+ListOfAddresses)
%       Used together with an Address of the form sentinel(MasterName)
%       to enable contacting a network of Redis servers guarded by a
%       sentinel network.
%     - sentinel_user(+User)
%     - sentinel_password(+Password)
%       Authentication information for the senitels.  When omitted we
%       try to connect withour authentication.
%
%   Instead of using these predicates, redis/2  and redis/3 are normally
%   used with a _server name_  argument registered using redis_server/3.
%   These  predicates  are  meant  for   creating  a  temporary  paralel
%   connection or using a connection with a _blocking_ call.
%
%   @compat   redis_connect(-Connection,   +Host,     +Port)    provides
%   compatibility to the original GNU-Prolog interface and is equivalent
%   to redis_connect(Host:Port, Connection, []).
%
%   @arg Address is a term Host:Port, unix(File) or the name of a server
%   registered  using  redis_server/3.  The  latter   realises  a  _new_
%   connection that is typically used for   blocking redis commands such
%   as listening for published messages, waiting on a list or stream.

redis_connect(Conn) :-
    redis_connect(default, Conn, []).

redis_connect(Conn, Host, Port) :-
    var(Conn),
    ground(Host), ground(Port),
    !,                                  % GNU-Prolog compatibility
    redis_connect(Host:Port, Conn, []).
redis_connect(Server, Conn, Options) :-
    atom(Server),
    !,
    (   server(Server, Address, DefaultOptions)
    ->  merge_options(Options, DefaultOptions, Options2),
        do_connect(Server, Address, Conn, [address(Address)|Options2])
    ;   existence_error(redis_server, Server)
    ).
redis_connect(Address, Conn, Options) :-
    do_connect(Address, Address, Conn, [address(Address)|Options]).

%!  do_connect(+Id, +Address, -Conn, +Options)
%
%   Open the connection.  A connection is a compound term of the shape
%
%       redis_connection(Id, Stream, Failures, Options)

do_connect(Id, sentinel(Pool), Conn, Options) =>
    sentinel_master(Id, Pool, Conn, Options).
do_connect(Id, Address0, Conn, Options) =>
    tcp_address(Address0, Address),
    tcp_connect(Address, Stream0, Options),
    tls_upgrade(Address, Stream0, Stream, Options),
    Conn = redis_connection(Id, Stream, 0, Options),
    hello(Conn, Options).

tcp_address(unix(Path), Path) :-
    !.                                  % Using an atom is ambiguous
tcp_address(Address, Address).

%!  tls_upgrade(+Address, +Raw, -Stream, +Options) is det.
%
%   Upgrade to a TLS connection when tls(true) is specified.

:- if(current_predicate(ssl_context/3)).
tls_upgrade(Host:_Port, Raw, Stream, Options) :-
    option(tls(true), Options),
    !,
    must_have_option(cacert(CacertFile), Options),
    must_have_option(key(KeyFile), Options),
    must_have_option(cert(CertFile), Options),
    ssl_context(client, SSL,
		[ host(Host),
		  certificate_file(CertFile),
		  key_file(KeyFile),
		  cacerts([file(CacertFile)]),
		  cert_verify_hook(tls_verify),
		  close_parent(true)
		]),
    stream_pair(Raw, RawRead, RawWrite),
    ssl_negotiate(SSL, RawRead, RawWrite, Read, Write),
    stream_pair(Stream, Read, Write).
:- endif.
tls_upgrade(_, Stream, Stream, _).

:- if(current_predicate(ssl_context/3)).

%!  tls_verify(+SSL, +ProblemCert, +AllCerts, +FirstCert, +Status) is semidet.
%
%   Accept  or reject  the certificate  verification.  Similar  to the
%   Redis  command   line  client   (``redis-cli``),  we   accept  the
%   certificate as long as it is signed, not verifying the hostname.

:- public tls_verify/5.
tls_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, verified) :-
    !.
tls_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, hostname_mismatch) :-
    !.
tls_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, _Error) :-
    fail.

:- endif.

%!  sentinel_master(+ServerId, +SetinelPool, -Connection, +Options) is det.
%
%   Discover the master and connect to it.

sentinel_master(Id, Pool, Master, Options) :-
    must_have_option(sentinels(Sentinels), Options),
    sentinel_auth(Options, Options1),
    setting(sentinel_timeout, TMO),
    (   sentinel(Pool, Sentinel)
    ;   member(Sentinel, Sentinels)
    ),
    catch(call_with_time_limit(
	      TMO,
	      do_connect(Id, Sentinel, Conn,
			 [sentinel(true)|Options1])),
	  Error,
	  (print_message(warning, Error),fail)),
    !,
    debug(redis(sentinel), 'Connected to sentinel at ~p', [Sentinel]),
    call_cleanup(
	query_sentinel(Pool, Conn, Sentinel, MasterAddr),
	redis_disconnect(Conn)),
    debug(redis(sentinel), 'Sentinel claims master is at ~p', [MasterAddr]),
    do_connect(Id, MasterAddr, Master, Options),
    debug(redis(sentinel), 'Connected to claimed master', []),
    redis(Master, role, Role),
    (   Role = [master|_Slaves]
    ->  debug(redis(sentinel), 'Verified role at ~p', [MasterAddr])
    ;   redis_disconnect(Master),
	debug(redis(sentinel), '~p is not the master: ~p', [MasterAddr, Role]),
	sleep(TMO),
	sentinel_master(Id, Pool, Master, Options)
    ).

sentinel_auth(Options0, Options) :-
    option(sentinel_user(User), Options0),
    option(sentinel_password(Passwd), Options0),
    !,
    merge_options([user(User), password(Passwd)], Options0, Options).
sentinel_auth(Options0, Options) :-
    select_option(password(_), Options0, Options, _).


query_sentinel(Pool, Conn, Sentinel, Host:Port) :-
    redis(Conn, sentinel('get-master-addr-by-name', Pool), MasterData),
    MasterData = [Host,Port],
    redis(Conn, sentinel(sentinels, Pool), Peers),
    transaction(update_known_sentinels(Pool, Sentinel, Peers)).

update_known_sentinels(Pool, Sentinel, Peers) :-
    retractall(sentinel(Pool, _)),
    maplist(update_peer_sentinel(Pool), Peers),
    asserta(sentinel(Pool, Sentinel)).

update_peer_sentinel(Pool, Attrs),
  memberchk(ip-Host, Attrs),
  memberchk(port-Port, Attrs) =>
    asserta(sentinel(Pool, Host:Port)).

must_have_option(Opt, Options) :-
    option(Opt, Options),
    !.
must_have_option(Opt, Options) :-
    existence_error(option, Opt, Options).

%!  hello(+Connection, +Option)
%
%   Initialize the connection. This is  used   to  upgrade  to the RESP3
%   protocol and/or to authenticate.

hello(Con, Options) :-
    option(version(V), Options),
    V >= 3,
    !,
    (   option(user(User), Options),
        option(password(Password), Options)
    ->  redis(Con, hello(3, auth, User, Password))
    ;   redis(Con, hello(3))
    ).
hello(Con, Options) :-
    option(password(Password), Options),
    !,
    redis(Con, auth(Password)).
hello(_, _).

%!  redis_stream(+Spec, --Stream, +DoConnect) is det.
%
%   Get the stream to a Redis server from  Spec. Spec is either the name
%   of       a       registered       server       or       a       term
%   redis_connection(Id,Stream,Failures,Options).  If  the    stream  is
%   disconnected it will be reconnected.

redis_stream(Var, S, _) :-
    (   var(Var)
    ->  !, instantiation_error(Var)
    ;   nonvar(S)
    ->  !, uninstantiation_error(S)
    ).
redis_stream(ServerName, S, Connect) :-
    atom(ServerName),
    !,
    (   connection(ServerName, S0)
    ->  S = S0
    ;   Connect == true,
        server(ServerName, Address, Options)
    ->  redis_connect(Address, Connection, Options),
        redis_stream(Connection, S, false),
        asserta(connection(ServerName, S))
    ;   existence_error(redis_server, ServerName)
    ).
redis_stream(redis_connection(_,S0,_,_), S, _) :-
    S0 \== (-),
    !,
    S = S0.
redis_stream(Redis, S, _) :-
    Redis = redis_connection(Id,-,_,Options),
    option(address(Address), Options),
    do_connect(Id,Address,Redis2,Options),
    arg(2, Redis2, S0),
    nb_setarg(2, Redis, S0),
    S = S0.

has_redis_stream(Var, _) :-
    var(Var),
    !,
    instantiation_error(Var).
has_redis_stream(Alias, S) :-
    atom(Alias),
    !,
    connection(Alias, S).
has_redis_stream(redis_connection(_,S,_,_), S) :-
    S \== (-).


%!  redis_disconnect(+Connection) is det.
%!  redis_disconnect(+Connection, +Options) is det.
%
%   Disconnect from a redis server. The   second  form takes one option,
%   similar to close/2:
%
%     - force(Force)
%       When `true` (default `false`), do not raise any errors if
%       Connection does not exist or closing the connection raises
%       a network or I/O related exception.  This version is used
%       internally if a connection is in a broken state, either due
%       to a protocol error or a network issue.

redis_disconnect(Redis) :-
    redis_disconnect(Redis, []).

redis_disconnect(Redis, Options) :-
    option(force(true), Options),
    !,
    (   Redis = redis_connection(_Id, S, _, _Opts)
    ->  (   S == (-)
        ->  true
        ;   close(S, [force(true)]),
            nb_setarg(2, Redis, -)
        )
    ;   has_redis_stream(Redis, S)
    ->  close(S, [force(true)]),
        retractall(connection(_,S))
    ;   true
    ).
redis_disconnect(Redis, _Options) :-
    redis_stream(Redis, S, false),
    close(S),
    retractall(connection(_,S)).

%!  redis(+Connection, +Request) is semidet.
%
%   This predicate is overloaded to handle two types of requests. First,
%   it is a shorthand for `redis(Connection, Command, _)` and second, it
%   can be used to exploit  Redis   _pipelines_  and _transactions_. The
%   second form is acticated if Request is  a _list_. In that case, each
%   element of the list is either a term  `Command -> Reply` or a simple
%   `Command`. Semantically this represents a   sequence  of redis/3 and
%   redis/2 calls.  It differs in the following aspects:
%
%     - All commands are sent in one batch, after which all replies are
%       read.  This reduces the number of _round trips_ and typically
%       greatly improves performance.
%     - If the first command is `multi` and the last `exec`, the
%       commands are executed as a Redis _transaction_, i.e., they
%       are executed _atomically_.
%     - If one of the commands returns an error, the subsequent commands
%       __are still executed__.
%     - You can not use variables from commands earlier in the list for
%       commands later in the list as a result of the above execution
%       order.
%
%   Procedurally, the process takes the following steps:
%
%     1. Send all commands
%     2. Read all replies and push messages
%     3. Handle all callbacks from push messages
%     4. Check whether one of the replies is an error.  If so,
%        raise this error (subsequent errors are lost)
%     5. Bind all replies for the `Command -> Reply` terms.
%
%   Examples
%
%   ```
%   ?- redis(default,
%            [ lpush(li,1),
%              lpush(li,2),
%              lrange(li,0,-1) -> List
%            ]).
%   List = ["2", "1"].
%   ```

redis(Redis, PipeLine) :-
    is_list(PipeLine),
    !,
    redis_pipeline(Redis, PipeLine).
redis(Redis, Req) :-
    redis(Redis, Req, _).

%!  redis(+Connection, +Command, -Reply) is semidet.
%
%   Execute a redis Command on  Connnection.   Next,  bind  Reply to the
%   returned result. Command is a  callable   term  whose functor is the
%   name of the Redis command  and   whose  arguments  are translated to
%   Redis arguments according to the rules below.  Note that all text is
%   always represented using UTF-8 encoding.
%
%     - Atomic values are emitted verbatim
%     - A term A:B:... where all arguments are either atoms,
%       strings or integers (__no floats__) is translated into
%       a string `"A:B:..."`.  This is a common shorthand for
%       representing Redis keys.
%     - A term Term as prolog is emitted as "\u0000T\u0000" followed
%       by Term in canonical form.
%     - Any other term is emitted as write/1.
%
%   Reply is either a plain term (often a  variable) or a term `Value as
%   Type`. In the latter form,  `Type`   dictates  how  the Redis _bulk_
%   reply is translated to Prolog. The default equals to `auto`, i.e.,
%   as a number of the content satisfies the Prolog number syntax and
%   as an atom otherwise.
%
%     - status(Atom)
%       Returned if the server replies with ``+ Status``.  Atom
%       is the textual value of `Status` converted to lower case,
%       e.g., status(ok) or status(pong).
%     - `nil`
%       This atom is returned for a NIL/NULL value.  Note that if
%       the reply is only `nil`, redis/3 _fails_.  The `nil` value
%       may be embedded inside lists or maps.
%     - A number
%       Returned if the server replies an integer (":Int"), double
%       (",Num") or big integer ("(Num")
%     - A string
%       Returned on a _bulk_ reply.  Bulk replies are supposed to be
%       in UTF-8 encoding.  The the bulk reply starts with
%       "\u0000T\u0000" it is supposed to be a Prolog term.
%       Note that this intepretation means it is __not__ possible
%       to read arbitrary binary blobs.
%     - A list of replies.  A list may also contain `nil`.  If Reply
%       as a whole would be `nil` the call fails.
%     - A list of _pairs_.  This is returned for the redis version 3
%       protocol "%Map".  Both the key and value respect the same
%       rules as above.
%
%   Redis _bulk_ replies are translated depending  on the `as` `Type` as
%   explained above.
%
%     - string
%     - string(Encoding)
%       Create a SWI-Prolog string object interpreting the blob as
%       following Encoding. Encoding is a restricted set of SWI-Prolog's
%       encodings: `bytes` (`iso_latin_1`), `utf8` and `text` (the
%       current locale translation).
%     - atom
%     - atom(Encoding)
%       As above, producing an atom.
%     - codes
%     - codes(Encoding)
%       As above, producing a list of integers (Unicode code points)
%     - chars
%     - chars(Encoding)
%       As above, producing a list of one-character atoms.
%     - integer
%     - float
%     - rational
%     - number
%       Interpret the bytes as a string representing a number.  If
%       the string does not represent a number of the requested type
%       a type_error(Type, String) is raised.
%     - tagged_integer
%       Same as integer, but demands the value to be between the Prolog
%       flags `min_tagged_integer` and `max_tagged_integer`, allowing
%       the value to be used as a dict key.
%     - auto
%       Same as auto(atom, number)
%     - auto(AsText,AsNumber)
%       If the bulk string confirms the syntax of AsNumber, convert
%       the value to the requested numberical type.  Else convert
%       the value to text according to AsText.  This is similar to
%       the Prolog predicate name/2.
%     - dict_key
%       Alias for auto(atom,tagged_integer).  This allows the value
%       to be used as a key for a SWI-Prolog dict.
%     - pairs(AsKey, AsValue)
%       Convert a map or array of even length into pairs for which the
%       key satisfies AsKey and the value AsValue.  The `pairs` type
%       can also be applied to a Redis array.  In this case the array
%       length must be even.  This notably allows fetching a Redis
%       _hash_ as pairs using ``HGETALL`` using version 2 of the
%       Redis protocol.
%     - dict(AsKey, AsValue)
%       Similar to pairs(AsKey, AsValue), but convert the resulting
%       pair list into a SWI-Prolog dict.  AsKey must convert to a
%       valid dict key, i.e., an atom or tagged integer. See `dict_key`.
%     - dict(AsValue)
%       Shorthand for dict(dict_key, AsValue).
%
%   Here are some simple examples
%
%   ```
%   ?- redis(default, set(a, 42), X).
%   X = status("OK").
%   ?- redis(default, get(a), X).
%   X = "42".
%   ?- redis(default, get(a), X as integer).
%   X = 42.
%   ?- redis(default, get(a), X as float).
%   X = 42.0.
%   ?- redis(default, set(swipl:version, 8)).
%   true.
%   ?- redis(default, incr(swipl:version), X).
%   X = 9.
%   ```
%
%   @error redis_error(Code, String)

redis(Redis, Req, Out) :-
    out_val(Out, Val),
    redis1(Redis, Req, Out),
    Val \== nil.

out_val(Out, Val) :-
    (   nonvar(Out),
        Out = (Val as _)
    ->  true
    ;   Val = Out
    ).

redis1(Redis, Req, Out) :-
    Error = error(Formal, _),
    catch(redis2(Redis, Req, Out), Error, true),
    (   var(Formal)
    ->  true
    ;   recover(Error, Redis, redis1(Redis, Req, Out))
    ).

redis2(Redis, Req, Out) :-
    atom(Redis),
    !,
    redis_stream(Redis, S, true),
    with_mutex(Redis,
               ( redis_write_msg(S, Req),
                 redis_read_stream(Redis, S, Out)
               )).
redis2(Redis, Req, Out) :-
    redis_stream(Redis, S, true),
    redis_write_msg(S, Req),
    redis_read_stream(Redis, S, Out).

%!  redis_pipeline(+Redis, +PipeLine)

redis_pipeline(Redis, PipeLine) :-
    Error = error(Formal, _),
    catch(redis_pipeline2(Redis, PipeLine), Error, true),
    (   var(Formal)
    ->  true
    ;   recover(Error, Redis, redis_pipeline(Redis, PipeLine))
    ).

redis_pipeline2(Redis, PipeLine) :-
    atom(Redis),
    !,
    redis_stream(Redis, S, true),
    with_mutex(Redis,
               redis_pipeline3(Redis, S, PipeLine)).
redis_pipeline2(Redis, PipeLine) :-
    redis_stream(Redis, S, true),
    redis_pipeline3(Redis, S, PipeLine).

redis_pipeline3(Redis, S, PipeLine) :-
    maplist(write_pipeline(S), PipeLine),
    flush_output(S),
    read_pipeline(Redis, S, PipeLine).

write_pipeline(S, Command -> _Reply) :-
    !,
    redis_write_msg_no_flush(S, Command).
write_pipeline(S, Command) :-
    redis_write_msg_no_flush(S, Command).

read_pipeline(Redis, S, PipeLine) :-
    E = error(Formal,_),
    catch(read_pipeline2(Redis, S, PipeLine), E, true),
    (   var(Formal)
    ->  true
    ;   reconnect_error(E)
    ->  redis_disconnect(Redis, [force(true)]),
        throw(E)
    ;   resync(Redis),
        throw(E)
    ).

read_pipeline2(Redis, S, PipeLine) :-
    maplist(redis_read_msg3(S), PipeLine, Replies, Errors, Pushed),
    maplist(handle_push(Redis), Pushed),
    maplist(handle_error, Errors),
    maplist(bind_reply, PipeLine, Replies).

redis_read_msg3(S, _Command -> ReplyIn, Reply, Error, Push) :-
    !,
    redis_read_msg(S, ReplyIn, Reply, Error, Push).
redis_read_msg3(S, Var, Reply, Error, Push) :-
    redis_read_msg(S, Var, Reply, Error, Push).

handle_push(Redis, Pushed) :-
    handle_push_messages(Pushed, Redis).
handle_error(Error) :-
    (   var(Error)
    ->  true
    ;   throw(Error)
    ).
bind_reply(_Command -> Reply0, Reply) :-
    !,
    Reply0 = Reply.
bind_reply(_Command, _).


%!  recover(+Error, +Redis, :Goal)
%
%   Error happened while running Goal on Redis. If this is a recoverable
%   error (i.e., a network or disconnected peer),  wait a little and try
%   running Goal again.

:- meta_predicate recover(+, +, 0).

recover(Error, Redis, Goal) :-
    reconnect_error(Error),
    auto_reconnect(Redis),
    !,
    debug(redis(recover), '~p: got error ~p; trying to reconnect',
          [Redis, Error]),
    redis_disconnect(Redis, [force(true)]),
    (   wait_to_retry(Redis, Error)
    ->  call(Goal),
        retractall(failure(Redis, _))
    ;   throw(Error)
    ).
recover(Error, _, _) :-
    throw(Error).

auto_reconnect(redis_connection(_,_,_,Options)) :-
    !,
    option(reconnect(true), Options).
auto_reconnect(Server) :-
    ground(Server),
    server(Server, _, Options),
    option(reconnect(true), Options, true).

reconnect_error(error(io_error(_Action, _On),_)).
reconnect_error(error(socket_error(_Code, _),_)).
reconnect_error(error(syntax_error(unexpected_eof),_)).

%!  wait(+Redis, +Error)
%
%   Wait for some time after a failure. First  we wait for 10ms. This is
%   doubled on each failure upto the   setting  `max_retry_wait`. If the
%   setting `max_retry_count` is exceeded we fail and the called signals
%   an exception.

:- dynamic failure/2 as volatile.

wait_to_retry(Redis, Error) :-
    redis_failures(Redis, Failures),
    setting(max_retry_count, Count),
    Failures < Count,
    Failures2 is Failures+1,
    redis_set_failures(Redis, Failures2),
    setting(max_retry_wait, MaxWait),
    Wait is min(MaxWait*100, 1<<Failures)/100.0,
    debug(redis(recover), '  Sleeping ~p seconds', [Wait]),
    retry_message_level(Failures, Level),
    print_message(Level, redis(retry(Redis, Failures, Wait, Error))),
    sleep(Wait).

redis_failures(redis_connection(_,_,Failures0,_), Failures) :-
    !,
    Failures = Failures0.
redis_failures(Server, Failures) :-
    atom(Server),
    (   failure(Server, Failures)
    ->  true
    ;   Failures = 0
    ).

redis_set_failures(Connection, Count) :-
    compound(Connection),
    !,
    nb_setarg(3, Connection, Count).
redis_set_failures(Server, Count) :-
    atom(Server),
    retractall(failure(Server, _)),
    asserta(failure(Server, Count)).

retry_message_level(0, warning) :- !.
retry_message_level(_, silent).


%!  redis(+Request)
%
%   Connect to the default redis server,   call  redist/3 using Request,
%   disconnect and print the result.  This   predicate  is  intended for
%   interactive usage.

redis(Req) :-
    setup_call_cleanup(
        redis_connect(default, C, []),
        redis1(C, Req, Out),
        redis_disconnect(C)),
    print(Out).

%!  redis_write(+Redis, +Command) is det.
%!  redis_read(+Redis, -Reply) is det.
%
%   Write command and read replies from a Redis server. These are
%   building blocks for subscribing to event streams.

redis_write(Redis, Command) :-
    redis_stream(Redis, S, true),
    redis_write_msg(S, Command).

redis_read(Redis, Reply) :-
    redis_stream(Redis, S, true),
    redis_read_stream(Redis, S, Reply).


		 /*******************************
		 *      HIGH LEVEL ACCESS	*
		 *******************************/

%!  redis_get_list(+Redis, +Key, -List) is det.
%!  redis_get_list(+Redis, +Key, +ChunkSize, -List) is det.
%
%   Get the content of a Redis list in   List. If ChunkSize is given and
%   smaller than the list length, List is returned as a _lazy list_. The
%   actual values are requested using   redis  ``LRANGE`` requests. Note
%   that this results in O(N^2) complexity. Using   a  lazy list is most
%   useful for relatively short lists holding possibly large items.
%
%   Note that values retrieved are _strings_, unless the value was added
%   using `Term as prolog`.
%
%   @see lazy_list/2 for a discussion  on   the  difference between lazy
%   lists and normal lists.

redis_get_list(Redis, Key, List) :-
    redis_get_list(Redis, Key, -1, List).

redis_get_list(Redis, Key, Chunk, List) :-
    redis(Redis, llen(Key), Len),
    (   (   Chunk >= Len
        ;   Chunk == -1
        )
    ->  (   Len == 0
        ->  List = []
        ;   End is Len-1,
            list_range(Redis, Key, 0, End, List)
        )
    ;   lazy_list(rlist_next(s(Redis,Key,0,Chunk,Len)), List)
    ).

rlist_next(State, List, Tail) :-
    State = s(Redis,Key,Offset,Slice,Len),
    End is min(Len-1, Offset+Slice-1),
    list_range(Redis, Key, Offset, End, Elems),
    (   End =:= Len-1
    ->  List = Elems,
        Tail = []
    ;   Offset2 is Offset+Slice,
        nb_setarg(3, State, Offset2),
        append(Elems, Tail, List)
    ).

% Redis LRANGE demands End > Start and returns inclusive.

list_range(DB, Key, Start, Start, [Elem]) :-
    !,
    redis(DB, lindex(Key, Start), Elem).
list_range(DB, Key, Start, End, List) :-
    !,
    redis(DB, lrange(Key, Start, End), List).



%!  redis_set_list(+Redis, +Key, +List) is det.
%
%   Associate a Redis key with a list.  As   Redis  has no concept of an
%   empty list, if List is `[]`, Key  is _deleted_. Note that key values
%   are always strings in  Redis.  The   same  conversion  rules  as for
%   redis/1-3 apply.

redis_set_list(Redis, Key, List) :-
    redis(Redis, del(Key), _),
    (   List == []
    ->  true
    ;   Term =.. [rpush,Key|List],
        redis(Redis, Term, _Count)
    ).


%!  redis_get_hash(+Redis, +Key, -Data:dict) is det.
%!  redis_set_hash(+Redis, +Key, +Data:dict) is det.
%
%   Put/get a Redis hash as a Prolog  dict. Putting a dict first deletes
%   Key. Note that in many cases   applications will manage Redis hashes
%   by key. redis_get_hash/3 is notably a   user friendly alternative to
%   the Redis ``HGETALL`` command. If the  Redis   hash  is  not used by
%   other (non-Prolog) applications one  may   also  consider  using the
%   `Term as prolog` syntax to store the Prolog dict as-is.

redis_get_hash(Redis, Key, Dict) :-
    redis(Redis, hgetall(Key), Dict as dict(auto)).

redis_set_hash(Redis, Key, Dict) :-
    redis_array_dict(Array, _, Dict),
    Term =.. [hset,Key|Array],
    redis(Redis, del(Key), _),
    redis(Redis, Term, _Count).

%!  redis_array_dict(?Array, ?Tag, ?Dict) is det.
%
%   Translate a Redis reply representing  hash   data  into a SWI-Prolog
%   dict. Array is either a list  of   alternating  keys and values or a
%   list of _pairs_. When translating to an array, this is always a list
%   of alternating keys and values.
%
%   @arg Tag is the SWI-Prolog dict tag.

redis_array_dict(Array, Tag, Dict) :-
    nonvar(Array),
    !,
    array_to_pairs(Array, Pairs),
    dict_pairs(Dict, Tag, Pairs).
redis_array_dict(TwoList, Tag, Dict) :-
    dict_pairs(Dict, Tag, Pairs),
    pairs_to_array(Pairs, TwoList).

array_to_pairs([], []) :-
    !.
array_to_pairs([NameS-Value|T0], [Name-Value|T]) :-
    !,                                  % RESP3 returns a map as pairs.
    atom_string(Name, NameS),
    array_to_pairs(T0, T).
array_to_pairs([NameS,Value|T0], [Name-Value|T]) :-
    atom_string(Name, NameS),
    array_to_pairs(T0, T).

pairs_to_array([], []) :-
    !.
pairs_to_array([Name-Value|T0], [NameS,Value|T]) :-
    atom_string(Name, NameS),
    pairs_to_array(T0, T).

%!  redis_scan(+Redis, -LazyList, +Options) is det.
%!  redis_sscan(+Redis, +Set, -LazyList, +Options) is det.
%!  redis_hscan(+Redis, +Hash, -LazyList, +Options) is det.
%!  redis_zscan(+Redis, +Set, -LazyList, +Options) is det.
%
%   Map the Redis ``SCAN``, ``SSCAN``,   ``HSCAN`` and `ZSCAN`` commands
%   into a _lazy list_. For redis_scan/3 and redis_sscan/4 the result is
%   a list of strings. For redis_hscan/4   and redis_zscan/4, the result
%   is a list of _pairs_.   Options processed:
%
%     - match(Pattern)
%       Adds the ``MATCH`` subcommand, only returning matches for
%       Pattern.
%     - count(Count)
%       Adds the ``COUNT`` subcommand, giving a hint to the size of the
%       chunks fetched.
%     - type(Type)
%       Adds the ``TYPE`` subcommand, only returning answers of the
%       indicated type.
%
%   @see lazy_list/2.

redis_scan(Redis, LazyList, Options) :-
    scan_options([match,count,type], Options, Parms),
    lazy_list(scan_next(s(scan,Redis,0,Parms)), LazyList).

redis_sscan(Redis, Set, LazyList, Options) :-
    scan_options([match,count,type], Options, Parms),
    lazy_list(scan_next(s(sscan(Set),Redis,0,Parms)), LazyList).

redis_hscan(Redis, Hash, LazyList, Options) :-
    scan_options([match,count,type], Options, Parms),
    lazy_list(scan_next(s(hscan(Hash),Redis,0,Parms)), LazyList).

redis_zscan(Redis, Set, LazyList, Options) :-
    scan_options([match,count,type], Options, Parms),
    lazy_list(scan_next(s(zscan(Set),Redis,0,Parms)), LazyList).

scan_options([], _, []).
scan_options([H|T0], Options, [H,V|T]) :-
    Term =.. [H,V],
    option(Term, Options),
    !,
    scan_options(T0, Options, T).
scan_options([_|T0], Options, T) :-
    scan_options(T0, Options, T).


scan_next(State, List, Tail) :-
    State = s(Command,Redis,Cursor,Params),
    Command =.. CList,
    append(CList, [Cursor|Params], CList2),
    Term =.. CList2,
    redis(Redis, Term, [NewCursor,Elems0]),
    scan_pairs(Command, Elems0, Elems),
    (   NewCursor == 0
    ->  List = Elems,
        Tail = []
    ;   nb_setarg(3, State, NewCursor),
        append(Elems, Tail, List)
    ).

scan_pairs(hscan(_), List, Pairs) :-
    !,
    scan_pairs(List, Pairs).
scan_pairs(zscan(_), List, Pairs) :-
    !,
    scan_pairs(List, Pairs).
scan_pairs(_, List, List).

scan_pairs([], []).
scan_pairs([Key,Value|T0], [Key-Value|T]) :-
    !,
    scan_pairs(T0, T).
scan_pairs([Key-Value|T0], [Key-Value|T]) :-
    scan_pairs(T0, T).


		 /*******************************
		 *              ABOUT		*
		 *******************************/

%!  redis_current_command(+Redis, ?Command) is nondet.
%!  redis_current_command(+Redis, ?Command, -Properties) is nondet.
%
%   True when Command has Properties. Fails   if Command is not defined.
%   The redis_current_command/3 version  returns   the  command argument
%   specification. See Redis documentation for an explanation.

redis_current_command(Redis, Command) :-
    redis_current_command(Redis, Command, _).

redis_current_command(Redis, Command, Properties) :-
    nonvar(Command),
    !,
    redis(Redis, command(info, Command), [[_|Properties]]).
redis_current_command(Redis, Command, Properties) :-
    redis(Redis, command, Commands),
    member([Name|Properties], Commands),
    atom_string(Command, Name).

%!  redis_property(+Redis, ?Property) is nondet.
%
%   True if Property is a property of   the Redis server. Currently uses
%   redis(info, String) and parses the result.   As  this is for machine
%   usage, properties names *_human are skipped.

redis_property(Redis, Property) :-
    redis(Redis, info, String),
    info_terms(String, Terms),
    member(Property, Terms).

info_terms(Info, Pairs) :-
    split_string(Info, "\n", "\r\n ", Lines),
    convlist(info_line_term, Lines, Pairs).

info_line_term(Line, Term) :-
    sub_string(Line, B, _, A, :),
    !,
    sub_atom(Line, 0, B, _, Name),
    \+ sub_atom(Name, _, _, 0, '_human'),
    sub_string(Line, _, A, 0, ValueS),
    (   number_string(Value, ValueS)
    ->  true
    ;   Value = ValueS
    ),
    Term =.. [Name,Value].


		 /*******************************
		 *            SUBSCRIBE		*
		 *******************************/

%!  redis_subscribe(+Redis, +Channels, -Id, +Options) is det.
%
%   Subscribe to one or more  Redis   PUB/SUB  channels.  This predicate
%   creates a thread using thread_create/3 with  the given Options. Once
%   running, the thread listens for messages.   The message content is a
%   string or Prolog term  as  described   in  redis/3.  On  receiving a
%   message, the following message is broadcasted:
%
%       redis(Id, Channel, Data)
%
%   If redis_unsubscribe/2 removes the  last   subscription,  the thread
%   terminates.
%
%   To simply print the incomming messages use e.g.
%
%       ?- listen(redis(_, Channel, Data),
%                 format('Channel ~p got ~p~n', [Channel,Data])).
%       true.
%       ?- redis_subscribe(default, test, Id, []).
%       Id = redis_pubsub_3,
%       ?- redis(publish(test, "Hello world")).
%       Channel test got "Hello world"
%       1
%       true.
%
%   @arg Id is the thread identifier of  the listening thread. Note that
%   the Options alias(Name) can be used to get a system wide name.

:- dynamic ( subscription/2,            % Id, Channel
             listening/3                % Id, Connection, Thread
           ) as volatile.

redis_subscribe(Redis, Spec, Id, Options) :-
    atom(Redis),
    !,
    channels(Spec, Channels),
    pubsub_thread_options(ThreadOptions, Options),
    thread_create(setup_call_cleanup(
                      redis_connect(Redis, Conn, [reconnect(true)]),
                      redis_subscribe1(Redis, Conn, Channels),
                      redis_disconnect(Conn)),
                  Thread,
                  ThreadOptions),
    pubsub_id(Thread, Id).
redis_subscribe(Redis, Spec, Id, Options) :-
    channels(Spec, Channels),
    pubsub_thread_options(ThreadOptions, Options),
    thread_create(redis_subscribe1(Redis, Redis, Channels),
                  Thread,
                  ThreadOptions),
    pubsub_id(Thread, Id).

pubsub_thread_options(ThreadOptions, Options) :-
    merge_options(Options, [detached(true)], ThreadOptions).

pubsub_id(Thread, Thread).
%pubsub_id(Thread, Id) :-
%    thread_property(Thread, id(TID)),
%    atom_concat('redis_pubsub_', TID, Id).

redis_subscribe1(Redis, Conn, Channels) :-
    Error = error(Formal, _),
    catch(redis_subscribe2(Redis, Conn, Channels), Error, true),
    (   var(Formal)
    ->  true
    ;   recover(Error, Conn, redis1(Conn, echo("reconnect"), _)),
        thread_self(Me),
        pubsub_id(Me, Id),
        findall(Channel, subscription(Id, Channel), CurrentChannels),
        redis_subscribe1(Redis, Conn, CurrentChannels)
    ).

redis_subscribe2(Redis, Conn, Channels) :-
    redis_subscribe3(Conn, Channels),
    redis_listen(Redis, Conn).

redis_subscribe3(Conn, Channels) :-
    thread_self(Me),
    pubsub_id(Me, Id),
    prolog_listen(this_thread_exit, pubsub_clean(Id)),
    maplist(register_subscription(Id), Channels),
    redis_stream(Conn, S, true),
    Req =.. [subscribe|Channels],
    redis_write_msg(S, Req).

pubsub_clean(Id) :-
    retractall(listening(Id, _Connection, _Thread)),
    retractall(subscription(Id, _Channel)).

%!  redis_subscribe(+Id, +Channels) is det.
%!  redis_unsubscribe(+Id, +Channels) is det.
%
%   Add/remove channels from for the   subscription. If no subscriptions
%   remain, the listening thread terminates.
%
%   @arg Channels is either a single  channel   or  a list thereof. Each
%   channel specification is either an atom   or a term `A:B:...`, where
%   all parts are atoms.

redis_subscribe(Id, Spec) :-
    channels(Spec, Channels),
    (   listening(Id, Connection, _Thread)
    ->  true
    ;   existence_error(redis_pubsub, Id)
    ),
    maplist(register_subscription(Id), Channels),
    redis_stream(Connection, S, true),
    Req =.. [subscribe|Channels],
    redis_write_msg(S, Req).

redis_unsubscribe(Id, Spec) :-
    channels(Spec, Channels),
    (   listening(Id, Connection, _Thread)
    ->  true
    ;   existence_error(redis_pubsub, Id)
    ),
    maplist(unregister_subscription(Id), Channels),
    redis_stream(Connection, S, true),
    Req =.. [unsubscribe|Channels],
    redis_write_msg(S, Req).

%!  redis_current_subscription(?Id, ?Channels)
%
%   True when a PUB/SUB subscription with Id is listening on Channels.

redis_current_subscription(Id, Channels) :-
    findall(Id-Channel, subscription(Id, Channel), Pairs),
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    member(Id-Channels, Grouped).

channels(Spec, List) :-
    is_list(Spec),
    !,
    maplist(channel_name, Spec, List).
channels(Ch, [Key]) :-
    channel_name(Ch, Key).

channel_name(Atom, Atom) :-
    atom(Atom),
    !.
channel_name(Key, Atom) :-
    phrase(key_parts(Key), Parts),
    !,
    atomic_list_concat(Parts, :, Atom).
channel_name(Key, _) :-
    type_error(redis_key, Key).

key_parts(Var) -->
    { var(Var), !, fail }.
key_parts(Atom) -->
    { atom(Atom) },
    !,
    [Atom].
key_parts(A:B) -->
    key_parts(A),
    key_parts(B).




register_subscription(Id, Channel) :-
    (   subscription(Id, Channel)
    ->  true
    ;   assertz(subscription(Id, Channel))
    ).

unregister_subscription(Id, Channel) :-
    retractall(subscription(Id, Channel)).

redis_listen(Redis, Conn) :-
    thread_self(Me),
    pubsub_id(Me, Id),
    setup_call_cleanup(
        assertz(listening(Id, Conn, Me), Ref),
        redis_listen_loop(Redis, Id, Conn),
        erase(Ref)).

redis_listen_loop(Redis, Id, Conn) :-
    redis_stream(Conn, S, true),
    (   subscription(Id, _)
    ->  redis_read_stream(Redis, S, Reply),
        redis_broadcast(Redis, Reply),
        redis_listen_loop(Redis, Id, Conn)
    ;   true
    ).

redis_broadcast(_, [subscribe, _Channel, _N]) :-
    !.
redis_broadcast(Redis, [message, Channel, Data]) :-
    !,
    catch(broadcast(redis(Redis, Channel, Data)),
          Error,
          print_message(error, Error)).
redis_broadcast(Redis, Message) :-
    assertion((Message = [Type, Channel, _Data],
               atom(Type),
               atom(Channel))),
    debug(redis(warning), '~p: Unknown message while listening: ~p',
          [Redis,Message]).


		 /*******************************
		 *          READ/WRITE		*
		 *******************************/

%!  redis_read_stream(+Redis, +Stream, -Term) is det.
%
%   Read a message from a Redis stream.  Term is one of
%
%     - A list of terms (array)
%     - A list of pairs (map, RESP3 only)
%     - The atom `nil`
%     - A number
%     - A term status(String)
%     - A string
%     - A boolean (`true` or `false`).  RESP3 only.
%
%   If something goes wrong, the connection   is closed and an exception
%   is raised.

redis_read_stream(Redis, SI, Out) :-
    E = error(Formal,_),
    catch(redis_read_msg(SI, Out, Out0, Error, Push), E, true),
    (   var(Formal)
    ->  handle_push_messages(Push, Redis),
        (   var(Error)
        ->  Out = Out0
        ;   resync(Redis),
            throw(Error)
        )
    ;   redis_disconnect(Redis, [force(true)]),
        throw(E)
    ).

handle_push_messages([], _).
handle_push_messages([H|T], Redis) :-
    (   catch(handle_push_message(H, Redis), E,
              print_message(warning, E))
    ->  true
    ;   true
    ),
    handle_push_messages(T, Redis).

handle_push_message(["pubsub"|List], Redis) :-
    redis_broadcast(Redis, List).
% some protocol version 3 push messages (such as
% __keyspace@* events) seem to come directly
% without a pubsub header
handle_push_message([message|List], Redis) :-
    redis_broadcast(Redis, [message|List]).


%!  resync(+Redis) is det.
%
%   Re-synchronize  after  an  error.  This  may  happen  if  some  type
%   conversion fails and we have read  a   partial  reply. It is hard to
%   figure out what to read from where we are, so we echo a random magic
%   sequence and read until we find the reply.

resync(Redis) :-
    E = error(Formal,_),
    catch(do_resync(Redis), E, true),
    (   var(Formal)
    ->  true
    ;   redis_disconnect(Redis, [force(true)])
    ).

do_resync(Redis) :-
    A is random(1_000_000_000),
    redis_stream(Redis, S, true),
    redis_write_msg(S, echo(A)),
    catch(call_with_time_limit(0.2, '$redis_resync'(S, A)),
          time_limit_exceeded,
          throw(error(time_limit_exceeded,_))).


%!  redis_read_msg(+Stream, -Message, -Error, -PushMessages) is det.
%!  redis_write_msg(+Stream, +Message) is det.
%
%   Read/write a Redis message. Both these predicates are in the foreign
%   module `redis4pl`.
%
%   @arg PushMessages is a list of push   messages that may be non-[] if
%   protocol version 3 (see redis_connect/3) is selected. Using protocol
%   version 2 this list is always empty.



		 /*******************************
		 *            MESSAGES		*
		 *******************************/

:- multifile
    prolog:error_message//1,
    prolog:message//1.

prolog:error_message(redis_error(Code, String)) -->
    [ 'REDIS: ~w: ~s'-[Code, String] ].

prolog:message(redis(retry(_Redis, _Failures, Wait, Error))) -->
    [ 'REDIS: connection error.  Retrying in ~2f seconds'-[Wait], nl ],
    [ '    '-[] ], '$messages':translate_message(Error).
