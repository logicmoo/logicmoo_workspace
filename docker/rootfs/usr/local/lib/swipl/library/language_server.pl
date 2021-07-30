:- module(language_server, [language_server/0, language_server/1, stop_language_server/1]).

% To generate docs:
% - Open SWI Prolog
% - consult("/.../swiplserver/swiplserver/language_server.pl")
% - doc_save("/.../swiplserver/swiplserver/language_server.pl", [doc_root("/.../swiplserver/docs/language_server")]).

/*  Prolog Language Server
    Author:        Eric Zinda
    E-mail:        ericz@inductorsoftware.com
    WWW:           http://www.inductorsoftware.com
    Copyright (c)  2021, Eric Zinda
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

/**
  language_server(+Options:list) is semidet.

Starts a Prolog language server using Options. The server is normally started automatically by a library built for a particular programming language such as the [`swiplserver` Python library](#language-server-python-installation), but starting manually can be useful when debugging Prolog code in some scenarios. See the documentation on ["Standalone Mode"](#language-server-standalone-mode) for more information.

Once started, the server listens for TCP/IP or Unix Domain Socket connections and authenticates them using the password provided before processing any messages.  The messages processed by the server are described [below](#language-server-messages).

For debugging, the server outputs traces using the `debug/3` predicate so that the server operation can be observed by using the `debug/1` predicate. Run the following commands to see them:

- `debug(language_server(protocol))`: Traces protocol messages to show the flow of commands and connections.  It is designed to avoid filling the screen with large queries and results to make it easier to read.
- `debug(language_server(query))`: Traces messages that involve each query and its results. Therefore it can be quite verbose depending on the query.

## Options {#language-server-options}
Options is a list containing any combination of the following options. When used in the Prolog top level (i.e. in [Standalone Mode](#language-server-standalone-mode)), these are specified as normal Prolog options like this:
~~~
language_server([unix_domain_socket(Socket), password('a password')])
~~~
When using ["Embedded Mode"](#language-server-embedded-mode) they are passed using the same name but as normal command line arguments like this:
~~~
swipl --quiet -g language_server -t halt -- --write_connection_values=true --password="a password" --create_unix_domain_socket=true
~~~
Note the use of quotes around values that could confuse command line processing like spaces (e.g. "a password") and that `unix_domain_socket(Variable)` is written as =|--create_unix_domain_socket=true|= on the command line. See below for more information.

- port(?Port)
The TCP/IP port to bind to on localhost. This option is ignored if the `unix_domain_socket/1` option is set. Port is either a legal TCP/IP port number (integer) or a variable term like `Port`. If it is a variable, it causes the system to select a free port and unify the variable with the selected port as in `tcp_bind/2`. If the option `write_connection_values(true)` is set, the selected port is output to STDOUT followed by `\n` on startup to allow the client language library to retrieve it in ["Embedded Mode"](#language-server-embedded-mode).

- unix_domain_socket(?Unix_Domain_Socket_Path_And_File)
If set, Unix Domain Sockets will be used as the way to communicate with the server. `Unix_Domain_Socket_Path_And_File` specifies the fully qualified path and filename to use for the socket.

To have one generated instead (recommended), pass `Unix_Domain_Socket_Path_And_File` as a variable when calling from the Prolog top level and the variable will be unified with a created filename. If launching in ["Embedded Mode"](#language-server-embedded-mode), instead pass =|--create_unix_domain_socket=true|= since there isn't a way to specify variables from the command line. When generating the file, a temporary directory will be created using `tmp_file/2` and a socket file will be created within that directory following the below requirements.  If the directory and file are unable to be created for some reason, language_server/1 fails.

Regardless of whether the file is specified or generated, if the option `write_connection_values(true)` is set, the fully qualified path to the generated file is output to STDOUT followed by `\n` on startup to allow the client language library to retrieve it.

Specifying a file to use should follow the same guidelines as the generated file:
    - If the file exists when the server is launched, it will be deleted.
    - The Prolog process will attempt to create and, if Prolog exits cleanly, delete this file (and directory if it was created) when the server closes.  This means the directory from a specified file must have the appropriate permissions to allow the Prolog process to do so.
    - For security reasons, the filename should not be predictable and the directory it is contained in should have permissions set so that files created are only accessible to the current user.
    - The path must be below 92 *bytes* long (including null terminator) to be portable according to the Linux documentation.

- password(?Password)
The password required for a connection. If not specified (recommended), the server will generate one as a Prolog string type since Prolog atoms are globally visible (be sure not to convert to an atom for this reason). If `Password` is a variable it will be unified with the created password. Regardless of whether the password is specified or generated, if the option `write_connection_values(true)` is set, the password is output to STDOUT followed by `\n` on startup to allow the client language library to retrieve it. This is the recommended way to integrate the server with a language as it avoids including the password as source code. This option is only included so that a known password can be supplied for when the server is running in Standalone Mode.

- query_timeout(+Seconds)
Sets the default time in seconds that a query is allowed to run before it is cancelled. This can be overridden on a query by query basis. If not set, the default is no timeout (`-1`).

- pending_connections(+Count)
Sets the number of pending connections allowed for the server as in `tcp_listen/2`. If not provided, the default is `5`.

- run_server_on_thread(+Run_Server_On_Thread)
Determines whether `language_server/1` runs in the background on its own thread or blocks until the server shuts down.  Must be missing or set to `true` when running in ["Embedded Mode"](#language-server-embedded-mode) so that the SWI Prolog process can exit properly. If not set, the default is `true`.

- server_thread(?Server_Thread)
Specifies or retrieves the name of the thread the server will run on if `run_server_on_thread(true)`. Passing in an atom for Server_Thread will only set the server thread name if run_server_on_thread(true).  If `Server_Thread` is a variable, it is unified with a generated name.

- write_connection_values(+Write_Connection_Values)
Determines whether the server writes the port (or generated Unix Domain Socket) and password to STDOUT as it initializes. Used by language libraries to retrieve this information for connecting. If not set, the default is `false`.

- write_output_to_file(+File)
Redirects STDOUT and STDERR to the file path specified.  Useful for debugging the server when it is being used in ["Embedded Mode"](#language-server-embedded-mode). If using multiple servers in one SWI Prolog instance, only set this on the first one.  Each time it is set the output will be redirected.

## Language Server Messages {#language-server-messages}
The messages the server responds to are described below. A few things are true for all of them:

- Every connection is in its own separate thread. Opening more than one connection means the code is running concurrently.
- Closing the socket without sending `close` and waiting for a response will halt the process if running in ["Embedded Mode"](#language-server-embedded-mode). This is so that stopping a debugger doesn't leave the process orphaned.
- All messages are request/response messages. After sending, there will be exactly one response from the server.
- Timeout in all of the commands is in seconds. Sending a variable (e.g. `_`) will use the default timeout passed to the initial `language_server/1` predicate and `-1` means no timeout.
- All queries are run in the default module context of `user`. `module/1` has no effect.

### Language Server Message Format {#language-server-message-format}
Every language server message is a single valid Prolog term. Those that run queries have an argument which represents the query as a single term. To run several goals at once use `(goal1, goal2, ...)` as the goal term.

The format of sent and received messages is identical (`\n` stands for the ASCII newline character which is a single byte): 
~~~
<stringByteLength>.\n<stringBytes>.\n. 
~~~
For example, to send `hello` as a message you would send this: 
~~~
7.\nhello.\n
~~~
 - =|<stringByteLength>|= is the number of bytes of the string to follow (including the =|.\n|=), in human readable numbers, such as `15` for a 15 byte string. It must be followed by =|.\n|=.
 - =|<stringBytes>|= is the actual message string being sent, such as =|run(atom(a), -1).\n|=. It must always end with =|.\n|=. The character encoding used to decode and encode the string is UTF-8.

To send a message to the server, send a message using the message format above to the localhost port or Unix Domain Socket that the server is listening on.  For example, to run the synchronous goal `atom(a)`, send the following message:
~~~
18.\nrun(atom(a), -1).\n<end of stream>
~~~
You will receive the response below on the receive stream of the same connection you sent on. Note that the answer is in JSON format. If a message takes longer than 2 seconds, there will be "heartbeat" characters (".") at the beginning of the response message, approximately 1 every 2 seconds. So, if the query takes 6 seconds for some reason, there will be three "." characters first:
~~~
...12\ntrue([[]]).\n
~~~

### Language Server Messages Reference {#language-server-messages}

The full list of language server messages are described below:


- run(Goal, Timeout)

Runs `Goal` on the connection's designated query thread. Stops accepting new commands until the query is finished and it has responded with the results.  If a previous query is still in progress, waits until the previous query finishes (discarding that query's results) before beginning the new query.

Timeout is in seconds and indicates a timeout for generating all results for the query. Sending a variable (e.g. `_`) will use the default timeout passed to the initial `language_server/1` predicate and `-1` means no timeout.

While it is waiting for the query to complete, sends a "." character *not* in message format, just as a single character, once every two seconds to proactively ensure that the client is alive. Those should be read and discarded by the client.

If a communication failure happens (during a heartbeat or otherwise), the connection is terminated, the query is aborted and (if running in ["Embedded Mode"](#language-server-embedded-mode)) the SWI Prolog process shuts down.

When completed, sends a response message using the normal message format indicating the result.

Response:

|`true([Answer1, Answer2, ... ])` | The goal succeeded at least once. The response always includes all answers as if run with findall() (see run_async/3 below to get individual results back iteratively).  Each `Answer` is a list of the assignments of free variables in the answer. If there are no free variables, `Answer` is an empty list. |
|`false` | The goal failed. |
|`exception(time_limit_exceeded)` | The query timed out. |
|`exception(Exception)` | An arbitrary exception was not caught while running the goal. |
|`exception(connection_failed)` | The query thread unexpectedly exited. The server will no longer be listening after this exception. |

- run_async(Goal, Timeout, Find_All)

Starts a Prolog query specified by `Goal` on the connection's designated query thread. Answers to the query, including exceptions, are retrieved afterwards by sending the `async_result` message (described below). The query can be cancelled by sending the `cancel_async` message. If a previous query is still in progress, waits until that query finishes (discarding that query's results) before responding.

Timeout is in seconds and indicates a timeout for generating all results for the query. Sending a variable (e.g. `_`) will use the default timeout passed to the initial `language_server/1` predicate and `-1` means no timeout.

If the socket closes before a response is sent, the connection is terminated, the query is aborted and (if running in ["Embedded Mode"](#language-server-embedded-mode)) the SWI Prolog process shuts down.

If it needs to wait for the previous query to complete, it will send heartbeat messages (see ["Language Server Message Format"](#language-server-message-format)) while it waits.  After it responds, however, it does not send more heartbeats. This is so that it can begin accepting new commands immediately after responding so the client.

`Find_All == true` means generate one response to an `async_result` message with all of the answers to the query (as in the `run` message above). `Find_All == false` generates a single response to an  `async_result` message per answer.

Response:

|`true([[]])` | The goal was successfully parsed. |
|`exception(Exception)` | An error occurred parsing the goal. |
|`exception(connection_failed)` | The goal thread unexpectedly shut down. The server will no longer be listening after this exception. |


- cancel_async
Attempt to cancel a query started by the `run_async` message in a way that allows further queries to be run on this Prolog thread afterwards.

If there is a goal running, injects a `throw(cancel_goal)` into the executing goal to attempt to stop the goal's execution. Begins accepting new commands immediately after responding. Does not inject `abort/0` because this would kill the connection's designated thread and the system is designed to maintain thread local data for the client. This does mean it is a "best effort" cancel since the exception can be caught.

`cancel_async` is guaranteed to either respond with an exception (if there is no query or pending results from the last query), or safely attempt to stop the last executed query even if it has already finished.

To guarantee that a query is cancelled, send `close` and close the socket.

It is not necessary to determine the outcome of `cancel_async` after sending it and receiving a response. Further queries can be immediately run. They will start after the current query stops.

However, if you do need to determine the outcome or determine when the query stops, send `async_result`. Using `Timeout = 0` is recommended since the query might have caught the exception or still be running.  Sending `async_result` will find out the "natural" result of the goal's execution. The "natural" result depends on the particulars of what the code actually did. The response could be:

|`exception(cancel_goal)` | The query was running and did not catch the exception. I.e. the goal was successfully cancelled. |
|`exception(time_limit_exceeded)` | The query timed out before getting cancelled. |
|`exception(Exception)` | They query hits another exception before it has a chance to be cancelled. |
| A valid answer | The query finished before being cancelled. |

Note that you will need to continue sending `async_result` until you receive an `exception(Exception)` message if you want to be sure the query is finished (see documentation for `async_result`).

Response:

| `true([[]])` | There is a query running or there are pending results for the last query. |
| `exception(no_query)` | There is no query or pending results from a query to cancel. |
| `exception(connection_failed)` | The connection has been unexpectedly shut down. The server will no longer be listening after this exception. |


- async_result(Timeout) 
Get results from a query that was started via a `run_async` message. Used to get results for all cases: if the query terminates normally, is cancelled by sending a `cancel_async` message, or times out. 

Each response to an `async_result` message responds with one result and, when there are no more results, responds with `exception(no_more_results)` or whatever exception stopped the query. Receiving any `exception` response except `exception(result_not_available)` means there are no more results. If `run_async` was run with `Find_All == false`, multiple `async_result` messages may be required before receiving the final exception.

Waits `Timeout` seconds for a result. `Timeout == -1` or sending a variable for Timeout indicates no timeout. If the timeout is exceeded and no results are ready, sends `exception(result_not_available)`.

Some examples:

|If the query succeeds with N answers...                             | `async_result` messages 1 to N will receive each answer, in order,  and `async_result` message N+1 will receive `exception(no_more_results)` |
|If the query fails (i.e. has no answers)...                         | `async_result` message 1 will receive `false` and `async_result` message 2 will receive `exception(no_more_results)` |
|If the query times out after one answer...                          | `async_result` message 1 will receive the first answer and `async_result` message 2 will receive `exception(time_limit_exceeded)` |
|If the query is cancelled after it had a chance to get 3 answers... | `async_result` messages 1 to 3 will receive each answer, in order,  and `async_result` message 4 will receive `exception(cancel_goal)` |
|If the query throws an exception before returning any results...    | `async_result` message 1 will receive `exception(Exception)`|

Note that, after sending `cancel_async`, calling `async_result` will return the "natural" result of the goal's execution. The "natural" result depends on the particulars of what the code actually did since this is multi-threaded and there are race conditions. This is described more below in the response section and above in `cancel_async`.

Response:

|`true([Answer1, Answer2, ... ])` | The next answer from the query is a successful answer. Whether there are more than one `Answer` in the response depends on the `findall` setting. Each `Answer` is a list of the assignments of free variables in the answer. If there are no free variables, `Answer` is an empty list.|
|`false`| The query failed with no answers.|
|`exception(no_query)` | There is no query in progress.|
|`exception(result_not_available)` | There is a running query and no results were available in `Timeout` seconds.|
|`exception(no_more_results)` | There are no more answers and no other exception occurred. |
|`exception(cancel_goal)`| The next answer is an exception caused by `cancel_async`. Indicates no more answers. |
|`exception(time_limit_exceeded)`| The query timed out generating the next answer (possibly in a race condition before getting cancelled).  Indicates no more answers. |
|`exception(Exception)`| The next answer is an arbitrary exception. This can happen after `cancel_async` if the `cancel_async` exception is caught or the code hits another exception first.  Indicates no more answers. |
|`exception(connection_failed)`| The goal thread unexpectedly exited. The server will no longer be listening after this exception.|


- close
Closes a connection cleanly, indicating that the subsequent socket close is not a connection failure. Thus it doesn't shutdown the server in ["Embedded Mode"](#language-server-embedded-mode).  The response must be processed by the client before closing the socket or it will be interpreted as a connection failure.

Any asynchronous query that is still running will be halted by using `abort/0` in the connection's query thread.

Response:
`true([[]])`


- quit
Stops the server and ends the SWI Prolog process. This allows client language libraries to ask for an orderly shutdown of the Prolog process.

Response:
`true([[]])`

*/
:- use_module(library(socket)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(option)).
:- use_module(library(term_to_json)).
% One for every language server running
:- dynamic(language_server_thread/3).

% One for every active connection
:- dynamic(language_server_worker_threads/3).
:- dynamic(language_server_socket/5).

% Indicates that a query is in progress on the goal thread or hasn't had its results drained
% Deleted once the last result from the queue has been drained
% Only deleted by the communication thread to avoid race conditions
:- dynamic(query_in_progress/1).

% Indicates to the communication thread that we are in a place
% that can be cancelled
:- dynamic(safe_to_cancel/1).


% Password is carefully constructed to be a string (not an atom) so that it is not
% globally visible
% Add ".\n" to the password since it will be added by the message when received
language_server(Options) :-
    Encoding = utf8,
    option(pending_connections(Connection_Count), Options, 5),
    option(query_timeout(Query_Timeout), Options, -1),
    option(port(Port), Options, _),
    option(run_server_on_thread(Run_Server_On_Thread), Options, true),
    option(exit_main_on_failure(Exit_Main_On_Failure), Options, false),
    option(write_connection_values(Write_Connection_Values), Options, false),
    option(unix_domain_socket(Unix_Domain_Socket_Path_And_File), Options, _),
    (   (   memberchk(unix_domain_socket(_), Options),
            var(Unix_Domain_Socket_Path_And_File)
        )
    ->  unix_domain_socket_path(Unix_Domain_Socket_Path, Unix_Domain_Socket_Path_And_File)
    ;   true
    ),
    option(server_thread(Server_Thread_ID), Options, _),
    (   var(Server_Thread_ID)
    ->  gensym(language_server, Server_Thread_ID)
    ;   true
    ),
    option(password(Password), Options, _),
    (   var(Password)
    ->  (   uuid(UUID, [format(integer)]),
            format(string(Password), '~d', [UUID])
        )
    ;   true
    ),
    string_concat(Password, '.\n', Final_Password),
    bind_socket(Server_Thread_ID, Unix_Domain_Socket_Path_And_File, Port, Socket, Client_Address),
    send_client_startup_data(Write_Connection_Values, user_output, Unix_Domain_Socket_Path_And_File, Client_Address, Password),
    option(write_output_to_file(File), Options, _),
    (   var(File)
    ->  true
    ;   write_output_to_file(File)
    ),
    Server_Goal = (
                    catch(server_thread(Server_Thread_ID, Socket, Client_Address, Final_Password, Connection_Count, Encoding, Query_Timeout, Exit_Main_On_Failure), error(E1, E2), true),
                    debug(language_server(protocol), "Stopped server on thread: ~w due to exception: ~w", [Server_Thread_ID, error(E1, E2)])
                 ),
    start_server_thread(Run_Server_On_Thread, Server_Thread_ID, Server_Goal, Unix_Domain_Socket_Path, Unix_Domain_Socket_Path_And_File).


%! language_server is semidet.
%Main entry point for running the Language Server in ["Embedded Mode"](#language-server-embedded-mode) and designed to be called from the command line. Embedded Mode is used when launching the Language Server as an embedded part of another language (e.g. Python). Calling language_server/0 from Prolog interactively is not recommended as it depends on Prolog exiting to stop the server, instead use language_server/1 for interactive use.
%
%To launch embedded mode:
%
%~~~
%swipl --quiet -g language_server -t halt -- --write_connection_values=true
%~~~
%
%This will start SWI Prolog and invoke the language_server/0 predicate and exit the process when that predicate stops. Any command line arguments after the standalone `--` will be passed as Options. These are the same Options that language_server/1 accepts and are passed to it directly. Some options are expressed differently due to command line limitations, see language_server/1 Options for more information.
%
%Any Option values that causes issues during command line parsing (such as spaces) should be passed with =|""|= like this:
%
%~~~
%swipl --quiet -g language_server -t halt -- --write_connection_values=true --password="HGJ SOWLWW WNDSJD"
%~~~


% Turn off int signal when running in embedded mode so the client language
% debugger signal doesn't put Prolog into debug mode
% run_server_on_thread must be missing or true (the default) so we can exit
% properly
% create_unix_domain_socket=true/false is only used as a command line argument
% since it doesn't seem possible to pass create_unix_domain_socket=_ on the command line
% and have it interpreted as a variable.
language_server :-
    current_prolog_flag(os_argv, Argv),
    argv_options(Argv, _Args, Options),
    append(Options, [exit_main_on_failure(true)], Options1),
    select_option(create_unix_domain_socket(Create_Unix_Domain_Socket), Options1, Options2, false),
    (   Create_Unix_Domain_Socket
    ->  append(Options2, [unix_domain_socket(_)], FinalOptions)
    ;   FinalOptions = Options2
    ),
    option(run_server_on_thread(Run_Server_On_Thread), FinalOptions, true),
    (   Run_Server_On_Thread
    ->  true
    ;   throw(domain_error(cannot_be_set_in_embedded_mode, run_server_on_thread))
    ),
    language_server(FinalOptions),
    on_signal(int, _, quit),
    thread_get_message(quit_language_server).


quit(_) :-
    thread_send_message(main, quit_language_server).


%! stop_language_server(+Server_Thread_ID:atom) is det.
%
% If `Server_Thread_ID` is a variable, stops all language servers and associated threads.  If `Server_Thread_ID` is an atom, then only the server with that `Server_Thread_ID` is stopped. `Server_Thread_ID` can be provided or retrieved using `Options` in `language_server/1`.
%
% Always succeeds.

% tcp_close_socket(Socket) will shut down the server thread cleanly so the socket is released and can be used again in the same session
% Closes down any pending connections using abort even if there were no matching server threads since the server thread could have died.
% At this point only threads associated with live connections (or potentially a goal thread that hasn't detected its missing communication thread)
% should be left so seeing abort warning messages in the console seems OK
stop_language_server(Server_Thread_ID) :-
    % First shut down any matching servers to stop new connections
    forall(retract(language_server_thread(Server_Thread_ID, _, Socket)),
        (
            debug(language_server(protocol), "Found server: ~w", [Server_Thread_ID]),
            catch(tcp_close_socket(Socket), Socket_Exception, true),
            abortSilentExit(Server_Thread_ID, Server_Thread_Exception),
            debug(language_server(protocol), "Stopped server thread: ~w, socket_close_exception(~w), stop_thread_exception(~w)", [Server_Thread_ID, Socket_Exception, Server_Thread_Exception])
        )),
    forall(retract(language_server_worker_threads(Server_Thread_ID, Communication_Thread_ID, Goal_Thread_ID)),
        (
            abortSilentExit(Communication_Thread_ID, CommunicationException),
            debug(language_server(protocol), "Stopped server: ~w communication thread: ~w, exception(~w)", [Server_Thread_ID, Communication_Thread_ID, CommunicationException]),
            abortSilentExit(Goal_Thread_ID, Goal_Exception),
            debug(language_server(protocol), "Stopped server: ~w goal thread: ~w, exception(~w)", [Server_Thread_ID, Goal_Thread_ID, Goal_Exception])
        )).


start_server_thread(Run_Server_On_Thread, Server_Thread_ID, Server_Goal, Unix_Domain_Socket_Path, Unix_Domain_Socket_Path_And_File) :-
    (   Run_Server_On_Thread
    ->  (   thread_create(Server_Goal, _, [ alias(Server_Thread_ID),
                                            at_exit((delete_unix_domain_socket_file(Unix_Domain_Socket_Path, Unix_Domain_Socket_Path_And_File),
                                                     detach_if_expected(Server_Thread_ID)
                                                    ))
                                          ]),
            debug(language_server(protocol), "Started server on thread: ~w", [Server_Thread_ID])
        )
    ;   (   Server_Goal,
            delete_unix_domain_socket_file(Unix_Domain_Socket_Path, Unix_Domain_Socket_Path_And_File),
            debug(language_server(protocol), "Halting.", [])
        )
    ).


% Unix domain sockets create a file that needs to be cleaned up
% If language_server generated it, there is also a directory that needs to be cleaned up
%   that will only contain that file
delete_unix_domain_socket_file(Unix_Domain_Socket_Path, Unix_Domain_Socket_Path_And_File) :-
    (   nonvar(Unix_Domain_Socket_Path)
    ->  catch(delete_directory_and_contents(Unix_Domain_Socket_Path), error(_, _), true)
    ;   (   nonvar(Unix_Domain_Socket_Path_And_File)
        ->  catch(delete_file(Unix_Domain_Socket_Path_And_File), error(_, _), true)
        ;   true
        )
    ).

:- if(current_predicate(unix_domain_socket/1)).
    optional_unix_domain_socket(Socket) :-
        unix_domain_socket(Socket).
:- else.
    optional_unix_domain_socket(_).
:- endif.

% Always bind only to localhost for security reasons
% Delete the socket file in case it is already around so that the same name can be reused
bind_socket(Server_Thread_ID, Unix_Domain_Socket_Path_And_File, Port, Socket, Client_Address) :-
    (   nonvar(Unix_Domain_Socket_Path_And_File)
    ->  debug(language_server(protocol), "Using Unix domain socket name: ~w", [Unix_Domain_Socket_Path_And_File]),
        optional_unix_domain_socket(Socket),
        catch(delete_file(Unix_Domain_Socket_Path_And_File), error(_, _), true),
        tcp_bind(Socket, Unix_Domain_Socket_Path_And_File),
        Client_Address = Unix_Domain_Socket_Path_And_File
    ;   (   tcp_socket(Socket),
            tcp_setopt(Socket, reuseaddr),
            tcp_bind(Socket, '127.0.0.1':Port),
            debug(language_server(protocol), "Using TCP/IP port: ~w", ['127.0.0.1':Port]),
            Client_Address = Port
        )
    ),
    assert(language_server_thread(Server_Thread_ID, Unix_Domain_Socket_Path_And_File, Socket)).

% Communicates the used port and password to the client via STDOUT so the client
% language library can use them to connect
send_client_startup_data(Write_Connection_Values, Stream, Unix_Domain_Socket_Path_And_File, Port, Password) :-
    (   Write_Connection_Values
    ->  (   (  var(Unix_Domain_Socket_Path_And_File)
            ->  format(Stream, "~d\n", [Port])
            ;   format(Stream, "~w\n", [Unix_Domain_Socket_Path_And_File])
            ),
            format(Stream, "~w\n", [Password]),
            flush_output(Stream)
        )
    ;   true
    ).


% Server thread worker predicate
% Listen for connections and create a connection for each in its own communication thread
% Uses tail recursion to ensure the stack doesn't grow
server_thread(Server_Thread_ID, Socket, Address, Password, Connection_Count, Encoding, Query_Timeout, Exit_Main_On_Failure) :-
    debug(language_server(protocol), "Listening on address: ~w", [Address]),
    tcp_listen(Socket, Connection_Count),
    tcp_open_socket(Socket, AcceptFd, _),
    create_connection(Server_Thread_ID, AcceptFd, Password, Encoding, Query_Timeout, Exit_Main_On_Failure),
    server_thread(Server_Thread_ID, Socket, Address, Password, Connection_Count, Encoding, Query_Timeout, Exit_Main_On_Failure).


% Wait for the next connection and create communication and goal threads to support it
% Create known IDs for the threads so we can pass them along before the threads are created
% First create the goal thread to avoid a race condition where the communication
% thread tries to queue a goal before it is created
create_connection(Server_Thread_ID, AcceptFd, Password, Encoding, Query_Timeout, Exit_Main_On_Failure) :-
    debug(language_server(protocol), "Waiting for client connection...", []),
    tcp_accept(AcceptFd, Socket, _Peer),
    debug(language_server(protocol), "Client connected", []),
    gensym('conn', Connection_Base),
    atomic_list_concat([Server_Thread_ID, "_", Connection_Base, '_comm'], Thread_Alias),
    atomic_list_concat([Server_Thread_ID, "_", Connection_Base, '_goal'], Goal_Alias),
    mutex_create(Goal_Alias, [alias(Goal_Alias)]),
    assert(language_server_worker_threads(Server_Thread_ID, Thread_Alias, Goal_Alias)),
    thread_create(goal_thread(Thread_Alias),
        _,
        [alias(Goal_Alias), at_exit(detach_if_expected(Goal_Alias))]),
    thread_create(communication_thread(Password, Socket, Encoding, Server_Thread_ID, Goal_Alias, Query_Timeout, Exit_Main_On_Failure),
        _,
        [alias(Thread_Alias), at_exit(detach_if_expected(Thread_Alias))]).


% The worker predicate for the Goal thread.
% Looks for a message from the connection thread, processes it, then recurses.
%
% Goals always run in the same thread in case the user is setting thread local information.
% For each answer to the user's query (including an exception), the goal thread will queue a message
% to the communication thread of the form result(Answer, Find_All), where Find_All == true if the user wants all answers at once
% Tail recurse to avoid growing the stack
goal_thread(Respond_To_Thread_ID) :-
    thread_self(Self_ID),
    throw_if_testing(Self_ID),
    thread_get_message(Self_ID, goal(Goal, Binding_List, Query_Timeout, Find_All)),
    debug(language_server(query), "Received Findall = ~w, Query_Timeout = ~w, binding list: ~w, goal: ~w", [Find_All, Query_Timeout, Binding_List, Goal]),
    (   Find_All
    ->  One_Answer_Goal = findall(Binding_List, @(user:Goal, user), Answers)
    ;
        One_Answer_Goal = ( @(user:Goal, user),
                            Answers = [Binding_List],
                            send_next_result(Respond_To_Thread_ID, Answers, _, Find_All)
                          )
    ),
    All_Answers_Goal = run_cancellable_goal(Self_ID, findall(Answers, One_Answer_Goal, [Find_All_Answers | _])),
    (   Query_Timeout == -1
    ->  catch(All_Answers_Goal, Top_Exception, true)
    ;   catch(call_with_time_limit(Query_Timeout, All_Answers_Goal), Top_Exception, true)
    ),
    (
        var(Top_Exception)
    ->  (
            Find_All
        ->
            send_next_result(Respond_To_Thread_ID, Find_All_Answers, _, Find_All)
        ;
            send_next_result(Respond_To_Thread_ID, [], no_more_results, Find_All)
        )
    ;
        send_next_result(Respond_To_Thread_ID, [], Top_Exception, true)
    ),
    goal_thread(Respond_To_Thread_ID).


% Used only for testing unhandled exceptions outside of the "safe zone"
throw_if_testing(Self_ID) :-
    (   thread_peek_message(Self_ID, testThrow(Test_Exception))
    ->  (   debug(language_server(query), "TESTING: Throwing test exception: ~w", [Test_Exception]),
            throw(Test_Exception)
        )
    ;   true
    ).


% run_cancellable_goal handles the communication
% to ensure the cancel exception from the communication thread
% is injected at a place we are prepared to handle in the goal_thread
% Before the goal is run, sets a fact to indicate we are in the "safe to cancel"
% zone for the communication thread.
% Then it doesn't exit this "safe to cancel" zone if the
% communication thread is about to cancel
run_cancellable_goal(Mutex_ID, Goal) :-
    thread_self(Self_ID),
    setup_call_cleanup(
        assert(safe_to_cancel(Self_ID), Assertion),
        Goal,
        with_mutex(Mutex_ID, erase(Assertion))
    ).


% Worker predicate for the communication thread.
% Processes messages and sends goals to the goal thread.
% Continues processing messages until communication_thread_listen() throws or ends with true/false
%
% Catches all exceptions from communication_thread_listen so that it can do an orderly shutdown of the goal
%   thread if there is a communication failure.
%
% True means user explicitly called close or there was an exception
%   only exit the main thread if there was an exception and we are supposed to Exit_Main_On_Failure
%   otherwise just exit the session
communication_thread(Password, Socket, Encoding, Server_Thread_ID, Goal_Thread_ID, Query_Timeout, Exit_Main_On_Failure) :-
    thread_self(Self_ID),
    (   (
            catch(communication_thread_listen(Password, Socket, Encoding, Server_Thread_ID, Goal_Thread_ID, Query_Timeout), error(Serve_Exception1, Serve_Exception2), true),
            debug(language_server(protocol), "Session finished. Communication thread exception: ~w", [error(Serve_Exception1, Serve_Exception2)]),
            abortSilentExit(Goal_Thread_ID, _),
            retractall(language_server_worker_threads(Server_Thread_ID, Self_ID, Goal_Thread_ID))
        )
    ->  Halt = (nonvar(Serve_Exception1), Exit_Main_On_Failure)
    ;   Halt = true
    ),
    (   Halt
    ->  (   debug(language_server(protocol), "Ending session and halting Prolog server due to thread ~w: exception(~w)", [Self_ID, error(Serve_Exception1, Serve_Exception2)]),
            quit(_)
        )
    ;   (   debug(language_server(protocol), "Ending session ~w", [Self_ID]),
            catch(tcp_close_socket(Socket), error(_, _), true)
        )
    ).


% Open socket and begin processing the streams for a connection using the Encoding if the password matches
% true: session ended
% exception: communication failure or an internal failure (like a thread threw or shutdown unexpectedly)
% false: halt
communication_thread_listen(Password, Socket, Encoding, Server_Thread_ID, Goal_Thread_ID, Query_Timeout) :-
    tcp_open_socket(Socket, Read_Stream, Write_Stream),
    thread_self(Communication_Thread_ID),
    assert(language_server_socket(Server_Thread_ID, Communication_Thread_ID, Socket, Read_Stream, Write_Stream)),
    set_stream(Read_Stream, encoding(Encoding)),
    set_stream(Write_Stream, encoding(Encoding)),
    read_message(Read_Stream, Sent_Password),
    (   Password == Sent_Password
    ->  (   debug(language_server(protocol), "Password matched.", []),
            thread_self(Self_ID),
            reply(Write_Stream, true([[threads(Self_ID, Goal_Thread_ID)]]))
        )
    ;   (   debug(language_server(protocol), "Password mismatch, failing. ~w", [Sent_Password]),
            reply_error(Write_Stream, password_mismatch),
            throw(password_mismatch)
        )
    ),
    process_language_server_messages(Read_Stream, Write_Stream, Goal_Thread_ID, Query_Timeout),
    debug(language_server(protocol), "Session finished.", []).


% process_language_server_messages implements the main interface to the language server.
% Continuously reads a language server message from Read_Stream and writes a response to Write_Stream,
% until the connection fails or a `quit` or `close` message is sent.
%
% Read_Stream and Write_Stream can be any valid stream using any encoding.
%
% Goal_Thread_ID must be the threadID of a thread started on the goal_thread predicate
%
% uses tail recursion to ensure the stack doesn't grow
%
% true: indicates we should terminate the session (clean termination)
% false: indicates we should exit the process if running in embedded mode
% exception: indicates we should terminate the session (communication failure termination) or
%    thread was asked to halt
process_language_server_messages(Read_Stream, Write_Stream, Goal_Thread_ID, Query_Timeout) :-
    process_language_server_message(Read_Stream, Write_Stream, Goal_Thread_ID, Query_Timeout, Command),
    (   Command == close
    ->  (   debug(language_server(protocol), "Command: close. Client closed the connection cleanly.", []),
            true
        )
    ;   (   Command == quit
        ->  (   debug(language_server(protocol), "Command: quit.", []),
                false
            )
        ;
            process_language_server_messages(Read_Stream, Write_Stream, Goal_Thread_ID, Query_Timeout)
        )
    ).

% process_language_server_message manages the protocol for the connection: receive message, parse it, process it.
% - Reads a single message from Read_Stream.
% - Processes it and issues a response on Write_Stream.
% - The message will be unified with Command to allow the caller to handle it.
%
% Read_Stream and Write_Stream can be any valid stream using any encoding.
%
% True if the message understood. A response will always be sent.
% False if the message was malformed.
% Exceptions will be thrown by the underlying stream if there are communication failures writing to Write_Stream or the thread was asked to exit.
%
% state_* predicates manage the state transitions of the protocol
% They only bubble up exceptions if there is a communication failure
%
% state_process_command will never return false
% since errors should be sent to the client
% It can throw if there are communication failures, though.
process_language_server_message(Read_Stream, Write_Stream, Goal_Thread_ID, Query_Timeout, Command) :-
    debug(language_server(protocol), "Waiting for next message ...", []),
    (   state_receive_raw_message(Read_Stream, Message_String)
    ->  (   state_parse_command(Write_Stream, Message_String, Command, Binding_List)
        ->  state_process_command(Write_Stream, Goal_Thread_ID, Query_Timeout, Command, Binding_List)
        ;   true
        )
    ;   false
    ).


% state_receive_raw_message: receive a raw message, which is simply a string
%   true: valid message received
%   false: invalid message format
%   exception: communication failure OR thread asked to exit
state_receive_raw_message(Read, Command_String) :-
    read_message(Read, Command_String),
    debug(language_server(protocol), "Valid message: ~w", [Command_String]).


% state_parse_command: attempt to parse the message string into a valid command
%
% Use read_term_from_atom instead of read_term(stream) so that we don't hang
% indefinitely if the caller didn't properly finish the term
% parse in the context of module 'user' to properly bind operators, do term expansion, etc
%
%   true: command could be parsed
%   false: command cannot be parsed.  An error is sent to the client in this case
%   exception: communication failure on sending a reply
state_parse_command(Write_Stream, Command_String, Parsed_Command, Binding_List) :-
    (   catch(read_term_from_atom(Command_String, Parsed_Command, [variable_names(Binding_List), module(user)]), Parse_Exception, true)
    ->  (   var(Parse_Exception)
        ->  debug(language_server(protocol), "Parse Success: ~w", [Parsed_Command])
        ;   (   reply_error(Write_Stream, Parse_Exception),
                fail
            )
        )
    ;   (   reply_error(Write_Stream, error(couldNotParseCommand, _)),
            fail
        )
    ).


% state_process_command(): execute the requested Command
%
% First wait until we have removed all results from any previous query.
% If query_in_progress(Goal_Thread_ID) exists then there is at least one
% more result to drain, by definition. Because the predicate is
% deleted by get_next_result in the communication thread when the last result is drained
%
%   true: if the command itself succeeded, failed or threw an exception.
%         In that case, the outcome is sent to the client
%   exception: only communication or thread failures are allowed to bubble up
% See language_server(Options) documentation
state_process_command(Stream, Goal_Thread_ID, Query_Timeout, run(Goal, Timeout), Binding_List) :-
    !,
    debug(language_server(protocol), "Command: run/1. Timeout: ~w", [Timeout]),
    repeat_until_false((
            query_in_progress(Goal_Thread_ID),
            debug(language_server(protocol), "Draining unretrieved result for ~w", [Goal_Thread_ID]),
            heartbeat_until_result(Goal_Thread_ID, Stream, Unused_Answer),
            debug(language_server(protocol), "Drained result for ~w", [Goal_Thread_ID]),
            debug(language_server(query), "    Discarded answer: ~w", [Unused_Answer])
        )),
    debug(language_server(protocol), "All previous results drained", []),
    send_goal_to_thread(Stream, Goal_Thread_ID, Query_Timeout, Timeout, Goal, Binding_List, true),
    heartbeat_until_result(Goal_Thread_ID, Stream, Answers),
    reply_with_result(Goal_Thread_ID, Stream, Answers).


% See language_server(Options) documentation for documentation
% See notes in run(Goal, Timeout) re: draining previous query
state_process_command(Stream, Goal_Thread_ID, Query_Timeout, run_async(Goal, Timeout, Find_All), Binding_List) :-
    !,
    debug(language_server(protocol), "Command: run_async/1.", []),
    debug(language_server(query),  "   Goal: ~w", [Goal]),
    repeat_until_false((
            query_in_progress(Goal_Thread_ID),
            debug(language_server(protocol), "Draining unretrieved result for ~w", [Goal_Thread_ID]),
            heartbeat_until_result(Goal_Thread_ID, Stream, Unused_Answer),
            debug(language_server(protocol), "Drained result for ~w", [Goal_Thread_ID]),
            debug(language_server(query), "    Discarded answer: ~w", [Unused_Answer])
            )),
    debug(language_server(protocol), "All previous results drained", []),
    send_goal_to_thread(Stream, Goal_Thread_ID, Query_Timeout, Timeout, Goal, Binding_List, Find_All),
    reply(Stream, true([[]])).


% See language_server(Options) documentation for documentation
state_process_command(Stream, Goal_Thread_ID, _, async_result(Timeout), _) :-
    !,
    debug(language_server(protocol), "Command: async_result, timeout: ~w.", [Timeout]),
    (   once((var(Timeout) ; Timeout == -1))
    ->  Options = []
    ;   Options = [timeout(Timeout)]
    ),
    (   query_in_progress(Goal_Thread_ID)
    ->  (   (   debug(language_server(protocol), "Pending query results exist for ~w", [Goal_Thread_ID]),
                get_next_result(Goal_Thread_ID, Stream, Options, Result)
            )
        ->  reply_with_result(Goal_Thread_ID, Stream, Result)
        ;   reply_error(Stream, result_not_available)
        )
   ;    (   debug(language_server(protocol), "No pending query results for ~w", [Goal_Thread_ID]),
            reply_error(Stream, no_query)
        )
   ).


% See language_server(Options) documentation for documentation
% To ensure the goal thread is in a place it is safe to cancel,
% we lock a mutex first that the goal thread checks before exiting
% the "safe to cancel" zone.
% It is not in the safe zone: it either finished
% or was never running.
state_process_command(Stream, Goal_Thread_ID, _, cancel_async, _) :-
    !,
    debug(language_server(protocol), "Command: cancel_async/0.", []),
    with_mutex(Goal_Thread_ID, (
        (   safe_to_cancel(Goal_Thread_ID)
        ->  (   thread_signal(Goal_Thread_ID, throw(cancel_goal)),
                reply(Stream, true([[]]))
            )
        ;   (   query_in_progress(Goal_Thread_ID)
            ->  (   debug(language_server(protocol), "Pending query results exist for ~w", [Goal_Thread_ID]),
                    reply(Stream, true([[]]))
                )
            ;   (   debug(language_server(protocol), "No pending query results for ~w", [Goal_Thread_ID]),
                    reply_error(Stream, no_query)
                )
            )
        )
    )).


% Used for testing how the system behaves when the goal thread is killed unexpectedly
% Needs to run a bogus command `run(true, -1)` to
% get the goal thread to process the exception
state_process_command(Stream, Goal_Thread_ID, Query_Timeout, testThrowGoalThread(Test_Exception), Binding_List) :-
    !,
    debug(language_server(protocol), "TESTING: requested goal thread unhandled exception", []),
    thread_send_message(Goal_Thread_ID, testThrow(Test_Exception)),
    state_process_command(Stream, Goal_Thread_ID, Query_Timeout, run(true, -1), Binding_List).


state_process_command(Stream, _, _, close, _) :-
    !,
    reply(Stream, true([[]])).


state_process_command(Stream, _, _, quit, _) :-
    !,
    reply(Stream, true([[]])).


%  Send an exception if the command is not known
state_process_command(Stream, _, _, Command, _) :-
    debug(language_server(protocol), "Unknown command ~w", [Command]),
    reply_error(Stream, unknownCommand).


% Wait for a result (and put in Answers) from the goal thread, but send a heartbeat message
% every so often until it arrives to detect if the socket is broken.
% Throws if If the heartbeat failed which will
% and then shutdown the communication thread
% Tail recurse to not grow the stack
heartbeat_until_result(Goal_Thread_ID, Stream, Answers) :-
    (   get_next_result(Goal_Thread_ID, Stream, [timeout(2)], Answers)
    ->  debug(language_server(query), "Received answer from goal thread: ~w", [Answers])
    ;   (   debug(language_server(protocol), "heartbeat...", []),
            write_heartbeat(Stream),
            heartbeat_until_result(Goal_Thread_ID, Stream, Answers)
        )
    ).


% True if write succeeded, otherwise throws as that
% indicates that heartbeat failed because the other
% end of the pipe terminated
write_heartbeat(Stream) :-
    put_char(Stream, '.'),
    flush_output(Stream).


% Send a goal to the goal thread in its queue
%
% Remember that we are now running a query using assert.
%   This will be retracted once all the answers have been drained.
%
% If Goal_Thread_ID died, thread_send_message throws and, if we don't respond,
%   the client could hang so catch and give them a good message before propagating
%   the exception
send_goal_to_thread(Stream, Goal_Thread_ID, Default_Timeout, Timeout, Goal, Binding_List, Find_All) :-
    (   var(Timeout)
    ->  Timeout = Default_Timeout
    ;   true
    ),
    (   var(Binding_List)
    ->  Binding_List = []
    ;   true
    ),
    debug(language_server(query),  "Sending to goal thread with timeout = ~w: ~w", [Timeout, Goal]),
    assert(query_in_progress(Goal_Thread_ID)),
    catch(thread_send_message(Goal_Thread_ID, goal(Goal, Binding_List, Timeout, Find_All)), Send_Message_Exception, true),
    (   var(Send_Message_Exception)
    ->  true
    ;   (   reply_error(Stream, connection_failed),
            throw(Send_Message_Exception)
        )
    ).


% Send a result from the goal thread to the communication thread in its queue
send_next_result(Respond_To_Thread_ID, Answer, Exception_In_Goal, Find_All) :-
    (   var(Exception_In_Goal)
    ->  (   (   debug(language_server(query), "Sending result of goal to communication thread, Result: ~w", [Answer]),
                Answer == []
            )
        ->  thread_send_message(Respond_To_Thread_ID, result(false, Find_All))
        ;   thread_send_message(Respond_To_Thread_ID, result(true(Answer), Find_All))
        )
    ;   (   debug(language_server(query), "Sending result of goal to communication thread, Exception: ~w", [Exception_In_Goal]),
            thread_send_message(Respond_To_Thread_ID, result(error(Exception_In_Goal), Find_All))
        )
    ).


% Gets the next result from the goal thread in the communication thread queue,
% and retracts query_in_progress/1 when the last result has been sent.
% Find_All == true only returns one message, so delete query_in_progress
% No matter what it is
% \+ Find_All: There may be more than one result. The first one we hit with any exception
% (note that no_more_results is also returned as an exception) means we are done
get_next_result(Goal_Thread_ID, Stream, Options, Answers) :-
    (   thread_property(Goal_Thread_ID, status(running))
    ->  true
    ;   (   reply_error(Stream, connection_failed),
            throw(connection_failed)
        )
    ),
    thread_self(Self_ID),
    thread_get_message(Self_ID, result(Answers, Find_All), Options),
    (   Find_All
    ->  (   debug(language_server(protocol), "Query completed and answers drained for findall ~w", [Goal_Thread_ID]),
            retractall(query_in_progress(Goal_Thread_ID))
        )
    ;   (   Answers = error(_)
        ->  (   debug(language_server(protocol), "Query completed and answers drained for non-findall ~w", [Goal_Thread_ID]),
                retractall(query_in_progress(Goal_Thread_ID))
            )
        ;   true
        )
    ).


% reply_with_result predicates are used to consistently return
% answers for a query from either run() or run_async()
reply_with_result(_, Stream, error(Error)) :-
    !,
    reply_error(Stream, Error).
reply_with_result(_, Stream, Result) :-
    !,
    reply(Stream, Result).


% Reply with a normal term
% Convert term to an actual JSON string
reply(Stream, Term) :-
    debug(language_server(query), "Responding with Term: ~w", [Term]),
    term_to_json_string(Term, Json_String),
    write_message(Stream, Json_String).


% Special handling for exceptions since they can have parts that are not
% "serializable". Ensures they they are always returned in an exception/1 term
reply_error(Stream, Error_Term) :-
    (   error(Error_Value, _) = Error_Term
    ->  Response = exception(Error_Value)
    ;   (   atom(Error_Term)
        ->
            Response = exception(Error_Term)
        ;   (   compound_name_arity(Error_Term, Name, _),
                Response = exception(Name)
            )
        )
    ),
    reply(Stream, Response).


% Send and receive messages are simply strings preceded by their length + ".\n"
% i.e. "<stringlength>.\n<string>"
% The desired encoding must be set on the Stream before calling this predicate


% Writes the next message.
% Throws if there is an unexpected exception
write_message(Stream, String) :-
    write_string_length(Stream, String),
    write(Stream, String),
    flush_output(Stream).


% Reads the next message.
% Throws if there is an unexpected exception or thread has been requested to quit
% the length passed must match the actual number of bytes in the stream
% in whatever encoding is being used
read_message(Stream, String) :-
    read_string_length(Stream, Length),
    read_string(Stream, Length, String).


% Terminate with '.\n' so we know that's the end of the count
write_string_length(Stream, String) :-
    stream_property(Stream, encoding(Encoding)),
    string_encoding_length(String, Encoding, Length),
    format(Stream, "~d.\n", [Length]).


% Note: read_term requires ".\n" after the length
% ... but does not consume the "\n"
read_string_length(Stream, Length) :-
    read_term(Stream, Length, []),
    get_char(Stream, _).


% converts a string to Codes using Encoding
string_encoding_length(String, Encoding, Length) :-
    setup_call_cleanup(
        open_null_stream(Out),
        (   set_stream(Out, encoding(Encoding)),
            write(Out, String),
            byte_count(Out, Length)
        ),
        close(Out)).


% Convert Prolog Term to a Prolog JSON term
% Add a final \n so that using netcat to debug works well
term_to_json_string(Term, Json_String) :-
    term_to_json(Term, Json),
    with_output_to(string(Json_String),
        (   current_output(Stream),
            json_write(Stream, Json),
            put(Stream, '\n')
        )).


% Execute the goal as once() without binding any variables
% and keep executing it until it returns false (or throws)
repeat_until_false(Goal) :-
    (\+ (\+ Goal)), !, repeat_until_false(Goal).
repeat_until_false(_).


% Used to kill a thread in an "expected" way so it doesn't leave around traces in thread_property/2 afterwards
%
% If the thread is alive OR it was already aborted (expected cases) then attempt to join
%   the thread so that no warnings are sent to the console. Other cases leave the thread for debugging.
% There are some fringe cases (like calling external code)
%   where the call might not return for a long time.  Do a timeout for those cases.
abortSilentExit(Thread_ID, Exception) :-
    catch(thread_signal(Thread_ID, abort), error(Exception, _), true),
    debug(language_server(protocol), "Attempting to abort thread: ~w. thread_signal_exception: ~w", [Thread_ID, Exception]).
% Workaround SWI Prolog bug: https://github.com/SWI-Prolog/swipl-devel/issues/852 by not joining
% The workaround just stops joining the aborted thread, so an inert record will be left if thread_property/2 is called.
%    ,
%    (   once((var(Exception) ; catch(thread_property(Thread_ID, status(exception('$aborted'))), error(_, _), true)))
%    ->  (   catch(call_with_time_limit(4, thread_join(Thread_ID)), error(JoinException1, JoinException2), true),
%            debug(language_server(protocol), "thread_join attempted because thread: ~w exit was expected, exception: ~w", [Thread_ID, error(JoinException1, JoinException2)])
%        )
%    ;   true
%    ).


% Detach a thread that exits with true or false so that it doesn't leave around a record in thread_property/2 afterwards
% Don't detach a thread if it exits because of an exception so we can debug using thread_property/2 afterwards
%
% However, `abort` is an expected exception but detaching a thread that aborts will leave an unwanted
% thread_property/2 record *and* print a message to the console. To work around this,
% the goal thread is always aborted by the communication thread using abortSilentExit.
detach_if_expected(Thread_ID) :-
    thread_property(Thread_ID, status(Status)),
    debug(language_server(protocol), "Thread ~w exited with status ~w", [Thread_ID, Status]),
    (   once((Status = true ; Status = false))
    ->  (   debug(language_server(protocol), "Expected thread status, detaching thread ~w", [Thread_ID]),
            thread_detach(Thread_ID)
        )
    ;   true
    ).


write_output_to_file(File) :-
    debug(language_server(protocol), "Writing all STDOUT and STDERR to file:~w", [File]),
    open(File, write, Stream, [buffer(false)]),
    set_prolog_IO(user_input, Stream, Stream).


% Creates a Unix Domain Socket file in a secured directory.
% Throws if the directory or file cannot be created in /tmp for any reason
% Requirements for this file are:
%    - The Prolog process will attempt to create and, if Prolog exits cleanly,
%           delete this file when the server closes.  This means the directory
%           must have the appropriate permissions to allow the Prolog process
%           to do so.
%    - For security reasons, the filename should not be predictable and the
%           directory it is contained in should have permissions set so that files
%           created are only accessible to the current user.
%    - The path must be below 92 *bytes* long (including null terminator) to
%           be portable according to the Linux documentation
%
% tmp_file finds the right /tmp directory, even on Mac OS, so the path is small
% Set 700 (rwx------)  permission so it is only accessible by current user
% Create a secure tmp file in the new directory
% {set,current}_prolog_flag is copied to a thread, so no need to use a mutex.
% Close the stream so sockets can use it
unix_domain_socket_path(Created_Directory, File_Path) :-
    tmp_file(udsock, Created_Directory),
    make_directory(Created_Directory),
    catch(  chmod(Created_Directory, urwx),
            Exception,
            (   catch(delete_directory(Created_Directory), error(_, _), true),
                throw(Exception)
            )
    ),
    setup_call_cleanup( (   current_prolog_flag(tmp_dir, Save_Tmp_Dir),
                            set_prolog_flag(tmp_dir, Created_Directory)
                        ),
                        tmp_file_stream(File_Path, Stream, []),
                        set_prolog_flag(tmp_dir, Save_Tmp_Dir)
                      ),
    close(Stream).


% Helper for installing the language_server.pl file to the right
% library directory.
% Call using swipl -s language_server.pl -g "language_server:install_to_library('language_server.pl')" -t halt
install_to_library(File) :-
    once(find_library(Path)),
    copy_file(File, Path),
    make.


% Find the base library path, i.e. the one that ends in
% "library/"
find_library(Path) :-
    file_alias_path(library, Path),
    atomic_list_concat(Parts, '/', Path),
    reverse(Parts, Parts_Reverse),
    nth0(0, Parts_Reverse, ''),
    nth0(1, Parts_Reverse, Library),
    string_lower(Library, 'library').