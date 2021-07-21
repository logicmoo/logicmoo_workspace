/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2013, University of Amsterdam
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

:- module(pce_server,
          [ pce_server/1
          ]).

:- meta_predicate(pce_server(:)).

:- use_module(library(pce)).
:- require([ atom_to_term/3
           , strip_module/3
           , term_to_atom/2
           ]).

%!  pce_server(+Address)
%
%   Create a PCE socket and interpret incomming lines as Prolog goals.
%   The Address argument is one of:
%
%           * Atom
%           Unix-domain socket.  Atom is used as filename
%           * Integer
%           Internet socket.  Integer is the port number
%
%   Output send to current_output is send to the client, as are error
%   messages
%
%   @see    The SWI-Prolog library(prolog_server) implements a telnet
%           server that provides a Prolog toplevel.

pce_server(Address) :-
    strip_module(Address, Module, Addr),
    new(S, socket(Addr)),
    send(S, attribute, attribute(module, Module)),
    send(S, attribute, attribute(prompt, '(pce) ')),
    attach_messages(S),
    send(S, listen).


attach_messages(S) :-
    send(S, input_message,
         message(@prolog, call_atom, @receiver, @arg1)),
    send(S, accept_message,
         message(@arg1, format, S?prompt)).


call_atom(Socket, Command) :-
    get(Socket, module, Module),
    get(Socket, prompt, Prompt),
    get(Command, value, CommandAtom),
    (   CommandAtom == ''
    ->  send(Socket, format, '\n%s', Prompt)
    ;   (   catch(atom_to_term(CommandAtom, Term, Bindings), _, fail)
        ->  (   Term == exit
            ->  send(Socket, close)
            ;   current_output(Old),
                pce_open(Socket, append, SockStream),
                set_output(SockStream),
                (   catch(call(Module:Term), E, true)
                ->  flush_output,
                    (   var(E)
                    ->  write_bindings(Bindings, Socket),
                        send(Socket, format, 'yes\n%s', Prompt)
                    ;   message_to_string(E, Message),
                        send(Socket, format, 'ERROR: %s\n%s',
                             Message, Prompt)
                    )
                ;   flush_output,
                    send(Socket, format, 'no\n%s', Prompt)
                ),
                set_output(Old),
                close(SockStream)
            )
        ;   send(Socket, format, 'Syntax error\n%s', Prompt)
        )
    ).


write_bindings([], _) :-
    flush_output.
write_bindings([Name = Value|Rest], Socket) :-
    format('    ~w = ~p~n', [Name, Value]),
    write_bindings(Rest, Socket).
