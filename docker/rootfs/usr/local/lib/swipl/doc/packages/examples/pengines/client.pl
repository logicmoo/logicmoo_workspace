:- module(pengine_demo,
          [ rpc_demo/2,                 % +Port, -Solution
            pengine_demo/0,
            pengine_demo/1              % +Port
          ]).
:- use_module(library(pengines)).

/** <module> Access pengines from Prolog
*/

%!  rpc_demo(Port, X)
%
%   True when X is a  result  of   the  query  below executed on the
%   pengines server at http://localhost:<Port>.
%
%     ==
%     member(X, [aap, noot, mies]
%     ==
%
%   To run this, start a Prolog process loading server.pl and start
%   the server using e.g.,
%
%     ==
%     ?- server(4040).
%     ==
%
%   Now, start a second Prolog on the same host, load this file and
%   run
%
%     ==
%     ?- rpc_demo(4040, X).
%     X = aap ;
%     X = noot ;
%     X = mies ;
%     false.
%     ==

rpc_demo(Port, X) :-
    format(atom(URL), 'http://localhost:~d', [Port]),
    pengine_rpc(URL, member(X, [aap, noot, mies])).

%!  pengine_demo is det.
%!  pengine_demo(+Port) is det.
%
%   Create a pengine, run a  simple  goal   in  it  and  process the
%   pengine events to collect  the  solutions.   If  Port  is given,
%   create a pengine on the server http://localhost:<Port>, else use
%   a _local_ pengine.

pengine_demo :-
    pengine_create(
        [ src_text("
\t        q(X) :- p(X).
                p(a). p(b). p(c).
\t      ")
        ]),
    pengine_event_loop(handle, []).

pengine_demo(Port) :-
    format(atom(URL), 'http://localhost:~d', [Port]),
    pengine_create(
        [ server(URL),
          src_text("
\t        q(X) :- p(X).
                p(a). p(b). p(c).
\t      ")
        ]),
    pengine_event_loop(handle, []).


handle(create(ID, _)) :-
    pengine_ask(ID, q(_X), []).
handle(success(ID, X, false)) :-
    !,
    writeln(X),
    pengine_destroy(ID).
handle(success(ID, X, true)) :-
    writeln(X),
    pengine_next(ID, []).
