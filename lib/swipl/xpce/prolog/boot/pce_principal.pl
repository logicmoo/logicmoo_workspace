/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.nu.nl
    WWW:           http://www.swi-prolog.nl/projects/xpce/
    Copyright (c)  1985-2019, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
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

:- module(pce_principal,
          [ new/2, free/1,

            send/2, send/3, send/4, send/5, send/6, send/7,
            send/8,

            get/3, get/4, get/5, get/6, get/7, get/8,

            send_class/3,
            get_class/4,

            object/1, object/2,

            pce_class/6,
            pce_lazy_send_method/3,
            pce_lazy_get_method/3,
            pce_uses_template/2,

            pce_method_implementation/2,

            pce_open/3,                 % +Object, +Mode, -Stream
            in_pce_thread/1,            % :Goal
            set_pce_thread/0,
            pce_dispatch/0,

            pce_postscript_stream/1,    % -Stream

            op(200, fy,  @),
            op(250, yfx, ?),
            op(800, xfx, :=)
          ]).
:- autoload(library(apply),[convlist/3,maplist/2]).
:- autoload(library(lists),[member/2,last/2,reverse/2]).
:- autoload(library(shlib),[load_foreign_library/1]).
:- autoload(library(swi_compatibility),[pce_info/1]).
:- autoload(library(system),[unlock_predicate/1]).

:- public
    in_pce_thread_sync2/2.

:- meta_predicate
    send_class(+, +, :),
    send(+, :),
    send(+, :, +),
    send(+, :, +, +),
    send(+, :, +, +, +),
    send(+, :, +, +, +, +),
    send(+, :, +, +, +, +, +),

    get_class(+, +, :, -),
    get(+, :, -),
    get(+, :, +, -),
    get(+, :, +, +, -),
    get(+, :, +, +, +, -),
    get(+, :, +, +, +, +, -),
    get(+, :, +, +, +, +, +, -),

    new(?, :).

                /********************************
                *             HOME              *
                ********************************/

%!  pce_home(-Home) is det.
%
%   True when Home is the home directory of XPCE.

pce_home(PceHome) :-
    absolute_file_name(pce('.'), PceHome,
                       [ file_type(directory),
                         file_errors(fail)
                       ]),
    exists_directory(PceHome),
    !.
pce_home(PceHome) :-
    getenv('XPCEHOME', PceHome),
    exists_directory(PceHome),
    !.
pce_home(PceHome) :-
    (   current_prolog_flag(xpce_version, Version),
        atom_concat('/xpce-', Version, Suffix)
    ;   Suffix = '/xpce'
    ),
    absolute_file_name(swi(Suffix), PceHome,
                       [ file_type(directory),
                         file_errors(fail)
                       ]),
    exists_directory(PceHome),
    !.
pce_home(PceHome) :-
    current_prolog_flag(saved_program, true),
    !,
    (   current_prolog_flag(home, PceHome)
    ->  true
    ;   current_prolog_flag(executable, Exe)
    ->  file_directory_name(Exe, PceHome)
    ;   PceHome = '.'
    ).
pce_home(_) :-
    throw(error(pce_error(no_home), _)).

%!  xpce_application_dir(-Dir)
%
%   Set the directory for storing user XPCE configuration and data.

xpce_application_dir(Dir) :-
    create_config_directory(user_app_config(xpce), Dir),
    !.
xpce_application_dir(Dir) :-
    expand_file_name('~/.xpce', [Dir]).


%!  create_config_directory(+Alias, -Dir) is semidet.
%
%   Try to find an  existing  config   directory  or  create a writeable
%   config directory below a directory owned   by this process. If there
%   are multiple possibilities, create the one   that requires the least
%   number of new directories.

create_config_directory(Alias, Dir) :-
    member(Access, [write, read]),
    absolute_file_name(Alias, Dir0,
                       [ file_type(directory),
                         access(Access),
                         file_errors(fail)
                       ]),
    !,
    Dir = Dir0.
create_config_directory(Alias, Dir) :-
    findall(Candidate,
            absolute_file_name(Alias, Candidate,
                               [ solutions(all),
                                 file_errors(fail)
                               ]),
            Candidates),
    convlist(missing, Candidates, Paths),
    member(_-Create, Paths),
    catch(maplist(make_directory, Create), _, fail),
    !,
    last(Create, Dir).

missing(Dir, Len-Create) :-
    missing_(Dir, Create0),
    reverse(Create0, Create),
    length(Create, Len).

missing_(Dir, []) :-
    exists_directory(Dir),
    access_file(Dir, write),
    '$my_file'(Dir),
    !.
missing_(Dir, [Dir|T]) :-
    file_directory_name(Dir, Parent),
    Parent \== Dir,
    missing_(Parent, T).


                /********************************
                *           LOAD C-PART         *
                ********************************/

init_pce :-
    catch(load_foreign_library(foreign(pl2xpce)),
          error(Error, _Context),           % suppress stack trace
          (   print_message(error, error(Error, _)),
              fail
          )),
    pce_home(Home),
    xpce_application_dir(AppDir),
    pce_init(Home, AppDir),
    !,
    create_prolog_flag(xpce, true, []),
    thread_self(Me),
    assert(pce:pce_thread(Me)).
init_pce :-
    print_message(error, error(pce_error(init_failed), _)),
    halt(1).

:- initialization(init_pce, now).

:- noprofile((send_implementation/3,
              get_implementation/4,
              send_class/3,
              get_class/4,
              new/2,
              send/2,
              get/3)).


                /********************************
                *          PROLOG LAYER         *
                ********************************/


%!  free(+Ref) is det.
%
%   Delete object if it exists.

free(Ref) :-
    object(Ref),
    !,
    send(Ref, free).
free(_).


%!  send(+Object, +Selector, +Arg...) is semidet.
%
%   Succeeds if sending a message to Object with Selector and the
%   given Arguments succeeds. Normally, goal_expansion/2 expands all
%   these goals into send(Receiver, Method(Args...)).

send(Receiver, M:Selector, A1) :-
    functor(Message, Selector, 1),
    arg(1, Message, A1),
    send(Receiver, M:Message).

send(Receiver, M:Selector, A1, A2) :-
    functor(Message, Selector, 2),
    arg(1, Message, A1),
    arg(2, Message, A2),
    send(Receiver, M:Message).

send(Receiver, M:Selector, A1, A2, A3) :-
    functor(Message, Selector, 3),
    arg(1, Message, A1),
    arg(2, Message, A2),
    arg(3, Message, A3),
    send(Receiver, M:Message).

send(Receiver, M:Selector, A1, A2, A3, A4) :-
    functor(Message, Selector, 4),
    arg(1, Message, A1),
    arg(2, Message, A2),
    arg(3, Message, A3),
    arg(4, Message, A4),
    send(Receiver, M:Message).

send(Receiver, M:Selector, A1, A2, A3, A4, A5) :-
    functor(Message, Selector, 5),
    arg(1, Message, A1),
    arg(2, Message, A2),
    arg(3, Message, A3),
    arg(4, Message, A4),
    arg(5, Message, A5),
    send(Receiver, M:Message).

send(Receiver, M:Selector, A1, A2, A3, A4, A5, A6) :-
    functor(Message, Selector, 6),
    arg(1, Message, A1),
    arg(2, Message, A2),
    arg(3, Message, A3),
    arg(4, Message, A4),
    arg(5, Message, A5),
    arg(6, Message, A6),
    send(Receiver, M:Message).


%!  get(+Object, :Selector, +Arg..., ?Rval) is semidet.
%
%   See the comments with send/[3-12].

get(Receiver, M:Selector, A1, Answer) :-
    functor(Message, Selector, 1),
    arg(1, Message, A1),
    get(Receiver, M:Message, Answer).

get(Receiver, M:Selector, A1, A2, Answer) :-
    functor(Message, Selector, 2),
    arg(1, Message, A1),
    arg(2, Message, A2),
    get(Receiver, M:Message, Answer).

get(Receiver, M:Selector, A1, A2, A3, Answer) :-
    functor(Message, Selector, 3),
    arg(1, Message, A1),
    arg(2, Message, A2),
    arg(3, Message, A3),
    get(Receiver, M:Message, Answer).

get(Receiver, M:Selector, A1, A2, A3, A4, Answer) :-
    functor(Message, Selector, 4),
    arg(1, Message, A1),
    arg(2, Message, A2),
    arg(3, Message, A3),
    arg(4, Message, A4),
    get(Receiver, M:Message, Answer).

get(Receiver, M:Selector, A1, A2, A3, A4, A5, Answer) :-
    functor(Message, Selector, 5),
    arg(1, Message, A1),
    arg(2, Message, A2),
    arg(3, Message, A3),
    arg(4, Message, A4),
    arg(5, Message, A5),
    get(Receiver, M:Message, Answer).


                 /*******************************
                 *           NEW SEND           *
                 *******************************/

:- multifile
    send_implementation/3,
    get_implementation/4.

%!  send_implementation(+Id, +Message, +Object)
%
%   Method-bodies are compiled into clauses for this predicate. Id
%   is a unique identifier for the implementation, Message is a
%   compound whose functor is the method name and whose arguments
%   are the arguments to the method-call. Object is the receiving
%   object.

send_implementation(true, _Args, _Obj).
send_implementation(fail, _Args, _Obj) :- fail.
send_implementation(once(Id), Args, Obj) :-
    send_implementation(Id, Args, Obj),
    !.
send_implementation(spy(Id), Args, Obj) :-
    (   current_prolog_flag(debug, true)
    ->  trace,
        send_implementation(Id, Args, Obj)
    ;   send_implementation(Id, Args, Obj)
    ).
send_implementation(trace(Id), Args, Obj) :-
    pce_info(pce_trace(enter, send_implementation(Id, Args, Obj))),
    (   send_implementation(Id, Args, Obj)
    ->  pce_info(pce_trace(exit, send_implementation(Id, Args, Obj)))
    ;   pce_info(pce_trace(fail, send_implementation(Id, Args, Obj)))
    ).


%!  get_implementation(+Id, +Message, +Object, -Return)
%
%   As send_implementation/3, but for get-methods.

get_implementation(true, _Args, _Obj, _Rval).
get_implementation(fail, _Args, _Obj, _Rval) :- fail.
get_implementation(once(Id), Args, Obj, Rval) :-
    get_implementation(Id, Args, Obj, Rval),
    !.
get_implementation(spy(Id), Args, Obj, Rval) :-
    (   current_prolog_flag(debug, true)
    ->  trace,
        get_implementation(Id, Args, Obj, Rval)
    ;   get_implementation(Id, Args, Obj, Rval)
    ).
get_implementation(trace(Id), Args, Obj, Rval) :-
    pce_info(pce_trace(enter, get_implementation(Id, Args, Obj, Rval))),
    (   get_implementation(Id, Args, Obj, Rval)
    ->  pce_info(pce_trace(exit, get_implementation(Id, Args, Obj, Rval)))
    ;   pce_info(pce_trace(fail, get_implementation(Id, Args, Obj, Rval))),
        fail
    ).

%       SWI-Prolog: make this a normal user (debug-able) predicate.

pce_ifhostproperty(prolog(swi), [
(:- unlock_predicate(send_implementation/3)),
(:- unlock_predicate(get_implementation/4)),
(:- '$set_predicate_attribute'(send_implementation(_,_,_),  hide_childs, false)),
(:- '$set_predicate_attribute'(get_implementation(_,_,_,_), hide_childs, false))
                   ]).


                 /*******************************
                 *          DECLARATIONS        *
                 *******************************/

:- multifile
    pce_class/6,
    pce_lazy_send_method/3,
    pce_lazy_get_method/3,
    pce_uses_template/2.


                 /*******************************
                 *            @PROLOG           *
                 *******************************/

:- initialization
   (object(@prolog) -> true ; send(@host, name_reference, prolog)).
