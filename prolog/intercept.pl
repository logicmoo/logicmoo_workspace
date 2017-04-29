/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.
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

:- module(intercept, [intercept/3,
                      send_signal/1]).

:- meta_predicate intercept(0,+,0).
intercept(Goal, Signal, Handler) :-
    call(Goal),
    keep_on_frame(Signal-Handler).

keep_on_frame(_).

send_signal(Signal) :-
    var(Signal), !,
    throw(error(instantiation_error, signal/1 -1)).
send_signal(Signal) :-
    prolog_current_frame(Frame),
    find_parent_handler(Frame, Signal, IFrame, Handler, []),
    call_handler(IFrame, Handler),
    keep_on_frame(Frame).

call_handler(IFrame, Handler) :-
    call(Handler),
    keep_on_frame(IFrame).

find_parent_handler(Frame, Signal, IFrame, Handler, SFrameL) :-
    prolog_frame_attribute(Frame, parent, Parent),
    ( member(Parent, SFrameL)
    ->find_parent_handler(Parent, Signal, IFrame, Handler, SFrameL)
    ; prolog_frame_attribute(Frame, goal, Goal),
      ( Goal \= call_handler(_, _)
      ->( Goal \= intercept(_, Signal, Handler)
        ->find_parent_handler(Parent, Signal, IFrame, Handler, SFrameL)
        ; copy_term(Goal, intercept(_, Signal, Handler)),
          IFrame = Parent
        )
      ; Goal = call_handler(SkipFrame, _),
        find_parent_handler(Parent, Signal, IFrame, Handler,
                            [SkipFrame|SFrameL])
      )
    ),
    keep_on_frame(Parent), !.
find_parent_handler(_, Signal, _, _, _) :-
    throw(error(unintercepted_signal(Signal), signal/1 -1)).
