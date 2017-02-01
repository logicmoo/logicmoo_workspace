/*  Part of Extended libraries for Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
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
