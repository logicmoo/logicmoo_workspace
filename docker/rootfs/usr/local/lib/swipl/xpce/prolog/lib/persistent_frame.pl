/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2002-2013, University of Amsterdam
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

:- module(persistent_frame, []).
:- use_module(library(pce)).
:- use_module(library(pce_config)).
:- require([ context_module/1,
	     chain_list/2,
	     member/2,
	     maplist/3
	   ]).

/** <module> Save/restore layout of XPCE windows

This library defines the class  persistent_frame,   a  subclass of class
frame remembering its geometry  and  optionally   (by  default  on)  the
subwindow layout.

This class cooperates with the   library(pce_config),  a generic package
for managing application preferences.  It collects the locations of user
frames in the file <profile-dir>/Geometry.cnf

Geometry information is stored in  the   internal  configuration DB (see
library(pce_config))  if  a  frame  is  closed   or  on  exit  from  the
application. The internal database is written   to  tehe above mentioned
file on exit from the application.

Somehow the system must identify the frame   to decide which geometry to
use. This is done using the  <->geometry_key.   If  not set, this is the
classname or, if the class is not subclassed   it  is the <-label of the
frame.

Exploiting this library is very simple,  just make your toplevel windows
for  which  you  want  the  geometry  remembered  a  subclass  of  class
persistent_frame rather than class frame.  Note   that  this implies you
have to create your frame explitely:

    ==
        ...
        new(F, persistent_frame('Pretty Application')),
        send(F, geometry_key, pretty_app),
        send(F, append, new(D, dialog)),
        send(new(V, view), right, D),
        ...
    ==
*/

:- pce_begin_class(persistent_frame, frame, "Frame remembering location").

variable(persistent_subwindow_layout, bool := @on, get,
         "Remember the layout of the subwindows?").
variable(geometry_key,                name*, send,
         "Key used to identify this frame").

unlink(F) :->
    "Save layout and destroy"::
    send(F, save_layout),
    send_super(F, unlink).

create(F) :->
    "Create and restore layout"::
    send_super(F, create),
    ignore(send(F, load_layout)).

:- pce_group(config).

geometry_key(F, Key:name) :<-
    "Name to store geometry"::
    (   get(F, slot, geometry_key, Key),
        Key \== @nil
    ->  true
    ;   get(F, class_name, Key),
        Key \== persistent_frame
    ->  true
    ;   get(F, label, Key)
    ).

save_layout(F) :->
    "Save current layout in config DB"::
    get(F, geometry, Geometry),
    get(F, geometry_key, Key),
    set_config(history/geometry/Key, Geometry),
    (   get(F, persistent_subwindow_layout, @on),
        get(F, tile, RootTile),
        get(RootTile, members, Members),
        Members \== @nil,
        get_tile_layout(RootTile, Layout),
        Layout \== *
    ->  set_config(history/subwindow_layout/Key, Layout)
    ;   true
    ).

load_layout(F) :->
    load_geometry_config,
    get(F, geometry_key, Key),
    (   get_config(history/geometry/Key, Geometry)
    ->  send(F, geometry, Geometry)
    ;   true
    ),
    (   get(F, persistent_subwindow_layout, @on),
        get_config(history/subwindow_layout/Key, Layout)
    ->  get(F, tile, RootTile),
        apply_tile_layout(RootTile, Layout)
    ;   true
    ).

%!  get_tile_layout(+Tile, -Layout)
%
%   Create a Prolog term representing the subwindow (tile) layout.
%   Note that we only save the width/height of resizeable subwindows,
%   leaving the others to the application.  This ensures proper behaviour
%   if the application is modified.

get_tile_layout(T, layout(Me, SubLayout)) :-
    get(T, members, Members),
    Members \== @nil,
    chain_list(Members, List),
    maplist(get_tile_layout, List, SubLayout),
    get_this_tile_layout(T, Me),
    has_specifier(layout(Me, SubLayout)),
    !.
get_tile_layout(T, Me) :-
    get_this_tile_layout(T, Me).

get_this_tile_layout(T, Size) :-
    get(T, can_resize, @on),
    !,
    get(T, area, A),
    (   get(T?super, orientation, horizontal)
    ->  get(A, width, Size)
    ;   get(A, height, Size)
    ).
get_this_tile_layout(_, *).

%!  has_specifier(+Layout)
%
%   See whether there is a specification somewhere, otherwise there
%   is no use storing it.

has_specifier(layout(Size, _)) :-
    Size \== *,
    !.
has_specifier(layout(_, Subs)) :-
    !,
    has_specifier(Subs).
has_specifier(X) :-
    integer(X),
    !.
has_specifier(Subs) :-
    member(Sub, Subs),
    has_specifier(Sub),
    !.


%!  apply_tile_layout(+Tile, +Layout)
%
%   Apply a previously saved layout description, sending ->width
%   or ->height messages to resizeable tiles.

apply_tile_layout(T, layout(Me, SubLayout)) :-
    !,
    apply_this_tile_layout(T, Me),
    (   get(T, members, Members),
        Members \== @nil
    ->  chain_list(Members, List),
        maplist(apply_tile_layout, List, SubLayout)
    ;   true
    ).
apply_tile_layout(T, Me) :-
    apply_this_tile_layout(T, Me).

apply_this_tile_layout(_, *) :- !.
apply_this_tile_layout(T, Size) :-
    get(T, super, Super),
    Super \== @nil,
    !,
    (   get(Super, orientation, horizontal)
    ->  get(T?area, width, W0),
        (   Size > W0
        ->  get(T, hor_stretch, S)
        ;   get(T, hor_shrink, S)
        ),
        (   S > 0
        ->  send(T, width, Size)
        ;   true
        )
    ;   get(T?area, height, H0),
        (   Size > H0
        ->  get(T, ver_stretch, S)
        ;   get(T, ver_shrink, S)
        ),
        (   S > 0
        ->  send(T, height, Size)
        ;   true
        )
    ).
apply_this_tile_layout(_, _).

:- pce_end_class(persistent_frame).


                 /*******************************
                 *          EXIT HOOKS          *
                 *******************************/

:- initialization
   send(@pce, exit_message,
    message(@display?frames,
            for_some,
            if(message(@arg1, instance_of, persistent_frame),
               message(@arg1, save_layout)))).


                 /*******************************
                 *         CONFIG HOOKS         *
                 *******************************/

config(config/file,
       [ default('Geometry')
       ]).
config(history/geometry/_Key,
       [ type(geometry),
         editable(false),
         comment('(X-)geometry for persistent frames')
       ]).
config(history/subwindow_layout/_Key,
       [ type(subwindow_layout),
         editable(false),
         comment('Sub-window layout for persistent frames')
       ]).

:- register_config(config).

load_geometry_config :-
    context_module(M),
    ensure_loaded_config(M:_).
