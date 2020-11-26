/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2015, University of Amsterdam
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

:- module(http_image,
          [ reply_image/2               % +Image, +Options
          ]).
:- use_module(library(readutil)).
:- use_module(library(pce)).

/** <module> Serve dynamically generated images through XPCE
*/

%!  reply_image(+Image, +Options)
%
%   Formulate a CGI reply from an XPCE graphical.  This call handles
%   anything that can be converted into a pixmap object, notably any
%   XPCE graphical object.
%
%   Currently the only option recognised   is content_type(+Type) to
%   specify the type. image/jpeg and image/gif are the only sensible
%   values. The default is to generate gif.
%
%   If this module is used as a server on X11-based systems the user
%   must ensure the presence of  an   X11  server.  The XPCE library
%   'Xserver' provides code to start a `head-less' (i.e. server that
%   doesn't  need  a  physical  display)    server  and  adjust  the
%   environment to make XPCE use this server.

% (*) Note that this code uses  a   text_buffer  as intermediate for the
% data. this is pretty dubious as binary data is not well supported this
% way. It still works, but only when using newline(posix) for Windows.

reply_image(Image, Options) :-
    (   memberchk(content_type(Type), Options)
    ->  image_format(Type, ImgType)
    ;   Type = image/gif,
        ImgType = gif
    ),
    get(@pce, convert, Image, pixmap, Pixmap),
    new(TB, text_buffer),
    send(TB, undo_buffer_size, 0),
    send(Pixmap, save, TB, ImgType),
    format('Content-type: ~w~n~n', [Type]),
    pce_open(TB, read, Data),
    set_stream(Data, newline(posix)),       % (*)
    copy_stream_data(Data, current_output),
    close(Data),
    free(TB),
    (   Pixmap \== Image
    ->  free(Pixmap)
    ;   true
    ).

image_format(image/Type, Type) :- !.
image_format(Format, Type) :-
    atom_concat('image/', Type, Format).

