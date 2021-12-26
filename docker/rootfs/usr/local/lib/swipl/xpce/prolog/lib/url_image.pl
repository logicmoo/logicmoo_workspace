/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2003-2011, University of Amsterdam
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

:- module(url_image, []).
:- use_module(library(pce)).

resource(noimg, image, image('16x16/noimg.xpm')).

:- pce_autoload(http_client, library(http_client)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Simple class to provide URL-based  images.   Currently  deals  only with
fetching images using the file and http protocols.

Fetched images may be cached in a table using @on for the cache argument
of ->initialise. Creating a url_image  for   the  2nd  time with caching
enabled returns the previously loaded image.

->initialise also provides a no_image argument. This  image is used as a
default image if the system cannot find the required image.

TBD: What to  do/how  to  implement  reload?   Could  we  be  using  the
broadcasting service for this?

This class was designed to be used with the library scaledbitmap.pl.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(url_image, image,
                   "Image whose source comes from a URL").

variable(url,    name, get, "Source of the image").
variable(cache,  bool, get, "Image is cached").
variable(exists, bool, get, "We succeeded loading the image from URL").

initialise(I, URL:url=name, Cache:cache=[bool], NoImage:no_image=[image]*) :->
    "Create image from URL data"::
    send_super(I, initialise),
    (   (   NoImage == @nil
        ->  send(I, load, URL, Cache)
        ;   pce_catch_error(_, send(I, load, URL, Cache))
        )
    ->  send(I, slot, exists, @on)
    ;   send(I, slot, exists, @off),
        (   send(NoImage, instance_of, image)
        ->  send(I, copy, NoImage)
        ;   NoImage == @default
        ->  send_super(I, load, resource(noimg))
        )
    ).

:- pce_global(@url_image_table, new(hash_table)).

lookup(_, URL:name, Cache:[bool], I:url_image) :<-
    "Lookup from image table"::
    Cache \== @off,
    get(@url_image_table, member, URL, I).

unlink(I) :->
    get(I, url, URL),
    (   get(I, cache, @on)
    ->  send(@url_image_table, delete, URL)
    ;   true
    ),
    send_super(I, unlink).

free(I) :->
    "Only free if not referenced"::
    (   get(I, references, 1)       % 1 from hash-table
    ->  send_super(I, free)
    ).

:- pce_group(file).

load(I, URL:name, Cache:[bool]) :->
    "load from URL data"::
    send(I, slot, url, URL),
    (   new(Re, regex('file:(.*)', @off)),
        send(Re, match, URL)
    ->  get(Re, register_value, URL, 1, name, FileName),
        send_super(I, load, FileName)
    ;   send(URL, prefix, 'http:', @on)
    ->  new(HC, http_client(URL)),
        new(TB, text_buffer),
        send(HC, fetch_data, TB),
        send_super(I, load, TB),
        free(TB),
        free(HC)
    ),
    (   Cache == @off
    ->  send(I, slot, cache, @off)
    ;   send(@url_image_table, append, URL, I),
        send(I, slot, cache, @on)
    ).

:- pce_end_class.

