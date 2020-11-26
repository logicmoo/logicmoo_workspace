/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1985-2002, University of Amsterdam
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

:- module(pce_image,
          [ pce_image_directory/1
          ]).
:- use_module(library(pce)).
:- require([ absolute_file_name/3
           , atomic_list_concat/2
           , is_absolute_file_name/1
           ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Prepend the given  directory  to  the   image  search-path.  Useful  for
applications defining a private image directory.

Typically one would put all images required   by an application in *.xpm
files in a directory called bitmaps. The   load  file of the application
makes a call

        :- pce_image_directory(bitmaps).

after which the images from the  directory   may  be accessed using -for
example-:

        send(Box, fill_pattern, image('my_image.bm')).

See also the ImageViewer demo  tool  to   get  an  overview of available
images in a directory.

Images and program resources
----------------------------

pce_image_directory/1   prepends   the   given     directory    to   the
file_search_path/2 declarations using the alias   `image'. Especially if
the application should (eventually)  be  turned   into  a  runtime,  the
following skeleton for using images as program resources is adviced:

resource(cute,  image, image('cute.xpm')).

        ...
        new(I, image(resource(cute)),
        ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

pce_image_directory(Dir) :-
    (   atom(Dir)
    ->  (   \+ is_absolute_file_name(Dir),
            prolog_load_context(directory, Cwd)
        ->  atomic_list_concat([Cwd, /, Dir], DirPath)
        ;   DirPath = Dir
        ),
        asserta(user:file_search_path(image, DirPath))
    ;   asserta(user:file_search_path(image, Dir)),
        absolute_file_name(Dir, DirPath,
                           [ file_type(directory),
                             access(read)
                           ])
    ),
    (   compiling,
        get(@display, open, @off)   % don't force the display
                                    % when compiling
    ->  true
    ;   get(class(image), class_variable, path, PathVar),
        get(PathVar, value, Path),
        send(PathVar, value, string('%s:%s', DirPath, Path))
    ).
