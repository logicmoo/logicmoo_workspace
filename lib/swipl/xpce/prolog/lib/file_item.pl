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

:- module(pce_file_item, []).
:- use_module(library(pce)).

:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).


                 /*******************************
                 *        CLASS SAVE-FILE       *
                 *******************************/

:- pce_begin_class(save_file, file,
                   "File as a destination (type-check only)").

convert(_, Name:name, SaveFile:save_file) :<-
    new(SaveFile, save_file(Name)).

:- pce_end_class(save_file).


                 /*******************************
                 *       FILE COMPLETION        *
                 *******************************/

:- pce_begin_class(file_item, text_item,
                   "text_item with file-name completion").

variable(exists,    'bool|{open,save}' := @off, both, "File must exist").
variable(directory, directory*,                 both, "Relative to").

initialise(FI, Name:[name], Def:[any|function], Msg:[code]*) :->
    clean_file_name(Def, Clean),
    send(FI, send_super, initialise, Name, Clean, Msg),
    send(FI, style, combo_box).


activate(FI, Val:bool) :->
    (   Val == @on
    ->  true
    ;   send(FI, send_super, activate, Val)
    ).


selected_completion(FI, Component:char_array, _Apply:[bool]) :->
    send(FI, send_super, selected_completion, Component, @off),
    get(FI, selection, @off, Selection),
    (   send(directory(Selection), exists)
    ->  send(FI, append, '/'),
        send(FI, caret, @default)
    ;   send(file(Selection), exists)
    ->  send(FI, apply, @on)
    ;   true
    ).


local_path(FI, In:char_array, Out:name) :<-
    (   get(FI, directory, Dir),
        Dir \== @nil
    ->  get(Dir, file_name, In, Out)
    ;   Out = In
    ).


completions(FI, Tuple:tuple, Matches:chain) :<-
    "Chain with completions of FileName in DirName"::
    get(Tuple, first, DirName0),
    get(Tuple, second, FileName),
    new(Matches, chain),
    new(Re, regex((string('^%s', FileName)))),
    (   send(class(file), has_feature, case_sensitive, @off)
    ->  send(Re, ignore_case, @on)
    ;   true
    ),
    (   is_absolute_file_name(DirName0)
    ->  DirName = DirName0
    ;   get(FI, local_path, DirName0, DirName)
    ),
    send(directory(DirName), scan, Matches, Matches, Re),
    send(Matches, delete_all, '.'),
    send(Matches, delete_all, '..').


split_completion(_FI, Value, Tuple:tuple) :<-
    "Split the current entry"::
    new(S, string('%s', Value)),
                                            % delete ...// or .../~
    get(S, size, L),
    (   get(regex('//|~|\\w:[/\\\\]'), search, S, L, 0, Start)
    ->  send(S, delete, 0, Start),
        (   send(S, prefix, '//')
        ->  send(S, delete, 0, 1)
        ;   true
        )
    ;   true
    ),

    (   send(S, suffix, /)
    ->  get(S, value, Path),
        BaseName = ''
    ;   new(F, file(S)),
        get(F, directory_name, DirName),
        get(F, base_name, BaseName),
        make_path(DirName, Path)
    ),
    new(Tuple, tuple(Path, BaseName)).

make_path('', '') :- !.
make_path('.', '') :- !.
make_path(Path, WithSlash) :-
    get(Path, ensure_suffix, /, WithSlash).


indicate_directory(_FI, Dir:string) :->
    (   send(directory(Dir), exists)
    ->  send(Dir, ensure_suffix, /)
    ;   true
    ).


selection(FI, Warn:[bool], FileName:name) :<-
    "Get the current selection"::
    get(FI, modified, Modified),
    get(FI?value_text, string, RawName),
    get(RawName, size, L),
    (   get(regex('//|/~'), search, RawName, L, 0, Start)
    ->  new(S, string('%s', RawName)),
        send(S, delete, 0, Start),
        (   send(S, prefix, '//')
        ->  send(S, delete, 0, 1)
        ;   true
        ),
        get(S, value, FileName0)
    ;   get(RawName, value, FileName0)
    ),
    get(FI, local_path, FileName0, FileName),
    (   (   Warn == @off
        ;   Modified == @off
        )
    ->  true
    ;   get(FI, exists, Exists),
        (   (   Exists == @on
            ;   Exists == open
            )
        ->  (   send(FI, check_existence, FileName)
            ->  true
            ;   send(FI, report, warning,
                     'File %s does not exist', FileName),
                fail
            )
        ;   Exists == save
        ->  (   send(FI, check_existence, FileName)
            ->  send(FI?display, confirm, 'Overwrite file %s?', FileName)
            ;   true
            )
        ;   true
        )
    ).


check_existence(_FI, Name:name) :->
    "Check existence of file"::
    send(file(Name), exists).

clean_file_name(Def, Clean) :-
    \+ send(Def, '_instance_of', function),
    get(@pce, convert, Def, string, Clean),
    !,
    send(regex(//), for_all, Clean,
         message(@arg1, replace, @arg2, '/')).
clean_file_name(Def, Def).


browse(FI) :->
    "Run finder to fill with value"::
    get(FI?value, value, Sofar),
    (   exists_directory(Sofar)
    ->  Dir = Sofar
    ;   file_directory_name(Sofar, Dir)
    ),
    get(FI, exists, Exists),
    get(@finder, file, Exists, directory := Dir, New),
    send(FI, value, New),
    send(FI, apply, @on).

:- pce_end_class(file_item).


                 /*******************************
                 *     CLASS DIRECTORY_ITEM     *
                 *******************************/

:- pce_begin_class(directory_item, file_item).

completions(_FI, Tuple:tuple, Matches:chain) :<-
    "Chain with completions of FileName in DirName"::
    get(Tuple, first, DirName),
    get(Tuple, second, FileName),
    get(directory(DirName), directories, string('^%s', FileName), Matches).


check_existence(_FI, Name:name) :->
    "Check existence of directory"::
    send(directory(Name), exists).

browse(DI) :->
    "Run finder to fill with value"::
    get(DI?value, value, Sofar0),
    (   Sofar0 == ''
    ->  Sofar = '.'
    ;   Sofar = Sofar0
    ),
    (   send(@display, has_get_method, win_directory)
    ->  get(@display, win_directory, @default, Sofar, DirName),
        send(DI, value, DirName),
        send(DI, apply, @on)
    ;   send_super(DI, browse)
    ).

:- pce_end_class.
