/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/projects/xpce/
    Copyright (c)  1985-2011, University of Amsterdam
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

:- module(emacs_buffer, []).
:- use_module(library(pce)).
:- require([ between/3
           , default/3
           , ignore/1
           ]).

:- pce_begin_class(emacs_buffer(file, name), text_buffer).

variable(name,            name,         get,  "Name of this buffer").
variable(directory,       directory,    both, "Associated CWD").
variable(file,            file*,        get,  "Associated file").
variable(prompt_reload,   bool := @on,  both, "Prompt before reloading").
variable(mode,            name,         get,  "Major mode of operation").
variable(time_stamp,      date*,        get,  "Time-stamp for file").
variable(ensure_newline,  bool := @on,  both, "Add newline when done").
variable(ensure_no_whitespace_errors,
                          bool,         both, "Remove trailing whitespace when done").
variable(tab_width,       int := 8,     both, "Width of a tab").
variable(auto_save_mode,  bool,         both, "Auto-save?").
variable(auto_save_count, number,       get,  "Auto-save at expiration").
variable(saved_caret,     int,          both, "Saved caret on last quit").
variable(saved_fill,      bool := @off, both, "Saved fill_mode on quit").
variable(margin_width,    '0..' := 0,   get,  "Margin width of editors").
variable(coloured_generation,
         int := -1,
         both,
         "Last generation of the text-buffer that was coloured").
variable(xref_generation,
         int := -1,
         both,
         "Last generation we analysed").

class_variable(undo_buffer_size,      int, 40000).
class_variable(ensure_no_whitespace_errors, bool, @on).
class_variable(newline_existing_file, {posix,dos,detect}, detect).
class_variable(newline_new_file,      {posix,dos},        posix).
:- if(current_prolog_flag(windows, true)).
class_variable(newline_new_file,      {posix,dos},        dos).
class_variable(unicode_encoding,      {utf8,unicode_le,unicode_be}, unicode_le).
:- else.
class_variable(newline_new_file,      {posix,dos},        posix).
class_variable(unicode_encoding,      {utf8,unicode_le,unicode_be}, utf8).
:- endif.

initialise(B, File:file*, Name:[name]) :->
    "Create from file and name"::
    send(B, send_super, initialise),
    send(B, saved_caret, 0),

    (   File == @nil
    ->  send(B, undo_buffer_size, 0),
        send(B, auto_save_mode, @off),
        default(Name, '*scratch*', BufBaseName),
        (   BufBaseName == '*scratch*'
        ->  send(B, slot, mode, prolog),
            scratch_text(Text),
            send(B, insert, 0, Text),
            send(B, saved_caret, B?size)
        ;   send(B, slot, mode, fundamental)
        ),
        send(B, directory, directory('.'))
    ;   send(File, absolute_path),
        get(File, base_name, FileBaseName),
        default(Name, FileBaseName, BufBaseName),
        send(B, file, File),
        send(B, auto_save_mode, @on),
        send(@emacs_base_names, append, FileBaseName, B),
        send(B, determine_initial_mode),
        (   object(@emacs_mark_list)
        ->  ignore(send(@emacs_mark_list, loaded_buffer, B))
        ;   true
        )
    ),

    send(B, init_mode_defaults),
    send(B, slot, auto_save_count, number(300)),
    send(B, name, BufBaseName).


unlink(B) :->
    "Remove from buffer-list and base_name table"::
    send(@emacs_buffers, delete, B?name),
    (   get(B, file, File), File \== @nil
    ->  send(@emacs_base_names, delete, File?base_name, B)
    ;   true
    ),
    send(B, send_super, unlink).

report(B,
       Kind:kind={status,inform,progress,done,warning,error,fatal},
       Format:format=[char_array],
       Argv:any ...) :->
    "Report to associated editors"::
    get(B, editors, Editors),
    Message =.. [message, @arg1, report, Kind, Format | Argv],
    send(Editors, for_all, Message).


scratch_text('% This buffer is for notes you don\'t want to save.\n\c
              % If you want to create a file, visit that file with C-x C-f,\n\c
              % then enter the text in that file\'s own buffer.\n\n').

:- pce_global(@emacs_interpreter_regex,
              new(regex('#!(\\S+)\\s'))).
:- pce_global(@emacs_mode_regex,        % -*- Mode -*-
                                        % -*- mode: Mode; ... -*-
              new(regex('.*-\\*-\\s*([Mm]ode:\\s*(\\w+);.*-\\*-|(\\w+)\\s*-\\*-)'))).


% ->determine_initial_mode uses the following steps:
%
%   1. If the file is a loaded file, it is a Prolog file
%   2. The Emacs magic sequences -*- Mode -*- or -*- mode: Mode; ... -*-
%   3. Try @emacs_content_mode_list
%   4. Try #! interpreter (PrologScript)
%   5. Try the file-name

determine_initial_mode(B) :->
    "Determine initial mode"::
    (   get(B, file, File), File \== @nil,
        get(File, name, FileName),
        absolute_file_name(FileName, FilePath),
        source_file(FilePath),
        \+ source_file_property(FilePath, derived_from(_,_))
    ->  send(B, slot, mode, prolog)
    ;   send(@emacs_mode_regex, match, B),
        member(Reg, [2,3]),
        get(@emacs_mode_regex, register_value, B, Reg, Mode0),
        get(Mode0?downcase, value, Mode),
        get(@pce, convert, Mode, emacs_mode, _ModeObject)
    ->  send(B, slot, mode, Mode)
    ;   content_from_mode(B, Mode)
    ->  send(B, slot, mode, Mode)
    ;   (   send(@emacs_interpreter_regex, match, B),
            get(@emacs_interpreter_regex, register_value, B, 1, Match),
            To = @emacs_interpreter_mode_list
        ;   get(B, file, File),
            get(File, base_name, Match),
            To = @emacs_mode_list
        ),
        get(To?members, find,
            message(@arg1?name, match, Match), Att)
    ->  send(B, slot, mode, Att?value)
    ;   send(B, slot, mode, @emacs_default_mode)
    ),
    send(B, set_temp_file).

set_temp_file(B) :->
    "Clear ->prompt_reload if this is a temp file"::
    get(B, file, File),
    (   no_backup(File)
    ->  send(B, prompt_reload, @off)
    ;   true
    ).


%!  content_from_mode(+Buffer, -Mode) is semidet.
%
%   Search Buffer with the patterns from @emacs_content_mode_list

content_from_mode(B, Mode) :-
    get(@emacs_content_mode_list?members, find,
         message(@arg1?name?first, search, B,
                 0, @arg1?name?second),
        Att),
    get(Att, value, Mode).


attach(B, E:editor) :->
    "A new editor is attached.  Prepare it"::
    get(B, editors, Editors),
    (   send(Editors, empty)
    ->  get(B, saved_caret, Caret),
        get(B, saved_fill, Fill),
        get(B, tab_width, TabWidth)
    ;   get(Editors?head, caret, Caret),
        get(Editors?head, fill_mode, Fill),
        get(Editors?head, tab_distance, TabWidth)
    ),
    get(B, margin_width, MW),
    send(B, send_super, attach, E),
    send(E, caret, Caret),
    send(E, fill_mode, Fill),
    send(E, margin_width, MW),
    send(E, tab_distance, TabWidth).


detach(B, E:editor) :->
    "An editor is detached"::
    get(B, editors, Editors),
    (   get(Editors, size, 1)
    ->  send(B, saved_caret, E?caret),
        send(B, saved_fill, E?fill_mode)
    ;   true
    ),
    send(B, send_super, detach, E).


name(B, Name:name) :->
    "Rename buffer to name"::
    get(B, name, OldName),
    (   Name == OldName
    ->  true
    ;   (   get(@emacs_buffers, member, Name, _)
        ->  between(2, 1000000, N),
            get(Name, append, string('<%d>', N), BufName),
            \+ get(@emacs_buffers, member, BufName, _),
            !
        ;   BufName = Name
        ),
        send(B, slot, name, BufName),
        (   OldName \== @nil,
            get(@emacs_buffers, member, OldName, DictItem)
        ->  send(DictItem, key, BufName)
        ;   send(@emacs_buffers, append, dict_item(BufName, @default, B))
        ),
        send(B, update_label),
        send(B?editors, for_some, message(@arg1?frame, label, BufName))
    ).


lookup(_Ctx, File:file*, Name:[name], Buffer:emacs_buffer) :<-
    "Lookup in name and file-table"::
    (   Name \== @default,
        get(@emacs_buffers, member, Name, DictItem),
        get(DictItem, object, Buffer)
    ->  true
    ;   File \== @nil,
        get(@emacs_base_names, member, File?base_name, Chain),
        get(Chain, find, message(@arg1?file, same, File), Buffer)
    ;   File \== @nil,
        send(File, exists),
        send(File, check_object),
        get(File, object, Buffer),
        send(Buffer, instance_of, emacs_buffer),
        get(Buffer, name, BufName),
        send(Buffer, slot, name, ''),
        send(Buffer, name, BufName),
        send(Buffer, slot, file, File),
        send(@emacs_base_names, append, File?base_name, Buffer),
        send(Buffer, reset_undo),
        send(Buffer, modified, @off),
        send(Buffer, slot, time_stamp, File?time),
        send(Buffer, loaded)
    ).


                 /*******************************
                 *           LOAD/SAVE          *
                 *******************************/

file(B, File:file) :->
    "Switch to indicated file"::
    send(B, clear),
    (   send(directory(File?name), exists)
    ->  send(File, error, open_file, read, 'is a directory')
    ;   send(File, exists)
    ->  get(B, newline_existing_file, OpenMode),
        send(File, newline_mode, OpenMode),
        ignore(send(B, insert_file, 0, File)),
        send(B, reset_undo),
        send(B, modified, @off),
        send(B, slot, time_stamp, File?time)
    ;   send(B, reset_undo),
        send(B, modified, @off),
        get(B, newline_new_file, Mode),
        send(File, newline_mode, Mode)
    ),
    send(B, slot, file, File),
    new(F2, file(File?absolute_path)),
    send(B, directory, F2?directory_name).


save(B, File:[file]) :->
    "->do_save and update time_stamp"::
    (   File == @default
    ->  get(B, file, SaveFile),
        (   SaveFile == @nil
        ->  send(B, report, error, 'No file associated to this buffer'),
            fail
        ;   true
        )
    ;   SaveFile = File,
        (   get(B, file, OldFile), OldFile \== @nil
        ->  send(@emacs_base_names, delete, OldFile?base_name, B)
        ;   true
        ),
        send(File, absolute_path),
        get(File, base_name, BaseName),
        send(B, slot, file, File),
        send(B, directory, File?directory_name),
        send(B, name, BaseName),
        send(@emacs_base_names, append, File?base_name, B)
    ),
    (   get(B, ensure_newline, @on)
    ->  send(B, complete_last_line)
    ;   true
    ),
    (   get(B, ensure_no_whitespace_errors, @on)
    ->  send(B, fix_whitespace_errors)
    ;   true
    ),
    (   no_backup(SaveFile)
    ->  true
    ;   ignore(send(SaveFile, backup))
    ),
    send(B, do_save, SaveFile),
    send(B, slot, time_stamp, SaveFile?time),
    (   object(@emacs_mark_list)
    ->  ignore(send(@emacs_mark_list, saved_buffer, B))
    ;   true
    ).

no_backup(File) :-
    get(@emacs_no_backup_list, find,
        message(@arg1, match, File?name), _).


complete_last_line(B) :->
    "Add \\n if needed"::
    get(B, size, Size),
    (   (   Size == 0
        ;   get(B, character, Size-1, 10)
        )
    ->  true
    ;   send(B, append, string('\n'))
    ).

fix_whitespace_errors(B) :->
    "Remove trailing spaces and tabs from lines"::
    new(Count, number(0)),
    send(B, fix_trailing_space_errors, Count),
    send(B, fix_space_tab_errors, Count),
    (   get(Count, value, 0)
    ->  true
    ;   send(B, report, status,
             'Fixed %d whitespace errors', Count)
    ).

fix_trailing_space_errors(B, Count:number) :->
    "Remove trailing spaces and tabs from lines"::
    new(Re, regex('[ \t]+\n')),
    send(Re, for_all, B,
         and(message(@arg1, replace, @arg2, '\n'),
             message(Count, plus, 1))).

fix_space_tab_errors(B, Count:number) :->
    "Replace space+tab sequences"::
    new(Re, regex(' +\t')),
    send(Re, for_all, B,
         and(message(B, fix_space_tab, Re),
             message(Count, plus, 1))).

fix_space_tab(B, Re:regex) :->
    "Fix matched spaces followed by tab"::
    (   get(B?editors, head, E)
    ->  get(Re, register_start, 0, Start),
        get(Re, register_end, 0, End),
        get(E, column, Start, StartCol),
        get(E, column, End, EndCol),
        get(E, tab_distance, TD),
        tabs(StartCol, EndCol, TD, Tabs),
        tab_atom(Tabs, Atom),
        send(Re, replace, B, Atom)
    ;   true
    ).

tabs(SC, EC, _, 0) :-
    SC >= EC,
    !.
tabs(SC, EC, TD, N) :-
    SC2 is ((SC+TD)//TD)*TD,
    tabs(SC2, EC, TD, N0),
    N is N0+1.

tab_atom(N, Atom) :-
    length(List, N),
    maplist(=(0'\t), List),
    atom_codes(Atom, List).


do_save(B, SaveFile:file, Start:[int], Length:[int]) :->
    "Do the actual saving"::
    get(B, unicode_encoding, FallBackEncoding),
    (   pce_catch_error(io_error,
                        send_super(B, save, SaveFile, Start, Length))
    ->  true
    ;   get(SaveFile, name, FileName),
        \+ access_file(FileName, write)
    ->  send(B, report, error, 'Cannot write %s (permission denied)', FileName),
        fail
    ;   get(SaveFile, encoding, Encoding),
        Encoding \== FallBackEncoding
    ->  send(SaveFile, encoding, FallBackEncoding),
        send(SaveFile, bom, @on),
        send_super(B, save, SaveFile, Start, Length),
        once(user_encoding(FallBackEncoding, UserEnc)),
        send(B, report, warning,
             'Could not save using default locale; saved using %s', UserEnc)
    ;   send_super(B, save, SaveFile, Start, Length)
    ).

user_encoding(utf8, 'UTF-8').
user_encoding(unicode_le, 'UTF-16 (little endian)').
user_encoding(unicode_be, 'UTF-16 (big endian)').
user_encoding(Enc, Enc).


write_region(B, File:file, Start:int, Length:int) :->
    "Wrote region to file (start, length)"::
    send(B, do_save, File, Start, Length).


save_if_modified(B, Confirm:[bool]) :->
    "Save if associated with a file and modified"::
    (   get(B, modified, @on),
        get(B, file, File), File \== @nil
    ->  (   (   Confirm == @off
            ;   send(@display, confirm,
                     '%s is modified.  Save?', File?name)
            )
        ->  send(B, save)
        ;   fail
        )
    ;   true
    ).


                 /*******************************
                 *           AUTO-SAVE          *
                 *******************************/

check_auto_save(B) :->
    "Check whether to auto_save"::
    (   get(B, modified, @on),
        get(B, auto_save_count, C),
        send(C, minus, 1),
        send(C, equal, 0),
        get(B, auto_save_mode, @on)
    ->  send(B, auto_save)
    ;   true
    ).


auto_save_file(B, F:file) :<-
    get(B, file, File), File \== @nil,
    get(File, backup_file_name, '#', Name),
    new(F, file(Name)).


auto_save(B) :->
    "Auto-save the buffer (when file)"::
    (   get(B, auto_save_file, File)
    ->  send(B, report, status, 'Auto saving ...'),
        send(@display, flush),
        ignore(send(B, send_super, save, File, 0, B?size)),
        send(B?auto_save_count, value, 300),
        send(B, report, status, 'Auto saving ... done')
    ;   true
    ).


delete_auto_save_file(B) :->
    "Delete the autosave-file if present"::
    (   get(B, auto_save_file, File)
    ->  ignore(send(File, remove))
    ;   true
    ).


                 /*******************************
                 *            KILL              *
                 *******************************/

kill(B) :->
    "->save_if_modified and ->free"::
    (   get(B, modified, @off)
    ->  send(B, free)
    ;   get(B, file, File), File \== @nil, \+ get(B, size, 0)
    ->  new(D, dialog('Kill modified buffer?')),
        send(D, append, new(L, label(reporter))),
        send(L, format, 'Buffer %s is modified', B?name),
        send(D, append,
             button('save & kill', message(D, return, save_and_kill))),
        send(D, append,
             button(kill, message(D, return, kill))),
        send(D, append,
             button(cancel, message(D, return, cancel))),
        get(D, confirm_centered, Rval),
        send(D, destroy),
        (   Rval == save_and_kill
        ->  send(B, save),
            send(B, free)
        ;   Rval == kill
        ->  send(B, free)
        ;   fail
        )
    ;   send(B, free)
    ).


revert(B) :->
    "Reload associated file"::
    get(B, file, File),
    (   File == @nil
    ->  send(B, report, warning, 'No file'),
        fail
    ;   new(Carets, chain),
        get(B, editors, Editors),
        send(Editors, for_all, message(Carets, append, @arg1?caret)),
        new(@emacs_reverting, object), % avoid trap
        send(B, file, File),
        send(Editors, for_all,
             and(message(@arg1, caret, Carets?head),
                 message(Carets, delete_head))),
        (   get(Editors, head, First)
        ->  send(First?mode, auto_colourise_buffer)
        ;   true
        ),
        free(@emacs_reverting),
        send(B, report, status, 'Reloaded %s', File?absolute_path)
    ).


                 /*******************************
                 *          NAME/LABEL          *
                 *******************************/

update_label(B) :->
    "Update label in the buffer-menu"::
    get(B, name, Name),
    (   Name \== @nil
    ->  get(@emacs_buffers, member, Name, DictItem),
        (   get(B, modified, @on)
        ->  send(DictItem, label, string('%s\t**', Name)),
            new(EditorLabel, string('%s [modified]', Name))
        ;   send(DictItem, label, Name),
            EditorLabel = Name
        ),
        send(B?editors, for_all,
             message(@arg1, label, EditorLabel))
    ;   true
    ).


                 /*******************************
                 *            MARGINS           *
                 *******************************/

margin_width(B, W:'0..') :->
    "Set width of the margin for associated editors"::
    send(B, slot, margin_width, W),
    send(B?editors, for_all,
         message(@arg1, margin_width, W)).


                 /*******************************
                 *           MODIFIED           *
                 *******************************/

modified(B, Val:bool) :->
    "Check the file; mark buffer-menu"::
    send_super(B, modified, Val),
    (   Val == @on
    ->  send(B, check_modified_file)
    ;   send(B, delete_auto_save_file)
    ),
    send(B, update_label).


check_modified_file(B, Frame:frame=[frame], Confirm:confirm=[bool]) :->
    "Check if file has been modified after buffer"::
    (   get(B, file, File),
        File \== @nil,
        send(File, exists),
        get(B, time_stamp, Stamp),
        get(File, time, FileStamp),
        \+ send(Stamp, equal, FileStamp),
        \+ object(@emacs_reverting)
    ->  (   confirm_reload(B, Frame, Confirm, File)
        ->  send(B, revert)
        ;   true
        )
    ;   true
    ).

confirm_reload(_, _, @off, _) :- !.
confirm_reload(B, _, @default, _) :-
    get(B, prompt_reload, @off),
    !,
    send(B, saved_caret, 0),
    send(B?editors, for_all, message(@arg1, caret, 0)).
confirm_reload(_, Frame, _, File) :-
    new(D, dialog('Modified file')),
    send(D, append,
         label(title,  string('File %N was modified', File))),
    send(D, append,
         button(reload_file, message(D, return, reload_file))),
    send(D, append,
         button(edit_buffer, message(D, return, edit_buffer))),
    (   Frame \== @default
    ->  EmacsFrame = Frame
    ;   get(@emacs, current_frame, EmacsFrame)
    ->  true
    ;   EmacsFrame = @nil
    ),
    (   EmacsFrame \== @default
    ->  get(EmacsFrame?area, center, Position),
        send(D, transient_for, EmacsFrame)
    ;   Position = @default
    ),
    get(D, confirm_centered, Position, RVal),
    send(D, destroy),
    RVal == reload_file.


                 /*******************************
                 *          OPEN WINDOW         *
                 *******************************/

open(B, How:[{here,tab,window}], Frame:emacs_frame) :<-
    "Create window for buffer"::
    (   How == window
    ->  send(new(Frame, emacs_frame(B)), open)
    ;   How == tab,
        get(@emacs, current_frame, Frame)
    ->  send(Frame, tab, B, @on),
        send(Frame, expose)
    ;   get(@emacs, current_frame, Frame)
    ->  send(Frame, buffer, B),
        send(Frame, expose)
    ;   send(new(Frame, emacs_frame(B)), open)
    ),
    send(B, check_modified_file, Frame).

open(B, How:[{here,tab,window}]) :->
    "Create window for buffer"::
    get(B, open, How, _).


                 /*******************************
                 *            MODE              *
                 *******************************/

mode(B, Mode:name) :->
    "Switch to named mode"::
    (   get(B, mode, Mode)
    ->  true
    ;   send(B, slot, mode, Mode),
        send(B, init_mode_defaults),
        send(B?editors, for_some, message(@arg1, mode, Mode))
    ).

init_mode_defaults(B) :->
    "Initialise defaults from the current mode"::
    get(B, mode, ModeName),
    atomic_list_concat([emacs_, ModeName, '_mode'], ClassName),
    get(@pce, convert, ClassName, class, ModeClass),
    (   copy_class_var(Name),
        get(ModeClass, class_variable, Name, CV),
        get(CV, value, Value),
        send(B, Name, Value),
        fail
    ;   true
    ).

copy_class_var(indent_tabs).
copy_class_var(tab_width).


                 /*******************************
                 *       LANGUAGE SUPPORT       *
                 *******************************/

%       emacs_buffer<-name_and_arity returns the name and arity if the
%       caret is in the functor of the term.  If the arity cannot be
%       determined, arity is returned as @default.

name_and_arity(TB, Pos:int, Tuple:tuple) :<-
    "Find name and arity of term at position"::
    (   get(TB, character, Pos, C0)
    ;   get(TB, character, Pos-1, C0)
    ),
    send(TB?syntax, has_syntax, C0, word),
    !,
    get(TB, scan, Pos, word, 0, start, P1),
    get(TB, scan, P1, word, 0, end, P2),
    get(TB, contents, P1, P2-P1, NameString),
    (   get(TB, character, P2, 0'()
    ->  P4 is P2 + 1,
        (   count_args(TB, P4, 0, 0, Arity)
        ->  true
        ;   Arity = @default
        )
    ;   Arity = 0
    ),
    new(Tuple, tuple(NameString?value, Arity)).


count_args(TB, Here, _, _, _) :-
    get(TB, size, Here),
    !,
    fail.
count_args(_TB, _Here, 20, _, _) :-
    !,
    fail.
count_args(TB, Here, NAT, A0, A) :-
    get(TB, scan, Here, term, 1, EndTerm),
    get(TB, skip_comment, EndTerm, Next),
    (   get(TB, character, Next, 0'))
    ->  A is A0 + 1
    ;   get(TB, character, Next, 0',)
    ->  A1 is A0 + 1,
        count_args(TB, EndTerm, 0, A1, A)
    ;   NNAT is NAT + 1,
        count_args(TB, EndTerm, NNAT, A0, A)
    ).

:- pce_end_class.


