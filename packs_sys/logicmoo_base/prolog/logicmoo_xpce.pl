:- module(logicmoo_xpce, []).

:- set_prolog_flag(generate_debug_info, true).


/** <module> Associate XPCE with SWI-Prolog Utility LOGICMOO XPCE

Loads swi-prolog reference libraries. 
This  file  initialises  XPCE,  the  SWI-Prolog   native  GUI.  XPCE  is
initialised only if it is detected.

The source-location of this file  is packages/xpce/swipl/swipl-rc. It is
installed as <plbase>/<exe-base>.rc, where   <exe-base> is =|swipl-win|=
to associate with the SWI-Prolog gui  application on Windows and =swipl=
on Unix/X11 platforms.

- @author Douglas R. Miles
- @license LGPL 
*/


:- op(200, fy,  user:(@)).
:- op(250, yfx, user:(?)).
:- op(990, xfx, user:(:=)).

:- multifile
        user:file_search_path/2.

:- dynamic
        pcehomestore_/1.
:- volatile
        pcehomestore_/1.

pcehome_(Home) :-
        pcehomestore_(Home), !.
pcehome_(Home) :-
        (   getenv('XPCEHOME', RawHome)
        ;   current_prolog_flag(home, PlHome),
            (   current_prolog_flag(xpce_version, Version),
                atom_concat('/xpce-', Version, Suffix)
            ;   Suffix = '/xpce'
            ),
            atom_concat(PlHome, Suffix, RawHome)
        ),
        exists_directory(RawHome), !,
        absolute_file_name(RawHome, Home),
        asserta(pcehomestore_(Home)).

user:file_search_path(pce, PceHome) :-
        pcehome_(PceHome).
user:file_search_path(library, pce('prolog/lib')).
user:file_search_path(foreign, pce(ArchLib)) :-
        current_prolog_flag(arch, Arch),
        atom_concat('lib/', Arch, ArchLib).

% We added a directory to the autoload directories: force reloading the
% index
:- reload_library_index.

gui_setup_ :-
        current_prolog_flag(gui, true), !.
gui_setup_ :-
        (   getenv('DISPLAY', D), D \== ''
        ;   current_prolog_flag(windows, true)
        ), !,
        create_prolog_flag(gui, true, []),
        menu_setup_,
        editor_setup,
        load_files(user:library(swi_hooks), [silent(true)]).    % help, etc.

menu_setup_ :-                                  % plwin.exe menus
        current_prolog_flag(console_menu, true),
        load_files(user:library(win_menu), [silent(true)]).
menu_setup_.

editor_setup :-
        current_prolog_flag(editor, default), !,
        set_prolog_flag(editor, pce_emacs).
editor_setup.

pce_setup_ :-
        current_prolog_flag(xpce, true), !.
pce_setup_ :-
        current_prolog_flag(argv, Argv),
        \+ memberchk('--nopce', Argv),  % explicitely no XPCE
        pcehome_(PceHome),
        exists_directory(PceHome),
        gui_setup_,
        (   memberchk('--pce', Argv)
        ;   current_prolog_flag(executable, Executable),
            file_base_name(Executable, Base),
            sub_atom_icasechk(Base, _, pce)
        ), !,
        load_files(user:library(pce), [silent(true)]).
pce_setup_.

% :- initialization pce_setup_.

