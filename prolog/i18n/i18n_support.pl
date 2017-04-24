/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2014, Process Design Center, Breda, The Netherlands.
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

:- module(i18n_support, [current_pot_file/2,
                         i18n_to_translate/4,
                         i18n_process_term/4,
                         expand_i18n_term/4,
                         i18n_record/4,
                         current_i18n_record/4,
                         i18n_entry_expander/4,
                         i18n_entry/4,
                         reference/2,
                         language_t/1,
                         language/1,
                         i18n_entry_exact/4,
                         show_i18n_terms/1,
                         dictionary/1,
                         variable_name/1,
                         get_lang_file/2,
                         get_lang_file/3,
                         '=~'/2,
                         '~='/2,
                         '=~~'/2]).

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(pairs)).
:- use_module(library(readutil)).
:- use_module(library(clambda)).
:- use_module(library(language_iso)).
:- use_module(library(i18n/i18n_op)).
:- use_module(library(i18n/i18n_parser)).

/*

  Note: For variables, use this pattern: Trans= ~Var, This will allow
  to solve ~/1 at Run-Time and unify the translation of Var with
  Trans.  To force Run-Time translation, use Trans =~ Var. Note that
  =~/2 is the predicate that performs Run-Time translation, while ~/1
  is a term that is expanded to the translation at Compile-Time.

  The system support one po file per several modules, but one module
  can not have several po files.  If there are no user resource file
  defined, nor a custom i18n_resource/1 declaration, the system will
  assume a separated po file per module using the module as base name
  as default using the predicate i18n_support:i18n_resource_dir/1.

  Implementors: To extend current functionality, which nowadays is a
  minimum to cover basic needs, please read this before to start:

  http://www.gnu.org/software/gettext/manual/gettext.html#PO-Files

*/

:- dynamic
    i18n_resource_dir/1,      % Global directory where the resources are stored.
    i18n_record/4,
    language/1,
    dictionary/1.

:- multifile
    i18n_resource_dir/1,
    i18n_resourceterm/2,
    i18n_resource/2,
    i18n_record/4,
    language/1,
    dictionary/1.               % for reverse translations, you can use more
                                % than one dictionary

:- public
    i18n_resourceterm/2.

:- volatile i18n_record/4.  % Only useful during compilation and debugging, save
                            % space in the final binary.

:- multifile variable_name/1. % Name of variable names that set the language
:- dynamic variable_name/1.

% Meta predicate declarations should be placed before its usage to allow correct
% expansion

:- meta_predicate
    i18n_process_term(3,+,?,?),
    '=~'(-,:),
    '~='(-,:),
    '=~~'(-,:),
    i18n_entry(1,+,?,?),
    i18n_entry_exact(1,+,?,?),
    i18n_entry_partial(1,+,?,?),
    expand_i18n_term_trans(4,+,?,-),
    expand_i18n_term_rtrans(4,+,?,-),
    expand_i18n_term(4,+,?,-),
    expand_i18n_term_arg(+,2,+,?,?).

% Some standard places where the language is defined:
variable_name('LC_MESSAGES').
variable_name('LANG').

language_t(Lang) :-
    language_iso(_, _, _, _, Lang, _, _, _, _, _).

%% language(+language_t(Lang)) is multi.
%
% if not defined, assume the system language or English. Although is
% not recommended, you can use several languages to look for
% translations.

language(Lang) :-
    ( %% Guess the system language using environment variables:
      variable_name(AppVariable),
      getenv(AppVariable, X),   % Read from application specific
                                % language variable
      atom_codes(X, C),
      SLang = [_, _],
      append(SLang, _, C),
      atom_codes(Lang, SLang)
    ->true
    ; Lang = en                 % take English by default
    ).

% Operator that Allows Run-Time language translation:

(Engl =~ M:Lang) :-
    i18n_process_term(i18n_entry(language), M, Lang, Engl).

% Run-Time language reverse translation:
(Engl ~= M:Dict) :-
    i18n_process_term(\ X^D^E^i18n_entry(dictionary, X, E, D), M, Dict, Engl).

(Term =~~ M:Term0) :-
    i18n_process_term(i18n_entry_dl, M, Term0, Term).

i18n_entry_dl(M, Dict, Lang) :-
    ( Dict = [S|_], nonvar(S) ->
      i18n_entry(dictionary, M, Engl, Dict),
      i18n_entry(language,   M, Engl, Lang)
    ; i18n_entry(language,   M, Engl, Lang),
      i18n_entry(dictionary, M, Engl, Dict)
    ).

/*
reference(Term, Ref) :-
        '$set_source_module'(M, M),
        ( Term = (Head :- _) -> functor(Head, F, A), PI = F/A
        ; Term = (:- Decl) -> functor(Decl, F, A), PI = (:- F/A)
        ; functor(Term, F, A), PI = F/A
        ),
        with_output_to(codes(Ref), M:PI).
*/

reference(M, [Ref]) :- atom_codes(M, Ref).


i18n_entry(GLang, M, MsgId, MsgStr) :-
    ( i18n_entry_exact(GLang, M, MsgId, MsgStr) -> true
    ; i18n_entry_partial(GLang, M, MsgId, MsgStr)
    ).

i18n_entry_exact_1(Lang, M, X, Y) :-
    once(i18n_record_2(M, Lang, [X], [Y])).

i18n_entry_exact(GLang, M, MsgId, MsgStr) :-
    ( call(GLang, Lang),
      i18n_record_2(M, Lang, MsgId, MsgStr) -> true % 1. full translation
    ; call(GLang, Lang),        % 2. full list translation
      maplist(i18n_entry_exact_1(Lang, M), MsgId, MsgStr) -> true
    ).

i18n_entry_partial(GLang, M, MsgId, MsgStr) :- % 3. partial translation
    maplist(i18n_record_3(M, GLang), MsgId, MsgStr).

i18n_record_2(M, Lang, MsgId, MsgStr) :-
    (M0 = M ; true),            % Be flexible, module is not strict
    i18n_record(M0, Lang, MsgId, MsgStr).

i18n_record_3(M, GLang, MsgId, MsgStr) :-
    ( call(GLang, Lang),
      i18n_record_2(M, Lang, [MsgId], [MsgStr]) -> true
    ; MsgStr = MsgId
    ).

show_i18n_terms(M:Term) :-
    expand_i18n_term(show_i18n_term, M, Term, _).

show_i18n_term(M, Op, MsgId, _) :-
    reference(M, Ref),
    maplist([Op]+\S^format(user_error, '~w~s~n', [Op, S]), Ref),
    nl(user_error),
    writeln(user_error, M),
    maplist([Op]+\S^format(user_error, '~w~s~n', [Op, S]), MsgId),
    nl(user_error).

i18n_entry_expander((~), M, MsgId, MsgStr) :-
    i18n_entry(language, M, MsgId, MsgStr).
i18n_entry_expander((~~), M, MsgId, MsgStr) :-
    i18n_entry_dl(M, MsgId, MsgStr).

expand_i18n_term_trans(_, _, Var0, ~Var1) :-
    var(Var0),
    var(Var1),
    Var0 = Var1,
    !.
expand_i18n_term_trans(Proc, _, M:Term, Translation) :- !,
    expand_i18n_term_trans(Proc, M, Term, Translation).
expand_i18n_term_trans(Proc, M, Term, Translation) :-
    i18n_process_term(call(Proc, (~)), M, Term, Translation).

expand_i18n_term_rtrans(_, _, Var0, ~~Var1) :-
    var(Var0),
    var(Var1),
    Var0 = Var1,
    !.
expand_i18n_term_rtrans(Proc, _, M:Term, Translation) :- !,
    expand_i18n_term_rtrans(Proc, M, Term, Translation).
expand_i18n_term_rtrans(Proc, M, Term, Translation) :-
    i18n_process_term(call(Proc, (~~)), M, Term, Translation).

expand_i18n_term(_, _, Var0, Var1) :-
    var(Var0),
    var(Var1),
    Var0=Var1,
    !.
expand_i18n_term(Proc, _, M:~Term, Translation) :- !,
    expand_i18n_term(Proc, M, ~Term, Translation).
expand_i18n_term(Proc, _, M:~~Term, Translation) :- !,
    expand_i18n_term(Proc, M, ~~Term, Translation).
expand_i18n_term(Proc, M, ~(Term), Translation) :- !,
    expand_i18n_term_trans(Proc, M, Term, Translation).
expand_i18n_term(Proc, M, ~~(Term), Translation) :- !,
    expand_i18n_term_rtrans(Proc, M, Term, Translation).
expand_i18n_term(Proc, M, Term0, Term) :-
    compound(Term0),
    functor(Term0, F, A),
    functor(Term, F, A), !,
    expand_i18n_term_arg(1, Proc, M, Term0, Term).
expand_i18n_term(_, _, Term, Term).

expand_i18n_term_arg(N0, Proc, M, Term0, Term) :-
    arg(N0, Term0, Arg0 ), !,
    arg(N0, Term,  Arg),
    expand_i18n_term(Proc, M, Arg0, Arg),
    succ(N0, N),
    expand_i18n_term_arg(N, Proc, M, Term0, Term).
expand_i18n_term_arg(_, _, _, _, _).

code(Code) :-
    integer(Code),
    Code >= 0,
    Code =< 0x7FFFFFFF,
    code_type(Code, _).

i18n_process_term(Proc, M, Term, Tran) :-
    translation_keys_values(Term, Tran, Keys, Values),
    call(Proc, M, Keys, Values).

translation_keys_values(Term, Tran, Keys, Values) :-
    ( nonvar(Tran)->NVTran=true ; NVTran=fail ),
    ( nonvar(Term)
    ->i18n_to_translate(Term, Tran, KeyValues, []),
      pairs_keys_values(KeyValues, Keys, Values)
    ; true
    ),
    ( NVTran==true
    ->i18n_to_translate(Tran, Term, ValueKeys, []),
      pairs_keys_values(ValueKeys, Values, Keys)
    ; true
    ).

i18n_to_translate(Var, Var) -->
    {var(Var)},
    !.
i18n_to_translate([],   [])   --> !, [].
i18n_to_translate([C|String0], String) -->
    {maplist(code, [C|String0])},
    [[C|String0]-String],
    !.
i18n_to_translate(Term0, Term) -->
    { compound(Term0),
      functor(Term0, F, A),
      functor(Term, F, A)
    },
    !,
    i18n_to_translate_arg(1, Term0, Term).
i18n_to_translate(String0, String) -->
    { string(String0),
      string_codes(String0, Codes0),
      ( string(String) -> string_codes(String, Codes)
      ; freeze(Codes, string_codes(String, Codes))
      )
    },
    [Codes0-Codes],
    !.
i18n_to_translate(Atom0, Atom) -->
    { atom(Atom0),
      atom_codes(Atom0, Codes0),
      ( atom(Atom) -> atom_codes(Atom, Codes)
      ; freeze(Codes, atom_codes(Atom, Codes))
      )
    },
    [Codes0-Codes],
    !.
i18n_to_translate(Term, Term) --> [].

i18n_to_translate_arg(N0, Term0, Term) -->
    { arg(N0, Term0, Arg0),
      !,
      arg(N0, Term, Arg)
    },
    i18n_to_translate(Arg0, Arg),
    {N is N0 + 1},
    i18n_to_translate_arg(N, Term0, Term).
i18n_to_translate_arg(_, _, _) --> [].

% In prolog, reference would be Module:Pred/Name, Module:(TermScheme), ...

%% resourceterm(+Term) is multi.
%
% Declaration. Tells the system that a string must be considered as a
% resource string, even if it don't appears in a ~/1 operator.

:- multifile user:prolog_file_type/2.
:- dynamic   user:prolog_file_type/2.

user:prolog_file_type(pot, pot).

/*
% Performance bug: this reads the file every time the predicate is
% consulted, tabling would be useful for this case. --EMM

:- table i18n_record/4.         % Speed up, decrease complexity
i18n_record(M, Lang, MsgId, MsgStr) :-
    ( current_module(M) *->true ; true ),
    current_i18n_record(M, Lang0, MsgId0, MsgStr0 ),
    Lang = Lang0, MsgId = MsgId0, MsgStr = MsgStr0.

:- table current_i18n_record/4.
*/
current_i18n_record(M, Lang, MsgId, MsgStr) :-
    ( language(Lang)
    ; dictionary(Lang),
      \+ language(Lang)
    ),
    Lang \= en,
    current_pot_file(M, PotFile),
    reference(M, Ref),
    get_lang_file(PotFile, Lang, PoFile),
    access_file(PoFile, read),
    read_file_to_codes(PoFile, Codes, []),
    parse_po_entries(Entries, Codes, []),
    member(Entry, Entries),
    valid_entry(Ref, M, Entry),
    Entry = entry(_, _, _, _, MsgId, MsgStr).

valid_entry(Ref, M, Entry) :-
    Entry \= entry(_, _, _, _, _, [""]),
    ( M = user -> true
    ; Entry = entry(_, _, Ref, _, _, _)
    ).

get_lang_file(PotFile, PoFile) :-
    language(Lang), Lang \= en,
    get_lang_file(PotFile, Lang, PoFile).

get_lang_file(PotFile, Lang, PoFile) :-
    atom_concat(PotBase, '.pot', PotFile),
    atom_concat(PotBase, '_', PotBase_),
    atom_concat(PotBase_, Lang, PoBase),
    atom_concat(PoBase, '.po', PoFile),
    !.

% A pot file can contain info for several modules, but a module can
% use only one pot file

:- multifile current_i18n_module/1. % Module that uses the i18n support
:- dynamic current_i18n_module/1.

current_pot_file(M, PotFile) :-
    (var(M) -> current_i18n_module(M) ; true),
    ( i18n_resource(M, PotAlias) -> true
    ; ( i18n_resource_dir(DirAlias)
      ->absolute_file_name(DirAlias, DirName),
        PotAlias = DirName/M
      ; module_property(M, file(File)),
        file_name_extension(PotAlias, _, File)
      ; PotAlias = '.'/M
      )
    ),
    absolute_file_name(PotAlias, PotFile, [file_type(pot)]).
