/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/packackes/xpce/
    Copyright (c)  1985-2019, University of Amsterdam
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

:- module(emacs, []).

:- use_module(library(pce)).
:- use_module(library(emacs_extend)).
:- require([ send_list/3
           ]).

:- multifile
    no_backup/1.
:- multifile
    user:file_search_path/2,
    default_emacs_mode/2.


                 /*******************************
                 *   PUT USER EXTENSIONS HERE   *
                 *******************************/

user:file_search_path(emacs_user_library,
                      app_config('xpce/emacs')).


                 /*******************************
                 *           PROLOG             *
                 *******************************/

:- new(@loading_emacs, object).
                                        % SWI-Prolog extensions
pce_ifhostproperty(prolog(swi),
                   (:- ensure_loaded(user:library('emacs/swi_prolog')))).


                 /*******************************
                 *          LIBRARIES           *
                 *******************************/

:- pce_autoload(file_item, library(file_item)).
:- pce_autoload(save_file, library(file_item)).


                 /*******************************
                 *          KERNEL FILES        *
                 *******************************/

:- consult(window).
:- consult(buffer).
:- consult(application).
:- consult(buffer_menu).
:- consult(server).
:- consult(history).
:- consult(fundamental_mode).
:- consult(language_mode).
:- consult(outline_mode).
:- consult(bookmarks).
:- consult(help_buffer).


                 /*******************************
                 *       AUTOLOAD CLASSES       *
                 *******************************/

:- pce_autoload(emacs_hit_list,         library('emacs/hit_list')).
:- pce_autoload(emacs_process_buffer,   library('emacs/shell')).
:- pce_autoload(emacs_gdb_buffer,       library('emacs/gdb')).
:- pce_autoload(emacs_annotate_buffer,  library('emacs/annotate_mode')).


                 /*******************************
                 *            MODES             *
                 *******************************/




%:- declare_emacs_mode(outline, library('emacs/outline_mode')).
%:- declare_emacs_mode(language,library('emacs/language_mode')).
:- declare_emacs_mode(prolog,   library('emacs/prolog_mode')).
:- declare_emacs_mode(xsb,      library('emacs/xsb_mode')).
:- declare_emacs_mode(chr,      library('emacs/chr_mode')).
:- declare_emacs_mode(latex,    library('emacs/latex_mode')).
:- declare_emacs_mode(logtalk,  library('emacs/logtalk_mode')).
%:- declare_emacs_mode(html,    library('emacs/html_mode')).
:- declare_emacs_mode(java,     library('emacs/java_mode')).
:- declare_emacs_mode(javascript, library('emacs/javascript_mode')).
:- declare_emacs_mode(c,        library('emacs/c_mode')).
:- declare_emacs_mode(cpp,      library('emacs/cpp_mode')).
:- declare_emacs_mode(script,   library('emacs/script_mode')).
:- declare_emacs_mode(man,      library('emacs/man_mode')).
:- declare_emacs_mode(text,     library('emacs/text_mode')).
:- declare_emacs_mode(annotate, library('emacs/annotate_mode')).
:- declare_emacs_mode(gdb,      library('emacs/gdb')).
:- declare_emacs_mode(sgml,     library('emacs/sgml_mode')).
:- declare_emacs_mode(xml,      library('emacs/sgml_mode')).
:- declare_emacs_mode(html,     library('emacs/sgml_mode')).
:- declare_emacs_mode(rdf,      library('emacs/rdf_mode')).
:- declare_emacs_mode(rdfs,     library('emacs/rdf_mode')).
:- declare_emacs_mode(owl,      library('emacs/rdf_mode')).
:- declare_emacs_mode(turtle,   library('emacs/turtle_mode')).
:- declare_emacs_mode(yaml,     library('emacs/yaml_mode')).
:- declare_emacs_mode(cmake,    library('emacs/cmake_mode')).


                 /*******************************
                 *     EMACS GLOBAL OBJECTS     *
                 *******************************/

:- pce_global(@emacs_base_names,
              new(chain_table)).                  % file-base --> buffers
:- pce_global(@emacs_buffers,
              new(dict)).                         % name --> buffer object
:- pce_global(@emacs_modes,
              new(hash_table)).                   % name --> mode object
:- pce_global(@emacs,
              new(emacs(@emacs_buffers))).
:- pce_global(@emacs_default_mode, new(var(value := script))).
:- pce_global(@emacs_mode_list, make_emacs_mode_list).
:- pce_global(@emacs_interpreter_mode_list, make_emacs_interpreter_mode_list).
:- pce_global(@emacs_content_mode_list, make_emacs_content_mode_list).
:- pce_global(@emacs_no_backup_list, make_no_backup_list).

make_emacs_mode_list(Sheet) :-
    new(Sheet, sheet),
    (   send(class(file), has_feature, case_sensitive, @off)
    ->  CaseSensitive = @on
    ;   CaseSensitive = @off
    ),
    (   default_emacs_mode(Regex, Mode),
           send(Sheet, value, regex(Regex, CaseSensitive), Mode),
        fail
    ;   true
    ).

%!  default_emacs_mode(+Regex, -Mode) is nondet.
%
%   True if Mode is the  PceEmacs  mode   associated  with  a  file that
%   matches Regex. This is a multifile predicate that can be extended to
%   support additional mappings.

default_emacs_mode('.*\\.pl~?$',                   prolog).
default_emacs_mode('.*\\.plu~?$',                  prolog).
default_emacs_mode('.*\\.plt~?$',                  prolog).
default_emacs_mode('\\.swiplrc~?',		   prolog).
default_emacs_mode('.*\\.P~?$',                    xsb).
default_emacs_mode('\\.yap~?',                     prolog).
default_emacs_mode('.*\\.chr~?$',                  chr).
default_emacs_mode('.*\\.(tex|sty)~?$',            latex).
default_emacs_mode('.*\\.doc~?$',                  latex).
default_emacs_mode('.*\\.lgt~?$',                  logtalk).
default_emacs_mode('.*\\.html~?$',                 html).
default_emacs_mode('.*\\.php[0-9]?~?$',            html).
default_emacs_mode('.*\\.sgml~?$',                 sgml).
default_emacs_mode('.*\\.xml~?$',                  xml).
default_emacs_mode('.*\\.ttl~?$',                  turtle).
default_emacs_mode('.*\\.rdf~?$',                  rdf).
default_emacs_mode('.*\\.rdfs~?$',                 rdfs).
default_emacs_mode('.*\\.owl~?$',                  owl).
default_emacs_mode('.*\\.ann~?$',                  annotate).
default_emacs_mode('.*\\.[ch]~?$',                 c).
default_emacs_mode('.*\\.java~?$',                 java).
default_emacs_mode('.*\\.js~?$',                   javascript).
default_emacs_mode('.*\\.C$',                      cpp).
default_emacs_mode('.*\\.cc$',                     cpp).
default_emacs_mode('.*\\.cpp$',                    cpp).
default_emacs_mode('.*\\.idl$',                    cpp).
default_emacs_mode('.*\\.yaml~?$',                 yaml).
default_emacs_mode('.*\\.cmake~?$|CMakeLists.txt', cmake).
default_emacs_mode('.*\\.txt~?$',                  text).
default_emacs_mode('.*\\.md~?$',                   text).
default_emacs_mode('.*\\.eml~?$',                  text).
default_emacs_mode('[Cc]ompose|README|\\.article', text).
default_emacs_mode(Pattern, prolog) :-
    user:prolog_file_type(Ext, prolog),
    Ext \== pl,
    atomic_list_concat(['.*\\.', Ext, '~?$'], Pattern).

make_emacs_interpreter_mode_list(Sheet) :-
    new(Sheet, sheet),
    (   emacs_interpreter_mode(Regex, Mode),
           send(Sheet, value, regex(Regex), Mode),
        fail
    ;   true
    ).

%!  emacs_interpreter_mode(+Regex, -Mode) is nondet.
%
%   True if Mode must be used for   a  file starting with #!Path and
%   Path matches Regex.

emacs_interpreter_mode('.*/swipl',                      prolog).
emacs_interpreter_mode('.*/xpce',                       prolog).
emacs_interpreter_mode('.*/perl',                       c).
emacs_interpreter_mode('.*/awk',                        c).

%!  emacs_content_mode(+Regex, +SearchLimit, +Mode)
%
%   Select Mode if Regex matches in the first SearchLimit characters
%   of the file.

make_emacs_content_mode_list(Sheet) :-
    new(Sheet, sheet),
    (   emacs_content_mode(Regex, SearchLimit, Mode),
           send(Sheet, value, tuple(regex(Regex), SearchLimit), Mode),
        fail
    ;   true
    ).

%!  emacs_content_mode(?Regex, ?Limit, ?Mode) is nondet.
%
%   True if Mode must be used for a   file in which Regex matches in
%   the first Limit characters of the file.

emacs_content_mode('library(chr)',      5000,   chr).


%       Do not make backup of a file matching this pattern

make_no_backup_list(Ch) :-
    new(Ch, chain),
    (   send(class(file), has_feature, case_sensitive, @off)
    ->  CaseSensitive = @on
    ;   CaseSensitive = @off
    ),
    forall(no_backup(Pattern),
           send(Ch, append, regex(Pattern, CaseSensitive))).

%!  no_backup(?Regex)
%
%   True if PceEmacs does not backup files that match Regex.

no_backup('/tmp/.*').
no_backup('.*/COMMIT_EDITMSG$').

:- free(@loading_emacs).


