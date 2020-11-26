/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.
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

:- module(ws_source, []).

:- reexport(library(ws_browser)).
:- use_module(library(http/html_write)).
:- use_module(library(module_files)).
:- use_module(library(pldoc/doc_htmlsrc)).

ws_browser:provides_method(live).

ws_browser:fetch_files_properties_hook(live, [module], FileMG) :-
    findall(File-[M],
            ( current_module(M),
              module_file(M, File)
            ), FileMU),
    sort(FileMU, FileML),
    group_pairs_by_key(FileML, FileMG).

ws_browser:show_source_hook(live, File) :-
    format('Content-type: text/html~n~n', []),
    source_to_html(File, stream(current_output),
                   [format_comments(false), skin(adjustments_js)]).

:- public
       adjustments_js/2.

adjustments_js(header, Out) :-
    phrase(html([style([],
                       [
"
span.directive {
    display: inline;
}
"
                       ])]), Tokens),
    print_html(Out, Tokens).
