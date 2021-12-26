/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
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

:- module(jquery, []).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_server_files), []).
:- use_module(library(settings)).
:- use_module(library(broadcast)).

:- setting(version, atom, 'jquery-1.11.3.min.js',
           'File name for jquery.js, served as HTML resource "jquery"').

/** <module> Provide JQuery

This module provides the HTML  resource   `jquery`.  To  get the default
version of jquery included in a web page,  make sure this file is loaded
and include the following into the HTML generation DCG.

  ==
    html_requires(jquery),
  ==

The file served is determined by the setting `jquery:version` and loaded
from the file search path `js`,  the   default  for which is provided by
library(http/http_server_files).

Note that including jquery into the   HTTP  infrastructure is not ideal.
However, components, such  as  PlDoc  and   Pengines  as  well  as  user
applications require jquery, causing this JavaScript  to be installed in
many places. That is even worse.

# Using your own copy

To use your own copy of jquery, add   your jquery file to a directory in
the   `js`   file   search    path     (see    file_search_path/2    and
absolute_file_name/3) and set `jquery:version` to   the file version you
provided. Alternatively, you  can  define   the  html  resource `jquery`
before loading this file.
*/

jquery_dir('web/js').

global_jquery :-
    jquery_dir(JQuery),
    is_absolute_file_name(JQuery).

:- if(global_jquery).
:- multifile user:file_search_path/2.
user:file_search_path(js, Dir) :-
    jquery_dir(Dir).
:- endif.

register_jquery :-
    setting(version, JQuery0),
    backward_compatibility_hack(JQuery0, JQuery),
    html_resource(jquery,
                  [ virtual(true),
                    requires([ js(JQuery)
                             ])
                  ]).

% Older versions of this library used only the jQuery version as setting
% value. As Debian provides central jQuery   files  and uses a different
% naming convention, we now use the complete file name. The first clause
% recognises the old setting.

backward_compatibility_hack(Version, File) :-
    sub_atom(Version, 0, 1, _, C0),
    char_type(C0, digit),
    !,
    atomic_list_concat(['jquery-', Version, '.js'], File).
backward_compatibility_hack(File, File).

:- if(\+html_current_resource(jquery)).
:- initialization register_jquery.
:- listen(settings(changed(jquery:version, _, _)),
          register_jquery).
:- endif.

