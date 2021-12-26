/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2014, University of Amsterdam
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

:- multifile
    prolog:doc_object_summary/4,    % Object, ?Category, ?Section, ?Summary
    prolog:doc_object_page//2,      % +Object, +Options
    prolog:doc_nav_tree//2,         % +Object, +Options
    prolog:doc_object_link//2,      % +Object, +Options
    prolog:doc_category/3,          % Name, Order, Description
    prolog:doc_file_index_header//2,% +File, +Options
    prolog:doc_object_title/2,      % +Object, -Title
    prolog:doc_object_href/2,       % +Object, -HREF
    prolog:doc_canonical_object/2,  % +ObjectIn, -CanonicalObj
    prolog:doc_search_field//1,     % +Options
    prolog:doc_places_menu//1,      % +Dir
    prolog:doc_directory/1,         % ?Dir
    prolog:doc_object_footer//2,    % +Object, +Options
    prolog:doc_object_page_footer//2, % +Object, +Options
    prolog:doc_page_header//2,      % +File, +Options
    prolog:doc_links//2,            % +Directory, +Options
    prolog:doc_file_title//3.       % +Title, +File, +Options
