/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2000-2011, University of Amsterdam
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

:- module(xml_hierarchy, []).
:- use_module(library(pce)).
:- use_module(library(pce_toc)).
:- require([ memberchk/2
           , append/3
           , nth1/3
           ]).

resource(open,  image, image('16x16/book2.xpm')).
resource(close, image, image('16x16/manual.xpm')).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Show hierarchy of elements as produced   by library(sgml). This class is
kept as simple as  possible.  In   general  it  requires subclassing and
defining the select_node message to  do   anything  sensible.  See class
toc_window for details on programming this library.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(xml_hierarchy, toc_window,
                   "Browse XML document hierarchy").

variable(xml,   prolog, get, "Represented XML term").

initialise(H, XML:[prolog]) :->
    send_super(H, initialise),
    (   XML == @default
    ->  true
    ;   send(H, xml, XML)
    ).

xml(H, XML:prolog) :->                  % list of elements
    "Visualise an XML structure"::
    content(XML, Content),
    send(H, slot, xml, Content),
    get(H, icon, Content, @on, Icons),
    get(H, caption, Content, Caption),
    send(H, root, xml_node(Caption, [], Icons, @on)),
    send(H, expand_root).

content(document(_Type, [Content]), Content).
content([Content], Content).
content(element(A,B,C), element(A,B,C)).
content([pi(_XML), Content], Content).  % bit of a hack to handle XML files

expand_node(H, Node:xml_node) :->
    "Expand clicked node"::
    get(Node, xml, element(_Name, _Attributes, Content)),
    get(Node, path, Loc0),
    append(Loc0, [Index], Loc),
    (   nth1(Index, Content, Esub),
        get(H, caption, Esub, Name),
        arg(3, Esub, SubContent),
        (   memberchk(element(_,_,_), SubContent)
        ->  get(H, icon, Esub, @on, Icons),
            send(H, son, Node, xml_node(Name, Loc, Icons, @on))
        ;   get(H, icon, Esub, @on, Icons),
            send(H, son, Node, xml_node(Name, Loc, Icons, @off))
        ),
        fail
    ;   true
    ).

:- pce_group(refine).

caption(_, XML:prolog, Title:name) :<-
    "Get title for a node"::
    element_name(XML, Title).

element_name(element(_NS:Title, _, _), Title) :- !.
element_name(element(Title, _, _), Title).

icon(_H, _XML:prolog, _HasSub:bool, Tuple:tuple) :<-
    "Return open/close icon"::
    new(Tuple, tuple(image(resource(open)),
                     image(resource(close)))).

:- pce_group(path).

node_from_path(H, Path:prolog, Node:xml_node) :<-
    "Find node at given path (possibly expanding tree)"::
    get(H, root, Root),
    find_node(Path, Root, Node).

find_node([], Node, Node).
find_node([N|T], Node, Sub) :-
    (   get(Node, sons, Sons),
        get(Sons, nth1, N, Sub0)
    ->  find_node(T, Sub0, Sub)
    ;   send(Node, collapsed, @off),
        get(Node, sons, Sons),
        get(Sons, nth1, N, Sub0)
    ->  find_node(T, Sub0, Sub)
    ).

:- pce_end_class(xml_hierarchy).


:- pce_begin_class(xml_node, toc_folder,
                   "Show XML node with sub-nodes").

variable(path, prolog, get, "Path from root").

initialise(Node, Name:name, Path:prolog, Icons:tuple, CanExpand:bool) :->
    get(Icons, first, Open),
    get(Icons, second, Close),
    send_super(Node, initialise, Name, @default, Close, Open, CanExpand),
    send(Node, slot, path, Path).

xml(Node, XML:prolog) :<-
    "Get XML as Prolog term"::
    get(Node?tree, window, Window),
    get(Window, xml, Content),
    get(Node, path, Path),
    find_xml(Path, Content, XML).

find_xml([], XML, XML) :- !.
find_xml([H|T], element(_, _, Content), XML) :-
    nth1(H, Content, XML0),
    find_xml(T, XML0, XML).

:- pce_end_class(xml_node).


