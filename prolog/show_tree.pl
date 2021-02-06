/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2021, Process Design Center, Breda, The Netherlands.
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

:- module(show_tree, [show_tree/1, show_trees/1]).

show_trees(TreeL) :-
    maplist(show_tree, TreeL).

show_tree(Tree) :-
    show_tree(Tree, [], [0' ], [0'─]).

show_tree(Node-List, Pre1, Mid, Pos) :-
    !,
    length(List, N),
    show_node(Node, Pre1, Pos, N),
    ( List = []
    ->true
    ; List = [Elem|Tail],
      append(Pre1, Mid, Pre2),
      foldl(show_tree(Pre2, [0'├]), Tail, Elem, Last),
      show_tree(Last, Pre2, [0' ], [0'└])
    ).
show_tree(Leaf, Pre, _, Pos) :-
    show_node(Leaf, Pre, 0, Pos).

show_node(Node, Pre, Pos, N) :-
    maplist(put_char, Pre),
    maplist(put_char, Pos),
    ( N = 0
    ->put_char(0'─)
    ; put_char(0'┬)
    ),
    put_char(0' ),
    writeln(Node).

show_tree(Pre, Pos, Tree, Tree1, Tree) :-
    show_tree(Tree1, Pre, [0'│], Pos).
