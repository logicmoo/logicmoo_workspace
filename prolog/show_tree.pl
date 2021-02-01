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

:- module(show_tree, [show_tree/1]).

show_tree(Tree) :- show_tree(Tree, [], []).

show_tree(Node-List, Pre1, _Pos) :-
    !,
    length(List, N),
    length(Mid,  N),
    show_node(Node, Pre1, Mid, []),
    foldl(show_tree_(Pre1), List, Mid-[], _).
show_tree(Leaf, Pre, Pos) :-
    show_node(Leaf, Pre, [], Pos).

show_node(Node, Pre1, Mid, Pos) :-
    ignore(( maplist(put_char, Pre1),
             put_char(0'└),
             maplist(=(0'┬), Mid),
             maplist(put_char, Mid),
             fail
           )),
    maplist(=(0'│), Mid),
    maplist(put_char, Pos),
    write('─ '),
    writeln(Node).

show_tree_(Pre1, Tree, [_|Pre2]-Pre3, Pre2-[0'─|Pre3]) :-
    append(Pre1, [0' |Pre2], Pre),
    show_tree(Tree, Pre, Pre3).
