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

:- module(dependency_trees, [dependency_trees/3]).

:- meta_predicate dependency_trees(2,+,-).
        
:- thread_local dependency_tree_done/2.

dependency_tree(GetEdge, Loaded, Leaf, Node-TreeL) :-
    ( memberchk(Leaf, Loaded)
    ->Node = ~(Leaf),
      TreeL = []
    ; dependency_tree_done(Leaf, L)
    ->( L = []
      ->Node = Leaf
      ; Node = +Leaf
      ),
      TreeL = []
    ; Node = Leaf,
      findall(ReqLeaf, call(GetEdge, Leaf, ReqLeaf), ReqLeafL),
      assertz(dependency_tree_done(Leaf, ReqLeafL)),
      maplist(dependency_tree(GetEdge, [Leaf|Loaded]), ReqLeafL, TreeL)
    ).

dependency_trees(GetEdge, NodeL, TreeL) :-
    call_cleanup(
        maplist(dependency_tree(GetEdge, []), NodeL, TreeL),
        retractall(dependency_tree_done(_, _))).
