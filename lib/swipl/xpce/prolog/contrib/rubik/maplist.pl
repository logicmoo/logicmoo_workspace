/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Christian Schlichtherle
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1995, Christian Schlichtherle
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

:- module(n_maplist,[n_maplist/4]).
:- meta_predicate n_maplist(+, :, ?, ?).
:- require([ maplist/3
           ]).

%!  n_maplist(+N,+FNC,?LX,?LY)
%
%   will do a maplist N times.

n_maplist(0,_,L,L):-!.
n_maplist(N,Pred,LX,LZ):-
    ground(LX),!,
    maplist(Pred,LX,LY),
    NN is N-1,
      n_maplist(NN,Pred,LY,LZ).
n_maplist(N,Pred,LX,LZ):-
    maplist(Pred,LY,LZ),
    NN is N-1,
      n_maplist(NN,Pred,LX,LY).
