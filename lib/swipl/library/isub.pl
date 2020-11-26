/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2014, VU University Amsterdam
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

:- module(isub,
          [ isub/4                      % +Text1, +Text2, +Normalize, -Distance
          ]).


:- use_foreign_library(foreign(isub)).

/** <module> isub: a string similarity measure

The library(isub) implements a similarity measure between strings, i.e.,
something similar to the _|Levenshtein distance|_.  This method is based
on the length of common substrings.

@author Giorgos Stoilos
@see    _|A string metric for ontology alignment|_ by Giorgos Stoilos,
        2005.
*/

%!  isub(+Text1:text, +Text2:text, +Normalize:bool,
%!       -Similarity:float) is det.
%
%   Similarity is a measure for the distance between Text1 and
%   Text2.  E.g.
%
%     ==
%     ?- isub('E56.Language', 'languange', true, D).
%     D = 0.711348.
%     ==
%
%   If Normalize is =true=, isub/4   applies string normalization as
%   implemented by the original authors: Text1  and Text2 are mapped
%   to lowercase and the characters  "._   "  are removed. Lowercase
%   mapping is done  with  the   C-library  function  towlower(). In
%   general, the required normalization is   domain dependent and is
%   better left to the caller.  See e.g., unaccent_atom/2.
%
%   @arg Text1 and Text2 are either an atom, string or a list of
%   characters or character codes.
%   @arg Similarity is a float in the range [0.0..1.0], where 1.0
%   means _|most similar|_

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(isub:isub(_,_,_,_)).
