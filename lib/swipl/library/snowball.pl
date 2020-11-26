/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2015, VU University, Amsterdam
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

:- module(snowball,
          [ snowball/3,                  % +Algorithm, +In, -Out
            snowball_current_algorithm/1 % ?algorithm
          ]).
:- autoload(library(apply),[maplist/3]).

/** <module> The Snowball multi-lingual stemmer library

This module encapsulates "The C version  of the libstemmer library" from
the Snowball project. This library  provides   stemmers  in a variety of
languages.  The interface to this library is very simple:

    * snowball/3 stems a word with a given algorithm
    * snowball_current_algorithm/1 enumerates the provided algorithms.

Here is an example:

    ==
    ?- snowball(english, walking, S).
    S = walk.
    ==

@see http://snowball.tartarus.org/
*/

:- use_foreign_library(foreign(snowball)).

%!  snowball(+Algorithm, +Input, -Stem) is det.
%
%   Apply the Snowball Algorithm on Input and unify the result
%   (an atom) with Stem.
%
%   The implementation maintains a cache of stemmers for each thread
%   that  accesses  snowball/3,   providing    high-perfomance   and
%   thread-safety without locking.
%
%   @param  Algorithm is the (english) name for desired algorithm
%           or an 2 or 3 letter ISO 639 language code.
%   @param  Input is the word to be stemmed.  It is either an
%           atom, string or list of chars/codes.  The library
%           accepts Unicode characters.  Input must be
%           _lowercase_.  See downcase_atom/2.
%   @error domain_error(snowball_algorithm, Algorithm)
%   @error type_error(atom, Algorithm)
%   @error type_error(text, Input)

%!  snowball_current_algorithm(?Algorithm) is nondet.
%
%   True if Algorithm is the official  name of an algorithm suported
%   by snowball/3. The predicate is =semidet= if Algorithm is given.

term_expansion(snowball_current_algorithm(dummy), Clauses) :-
    snowball_algorithms(Algos),
    maplist(wrap, Algos, Clauses).

wrap(X, snowball_current_algorithm(X)).

snowball_current_algorithm(dummy).

:- multifile
    sandbox:safe_primitive/1.

sandbox:safe_primitive(snowball:snowball(_,_,_)).
