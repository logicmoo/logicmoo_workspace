/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2021, VU University Amsterdam
                              SWI-Prolog Soutions b.v.
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
          [ isub/4,              % +Text1, +Text2, -Distance, +Options
            '$isub'/5            % +Text1, +Text2, -Distance, +Flags, +Threshold
          ]).
:- autoload(library(option), [option/3]).

:- use_foreign_library(foreign(isub)).

/** <module> isub: a string similarity measure

The library(isub) implements a similarity measure between strings, i.e.,
something similar to the _|Levenshtein distance|_.  This method is based
on the length of common substrings.

@author Giorgos Stoilos
@see    _|A string metric for ontology alignment|_ by Giorgos Stoilos,
        2005 - http://www.image.ece.ntua.gr/papers/378.pdf .
*/

%!  isub(+Text1:text, +Text2:text,
%!       -Similarity:float, +Options:list ) is det.
%
%   Similarity is a measure  of   the  similarity/dissimilarity  between
%   Text1 and Text2. E.g.
%
%     ```
%     ?- isub('E56.Language', 'languange', D, [normalize(true)]).
%     D = 0.4226950354609929.                       % [-1,1] range
%
%     ?- isub('E56.Language', 'languange', D, [normalize(true),zero_to_one(true)]).
%     D = 0.7113475177304964.                       % [0,1] range
%
%     ?- isub('E56.Language', 'languange', D, []).  % without normalization
%     D = 0.19047619047619047.                      % [-1,1] range
%
%     ?- isub(aa, aa, D, []).  % does not work for short substrings
%     D = -0.8.
%
%     ?- isub(aa, aa, D, [substring_threshold(0)]). % works with short substrings
%     D = 1.0.                                      % but may give unwanted values
%                                                   % between e.g. 'store' and 'spore'.
%
%     ?- isub(joe, hoe, D, [substring_threshold(0)]).
%     D = 0.5315315315315314.
%
%     ?- isub(joe, hoe, D, []).
%     D = -1.0.
%     ```
%
%   This is a new version of isub/4 which replaces the old version while
%   providing backwards compatibility. This new   version allows several
%   options to tweak the algorithm.
%
%   @arg Text1 and Text2 are either an atom, string or a list of
%   characters or character codes.
%   @arg Similarity is a float in the range [-1,1.0], where 1.0
%   means _|most similar|_. The range can be set to [0,1] with
%   the zero_to_one option described below.
%   @arg Options is a list with elements described below. Please
%   note that the options are processed at compile time using
%   goal_expansion to provide much better speed. Supported options
%   are:
%
%   - normalize(+Boolean)
%   Applies string normalization as implemented by the original
%   authors: Text1  and Text2 are mapped
%   to lowercase and the characters  "._   "  are removed. Lowercase
%   mapping is done  with  the   C-library  function  towlower(). In
%   general, the required normalization is   domain dependent and is
%   better left to the caller.  See e.g., unaccent_atom/2. The default
%   is to skip normalization (`false`).
%
%   - zero_to_one(+Boolean)
%   The old isub implementation deviated from the original algorithm
%   by returning a value in the [0,1] range. This new isub/4 implementation
%   defaults to the original range of [-1,1], but this option can be set
%   to `true` to set the output range to [0,1].
%
%   - substring_threshold(+Nonneg)
%   The original algorithm was meant to compare terms in semantic web
%   ontologies, and it had a hard coded parameter that only considered
%   substring similarities greater than 2 characters. This caused the
%   similarity between, for example 'aa' and 'aa' to return -0.8 which
%   is not expected. This option allows the user to set any threshold,
%   such as 0, so that the similatiry between short substrings can be
%   properly recognized. The default value is 2 which is what the
%   original algorithm used.

isub(T1, T2, Normalize, Similarity) :-
   (   Normalize == true
   ->  !, '$isub'(T1,T2,Similarity,0x3,2)
   ;   Normalize == false
   ->  !, '$isub'(T1,T2,Similarity,0x1,2)
   ).
isub(T1, T2, Similarity, Options) :-
   isub_options(NumOpts,SubstringThreshold, Options),
   '$isub'(T1,T2,Similarity,NumOpts,SubstringThreshold).

isub_options(NumOpts,SubstringThreshold, Options) :-
   option(normalize(Normalize), Options, false),
   option(zero_to_one(ZeroToOne), Options, false),
   option(substring_threshold(SubstringThreshold), Options, 2),
   normalize_int(Normalize,NInt),
   zero_one_range_int(ZeroToOne,ZInt),
   NumOpts is NInt \/ ZInt.

normalize_int(true,0x2).
normalize_int(false,0x0).

zero_one_range_int(true,0x1).
zero_one_range_int(false,0x0).

user:goal_expansion(isub(T1,T2,Normalize,D),
                    '$isub'(T1,T2,D,NumOpts,SubstringThreshold)) :-
   (   Normalize == true
   ->  NumOpts = 0x3, SubstringThreshold = 2
   ;   Normalize == true
   ->  NumOpts = 0x1, SubstringThreshold = 2
   ).
user:goal_expansion(isub(T1,T2,D,Options),
                    '$isub'(T1,T2,D,NumOpts,SubstringThreshold)) :-
   isub_options(NumOpts,SubstringThreshold, Options).

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(isub:isub(_,_,_,_)).
sandbox:safe_primitive(isub:'$isub'(_,_,_,_,_)).
