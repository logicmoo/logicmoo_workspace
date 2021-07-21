/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2005-2015, University of Amsterdam
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

:- module(double_metaphone,
          [ double_metaphone/2,         % +In, -Primary
            double_metaphone/3          % +In, -Primary, -Secondary
          ]).

:- use_foreign_library(foreign(double_metaphone)).

/** <module> Phonetic string matching

The library library(double_metaphone) implements   the  Double Metaphone
algorithm  developed  by  Lawrence  Philips    and   described  in  "The
Double-Metaphone Search Algorithm" by L   Philips, C/C++ User's Journal,
2000. Double Metaphone creates a key  from   a  word that represents its
phonetic properties. Two words  with  the   same  Double  Metaphone  are
supposed to sound similar. The Double Metaphone algorithm is an improved
version of the Soundex algorithm.

@license The Double Metaphone algorithm is copied from the Perl library
that holds the following copyright notice. To the best of our knowledge
the Perl license is compatible to the SWI-Prolog license schema and
therefore including this module poses no additional license conditions.

    ==
    Copyright 2000, Maurice Aubrey <maurice@hevanet.com>.
    All rights reserved.

    This code is based heavily on the C++ implementation by Lawrence
    Philips and incorporates several bug fixes courtesy of Kevin
    Atkinson <kevina@users.sourceforge.net>.

    This module is free software; you may redistribute it and/or
    modify it under the same terms as Perl itself.
    ==
*/

%!  double_metaphone(+In, -MetaPhone) is det.
%
%   Same as double_metaphone/3,  but  only   returning  the  primary
%   metaphone.

%!  double_metaphone(+In, -MetaPhone, -AltMetaphone) is det.
%
%   Create metaphone and alternative metaphone  from In. The primary
%   metaphone is based on english, while   the  secondary deals with
%   common alternative pronounciation in  other   languages.  In  is
%   either and atom, string object,  code-   or  character list. The
%   metaphones are always returned as atoms.

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(double_metaphone:double_metaphone(_,_)).
sandbox:safe_primitive(double_metaphone:double_metaphone(_,_,_)).
