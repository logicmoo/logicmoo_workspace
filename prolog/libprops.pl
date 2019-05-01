/*  Part of Assertion Reader for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/assertions
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
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

:- module(libprops, []).

/** <module> Properties of library predicates
*/

:- use_module(library(assertions)).
:- use_module(library(plprops)).

:- pred functor(+, -constant, -nnegint) is det.
:- pred functor(-, +constant, +nnegint) is det.
:- pred functor(+, +constant, ?nnegint) is semidet.
:- pred functor(+, -constant, +nnegint) is semidet.
:- pred predicate_property(callable, ?).
:- pred current_predicate(atm, callable).

:- pred (+arithexpression  >  +arithexpression) is semidet.
:- pred (+arithexpression  <  +arithexpression) is semidet.
:- pred (+arithexpression  >= +arithexpression) is semidet.
:- pred (+arithexpression =<  +arithexpression) is semidet.
:- pred (+arithexpression =\= +arithexpression) is semidet.
:- pred (+arithexpression =:= +arithexpression) is semidet.
:- pred ((is)/2) : (var*arithexpression) => (num*arithexpression) is det.
:- pred ((is)/2) : (num*arithexpression) is semidet.

:- pred atomic_list_concat(+list(constant), +constant) is semidet.
:- pred atomic_list_concat(+list(constant), -atm) is det.

:- pred atomic_list_concat(?list(constant), +constant, +constant) is semidet.
:- pred atomic_list_concat(+list(constant), +constant, -atm) is det.

:- pred atom_number(+atm,-num) is semidet.
:- pred atom_number(?atm,+num) is det.

:- pred atom_codes(+atomic, -list) is det.
:- pred atom_codes(-atm,    +list) is det.
:- pred atom_codes(+atomic, ?list) is semidet.

:- pred sub_atom(+atomic,?int,?int,?int,?atm).

:- use_module(library(apply)).

:- pred maplist(1, list).
:- pred maplist(2, list, list).
:- pred maplist(3, list, list, list).
:- pred maplist(4, list, list, list, list).

:- pred memberchk(?, ?list) is semidet.

:- true pred [(==)/2,
              (\==)/2,
              (=)/2,
              (\=)/2,
              (=@=)/2,
              is_list/1
             ] is semidet.

:- true prop [[ground/1,
               atom/1,
               atomic/1,
               float/1,
               integer/1,
               nonvar/1,
               number/1,
               var/1,
               string/1,
               callable/1,
               compound/1
              ] is semidet,
              member/2].

:- pred [asserta/1,
         assertz/1,
         retractall/1,
         findall/3,
         ignore/1,
         writeln/1,
         writeln/2,
         format/2,
         format/3] is det.

:- pred [(\+)/1,
         forall/2,
         once/1
        ] is semidet.
