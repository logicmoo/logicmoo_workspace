/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.
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

:- module(trim_utils,
          [left_trim/2,
           right_trim/2,
           trim/2,
           atom_left_trim/2,
           atom_right_trim/2,
           atom_trim/2,
           string_left_trim/2,
           string_right_trim/2,
           string_trim/2
           ]).

:- use_module(library(lists)).

left_trim([], []).
left_trim([Code|Codes], LTrim) :-
    ( char_type(Code, space)
    ->left_trim(Codes, LTrim)
    ; LTrim = [Code|Codes]
    ).

right_trim(Codes, RTrim) :-
    reverse(Codes, Reverse),
    left_trim(Reverse, LTrim),
    reverse(LTrim, RTrim).

trim(Codes, Trim) :-
    left_trim(Codes, LTrim),
    right_trim(LTrim, Trim).

atom_left_trim(Atom, LTrim) :-
    atom_codes(Atom, Codes),
    left_trim(Codes, CTrim),
    atom_codes(LTrim, CTrim).

atom_right_trim(Atom, RTrim) :-
    atom_codes(Atom, Codes),
    right_trim(Codes, CTrim),
    atom_codes(RTrim, CTrim).

atom_trim(Atom, Trim) :-
    atom_codes(Atom, Codes),
    trim(Codes, CTrim),
    atom_codes(Trim, CTrim).

string_left_trim(String, LTrim) :-
    string_codes(String, Codes),
    left_trim(Codes, CTrim),
    string_codes(LTrim, CTrim).

string_right_trim(String, RTrim) :-
    string_codes(String, Codes),
    right_trim(Codes, CTrim),
    string_codes(RTrim, CTrim).

string_trim(String, Trim) :-
    string_codes(String, Codes),
    trim(Codes, CTrim),
    string_codes(Trim, CTrim).
