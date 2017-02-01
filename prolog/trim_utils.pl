/*  Part of Extended libraries for Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
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
