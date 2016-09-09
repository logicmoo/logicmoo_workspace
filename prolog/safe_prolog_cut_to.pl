/*  Part of Extended libraries for Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2016, Process Design Center, Breda, The Netherlands.

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

:- module(safe_prolog_cut_to,
	  [safe_prolog_cut_to/1,
	   safe_prolog_cut_to/3,
	   fix_choice/3]).

safe_prolog_cut_to(CP) :-
    prolog_current_choice(CPC),
    safe_prolog_cut_to(CPC, CP, _).

safe_prolog_cut_to(CPC, CP, CPF) :-
    fix_choice(CPC, CP, CPF),
    prolog_cut_to(CPF).

fix_choice(CPC, CP, CPC) :-
    CPC =< CP, !.
fix_choice(CPC, CP, CPF) :-
    prolog_choice_attribute(CPC, parent, CPP),
    fix_choice(CPP, CP, CPF).
