/*  Part of Extended libraries for Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2014, Process Design Center, Breda, The Netherlands.

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

:- module(mklinear, [mklinear/3]).

mklinear(Term, Linear, BindingL) :-
    term_variables(Term, VarL),
    exclude(singleton(Term), VarL, VarM), 
    mklinear(Term, Linear, VarM, BindingL, []).

singleton(T, V) :-
    occurrences_of_var(V, T, 1).

mklinear(Term, Linear, VarL) -->
    {compound(Term)},
    !,
    { functor(Term, F, A),
      functor(Linear, F, A)
    },
    mklinear(1, Term, Linear, VarL).
mklinear(Term, Var, VarL) -->
    ( { var(Term),
	member(Var0, VarL),
	Term==Var0
      }
    ->[Term=Var]
    ; {Term=Var}
    ).

mklinear(N, Term, Linear, VarL) -->
    { arg(N, Term, Arg),
      !,
      arg(N, Linear, LArg),
      succ(N, N1)
    },
    mklinear(Arg, LArg, VarL),
    mklinear(N1, Term, Linear, VarL).
mklinear(_, _, _, _) --> [].
