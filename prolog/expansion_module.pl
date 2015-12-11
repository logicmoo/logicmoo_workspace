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

:- module(expansion_module, [expansion_module/2]).

%% expansion_module(?, ?)
% Kludge: using swipl internals. Perhaps is not a good idea --EMM
% Warning: could report duplicate solutions
%
expansion_module(M, EM) :-
    CM = compound_expand,
    module_property(CM, file(CF)),
    ( nonvar(M)
    ->'$load_context_module'(EF, M, _),
      module_property(EM, file(EF)),
      '$load_context_module'(CF, EM, _)
    ; '$load_context_module'(CF, EM, _),
      module_property(EM, file(EF)),
      '$load_context_module'(EF, M, _)
    ).
