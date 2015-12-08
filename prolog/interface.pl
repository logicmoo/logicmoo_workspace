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

:- module(interface, [implements/1,
		      bind_interface/2,
		      end_interface/0]).

:- use_module(library(apply)).
:- use_module(library(error)).

:- multifile
    '$interface'/3,
    '$implementation'/2.

:- meta_predicate implements(:).
implements(Implementation:Alias) :-
    Implementation:use_module(Alias, []), % Ensure that the module is loaded
    absolute_file_name(Alias, File,
		       [file_type(prolog), access(read)]),
    module_property(Interface, file(File)),
    module_property(Interface, exports(PIL)),
    compile_aux_clauses(interface:'$implementation'(Implementation, Interface)),
    maplist(Implementation:export, PIL).

direct_interface(M, F/A) :-
    \+ ( current_predicate(M:F/A),
	 functor(H, F, A),
	 predicate_property(M:H, defined),
	 \+ predicate_property(M:H, imported_from(_))
       ).

:- module_transparent end_interface/0.
end_interface :-
    context_module(Interface),
    end_interface(Interface).

end_interface(Interface) :-
    module_property(Interface, exports(PIL)),
    partition(interface:direct_interface(Interface), PIL, DIL, IIL),
    compile_aux_clauses(interface:'$interface'(Interface, DIL, IIL)),
    %% multifile used to avoid warnings, will be abolished:
    forall(member(DI, DIL), compile_aux_clauses((:- multifile(DI)))).

bind_interface(Interface, Implementation) :-
    ( '$interface'(Interface, DIL, IIL)
    ->true
    ; existence_error(interface, Interface)
    ),
    ( '$implementation'(Implementation, Interface)
    ->true
    ; ( '$implementation'(Implementation, _)
      ->existence_error(implementation, Implementation)
      ; existence_error(binding, Interface->Implementation)
      )
    ),
    maplist(Interface:abolish, DIL),
    '$import_from_loaded_module'(Implementation, Interface, [imports(DIL)]),
    atom_concat(Interface, '$impl', II),
    maplist(II:abolish, IIL),
    '$import_from_loaded_module'(Implementation, II, [imports(IIL)]).
