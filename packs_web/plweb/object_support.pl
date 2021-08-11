/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, VU University Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(object_support,
	  [ object_label/2,		% +Object:compound
					% -Label:atom
	    object_id/2			% ?Object:compound
					% ?Id:atom
	  ]).

/** <module> Object support

*/

:- use_module(wiki).

:- dynamic
	object_id_cache/2.

%!	object_id(+Object:compound, -Id:atom) is det.
%!	object_id(-Object:compound, +Id:atom) is semidet.
%
%	True when Id is a (hash) id for Object.

object_id(Object, Id) :-
	object_id_cache(Object, Id), !.
object_id(Object, Id) :-
	ground(Object),
	variant_sha1(Object, Id),
	assertz(object_id_cache(Object, Id)).


%!	object_label(+Object:compound, -Label:atom) is det.
%
%	True when Label is a label for Object.

object_label(Name/Arity, Label) :- !,
	format(atom(Label), 'predicate ~w/~w', [Name, Arity]).
object_label(Name//Arity, Label) :- !,
	format(atom(Label), 'non-terminal ~w/~w', [Name, Arity]).
object_label(M:Name/Arity, Label) :- !,
	format(atom(Label), 'predicate ~w:~w/~w', [M, Name, Arity]).
object_label(M:Name//Arity, Label) :- !,
	format(atom(Label), 'non-terminal ~w:~w//~w', [M, Name, Arity]).
object_label(f(Name/Arity), Label) :- !,
	format(atom(Label), 'function ~w/~w', [Name, Arity]).
object_label(c(Function), Label) :- !,
	format(atom(Label), 'C API function ~w()', [Function]).
object_label(Module:module(_Title), Label) :-
	module_property(Module, file(File)), !,
	file_base_name(File, Base),
	format(atom(Label), 'module ~w', [Base]).
object_label(section(ID), Label) :-
	prolog:doc_object_summary(section(_Level, _No, ID, _File),_,_,Title), !,
	format(atom(Label), 'Section "~w"', [Title]).
object_label(wiki(Location), Label) :-
	wiki_page_title(Location, Title),
	format(atom(Label), 'Wiki page "~w"', [Title]).
object_label(Obj, Label) :-
	term_to_atom(Obj, Label).

