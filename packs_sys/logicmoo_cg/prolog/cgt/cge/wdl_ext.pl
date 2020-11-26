/* COPYRIGHT ************************************************************

Conceptual Graph Editor (CGE) - an X-Windows graphical interface to CGT
Copyright (C) 1990 Miguel Alexandre Wermelinger

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

************************************************************************/
:- use_module(library(cgt/cge/swi_apeal)).

/* AUTHOR(S) ************************************************************

Michel Wermelinger
Dept. de Informatica, Univ. Nova de Lisboa, Quinta da Torre
P - 2825 Monte da Caparica, PORTUGAL
Phone: (+351) (1) 295 44 64 ext. 1360  Internet: mw@fct.unl.pt

************************************************************************/

xt_accelerators(Term, Acc) :-
	xt_translate(Term, String, []),
	xt_convert(string, to_C, String, A_String),
	xt_parse_accelerator_table(A_String, Acc).

xt_flush :-
	xt_context(C, C), 
	repeat, xt_app_pending(C, Mask), 
		( Mask = 0, ! ; next_event(_), fail ).

:- op(800, xfx, wgetl).

[] wgetl _ :- !.
_ wgetl [] :- !.
WIDs wgetl [Attr|AttrList] :-
	WIDs wgetl Attr, WIDs wgetl AttrList.
[WID] wgetl MultiAttr :-
	MultiAttr =.. [AttrName, [AttrVal]], 
	SimpleAttr =.. [AttrName, AttrVal], WID wget SimpleAttr.
[WID|WIDList] wgetl MultiAttr :-
	MultiAttr =.. [Name, [Val|RestVal]],
	SimpleAttr =.. [Name, Val], WID wget SimpleAttr,
	RestAttr =.. [Name, RestVal], WIDList wgetl RestAttr.

