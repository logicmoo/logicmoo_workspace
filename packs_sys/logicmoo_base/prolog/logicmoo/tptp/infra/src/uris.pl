/*
* Copyright (C) 2002, 2007 Christoph Wernhard
* 
* This program is free software; you can redistribute it and/or modify it
* under the terms of the GNU General Public License as published by the Free
* Software Foundation; either version 2 of the License, or (at your option)
* any later version.
* 
* This program is distributed in the hope that it will be useful, but WITHOUT
* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
* more details.
* 
* You should have received a copy of the GNU General Public License along with
* this program; if not, see <http://www.gnu.org/licenses/>.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Uris
%%%%
%%%% Resolving uri references with base uris - should correspond to RFC 2396.
%%%%
%%%% RFC 2396 is however rather imprecise, see e.g.
%%%% http://lists.w3.org/Archives/Public/www-rdf-interest/2001Dec/0021.html.
%%%%
%%%% - Unresolvable ".."s  are retained in the resulting uri, according
%%%%   to test012.rdf in the xmlbase tests of 
%%%%   http://www.w3.org/2000/10/rdf-tests/rdfcore/.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(uris, [resolve_uri/3, decompose_uri/6]).

%% 
%% Note: There seem some possibilities for improving efficiency here by
%% shortcuts for certain cases such as just-fragment uri-references and
%% exporting resolve_uri/5.
%% 

resolve_uri(_, Uri, Uri) :-
	is_global(Uri),
	!.
resolve_uri(Base, Uri, Uri1) :-
	decompose_uri(Base, RScheme, BAuthority, BPath, _, _),
	resolve_uri(RScheme, BAuthority, BPath, Uri, Uri1).

resolve_uri(RScheme, BAuthority, BPath, Uri, Uri1) :-
	decompose_uri(Uri, _, UAuthority, UPath, RQuery, RFragment),
	( var(UPath), var(UAuthority), var(RQuery) ->
	  RAuthority = BAuthority,
	  ( var(BPath) ->
	    RPath = ''
	  ; RPath = BPath
	  )
        ; nonvar(UAuthority) ->
	  RAuthority = UAuthority,
	  ( var(UPath) ->
	    RPath = ''
          ; RPath = UPath
	  )
	; nonvar(UPath), atom_prefix(UPath, '/') ->
	  RAuthority = BAuthority,
	  RPath = UPath
	; RAuthority = BAuthority,
	  ( var(BPath) -> BPath = '/' ; true ),
	  ( var(UPath) -> UPath = '' ; true ),
	  concat_atom(BPath1, '/', BPath),
	  concat_atom(UPath1, '/', UPath),
	  append(BPath2, [_], BPath1),
	  append(BPath2, UPath1, MPath1),
	  reverse(MPath1, MPath2),
	  (( MPath2 = ['..'|_] ; MPath2 = ['.'|_] ) ->
	    MPath3 = [''|MPath2]
	  ; MPath3 = MPath2
	  ),
	  mp_1(MPath3, [], MPath4),
	  reverse(MPath4, MPath5),
	  concat_atom(MPath5, '/', RPath)
        ),
	( nonvar(RFragment) ->
	  R1 = ['#', RFragment] 
	; R1 = [] 
        ),
	( nonvar(RQuery) ->
	  R2 = [RPath, '?', RQuery| R1] 
	; R2 = [RPath | R1] 
	),
	( nonvar(RAuthority) -> 
	  R3 = ['//', RAuthority | R2]
        ; R3 = R2 
	),
	( nonvar(RScheme) ->
	  R4 = [RScheme, ':' | R3]
	; R4 = R3
	),
	concat_atom(R4, Uri1).

mp_1(['.'|Parts], Skip, Parts1) :-
	!,
	mp_1(Parts, Skip, Parts1).
mp_1(['..'|Parts], Skip, Parts1) :-
	!,
	mp_1(Parts, ['..'|Skip], Parts1).
mp_1([''], Skip, Parts1) :-
	!,
	append(Skip, [''], Parts1).
mp_1([_|Parts], [_|Skip], Parts1) :-
	!,
	mp_1(Parts, Skip, Parts1).
mp_1([Part|Parts], [], [Part|Parts1]) :-
	!,
	mp_1(Parts, [], Parts1).
mp_1([], Skip, Skip).
	

decompose_uri(Uri, Scheme, Authority, Path, Query, Fragment) :-
	atom_codes(Uri, Cs),
	( scan_scheme(Cs, SchemeCs, Cs1) ->
	  atom_codes(Scheme, SchemeCs)
	; Cs1 = Cs
	),
	( Cs1 = [0'/, 0'/|Cs2] ->
	  scan_authority(Cs2, AuthorityCs, Cs3),
	  atom_codes(Authority, AuthorityCs)
        ; Cs3 = Cs1
	),
	( Cs3 = [CP|_], CP \= 0'?, CP \= 0'# ->
	  scan_path(Cs3, PathCs, Cs4),
	  atom_codes(Path, PathCs)
	; Cs4 = Cs3
	),
	( Cs4 = [0'?|Cs5] ->
	  scan_query(Cs5, QueryCs, Cs6),
	  atom_codes(Query, QueryCs)
	; Cs6 = Cs4
	),
	( Cs6 = [0'#|FragmentCs] ->
	  atom_codes(Fragment, FragmentCs)
	; true
	).

is_global(URI) :-
	sub_atom(URI, P, _, _, :),
	sub_atom(URI, 0, P, _, Protocol),
	atom_codes(Protocol, Codes),
	scheme_chars(Codes).

scheme_chars([]).
scheme_chars([H|T]) :-
	scheme_char(H),
	scheme_chars(T).

scheme_char(H) :-
	code_type(H, alnum).
scheme_char(0'+).
scheme_char(0'-).
scheme_char(0'.).

scan_scheme([0':|Cs], [], Cs) :-
	!.
scan_scheme([C|Cs], [C|Cs1], Cs2) :-
	scheme_char(C),
	!,
	scan_scheme(Cs, Cs1, Cs2).

scan_authority([0'/|Cs], [], [0'/|Cs]) :-
	!.
scan_authority([0'?|Cs], [], [0'?|Cs]) :-
	!.
scan_authority([0'#|Cs], [], [0'#|Cs]) :-
	!.
scan_authority([C|Cs], [C|Cs1], Cs2) :-
	scan_authority(Cs, Cs1, Cs2).
scan_authority([], [], []).

scan_path([0'?|Cs], [], [0'?|Cs]) :-
	!.
scan_path([0'#|Cs], [], [0'#|Cs]) :-
	!.
scan_path([C|Cs], [C|Cs1], Cs2) :-
	scan_path(Cs, Cs1, Cs2).
scan_path([], [], []).

scan_query([0'#|Cs], [], [0'#|Cs]) :-
	!.
scan_query([C|Cs], [C|Cs1], Cs2) :-
	scan_query(Cs, Cs1, Cs2).
scan_query([], [], []).




