/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, VU University Amsterdam

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

:- module(pack_analyzer,
	  [ pack_analysis/2,			% +Pack, -Results
	    xref_pack/1,			% +Pack
	    pack_prolog_entry/1,		% +Entry
	    xref_pack_file/2,			% +Pack, +File
	    pack_members/2,			% +Pack, -Members
	    pack_open_entry/3			% +Pack, +Entry, -Stream
	  ]).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(archive)).
:- use_module(library(filesex)).
:- use_module(library(prolog_xref)).
:- use_module(library(prolog_source)).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(git)).

:- use_module(pack_mirror).
:- use_module(pack_info).

/** <module> Analyse the content of a Prolog pack

This module analyses a Prolog pack without loading it.
*/

:- dynamic
	pack_dependency/4.

:- meta_predicate
	find_unique(?, 0, ?, ?).

%%	pack_analysis(+Pack, -Results)
%
%	Result is the analysis  result  for   Pack.  Results  is a list,
%	containing the following elements:
%
%	  * size(SizeBytes)
%	  Summed size of all files in Pack.  Always exactly one.
%	  * files(Files)
%	  List of files in the pack.  Each file is represented by a
%	  term file(Name, SizeBytes).
%	  * prolog_source(File, Size, Module, Exports, Extra)
%	  One for each Prolog file in pack.  If the file is not a module
%	  file, Module is unified to =|-|= and Exports is []. Each
%	  member of Exports is a term export(PI, Info), where Info
%	  is a list with additional properties.  Currently defined
%	  Info terms are:
%	    - doc(Summary, Comment)
%	  Extra is reserved for additional information.  Currently defines:
%	    - doc(Title, Comment)

pack_analysis(Pack,
	      [ size(Size),
		files(Members)
	      | Results
	      ]) :-
	pack_members(Pack, Members),
	maplist(arg(2), Members, SizeByFile),
	sum_list(SizeByFile, Size),
	xref_pack(Pack),
	find_unique(Readme, readme(Pack, Members, Readme), Results, Res0),
	find_unique(Info,   file_info(Pack, Members, Info), Res0, Res1),
	find_unique(ReqF,   required_file(Pack, ReqF), Res1, Res2),
	find_unique(ReqP,   required_predicate(Pack, ReqP), Res2, []).

find_unique(Templ, Goal, List, Tail) :-
	findall(Templ, Goal, List0),
	sort(List0, List1),
	append(List1, Tail, List).

readme(Pack, Members, readme(Text)) :-
	member(file(Name, _Size), Members),
	downcase_atom(Name, Down),
	readme_file(Down), !,
	setup_call_cleanup(
	    pack_open_entry(Pack, Name, Stream),
	    read_stream_to_codes(Stream, Codes),
	    close(Stream)),
	string_to_list(Text, Codes).

readme_file(readme).
readme_file('readme.txt').


file_info(Pack, Members,
	  prolog_source(Entry, Size, Module, Exports, Documentation)) :-
	xref_pack_source(Pack, Entry, Source),
	memberchk(file(Entry,Size), Members),
	(   xref_module(Source, Module)
	->  findall(export(Name/Arity, Info),
		    ( xref_exported(Source, Head),
		      functor(Head, Name, Arity),
		      (	  xref_comment(Source, Head, Summary, PredComment)
		      ->  Info = [doc(Summary, PredComment)]
		      ;	  Info = []
		      )
		    ),
		    Exports)
	;   Module = (-),			% Warning?
	    Exports = []
	),
	(   xref_comment(Source, Title, FileComment)
	->  Documentation = [ doc(Title, FileComment) ]
	;   Documentation = []
	).

required_file(Pack, required_file(Spec, From)) :-
	xref_pack_source(Pack, _, Source),
	xref_uses_file(Source, Spec, File),
	classify_file(Pack, File, From0),
	from_class(From0, From).

from_class(pack(Pack), pack(Pack)) :- !.
from_class(Alias, From) :-
	functor(Alias, From, 1), !.
from_class(From, From).

classify_file(Pack, File, From) :-
	(   xref_pack_source(Pack, _, File)
	->  From = pack
	;   absolute_file_name(pack(.), PackRoot,
			       [ file_type(directory),
				 solutions(all)
			       ]),
	    atom_concat(PackRoot, Local, File)
	->  atomic_list_concat(Segments, /, Local),
	    Segments = [FromPack|_],
	    From = pack(FromPack)
	;   absolute_file_name(swi(.), SwiRoot,
			       [ file_type(directory)
			       ]),
	    atom_concat(SwiRoot, _, File)
	->  file_name_on_path(File, From)
	;   From = File				% Needs further classification
	).

%%	required_predicate(+Pack, -Required) is nondet.
%
%	True if Required is a predicate that is required by Pack.

required_predicate(Pack, required_predicate(Name/Arity, From)) :-
	xref_pack_source(Pack, _, Source),
	xref_called(Source, Head, _By),
	functor(Head, Name, Arity),
	(   xref_defined(Source, Head, How)
	->  classify_predicate_source(How, Pack, From)
	;   predicate_property(Head, iso)
	->  From = iso
	;   predicate_property(system:Head, visible)
	->  From = swi
	;   predicate_property(Head, autoload(Autoload))
	->  classify_file(Pack, Autoload, From)
	;   From = undefined
	).

classify_predicate_source(imported(File), Pack, From) :-
	classify_file(Pack, File, From).


%%	xref_pack(+Pack) is det.
%
%	Run the cross-referencer on all Prolog files inside pack.

xref_pack(Pack) :-
	absolute_file_name(Pack, PackPath),
	retractall(pack_dependency(PackPath, _Spec, _How, _Dep)),
	pack_members(PackPath, Members),
	maplist(arg(1), Members, Entries),
	include(pack_prolog_entry, Entries, PrologEntries),
	maplist(xref_pack_file(PackPath), PrologEntries).

pack_prolog_entry(Entry) :-
	sub_atom(Entry, 0, _, _, 'prolog/'),
	file_name_extension(_, Ext, Entry),
	user:prolog_file_type(Ext, prolog), !.


%%	xref_pack_file(+Pack, +File) is det.
%
%	Run the cross-referencer on File inside Pack.

xref_pack_file(Pack, File) :-
	catch(xref_pack_file_2(Pack, File),
	      E, print_message(error, E)), !.
xref_pack_file(Pack, File) :-
	print_message(warning,
		      error(goal_failed(xref_pack_file(Pack, File)),
			    _)).

xref_pack_file_2(Pack, File) :-
	exists_directory(Pack), !,
	directory_file_path(Pack, File, Path),
	xref_source(Path, [register_called(all)]).
xref_pack_file_2(Pack, File) :-
	absolute_file_name(Pack, AbsPack,
			   [ access(read)
			   ]),
	directory_file_path(AbsPack, File, Path),
	xref_source(Path, [register_called(all)]).


		 /*******************************
		 *	      MEMBERS		*
		 *******************************/

%%	pack_open_entry(+Pack, +Entry, -Stream) is det.
%
%	Open an entry in the Pack for  reading. The entry must be closed
%	with close/1.

pack_open_entry(Directory, Entry, Stream) :-
	exists_directory(Directory), !,
	directory_file_path(Directory, Entry, File),
	open(File, read, Stream).
pack_open_entry(Archive, Entry, Stream) :-
	ar_prefix(Archive, Prefix),
	atom_concat(Prefix, Entry, Name),
	setup_call_cleanup(
	    archive_open(Archive, Handle, []),
	    ( archive_next_header(Handle, Name),
	      archive_open_entry(Handle, Stream)
	    ),
	    archive_close(Handle)),
	format(atom(StreamName), '~w/~w', [Archive, Entry]),
	set_stream(Stream, file_name(StreamName)).

:- dynamic
	ar_prefix_cache/2,
	ar_members_cache/3.

ar_prefix(Archive, Prefix) :-
	ar_prefix_cache(Archive, Prefix0), !,
	Prefix = Prefix0.
ar_prefix(Archive, Prefix) :-
	ar_pack_members(Archive, _, Prefix),
	assertz(ar_prefix_cache(Archive, Prefix)).

%%	pack_members(+Pack, -Members:list) is det.
%
%	Members is a list of file(File,Size) that represent the files in
%	Pack. Pack is either a git repository, a directory holding files
%	or an archive.

:- dynamic
	pack_member_cache/3.

pack_members(Dir, Members) :-
	time_file(Dir, T),
	pack_member_cache(Dir, T, Members0), !,
	Members = Members0.
pack_members(Dir, Members) :-
	pack_members_no_cache(Dir, Members0),
	time_file(Dir, T),
	asserta(pack_member_cache(Dir, T, Members0)),
	Members = Members0.

pack_members_no_cache(Directory, Members) :-
	is_git_directory(Directory), !,
	git_ls_tree(Entries, [directory(Directory)]),
	include(git_blob, Entries, Blobs),
	maplist(git_entry, Blobs, Members).
pack_members_no_cache(Directory, Members) :-
	exists_directory(Directory), !,
	recursive_directory_files(Directory, Files),
	maplist(file_entry(Directory), Files, Members).
pack_members_no_cache(Archive, Members) :-
	E = error(archive_error(_,_),_),
	catch(ar_pack_members(Archive, Members0, Prefix),
	      E, bad_archive(Archive, E)),
	maplist(strip_prefix(Prefix), Members0, Members).

bad_archive(Archive, Error) :-
	delete_file(Archive),
	throw(Error).

git_blob(object(_Mode, blob, _Hash, _Size, _Name)).
git_entry(object(_Mode, blob, _Hash, Size, Name), file(Name, Size)).

ar_pack_members(Archive, Members, Prefix) :-
	(   ar_members_cache(Archive, Members0, Prefix0)
	->  true
	;   read_ar_pack_members(Archive, Members0, Prefix0)
	->  asserta(ar_members_cache(Archive, Members0, Prefix0))
	),
	Members = Members0,
	Prefix  = Prefix0.

read_ar_pack_members(Archive, Members0, Prefix) :-
	setup_call_cleanup(
	    archive_open(Archive, Handle, []),
	    findall(Member, ar_member(Handle, Member), Members0),
	    archive_close(Handle)),
	(   member(file(InfoFile,_), Members0),
	    atom_concat(Prefix, 'pack.pl', InfoFile)
	->  true
	;   existence_error(pack_file, 'pack.pl')
	).

ar_member(Handle, Entry) :-
	repeat,
	(   archive_next_header(Handle, File)
	->  true
	;   !, fail
	),
	archive_header_property(Handle, filetype(Type)),
	make_entry(Type, Handle, File, Entry).

make_entry(file, Handle, File, file(File, Size)) :- !,
	archive_header_property(Handle, size(Size)).
make_entry(link, Handle, File, link(File, Target)) :- !,
	archive_header_property(Handle, link_target(Target)).
make_entry(directory, _, _, _) :- !,
	fail.
make_entry(Type, _, Name, Entry) :-
	atom(Type), !,
	Entry =.. [Type, Name].
make_entry(Type, _, Name, _Entry) :-
	print_message(warning, unknown_archive_type(Type, Name)),
	fail.

strip_prefix(Prefix, Term0, Term) :-
	Term0 =.. [Type, Name, Size],
	atom_concat(Prefix, Stripped, Name),
	Term =.. [Type, Stripped, Size].

file_entry(Pack, File, file(File,Size)) :-
	directory_file_path(Pack, File, Path),
	size_file(Path, Size).

%%	recursive_directory_files(+Dir, -Files) is det.
%
%	True when Files is a list holding all files in Dir, recursively.

recursive_directory_files(Dir, Files) :-
	dir_prefix(Dir, Prefix),
	recursive_directory_files(Dir, Prefix, Files, []).

recursive_directory_files(Dir, Prefix, AllFiles, Rest) :-
	directory_files(Dir, Files),
	dir_files(Files, Dir, Prefix, AllFiles, Rest).

dir_files([], _, _, Files, Files).
dir_files([H|T], Dir, Prefix, Files, Rest) :-
	(   special(H)
	->  dir_files(T, Dir, Prefix, Files, Rest)
	;   directory_file_path(Dir, H, Entry),
	    (	exists_directory(Entry)
	    ->	recursive_directory_files(Entry, Prefix, Files, Rest0)
	    ;	atom_concat(Prefix, File, Entry),
		Files = [File|Rest0]
	    ),
	    dir_files(T, Dir, Prefix, Rest0, Rest)
	).

dir_prefix(., '') :- !.
dir_prefix(Dir, Prefix) :-
	(   sub_atom(Dir, _, _, 0, /)
	->  Prefix = Dir
	;   atom_concat(Dir, /, Prefix)
	).

special(.).
special(..).

		 /*******************************
		 *	     XREF HOOKS		*
		 *******************************/

:- multifile
	prolog:xref_open_source/2,
	prolog:xref_source_identifier/2,
	prolog:xref_source_file/3.

%%	prolog:xref_open_source(+Id, -Stream) is semidet.
%
%	If Id refers to a known  Prolog   pack,  open  the pack entry. A
%	pack-file identifier is the path-name  of   the  archive or pack
%	directory, followed by the entry in the pack.

prolog:xref_open_source(File, Stream) :-
	pack_prefix(Pack, Prefix),
	atom_concat(Prefix, Entry, File),
	pack_open_entry(Pack, Entry, Stream).

%%	prolog:xref_source_identifier(+Path, -Id) is semidet.

prolog:xref_source_identifier(Path, Path) :-
	atom(Path),
	pack_mirror_directory(MirrorDir),
	sub_atom(Path, 0, _, _, MirrorDir),
	atom(Path),
	pack_prefix(_Pack, Prefix),
	sub_atom(Path, 0, _, _, Prefix), !.

%%	pack_file(+Path, -Pack, -Entry) is semidet.
%
%	True if Path originates from Entry in Pack.

pack_file(Path, Pack, Entry) :-
	pack_prefix(Pack, Prefix),
	atom_concat(Prefix, Entry, Path),
	pack_members(Pack, Members),
	memberchk(file(Entry,_Size), Members).

%%	resolve_pack_file(+Spec, -Source, -SourcePack, -SourceEntry) is	nondet.
%
%	True if Spec appearing in OrgPack can  be resolved by file Entry
%	in ResPack.

resolve_pack_file(library(File), Source, SourcePack, SourceEntry) :-
	(   atom(File)
	->  FileName = File
	;   path_segments_atom(File, FileName)
	),
	directory_file_path(prolog, FileName, EntryNoExt),
	user:prolog_file_type(Ext, prolog),
	file_name_extension(EntryNoExt, Ext, SourceEntry),
	pack_file(Source, SourcePack, SourceEntry).

%%	assert_dependency(OrgPack, OrgSpec, How, Src) is det.

assert_dependency(OrgPack, OrgSpec, How, Src) :-
	pack_dependency(OrgPack, OrgSpec, How, Src), !.
assert_dependency(OrgPack, OrgSpec, How, Src) :-
	asserta(pack_dependency(OrgPack, OrgSpec, How, Src)).

%%	prolog:xref_source_file(+Spec, -SourceID, +Options) is semidet.

prolog:xref_source_file(library(File), Source, Options) :-
	option(relative_to(Origin), Options),
	pack_file(Origin, OrgPack, _OrigEntry),
	debug(pack(xref), 'Search for ~q from pack ~q',
	      [library(File), OrgPack]),
	findall(t(Src, SrcPack, SrcEntry),
		resolve_pack_file(library(File), Src, SrcPack, SrcEntry),
		Triples),
	(   select(t(Source, OrgPack, _), Triples, Alt)
	->  true
	;   select(t(Source, _, _), Triples, Alt),
	    assert_dependency(OrgPack, library(File), dep, Source)
	),
	forall(member(t(AltSrc,_,_), Alt),
	       assert_dependency(OrgPack, library(File), alt, AltSrc)).
prolog:xref_source_file(Spec, Source, _Options) :-
	atom(Spec),
	(   pack_file(Spec, _, _)
	->  Source = Spec
	;   user:prolog_file_type(Ext, prolog),
	    file_name_extension(Spec, Ext, Source),
	    pack_file(Source, _, _)
	),
	debug(pack(xref), 'Resolved ~q to ~q', [Spec, Source]).

%%	xref_pack_source(+Pack, ?Entry, ?Source) is nondet.
%
%	True when Source is the canonical xref source of Entry in Pack.

xref_pack_source(Pack, Entry, Source) :-
	xref_current_source(Source),
	pack_prefix(Pack, Prefix),
	atom_concat(Prefix, Entry, Source).


pack_prefix(Archive, Prefix) :-
	pack_archive(_Pack, _Hash, Archive),
	atom_concat(Archive, /, Prefix).
