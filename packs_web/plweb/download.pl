/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009-2019, VU University Amsterdam
			      CWI, Amsterdam

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

:- module(plweb_download, []).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_dirindex)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(dcg/basics)).
:- use_module(library(broadcast)).
:- use_module(library(pairs)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(filesex)).
:- use_module(library(persistency)).
:- use_module(library(crypto)).
:- use_module(library(random)).
:- use_module(wiki).

%%	download(+Request) is det.
%
%	HTTP handler for SWI-Prolog download pages.

:- http_handler(download(devel),        download_table, []).
:- http_handler(download(stable),       download_table, []).
:- http_handler(download(old),          download_table, []).
:- http_handler(download('daily/bin/'), download_daily, []).
:- http_handler(download(.),	        download,
		[prefix, spawn(download), priority(10)]).
:- http_handler(root(download),	        http_redirect(moved, download(.)),
		[priority(10)]).

%%	download_table(+Request)
%
%	Provide a table with possible download targets.
%	test edit

download_table(Request) :-
	http_parameters(Request,
			[ show(Show, [oneof([all,latest]), default(latest)])
			]),
	memberchk(path(Path), Request),
	http_absolute_location(root(download), DownLoadRoot, []),
	atom_concat(DownLoadRoot, DownLoadDir, Path),
	absolute_file_name(download(DownLoadDir),
			   Dir,
			   [ file_type(directory),
			     access(read)
			   ]),
	list_downloads(Dir, [show(Show), request(Request)]).

%%	list_downloads(+Directory)

list_downloads(Dir, Options) :-
	(   wiki_file_to_dom(Dir, 'header.txt', Header0)
	->  (   Header0 = [h1(_, Title)|Header]
	    ->	true
	    ;	Header = Header0
	    )
	;   Header = []
	),
	(   var(Title)
	->  Title = 'SWI-Prolog downloads'
	;   true
	),
	reply_html_page(
	    download(Dir, Title),
	    title('SWI-Prolog downloads'),
	    [ \html(Header),
	      br(clear(all)),
	      table(class(downloads),
		    \download_table(Dir, Options)),
	      \machine_download_text,
	      \wiki(Dir, 'footer.txt')
	    ]).

wiki_file_to_dom(Dir, File, DOM) :-
	directory_file_path(Dir, File, WikiFile),
	access_file(WikiFile, read), !,
	wiki_file_to_dom(WikiFile, DOM).

wiki(Dir, File) -->
	{ wiki_file_to_dom(Dir, File, DOM) }, !,
	html(DOM).
wiki(_, _) -->
	[].

machine_download_text -->
	html({|html||
<div class="machine-download">
Install scripts may download the SHA256 checksum by appending
<code>.sha256</code> to the file name.  Scripts can download
the latest version by replacing the version of the file with
<code>latest</code>.  This causes the server to reply with the
location of the latest version using an
<code>HTTP 303 See Other</code> message.
</div>
	     |}).


download_table(Dir, Options) -->
	list_files(Dir, bin, bin,    'Binaries',         Options),
	list_files(Dir, src, src,    'Sources',          Options),
	list_files(Dir, doc, doc,    'Documentation',    Options),
	toggle_show(Options).

%%	toggle_show(+Options) is det.
%
%	Add a toggle to switch between   showing only the latest version
%	and all versions.

toggle_show(Options) -->
	{ option(request(Request), Options),
	  memberchk(path(Path), Request), !,
	  file_base_name(Path, MySelf),
	  (   option(show(all), Options)
	  ->  NewShow = latest
	  ;   NewShow = all
	  )
	},
	html(tr(td([class(toggle), colspan(3)],
		   a(href(MySelf+'?show='+NewShow),
		     [ 'Show ', NewShow, ' files' ])))).
toggle_show(_) -->
	[].

%%	list_files(+Dir, +SubDir, +Class, +Label, +Options) is det.
%
%	Create table rows for all  files   in  Dir/SubDir.  If files are
%	present, emit a =tr= with Label  and   a  =tr= row for each each
%	matching file.  Options are:
%
%	    * show(Show)
%	    One of =all= or =latest= (default).

list_files(Dir, SubDir, Class, Label, Options) -->
	{ directory_file_path(Dir, SubDir, Directory),
	  download_files(Directory, Class, Files, Options),
	  Files \== []
	},
	html(tr(th(colspan(3), Label))),
	list_files(Files).
list_files(_, _, _, _, _) -->
	[].

list_files([]) --> [].
list_files([H|T]) -->
	list_file(H),
	list_files(T).

list_file(File) -->
	html(tr(class(download),
		[ td(class(dl_icon), \file_icon(File)),
		  td(class(dl_size), \file_size(File)),
		  td(class(dl_file), \file_description(File))
		])).

file_icon(file(Type, PlatForm, _, _, _)) -->
	{ icon_for_file(Type, PlatForm, Icon, Alt), !,
	  http_absolute_location(icons(Icon), HREF, [])
	},
	html(img([src(HREF), alt(Alt)])).
file_icon(_) -->
	html(?).			% no defined icon

icon_for_file(bin, linux(universal),
	      'linux.png', 'Linux 32/64 intel').
icon_for_file(bin, linux(_,_),
	      'linux32.gif', 'Linux RPM').
icon_for_file(bin, macos(lion,_),
	      'lion.png', 'Lion').
icon_for_file(bin, macos(snow_leopard,_),
	      'snowleopard.gif', 'Snow Leopard').
icon_for_file(bin, macos(snow_leopard_and_later,_),
	      'macapp.png', 'Snow Leopard and later').
icon_for_file(bin, macos(bundle,_),
	      'macapp.png', 'MacOS bundle').
icon_for_file(bin, macos(_,_),
	      'mac.gif', 'MacOSX version').
icon_for_file(_, windows(win32),
	      'win32.gif', 'Windows version (32-bits)').
icon_for_file(_, windows(win64),
	      'win64.gif', 'Windows version (64-bits)').
icon_for_file(src, _,
	      'src.gif', 'Source archive').
icon_for_file(_, pdf,
	      'pdf.gif', 'PDF file').


file_size(file(_, _, _, _, Path)) -->
	{ size_file(Path, Bytes)
	},
	html('~D bytes'-[Bytes]).

file_description(file(bin, PlatForm, Version, _, Path)) -->
	{ down_file_href(Path, HREF)
	},
	html([ a(href(HREF),
		 [ 'SWI-Prolog ', \version(Version), ' for ',
		   \platform(PlatForm)
		 ]),
	       \platform_notes(PlatForm, Path),
	       \checksum(Path)
	     ]).
file_description(file(src, Format, Version, _, Path)) -->
	{ down_file_href(Path, HREF)
	},
	html([ a(href(HREF),
		 [ 'SWI-Prolog source for ', \version(Version)
		 ]),
	       \platform_notes(Format, Path),
	       \checksum(Path)
	     ]).
file_description(file(doc, Format, Version, _, Path)) -->
	{ down_file_href(Path, HREF)
	},
	html([ a(href(HREF),
		 [ 'SWI-Prolog ', \version(Version),
		   ' reference manual in PDF'
		 ]),
	       \platform_notes(Format, Path)
	     ]).
file_description(file(pkg(Pkg), PlatForm, Version, _, Path)) -->
	{ down_file_href(Path, HREF)
	},
	html([ a(href(HREF),
		 [ \package(Pkg), ' (version ', \version(Version), ') for ',
		   \platform(PlatForm)
		 ]),
	       \platform_notes(pkg(Pkg), Path)
	     ]).

package(Name) -->
	html([ 'Package ', Name ]).

version(version(Major, Minor, Patch, '')) --> !,
	html(b('~w.~w.~w'-[Major, Minor, Patch])).
version(version(Major, Minor, Patch, Tag)) -->
	html(b('~w.~w.~w-~w'-[Major, Minor, Patch, Tag])).

checksum(Path) -->
	{ file_checksum(Path, SHA256) },
	html(div([ class(checksum),
		   title('You can use the checksum to verify the integrity \c
		          of the downloaded file.  It provides some protection \c
			  against deliberate tamporing with the file.')
		 ],
		 [ span(class('checkum-header'), 'SHA256'), :,
		   span(class([checksum,sha256]), SHA256)
		 ])).

down_file_href(Path, HREF) :-
	absolute_file_name(download(.),
			   Dir,
			   [ file_type(directory),
			     access(read)
			   ]),
	atom_concat(Dir, SlashLocal, Path),
	delete_leading_slash(SlashLocal, Local),
	add_envelope(Local, SafeLocal),
	http_absolute_location(download(SafeLocal), HREF, []).

delete_leading_slash(SlashPath, Path) :-
	atom_concat(/, Path, SlashPath), !.
delete_leading_slash(Path, Path).

platform(linux(universal)) -->
	html(['Linux 32/64 bits (TAR)']).
platform(linux(rpm, _)) -->
	html(['i586/Linux (RPM)']).
platform(macos(Name, CPU)) -->
	html(['MacOSX ', \html_macos_version(Name), ' on ', b(CPU)]).
platform(windows(win32)) -->
	html(['Microsoft Windows (32 bit)']).
platform(windows(win64)) -->
	html(['Microsoft Windows (64 bit)']).

html_macos_version(tiger)        --> html('10.4 (Tiger)').
html_macos_version(leopard)      --> html('10.5 (Leopard)').
html_macos_version(snow_leopard) --> html('10.6 (Snow Leopard)').
html_macos_version(lion)	 --> html('10.7 (Lion)').
html_macos_version(snow_leopard_and_later) --> html('10.6 (Snow Leopard) and later').
html_macos_version(bundle)       --> html('10.12 (Sierra) and later').
html_macos_version(OS)	         --> html(OS).

%%	platform_notes(+Platform, +Path) is det.
%
%	Include notes on the platform. These notes  are stored in a wiki
%	file in the same directory as the download file.

platform_notes(Platform, Path) -->
	{ file_directory_name(Path, Dir),
	  platform_note_file(Platform, File),
	  atomic_list_concat([Dir, /, File], NoteFile),
	  debug(download, 'Trying note-file ~q', [NoteFile]),
	  access_file(NoteFile, read), !,
	  debug(download, 'Found note-file ~q', [NoteFile]),
	  wiki_file_to_dom(NoteFile, DOM)
	},
	html(DOM).
platform_notes(_, _) -->
	[].

platform_note_file(linux(rpm,_),     'linux-rpm.txt').
platform_note_file(linux(universal), 'linux.txt').
platform_note_file(windows(win32),   'win32.txt').
platform_note_file(windows(win64),   'win64.txt').
platform_note_file(pkg(Pkg),         File) :-
	file_name_extension(Pkg, txt, File).
platform_note_file(macos(Version,_), File) :-
	atomic_list_concat([macosx, -, Version, '.txt'], File).
platform_note_file(macos(_,_),	     'macosx.txt').
platform_note_file(tgz,		     'src-tgz.txt').
platform_note_file(pdf,		     'doc-pdf.txt').


		 /*******************************
		 *	   CLASSIFY FILES	*
		 *******************************/

%%	download_files(+Dir, +Class, -Files, +Options)
%
%	Files is a list of  files  that   satisfy  Class  and Options in
%	Dir/Subdir.

:- dynamic
	download_cache/6.  % Hash, Dir, Class, Opts, Time, Files

download_files(Dir, Class, Files, Options0) :-
	exists_directory(Dir), !,
	include(download_option, Options0, Options),
	term_hash(ci(Dir,Class,Options), Hash),
	time_file(Dir, DirTime),
	(   download_cache(Hash, Dir, Class, Options, Time, Files0),
	    (	DirTime == Time
	    ->	true
	    ;	retractall(download_cache(Hash, Dir, Class, Options, _, _)),
		fail
	    )
	->  true
	;   download_files_nc(Dir, Class, Files0, Options),
	    asserta(download_cache(Hash, Dir, Class, Options, DirTime, Files0))
	),
	Files = Files0.
download_files(_, _, [], _).

clear_download_cache :-
	retractall(download_cache(_Hash, _Dir, _Class, _Options, _Time, _Files0)).

download_option(show(_)).


download_files_nc(Directory, Class, Sorted, Options) :-
	atom_concat(Directory, '/*', Pattern),
	expand_file_name(Pattern, Files),
	classify_files(Files, Class, Classified, Options),
	sort_files(Classified, Sorted, Options).

classify_files([], _, [], _).
classify_files([H0|T0], Class, [H|T], Options) :-
	classify_file(H0, H, Options),
	arg(1, H, Classification),
	subsumes_term(Class, Classification), !,
	classify_files(T0, Class, T, Options).
classify_files([_|T0], Class, T, Options) :-
	classify_files(T0, Class, T, Options).

%%	classify_file(+Path, -Term, +Options) is semidet.

classify_file(Path, file(Type, Platform, Version, Name, Path), Options) :-
	file_base_name(Path, Name),
	atom_codes(Name, Codes),
	phrase(file(Type, Platform, Version, Options), Codes).

file(bin, macos(OSVersion, CPU), Version, Options) -->
	{ option(show(all), Options) },
	"swi-prolog-", opt_devel, long_version(Version), "-",
	macos_version(OSVersion),
	(   "-",
	    macos_cpu(CPU)
	->  ""
	;   { macos_def_cpu(OSVersion, CPU) }
	),
	".mpkg.zip", !.
% Cmake version
file(bin, macos(bundle, intel), Version, _) -->
	"swipl-", long_version(Version), opt_release(_),
	opt_cpu(_),
	".dmg", !.
file(bin, macos(snow_leopard_and_later, intel), Version, _) -->
	"SWI-Prolog-", long_version(Version),
	".dmg", !.
file(bin, windows(WinType), Version, _) -->
	"swipl-", long_version(Version), opt_release(_),
	cmake_win_type(WinType),
	".exe", !.
file(bin, windows(WinType), Version, _) -->
	win_type(WinType), "pl",
	short_version(Version),
	".exe", !.
file(bin, windows(WinType), Version, _) -->
	swipl, win_type(WinType), "-",
	short_version(Version),
	".exe", !.
file(bin, linux(rpm, suse), Version, _) -->
	swipl, long_version(Version), "-", digits(_Build), ".i586.rpm", !.
file(bin, linux(universal), Version, _) -->
	"swipl-",
	long_version(Version), "-", "linux",
	".tar.gz", !.
file(src, tgz, Version, _) -->
	swipl, long_version(Version), ".tar.gz", !.
file(doc, pdf, Version, _) -->
	"SWI-Prolog-", long_version(Version), ".pdf", !.

swipl --> "swipl-", !.
swipl --> "pl-".

opt_release(Rel) --> "-", int(Rel, 4), !.
opt_release(-)   --> "".

opt_devel --> "devel-", !.
opt_devel --> "".

opt_cpu(x86_64) --> ".", "x86_64", !.
opt_cpu(unknown) --> "".

macos_version(tiger)        --> "tiger".
macos_version(leopard)      --> "leopard".
macos_version(snow_leopard) --> "snow-leopard".
macos_version(lion)         --> "lion".

macos_cpu(ppc)   --> "powerpc".
macos_cpu(intel) --> "intel".
macos_cpu(x86)   --> "32bit".

macos_def_cpu(snow_leopard, intel) :- !.
macos_def_cpu(lion, intel) :- !.
macos_def_cpu(_, ppc).

win_type(win32) --> "w32".
win_type(win64) --> "w64".

cmake_win_type(win64) --> ".", "x64".
cmake_win_type(win32) --> ".", "x86".

long_version(version(Major, Minor, Patch, Tag)) -->
	int(Major, 1), ".", int(Minor, 2), ".", int(Patch, 2), !,
        tag(Tag), !.
long_version(latest) -->
	"latest".

tag(Tag) -->
	"-", alnums(Codes), !,
        { atom_codes(Tag, Codes) }.
tag('') --> "".

int(Value, MaxDigits) -->
	digits(Digits),
	{ length(Digits, Len),
	  Len =< MaxDigits,
	  Len > 0,
	  number_codes(Value, Digits)
	}.

alnums([H|T]) -->
	[H], { code_type(H, alnum) }, !,
        alnums(T).
alnums([]) --> "".

short_version(version(Major, Minor, Patch, Tag)) -->
	digits(Digits),
	{   Digits = [D1,D2,D3]
	->  number_codes(Major, [D1]),
	    number_codes(Minor, [D2]),
	    number_codes(Patch, [D3])
	;   Digits = [D1,D2,D3,D4]
	->  (   number_codes(51, [D1,D2])		% 5.1X.Y
	    ->  number_codes(Major, [D1]),
	        number_codes(Minor, [D2,D3]),
		number_codes(Patch, [D4])
	    ;   number_codes(Major, [D1]),
	        number_codes(Minor, [D2]),
		number_codes(Patch, [D3,D4])
	    )
	;   Digits = [D1,D2,D3,D4,D5]
	->  number_codes(Major, [D1]),
	    number_codes(Minor, [D2,D3]),
	    number_codes(Patch, [D4,D5])
	},
        tag(Tag), !.
short_version(latest) -->
	"latest".

%%	sort_files(+In, -Out, +Options)
%
%	Sort files by type and version. Type: linux, windows, mac, src,
%	doc.  Versions: latest first.
%
%	Options:
%
%	    * show(Show)
%	    One of =all= or =latest=.

sort_files(In, Out, Options) :-
	map_list_to_pairs(map_type, In, Typed0),
	(   option(show(all), Options)
	->  Typed = Typed0
	;   exclude(old_tagged_file, Typed0, Typed)
	),
	keysort(Typed, TSorted),
	group_pairs_by_key(TSorted, TGrouped),
	maplist(sort_group_by_version, TGrouped, TGroupSorted),
	(   option(show(all), Options)
	->  pairs_values(TGroupSorted, TValues),
	    flatten(TValues, Out)
	;   take_latest(TGroupSorted, Out)
	).

map_type(File, Tag) :-
	File = file(Type, Platform, _Version, _Name, _Path),
	type_tag(Type, Platform, Tag).

type_tag(bin, linux(A),   tag(10, linux(A))) :- !.
type_tag(bin, linux(A,B), tag(11, linux(A,B))) :- !.
type_tag(bin, windows(A), tag(Tg, windows(A))) :- !,
	win_tag(A, Tg2),
        Tg is 20+Tg2.
type_tag(bin, macos(A,B), tag(Tg, macos(A,B))) :- !,
	mac_tag(A, Tg2),
	Tg is 30+Tg2.
type_tag(src, Format,     tag(40, Format)) :- !.
type_tag(doc, Format,     tag(50, Format)) :- !.
type_tag(X,   Y,	  tag(60, X-Y)).

mac_tag(bundle,			4).
mac_tag(snow_leopard_and_later,	5).
mac_tag(lion,			6).
mac_tag(snow_leopard,		7).
mac_tag(leopard,		8).
mac_tag(tiger,			9).

win_tag(win64, 1).
win_tag(win32, 2).

sort_group_by_version(Tag-Files, Tag-Sorted) :-
	map_list_to_pairs(tag_version, Files, TFiles),
	keysort(TFiles, TRevSorted),
	pairs_values(TRevSorted, RevSorted),
	reverse(RevSorted, Sorted).

tag_version(File, Tag) :-
	File = file(_,_,Version,_,_),
	version_tag(Version, Tag).

version_tag(version(Major, Minor, Patch, Tag),
	    version(Major, Minor, Patch, Order)) :-
	(   pre_version(Tag, Order)
	->  true
	;   print_message(error,
			  error(domain_error(pre_release_version, Tag),_)),
	    Order = pre(-100, 0)
	).

pre_version('', pre(0, 0)) :- !.
pre_version(NrA, pre(0, 0)) :-
	atom_number(NrA, _Nr), !.
pre_version(Tag, pre(TagOrder, N)) :-
	tag(TagPrefix, TagOrder),
	atom_concat(TagPrefix, NA, Tag),
	atom_number(NA, N).

tag(rc,    -1).
tag(beta,  -2).
tag(alpha, -3).

take_latest([], []).
take_latest([_-[H|_]|T0], [H|T]) :- !,
	take_latest(T0, T).
take_latest([_-[]|T0], T) :- !,		% emty set
	take_latest(T0, T).

%%	old_tagged_file(+TypeFile) is semidet.

old_tagged_file(tag(_,Type)-_File) :-
	old_file_type(Type).

old_file_type(linux(_)).
old_file_type(linux(_,_)).
old_file_type(macos(_,ppc)).
old_file_type(macos(tiger,_)).
old_file_type(macos(snow_leopard_and_later,_)).


		 /*******************************
		 *	     DOWNLOAD		*
		 *******************************/

%%	download(+Request) is det.
%
%	Actually download a file.  Two special requests are supported:
%
%	  - By postfixing the file with `.sha256` you get the SHA256
%	    checksum rather than the file.
%	  - If you replace the version with `latest` you get an HTTP
%	    303 (See Other) reply pointing at the latest version.

download(Request) :-
	memberchk(path_info(Download), Request),
	file_name_extension(File, envelope, Download), !,
	envelope(File).
download(Request) :-
	memberchk(path_info(Download), Request),
	(   file_name_extension(File, sha256, Download)
	->  true
	;   File = Download
	),
	download_file(File, AbsFile),
	(   File == Download
	->  http_peer(Request, Remote),
	    broadcast(download(File, Remote)),
	    http_reply_file(AbsFile, [unsafe(true)], Request)
	;   file_checksum(AbsFile, SHA256),
	    format('Content-type: text/plain~n~n'),
	    format('~w~n', [SHA256])
	).
download(Request) :-
	memberchk(path_info(Download), Request),
	classify_file(Download, file(Class,Platform,latest,_,_), [show(last)]),
	file_directory_name(Download, Dir),
	absolute_file_name(download(Dir),
			   AbsDir,
			   [ access(read),
			     file_type(directory),
			     file_errors(fail)
			   ]),
	download_files(AbsDir, Class, Files, [show(last)]),
	memberchk(file(Class, Platform, _, File, _), Files), !,
	directory_file_path(Dir, File, Redirect),
	http_link_to_id(download, path_postfix(Redirect), URI),
	http_redirect(see_other, URI, Request).
download(Request) :-
	(   memberchk(path_info(Download), Request)
	->  true
	;   Download = '.'
	),
	absolute_file_name(download(Download),
			   AbsFile,
			   [ access(read),
			     file_errors(fail),
			     file_type(directory)
			   ]), !,
	http_reply_dirindex(AbsFile,
			    [ unsafe(true),
			      name(name_cell)
			    ], Request).
download(Request) :-
	memberchk(path(Path), Request),
	existence_error(http_location, Path).

download_file(File, AbsFile) :-
	absolute_file_name(download(File),
			   AbsFile,
			   [ access(read),
			     file_errors(fail)
			   ]).

:- public
	name_cell//1.

name_cell(File) -->
	{ needs_envelope(File),
	  file_base_name(File, Name),
	  uri_encoded(path, Name, Ref0),
	  file_name_extension(Ref0, envelope, Ref)
	},
	html(a(href(Ref), Name)).
name_cell(File) -->
	{ file_base_name(File, Name),
	  uri_encoded(path, Name, Ref)
	},
	html(a(href(Ref), Name)).

%%	download_daily(+Request)
%
%	Provide the download page for the windows binaries.

download_daily(_Request) :-
	absolute_file_name(download('daily/bin'), Dir,
			   [ file_type(directory),
			     access(read)
			   ]),
	reply_html_page(
	    download(Dir, 'Download daily builds for Windows'),
	    title('Download daily builds for Windows'),
	    [ \explain_win_daily,
	      \directory_index(Dir,
			       [ order_by(time),
				 order(descending),
				 name(name_cell)
			       ])
	    ]).


explain_win_daily -->
	html({|html||
	      <p>The table below provides access to the most recent 7
	      daily builds of SWI-Prolog for Windows, both the 32- and
	      64-bit versions.  The build is done automatically from the
	      <a href="/git/">GIT sources</a>.  The files use the following
	      naming convention:
	      </p>
	      <ul>
	        <li><code>swipl-w</code><var>bits</var><code>-</code><var>date</var><code>.exe</code>
	      </ul>
	      <p>
	      Please note that these versions <b>may be unstable!</b>  It is
	      adviced to follow current discussions on the
	      <a href="/Mailinglist.html">mailing
	      list</a> and/or the git commit messages at
	      <a href="https://github.com/SWI-Prolog/swipl-devel">GitHub</a>.
	      The primary purpose of the daily builds is to quickly provide
	      binaries after a bug report.
	      </p>
	     |}).


		 /*******************************
		 *	      ENVELOPE		*
		 *******************************/

needs_envelope(File) :-
	file_name_extension(_, exe, File).

add_envelope(File, Envelope) :-
	needs_envelope(File),
	!,
	file_name_extension(File, envelope, Envelope).
add_envelope(File, File).

envelope(File) :-
	maybe(0.1),
	download_file(File, AbsFile),
	file_checksum(AbsFile, OkHash),
	compute_file_checksum(AbsFile, NewHash),
	NewHash \== OkHash,
	!,
	reply_html_page(
	    download(File, 'Possibly tampered binary'),
	    title('Possibly tampered binary'),
	    \tampered(File, OkHash, NewHash)).
envelope(File) :-
	file_base_name(File, Base),
	reply_html_page(
	    download(Base, 'Download binary'),
	    title('Download a binary file'),
	    \envelope(File)).

envelope(File) -->
	{ http_absolute_location(icons('alert.gif'), Alert, []),
	  http_absolute_location(icons('vt_logo.png'), VTLogo, []),
	  download_file(File, AbsFile),
	  file_checksum(AbsFile, Hash),
	  file_base_name(File, Base),
	  format(atom(VTHREF), 'https://www.virustotal.com/file/~w/analysis/', Hash)
	},
	html({|html(Base, Hash, VTHREF, VTLogo, Alert)||
<p><img src=Alert style="float:left">
Windows antivirus software works using <i>signatures</i> and <i>heuristics</i>.
Using the huge amount of virusses and malware known today, arbitrary executables
are often <a href="https://en.wikipedia.org/wiki/Antivirus_software#Problems_caused_by_false_positives">falsily classified as malicious</a>.
<a href="https://safebrowsing.google.com/">Google Safe Browsing</a>, used by
most modern browsers, therefore often classifies our Windows binaries as
malware. You can use e.g., <a href="https://www.virustotal.com/gui/home/url">virustotal</a> to verify files with a large number of antivirus programs.
</p>

<p>
Our Windows binaries are cross-compiled on an isolated Linux container.  The
integrity of the binaries on the server is regularly verified by validating its
SHA256 fingerprint.
</p>

<p>
Please select the checkbox below to enable the actual download link.
</p>

<table>
<tr><td><input type="checkbox" id="understand"><td>I understand</tr>
<tr><td><td><a id="download">Download <code>Base</code></a>
<span style="color:#888; font-size:small;">(SHA256: <code>Hash</code>)</span></tr>
<tr><td style="text-align:right"><img src=VTLogo style="width:1.5ex"><td><a href=VTHREF>VIRUSTOTAL Scan Result</a></tr>
</table>
	     |}),
	js_script({|javascript(Base)||
$(function() {
  $("#understand").prop("checked", false)
                  .on("click", function() {
    $("#download").attr("href", Base);
  });
});

		  |}).

tampered(File, OkHash, NewHash) -->
	{ http_absolute_location(icons('alert.gif'), Alert, [])
	},
	html({|html(File, Alert, OkHash, NewHash)||
<p><img src=Alert style="float:left">
The file <code>File</code> SHA256 signature has changed.  Please
report this at <a href="mailto:bugs@swi-prolog.org">bugs@swi-prolog.org</a>
	     |}).


		 /*******************************
		 *	     CHECKSUMS		*
		 *******************************/

:- persistent
	sha256(path:atom,
	       sha256:atom).

attach_db :-
	db_attached('checksum.db'), !.
attach_db :-
	db_attach('checksum.db', []).

%!	file_checksum(+Path:atom, -Sum:atom) is det.
%
%	True when Sum is the SHA256 checksum   of  file. We keep this in
%	the Prolog database because  this   simplifies  uploading files.
%	Although the data under control  of   the  server  and thus more
%	vulnerable than the download area on   disk  because that is not
%	writeable by the server, I think  this   is  also  better from a
%	security point of view because it  requires the attacker to both
%	modify the filesystem and the   server,  something that requires
%	different rights and expertise.

file_checksum(Path, Sum) :-
	attach_db,
	sha256(Path, Sum0), !,
	Sum = Sum0.
file_checksum(Path, Sum) :-
	compute_file_checksum(Path, Sum).

compute_file_checksum(Path, Sum) :-
	crypto_file_hash(Path, Sum,
			 [ encoding(octet),
			   algorithm(sha256)
			 ]),
	assert_sha256(Path, Sum).
