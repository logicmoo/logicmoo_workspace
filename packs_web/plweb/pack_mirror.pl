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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(pack_mirror,
	  [ pack_mirror/3,		% +Pack, -MirrorArchive, -Hash
	    pack_unmirror/1,		% +Pack
	    pack_mirror_directory/1	% -Dir
	  ]).
:- use_module(pack).
:- use_module(library(sha)).
:- use_module(library(git)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(debug)).

:- debug(pack(mirror)).

/** <module> Mirror pack archives

This module maintains mirrors of the latest versions of pack archives as
they are registered. This data will be   used  to maintain a database of
meta-information on packs.
*/

pack_mirror_dir('pack/mirror').

%!	pack_mirror_directory(-Dir)
%
%	True when Dir is the absolute file name for the mirrors.

:- dynamic
	cached_pack_mirror_dir/1.

pack_mirror_directory(Dir) :-
	cached_pack_mirror_dir(Dir), !.
pack_mirror_directory(Dir) :-
	pack_mirror_dir(Dir0),
	absolute_file_name(Dir0, Dir,
			   [ access(read),
			     file_type(directory),
			     file_errors(fail)
			   ]),
	asserta(cached_pack_mirror_dir(Dir)).


%%	pack_mirror(+Pack, -File, -Hash) is semidet.
%
%	Try to mirror the latest version of  Pack into File. Hash is the
%	SHA1 hash of the pack archive.  If   the  hash of the downloaded
%	file does not match, the download file is deleted.

pack_mirror(Pack, Mirror, Hash) :-
	pack_version_hashes(Pack, [_Latest-Hashes|_Older]),
	pack_mirror(Pack, Hashes, Mirror, Hash).

pack_mirror(Pack, Hashes, MirrorDir, Hash) :-
	setof(GitURL, hashes_git_url(Hashes, GitURL), GitURLs),
	pack_git_mirror(Pack, MirrorDir),
	GitOptions = [directory(MirrorDir), askpass(path(echo))],
	(   exists_directory(MirrorDir)
	->  (   Hashes = [Hash],
		git_hash(Hash, GitOptions)
	    ->	true
	    ;	forall(member(Hash, Hashes),
		       git_has_commit(MirrorDir, Hash))
	    ->	git_hash(Hash, GitOptions)
	    ;	member(URL, GitURLs),
	        git_remote_url(origin, URL, GitOptions),
		debug(pack(mirror), 'git pull in ~q', [MirrorDir]),
		catch(git([pull], GitOptions), E,
		      ( print_message(warning, E), fail))
	    ->	git_hash(Hash, GitOptions)
	    ;	print_message(warning, pack_mirror(Pack)), % TBD
		fail
	    )
	;   member(URL, GitURLs),
	    debug(pack(mirror), 'git clone ~q into ~q', [URL, MirrorDir]),
	    catch(git([clone, URL, MirrorDir], [askpass(path(echo))]), E,
		  ( print_message(warning, E), fail))
	->  git_hash(Hash, GitOptions)
	), !.
pack_mirror(_Pack, Hashes, File, Hash) :-
	member(Hash, Hashes),
	hash_file_url(Hash, URL),
	hash_file(Hash, File),
	(   exists_file(File)
	;   pack_url_hash(URL, Hash),
	    debug(pack(mirror), 'Downloading ~q into ~q', [URL, File]),
	    catch(setup_call_cleanup(
		      http_open(URL, In,
				[ cert_verify_hook(ssl_verify)
				]),
		      setup_call_cleanup(
			  open(File, write, Out, [type(binary)]),
			  copy_stream_data(In, Out),
			  close(Out)),
		      close(In)),
		  E,
		  ( print_message(warning, E),
		    fail
		  )),
	    file_sha1(File, FileSHA1),
	    (	Hash == FileSHA1
	    ->	true
	    ;	print_message(warning,
			      pack(hash_mismatch(URL, Hash, FileSHA1))),
		delete_file(File),
		fail
	    )
	), !.

hashes_git_url(Hashes, URL) :-
	member(Hash, Hashes),
	hash_git_url(Hash, URL).

%%	git_has_commit(+Repo, +Commit)
%
%	True if Repo contains Commit.  Cashed,   which  is  safe because
%	objects to not vanish in GIT.

:- dynamic
	git_commit_in_repo/2.

git_has_commit(Repo, Commit) :-
	git_commit_in_repo(Commit, Repo), !.
git_has_commit(Repo, Commit) :-
	catch(git_branches(_,
			   [ commit(Commit),
			     error(_),
			     directory(Repo)
			   ]), _, fail),
	assertz(git_commit_in_repo(Commit, Repo)).

%%	pack_unmirror(+Pack)
%
%	Delete all mirrors we have for Pack

pack_unmirror(Pack) :-
	(   pack_git_mirror(Pack, MirrorDir),
	    exists_directory(MirrorDir)
	->  print_message(informational, pack(unmirror(dir(MirrorDir)))),
	    catch(delete_directory_and_contents(MirrorDir), E,
		  print_message(warning, E))
	;   true
	),
	pack_version_hashes(Pack, VersionHashes),
	forall(member(_Version-Hashes, VersionHashes),
	       forall(member(Hash, Hashes),
		      delete_mirror_hash(Hash))).

delete_mirror_hash(Hash) :-
	hash_file(Hash, File),
	(   exists_file(File)
	->  print_message(informational, pack(unmirror(file(File)))),
	    catch(delete_file(File), E, print_message(warning, E))
	;   true
	).

:- public ssl_verify/5.

%%	ssl_verify(+SSL, +ProblemCert, +AllCerts, +FirstCert, +Error)
%
%	Currently we accept  all  certificates.   We  organise  our  own
%	security using SHA1 signatures, so  we   do  not  care about the
%	source of the data.

ssl_verify(_SSL,
	   _ProblemCertificate, _AllCertificates, _FirstCertificate,
	   _Error).

%%	hash_file(+Hash, -File) is det.
%
%	True when File is the location for storing Hash

hash_file(Hash, File) :-
	pack_mirror_dir(Root),
	sub_atom(Hash, 0, 2, _, Dir0),
	sub_atom(Hash, 2, 2, _, Dir1),
	atomic_list_concat([Root, Dir0, Dir1], /, Dir),
	make_directory_path(Dir),
	directory_file_path(Dir, Hash, File).

%%	pack_git_mirror(+Pack, -GitDir)
%
%	True when MirrorDir is the directory in which we mirror Pack.

pack_git_mirror(Pack, GitDir) :-
	pack_mirror_dir(Root),
	directory_file_path(Root, 'GIT', GitRoot),
	make_directory_path(GitRoot),
	directory_file_path(GitRoot, Pack, GitDir).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message//1.

prolog:message(pack(hash_mismatch(URL, Hash, FileSHA1))) -->
	[ '~q: Hash mismatch'-[URL], nl,
	  '   Got      ~w'-[FileSHA1], nl,
	  '   Expected ~w'-[Hash]
	].
prolog:message(pack(mirror_failed(Pack))) -->
	[ 'Mirror for pack ~q failed'-[Pack] ].
prolog:message(pack(unmirror(dir(MirrorDir)))) -->
	[ 'Deleting GIT mirror directory ~p'-[MirrorDir] ].
prolog:message(pack(unmirror(file(Hash)))) -->
	[ 'Deleting mirror archive ~p'-[Hash] ].

