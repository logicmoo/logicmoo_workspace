%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2016 Christoph Wernhard
%%%%
%%%% This file is part of PIE.
%%%%
%%%% PIE is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%% 
%%%% PIE is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%% 
%%%% You should have received a copy of the GNU General Public License
%%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(swilib(info)).

%%%% 
%%%% Import some modules that are possibly not loaded by default to
%%%% ensure that they are in the distro:
%%%% 
:- use_module(folelim(view_support)).
:- use_module(cmprover(runtime_support_cm)).
:- use_module(swilib(xml_writer)).
:- consult(pplatex(load_pplatex)).

% source_path(X) :-
% 	( getenv('TOYELIM_BUILD_SOURCE_PATH', X) -> true
% 	; X = '/Users/ch/provers/folelim'
% 	).

:- dynamic local_load_context/1.

:- prolog_load_context(directory, Dir),
   retractall( local_load_context(_) ),
   assert( local_load_context(Dir) ).
	

lib_path(X) :-
	( getenv('TOYELIM_BUILD_LIB_PATH', X) ->
	  info(0, 'Using lib directory from environment variable: ~q', [X])
	; local_load_context(Dir),
	  concat_atom([X, '/folelim'], Dir) ->
	  info(0, 'Using lib directory from load context: ~q', [X])
	; X = '/Users/ch/w/provers',
	  info(0, 'Using lib directory from default value: ~q', [X])
	).

build_dist(TgtDir) :-
	%% copies all files loaded into the system, so best to use with a
	%% freshly started system
	% build_doc,
	lib_path(LibPath),
	info(0, 'Using target directory: ~q', [TgtDir]),
	ensure_directory(TgtDir),
	SrcExclude = ['build_dist.pl', 'build_te_dist.pl'],
	( source_file(SrcFile),
	  concat_atom([LibPath, '/'], LibPath1),
	  concat_atom([LibPath1, SrcRelative], SrcFile),
	  \+ memberchk(SrcRelative, SrcExclude),
	  concat_atom([TgtDir, '/src/', SrcRelative], TgtFile),
	  ensure_file_directory(TgtFile),
	  info(0, 'Copying ~q -> ~q', [SrcFile, TgtFile]),
	  copy_file(SrcFile, TgtFile),
	  fail
	; true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% build_doc :-
% 	source_path(Source),
% 	concat_atom([Source, '/doc/'], DocPath),
% 	File = 'user_manual.tex',
% 	format(atom(PDFLatex), 'cd ~w ; pdflatex ~w ; pdflatex ~w',
% 	       [DocPath, File, File]),
% 	shell(PDFLatex).
% 	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
ensure_file_directory(File) :-
	file_directory_name(File, Dir),
	ensure_directory(Dir).

ensure_directory(Dir) :-
	( exists_directory(Dir) ->
	  true
	; info(0, 'Creating directory ~q', [Dir]),
	  make_directory_path(Dir)
	).
