%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2016 Christoph Wernhard
%%%%
%%%% This program is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%% 
%%%% This program is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%% 
%%%% You should have received a copy of the GNU General Public License
%%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(tempfiles,
	  [ set_tempfile_directory/1,
	    set_tempfile_mode/1,
	    tempfile/3,
	    delete_tempfiles/1 ]).

:- dynamic tempfile_directory/1.
:- dynamic tempfile_mode/1.
:- dynamic tempfile/2.

set_tempfile_directory(Dir) :-
	retractall( tempfile_directory(_) ),
	assert( tempfile_directory(Dir) ).

%%
%% Mode: 'delete' or 'keep'
%% In mode 'keep' delete_tempfiles/1 has no effects.
%%
set_tempfile_mode(Mode) :-
	retractall( tempfile_mode(_) ),
	assert( tempfile_mode(Mode) ).

:- set_tempfile_directory('/tmp').
:- set_tempfile_mode(delete).

delete_tempfiles :-
	delete_tempfiles(_).

%%%% 
%%%% Group is an atom to identify a set of tempfiles, i.e., deleting all
%%%% tempfiles in a given group.
%%%%
tempfile(Group, Base, TempFile) :-
	Id is random(4294967296),
	tempfile_directory(Dir),
	concat_atom([Dir, '/', Group, '_', Id, '_', Base], TempFile),
	assert( tempfile(Group, TempFile) ).

delete_tempfiles(Group) :-
	tempfile_mode(delete),
	!,
	( retract( tempfile(Group, TempFile) ),
	  ( exists_file(TempFile) ->
	    delete_file(TempFile)
	  ; true
	  ),
	  fail
	; true
	).
delete_tempfiles(_).

:- at_halt(delete_tempfiles).
