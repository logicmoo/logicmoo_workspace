/*
* Copyright (C) 2002, 2003, 2016 Christoph Wernhard
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
* this program; if not, write to the Free Software Foundation, Inc., 59 Temple
* Place, Suite 330, Boston, MA 02111-1307 USA
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Fromonto
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(fromonto, [from_file/2, onto_file/2, from_stream/2, onto_stream/2]). 


:- module_transparent from_file/2.
:- module_transparent onto_file/2.
:- module_transparent from_stream/2.
:- module_transparent onto_stream/2.


:- op(800, yfx, user:from_file).
:- op(800, yfx, user:onto_file).
:- op(800, yfx, user:from_stream).
:- op(800, yfx, user:onto_stream).

from_file(Goal, File) :-
	( sub_atom(File, _, 3, 0, '.gz') ->
	  %%
	  %% in SWI 10.2, gzopen seemed bugged for larger files 
	  %% format(atom(UnzipCall), 'gunzip < \'~w\'', File),
	  %% open(pipe(UnzipCall), read, Stream)
	  %%
	  gzopen(File, read, Stream)
	  %%
	; open(File, read, Stream)
	),
        current_input(In),
	catch( ( set_input(Stream),
                 (call(Goal) -> Result = true ; Result = false)
               ),
	       E,
	       ( set_input(In),
                 close(Stream),
		 throw(E)
	       )
             ),
        set_input(In),
        close(Stream),
        Result = true.

onto_file(Goal, File) :-
	( sub_atom(File, _, 3, 0, '.gz') ->
	  gzopen(File, write, Stream)
	; open(File, write, Stream)
	),
        current_output(Out),
        catch( ( set_output(Stream),
                 (call(Goal) -> Result = true ; Result = false)
	       ),
	       E,
	       ( set_output(Out),
                 close(Stream),
		 throw(E)
	       )
	     ),
        set_output(Out),
        close(Stream),
        Result = true.


from_stream(Goal, Stream) :-
        current_input(In),
	catch( ( set_input(Stream),
                 (call(Goal) -> Result = true ; Result = false)
               ),
	       E,
	       ( set_input(In),
		 throw(E)
	       )
             ),
        set_input(In),
        Result = true.

onto_stream(Goal, Stream) :-
        current_output(Out),
        catch( ( set_output(Stream),
                 (call(Goal) -> Result = true ; Result = false)
	       ),
	       E,
	       ( set_output(Out),
		 throw(E)
	       )
	     ),
        set_output(Out),
        Result = true.


