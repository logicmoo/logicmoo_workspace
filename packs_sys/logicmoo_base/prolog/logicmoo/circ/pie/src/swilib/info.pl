%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2014, 2015 Christoph Wernhard
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

:- module(info, [info/2,
		 info/3,
		 info_progress/2,
		 info_progress/3,
		 info_progress_start/0,
		 info_progress_done/0,
		 set_info_verbosity/1,
		 get_info_verbosity/1,
		 set_info_progress/1
% 		 warn/1,
% 		 warn/2,
% 		 set_warnings/1
		]).

:- use_module(swilib(err)).
:- use_module(swilib(sysdep)).

:- flag(verbosity, _, 50).
:- flag(warnings, _, 1).
:- flag(progress, _, 1).

set_info_verbosity(N) :-
	flag(verbosity, _, N).
set_info_progress(N) :-
	flag(progress, _, N).

set_warnings(N) :-
	flag(warnings, _, N).

get_info_verbosity(N) :-
	flag(verbosity, N, N).

info(V, Format) :-
	info(V, Format , []).
info(V, Format, Args) :-
	flag(verbosity, V1, V1),
	( V1 >= V ->
	  ( stream_property(user_error, position(Pos)),
	    stream_position_data(line_position, Pos, N),
	    N =\= 0 ->
	    nl(user_error)
	  ; true
	  ),
	  format(user_error, '% ', []),
	  \+ \+ ( numbervars(Args, 0, _),
		  format(user_error, Format, Args) ),
	  nl(user_error),
	  flush_output(user_error)
	; true
	).

warn(Format) :-
	warn(Format , []).
warn(Format, Args) :-
	flag(warnings, W, W),
	W > 0,
	format(atom(Format1), 'WARNING: ~w', [Format]),
	info(0, Format1, Args).

:- flag(progress_call_pos, _, 0).
:- flag(progress_count, _, 0).

record_progress_call :-
	character_count(user_error, Pos),
	flag(progress_call_pos, _, Pos).

position_is_immediately_after_progress_call :-
	character_count(user_error, Pos),
	flag(progress_call_pos, Pos, Pos).

info_progress_done :-
	flag(progress, 1, 1),
	!,
	flag(progress_count, _, 0),
	( position_is_immediately_after_progress_call ->
	  nl(user_error)
	; true
	).
info_progress_done.

info_progress(V, MOD, N) :-
	flag(progress, 1, 1),
	!,
	flag(verbosity, V1, V1),
	( V1 >= V, N mod MOD =:= 0 ->
	  ( position_is_immediately_after_progress_call ->
	    write(user_error, '\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b')
	  ; true
	  ),
	  format(user_error, '% Progress: ~|~t~D~12+', [N]),
	  flush_output(user_error),
	  record_progress_call
	; true
	).
info_progress(_, _, _).

info_progress_start :-
	flag(progress, 1, 1),
	!,
	flag(progress_count, _, 0).
info_progress_start.

info_progress(V, MOD) :-
	flag(progress, 1, 1),
	!,
	flag_inc(progress_count, N),
	info_progress(V, MOD, N).
info_progress(_, _).
