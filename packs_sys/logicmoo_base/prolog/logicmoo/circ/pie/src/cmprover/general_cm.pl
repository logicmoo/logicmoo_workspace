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

:- module(general_cm,
          [ date_atom/1,
 	    current_cpu_time/1,
 	    current_times/3,
 	    write_number/2,
 	    write_number/3,
	    write_natnum/2,
 	    write_natnum/3
	  ]). 

:- (prolog_flag(dialect, yap) -> use_module(swilib(yap_support)) ; true ).

date_atom(Date) :-
	get_time(T),
	convert_time(T, S),
	format(atom(Date), '~w', S).
	
current_cpu_time(Milliseconds) :-
	prolog_flag(dialect, yap),
	!,
	statistics(cputime, [Milliseconds,_]).
current_cpu_time(Milliseconds) :-
	statistics(cputime, Seconds),
	Milliseconds is Seconds * 1000.

current_times(Milliseconds, SystemSeconds, RealSeconds) :-
	prolog_flag(dialect, yap),
	!,
	statistics(cputime, [Milliseconds,_]),
	SystemSeconds = 0,
	RealSeconds = 0.
current_times(CpuSeconds, SystemSeconds, RealSeconds) :-
	statistics(cputime, CpuSeconds),
	SystemSeconds = 0,
	RealSeconds = 0.

write_number(Stream, Number, HSpaceUsed) :-
	( Number =:= 0 ->
	  Fill is HSpaceUsed - 1
	; Fill is HSpaceUsed - (max(floor(log10(Number)),0) + 1)
	),
	n_spaces(Stream, Fill),
	format(Stream, '~3f', [Number]).

write_number(Number, HSpaceUsed) :-
	current_output(Stream),
	write_number(Stream, Number, HSpaceUsed).

write_natnum(Stream, Natnum, HSpaceUsed) :-
	( Natnum =:= 0 ->
	  Fill is HSpaceUsed - 1
	; Fill is HSpaceUsed - (max(floor(log10(Natnum)),0) + 1)
	),
	n_spaces(Stream, Fill),
	format(Stream, '~d', [Natnum]).

write_natnum(Natnum, HSpaceUsed) :-
	current_output(Stream),
	write_natnum(Stream, Natnum, HSpaceUsed).

n_spaces(Stream, N) :-
	N > 0,
	!,
	write(Stream, ' '),
	N1 is N - 1,
	n_spaces(Stream, N1).
n_spaces(_, _).

