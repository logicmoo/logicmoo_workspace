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

:- module(options,
	  [from_options/3,
	   from_options/2,
	   to_options/2,
	   default_options/3,
	   is_specified_option/2]).

:- use_module(swilib(err)).

is_specified_option(Key, Options) :-
	memberchk(Key=_, Options),
	!.
is_specified_option(Key, Options) :-
	memberchk(Key-_, Options).

default_options(Options, Defaults, Options1) :-
	do(Defaults, Options, Options1).

do([K=V|KVs], Os, Os1) :-
	( memberchk(K=_, Os) ->
	  Os2 = Os
	; Os2 = [K=V|Os]
	),
	do(KVs, Os2, Os1).
do([], Os, Os).
   
to_options(Key-Value, Options) :-
	!,
	to_options(Key=Value, Options).
to_options(Key=Value, Options) :-
	( memberchk(Key=SpecifiedValue, Options) ->
	  SpecifiedValue = Value
	; memberchk(Key-SpecifiedValue, Options) ->
	  SpecifiedValue = Value
	; true
	).

from_options(Key-Value, Options) :-
	!,
	from_options(Key=Value, Options).
from_options(Key=Value, Options) :-
	( memberchk(Key=SpecifiedValue, Options) ->
	  SpecifiedValue = Value
	; memberchk(Key-SpecifiedValue, Options) ->
	  SpecifiedValue = Value
        ; err('Missing key ~q in options ~q', [Key, Options])
	).

from_options(Key-Value, Options, Default) :-
	!,
	from_options(Key=Value, Options, Default).
from_options(Key=Value, Options, Default) :-
	( memberchk(Key=SpecifiedValue, Options) ->
	  SpecifiedValue = Value
	; memberchk(Key-SpecifiedValue, Options) ->
	  SpecifiedValue = Value
        ; Value = Default
	).


