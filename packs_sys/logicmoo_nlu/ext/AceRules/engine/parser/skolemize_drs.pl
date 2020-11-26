% This file is part of AceRules.
% Copyright 2008-2012, Tobias Kuhn, http://www.tkuhn.ch
%
% AceRules is free software: you can redistribute it and/or modify it under the terms of the GNU
% Lesser General Public License as published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% AceRules is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
% the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
% General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License along with AceRules. If
% not, see http://www.gnu.org/licenses/.


:- module(skolemize_drs, [
			  skolemize_drs/1  % +DRS
			 ]).

:- use_module('../skolemizer/skolemizer').
:- use_module('../op_defs').

/** <module> DRS skolemizer

Skolemizes all the existentially quantified variables of the DRS. The
variables are replaced by atoms of the form v(1), v(2), v(3), etc.

@author Tobias Kuhn
@version 2007-02-07
*/


%% skolemize_drs(+DRS)
%
% Skolemizes the existentially quantified variables of the DRS.

skolemize_drs(drs(_, Conds)) :-
    skolemize_conds(Conds).


skolemize_conds([]).

skolemize_conds([Fact-_|Rest]) :-
	!,
	skolemize(v, Fact),
	skolemize_conds(Rest).

skolemize_conds([_|Rest]) :-
	skolemize_conds(Rest).
