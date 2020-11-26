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


:- module(atom_map, [
		richatom_pooratom/2  % +RichAtom, -PoorAtom
		                     % -RichAtom, +PoorAtom
	]).

/** <module> Atom map

Some atoms have a predefined meaning in Smodels (the tool that is used for the
calculation of the stable models). For that reason we need to map these atoms
to other atoms that do not have a predefined meaning in Sodels.

@author Tobias Kuhn
@version 2007-02-09

@see stable_interpreter.pl
*/


%% richatom_pooratom(+RichAtom, -PoorAtom).
%% richatom_pooratom(-RichAtom, +PoorAtom).
%
% Maps some atoms (that have a predefined meaning in Smodels) to atoms starting
% with 'xxx_', and backwards. The predicate fails for all the other atoms.

richatom_pooratom([], xxx_emptylist).
richatom_pooratom(eq, xxx_eq).
richatom_pooratom(external, xxx_external).
richatom_pooratom(const, xxx_const).
richatom_pooratom(domain, xxx_domain).
richatom_pooratom(function, xxx_function).
richatom_pooratom(hide, xxx_hide).
richatom_pooratom(option, xxx_option).
richatom_pooratom(show, xxx_show).
richatom_pooratom(weight, xxx_weight).
richatom_pooratom(compute, xxx_compute).
richatom_pooratom(maximize, xxx_maximize).
