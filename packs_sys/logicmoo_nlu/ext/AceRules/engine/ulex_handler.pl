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


:- module(ulex_handler, [
		init_ulex/0
	]).


:- use_module(ape('lexicon/ulex')).


init_ulex :-
    discard_ulex,
    % these nouns occur in the AceRules examples:
    add_lexicon_entry(noun_sg(quaker, quaker, human)),
    add_lexicon_entry(noun_sg(platypus, platypus, neutr)),
    add_lexicon_entry(noun_sg(monotreme, monotreme, neutr)),
    add_lexicon_entry(noun_sg(cephalopod, cephalopod, neutr)),
    add_lexicon_entry(noun_sg(mollusk, mollusk, neutr)),
    add_lexicon_entry(noun_sg(nautilus, nautilus, neutr)),
    add_lexicon_entry(noun_sg(shellbearer, shellbearer, neutr)),
    add_lexicon_entry(noun_sg(patricide, patricides, human)).
