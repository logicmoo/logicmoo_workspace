/*
    This file is part of NSM-DALIA, an extensible parser and generator
    for NSM grammars.
       
    Copyright (C) 2009 Francesco Zamblera.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/


/* FORMATS */
o_format(e,A,A).
o_format(tagm,m(CLASS,Exp),m(CLASS,Exp)).
o_format(cg,m(CLASS,Exp),cat(CLASS,Exp)).
o_format(dg,m(CLASS,Exp),ct(CLASS,Exp)).


/* TEXT @i FORMATS */
text_id_format(0,_,[]).

text_id_format(1,[_,_,45,_,_,45,A],[A]).
text_id_format(1,[_,_,45,_,_,45,48,B],[B]).
text_id_format(1,[_,_,45,_,_,45,A,B],[A,B]).
text_id_format(1,[_,_,45,_,_,48,B,C],[B,C]).
text_id_format(1,[_,_,45,_,_,A,B,C],[A,B,C]).

text_id_format(2,[_,_,45,48,W,45,A],[W,44,A]).
text_id_format(2,[_,_,45,Q,W,45,A],[Q,W,44,A]).

text_id_format(2,[_,_,45,48,W,45,48,B],[W,44,B]).
text_id_format(2,[_,_,45,48,W,45,A,B],[W,44,A,B]).
text_id_format(2,[_,_,45,Q,W,45,48,B],[Q,W,44,B]).
text_id_format(2,[_,_,45,Q,W,45,A,B],[Q,W,44,A,B]).

text_id_format(2,[_,_,45,48,W,45,48,B,C],[W,44,B,C]).
text_id_format(2,[_,_,45,48,W,45,A,B,C],[W,44,A,B,C]).
text_id_format(2,[_,_,45,Q,W,45,48,B,C],[Q,W,44,B,C]).
text_id_format(2,[_,_,45,Q,W,45,A,B,C],[Q,W,44,A,B,C]).

text_id_format(_,[_,_,_,_],[]).

text_id_format(3,Id,Id).
