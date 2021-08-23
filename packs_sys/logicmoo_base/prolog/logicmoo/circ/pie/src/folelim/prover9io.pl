%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2019 Christoph Wernhard
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

:- module(prover9io, [prover9_read_from_chars/2,
		      prover9_fol_read_from_chars/2]).

:- use_module(swilib(err)).

:- op(300, yf, '\'').
:- op(350, fy, '-').
:- op(500, xfx, 'v').
:- op(500, xfx, '^').
:- op(500, xfx, '\\').
:- op(500, xfx, '/').
:- op(500, xfx, '@').
:- op(500, xfx, '*').
:- op(500, xfx, '+').
:- op(700, xfx, '>=').	    
:- op(700, xfx, '>').
:- op(700, xfx, '<=').
:- op(700, xfx, '<').
:- op(700, xfx, '==').  
:- op(700, xfx, '!=').
:- op(700, xfx, '=').
% Quantifiers (a special case) have precedence 750.	    
:- op(780, xfy, '&').
%%%%
%%%% We use for the remaining priorities >= 1001 because of the
%%%% restriction by SWI prolog for bar (|). The system_mode or
%%%% access_level seems of no help there in version 8.0
%%%%
:- op(1002,xfy,'|').
:- op(1010, xfy, '#').
:- op(1020, xfx, '<->').
:- op(1020, xfx, '->').
:- op(1020, xfx, '<-').
%%%% 
%%%% Auxilary for reading-in quantifiers (after string-level replacement):
%%%% 
:- op(750, xfy, ':').

prover9_read_from_chars(Codes, X) :-
	atom_chars(Codes, Codes1),
	fq(Codes1, Codes2),
	read_term_from_chars(Codes2, X, [module(prover9io)]).

prover9_fol_read_from_chars(Codes, X) :-
	atom_chars(Codes, Codes1),
	fq(Codes1, Codes2),
	( cq_0(Codes2, Codes3) ->
	  true
	; err('Failed to convert Prover9 quantifiers: ~q', [Codes])
	),
	read_term_from_chars(Codes3, X, [module(prover9io)]).

cq_0([a,l,l,C|Cs], [a,l,l,':',C|Cs1]) :-
	sepchar(C),
	!,
	cq_1(Cs, Cs1).
cq_0([e,x,i,s,t,s,C|Cs], [e,x,i,s,t,s,':',C|Cs1]) :-
	sepchar(C),
	!,
	cq_1(Cs, Cs1).
cq_0([C|Cs], [C|Cs1]) :-
	cq_0(Cs, Cs1).
cq_0([], []).

cq_1([C|Cs], [C|Cs1]) :- sepchar(C), !,	cq_1(Cs, Cs1).
cq_1([C|Cs], [C|Cs1]) :- cq_2(Cs, Cs1).

cq_2([C|Cs], [':',C|Cs1]) :- sepchar(C), !, cq_0(Cs, Cs1).
cq_2([C|Cs], [C|Cs1]) :- cq_2(Cs, Cs1).

sepchar(C) :- \+ char_type(C, csym).

fq(['$','T'|Cs], ['\'','$','T','\''|Cs1]) :- !, fq(Cs, Cs1).
fq(['$','F'|Cs], ['\'','$','F','\''|Cs1]) :- !, fq(Cs, Cs1).
fq(['$','p'|Cs], ['\'','$','p'|Cs1]) :- !, fq_num(Cs, Cs1).
fq(['!','='|Cs], ['\'','!','=','\''|Cs1]) :- !, fq(Cs, Cs1).
fq(['\''|Cs], ['\\','\''|Cs1]) :- !, fq(Cs, Cs1).
fq(['\\'|Cs], ['\\','\\'|Cs1]) :- !, fq(Cs, Cs1).
fq(['"'|Cs], ['\''|Cs1]) :- !, fq(Cs, Cs1).
fq([C|Cs], [C|Cs1]) :- fq(Cs, Cs1).
fq([], []).

fq_num([C|Cs], [C|Cs1]) :- char_type(C, digit), !, fq_num(Cs, Cs1).
fq_num([C|Cs], ['\'',C|Cs1]) :- fq(Cs, Cs1).
fq_num([], ['\'']).