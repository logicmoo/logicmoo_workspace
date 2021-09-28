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


:- include('operators.pl').
:- include('dynamic.pl').

/*
Date le regole:
ph : [ X+"t" => X+C, [] - [C],   cond : [C << cons]].
ph(Lang,Morph,Morpheme,Residue,PredSurf,SuccSurf,PredLex,SuccLex,Conditions).

Produce:
allo(Lang,Morph,Morpheme,Residue,PredSurf,SuccSurf,PredLex,SuccLex,Conditions).

*/

build_allo(Lang) :-
	findall(a(Shape),
		m(Lang,_Class,Shape,_Exp),
		Morphemes),
	build_allo_list(Lang,Morphemes),
	retractall(ph(Lang,_,_,_,_,_,_,_,_,_)),
	retractall(ph_to_expand(Lang,_)).


ph_expand_rules([],[]).
ph_expand_rules([R|Rules],[R1|Rules1]) :-
	ph_expand(R,R1),
	ph_expand_rules(Rules,Rules1).






build_allo_list(_Lang,[]).
build_allo_list(Lang,[Morpheme|List]) :-
%	findall(Rule,
%		ph_to_expand(Lang,Rule),
%	       Rules1),
%	ph_expand_rules(Rules1,Rules2),
	findall(ph(A,B,C,D,E,F,G,H,I),
		ph(Lang,A,B,C,D,E,F,G,H,I),
		Rules),
%	append(Rules2,Rules3,Rules),
	build_allo(Lang,Morpheme,Rules),
	build_allo_list(Lang,List).

build_allo(_Lang,_Morpheme,[]).
build_allo(Lang,Morpheme,[Rule|Rules]) :-
	match_rule(Lang,Morpheme,Rule),
	build_allo(Lang,Morpheme,Rules).

match_rule(Lang,a(Morpheme),ph(Shape,AlloShape,Residue,PredSurf,SuccSurf,PredLex,SuccLex,Cond,PreCond)) :-
	match_shape(Morpheme,Shape),
	match_shape(Allo,AlloShape),
	check_cond_list(Lang,PreCond),
	!,
	assertz(allo(Lang,Allo,Morpheme,Residue,PredSurf,SuccSurf,PredLex,SuccLex,Cond)).
match_rule(_Lang,_,_).




% Nuova match_shape in utils.pl

/*

match_shape(A,X+Y) :-
	!,
	append(X,Y,A).
match_shape(A,A).
*/



plist( ["angut","angutip","angummut","angutaa",
	"nuna", "nunap", "nunamut", "nunaa",
	"nuuk", "nuummut",
	"neqi", "neqimut", "neqip", "neqaa",
	"qaqqaq", "qaqqap", "qaqqamut", "qaqqaa",
	"erneq", "ernerup", "ernermut", "erneraa",
	"nerrivik", "nerriviup", "nerrivia", "nerrivimmut",
	"assik", "assiNut", "assiNa", "assimmut"]).

