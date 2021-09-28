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


/* MORPHOPHONEMIC PARSER */

:- module(morphophon,[
		      find_morph/7,
		      match_pattern/4
		     ]).

:- use_module(messages).
:- use_module(checkers).

:- include('dynamic.pl').
:- include('operators.pl').

/** <module> Morpheme analysis


*/

%%	find_morph(+Lang,-Class,-Exponent,-W,-Rest,+OldParsed,-NewParsed)
%%      is det
%
%	Calls the word-parsing routine parse_morph/7, then stores
%	the found morpheme in the found_morph/3 database if
%	the _|dictionary making|_ modality is active.
%	
find_morph(Lang,Class,Exponent,W,Rest,OldParsed,NewParsed) :-
	parse_morph(Lang,morph(Class,Exponent,Shape),[],W,Rest,OldParsed,NewParsed),
        (   (\+global_par:makedict(no)) 
	; nsm_dict:assert_found_morph(Class,Shape,Exponent)
	).

%%	parse_morph(+Lang,-Morph,+Sofar,+Rest1,-Rest,+Old,-New) is det
%
%	This is the word-parsing routine. The second clause scans
%	the morphemic string ading a phoneme at a time, and 
%       it is called until a morpheme is found which matches
%       the substring scanned so far (first clause). Then,
%       the found morpheme is unified with the Morph argument.
%       
parse_morph(Lang,morph(Class,Exponent,Shape),SoFar,Rest1,Rest,parsed(Surf,Lex,PSL),parsed(NewSurf,NewLex,NSL)) :-
	morph_or_allo(Lang,SoFar,morph(Class,Exponent,Shape),Residue,Rest1,Surf,Lex,PSL,NSL),
	append(Surf,SoFar,NewSurf),
	append(Lex,[45|Shape],NewLex),
	check_rest(Rest1,Rest2),
	append(Residue,Rest2,Rest).
parse_morph(Lang,Morpheme,SoFar,[Phon|W],Rest,OldP,NewP) :-
	append(SoFar,[Phon],X),
	parse_morph(Lang,Morpheme,X,W,Rest,OldP,NewP).

check_rest("#",[]) :- !.
check_rest(R,R).

morph_or_allo(Lang,Morph,morph(Class,Exponent,Morpheme),Residue,Rest,ParsedSurf,ParsedLex,PreviousSuccLex,NextSuccLex) :-
	grammar:allo(Lang,Morph,Morpheme,Residue,PredSurf,SuccSurf,PredLex,NextSuccLex,Conditions),
	grammar:m(Lang,Class,Morpheme,Exponent),
	notify_try_allo(allo(Morpheme-Residue => Morph,PredSurf-SuccSurf,l:PredLex-NextSuccLex,
			  cond :Conditions),cont(prev:PreviousSuccLex,ParsedSurf-Rest,l:ParsedLex-[])),
	check_pred(PredSurf,ParsedSurf),
	check_pred(PredLex,ParsedLex),
	check_succ(SuccSurf,Rest),
%	check_succ(PreviousSuccLex,Morpheme),
	check_cond_list_aux(Lang,Conditions,PredSurf,PredLex).
morph_or_allo(Lang,Morph,morph(Class,Exponent,Morph),[],Rest,ParsedSurf,ParsedLex,PreviousSuccLex,[]) :-
	grammar:m(Lang,Class,Morph,Exponent),
	notify_try_morph(Morph),
%	check_succ(PreviousSuccLex,Morph),
	check_oblig_allo(Lang,Morph,Rest,p(ParsedSurf,ParsedLex,PreviousSuccLex)).
%	
%	
	
	
/*

check_oblig_allo(Lang,Morpheme,Rest,p(ParsedSurf,ParsedLex,PreviousSuccLex)) :-
	allo(Lang,Morph,Morpheme,Residue,PredSurf,SuccSurf,PredLex,_SuccLex,Conditions),	
	check_pred(PredSurf,ParsedSurf),
	check_pred(PredLex,ParsedLex),
	append(Residue,SuccSurf,S1),
	check_succ(S1,Rest),
	check_succ(PreviousSuccLex,Morph),	
	check_cond_list_aux(Lang,Conditions,PredSurf,PredLex),
	!, 
        notify_discard_morph(allo(Morpheme-Residue => Morph,PredSurf-SuccSurf,l:PredLex-_NextSuccLex, cond :Conditions),cont(prev:PreviousSuccLex,ParsedSurf-Rest,l:ParsedLex-[])),
	fail.
*/
check_oblig_allo(_Lang,_Morph,_Rest,_Parsed).


/* MORPHOPHONOLOGICAL PARSER */

/*
match_pattern(Lang,String,Pref,Suff) :-
	pattern(Lang,String,p(Pref,Suff),Cond),
	check_cond_list(Lang,Cond).
match_pattern(Lang,String,Pref,Suff) :-
	pattern(Lang,A+B,p(Pref,Suff),Cond),
	append(A,B,String),
	check_cond_list(Lang,Cond).
*/


match_pattern(Lang,String,Pref,Suff) :-
	grammar:pattern(Lang,A,p(Pref1,Suff1),Cond),
	match_shape(String,A),
	match_shape(Pref,Pref1),
	match_shape(Suff,Suff1),
	check_cond_list(Lang,Cond).


	
ends(A,B) :-
	append(_,B,A).
starts(A,B) :-
	append(B,_,A).
