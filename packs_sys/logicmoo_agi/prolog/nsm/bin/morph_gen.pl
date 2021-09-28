:- module(morph_gen,[
		    scan_phonemes/3
		   ]).

:- use_module(checkers).


%%	scan_phonemes(+Lang,+Lex,-Surf) is det
%
%	This is the backend routine for generation. It receives
%	a "sentence" in the form of a list of morphemes, and produces
%	its phonemic (or, better, graphemic) representation.
%	
scan_phonemes(Lang,Lex,Surf) :-
	append("#",Lex,Lex1),
	append(Lex1,"#",Lex2),
	scan_phonemes(Lang,[],[],Lex2,[],Surf).

scan_phonemes(_Lang,[m(_M1,Mo1),m("#"," ")],[],[],Surf,Surf1) :-
	!,
	append(Surf,Mo1,Surf1).

scan_phonemes(Lang,[m(M1,Mo1),m(M2,Mo2),m("#"," ")],[],[],Surf,Surf1) :-
	!,
	append(Surf,Mo1,Surf2),
	check_allo(Lang,[m(M1,Mo1),m(M2,Mo2),m("#"," ")],_,_),
	append(Surf2,Mo2,Surf1).

scan_phonemes(Lang,Stack,[],[35|Lex],Surf,Surf1) :-
	!,
	append(Stack,[m("#"," ")],NewStack),
%	append(Surf," ",NewSurf),
	scan_phonemes(Lang,NewStack,[],Lex,Surf,Surf1).


scan_phonemes(Lang,[m(M1,Mo1),m([],_),m(M3,Mo3)],M3,[35|Lex],Surf,Surf1) :-
	!,
	scan_phonemes(Lang,[m(M1,Mo1),m(M3,Mo3)],[],Lex,Surf,Surf1).
scan_phonemes(Lang,Stack,Morpheme,[35|Lex],Surf,Surf1) :-
	!,
	append(Stack,[m(Morpheme,_Mo3)],NewStack),
	check_allo(Lang,NewStack,NewStack1,Mo1),
	append(Surf,Mo1,NewSurf),
	append(NewStack1,[m("#"," ")],NewStack2),
	check_allo(Lang,NewStack2,NewStack3,Mo2),
	append(NewSurf,Mo2,NewSurf1),
	scan_phonemes(Lang,NewStack3,[],Lex,NewSurf1,Surf1).

scan_phonemes(Lang,Stack,M3,[45|Lex],Surf,Surf1) :-
	!,
	append(Stack,[m(M3,_Morph3)],NewStack),
	check_allo(Lang,NewStack,NewStack1,Mo1),
	append(Surf,Mo1,NewSurf),
	scan_phonemes(Lang,NewStack1,[],Lex,NewSurf,Surf1).

scan_phonemes(Lang,Stack,Morph,[C|Lex],Surf,Surf1) :-
	append(Morph,[C],NewMorph),
	scan_phonemes(Lang,Stack,NewMorph,Lex,Surf,Surf1).


check_allo(_Lang,[A],[A],[]).
check_allo(_Lang,[A,B],[A,B],[]).
check_allo(Lang,[m(M1,Morph1),m(M2,Morph2),m(M3,Morph3)],[m(M2,Morph2),m(NewM3,Morph3)],Morph1) :-
	grammar:allo(Lang,Morph2,M2,Residue,PredSurf,SuccSurf,PredLex,SuccLex,Conditions),
	make_succ(SuccSurf,SuccLex,Succ),
	append(M3,"#",M3a),
	append(_,PredLex,M1),
	append(Succ,_,M3a),
	append(_,PredSurf,Morph1),
	append(Residue,NewM3,M3),
	check_cond_list_reverse(Lang,Conditions),
	!.

check_allo(_Lang,[m(_M1,Mo1),m(M2,M2),M3],[m(M2,M2),M3],Mo1).

make_succ(SuccSurf,[],SuccSurf) :- !.
make_succ(_,SuccLex,SuccLex).
