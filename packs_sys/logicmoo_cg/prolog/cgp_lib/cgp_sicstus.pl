
/*

Somehow these still appeared in operations.pl file

subset([],_).
subset([E|Sub],Set):-member(E,Set),!,subset(Sub,Set).

intersection([],_,[]).
intersection([H|L1],L2,[H|L3]):-member(H,L2),!,
	intersection(L1,L2,L3).
 intersection([_|L1],L2,L3):-intersection(L1,L2,L3).

flatten([],[]):-!.
flatten([[]|T],Res):-!,flatten(T,Res).
flatten([[H1|T1]|T],Res):-!,flatten(T,Res1),!,
	append([H1|T1],Res1,Res).
flatten([H|T],[H|Res]):-!,flatten(T,Res).
*/
