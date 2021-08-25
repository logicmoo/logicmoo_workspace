/*
*  This library manages formules and their combination through AND - OR operations
*
*  Written by: Fabiano Aguiari
*
*/ 

:- use_module(library(lists)).

% and between two formulae
and_f([],[],[]):-!.
and_f([],F2,F2):-!.
and_f(F1,[],F1):-!.
and_f([and(L1)],[and(L2)],[and(L)]):-!,
  flatten([L1,L2],L0),
  remove_duplicates(L0,L).
and_f([or(L1)],[and(L2)],[and(L)]):-!,
  flatten([L2,or(L1)],L0),
  remove_duplicates(L0,L).
and_f([and(L1)],[or(L2)],[and(L)]):-!,
  flatten([L1,or(L2)],L0),
  remove_duplicates(L0,L).  
and_f([or(L1)],[or(L2)],[and(L)]):-!,
  flatten([or(L1),or(L2)],L0),
  remove_duplicates(L0,L).
and_f([El],[and(L2)],[and(L)]):-!,
  flatten([El,L2],L0),
  remove_duplicates(L0,L).  
and_f([El],[or(L2)],[and(L)]):-!,
  flatten([El,or(L2)],L0),
  remove_duplicates(L0,L).
and_f([and(L1)],[El],[and(L)]):-!,
  flatten([El,L1],L0),
  remove_duplicates(L0,L).  
and_f([or(L1)],[El],[and(L)]):-!,
  flatten([El,or(L1)],L0),
  remove_duplicates(L0,L).  
and_f([El1],[El2],[and(L)]):-
  flatten([El1,El2],L0),
  remove_duplicates(L0,L).
  
  
/*or_f([],[],[]):-!.
or_f([],F2,F2):-!.
or_f(F1,[],F1):-!.
or_f([or(L1)],[or(L2)],[or(L)]):-!,
  flatten([L1,L2],L0),
  remove_duplicates(L0,L).
or_f([and(L1)],[or(L2)],[or(L)]):-!,
  flatten([L2,and(L1)],L0),
  remove_duplicates(L0,L).
or_f([or(L1)],[and(L2)],[or(L)]):-!,
  flatten([L1,and(L2)],L0),
  remove_duplicates(L0,L).  
or_f([and(L1)],[and(L2)],[or(L)]):-!,
  flatten([and(L1),and(L2)],L0),
  remove_duplicates(L0,L).
or_f([El],[or(L2)],[or(L)]):-!,
  flatten([El,L2],L0),
  remove_duplicates(L0,L).  
or_f([El],[and(L2)],[or(L)]):-!,
  flatten([El,and(L2)],L0),
  remove_duplicates(L0,L).
or_f([or(L1)],[El],[or(L)]):-!,
  flatten([El,L1],L0),
  remove_duplicates(L0,L).  
or_f([and(L1)],[El],[or(L)]):-!,
  flatten([El,and(L1)],L0),
  remove_duplicates(L0,L).  
or_f([El1],[El2],[or(L)]):-
  flatten([El1,El2],L0),
  remove_duplicates(L0,L). 
*/

/*
* Cleans a complex formula A by removing sub-formulae which are permutation or subset 
* of other formulae contained in A
*/
val_min(F,LO):-
  formule_gen(F,LF),
  val_min2(LF,LO).
  
val_min2(L,LO):-
  val_min0(L,L,LSov),
  val_min1(L,L,LPer),
  remove_duplicates(LPer,LPer1),
  differenceFML(LSov,LPer1,LD),
  differenceFML(L,LD,LO).

val_min0([],_,[]):-!.
val_min0([X|T],L,[X|L2]):-
  member(Y,L),
  Y \== X,
  subset(Y,X),!,
  val_min0(T,L,L2).
val_min0([_|T],L,L2):-
  val_min0(T,L,L2).  
  
val_min1([],_,[]):-!.
val_min1([X|T],L,[Y|L2]):-
  member(Y,L),
  Y \== X,
  permutation(X,Y),!,
  val_min1(T,L,L2).
val_min1([_|T],L,L2):-
  val_min1(T,L,L2).

subset([],_).
subset([H|T],L):-
  member(H,L),
  subset(T,L).  
  
% difference between formule
differenceFML([],_,[]).
differenceFML([T|Tail],L2,[T|Other]):- \+ member(T,L2),!,differenceFML(Tail,L2,Other).
differenceFML([_|C],L2,Diff):- differenceFML(C,L2,Diff).

% intersection between formule
intersectionFML([],_,[]).
intersectionFML([T|C],L2,[T|Resto]):- member(T,L2),!,intersectionFML(C,L2,Resto).
intersectionFML([_|C],L2,LInt):- intersectionFML(C,L2,LInt).

%
is_or(or(_)).
is_and(and(_)).


find_and_in_formula(F,And):- findall( X, (member(X,F), \+ is_or(X)), And).
find_or_in_formula(F,Or):- member(or(Or),F),!.
  


% develops a formula
formule_gen([],[]):- !.
formule_gen(FC,F):-findall(XRD, (formula_gen(FC,X), remove_duplicates(X,XRD)), FCD), remove_duplicates(FCD,F).

formula_gen([],[]):-!.
formula_gen([X],[X]):- \+ is_and(X), \+ is_or(X),!.
formula_gen([and(FC)],F):-
  find_or_in_formula(FC,Or),!,
  find_and_in_formula(FC,And),
  member(X,Or),
  formula_gen([X],XF),
  append(And,XF,F).
formula_gen([and(FC)],And):- 
  find_and_in_formula(FC,And),!.
formula_gen([or(FC)],F):- 
  member(X,FC),
  formula_gen([X],XF),
  append([],XF,F).

% Decomposes a fomula
formule_decomp([],[],[],[],[],[],[]):- !.
formule_decomp([],[or(F2)],[],[],[],[],[]):- !.
formule_decomp([and(F1)],[],AndF1,[],[],AndF1,[]):-
  find_and_in_formula(F1,AndF1),!.
formule_decomp([],[and(F2)],[],AndF2,[],[],AndF2):-
  find_and_in_formula(F2,AndF2),!.
formule_decomp([and(F1)],[and(F1)],AndF1,AndF1,AndF1,[],[]):- 
  find_and_in_formula(F1,AndF1),!.   
formule_decomp([and(F1)],[or(F2)],AndF1,[],[],AndF1,[]):-
  find_and_in_formula(F1,AndF1),!.  
formule_decomp([and(F1)],[and(F2)],AndF1,AndF2,AndUguali,AndDiversiF1,AndDiversiF2):-
  find_and_in_formula(F1,AndF1),
  find_and_in_formula(F2,AndF2),
  intersectionFML(AndF1,AndF2,AndUguali),
  differenceFML(AndF1,AndUguali,AndDiversiF1),
  differenceFML(AndF2,AndUguali,AndDiversiF2),!. 
formule_decomp([El1],[or(F2)],[El1],[],[],[El1],[]):- !.
formule_decomp([El1],[and(F2)],[El1],AndF2,AndUguali,AndDiversiF1,AndDiversiF2):-
  find_and_in_formula(F2,AndF2),
  intersectionFML([El1],AndF2,AndUguali),
  differenceFML([El1],AndUguali,AndDiversiF1),
  differenceFML(AndF2,AndUguali,AndDiversiF2),!.   
formule_decomp([and(F1)],[El2],AndF1,[El2],AndUguali,AndDiversiF1,AndDiversiF2):-
  find_and_in_formula(F1,AndF1),
  intersectionFML(AndF1,[El2],AndUguali),
  differenceFML(AndF1,AndUguali,AndDiversiF1),
  differenceFML([El2],AndUguali,AndDiversiF2),!.   
formule_decomp([],[El2],[],[El2],[],[],[El2]):- !.
formule_decomp([El1],[],[El1],[],[],[El1],[]):- !.
formule_decomp([El],[El],[El],[El],[El],[],[]):- !.
formule_decomp([El1],[El2],[El1],[El2],[],[El1],[El2]):- !.
  
 
%computes a compact formula strarting from 2 formulae 
or_f([and(FC1)],[FC2],OrF):- !,
  findall( or(X), (member(or(X),FC1)), Or), length(Or,Length), 
  ( Length @> 1 ->
     OrF = or([and(FC1),FC2]);
     formule_gen([and(FC1)],F1), or_scan(F1,[FC2],OrF)).

or_f(FC1,FC2,OrF):- formule_gen(FC1,F1), or_scan(F1,FC2,OrF).

or_scan([],F2,F2):-!.
or_scan([T|C],F2,OrF):- ( T = [_] -> NT = T ; NT = [and(T)] ), or_between_formule(NT,F2,OrF1),or_scan(C,OrF1,OrF).
  
%computes a compact formula strarting from 2 formulae
or_between_formule(F1,[],F1):- !.
or_between_formule([],F2,F2):- !.
or_between_formule(F,F,F):- !.
/*
or_between_formule(F1,F2,F2):-
  nl,write('Zeresimo caso'),nl,
  formule_gen(F1,F1F),
  formule_gen(F2,F2F),
  findall(X1, (member(X,F1F),permutation(X,X1),member(X1,F2F)), Ris), permutation(Ris,F2F),!.
*/
or_between_formule(F1,[or(F2)],OrF):-
  formule_decomp(F1,[or(F2)],AndF1,AndF2,AndUguali,AndDiversiF1,AndDiversiF2),
  or_between_formule1(F1,[or(F2)],AndF1,AndF2,AndUguali,AndDiversiF1,AndDiversiF2,F2,OrF),!. 
or_between_formule(F1,[and(F2)],OrF):-
  formule_decomp(F1,[and(F2)],AndF1,AndF2,AndUguali,AndDiversiF1,AndDiversiF2),
  ( find_or_in_formula(F2,OrF2) -> true ; OrF2 = [] ),
  or_between_formule1(F1,[and(F2)],AndF1,AndF2,AndUguali,AndDiversiF1,AndDiversiF2,OrF2,OrF),!.
or_between_formule(F1,[El2],OrF):-
  formule_decomp(F1,[El2],AndF1,AndF2,AndUguali,AndDiversiF1,AndDiversiF2),
  or_between_formule1(F1,[El2],AndF1,AndF2,AndUguali,AndDiversiF1,AndDiversiF2,[],OrF).  
  
or_between_formule1(F1,F2,AndF1,AndF2,AndUguali,[],AndDiversiF2,OrF2,OrF):-
  %nl,write('First case'),nl,
  !,
  ( AndUguali = [_] -> append([],AndUguali,OrF) ; OrF = [and(AndUguali)]).
or_between_formule1(F1,F2,AndF1,AndF2,AndUguali,AndDiversiF1,[],[],OrF):-
  %nl,write('2nd case'),nl,
  !,
  ( AndUguali = [_] -> append([],AndUguali,OrF) ; OrF = [and(AndUguali)] ).
or_between_formule1(F1,F2,AndF1,AndF2,[],AndDiversiF1,AndDiversiF2,[],OrF):-
  %nl,write('3rd case'),nl,
  AndDiversiF1 \== [], AndDiversiF2 \== [],!,
  ( AndDiversiF1 = [_] -> append([],AndDiversiF1,NAndF1) ; NAndF1 = and(AndDiversiF1) ),
  ( AndDiversiF2 = [_] -> append([],AndDiversiF2,NAndF2) ; NAndF2 = and(AndDiversiF2) ),
  flatten([NAndF1, NAndF2], Or),
  OrF = [or(Or)].
or_between_formule1(F1,F2,AndF1,AndF2,AndUguali,AndDiversiF1,AndDiversiF2,[],OrF):-
  %nl,write('4th case'),nl,
  AndDiversiF1 \== [], AndDiversiF2 \== [], AndUguali \== [],!,
  ( AndDiversiF1 = [_] -> append([],AndDiversiF1,NAndF1) ; NAndF1 = and(AndDiversiF1) ),
  ( AndDiversiF2 = [_] -> append([],AndDiversiF2,NAndF2) ; NAndF2 = and(AndDiversiF2) ),
  flatten([NAndF1, NAndF2], Or),
  flatten([AndUguali, or(Or)], And),
  OrF = [and(And)].
or_between_formule1(F1,F2,AndF1,AndF2,[],AndDiversiF1,[],OrF2,OrF):-
  %nl,write('5th case'),nl,
  AndDiversiF1 \== [], OrF2 \== [],!,
  find_compatible_or(AndDiversiF1,OrF2,OrF2C,OrF2NC),
  ( OrF2C \== [] -> (or_f(OrF2C,F1,OrFC), flatten([OrFC, OrF2NC], NOr) ) ; append(F1, OrF2,NOr) ),
  OrF = [or(NOr)].  
or_between_formule1(F1,F2,AndF1,AndF2,AndUguali,AndDiversiF1,[],OrF2,OrF):-
  %nl,write('6th case'),nl,
  AndDiversiF1 \== [], AndUguali \== [],OrF2 \== [],!,
  ( AndDiversiF1 = [_] -> append([],AndDiversiF1,AndDiversiF1N) ; AndDiversiF1N = [and(AndDiversiF1)] ),
  find_compatible_or(AndDiversiF1,OrF2,OrF2C,OrF2NC),
  ( OrF2C \== [] -> (or_f(OrF2C,AndDiversiF1N,OrFC),flatten([OrFC, OrF2NC], NOr) ) ; append(AndDiversiF1N, OrF2,NOr) ),
  flatten([AndUguali, or(NOr)], And),
  OrF = [and(And)].
or_between_formule1(F1,F2,AndF1,AndF2,[],AndDiversiF1,AndDiversiF2,OrF2,OrF):-
  %nl,write('7th case'),nl,
  AndDiversiF1 \== [], AndDiversiF2 \== [], OrF2 \== [],!,
  ( AndDiversiF1 = [_] -> append([],AndDiversiF1,AndDiversiF1N) ; AndDiversiF1N = and(AndDiversiF1) ),
  flatten([AndDiversiF2, or(OrF2)], AndF2N),
  NOrF2 = and(AndF2N),
  flatten([AndDiversiF1N, NOrF2], And),
  OrF = [or(And)].
or_between_formule1(F1,F2,AndF1,AndF2,AndUguali,AndDiversiF1,AndDiversiF2,OrF2,OrF):-
  %nl,write('8th case'),nl,
  AndDiversiF1 \== [], AndDiversiF2 \== [], AndUguali \== [], OrF2 \== [],!,
  ( AndDiversiF1 = [_] -> append([],AndDiversiF1,AndDiversiF1N) ; AndDiversiF1N = and(AndDiversiF1) ),
  flatten([AndDiversiF2, or(OrF2)], AndF2N),
  NOrF2 = and(AndF2N),
  flatten([AndDiversiF1N, NOrF2], AndDiversi),
  flatten([AndUguali, or(AndDiversi)], And),
  OrF = [and(And)].

%optimization for 5th and 6th cases
find_compatible_or(F1,OrF2,OrF2C,OrF2NC):-  
  findall(  Y, ( member(Y,OrF2), ( Y = and(YN) -> find_and_in_formula(YN,AndY) ; AndY = [Y] ), intersectionFML(F1,AndY,I), I \= [],!), OrF2C),
  differenceFML(OrF2,OrF2C,OrF2NC).
