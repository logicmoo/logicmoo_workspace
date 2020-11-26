/* @(#)readin.pl	24.1 2/23/88 */

/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
/* Read a sentence */

 :- mode initread(-).
 :- mode readrest(+,-).
 :- mode word(-,?,?).
 :- mode words(-,?,?).
 :- mode alphanum(+,-).
 :- mode alphanums(-,?,?).
 :- mode digits(-,?,?).
 :- mode digit(+).
 :- mode lc(+,-).

 :- public read_in/1.

/* Read sentence */

read_in(P):-initread(L),words(P,L,[]),!,to_nl.

initread([K1,K2|U]):-get(K1),get0(K2),readrest(K2,U).

readrest(46,[]):-!.
readrest(63,[]):-!.
readrest(33,[]):-!.
readrest(K,[K1|U]):-K=<32,!,get(K1),readrest(K1,U).
readrest(K1,[K2|U]):-get0(K2),readrest(K2,U).

words([V|U]) --> word(V),!,blanks,words(U).
words([]) --> [].

word(U1) --> [K],{lc(K,K1)},!,alphanums(U2),{name(U1,[K1|U2])}.
word(nb(N)) --> [K],{digit(K)},!,digits(U),{name(N,[K|U])}.
word(V) --> [K],{name(V,[K])}.

alphanums([K1|U]) --> [K],{alphanum(K,K1)},!,alphanums(U).
alphanums([]) --> [].

alphanum(95,95) :- !.
alphanum(K,K1):-lc(K,K1).
alphanum(K,K):-digit(K).

digits([K|U]) --> [K],{digit(K)},!,digits(U).
digits([]) --> [].

blanks--> [K],{K=<32},!,blanks.
blanks --> [].

digit(K):-K>47,K<58.

lc(K,K1):-K>64,K<91,!,K1 is K\/8'40.
lc(K,K):-K>96,K<123.

to_nl :-
   repeat,
   get0(10), !.

