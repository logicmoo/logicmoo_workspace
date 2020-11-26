%-----------------------------------Dictionnaire ------------------

nom_commun([customer,employee,year,card,train,monday,number,station,key,machine,button,bank,slot,end,bill,code]).

nom_propre([john,simplemat]). 

and_or([and,or]).

adverbe([correctly,manually,ponctually]). 

be([is]). 

not([not]). 
%not(L,L).

prep([to,into,of,with]). 

than([than]).

the([the,a]).

theire([theire]).

adj([invalid,valid,blue,fast]).


verbe_intransf([arrive]).

verbe_transf([enters,rejects,has,retains,presses,enters,type,talks]).

verbe_2cpl([sends, works , gives , inserts]).

aux([is,does]).

pi([?]).

ponct([;,.,,]).

%/________________________________________________
ponct1([A|R],R):- ponct(L),element(A,L).
ponct1(L,L).

and_or1([A|R],R):- and_or(L),element(A,L).
%and_or1(L,L).

pi1([A|R],R):- pi(L),element(A,L).

aux1([A|R],R):- aux(L),element(A,L).

%Sujet1([A|R],R):- Sujet(L),element(A,L).

nom_propre1([A|R],R):- nom_propre(L),element(A,L).

nom_commun1([A|R],R):- nom_commun(L),element(A,L).

theire1([A|R],R):- theire(L),element(A,L).

adj1([A|R],R):- adj(L),element(A,L).

prep1([A|R],R):- prep(L),element(A,L).

verbe_intransf1([A|R],R):-verbe_intransf(L),element(A,L).
verbe_transf1([A|R],R):-verbe_transf(L),element(A,L).
verbe_2cpl1([A|R],R):-verbe_2cpl(L),element(A,L).

verbe_c1([A|R],R):- verbe_c(L),element(A,L).


the1([A|R],R):- the(L),element(A,L).

relative_prenom1([A|R],R):- relative_prenom(L),element(A,L).


%_________________________________________________________________Grammaire ____________________________________________
%/_______________________ phrase

phrase(L):- phrase_simple(L,L1), pi1(L1,[]).
phrase(L):- phrase_simple(L,L1), phrase_coord(L1,L3), pi1(L3,[]).

%/_______________________ phrase_coord

phrase_coord(L,L1):- ponct1(L,L2), and_or1(L2,L3), ponct1(L3,L4), suite_coord(L4,L1).
phrase_coord(L,L).

%/_______________________________________element

element([],_).
element(X,[X|Z]).
element(X,[Y|Z]):-element(X,Z).

%/_______________________________________the_nom

sujet1(L,Suite) :- nom_propre1(L,Suite).
sujet1(L,Suite) :- the1(L,L1), adject(L1,L2),nom_commun1(L2,Suite).
sujet1(L,Suite) :- the1(L,L1), adject(L1,L2), nom_propre1(L2,Suite).
sujet(L,L1):- theire1(L,L1).


%/________________________________________ complement
complement(L,Suite) :- sujet1(L,Suite).
complement(L,Suite) :- sujet1(L,L1),nompropre1(L1,Suite).
complement(L,Suite) :- sujet1(L,Lu1),relative_prenom1(L1,L2), predicat(L2, Suite).
complement(L,Suite) :- sujet1(L,L1),complement(L1,Suite),/.
complement(L,Suite) :- prep1(L,L2),sujet1(L2, Suite).

%/______________________________________verbe_c

verbe_c(L, Suite):- verbe_intransf1(L, Suite).
verbe_c(L, Suite):- verbe_transf1(L, L1), complement(L1, Suite).
verbe_c(L, Suite):- verbe_2cpl1(L, L1), complement(L1, Suite).
verbe_c(L, Suite):- verbe_2cpl1(L, L1), complement(L1, L2), 
                   complement(L2, suite).

%/_______________________________________________phrase_simple

phrase_simple(L,Reste):- aux1(L,L1), sujet1(L1,L2), suite_phrase(L2,Reste).

suite_phrase(L,L1):- gn(L,L1).
suite_phrase(L,L1):- predicat(L,L1).
suite_phrase(L,L).

 %sujet(L,L1):- nom_propre1(L,L1).
%sujet(L,L1):- the1(L,L2), nom_commun1(L2,L1).
%sujet(L,L1):- theire1(L,L1).

gn(L,L1):- adject(L,L1).
gn(L,L1):- prep1(L,L2), adject(L,L3),nom_commun1(L3,L1).

adject(L,L1):- adj1(L,L1).
adject(L,L).

%predicat(L,L1):- verbe_c(L,L2),prep1(L2,L3),nom_commun1(L3,L1).
predicat(L,L1):- verbe_c(L,L1).
           
%____________________________________________________Suite_coord

suite_coord(L,S):- verbe(L,L1), the1(L1,L2), nom_commun1(L2,L3), suite(L3,S).

suite(L,S):- prep1(L,L1), det1(L1,L2),nom_commun1(L3,S).
suite(L,L).

verbe(L,S):- verbe_c(L,S).
verbe(L,L).
