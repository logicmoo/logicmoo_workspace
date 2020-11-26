%________________________________  Dictionnaire   _____________________________________

nom_commun([customer,employee,year,card,shop,train,monday,number,station,key,machine,door,room,slot,end,bill]).

nom_propre([john,simplemat]). 

and_or([and,or]).

adverbe([correctly,manually,ponctually]). 

be([is]). 

not([not]). 
%not(L,L).

prep([to,into,of]). 

more_less([more,less]).

than([than]).

the([the,a]).

theire([theire]).

adj([invalid,valid,blue,fast]).

qualificat([no,every,each,there_is_a,there_is_an,there_is_no]).

verbe_intransf([arrive,works,stop]).

verbe_transf([enter,reject,has,retain,presse,enter,type,open,talks,cheek]).

verbe_2cpl([sends,insert]).


relative_prenom([that,who]).

which([which]).

most_least([most,least]).

%if2([if].
%then2([then]).
for_every([for_every]).

%for_every1([for_every]).
%for_every1(L,L).

prenon_p_sujet([she,he,it]).

aux([is,does]).

pi([?]).

ponct([;,.,,]).
nom_wh([what,with,who,when, since,until,how,where,from]).
nom_wh22([what,whom,when,long,often,from,with,where]).


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

there1([A|R],R):- there(L),element(A,L).

adj1([A|R],R):- adj(L),element(A,L).

prep1([A|R],R):- prep(L),element(A,L).

%verbe_c1([A|R],R):- verbe_c(L),element(A,L).


the1([A|R],R):- the(L),element(A,L).

relative_prenom1([A|R],R):- relative_prenom(L),element(A,L).

nom_wh1([A|R],R):- nom_wh(L),element(A,L).

nom_wh2([A|R],R):- nom_wh22(L),element(A,L).
nom_wh2(L,L).

verbe_intransf1([A|R],R):-verbe_intransf(L),element(A,L).
verbe_transf1([A|R],R):-verbe_transf(L),element(A,L).
verbe_2cpl1([A|R],R):-verbe_2cpl(L),element(A,L).


%/_______________________________________element

element([],_).
element(X,[X|Z]).
element(X,[Y|Z]):-element(X,Z).



%_________________________________________________________________Grammaire ____________________________________________
%/_________________________________  phrase


phrase(L):- phrase_simple(L,L1), pi1(L1,[]).
%phrase(L):- phrase_simple(L,L1), phrase_coord(L1,L3), pi1(L3,[]).


%/_______________________  phrase_simple

phrase_simple(L,S):- nom_wh1(L,L1),nom_wh2(L1,L2),aux1(L2,L3),sujet1(L3,L4), verbe_c1(L4,L5), nom_wh2(L5,S).
phrase_simple(L,S):-nom_wh1(L,L1),aux1(L1,L2),sujet1(L2,S).

%/___________________________sujet1

sujet1(L,Suite) :- nom_propre1(L,Suite).
sujet1(L,Suite) :- the1(L,L1), nom_commun1(L1,Suite).
sujet1(L,Suite) :- the1(L,L1), adject(L1,L2),sujet1(L2,Suite).
sujet1(L,L1):- theire1(L,L1).

%/___________adject


adject(L,L1):- adj1(L,L1).
adject(L,L).

%/_______________________________________the_nom

the_nom(L,Suite) :- nom_propre(L,Suite).
the_nom(L,Suite) :- the1(L,L1), nom_commun1(L1,Suite).
the_nom(L,Suite) :- the1(L,L1), adject(L1,L2),sujet(L2,Suite).
%the_nom(L,L1):- theire1(L,L1).


%/________________________________________ complement
complement(L,Suite) :- the_nom(L,Suite).
complement(L,Suite) :- the_nom(L,L1),complement(L1,Suite),/.
complement(L,Suite) :- prep1(L,L1), the_nom(L1,Suite).


%/_______________________ verbe_c1

verbe_c1(L,S):- verbe_intransf1(L,S).
verbe_c1(L,S):- verbe_transf1(L,S).
verbe_c1(L,S):- verbe_transf1(L,L1),complement(L1,S).
%verbe_c1(L,S):- verbe_2cpl1(L,L1), complement(L1,S).
verbe_c1(L,S):- verbe_2cpl1(L,L1), complement(L1,L2), prep1(L2,S).
verbe_c1(L,S):- verbe_2cpl1(L,L1), prep1(L1,S).


