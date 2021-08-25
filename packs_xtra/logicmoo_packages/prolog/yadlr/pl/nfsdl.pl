%% Compute normal forms for SHOIQ formulae.
%% Skolemize SHOIQ formula.
%%
%% Copyright (C) 1999 Anthony A. Aaby <aabyan@wwc.edu>
%% Copyright (C) 2006-2007 Stasinos Konstantopoulos <stasinos@users.sourceforge.net>
%%
%% This program is free software; you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation; either version 2 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License along
%% with this program; if not, write to the Free Software Foundation, Inc.,
%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

%% FORMULA SYNTAX
%%
%% dlnot(A)
%% dland(F, F)
%% dlor(F, F)
%% dlimplies(F, F)
%% dlequiv(F, F)
%%    all(X,A)
%%    exists(X,A)
%%    atleast(X,N,A)
%%    atmost(X,N,A)

:- module(nfsdl,[nnf/2, pnf/2, cf/2]).

% SWI Prolog modules do not export operators by default
% so they must be explicitly placed in the user namespace

:- ( prolog_engine(swi) ->
     op( 400, fy, user:(box) ),	% Necessity, Always
     op( 400, fy, user:(dia) ),	% Possibly, Eventually
     op( 400, fy, user:(cir) )	% Next time
   ;
     op(400,fy,box),		% Necessity, Always
     op(400,fy,dia),		% Possibly, Eventually
     op(400,fy,cir)		% Next time
   ).



%%% Negation Normal Form

% Usage: nnf(+Fml, ?NNF)

nnf(Fml,NNF) :- nnf(Fml,[],NNF,_).

% -----------------------------------------------------------------
%  nnf(+Fml,+FreeV,-NNF,-Paths)
%
% Fml,NNF:    See above.
% FreeV:      List of free variables in Fml.
% Paths:      Number of disjunctive paths in Fml.

nnf(box F,FreeV,BOX,Paths) :- !,
	nnf(F,FreeV,NNF,Paths), cnf(NNF,CNF), boxRule(box CNF, BOX).

nnf(dia F,FreeV,DIA,Paths) :- !,
	nnf(F,FreeV,NNF,Paths), dnf(NNF,DNF), diaRule(dia DNF, DIA).

nnf(cir F,FreeV,CIR,Paths) :- !,
	nnf(F,FreeV,NNF,Paths), cirRule(cir NNF, CIR).

nnf(until(A,B),FreeV,NNF,Paths) :- !,
	nnf(A,FreeV,NNF1,Paths1),
	nnf(B,FreeV,NNF2,Paths2),
	Paths is Paths1 + Paths2,
	NNF = until(NNF1, NNF2).

nnf(all(X,F),FreeV,all(X,NNF),Paths) :- !,
	nnf(F,[X|FreeV],NNF,Paths).

nnf(exists(X,Fml),FreeV,NNF,Paths) :- !,
	skolem(Fml,X,FreeV,FmlSk),
	nnf(FmlSk,FreeV,NNF,Paths).

/*
nnf(atleast(1,X,Fml),FreeV,NNF,Paths) :-
	!,
	nnf(exists(X,Fml),FreeV,NNF,Paths).
nnf(atleast(N,X,Fml),FreeV,NNF,Paths) :-
	!,
	NewN is N - 1,
	nnf(dland(exists(X,Fml),atleast(NewN,Y,Fml)),FreeV,NNF,Paths).

nnf(atmost(1,X,Fml),FreeV,NNF,Paths) :-
	!,
	nnf(dlnot(dland(exists(Y,Fml),exists(Z,Fml))),FreeV,NNF,Paths).
nnf(atmost(N,X,Fml),FreeV,NNF,Paths) :-
	!,
	NewN is N - 1,
	nnf(dland(exists(Y,Fml),atmost(NewN,X,Fml)),FreeV,NNF,Paths).
*/

nnf(dland(A,B),FreeV,NNF,Paths) :- !,
	nnf(A,FreeV,NNF1,Paths1),
	nnf(B,FreeV,NNF2,Paths2),
	Paths is Paths1 * Paths2,
	(Paths1 > Paths2 -> NNF = dland(NNF2,NNF1);
		            NNF = dland(NNF1,NNF2)).

nnf(dlor(A,B),FreeV,NNF,Paths) :- !,
	nnf(A,FreeV,NNF1,Paths1),
	nnf(B,FreeV,NNF2,Paths2),
	Paths is Paths1 + Paths2,
	(Paths1 > Paths2 -> NNF = dlor(NNF2,NNF1);
		            NNF = dlor(NNF1,NNF2)).

nnf(Fml,FreeV,NNF,Paths) :- 
	(Fml = dlnot(dlnot(A))   -> Fml1 = A;
	 Fml = dlnot(box F)      -> Fml1 = dia dlnot(F);
	 Fml = dlnot(dia F)      -> Fml1 = box dlnot(F);
	 Fml = dlnot(cir F)      -> Fml1 = cir dlnot(F);
	 Fml = dlnot(until(A,B)) -> (nnf(dlnot(A),FreeV,NNA,_), nnf(dlnot(B),FreeV,NNB,_),
                                     Fml1 = dlor( all(NNB), until(NNB,dland(NNA,NNB))));
	 Fml = dlnot(all(X,F))   -> Fml1 = exists(X,dlnot(F));
	 Fml = dlnot(exists(X,F))    -> Fml1 = all(X,dlnot(F));
/*
	 Fml = dlnot(atleast(N,X,F)) -> Fml1 = atmost(N,X,F);
	 Fml = dlnot(atmost(N,X,F)) -> Fml1 = atleast(N,X,F);
*/
	 Fml = dlnot(dlor(A,B))  -> Fml1 = dland( dlnot(A), dlnot(B) );
	 Fml = dlnot(dland(A,B)) -> Fml1 = dlor( dlnot(A), dlnot(B) );
	 Fml = dlimplies(A,B)        -> Fml1 = dlor( dlnot(A), B );
	 Fml = dlnot(dlimplies(A,B)) -> Fml1 = dland( A, dlnot(B) );
	 Fml = dlequiv(A,B)        -> Fml1 = dlor( dland(A, B), dland(dlnot(A), dlnot(B)) );
	 Fml = dlnot(dlequiv(A,B)) -> Fml1 = dlor( dland(A, dlnot(B)) , dland(dlnot(A), B) )
	),!,
	nnf(Fml1,FreeV,NNF,Paths).

nnf(Lit,_,Lit,1).

boxRule(box dland(A,B), dland(BA,BB)) :- !, boxRule(box A,BA), boxRule(box B,BB).
boxRule(BOX, BOX).

diaRule(dia dlor(A,B), dlor(DA,DB)) :- !, diaRule(dia A,DA), diaRule(dia B,DB).
diaRule(DIA, DIA).

cirRule(cir dlor(A,B), dlor(DA,DB)) :- !, cirRule(cir A,DA), cirRule(cir B,DB).
cirRule(cir dland(A,B), dland(DA,DB)) :- !, cirRule(cir A,DA), cirRule(cir B,DB).
cirRule(CIR, CIR).


%%%
%%%  Conjunctive Normal Form (CNF) -- assumes Fml in NNF
%%%

% Usage: cnf( +NNF, ?CNF )

cnf(dland(P,Q), dland(P1,Q1)):- !, cnf(P, P1), cnf(Q, Q1).
cnf(dlor(P,Q),     CNF):- !, cnf(P, P1), cnf(Q, Q1), cnf1( dlor(P1,Q1), CNF ).
cnf(CNF,       CNF).

cnf1( dlor(dland(P,Q), R), dland(P1,Q1) ):- !, cnf1( dlor(P,R), P1), cnf1( dlor(Q,R), Q1).
cnf1( dlor(P, dland(Q,R)), dland(P1,Q1) ):- !, cnf1( dlor(P,Q), P1), cnf1( dlor(P,R), Q1).
cnf1( CNF,                 CNF).


%%%
%%% Disjunctive Normal Form (DNF) -- assumes Fml in NNF
%%%
% Usage: dnf( +NNF, ?DNF )

dnf( dlor(P,Q),  dlor(P1,Q1) ) :- !, dnf(P, P1), dnf(Q, Q1).
dnf( dland(P,Q), DNF) :- !, dnf(P, P1), dnf(Q, Q1), dnf1( dland(P1,Q1), DNF).
dnf(DNF,       DNF).

dnf1( dland(P, dlor(Q,R)),  dlor(P1,Q1) ):- !, dnf1( dland(P,Q), P1), dnf1( dland(P,R), Q1).
dnf1( dland( dlor(P,Q), R), dlor(P1,Q1) ):- !, dnf1( dland(P,R), P1), dnf1( dland(Q,R), Q1).
dnf1( DNF,                  DNF ).



%%
%%  Prenex Normal Form (PNF)
%%

% Usage: pnf( +Fml, ?PNF ) -- assumes Fml in NNF

pnf(F,PNF) :- pnf(F,[],PNF).

% pnf(+Fml, +Vars, ?PNF)

pnf(     all(X,F),Vs,   all(X,PNF)) :- !, pnf(F,[X|Vs], PNF).
pnf(  exists(X,F),Vs,exists(X,PNF)) :- !, pnf(F,[X|Vs], PNF).

pnf(  dland(exists(X,A) , B),Vs,  exists(Y,PNF)) :- !, copy_term((X,A,Vs),(Y,Ay,Vs)),
                                        pnf(dland(Ay,B),[Y|Vs], PNF).
pnf(  dlor(exists(X,A), B),Vs,  exists(Y,PNF)) :- !, copy_term((X,A,Vs),(Y,Ay,Vs)),
                                        pnf(dlor(Ay,B),[Y|Vs], PNF).
pnf( dland(all(X,A), B),Vs, all(Y,PNF)) :- !, copy_term((X,A,Vs),(Y,Ay,Vs)),
                                        pnf(dland(Ay , B),[Y|Vs], PNF).
pnf( dlor(all(X,A), B),Vs, all(Y,PNF)) :- !, copy_term((X,A,Vs),(Y,Ay,Vs)),
                                        pnf(dlor(Ay,B),[Y|Vs], PNF).

pnf( dland(A,exists(X,B)),Vs,  exists(Y,PNF)) :- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnf(dland(A, By),[Y|Vs], PNF).
pnf( dlor(A,exists(X,B)),Vs,  exists(Y,PNF)) :- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnf(dlor(A,By),[Y|Vs], PNF).
pnf( dland(A,all(X,B)),Vs, all(Y,PNF)) :- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnf(dland(A,By),[Y|Vs], PNF).
pnf( dlor(A,all(X,B)),Vs, all(Y,PNF)) :- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnf(dlor(A,By),[Y|Vs], PNF).

pnf( dland(A, B),Vs,       PNF ) :- pnf(A,Vs,Ap), pnf(B,Vs,Bp), 
                                     (A\=Ap; B\=Bp), pnf(dland(Ap,Bp),Vs,PNF).
pnf( dlor(A, B),Vs,       PNF ) :- pnf(A,Vs,Ap), pnf(B,Vs,Bp), 
                                     (A\=Ap; B\=Bp), pnf(dlor(Ap,Bp),Vs,PNF).

pnf(          PNF, _,       PNF ).

%%%  Clausal Form (CF) -- assumes Fml in PNF and
%                                 each quantified variable is unique

% cf(+Fml, ?Cs)
% Cs is a list of the form: [cl(Head,Body), ...]
% Head and Body are lists.

cf(PNF, Cla):- removeQ(PNF,[], UnQ), cnf(UnQ,CNF), clausify(CNF,Cla,[]).

% removes quantifiers
removeQ( all(X,F),Vars, RQ) :- removeQ(F,[X|Vars], RQ).
removeQ(  exists(X,F),Vars, RQ) :-
	skolem(F,X,Vars,Fsk),
	removeQ(Fsk,Vars, RQ).
removeQ( F,_,F ).

clausify( dland(P,Q), C1, C2 ) :-
	!,
	clausify( P, C1, C3 ),
	clausify( Q, C3, C2 ).
clausify( P, [cl(A,B)|Cs], Cs ) :-
	inclause( P, A, [], B, [] ),
	!.
clausify( _, C, C ).

inclause( dlor(P,Q), A, A1, B, B1 ) :-
	!,
	inclause( P, A2, A1, B2, B1 ),
	inclause( Q, A,  A2, B,  B2 ).
inclause( dlnot(P), A,  A, B1, B ) :-
	!,
	notin( P, A ),
	putin( P, B, B1 ).
inclause( P,  A1, A, B,  B ) :-
	!,
	notin( P, B ),
	putin( P, A, A1 ).

notin(X,[Y|_]) :- X==Y, !, fail.
notin(X,[_|Y]) :- !,notin(X,Y).
notin(_,[]).

putin(X,[],   [X]   ) :- !.
putin(X,[Y|L],[Y|L] ) :- X == Y,!.
putin(X,[Y|L],[Y|L1]) :- putin(X,L,L1).



%%%  Skolemizing -- method 1

% Usage: skolemize(+Fml,+X,+FreeV,?FmlSk)
% Replaces existentially quantified variable with the formula
% VARIABLES MUST BE PROLOG VARIABLES
% ex(X,p(X)) --> p(p(ex))

skolem(Fml,X,FreeV,FmlSk):-
	copy_term((X,Fml,FreeV),(Fml,Fml1,FreeV)),
	copy_term((X,Fml1,FreeV),(exists,FmlSk,FreeV)).



%%%  Skolemizing -- method 2

% Usage: skolem( +Fml, +X, +FreeV, ?FmlSk )
% Replaces existentially quantified variable with a unique function
% fN(Vars) N=1,...
% VARIABLES MAYBE EITHER PROLOG VARIABLES OR TERMS

skolem_not( F, X, FreeV, FmlSk) :-
	genatom( Fun ),
	Sk =..[Fun|FreeV],
	subst( F, X, Sk, FmlSk ).


%%% generate new atomic symbols

genatom( A ) :-
	db_recorded( nfsdl, Inc, Ref ),
	!,
	erase( Ref ),
	NewInc is Inc + 1,
	db_recordz( nfsdl, NewInc, _ ),
	atom_concat( f, NewInc, A ).
genatom( f1 ) :-
	db_recordz( nfsdl, 1, _ ).


%%% Substitution

% Usage: subst(+Fml,+X,+Sk,?FmlSk)

subst(   all(Y,P), X,Sk,   all(Y,P1) ) :- !, subst( P,X,Sk,P1 ).
subst(exists(Y,P), X,Sk,exists(Y,P1) ) :- !, subst( P,X,Sk,P1 ).
subst( dland(P,Q), X,Sk,dland(P1,Q1) ) :- !, subst( P,X,Sk,P1 ), subst( Q,X,Sk,Q1 ).
subst(  dlor(P,Q), X,Sk, dlor(P1,Q1) ) :- !, subst( P,X,Sk,P1 ), subst( Q,X,Sk,Q1 ).
subst(       P,    X,Sk,       P1    ) :- functor(P,_,N), subst1( X, Sk, P, N, P1 ).

subst1( _,  _, P, 0, P  ).
subst1( X, Sk, P, N, P1 ) :- N > 0, P =..[F|Args], subst2( X, Sk, Args, ArgS ),
                             P1 =..[F|ArgS].

subst2( _,  _, [], [] ).
subst2( X, Sk, [A|As], [Sk|AS] ) :- X == A, !, subst2( X, Sk, As, AS).
subst2( X, Sk, [A|As], [A|AS]  ) :- var(A), !, subst2( X, Sk, As, AS).
subst2( X, Sk, [A|As], [Ap|AS] ) :- subst( A,X,Sk,Ap ),
                                    subst2( X, Sk, As, AS).
