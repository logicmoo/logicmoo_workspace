/*************************************************************************

    File: semLexLambda.pl
    Copyright (C) 2004 Patrick Blackburn & Johan Bos

    This file is part of BB1, version 1.2 (August 2005).

    BB1 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB1 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB1; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

semLex(det,M):-
   M = [type:uni,
        sem:lam(U,lam(V,all(X,imp(app(U,X),app(V,X)))))].

semLex(det,M):-
   M = [type:indef,
        sem:lam(P,lam(Q,some(X,and(app(P,X),app(Q,X)))))].

semLex(det,M):-
   M = [type:wh,
        sem:lam(P,lam(Q,que(X,app(P,X),app(Q,X))))].

semLex(pn,M):-
   M = [symbol:Sym,
        sem:lam(P,app(P,Sym))].

semLex(noun,M):-
   M = [symbol:Sym,
        sem:lam(X,Formula)],
   compose(Formula,Sym,[X]).

semLex(iv,M):-
   M = [symbol:Sym,
        sem:lam(X,Formula)],
   compose(Formula,Sym,[X]).

semLex(tv,M):-
   M = [symbol:Sym,
        sem:lam(K,lam(Y,app(K,lam(X,Formula))))], 
   compose(Formula,Sym,[Y,X]).

semLex(qnp,M):-
   M = [type:wh,
        symbol:Sym,
        sem:lam(Q,que(X,Formula,app(Q,X)))], 
   compose(Formula,Sym,[X]).

semLex(cop,M):-
   M = [pol:pos,
        sem:lam(K,lam(Y,app(K,lam(X,eq(Y,X)))))];
   M = [pol:neg,
        sem:lam(K,lam(Y,not(app(K,lam(X,eq(Y,X))))))].

semLex(relpro,M):-
   M = [sem:lam(P,lam(Q,lam(X,and(app(P,X),app(Q,X)))))].

semLex(prep,M):-
   M = [symbol:Sym,
        sem:lam(K,lam(P,lam(Y,and(app(K,lam(X,F)),app(P,Y)))))],
   compose(F,Sym,[Y,X]).

semLex(adj,M):-
   M = [symbol:Sym,
        sem:lam(P,lam(X,and(F,app(P,X))))],
   compose(F,Sym,[X]).

semLex(av,M):-
   M = [pol:neg,
        sem:lam(P,lam(X,not(app(P,X))))];
   M = [pol:pos,
        sem:lam(P,lam(X,app(P,X)))].

semLex(coord,M):-
   M = [type:conj,
        sem:lam(X,lam(Y,lam(P,and(app(X,P),app(Y,P)))))];  
   M = [type:disj,
        sem:lam(X,lam(Y,lam(P,or(app(X,P),app(Y,P)))))].

