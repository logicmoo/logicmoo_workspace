/*************************************************************************

    File: semLexPronounDRT.pl
    Copyright (C) 2004 Patrick Blackburn & Johan Bos

    This file is part of BB2, version 1.0 (June 2004).

    BB2 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB2 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB2; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

semLex(det,M):-
   M = [type:uni,
        sem:lam(U,lam(V,drs([],[imp(merge(drs([X],[]),app(U,X)),app(V,X))])))].

semLex(det,M):-
   M = [type:indef,
        sem:lam(U,lam(V,merge(merge(drs([X],[]),app(U,X)),app(V,X))))].

semLex(det,M):-
   M = [type:neg,
        sem:lam(U,lam(V,drs([],[not(merge(merge(drs([X],[]),app(U,X)),app(V,X)))])))].

semLex(pn,M):-
   M = [symbol:Sym,
        sem:lam(P,merge(drs([X],[eq(X,Sym):[]]),app(P,X)))].

semLex(pro,M):-
   M = [symbol:Sym,
        sem:lam(P,alfa(pro,drs([X],[Cond:[]]),app(P,X)))],
   compose(Cond,Sym,[X]).

semLex(noun,M):-
   M = [symbol:Sym,
        sem:lam(X,drs([],[Cond:[]]))],
   compose(Cond,Sym,[X]).

semLex(iv,M):-
   M = [symbol:Sym,
        sem:lam(X,drs([],[Cond:[]]))],
   compose(Cond,Sym,[X]).

semLex(tv,M):-
   M = [symbol:Sym,
        Ref,
        sem:lam(K,lam(Y,app(K,lam(X,drs([],[Cond:[Ref]])))))],
   compose(Cond,Sym,[Y,X]).

semLex(cop,M):-
   M = [pol:pos,
        sem:lam(K,lam(Y,app(K,lam(X,drs([],[eq(Y,X):[]])))))];
   M = [pol:neg,
        sem:lam(K,lam(Y,drs([],[not(app(K,lam(X,drs([],[eq(Y,X):[]]))))])))].

semLex(relpro,M):-
   M = [sem:lam(P,lam(Q,lam(X,merge(app(P,X),app(Q,X)))))].

semLex(prep,M):-
   M = [symbol:Sym,
        sem:lam(K,lam(P,lam(Y,merge(app(K,lam(X,drs([],[Cond:[]]))),app(P,Y)))))],
   compose(Cond,Sym,[Y,X]).

semLex(adj,M):-
   M = [symbol:Sym,
        sem:lam(P,lam(X,merge(drs([],[Cond:[]]),app(P,X))))],
   compose(Cond,Sym,[X]).

semLex(av,M):-
   M = [pol:neg,
        sem:lam(P,lam(X,drs([],[not(app(P,X))])))];
   M = [pol:pos,
        sem:lam(P,lam(X,app(P,X)))].

semLex(coord,M):-
   M = [type:conj,
        sem:lam(X,lam(Y,lam(P,merge(app(X,P),app(Y,P)))))];  
   M = [type:disj,
        sem:lam(X,lam(Y,lam(P,drs([],[or(app(X,P),app(Y,P))]))))].




