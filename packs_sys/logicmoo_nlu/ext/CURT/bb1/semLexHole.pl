/*************************************************************************

    File: semLexHole.pl
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

/*========================================================================
   Semantic Macros
========================================================================*/

semLex(det,M):-
   M = [type:uni,
        sem:lam(N,lam(V,lam(H,lam(L,some(H1,some(L1,some(L2,some(L3,some(X,
                and(hole(H1),and(label(L1),and(label(L2),and(label(L3),
                    and(all(L2,X,L3),and(imp(L3,L1,H1),and(leq(L,H1),
                        and(leq(L2,H),and(app(app(app(N,X),H),L1),
                            app(app(app(V,X),H),L)))))))))))))))))))].

semLex(det,M):-
   M = [type:indef,
        sem:lam(N,lam(V,lam(H,lam(L,some(H1,some(L1,some(L2,some(L3,some(X,
                and(hole(H1),and(label(L1),and(label(L2),and(label(L3),
                    and(some(L2,X,L3),and(and(L3,L1,H1),and(leq(L,H1),
                        and(leq(L2,H),and(app(app(app(N,X),H),L1),
                            app(app(app(V,X),H),L)))))))))))))))))))].

semLex(det,M):-
   M = [type:wh,
        sem:lam(N,lam(V,lam(H,lam(L,some(H1,some(L1,some(L2,some(X,and(hole(H1),
                and(label(L1),and(label(L2),and(que(L2,X,L1,H1),and(leq(L,H1),
                    and(leq(L2,H),and(app(app(app(N,X),H),L1),
                        app(app(app(V,X),H),L))))))))))))))))].

semLex(pn,M):-
   M = [symbol:Sym,
        sem:lam(V,lam(H,lam(L,app(app(app(V,Sym),H),L))))].

semLex(noun,M):-
   M = [symbol:Sym,
        sem:lam(X,lam(H,lam(L,and(pred1(L,Sym,X),leq(L,H)))))].

semLex(iv,M):-
   M = [symbol:Sym,
        sem:lam(X,lam(H,lam(L,and(pred1(L,Sym,X),leq(L,H)))))].

semLex(tv,M):-
   M = [symbol:Sym,
        sem:lam(Z,lam(X,app(Z,lam(Y,lam(H,lam(L,and(pred2(L,Sym,Y,X),leq(L,H))))))))]. 

semLex(qnp,M):-
   M = [type:wh,
        symbol:Sym,
        sem:lam(V,lam(H,lam(L,some(H1,some(L2,some(L3,some(X,and(hole(H1),and(label(L2),
                and(label(L3),and(que(L2,X,L3,H1),and(pred1(L3,Sym,X),and(leq(L,H1),
                    and(leq(L2,H),app(app(app(V,X),H),L)))))))))))))))]. 

semLex(cop,M):-
    M = [pol:pos,
         sem:lam(Z,lam(X,app(Z,lam(Y,lam(H,lam(L,and(eq(L,Y,X),leq(L,H))))))))];
    M = [pol:neg,
         sem:lam(Z,lam(X,app(Z,lam(Y,lam(H,lam(L,some(L1,and(label(L1),some(H1,and(hole(H1),and(not(L1,H1),
                 and(eq(L,Y,X),and(leq(L,H1),leq(L1,H))))))))))))))].

semLex(relpro,M):-
   M = [sem:lam(V,lam(N,lam(X,lam(H,lam(L,some(H1,some(L1,some(L2,and(hole(H1),and(label(L1),
                and(label(L2),and(and(L,L1,H1),and(leq(L,H),and(leq(L2,H1),
                    and(app(app(app(V,X),H),L2),app(app(app(N,X),H),L1))))))))))))))))].

semLex(prep,M):-
   M = [symbol:Sym,
        sem:lam(Z,lam(N,lam(X,lam(H,lam(L,some(H2,some(L2,some(L3,and(hole(H2),and(label(L2),
                and(label(L3),and(and(L,L2,H2),and(leq(L,H),and(leq(L3,H2),and(app(app(app(Z,
                    lam(Y,lam(H1,lam(L1,and(pred2(L1,Sym,Y,X),leq(L1,H1)))))),H),L3),
                        app(app(app(N,X),H),L2))))))))))))))))].

semLex(adj,M):-
   M = [symbol:Sym,
        sem:lam(P,lam(X,lam(H,lam(L,some(L1,some(L2,and(label(L1),and(label(L2),and(and(L,L1,L2),
                and(pred1(L2,Sym,X),and(leq(L,H),app(app(app(P,X),H),L1))))))))))))].

semLex(av,M):-
   M = [pol:neg,
        sem:lam(V,lam(X,lam(H,lam(L,some(S,some(N,and(hole(S),and(label(N),and(not(N,S),
                and(leq(N,H),and(leq(L,S),app(app(app(V,X),H),L))))))))))))];
   M = [pol:pos,
        sem:lam(V,lam(X,lam(H,lam(L,app(app(app(V,X),H),L)))))]. 

semLex(coord,M):-
   M = [type:conj,
        sem:lam(C1,lam(C2,lam(X,lam(H,lam(L,some(L1,some(L2,and(label(L1),and(label(L2),
                and(and(L,L1,L2),and(leq(L,H),and(app(app(app(C1,X),H),L1),
                    app(app(app(C2,X),H),L2)))))))))))))];  
   M = [type:disj,
        sem:lam(C1,lam(C2,lam(X,lam(H,lam(L,some(L1,some(L2,and(label(L1),and(label(L2),
                and(and(L,L1,L2),and(leq(L,H),and(app(app(app(C1,X),H),L1),
                    app(app(app(C2,X),H),L2)))))))))))))].

