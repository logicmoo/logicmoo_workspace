/*************************************************************************

    File: betaConversionTestSuite.pl
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

:- module(betaConversionTestSuite,[expression/2]).


/*========================================================================
   Lambda-Expressions
========================================================================*/

expression(app(lam(P,app(P,mia)),lam(X,walk(X))),
	   walk(mia)).

expression(some(X,and(man(X),app(lam(P,some(X,and(woman(X),app(P,X)))),lam(Y,love(X,Y))))),
	   some(X,and(man(X),some(Y,and(woman(Y),love(X,Y)))))).

%%-----------------------------------------------------------------------
%% Simple reduction
%%   Ly. sleep(y) mia --> sleep(mia)
%%--------------------------------------

expression(app(lam(A,sleep(A)),mia),
           sleep(mia)).

%%-----------------------------------------------------------------------
%% Reduction to another function: 
%%   Ly.(Lx.like(x,y)) mia --> Lx.like(x,mia)  
%%-----------------------------------------------

expression(app(lam(A,lam(B,like(B,A))),mia),
           lam(C,like(C,mia))).

%%-----------------------------------------------------------------------
%% Reduction of inner expression leaving outer function: 
%%   Ly.(Lx.like(x,y) (vincent)) --> Ly.like(vincent,y)  
%%------------------------------------------------------

expression(lam(A,app(lam(B,like(B,A)),vincent)),
           lam(C,like(vincent,C))).

%%-------------------------------------------------------------------------
%% Reduction of inner expression leaving outer function, with var y shared.
%%   Ly.and((Lx.like(x,y) (vincent)),sleep(y)) --> 
%%                         Ly.and(like(vincent,y),sleep(y))
%%--------------------------------------------------------

expression(lam(A,and(app(lam(B,like(B,A)),vincent),sleep(A))),
           lam(C,and(like(vincent,C),sleep(C)))).

%%-----------------------------------------------------------------------
%% Reduction twice:  
%%   (Ly.(Lx.like(x,y)) mia) vincent) --> like(vincent,mia)  
%%------------------------------------------------------

expression(app(app(lam(A,lam(B,like(B,A))),mia),vincent),
           like(vincent,mia)).

%%-----------------------------------------------------------------------
%% Reductions in a nested position
%%    p((Lx.sleep(X))vincent) --> p(sleep(vincent))
%%---------------------------------------------

expression(p(app(lam(A,sleep(A)),vincent)),
           p(sleep(vincent))).

%%-----------------------------------------------------------------------
%% Reductions inside a variable predicate
%%    LP. P((Lx.sleep(x)) vincent) --> LP. P(sleep(vincent))
%%------------------------------------------------------

expression(lam(A,app(A,app(lam(B,sleep(B)),vincent))),
           lam(A,app(A,sleep(vincent)))).

%%-----------------------------------------------------------------------
%% No reductions possible
%%    LP. P(sleep(vincent)) --> LP. P(sleep(vincent))
%%------------------------------------------------------

expression(lam(A,app(A,sleep(vincent))),
           lam(A,app(A,sleep(vincent)))).

%%-----------------------------------------------------------------------
%% Nested reductions (apply to a function which must be applied again)
%%  LP. (P(vincent)) (Lx.sleep(x)) --> sleep(vincent)
%%-----------------------------------------------

expression(app(lam(A,app(A,vincent)),lam(B,sleep(B))),
           sleep(vincent)).

%%---------------------------------------------------------------------------
%%  LP. believe(mia,(P(vincent)) (Lx.sleep(x)) --> believe(mia,sleep(vincent))
%%---------------------------------------------------------------------------

expression(app(lam(A,believe(mia,app(A,vincent))),lam(B,sleep(B))),
           believe(mia,sleep(vincent))).

%%---------------------------------------------------------------------------
%% Copied functions
%% (LP. P*vincent and P*mia) Lx.sleep(x) --> sleep(vincent) and sleep(mia)
%%-----------------------------------------------------------------------

expression(app(lam(A,and(app(A,vincent),app(A,mia))),lam(B,sleep(B))),
           and(sleep(vincent),sleep(mia))).

%%------------------------------------------------------------------------
%%   (LP.(LQ.and(LR.R(P) prob), LR.R(Q) improb) (Lx.walk(x)) (Lx.talk(X)) 
%%      -> and(prob(walk(vincent)), improb(talk(mia)))   
%%------------------------------------------------------

expression(app(app(lam(A,lam(B,and(app(lam(C,app(C,app(A,vincent))),lam(D,probably(D))),app(lam(C,app(C,app(B,mia))),lam(D,improbably(D)))))),lam(E,walk(E))),lam(E,talk(E))),
           and(probably(walk(vincent)),improbably(talk(mia)))).


%%---------------------------------------------------------------------------
%% Double application.
%%
%% (Lu.(Lv . (LP.P(u,v) Ly.Lx.love(x,y)) mia) jules) --> love(jules,mia)
%%--------------------------------------------------------------------

expression(app(app(lam(A,lam(B,app(lam(C,app(app(C,A),B)),lam(D,lam(E,love(D,E)))))),jules),mia),
           love(jules,mia)).

%%---------------------------------------------------------------------------
%% Two functions with the same use of variable.
%% ((LP.LQ. Ex.(P*x and Q*x))) Ly.boxer(y)) Ly.sleep(y) --> 
%%                  Some(boxer(x) and sleep(x))
%%----------------------------------------------

expression(app(app(lam(A,lam(B,some(C,and(app(A,C),app(B,C))))),lam(D,boxer(D))),lam(D,sleep(D))),
           some(C,and(boxer(C),sleep(C)))).

%%------------------------------------------------------------------------
%% Test for correctly dealing with the same variable name occurring twice
%%    Lx(P(x)) (Ly.Lx.like(x,y)) --> P(Lx.Lz.like(z,x))
%% (loops without alpha-conversion)
%%-------------------------------------------------------

expression(app(lam(A,app(_,A)),lam(C,lam(A,like(A,C)))),
           app(_,lam(E,lam(F,like(F,E))))).

%%------------------------------------------------------------------------
%% Test for correctly performing alpha conversion
%%   LP.Lx(P(x)) (Ly.Lx.like(x,y)) --> Lx.Lz.like(z,x)
%%----------------------------------------------------

expression(app(lam(A,lam(B,app(A,B))),lam(C,lam(B,like(B,C)))),
           lam(D,lam(E,like(E,D)))).

%%---------------------------------------------------------------------------
%% Test for correctly performing alpha conversion
%% (Lx.(Ly. (LP.P(x,y) Ly.Lx.love(x,y)) mia) jules) --> love(jules,mia)
%%--------------------------------------------------------------------

expression(app(app(lam(A,lam(B,app(lam(C,app(app(C,A),B)),lam(B,lam(A,love(B,A)))))),jules),mia),
           love(jules,mia)).

%%---------------------------------------------------------------------------
%% Test for correctly performing alpha conversion
%% (Lx.(Ly. (LP.P(x,y) Ly.Lx.love(x,y)))) --> Lu.Lv. love(u,v)
%%--------------------------------------------------------------------

expression(lam(A,lam(B,app(lam(C,app(app(C,A),B)),lam(B,lam(A,love(B,A)))))),
           lam(D,lam(E,love(D,E)))).

%%---------------------------------------------------------------------------
%% Further alpha conversion testing
%% (LP. Lx. (Ex.P*x) and P*x) Ly.boxer(y) vincent
%%----------------------------------------------

expression(app(app(lam(A,lam(B,and(some(B,app(A,B)),app(A,B)))),lam(C,boxer(C))),vincent),
           and(some(B,boxer(B)),boxer(vincent))).
