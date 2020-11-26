/*************************************************************************

         name: betaConversionTestSuite.pl
      version: April 27, 2001
  description: Testsuite with Lambda Expressions
      authors: David Milward. Adapted and extended by
               Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(betaConversionTestSuite,[expression/2]).

:- ensure_loaded(comsemOperators).


/*========================================================================
   Lambda-Expressions
========================================================================*/

expression(lambda(P,P@mia)@lambda(X,walk(X)),
	   walk(mia)).

expression(exists(X,man(X)&(lambda(P,exists(X,woman(X)&(P@X)))@lambda(Y,love(X,Y)))),
	   exists(X,man(X)&exists(Y,woman(Y)&love(X,Y)))).

%%-----------------------------------------------------------------------
%% Simple reduction
%%   Ly. sleeps(y) mary --> sleeps(mary)
%%--------------------------------------

expression((lambda(A,sleeps(A))@mary),
           sleeps(mary)).

%%-----------------------------------------------------------------------
%% Reduction to another function: 
%%   Ly.(Lx.likes(x,y)) mary --> Lx.likes(x,mary)  
%%-----------------------------------------------

expression((lambda(A,lambda(B,likes(B,A)))@mary),
           lambda(C,likes(C,mary))).

%%-----------------------------------------------------------------------
%% Reduction of inner expression leaving outer function: 
%%   Ly.(Lx.likes(x,y) (john)) --> Ly.likes(john,y)  
%%------------------------------------------------------

expression(lambda(A,(lambda(B,likes(B,A))@john)),
           lambda(C,likes(john,C))).

%%-------------------------------------------------------------------------
%% Reduction of inner expression leaving outer function, with var y shared.
%%   Ly.and((Lx.likes(x,y) (john)),sleeps(y)) --> 
%%                         Ly.and(likes(john,y),sleeps(y))
%%--------------------------------------------------------

expression(lambda(A,((lambda(B,likes(B,A))@john) & sleeps(A))),
           lambda(C,(likes(john,C) & sleeps(C)))).

%%-----------------------------------------------------------------------
%% Reduction twice:  
%%   (Ly.(Lx.likes(x,y)) mary)john) --> likes(john,mary)  
%%------------------------------------------------------

expression(((lambda(A,lambda(B,likes(B,A)))@mary)@john),
           likes(john,mary)).

%%-----------------------------------------------------------------------
%% Reductions in a nested position
%%    p((Lx.sleeps(X))john) --> p(sleeps(john))
%%---------------------------------------------

expression(p((lambda(A,sleeps(A))@john)),
           p(sleeps(john))).

%%-----------------------------------------------------------------------
%% Reductions inside a variable predicate
%%    LP. P((Lx.sleeps(x)) john) --> LP. P(sleeps(john))
%%------------------------------------------------------

expression(lambda(A,(A@(lambda(B,sleeps(B))@john))),
           lambda(A,(A@sleeps(john)))).

%%-----------------------------------------------------------------------
%% No reductions possible
%%    LP. P(sleeps(john)) --> LP. P(sleeps(john))
%%------------------------------------------------------

expression(lambda(A,(A@sleeps(john))),
           lambda(A,(A@sleeps(john)))).

%%-----------------------------------------------------------------------
%% Nested reductions (apply to a function which must be applied again)
%%  LP. (P(john)) (Lx.sleeps(x)) --> sleeps(john)
%%-----------------------------------------------

expression((lambda(A,(A@john))@lambda(B,sleeps(B))),
           sleeps(john)).

%%---------------------------------------------------------------------------
%%  LP. believes(mary,(P(john)) (Lx.sleeps(x)) --> believes(mary,sleeps(john))
%%---------------------------------------------------------------------------

expression((lambda(A,believes(mary,(A@john)))@lambda(B,sleeps(B))),
           believes(mary,sleeps(john))).

%%---------------------------------------------------------------------------
%% Copied functions
%% (LP. P*john and P*mary) Lx.sleeps(x) --> sleeps(john) and sleeps(mary)
%%-----------------------------------------------------------------------

expression((lambda(A,((A@john) & (A@mary)))@lambda(B,sleeps(B))),
           (sleeps(john) & sleeps(mary))).

%%------------------------------------------------------------------------
%%   (LP.(LQ.and(LR.R(P) prob), LR.R(Q) improb) (Lx.walks(x)) (Lx.talks(X)) 
%%      -> and(prob(walks(john)), improb(talks(mary)))   
%%------------------------------------------------------

expression(((lambda(A,lambda(B,((lambda(C,(C@(A@john)))@lambda(D,probably(D))) & (lambda(C,(C@(B@mary)))@lambda(D,improbably(D))))))@lambda(E,walks(E)))@lambda(E,talks(E))),
           (probably(walks(john)) & improbably(talks(mary)))).


%%---------------------------------------------------------------------------
%% Double application. Compare with 8f
%%
%% (Lu.(Lv . (LP.P(u,v) Ly.Lx.love(x,y)) mary) peter) --> love(peter,mary)
%%--------------------------------------------------------------------

expression(((lambda(A,lambda(B,(lambda(C,((C@A)@B))@lambda(D,lambda(E,love(D,E))))))@peter)@mary),
           love(peter,mary)).

%%---------------------------------------------------------------------------
%% Two functions with the same use of variable.
%% ((LP.LQ. Ex.(P*x and Q*x))) Ly.student(y)) Ly.sleeps(y) --> 
%%                  Ex(student(x) and sleeps(x))
%%----------------------------------------------

expression(((lambda(A,lambda(B,exists(C,((A@C) & (B@C)))))@lambda(D,student(D)))@lambda(D,sleeps(D))),
           exists(C,(student(C) & sleeps(C)))).

%%------------------------------------------------------------------------
%% Test for correctly dealing with the same variable name occurring twice
%%    Lx(P(x)) (Ly.Lx.likes(x,y)) --> P(Lx.Lz.likes(z,x))
%%-------------------------------------------------------

expression((lambda(A,(_@A))@lambda(C,lambda(A,likes(A,C)))),
           (_@lambda(E,lambda(F,likes(F,E))))).

%%------------------------------------------------------------------------
%% Test for correctly performing alpha conversion
%%   LP.Lx(P(x)) (Ly.Lx.likes(x,y)) --> Lx.Lz.likes(z,x)
%%----------------------------------------------------

expression((lambda(A,lambda(B,(A@B)))@lambda(C,lambda(B,likes(B,C)))),
           lambda(D,lambda(E,likes(E,D)))).

%%---------------------------------------------------------------------------
%% Test for correctly performing alpha conversion
%% (Lx.(Ly. (LP.P(x,y) Ly.Lx.love(x,y)) mary) peter) --> love(peter,mary)
%%--------------------------------------------------------------------

expression(((lambda(A,lambda(B,(lambda(C,((C@A)@B))@lambda(B,lambda(A,love(B,A))))))@peter)@mary),
           love(peter,mary)).

%%---------------------------------------------------------------------------
%% Test for correctly performing alpha conversion
%% (Lx.(Ly. (LP.P(x,y) Ly.Lx.love(x,y)))) --> Lu.Lv. love(u,v)
%%--------------------------------------------------------------------

expression(lambda(A,lambda(B,(lambda(C,((C@A)@B))@lambda(B,lambda(A,love(B,A)))))),
           lambda(D,lambda(E,love(D,E)))).

%%---------------------------------------------------------------------------
%% Further alpha conversion testing
%% (LP. Lx. (Ex.P*x) and P*x) Ly.student(y) john
%%----------------------------------------------

expression(((lambda(A,lambda(B,(exists(B,(A@B)) & (A@B))))@lambda(C,student(C)))@john),
           (exists(B,student(B)) & student(john))).
