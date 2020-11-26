
/*************************************************************************

         name: propTestSuite.pl
      version: May 19, 2000
  description: Test Suite with Propositional Formulas for Provers 
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(propTestSuite,[formula/2]).

:- ensure_loaded(comsemOperators).


/*========================================================================
   Formulas
========================================================================*/

formula(p>q,'Not a theorem').

formula(p>p,'Theorem').

formula(((p>q)>r) >((p >q) >(p>r)),'Theorem').

formula(p>(q>p),'Theorem').

formula((p>q)>(~q > ~p),'Theorem').

formula((~ p > ~ q) > (q > p),'Theorem').

formula((~ ~ p > p),'Theorem').

formula((p > ~ ~p),'Theorem').

formula((~ ~ ~ ~p > p),'Theorem').

formula((p > ~ ~ ~ ~ p),'Theorem').

formula(~(p v q) > (~ p & ~ q),'Theorem').

formula((~ p & ~ q) > ~(p v q),'Theorem').

formula(~(p & q) > (~p v ~q),'Theorem').

formula((~p & ~q) > ~(p v q),'Theorem').

formula((r v (p & q)) > ((r v p) & (r v q)),'Theorem').

formula(((r v p) & (r v q)) > (r v (p & q)),'Theorem').

formula((r & (p v q)) > ((r & p) v (r & q)),'Theorem').

formula(((r & p) v (r & q)) > (r & (p v q)),'Theorem').

formula(((p>r) & (q>s)) > ((p&q) >(r&s)),'Theorem').

formula(p > (p v q),'Theorem').

formula(p > (q v p),'Theorem').

formula((p & q) > p,'Theorem').

formula((p & q) > q,'Theorem').

formula(( p v ~q) > (p v q),'Not a theorem').

formula(((p v ~p)  & ( p v ~q)) > (p v q),'Not a theorem').

formula(((p v ~p) & (r v ~r)  & ( p v ~q)) > (p v q),'Not a theorem').

formula((p v (~p & q)) > (p v q),'Theorem').

formula(((p v (~p & q)) > (p v q)) & ((p v q) > (p v (~p & q))),'Theorem').


formula(

((p11 v p12) & (p21 v p22) & (p31 v p32))
>
(
(p11 & p21) v (p11 & p31) v (p21 & p31) v
(p12 & p22) v (p12 & p32) v (p22 & p32) 
)
         ,
         'Theorem').


formula(

((p11 v p12 v p13) & (p21 v p22 v p23) & (p31 v p32 v p33) & (p41 v p42 v p43))
>
(
(p11 & p21) v (p11 & p31) v (p11 & p41) v (p21 & p31) v (p21 & p41) v (p31 & p41) v
(p12 & p22) v (p12 & p32) v (p12 & p42) v (p22 & p32) v (p22 & p42) v (p32 & p42) v
(p13 & p23) v (p13 & p33) v (p13 & p43) v (p23 & p33) v (p23 & p43) v (p33 & p43) 
)
         ,
         'Theorem').

