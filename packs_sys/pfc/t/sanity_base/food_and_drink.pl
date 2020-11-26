/*

Joe is takin Sue on a date but he doesnt have enough money to buy them both food and drink

Joe wants food .66 more than anyhting
Sue is on a diet so she wants food at .05 (so must be something else on menu?)

*/
:- use_module(library(clpfd)).
:- style_check(-discontiguous).
clause(P):-clause(P,true).

cost(food,35).
cost(drink,15).

buyable(A):- cost(A,_).
person(P):- wants(P,_,_).

has(joe,money,50).

has(joe,food,0).
has(joe,drink,0).
has(sue,food,0).
has(sue,drink,0).


wants(joe,food,66).
wants(sue,food,5).

% This rule helps us infer sue wants drink at 95 and joe at 34
wants(Person,Type1,Amount):- 
  buyable(Type2),
  clause(wants(Person,Type2,Other)),  
  dif(Type1,Type2),
  buyable(Type1),  
  Amount #= 100 - Other.

wants_more(P,Thing1):- 
   person(P),
   dif(Thing1,Thing2),
   wants(P,Thing1,A1),
   wants(P,Thing2,A2),
   A1 #>= A2.


do_test :-
   dif(P1,P2),
   wants_more(P1,Thing1),
   wants_more(P2,Thing2),
   cost(Thing1,Cost1),
   cost(Thing2,Cost2),
   has(P1,money,Cash),
   Cash #>= Cost1+Cost2,
   Spent is Cost1+Cost2,
   nl,
   write([spent=Spent,
     orders_for(Thing1,P1),
     orders_for(Thing2,P2)]),nl.


:- do_test.
% [spent=50,orders_for(food,joe),orders_for(drink,sue)]


