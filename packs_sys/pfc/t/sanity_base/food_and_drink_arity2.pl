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

% person(P):- wants(P,_,_).
person(P):- pred(Cn,wants),arg1(Cn,P).

% has(joe,money,50).
pred(has1,has). arg1(has1,joe). arg2(has1,money). arg3(has1,50).


% has(joe,food,0).
pred(has2,has). arg1(has2,joe). arg2(has2,food). arg3(has2,0).
% has(joe,drink,0).
pred(has3,has). arg1(has3,joe). arg2(has3,drink). arg3(has3,0).
% has(sue,food,0).
pred(has4,has). arg1(has4,sue). arg2(has4,food). arg3(has4,0).
% has(sue,drink,0).
pred(has5,has). arg1(has5,sue). arg2(has5,drink). arg3(has5,0).


% wants(joe,food,66).
pred(wants1,wants). arg1(wants1,joe). arg2(wants1,food). arg3(wants1,66).
% wants(sue,food,5).
pred(wants2,wants). arg1(wants2,sue). arg2(wants2,food). arg3(wants3,5).

% This rule helps us infer sue wants drink at 95 and joe at 34
% wants(Person,Type,Amount):- dif(Type1,Type2), buyable(Type2), person(Person),wants(Person,Type2,Other), Amount #= 1 - Other.
pred(rule1,wants).
arg1(rule1,Person):- arg2(rule1,Type),arg3(rule1,Amount),rule1_wants(Person,Type,Amount).
arg2(rule1,Type):-   arg1(rule1,Person),arg3(rule1,Amount),rule1_wants(Person,Type,Amount).
arg3(rule1,Amount):- arg2(rule1,Type), arg1(rule1,Person),rule1_wants(Person,Type,Amount).

rule1_wants(Person,Type1,Amount):- 
  buyable(Type2),
  clause(pred(Cn,wants),true),arg1(Cn,Person), arg2(Cn,Type2), clause(arg3(Cn,Other)),  
  dif(Type1,Type2),
  buyable(Type1),  
  Amount #= 100 - Other.


% wants_more(P,Thing1):- dif(Thing1,Thing2), wants(P,Thing1,A1), wants(P,Thing2,A2), A1 #>= A2.
wants_more(P,Thing1):- 
   dif(Thing1,Thing2),
   pred(F1,wants),arg1(F1,P),arg2(F1,Thing1),arg3(F1,A1),
   pred(F2,wants),arg1(F2,P),arg2(F2,Thing2),arg3(F2,A2),
   A1 #>= A2.


do_test :-
   dif(P1,P2),
   wants_more(P1,Thing1),
   wants_more(P2,Thing2),
   cost(Thing1,Cost1),
   cost(Thing2,Cost2),
   pred(F1,has),arg1(F1,P1),arg2(F1,cash),arg3(F1,Cash),
   Cash #>= Cost1+Cost2,
   Spent is Cost1+Cost2,
   nl,
   write([spent=Spent,
     orders_for(Thing1,P1),
     orders_for(Thing2,P2)]),nl.


:- do_test.
% [spent=50,orders_for(food,joe),orders_for(drink,sue)]

