:- include(test_header).




 

existing_count(X,G,EC):- findall(X,G,List),length(List,EC).

:- debug_logicmoo(_).
:- nodebug_logicmoo(http(_)).
:- begin_pfc.

house(red).
house(blue).
% house(green).

:- must((existing_count(X,house(X),EC),EC==2)).

singleValuedInArg(existing_count,1).

exists_count(3,X,house(X)).

exists_count(N,X,G),{(need_plugs(X,G,EP); EP=0),existing_count(X,G,EC),Need is N-(EC-EP), copy_term(G,GG)}
  ==>
    (need_plugs(Need,X,G),
    (\+ GG ==> exists_count(N,X,G)),
    (   GG ==> exists_count(N,X,G))).

need_plugs(EP,X,G) ==> {between(1,EP,Plug),copy_term(G,GG,_),X=skFn(Plug,GG)},G.

:- listing(exists_count).
:- listing(need_plugs).
:- listing(house).

/*

exists_count(3, A, house(A)).


need_plugs(1, A, house(A)).

house(red).
house(blue).
house(skFn(1, house(_))).

*/



:- must((existing_count(X,house(X),EC),EC==3)).
:- break.

