 /* ========== updated and defined version at 12/7/96 ============ */ 


 /*  initialization  */ 
start:-  maplist(must,[
 backinit, set, generic_class, int, roles_soc, people, 
 objects, places, events, states, processes, charge_sentences]).


 /*  definition of sets  */ 
set :- 
 own_names := aset([john, mary]),
 masfem := aset([male, female]),
 pol := '..'(0 , 1),
special := aset([(in), (on), (under), (near)]).
 

 /*  definition of general class  */ 
generic_class:-  general :< anything.


 /*  temporary interface definition  */ 
int  :- tempo  :< general,
	tloc   :< tempo.


 /*  definition of social roles  */ 
roles_soc:- social_roles :< general.


 /*  definition and personal stages  */ 
people:- man :< general, 
 has_name :< domain(man) and range(own_names),
sex :< domain(man) and range(masfem),
waiter :< man and all(works_in, restaurant).


 /*  definition and objectives  */ 
objects:- thing :< general,
table :< thing and all(location, location),
          corner :< thing and all(spec, special) and all(location, place),
          book :< thing  and all(location, location).



 /*  definition and places stations  */ 
places:- place :< general,
restaurant :< place.

 
 /*  definition and event stands  */ 
events:-  ev :< general and all(time, tempo) and all(polarities, poles),
begin :< ev and all(agent, man) and all(prop, pr),
   go :< ev and all(agent, man) and all(location, location).


 /*  definition and state stations  */ 
states:- 
            st :< general and all(time, tempo),
      there_be :< st and all(theme_nonaff, thing),
have :< st and all(agent, man) and all(theme_aff, thing),
         there_in :< st and all(location, place) and all(goal, general).


 /*  definition and process stations  */ 
processes:- pr :< general and all(time, tempo) ,
 read :< pr and all(agent, man) and all(theme_aff, thing),
 take_order :< pr and all(agent, waiter) and all(goal, man).

 /*  definition of the reading cycle  */ 

charge_phrases:- fatto, fail.
charge_phrases.

 /* ================================================== */ 


fatto:- fact(_, inst_of, [ind:Y, class:man], 1, _, _), Y :: man.


fatto:- fact(_, inst_of, [ind:Y, class:thing], 1, _, _), Y :: thing.


fatto:- fact(_, inst_of, [ind:Y, class:social_roles], 1, _, _), Y :: waiter.



 /* ================================================== */ 


fatto:- fact(_, name, [X, Y], 1, _, _),Y :: ha_name:X.

fatto:- fact(_, roles, [waiter, X, Y], 1, _, _), Y :: works_in:X.


 /* ================================================== */ 


fatto:- fact(_, isa, [ind:X, class:Y], 1, A, Z), X :: Y and time:A and location:Z.


fatto:- fact(_, isa, [arg:X, arg:ev], 1, A, B), X :: ev and time:A and location:B.


fatto:- fact(_, isa, [arg:X, arg:pr], 1, A, _), X :: pr and time:A.


fatto:- fact(_, isa, [arg:X, arg:tloc], 1, _, _), X :: tempo.


fatto:- fact(_, isa, [arg:X, arg:st], 1, A, _), X :: st and time:A.


fatto:- fact(_, isa, [arg:X,arg:Y], 1, A, B), X :: Y and time:A and location:B.

 /* ================================================== */ 


fatto:- fact(X, go, [agent:Z, locat:A], 1, B, _),
  X :: go and agent:Z and location:A and time:B.


fatto:- fact(X, there_be, [theme_nonaff:Z], 1, B, _), X :: there_be and theme_nonaff:Z and time:B.


fatto:- fact(X, take_order, [actor:Z, goal:A], 1, B, _), X :: take_order and agent:Z and goal:A and time:B.


fatto:- fact(X, read, [agent:Z, topic_aff:A], 1, B, _), X :: read and agent:Z and theme_aff:A and time:B.


fatto:- fact(X, start, [agent:Z, prop:A], 1, B, _), X :: begin and agent:Z and prop:A and time:B.


fatto:- fact(_, X, [arg:Y, poss:Z], 1, B, _), X :: have and agent:Z and theme_aff:Y and time:B.


fatto:- fact(_, X, [arg:Y, renting:Z], 1, A, B), Y :: thing and spec:X and locaz:Z and time:A and location:B.



/******************************************************************************/



 /*  Lista completa degli identificativi  */ 

/* id2  = restaurant
 id3 = john
 id4 = go
 id5 = tloc
 id6 = table
 id7 = corner
 id8 = there_be
 id9 = tloc
 id10 = waiter
 id12 = take_order 
 id13 = tloc
 id14 = tloc
 id18 = book
 id19 = read
 id20 = tloc
 id21 = begin
 id22 = tloc
 infon27 = there_in
 infon77 = have 
 */ 

 

 /* queries========================================== */ 

who(X):- backretrieve(getall(X)).
who(X, Y):- backretrieve(X = getall(Y)).
look_for(X):- backretrieve(getall(man and ha_name:X)).

 /* ================================================== */ 

describe(X):- backretrieve(describe X).

 /*  example:

| ?- describe(id3).

>>> id3
describe:

man
and has_name:john
and inv(agent):( poss and id19 and id21 and id4)
and inv(goal):id12
and oneof([id3])


yes
| ?- describe(id4).

>>> id4
describe:

go
and location:id2
and tempo:tes(f5_r01)
and agent:id3
and oneof([id4])


yes
 */ 

 /* ================================================== */ 


 /*  there was a table, there was a book ....  */ 

c_era(X):- backretrieve(getall(X and lease:id2)).

 /*  example:

| ?- c_era(book).
[id18]

yes
| ?- c_era(table).
[id6]

yes
 */ 

 /* ================================================== */ 


 /*  what was in the restaurant  */ 

what_wax(Z):- backretrieve(X = getall(Z)),
member([Y], X),
backretrieve(A = getall(thing and location:Y)),
write(A), nl.

 /*  example:

|?- what_wax(restaurant).
[[id7], [id6], [id18]]

yes
 */ 


 /* ================================================== */ 


 /*  who was in the restaurant  */ 

who_wax(Z):- backretrieve(X = getall(Z)),
member([Y], X),
backretrieve(A = getall(man and location:Y)),
write(A), nl.

 /*  example:

|- who_wax(restaurant).
[[id10]]

yes
 */ 

 /* ================================================== */ 


 /*  who ordered  */ 

who_has_ordered:- backretrieve(X = getall(take_order)),
X = [[Y]],
backretrieve(Z = getall(man and inv(goal):Y)), nl,
write('take_order ='),
write(Y), nl,
write('goal ='),
write(Z), nl.

 /*  example:

| ?- who_has_ordered.

take_order = id12
goal = [[id3]]

yes
 */ 

 /* ================================================ */ 


 /*  who took the order-> syntax:who_has_taken_order  */ 

who_has_taken_order:- backretrieve(X = getall(take_order)),
  X = [[Y]],
  backretrieve(Z = getall(man and inv(agent):Y)), nl,
  write('take_order ='),
  write(Y), nl,
  write('agent ='),
  write(Z), nl.
  
 /*  example:
|?- who_has_taken_order.

take_order = id12
agent = [[id10]]

yes
 */ 

 /* ================================================== */ 


 /*  where is the waiter, john, the table etc ...  */ 

where_and(Z):- backretrieve(X = getall(Z)), 
 X = [[Y]], backretrieve(A = getall(anything and inv(lease):Y)), nl, write('lease ='), write(A), nl.

 /*  example:

|?- and(waiter).

lease = [[id2]]

yes
|?- where(book).

lease = [[id2]]

yes
| ?- where(table).

lease = [[id2]]

yes   */ 


