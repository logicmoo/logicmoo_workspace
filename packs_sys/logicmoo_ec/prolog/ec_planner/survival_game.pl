
:- use_module(library(logicmoo_common)).
:- use_module(library(lps_corner)).
:- expects_dialect(lps).


/* Fluents regarding units */
fluent(person(_, _)).
fluent(position(_, _, _, _)).
fluent(health(_, _)).
fluent(hunger(_, _)).
fluent(holds_wood(_,_)).
fluent(holds_food(_, _, _, _, _)).
fluent(has_shelter(_,_)).
fluent(shelter(_,_,_,_)).
fluent(turns(_,_)).
fluent(dead(_)).

/* Inanimate object fluents */
fluent(tree(_,_)).
fluent(wood(_,_)).
fluent(animal(_, _, _, _, _)).
fluent(food(_, _, _, _, _)).

/* Cycles */
fluent(cycles(_)).

/* Start game event */
event(start_game(_)).



initial_state([
	
	/* The people in the game */
	person(amanda, cautious),
	
	person(katherine, normal),
	
	person(peter, normal),
	
	person(tom, normal),

	person(alex, violent),

	
	/* Defining the information for Amanda */
	position(amanda, north, 3, 3),
	holds_wood(amanda, 0),
	holds_food(amanda, 0, 0, 0, 0),
	health(amanda, 50),
	hunger(amanda, 20), 
	has_shelter(amanda, true),
	shelter(amanda, 100, 4, 5),
	turns(amanda, 0),
	
	/* Defining the information for Katherine */
	position(katherine, east, 4, 18),
	holds_wood(katherine, 0),
	holds_food(katherine, 0, 0, 0, 0),
	health(katherine, 50),
	hunger(katherine, 20),
	has_shelter(katherine, true),
	shelter(katherine, 100, 8, 19),
	turns(katherine, 0),

	/* Defining the information for Peter */
	position(peter, north, 17, 3),
	holds_wood(peter, 0),
	holds_food(peter, 0, 0, 0, 0),
	health(peter, 50),
	hunger(peter, 20),
	has_shelter(peter, true),
	shelter(peter, 100, 13, 4),
	turns(peter, 0),

	/* Defining the information for Tom */
	position(tom, west, 19, 18),
	holds_wood(tom, 0),
	holds_food(tom, 0, 0, 0, 0),
	health(tom, 50),
	hunger(tom, 20),
	has_shelter(tom, true),
	shelter(tom, 100, 14, 20),
	turns(tom, 0),

	/* Defining the information for Alex */
	position(alex, north, 10, 9),
	holds_wood(alex, 0),
	holds_food(alex, 10, 0, 0, 2),
	health(alex, 50),
	hunger(alex, 20),
	has_shelter(alex, false),
	turns(alex, 0),

	/* Defining locations for animals */
	animal(rabbit, 5, west, 5, 7),
	animal(rabbit, 5, east, 12, 8),
	animal(rabbit, 5, west, 4, 9),
	animal(rabbit, 5, south, 1, 10),
	animal(rabbit, 5, west, 6, 13),
	animal(rabbit, 5, north, 14, 16),
	animal(rabbit, 5, north, 9, 16),

	animal(chicken, 10, north, 7, 6),
	animal(chicken, 10, east, 16, 6),
	animal(chicken, 10, east, 8, 8),
	animal(chicken, 10, west, 2, 13),
	animal(chicken, 10, east, 3, 15),
	animal(chicken, 10, south, 12, 15),
	animal(chicken, 10, south, 13, 18),

	animal(cow, 15, north, 9, 3),
	animal(cow, 15, north, 18, 8),
	animal(cow, 15, south, 17, 13),
	animal(cow, 15, west, 6, 16),

	/* Defining locations for food */
	food(2, 1, 0, 3, 8),
	food(2, 1, 1, 15, 9),
	food(2, 1, 2, 20, 9),
	food(2, 1, 0, 8, 14),

	/* Defining locations for trees */
	tree(1, 1),
	tree(4, 1),
	tree(11, 1),
	tree(18, 1),
	tree(2, 2),
	tree(13, 2),
	tree(6, 3),
	tree(19, 4),
	tree(1, 5),
	tree(11, 6),
	tree(13, 7),
	tree(12, 10),
	tree(8, 11),
	tree(11, 12),
	tree(14, 12),
	tree(19, 14),
	tree(1, 15),
	tree(3, 17),
	tree(1, 19),
	tree(6, 19),
	tree(4, 20),
	tree(8, 20),
	tree(17, 20),
	tree(20, 20),

	/* Defining locations for food */
	wood(7, 1),
	wood(4, 11),
	wood(18, 11),
	wood(10, 14),

	/* Cycles start at zero */
	cycles(0)
]).


/**************************************** l_timeless' for universal use ****************************************/

/* Defining weapon information for Amanda */

l_timeless(has(amanda, weapon(spear)), []).
l_timeless(power(weapon(spear), 6), []).
l_timeless(range(weapon(spear), 1, 3),[]).

/* Defining weapon information for Katherine */

l_timeless(has(katherine, weapon(bow)),[]).
l_timeless(power(weapon(bow), 6),[]).
l_timeless(range(weapon(bow), 1, 6),[]).

/* Defining weapon information for Peter */

l_timeless(has(peter, weapon(bombs)),[]).
l_timeless(power(weapon(bombs), 8),[]).
l_timeless(range(weapon(bombs), 6, 6),[]).

/* Defining weapon information for Tom */

l_timeless(has(tom, weapon(whip)),[]).
l_timeless(power(weapon(whip), 4),[]).
l_timeless(range(weapon(whip), 1, 3),[]).

/* Defining weapon information for Alex */

l_timeless(has(alex, weapon(sword)),[]).
l_timeless(power(weapon(sword), 8),[]).
l_timeless(range(weapon(sword), 1, 1),[]).


/* Comparators */

l_timeless(less_than(X, Y), [X < Y]).
l_timeless(less_or_equal(X, Y), [X =< Y]).
l_timeless(greater_or_equal(X, Y), [X >= Y]).
l_timeless(greater_than(X, Y), [X > Y]).
l_timeless(equal(X, Y), [X == Y]).
l_timeless(not_equal(X, Y), [X \= Y]).

/* Mathematical usuage */

l_timeless(decrement(X, X1), [X1 is X - 1]).
l_timeless(increment(X, X1), [X1 is X + 1]).
l_timeless(decrease(X, X1, N), [X1 is X - N]).
l_timeless(increase(X, X1, N), [X1 is X + N]).
l_timeless(calculate(N, A, B, C), [N is A * 1 + B * 3 + C * 5]).
l_timeless(modu(X, X1), [X is mod(X1, 8)]).

/* Defining the opposite compass direction */

l_timeless(opposite(north, south), []).
l_timeless(opposite(east, west), []).
l_timeless(opposite(south, north), []).
l_timeless(opposite(west, east), []).

/**************************************** l_int's for universal use ****************************************/


/* Different cases to determine whether a unit is next to a specified object depending on the unit's direction */

l_int(
	holds(next_to(Unit, Type), T),
	[
		holds(position(Unit, south, X, Y), T),
		increment(Y, Y1),
		holds(item(Type, X, Y1), T)
	]

).


l_int(
	holds(next_to(Unit, Type), T),
	[
		holds(position(Unit, north, X, Y), T),
		decrement(Y, Y1),
		holds(item(Type, X, Y1), T)
	]

).

l_int(
	holds(next_to(Unit, Type), T),
	[
		holds(position(Unit, east, X, Y), T),
		increment(X, X1),
		holds(item(Type, X1, Y), T)
	]

).

l_int(
	holds(next_to(Unit, Type), T),
	[
		holds(position(Unit, west, X, Y), T),
		decrement(X, X1),
		holds(item(Type, X1, Y), T)
	]

).



/* Different cases for if a unit is in sight of a specified item depending on depending on the unit's direction */

l_int(
	holds(in_sight(Unit, Type), T),
	[
		holds(position(Unit, east, X, Y), T),
		holds(item(Type, A, Y), T),
		greater_than(A, X),
		increase(X, X1, 6),
		less_or_equal(A, X1)
	]

).

l_int(
	holds(in_sight(Unit, Type), T),
	[
		holds(position(Unit, north, X, Y), T),
		holds(item(Type, X, A), T),
		less_than(A, Y),
		decrease(Y, Y1, 6),
		greater_or_equal(A, Y1)
	]

).

l_int(
	holds(in_sight(Unit, Type), T),
	[
		holds(position(Unit, south, X, Y), T),
		holds(item(Type, X, A), T),
		greater_than(A, Y),
		increase(Y, Y1, 6),
		less_or_equal(A, Y1)
	]

).


l_int(
	holds(in_sight(Unit, Type), T),
	[
		holds(position(Unit, west, X, Y), T),
		holds(item(Type, A, Y), T),
		less_than(A, X),
		decrease(X, X1, 6),
		greater_or_equal(A, X1)
	]

).

/* Defining the different items, used to determine if an item is of a certain type */

l_int(
	holds(item(tree, X, Y), T),
	[
		holds(tree(X,Y), T)
	]

).

l_int(
	holds(item(tree, X, Y), T),
	[
		holds(wood(X,Y), T)
	]

).

l_int(
	holds(item(animal, X, Y), T),
	[
		holds(animal(Type, N, D, X, Y), T)
	]
).

l_int(
	holds(item(animal, X, Y), T),
	[
		holds(food(A, B, C, X, Y), T)
	]

).

l_int(
	holds(item(food, X, Y), T),
	[
		holds(food(A, B, C, X, Y), T)
	]

).

l_int(
	holds(item(person, X, Y), T),
	[
		holds(position(P, D, X, Y), T)
	]
).

l_int(
	holds(item(wood, X, Y), T),
	[
		holds(wood(X, Y), T)
	]
).

/* Different cases determining if something is in view, depending on the unit's position */

l_int(
	holds(in_view(Unit, east, Type), T),
	[
		holds(position(Unit, D, X, Y), T),
		holds(item(Type, A, Y), T),
		greater_than(A, X),
		increase(X, X1, 6),
		less_or_equal(A, X1)
	]

).

l_int(
	holds(in_view(Unit, north, Type), T),
	[
		holds(position(Unit, D, X, Y), T),
		holds(item(Type, X, A), T),
		less_than(A, Y),
		decrease(Y, Y1, 6),
		greater_or_equal(A, Y1)
	]

).

l_int(
	holds(in_view(Unit, south, Type), T),
	[
		holds(position(Unit, D, X, Y), T),
		holds(item(Type, X, A), T),
		greater_than(A, Y),
		increase(Y, Y1, 6),
		less_or_equal(A, Y1)
	]

).


l_int(
	holds(in_view(Unit, west, Type), T),
	[
		holds(position(Unit, D, X, Y), T),
		holds(item(Type, A, Y), T),
		less_than(A, X),
		decrease(X, X1, 6),
		greater_or_equal(A, X1)
	]

).

/* Different cases determining if something is in range, depending on the unit's position */

l_int(
	holds(in_range(Unit, Unit1, north, A), T1),
	[
		holds(position(Unit, D, X, Y), T1),
		holds(position(Unit1, D1, X, Y1), T1),
		not_equal(Unit, Unit1),
		range(weapon(A), L, U),
		decrease(Y, Y2, Y1),
		greater_or_equal(Y2, L),
		less_or_equal(Y2, U)
	]
).

l_int(
	holds(in_range(Unit, Unit1, east, A), T1),
	[
		holds(position(Unit, D, X, Y), T1),
		holds(position(Unit1, D1, X1, Y), T1),
		not_equal(Unit, Unit1),
		range(weapon(A), L, U),
		decrease(X1, X2, X),
		greater_or_equal(X2, L),
		less_or_equal(X2, U)
	]
).

l_int(
	holds(in_range(Unit, Unit1, south, A), T1),
	[
		holds(position(Unit, D, X, Y), T1),
		holds(position(Unit1, D1, X, Y1), T1),
		not_equal(Unit, Unit1),
		range(weapon(A), L, U),
		decrease(Y1, Y2, Y),
		greater_or_equal(Y2, L),
		less_or_equal(Y2, U)

	]
).

l_int(
	holds(in_range(Unit, Unit1, west, A), T1),
	[
		holds(position(Unit, D, X, Y), T1),
		holds(position(Unit1, D1, X1, Y), T1),
		not_equal(Unit, Unit1),
		range(weapon(A), L, U),
		decrease(X, X2, X1),
		greater_or_equal(X2, L),
		less_or_equal(X2, U)
	]
).


l_int(
	holds(in_range(Unit, animal, north, A), T1),
	[
		holds(position(Unit, D, X, Y), T1),
		holds(animal(Type, H, D1, X, Y1), T1),
		range(weapon(A), L, U),
		decrease(Y, Y2, Y1),
		greater_or_equal(Y2, L),
		less_or_equal(Y2, U)
	]
).

l_int(
	holds(in_range(Unit, animal, east, A), T1),
	[
		holds(position(Unit, D, X, Y), T1),
		holds(animal(Type, H, D1, X1, Y), T1),
		range(weapon(A), L, U),
		decrease(X1, X2, X),
		greater_or_equal(X2, L),
		less_or_equal(X2, U)
	]
).

l_int(
	holds(in_range(Unit, animal, south, A), T1),
	[
		holds(position(Unit, D, X, Y), T1),
		holds(animal(Type, H, D1, X, Y1), T1),
		range(weapon(A), L, U),
		decrease(Y1, Y2, Y),
		greater_or_equal(Y2, L),
		less_or_equal(Y2, U)

	]
).

l_int(
	holds(in_range(Unit, animal, west, A), T1),
	[
		holds(position(Unit, D, X, Y), T1),
		holds(animal(Type, H, D1, X1, Y), T1),
		range(weapon(A), L, U),
		decrease(X, X2, X1),
		greater_or_equal(X2, L),
		less_or_equal(X2, U)
	]
).


/********************************************* Preconditions *********************************************/

/* Preconditions to ensure a unit cannot do to actions at once */

d_pre([happens(turn(Unit, N, D, X, Y), T1, T2), happens(turn(Unit, N1, D1, X1, Y1), T1, T2)]).

d_pre([happens(turn(Unit, N, D, X, Y), T1, T2), happens(walk(Unit, D1, X1, Y1), T1, T2)]).

d_pre([happens(turn(Unit, N, D, X, Y), T1, T2), happens(successful_shelter(Unit, N1, X1, Y1), T1, T2)]).

d_pre([happens(turn(Unit, N, D, X, Y), T1, T2), happens(break_tree(Unit, X1, Y1), T1, T2)]).

d_pre([happens(turn(Unit, N, D, X, Y), T1, T2), happens(collect_wood(Unit, W, X1, Y1), T1, T2)]).

d_pre([happens(turn(Unit, N, D, X, Y), T1, T2), happens(change_animal(Unit, N1, D1, X1, Y1, N2, A), T1, T2)]).

d_pre([happens(turn(Unit, N, D, X, Y), T1, T2), happens(collect_food(Unit, A, B, C, K, L, M, X1, Y1), T1, T2)]).

d_pre([happens(turn(Unit, N, D, X, Y), T1, T2), happens(hit_shelter(Unit, Unit1, H, D1, X1, Y1), T1, T2)]).

d_pre([happens(turn(Unit, N, D, X, Y), T1, T2), happens(hit_from(Unit, Unit1, H, D1), T1, T2)]).

d_pre([happens(turn(Unit, N, D, X, Y), T1, T2), happens(turn_hit(Unit, D1, X1, Y1, Unit1, H, D2), T1, T2)]).

d_pre([happens(turn(Unit, N, D, X, Y), T1, T2), happens(turn_hit_shelter(Unit, D1, X1, Y1, Unit1, H, D2, X2, Y2), T1, T2)]).

d_pre([happens(turn(Unit, N, D, X, Y), T1, T2), happens(eat(Unit, V, F, N1, A, B, C), T1, T2)]).



d_pre([happens(walk(Unit, D, X, Y), T1, T2), happens(walk(Unit, D1, X1, Y1), T1, T2)]).

d_pre([happens(walk(Unit, D, X, Y), T1, T2), happens(successful_shelter(Unit, N1, X1, Y1), T1, T2)]).

d_pre([happens(walk(Unit, D, X, Y), T1, T2), happens(break_tree(Unit, X1, Y1), T1, T2)]).

d_pre([happens(walk(Unit, D, X, Y), T1, T2), happens(collect_wood(Unit, W, X1, Y1), T1, T2)]).

d_pre([happens(walk(Unit, D, X, Y), T1, T2), happens(change_animal(Unit, N1, D1, X1, Y1, N2, A), T1, T2)]).

d_pre([happens(walk(Unit, D, X, Y), T1, T2), happens(collect_food(Unit, A, B, C, K, L, M, X1, Y1), T1, T2)]).

d_pre([happens(walk(Unit, D, X, Y), T1, T2), happens(hit_shelter(Unit, Unit1, H, D1, X1, Y1), T1, T2)]).

d_pre([happens(walk(Unit, D, X, Y), T1, T2), happens(hit_from(Unit, Unit1, H, D1), T1, T2)]).

d_pre([happens(walk(Unit, D, X, Y), T1, T2), happens(turn_hit(Unit, D1, X1, Y1, Unit1, H, D2), T1, T2)]).

d_pre([happens(walk(Unit, D, X, Y), T1, T2), happens(turn_hit_shelter(Unit, D1, X1, Y1, Unit1, H, D2, X2, Y2), T1, T2)]).

d_pre([happens(walk(Unit, D, X, Y), T1, T2), happens(eat(Unit, V, F, N1, A, B, C), T1, T2)]).



d_pre([happens(successful_shelter(Unit, N, X, Y), T1, T2), happens(successful_shelter(Unit, N1, X1, Y1), T1, T2)]).

d_pre([happens(successful_shelter(Unit, N, X, Y), T1, T2), happens(break_tree(Unit, X1, Y1), T1, T2)]).

d_pre([happens(successful_shelter(Unit, N, X, Y), T1, T2), happens(collect_wood(Unit, W, X1, Y1), T1, T2)]).

d_pre([happens(successful_shelter(Unit, N, X, Y), T1, T2), happens(change_animal(Unit, N1, D1, X1, Y1, N2, A), T1, T2)]).

d_pre([happens(successful_shelter(Unit, N, X, Y), T1, T2), happens(collect_food(Unit, A, B, C, K, L, M, X1, Y1), T1, T2)]).

d_pre([happens(successful_shelter(Unit, N, X, Y), T1, T2), happens(hit_shelter(Unit, Unit1, H, D1, X1, Y1), T1, T2)]).

d_pre([happens(successful_shelter(Unit, N, X, Y), T1, T2), happens(hit_from(Unit, Unit1, H, D1), T1, T2)]).

d_pre([happens(successful_shelter(Unit, N, X, Y), T1, T2), happens(turn_hit(Unit, D1, X1, Y1, Unit1, H, D2), T1, T2)]).

d_pre([happens(successful_shelter(Unit, N, X, Y), T1, T2), happens(turn_hit_shelter(Unit, D1, X1, Y1, Unit1, H, D2, X2, Y2), T1, T2)]).

d_pre([happens(successful_shelter(Unit, N, X, Y), T1, T2), happens(eat(Unit, V, F, N1, A, B, C), T1, T2)]).



d_pre([happens(break_tree(Unit, X, Y), T1, T2), happens(break_tree(Unit, X1, Y1), T1, T2)]).

d_pre([happens(break_tree(Unit, X, Y), T1, T2), happens(collect_wood(Unit, W, X1, Y1), T1, T2)]).

d_pre([happens(break_tree(Unit, X, Y), T1, T2), happens(change_animal(Unit, N1, D1, X1, Y1, N2, A), T1, T2)]).

d_pre([happens(break_tree(Unit, X, Y), T1, T2), happens(collect_food(Unit, A, B, C, K, L, M, X1, Y1), T1, T2)]).

d_pre([happens(break_tree(Unit, X, Y), T1, T2), happens(hit_shelter(Unit, Unit1, H, D1, X1, Y1), T1, T2)]).

d_pre([happens(break_tree(Unit, X, Y), T1, T2), happens(hit_from(Unit, Unit1, H, D1), T1, T2)]).

d_pre([happens(break_tree(Unit, X, Y), T1, T2), happens(turn_hit(Unit, D1, X1, Y1, Unit1, H, D2), T1, T2)]).

d_pre([happens(break_tree(Unit, X, Y), T1, T2), happens(turn_hit_shelter(Unit, D1, X1, Y1, Unit1, H, D2, X2, Y2), T1, T2)]).

d_pre([happens(break_tree(Unit, X, Y), T1, T2), happens(eat(Unit, V, F, N1, A, B, C), T1, T2)]).



d_pre([happens(collect_wood(Unit, W, X, Y), T1, T2), happens(collect_wood(Unit, W1, X1, Y1), T1, T2)]).

d_pre([happens(collect_wood(Unit, W, X, Y), T1, T2), happens(change_animal(Unit, N1, D1, X1, Y1, N2, A), T1, T2)]).

d_pre([happens(collect_wood(Unit, W, X, Y), T1, T2), happens(collect_food(Unit, A, B, C, K, L, M, X1, Y1), T1, T2)]).

d_pre([happens(collect_wood(Unit, W, X, Y), T1, T2), happens(hit_shelter(Unit, Unit1, H, D1, X1, Y1), T1, T2)]).

d_pre([happens(collect_wood(Unit, W, X, Y), T1, T2), happens(hit_from(Unit, Unit1, H, D1), T1, T2)]).

d_pre([happens(collect_wood(Unit, W, X, Y), T1, T2), happens(turn_hit(Unit, D1, X1, Y1, Unit1, H, D2), T1, T2)]).

d_pre([happens(collect_wood(Unit, W, X, Y), T1, T2), happens(turn_hit_shelter(Unit, D1, X1, Y1, Unit1, H, D2, X2, Y2), T1, T2)]).

d_pre([happens(collect_wood(Unit, W, X, Y), T1, T2), happens(eat(Unit, V, F, N1, A, B, C), T1, T2)]).



d_pre([happens(change_animal(Unit, N, D, X, Y, N1, A), T1, T2), happens(change_animal(Unit, N2, D1, X1, Y1, N3, A1), T1, T2)]).

d_pre([happens(change_animal(Unit, N, D, X, Y, N1, A), T1, T2), happens(collect_food(Unit, A1, B, C, K, L, M, X1, Y1), T1, T2)]).

d_pre([happens(change_animal(Unit, N, D, X, Y, N1, A), T1, T2), happens(hit_shelter(Unit, Unit1, H, D1, X1, Y1), T1, T2)]).

d_pre([happens(change_animal(Unit, N, D, X, Y, N1, A), T1, T2), happens(hit_from(Unit, Unit1, H, D1), T1, T2)]).

d_pre([happens(change_animal(Unit, N, D, X, Y, N1, A), T1, T2), happens(turn_hit(Unit, D1, X1, Y1, Unit1, H, D2), T1, T2)]).

d_pre([happens(change_animal(Unit, N, D, X, Y, N1, A), T1, T2), happens(turn_hit_shelter(Unit, D1, X1, Y1, Unit1, H, D2, X2, Y2), T1, T2)]).

d_pre([happens(change_animal(Unit, N, D, X, Y, N1, A), T1, T2), happens(eat(Unit, V, F, N2, A1, B, C), T1, T2)]).



d_pre([happens(collect_food(Unit, A, B, C, K, L, M, X, Y), T1, T2), happens(collect_food(Unit, A1, B1, C1, K1, L1, M1, X1, Y1), T1, T2)]).

d_pre([happens(collect_food(Unit, A, B, C, K, L, M, X, Y), T1, T2), happens(hit_shelter(Unit, Unit1, H, D1, X1, Y1), T1, T2)]).

d_pre([happens(collect_food(Unit, A, B, C, K, L, M, X, Y), T1, T2), happens(hit_from(Unit, Unit1, H, D1), T1, T2)]).

d_pre([happens(collect_food(Unit, A, B, C, K, L, M, X, Y), T1, T2), happens(turn_hit(Unit, D1, X1, Y1, Unit1, H, D2), T1, T2)]).

d_pre([happens(collect_food(Unit, A, B, C, K, L, M, X, Y), T1, T2), happens(turn_hit_shelter(Unit, D1, X1, Y1, Unit1, H, D2, X2, Y2), T1, T2)]).

d_pre([happens(collect_food(Unit, A, B, C, K, L, M, X, Y), T1, T2), happens(eat(Unit, V, F, N1, A1, B1, C1), T1, T2)]).



d_pre([happens(hit_shelter(Unit, Unit1, H, D, X, Y), T1, T2), happens(hit_shelter(Unit, Unit2, H2, D2, X2, Y2), T1, T2)]).

d_pre([happens(hit_shelter(Unit, Unit1, H, D, X, Y), T1, T2), happens(hit_from(Unit, Unit2, H1, D1), T1, T2)]).

d_pre([happens(hit_shelter(Unit, Unit1, H, D, X, Y), T1, T2), happens(turn_hit(Unit, D1, X1, Y1, Unit2, H2, D2), T1, T2)]).

d_pre([happens(hit_shelter(Unit, Unit1, H, D, X, Y), T1, T2), happens(turn_hit_shelter(Unit, D1, X1, Y1, Unit2, H2, D2, X2, Y2), T1, T2)]).

d_pre([happens(hit_shelter(Unit, Unit1, H, D, X, Y), T1, T2), happens(eat(Unit, V, F, N1, A1, B1, C1), T1, T2)]).



d_pre([happens(hit_from(Unit, Unit1, H, D), T1, T2), happens(hit_from(Unit, Unit2, H2, D2), T1, T2)]).

d_pre([happens(hit_from(Unit, Unit1, H, D), T1, T2), happens(turn_hit(Unit, D1, X1, Y1, Unit2, H2, D2), T1, T2)]).

d_pre([happens(hit_from(Unit, Unit1, H, D), T1, T2), happens(turn_hit_shelter(Unit, D1, X1, Y1, Unit2, H2, D2, X2, Y2), T1, T2)]).

d_pre([happens(hit_from(Unit, Unit1, H, D), T1, T2), happens(eat(Unit, V, F, N1, A1, B1, C1), T1, T2)]).



d_pre([happens(turn_hit(Unit, D, X, Y, Unit1, H1, D1), T1, T2), happens(turn_hit(Unit, D2, X2, Y2, Unit3, H3, D3), T1, T2)]).

d_pre([happens(turn_hit(Unit, D, X, Y, Unit1, H1, D1), T1, T2), happens(turn_hit_shelter(Unit, D1, X1, Y1, Unit2, H2, D2, X2, Y2), T1, T2)]).

d_pre([happens(turn_hit(Unit, D, X, Y, Unit1, H1, D1), T1, T2), happens(eat(Unit, V, F, N1, A1, B1, C1), T1, T2)]).



d_pre([happens(turn_hit_shelter(Unit, D, X, Y, Unit1, H1, D1, X1, Y1), T1, T2), happens(turn_hit_shelter(Unit, D2, X2, Y2, Unit3, H3, D3, X3, Y3), T1, T2)]).

d_pre([happens(turn_hit_shelter(Unit, D, X, Y, Unit1, H1, D1, X1, Y1), T1, T2), happens(eat(Unit, V, F, N1, A1, B1, C1), T1, T2)]).



d_pre([happens(eat(Unit, V, F, N, A, B, C), T1, T2), happens(eat(Unit, V1, F1, N1, A1, B1, C1), T1, T2)]).

/* Preconditions to ensure a two different units performing an action that would clash */

d_pre([happens(walk(Unit, D, X, Y), T1, T2), happens(walk(Unit1, D1, X, Y), T1, T2), not_equal(Unit, Unit1)]).

d_pre([happens(walk(Unit, D, X, Y), T1, T2), happens(successful_shelter(Unit1, N1, X, Y), T1, T2), not_equal(Unit, Unit1)]).

d_pre([happens(walk(Unit, D, X, Y), T1, T2), happens(turn_hit(Unit1, D1, X, Y, Unit2, H2, D2), T1, T2), not_equal(Unit, Unit1)]).

d_pre([happens(walk(Unit, D, X, Y), T1, T2), happens(turn_hit_shelter(Unit1, D1, X, Y, Unit2, H, D2, X2, Y2), T1, T2), not_equal(Unit, Unit1)]).



d_pre([happens(successful_shelter(Unit, N, X, Y), T1, T2), happens(successful_shelter(Unit1, N, X, Y), T1, T2), not_equal(Unit, Unit1)]).

d_pre([happens(successful_shelter(Unit, N, X, Y), T1, T2), happens(turn_hit(Unit1, D1, X, Y, Unit2, H, D2), T1, T2), not_equal(Unit, Unit1)]).

d_pre([happens(successful_shelter(Unit, N, X, Y), T1, T2), happens(turn_hit_shelter(Unit1, D1, X, Y, Unit2, H, D2, X2, Y2), T1, T2), not_equal(Unit, Unit1)]).



d_pre([happens(break_tree(Unit, X, Y), T1, T2), happens(break_tree(Unit1, X, Y), T1, T2), not_equal(Unit, Unit1)]).



d_pre([happens(collect_wood(Unit, W, X, Y), T1, T2), happens(collect_wood(Unit1, W1, X, Y), T1, T2), not_equal(Unit, Unit1)]).



d_pre([happens(change_animal(Unit, N, D, X, Y, N1, A), T1, T2), happens(change_animal(Unit1, N2, D2, X, Y, N3, A1), T1, T2), not_equal(Unit, Unit1)]).



d_pre([happens(collect_food(Unit, A, B, C, K, L, M, X, Y), T1, T2), happens(collect_food(Unit1, A1, B1, C1, K1, L1, M1, X, Y), T1, T2), not_equal(Unit, Unit1)]).



d_pre([happens(hit_shelter(Unit, Unit2, H, D, X, Y), T1, T2), happens(hit_shelter(Unit1, Unit3, H1, D1, X, Y), T1, T2), not_equal(Unit, Unit1)]).

d_pre([happens(hit_shelter(Unit, Unit2, H, D, X, Y), T1, T2), happens(turn_hit_shelter(Unit1, D1, X1, Y1, Unit3, H1, D1, X, Y), T1, T2), not_equal(Unit, Unit1)]).



d_pre([happens(hit_from(Unit, Unit2, H, D), T1, T2), happens(hit_from(Unit1, Unit2, H1, D1), T1, T2), not_equal(Unit, Unit1)]).

d_pre([happens(hit_from(Unit, Unit2, H, D), T1, T2), happens(turn_hit(Unit1, D1, X1, Y1, Unit2, H2, D2), T1, T2), not_equal(Unit, Unit1)]).



d_pre([happens(turn_hit(Unit, D, X, Y, Unit2, H2, D2), T1, T2), happens(turn_hit(Unit1, D1, X1, Y1, Unit2, H3, D3), T1, T2), not_equal(Unit, Unit1)]).

d_pre([happens(turn_hit(Unit, D, X, Y, Unit2, H2, D2), T1, T2), happens(turn_hit(Unit1, D1, X, Y, Unit3, H3, D3), T1, T2), not_equal(Unit, Unit1)]).



d_pre([happens(turn_hit_shelter(Unit, D, X, Y, Unit2, H2, D2, X2, Y2), T1, T2), happens(turn_hit_shelter(Unit1, D1, X1, Y1, Unit3, H3, D3, X2, Y2), T1, T2), not_equal(Unit, Unit1)]).

d_pre([happens(turn_hit_shelter(Unit, D, X, Y, Unit2, H2, D2, X2, Y2), T1, T2), happens(turn_hit_shelter(Unit1, D1, X, Y, Unit3, H3, D3, X3, Y3), T1, T2), not_equal(Unit, Unit1)]).

/* Preconditions to stop clash between foreground and background actions or two clashing background items */


d_pre([happens(turn_hit(Unit, D, X, Y, Unit1, H, D1), T1, T2), happens(lower_health(Unit1, N), T1, T2)]).

d_pre([happens(hit_from(Unit, Unit1, H, D), T1, T2), happens(lower_health(Unit1, N), T1, T2)]).

d_pre([happens(turn_hit(Unit, D, X, Y, Unit1, H, D1), T1, T2), happens(burn(Unit1, N), T1, T2)]).

d_pre([happens(hit_from(Unit, Unit1, H, D), T1, T2), happens(burn(Unit1, N), T1, T2)]).

d_pre([happens(turn_hit(Unit, D, X, Y, Unit1, H, D1), T1, T2), happens(heal(Unit1, N), T1, T2)]).

d_pre([happens(heal(Unit1, N), T1, T2), happens(hit_from(Unit, Unit1, H, D), T1, T2)]).

d_pre([happens(lower_health(Unit, N), T1, T2), happens(burn(Unit, N1), T1, T2)]).

d_pre([happens(lower_health(Unit, N), T1, T2), happens(heal(Unit, N1), T1, T2)]).

d_pre([happens(burn(Unit, N), T1, T2), happens(heal(Unit, N1), T1, T2)]).

d_pre([happens(eat(Unit, V, F, N, A, B, C), T1, T2), happens(reduce_hunger(Unit, H), T1, T2)]).

/* Preconditions to ensure the unit does not move into an already occupied space */

d_pre([happens(walk(Unit, D, X, Y), T1, T2), holds(position(Unit1, Direction, X, Y), T2)]).

d_pre([happens(walk(Unit, D, X, Y), T1, T2), holds(animal(Type, N, Direction, X, Y), T2)]).

d_pre([happens(walk(Unit, D, X, Y), T1, T2), holds(shelter(Unit1, H, X, Y), T2), not_equal(Unit, Unit1)]).

d_pre([happens(walk(Unit, D, X, Y), T1, T2), holds(tree(X, Y), T2)]).

d_pre([happens(walk(Unit, D, X, Y), T1, T2), holds(wood(X, Y), T2)]).

d_pre([happens(walk(Unit, D, X, Y), T1, T2), holds(food(A, B, C, X, Y), T2)]).

d_pre([happens(walk(Unit, D, X, Y), T1, T2), greater_than(X, 20)]).

d_pre([happens(walk(Unit, D, X, Y), T1, T2), greater_than(Y, 20)]).

d_pre([happens(walk(Unit, D, X, Y), T1, T2), less_than(X, 1)]).

d_pre([happens(walk(Unit, D, X, Y), T1, T2), less_than(Y, 1)]).



d_pre([happens(successful_shelter(Unit, N, X, Y), T1, T2), holds(position(Unit1, Direction, X, Y), T2)]).

d_pre([happens(successful_shelter(Unit, N, X, Y), T1, T2), holds(animal(Type, N1, Direction, X, Y), T2)]).

d_pre([happens(successful_shelter(Unit, N, X, Y), T1, T2), holds(shelter(Unit1, H, X, Y), T2), not_equal(Unit, Unit1)]).

d_pre([happens(successful_shelter(Unit, N, X, Y), T1, T2), holds(tree(X, Y), T2)]).

d_pre([happens(successful_shelter(Unit, N, X, Y), T1, T2), holds(wood(X, Y), T2)]).

d_pre([happens(successful_shelter(Unit, N, X, Y), T1, T2), holds(food(A, B, C, X, Y), T2)]).

d_pre([happens(successful_shelter(Unit, N, X, Y), T1, T2), greater_than(X, 20)]).

d_pre([happens(successful_shelter(Unit, N, X, Y), T1, T2), greater_than(Y, 20)]).

d_pre([happens(successful_shelter(Unit, N, X, Y), T1, T2), less_than(X, 1)]).

d_pre([happens(successful_shelter(Unit, N, X, Y), T1, T2), less_than(Y, 1)]).

/* Preconditions to ensure the unit cannot perform any action when they are dead */

d_pre([happens(turn(Unit, N1, D1, X1, Y1), T1, T2), holds(dead(Unit), T2)]).

d_pre([happens(walk(Unit, D1, X1, Y1), T1, T2), holds(dead(Unit), T2)]).

d_pre([happens(successful_shelter(Unit, N1, X1, Y1), T1, T2), holds(dead(Unit), T2)]).

d_pre([happens(break_tree(Unit, X1, Y1), T1, T2), holds(dead(Unit), T2)]).

d_pre([happens(collect_wood(Unit, W, X1, Y1), T1, T2), holds(dead(Unit), T2)]).

d_pre([happens(change_animal(Unit, N1, D1, X1, Y1, N2, A), T1, T2), holds(dead(Unit), T2)]).

d_pre([happens(collect_food(Unit, A, B, C, K, L, M, X1, Y1), T1, T2), holds(dead(Unit), T2)]).

d_pre([happens(hit_shelter(Unit, Unit1, H, D1, X1, Y1), T1, T2), holds(dead(Unit), T2)]).

d_pre([happens(hit_from(Unit, Unit1, H, D1), T1, T2), holds(dead(Unit), T2)]).

d_pre([happens(turn_hit(Unit, D1, X1, Y1, Unit1, H, D2), T1, T2), holds(dead(Unit), T2)]).

d_pre([happens(turn_hit_shelter(Unit, D1, X1, Y1, Unit1, H, D2, X2, Y2), T1, T2), holds(dead(Unit), T2)]).

d_pre([happens(eat(Unit, V, F, N1, A, B, C), T1, T2), holds(dead(Unit), T2)]).

/********************************************* l_events' for universal use *********************************************/


action(find(_, _)).

action(turn(_, _, _, _, _)).

action(walk_towards(_, _)).

action(walk(_, _, _, _)).

/* Defining how to find an item of specified type */

l_events(
	happens(find(Unit, Type), T1, T2),
	[
		holds(in_sight(Unit, Type), T1)
	]
).

/* Recursive case */

l_events(
	happens(find(Unit, Type), T1, T3),
	[
		holds(position(Unit, north, X, Y), T1),
		holds(turns(Unit, N), T1),
		less_than(N, 4),
		happens(turn(Unit, N, west, X, Y), T1, T2),
		happens(find(Unit, Type), T2, T3)
	]
).

l_events(
	happens(find(Unit, Type), T1, T3),
	[
		holds(position(Unit, west, X, Y), T1),
		holds(turns(Unit, N), T1),
		less_than(N, 4),
		happens(turn(Unit, N, south, X, Y), T1, T2),
		happens(find(Unit, Type), T2, T3)
	]
).

l_events(
	happens(find(Unit, Type), T1, T3),
	[
		holds(position(Unit, south, X, Y), T1),
		holds(turns(Unit, N), T1),
		less_than(N, 4),
		happens(turn(Unit, N, east, X, Y), T1, T2),
		happens(find(Unit, Type), T2, T3)
	]
).

l_events(
	happens(find(Unit, Type), T1, T3),
	[
		holds(position(Unit, east, X, Y), T1),
		holds(turns(Unit, N), T1),
		less_than(N, 4),
		happens(turn(Unit, N, north, X, Y), T1, T2),
		happens(find(Unit, Type), T2, T3)
	]
).

/* Recursive case when too many turns are made */

l_events(
	happens(find(Unit, Type), T1, T3),
	[
		holds(position(Unit, north, X, Y), T1),
		holds(turns(Unit, N), T1),
		greater_or_equal(N, 4),
		decrement(Y, Y2),
		greater_than(Y2, 1),
		happens(walk(Unit, north, X, Y2), T1, T2),
		happens(find(Unit, Type), T2, T3)

	]
).

l_events(
	happens(find(Unit, Type), T1, T3),
	[
		holds(position(Unit, east, X, Y), T1),
		holds(turns(Unit, N), T1),
		greater_or_equal(N, 4),
		increment(X, X2),
		less_or_equal(X2, 20),
		happens(walk(Unit, east, X2, Y), T1, T2),
		happens(find(Unit, Type), T2, T3)

	]
).

l_events(
	happens(find(Unit, Type), T1, T3),
	[
		holds(position(Unit, south, X, Y), T1),
		holds(turns(Unit, N), T1),
		greater_or_equal(N, 4),
		increment(Y, Y2),
		less_or_equal(Y2, 20),
		happens(walk(Unit, south, X, Y2), T1, T2),
		happens(find(Unit, Type), T2, T3)

	]
).

l_events(
	happens(find(Unit, Type), T1, T3),
	[
		holds(position(Unit, west, X, Y), T1),
		holds(turns(Unit, N), T1),
		greater_or_equal(N, 4),
		decrement(X, X2),
		greater_or_equal(X2, 1),
		happens(walk(Unit, west, X2, Y), T1, T2),
		happens(find(Unit, Type), T2, T3)

	]
).

/* Defining how to walk towards an item of specified type */

/* Base Case */

l_events(
	happens(walk_towards(Unit, Type), T1, T2),
	[
		holds(next_to(Unit, Type), T1)
	]

).

/* Recursive case */

l_events(
	happens(walk_towards(Unit, Type), T1, T3),
	[
		holds(position(Unit, north, X, Y), T1),
		holds(in_sight(Unit, Type), T1),
		holds(item(Type, X, A), T1),
		decrease(Y, Y1, A),
		greater_than(Y1, 1),
		decrement(Y, Y2),
		greater_or_equal(Y2, 1),
		happens(walk(Unit, north, X, Y2), T1, T2),
		happens(walk_towards(Unit, Type), T2, T3)

	]

).

l_events(
	happens(walk_towards(Unit, Type), T1, T3),
	[
		holds(position(Unit, east, X, Y), T1),
		holds(in_sight(Unit, Type), T1),
		holds(item(Type, A, Y), T1),
		decrease(A, X1, X),
		greater_than(X1, 1),
		increment(X, X2),
		less_or_equal(X2, 20),
		happens(walk(Unit, east, X2, Y), T1, T2),
		happens(walk_towards(Unit, Type), T2, T3)
	]

).

l_events(
	happens(walk_towards(Unit, Type), T1, T3),
	[
		holds(position(Unit, south, X, Y), T1),
		holds(in_sight(Unit, Type), T1),
		holds(item(Type, X, A), T1),
		decrease(A, Y1, Y),
		greater_than(Y1, 1),
		increment(Y, Y2),
		less_or_equal(Y2, 20),
		happens(walk(Unit, south, X, Y2), T1, T2),
		happens(walk_towards(Unit, Type), T2, T3)
	]

).

l_events(
	happens(walk_towards(Unit, Type), T1, T3),
	[
		holds(position(Unit, west, X, Y), T1),
		holds(in_sight(Unit, Type), T1),
		holds(item(Type, A, Y), T1),
		decrease(X, X1, A),
		greater_than(X1, 1),
		decrement(X, X2),
		greater_or_equal(X2, 1),
		happens(walk(Unit, west, X2, Y), T1, T2),
		happens(walk_towards(Unit, Type), T2, T3)
	]

).

/* Post conditions to update the state */

terminated(happens(turn(Unit, N, D, X, Y), T1, T2), position(Unit, Direction, X, Y), []).

initiated(happens(turn(Unit, N, D, X, Y), T1, T2), position(Unit, D, X, Y), []).

initiated(happens(turn(Unit, N, D, X, Y), T1, T2), turns(Unit, N1), [increment(N, N1)]).
terminated(happens(turn(Unit, N, D, X, Y), T1, T2), turns(Unit, N), []).

initiated(happens(turn(Unit, N, D, X, Y), T1, T2), cycles(T), [modu(T, T1)]).
terminated(happens(turn(Unit, N, D, X, Y), T1, T2), cycles(T), []).


terminated(happens(walk(Unit, D, X, Y), T1, T2), position(Unit, D1, X1, Y1), []).

initiated(happens(walk(Unit, D, X, Y), T1, T2), position(Unit, D, X, Y), []).

terminated(happens(walk(Unit, D, X, Y), T1, T2), turns(Unit, N), []).
initiated(happens(walk(Unit, D, X, Y), T1, T2), turns(Unit, 0), []).

terminated(happens(walk(Unit, D, X, Y), T1, T2), cycles(T), []).
initiated(happens(walk(Unit, D, X, Y), T1, T2), cycles(T), [modu(T, T1)]).

/*********************************** Logic to make a shelter ***********************************/

action(make_shelter(_)).

action(build_shelter(_, _)).

action(get_wood(_)).

action(successful_shelter(_, _, _, _)).

/* Reactive rules to make a shelter */
reactive_rule(
	[
		happens(start_game(Unit), T1, T2),
		holds(person(Unit, cautious), T2),
		holds(has_shelter(Unit, false), T2)
	],
	[
		happens(make_shelter(Unit), T3, T4),
		tc(T2 =< T3)
	],
	95
).

reactive_rule(
	[
		happens(start_game(Unit), T1, T2),
		holds(person(Unit, normal), T2),
		holds(has_shelter(Unit, false), T2)
	],
	[
		happens(make_shelter(Unit), T3, T4),
		tc(T2 =< T3)
	],
	95
).

/* Base case when unit has enough wood */

l_events(
	happens(make_shelter(Unit), T1, T2),
	[
		holds(holds_wood(Unit, N), T1),
		greater_or_equal(N, 25),
		happens(build_shelter(Unit, N), T1, T2)
	]
).

/* Recursive case when unit doesn't have enough wood */

l_events(
	happens(make_shelter(Unit), T1, T3),
	[
		holds(holds_wood(Unit, N), T1),
		less_than(N, 25),
		happens(get_wood(Unit), T1, T2),
		happens(make_shelter(Unit), T2, T3)
	]
).

/* Case when unit has enough wood */
/* Different cases to build a shelter depending on unit's direction */

l_events(
	happens(build_shelter(Unit, N), T1, T2),
	[
		holds(position(Unit, north, X, Y), T1),
		decrement(Y, Y1),
		happens(successful_shelter(Unit, N, X, Y1), T1, T2)
	]

).

l_events(
	happens(build_shelter(Unit, N), T1, T2),
	[
		holds(position(Unit, east, X, Y), T1),
		increment(X, X1),
		happens(successful_shelter(Unit, N, X1, Y), T1, T2)
	]

).

l_events(
	happens(build_shelter(Unit, N), T1, T2),
	[
		holds(position(Unit, south, X, Y), T1),
		increment(Y, Y1),
		happens(successful_shelter(Unit, N, X, Y1), T1, T2)
	]

).

l_events(
	happens(build_shelter(Unit, N), T1, T2),
	[
		holds(position(Unit, west, X, Y), T1),
		decrement(X, X1),
		happens(successful_shelter(Unit, N, X1, Y), T1, T2)
	]

).		

/* Case when unit doesn't have enough wood */

action(break_tree(_, _, _)).
action(collect_wood(_, _, _, _)).
action(hit_tree(_)).

/* Case where unit can see wood */

l_events(
	happens(get_wood(Unit), T1, T3),
	[
		holds(in_sight(Unit, wood), T1),
		happens(walk_towards(Unit, wood), T1, T2),
		happens(pick_wood(Unit), T2, T3)
	]
).

/* Alternative case for unit to find trees */

l_events(
	happens(get_wood(Unit), T1, T5),
	[
		holds(position(Unit, Direction, X, Y), T1),
		happens(find(Unit, tree), T1, T2),
		happens(walk_towards(Unit, tree), T2, T3),
		happens(hit_tree(Unit), T3, T4),
		happens(pick_wood(Unit), T4, T5)
	]

).


/* Cases when tree is already wood */

l_events(
	happens(hit_tree(Unit), T1, T2),
	[
		holds(position(Unit, north, X, Y), T1),
		decrement(Y, Y1),
		holds(wood(X, Y1), T1)
	]

).

l_events(
	happens(hit_tree(Unit), T1, T2),
	[
		holds(position(Unit, east, X, Y), T1),
		increment(X, X1),
		holds(wood(X1, Y), T1)
	]

).

l_events(
	happens(hit_tree(Unit), T1, T2),
	[
		holds(position(Unit, south, X, Y), T1),
		increment(Y, Y1),
		holds(wood(X, Y1), T1)
	]

).

l_events(
	happens(hit_tree(Unit), T1, T2),
	[
		holds(position(Unit, west, X, Y), T1),
		decrement(X, X1),
		holds(wood(X1, Y), T1)
	]

).


/* Different cases to hit a tree depending on unit's direction */

l_events(
	happens(hit_tree(Unit), T1, T2),
	[
		holds(position(Unit, north, X, Y), T1),
		decrement(Y, Y1),
		holds(tree(X, Y1), T1),
		happens(break_tree(Unit, X, Y1), T1, T2)
	]

).

l_events(
	happens(hit_tree(Unit), T1, T2),
	[
		holds(position(Unit, east, X, Y), T1),
		increment(X, X1),
		holds(tree(X1, Y), T1),
		happens(break_tree(Unit, X1, Y), T1, T2)
	]

).

l_events(
	happens(hit_tree(Unit), T1, T2),
	[
		holds(position(Unit, south, X, Y), T1),
		increment(Y, Y1),
		holds(tree(X, Y1), T1),
		happens(break_tree(Unit, X, Y1), T1, T2)
	]

).

l_events(
	happens(hit_tree(Unit), T1, T2),
	[
		holds(position(Unit, west, X, Y), T1),
		decrement(X, X1),
		holds(tree(X1, Y), T1),
		happens(break_tree(Unit, X1, Y), T1, T2)
	]

).

/* Different cases to pick wood depending on unit's direction */

action(pick_wood(_)).

l_events(
	happens(pick_wood(Unit), T1, T2),
	[
		holds(position(Unit, north, X, Y), T1),
		holds(holds_wood(Unit, N), T1),
		decrement(Y, Y1),
		holds(wood(X, Y1), T1),
		happens(collect_wood(Unit, N, X, Y1), T1, T2)
	]

).

l_events(
	happens(pick_wood(Unit), T1, T2),
	[
		holds(position(Unit, east, X, Y), T1),
		holds(holds_wood(Unit, N), T1),
		increment(X, X1),
		holds(wood(X1, Y), T3),
		happens(collect_wood(Unit, N, X1, Y), T1, T2)
	]

).

l_events(
	happens(pick_wood(Unit), T1, T2),
	[
		holds(position(Unit, south, X, Y), T1),
		holds(holds_wood(Unit, N), T1),
		increment(Y, Y1),
		holds(wood(X, Y1), T3),
		happens(collect_wood(Unit, N, X, Y1), T1, T2)
	]

).

l_events(
	happens(pick_wood(Unit), T1, T2),
	[
		holds(position(Unit, west, X, Y), T1),
		holds(holds_wood(Unit, N), T1),
		decrement(X, X1),
		holds(wood(X1, Y), T3),
		happens(collect_wood(Unit, N, X1, Y), T1, T2)
	]

).


/* Post conditions to update the state */

/* To make a shelter */

initiated(happens(successful_shelter(Unit, N, X, Y), T1, T2), holds_wood(Unit, N1), [decrease(N, N1, 25)]).
terminated(happens(successful_shelter(Unit, N, X, Y), T1, T2), holds_wood(Unit, N), []).

initiated(happens(successful_shelter(Unit, N, X, Y), T1, T2), has_shelter(Unit, true), []).
terminated(happens(successful_shelter(Unit, N, X, Y), T1, T2), has_shelter(Unit, false), []).

initiated(happens(successful_shelter(Unit, N, X, Y), T1, T2), shelter(Unit, 100, X, Y), []).

initiated(happens(successful_shelter(Unit, N, X, Y), T1, T2), cycles(T), [modu(T, T1)]).
terminated(happens(successful_shelter(Unit, N, X, Y), T1, T2), cycles(T), []).

terminated(happens(successful_shelter(Unit, N, X, Y), T1, T2), turns(Unit, N1), []).
initiated(happens(successful_shelter(Unit, N, X, Y), T1, T2), turns(Unit, 0), []).

/* When a tree is broken */

terminated(happens(break_tree(Unit, X, Y), T1, T2), tree(X, Y), []).
initiated(happens(break_tree(Unit, X, Y), T1, T2), wood(X, Y), []).

terminated(happens(break_tree(Unit, X, Y), T1, T2), turns(Unit, N), []).
initiated(happens(break_tree(Unit, X, Y), T1, T2), turns(Unit, 0), []).

initiated(happens(break_tree(Unit, X, Y), T1, T2), cycles(T), [modu(T, T1)]).
terminated(happens(break_tree(Unit, X, Y), T1, T2), cycles(T), []).

terminated(happens(break_tree(Unit, X, Y), T1, T2), turns(Unit, N), []).
initiated(happens(break_tree(Unit, X, Y), T1, T2), turns(Unit, 0), []).


/* When a unit collects wood */

terminated(happens(collect_wood(Unit, N, X, Y), T1, T2), wood(X, Y), []).
terminated(happens(collect_wood(Unit, N, X, Y), T1, T2), holds_wood(Unit, N), []).
initiated(happens(collect_wood(Unit, N, X, Y), T1, T2), holds_wood(Unit, N1), [increase(N, N1, 5)]).

initiated(happens(collect_wood(Unit, N, X, Y), T1, T2), cycles(T), [modu(T, T1)]).
terminated(happens(collect_wood(Unit, N, X, Y), T1, T2), cycles(T), []).

terminated(happens(collect_wood(Unit, N, X, Y), T1, T2), turns(Unit, N1), []).
initiated(happens(collect_wood(Unit, N, X, Y), T1, T2), turns(Unit, 0), []).


/**************************************** Logic to get food ****************************************/

action(get_food(_)).

action(hit_animal(_)).

action(pick_food(_)).

action(pick_up_food(_, _)).

action(collect_food(_, _, _, _, _, _, _, _, _)).

action(kill_animal(_)).


/* Reactive rule to get food */

reactive_rule(
	[
		happens(start_game(Unit), T1, T2)
	],
	[
		happens(need_food(Unit), T3, T4),
		tc(T2 =< T3)
	],
	94
).

/* Case for cautious unit needing food */

/* Base case for cautious units */

l_events(
	happens(need_food(Unit), T1, T2),
	[
		holds(person(Unit, cautious), T1),
		holds(holds_food(Unit, N, A, B, C), T1),
		greater_or_equal(N, 15)
	]
).

/* Recursive case for cautious units */

l_events(
	happens(need_food(Unit), T1, T3),
	[
		holds(person(Unit, cautious), T1),
		holds(holds_food(Unit, N, A, B, C), T1),
		less_than(N, 15),
		happens(get_food(Unit), T1, T2),
		happens(need_food(Unit), T2, T3)
	]

).

/* Base case for normal units */

l_events(
	happens(need_food(Unit), T1, T2),
	[
		holds(person(Unit, normal), T1),
		holds(holds_food(Unit, N, A, B, C), T1),
		greater_or_equal(N, 10)
	]
).

/* Recursive case for normal units */

l_events(
	happens(need_food(Unit), T1, T3),
	[
		holds(person(Unit, normal), T1),
		holds(holds_food(Unit, N, A, B, C), T1),
		less_than(N, 10),
		happens(get_food(Unit), T1, T2),
		happens(need_food(Unit), T2, T3)
	]

).

/* Base case for violent units */

l_events(
	happens(need_food(Unit), T1, T2),
	[
		holds(person(Unit, violent), T1),
		holds(holds_food(Unit, N, A, B, C), T1),
		greater_or_equal(N, 5)
	]
).

/* Recursive case for violent units */

l_events(
	happens(need_food(Unit), T1, T3),
	[
		holds(person(Unit, violent), T1),
		holds(holds_food(Unit, N, A, B, C), T1),
		less_than(N, 5),
		happens(get_food(Unit), T1, T2),
		happens(need_food(Unit), T2, T3)
	]

).

/* Case if food is in in_sight */

l_events(
	happens(get_food(Unit), T1, T2),
	[
		holds(in_sight(Unit, food), T1),
		happens(pick_food(Unit), T1, T2)
	]
).

/* Case if food is not in sight, find and hit animal, then recurse */

l_events(
	happens(get_food(Unit), T1, T5),
	[
		happens(find(Unit, animal), T1, T2),
		happens(kill_animal(Unit), T2, T3),
		happens(hit_animal(Unit), T3, T4),
		happens(get_food(Unit), T4, T5)
	]
).

/* Base case when food is in range */

l_events(
	happens(kill_animal(Unit), T1, T2),
	[
		holds(position(Unit, D, X, Y), T1),
		holds(in_sight(Unit, food), T1)
	]
).

/* Base case when animal is in range */

l_events(
	happens(kill_animal(Unit), T1, T2),
	[
		holds(position(Unit, D, X, Y), T1),
		has(Unit, weapon(A)),
		holds(in_range(Unit, animal, D, A), T1)
	]
).


/* Recursive case when animal is too far away from enemy */

l_events(
	happens(kill_animal(Unit), T1, T3),
	[
		holds(position(Unit, north, X, Y), T1),
		holds(animal(Type, H, D1, X, Y1), T1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(Y, Y2, Y1),
		greater_than(Y2, U),
		decrement(Y, NewY),
		happens(walk(Unit, north, X, NewY), T1, T2),
		happens(kill_animal(Unit), T2, T3)
	]
).

l_events(
	happens(kill_animal(Unit), T1, T3),
	[
		holds(position(Unit, east, X, Y), T1),
		holds(animal(Type, H, D1, X1, Y), T1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(X1, X2, X),
		greater_than(X2, U),
		increment(X, NewX),
		happens(walk(Unit, east, NewX, Y), T1, T2),
		happens(kill_animal(Unit), T2, T3)
	]
).

l_events(
	happens(kill_animal(Unit), T1, T3),
	[
		holds(position(Unit, south, X, Y), T1),
		holds(animal(Type, H, D1, X, Y1), T1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(Y1, Y2, Y),
		greater_than(Y2, U),
		increment(Y, NewY),
		happens(walk(Unit, south, X, NewY), T1, T2),
		happens(kill_animal(Unit), T2, T3)

	]
).

l_events(
	happens(kill_animal(Unit), T1, T3),
	[
		holds(position(Unit, west, X, Y), T1),
		holds(animal(Type, H, D1, X1, Y), T1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(X, X2, X1),
		greater_than(X2, U),
		decrement(X, NewX),
		happens(walk(Unit, west, NewX, Y), T1, T2),
		happens(kill_animal(Unit), T2, T3)
	]
).


/* Recursive case when animal is too close to the enemy unit */

l_events(
	happens(kill_animal(Unit), T1, T3),
	[
		holds(position(Unit, north, X, Y), T1),
		holds(animal(Type, H, D1, X, Y1), T1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(Y, Y2, Y1),
		less_than(Y2, L),
		increment(Y, NewY),
		happens(walk(Unit, north, X, NewY), T1, T2),
		happens(kill_animal(Unit), T2, T3)
	]
).

l_events(
	happens(kill_animal(Unit), T1, T3),
	[
		holds(position(Unit, east, X, Y), T1),
		holds(animal(Type, H, D1, X1, Y), T1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(X1, X2, X),
		less_than(X2, L),
		decrement(X, NewX),
		happens(walk(Unit, east, NewX, Y), T1, T2),
		happens(kill_animal(Unit), T2, T3)
	]
).

l_events(
	happens(kill_animal(Unit), T1, T3),
	[
		holds(position(Unit, south, X, Y), T1),
		holds(animal(Type, H, D1, X, Y1), T1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(Y1, Y2, Y),
		less_than(Y2, L),
		decrement(Y, NewY),
		happens(walk(Unit, south, X, NewY), T1, T2),
		happens(kill_animal(Unit), T2, T3)

	]
).

l_events(
	happens(kill_animal(Unit), T1, T3),
	[
		holds(position(Unit, west, X, Y), T1),
		holds(animal(Type, H, D1, X1, Y), T1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(X, X2, X1),
		less_than(X2, L),
		increment(X, NewX),
		happens(walk(Unit, west, NewX, Y), T1, T2),
		happens(kill_animal(Unit), T2, T3)
	]
).

/* Base case when food is in range */

l_events(
	happens(hit_animal(Unit), T1, T2),
	[
		holds(position(Unit, D, X, Y), T1),
		holds(in_sight(Unit, food), T1)
	]
).

/* Different cases to hit an animal depending on direction of unit */

l_events(
	happens(hit_animal(Unit), T1, T2),
	[
		holds(position(Unit, north, X, Y), T1),
		holds(animal(Type, H, D1, X, Y1), T1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(Y, Y2, Y1),
		greater_or_equal(Y2, L),
		less_or_equal(Y2, U),
		happens(reduce_health(Unit, animal(Type, H, D1, X, Y1)), T1, T2)
	]

).

l_events(
	happens(hit_animal(Unit), T1, T2),
	[
		holds(position(Unit, east, X, Y), T1),
		holds(animal(Type, H, D1, X1, Y), T1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(X1, X2, X),
		greater_or_equal(X2, L),
		less_or_equal(X2, U),
		happens(reduce_health(Unit, animal(Type, H, D1, X1, Y)), T1, T2)
	]

).

l_events(
	happens(hit_animal(Unit), T1, T2),
	[
		holds(position(Unit, south, X, Y), T1),
		holds(animal(Type, H, D1, X, Y1), T1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(Y1, Y2, Y),
		greater_or_equal(Y2, L),
		less_or_equal(Y2, U),
		happens(reduce_health(Unit, animal(Type, H, D1, X, Y1)), T1, T2)
	]

).

l_events(
	happens(hit_animal(Unit), T1, T2),
	[
		holds(position(Unit, west, X, Y), T1),
		holds(animal(Type, H, D1, X1, Y), T1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(X, X2, X1),
		greater_or_equal(X2, L),
		less_or_equal(X2, U),
		happens(reduce_health(Unit, animal(Type, H, D1, X1, Y)), T1, T2)
	]

).

/* Reducing the health of the animal */

action(reduce_health(_, _)).
action(check_animal(_, _, _, _, _, _, _)).
action(change_animal(_, _, _, _, _, _, _)).


/* Chicken case */

l_events(
	happens(reduce_health(Unit, animal(Type, N, D, X, Y)), T1, T2),
	[
		holds(person(Unit, Kind), T1),
		has(Unit, weapon(A)),
		power(weapon(A), P),
		decrease(N, N1, P),
		happens(check_animal(Unit, N, D, X, Y, N1, Type), T1, T2) 
	]

).

/* Case where N1 is less than zero, then animal is dead */

l_events(
	happens(check_animal(Unit, N, D, X, Y, N1, A), T1, T2),
	[
		less_than(N1, 0),
		happens(change_animal(Unit, N, D, X, Y, 0, A), T1, T2)
	]

).

/* Case where N1 is greater than zero, then animal's health is reduced */

l_events(
	happens(check_animal(Unit, N, D, X, Y, N1, A), T1, T2),
	[
		greater_or_equal(N1, 0),
		happens(change_animal(Unit, N, D, X, Y, N1, A), T1, T2)
	]

).

/* Case for when there is food in sight */

l_events(
	happens(pick_food(Unit), T1, T3),
	[
		happens(walk_towards(Unit, food), T1, T2),
		happens(pick_up_food(Unit), T2, T3)
	]
).

/* Different cases for unit to pick up food depending on the unit's direction */

l_events(
	happens(pick_up_food(Unit), T1, T2),
	[
		holds(position(Unit, north, X, Y), T1),
		holds(holds_food(Unit, N, A, B, C), T1),
		decrement(Y, Y1),
		holds(food(K, L, M, X, Y1), T1),
		happens(collect_food(Unit, A, B, C, K, L, M, X, Y1), T1, T2)
	]

).

l_events(
	happens(pick_up_food(Unit), T1, T2),
	[
		holds(position(Unit, east, X, Y), T1),
		holds(holds_food(Unit, N, A, B, C), T1),
		increment(X, X1),
		holds(food(K, L, M, X1, Y), T1),
		happens(collect_food(Unit, A, B, C, K, L, M, X1, Y), T1, T2)
	]

).

l_events(
	happens(pick_up_food(Unit), T1, T2),
	[
		holds(position(Unit, south, X, Y), T1),
		holds(holds_food(Unit, N, A, B, C), T1),
		increment(Y, Y1),
		holds(food(K, L, M, X, Y1), T1),
		happens(collect_food(Unit, A, B, C, K, L, M, X, Y1), T1, T2)
	]

).

l_events(
	happens(pick_up_food(Unit), T1, T2),
	[
		holds(position(Unit, west, X, Y), T1),
		holds(holds_food(Unit, N, A, B, C), T1),
		decrement(X, X1),
		holds(food(K, L, M, X1, Y), T1),
		happens(collect_food(Unit, A, B, C, K, L, M, X1, Y), T1, T2)
	]

).

/* Post conditions to update the state */

initiated(happens(change_animal(Unit, N, D, X, Y, N1, Type), T1, T2), animal(Type, N1, D, X, Y), [greater_than(N1, 0)]).
terminated(happens(change_animal(Unit, N, D, X, Y, N1, Type), T1, T2), animal(Type, N, D, X, Y), [greater_than(N1, 0)]).

initiated(happens(change_animal(Unit, N, D, X, Y, 0, rabbit), T1, T2), food(2, 1, 0, X, Y), []).
terminated(happens(change_animal(Unit, N, D, X, Y, 0, rabbit), T1, T2), animal(rabbit, N1, D, X, Y), []).

initiated(happens(change_animal(Unit, N, D, X, Y, 0, chicken), T1, T2), food(2, 1, 1, X, Y), []).
terminated(happens(change_animal(Unit, N, D, X, Y, 0, chicken), T1, T2), animal(chicken, N1, D, X, Y), []).

initiated(happens(change_animal(Unit, N, D, X, Y, 0, cow), T1, T2), food(2, 1, 2, X, Y), []).
terminated(happens(change_animal(Unit, N, D, X, Y, 0, cow), T1, T2), animal(cow, N1, D, X, Y), []).

initiated(happens(change_animal(Unit, N, D, X, Y, N1, Type), T1, T2), cycles(T), [modu(T, T1)]).
terminated(happens(change_animal(Unit, N, D, X, Y, N1, Type), T1, T2), cycles(T), []).

terminated(happens(change_animal(Unit, N, D, X, Y, N1, Type), T1, T2), turns(Unit, N2), []).
initiated(happens(change_animal(Unit, N, D, X, Y, N1, Type), T1, T2), turns(Unit, 0), []).

terminated(happens(collect_food(Unit, A, B, C, K, L, M, X, Y), T1, T2), food(K, L, M, X, Y), []).
terminated(happens(collect_food(Unit, A, B, C, K, L, M, X, Y), T1, T2), holds_food(Unit, N, A, B, C), []).

initiated(happens(collect_food(Unit, A, B, C, K, L, M, X, Y), T1, T2), holds_food(Unit, N1, A1, B1, C1), 
		[increase(A, A1, K), increase(B, B1, L), increase(C, C1, M), calculate(N1, A1, B1, C1)]).

initiated(happens(collect_food(Unit, A, B, C, K, L, M, X, Y), T1, T2), cycles(T), [modu(T, T1)]).
terminated(happens(collect_food(Unit, A, B, C, K, L, M, X, Y), T1, T2), cycles(T), []).

terminated(happens(collect_food(Unit, A, B, C, K, L, M, X, Y), T1, T2), turns(Unit, N), []).
initiated(happens(collect_food(Unit, A, B, C, K, L, M, X, Y), T1, T2), turns(Unit, 0), []).


/**************************************** Logic when attacked or to attack ****************************************/

action(attack(_)).

action(hit_from(_, _, _, _)).

action(walk_backward(_, _, _, _)).

action(hit(_, _, _, _, _)).

action(hit_shelter(_, _, _, _, _, _)).

action(move_away(_, _)).

action(turn_and_hit(_, _, _, _, _, _, _, _)).

action(turn_hit_shelter(_, _, _, _, _, _, _, _, _)).

action(turn_hit(_, _, _, _, _, _, _)).

action(walk_away(_)).

action(look(_, _)).

/* Reactive rules */

reactive_rule(
	[
		holds(person(Unit, cautious), T1),
		holds(in_sight(Unit, person), T1)
	],
	[
		happens(walk_away(Unit), T2, T3)
	],
	97
).

reactive_rule(
	[
		happens(hit_from(Unit, Unit1, H, D), T1, T2),
		holds(person(Unit1, cautious), T2),
		holds(in_view(Unit1, D, person), T2)
	],
	[
		happens(move_away(Unit1, D), T3, T4)
	],
	98
).

reactive_rule(
	[
		happens(turn_hit(Unit, D, X, Y, Unit, H, D1), T1, T2),
		holds(person(Unit1, cautious), T2),
		holds(in_view(Unit1, D, person), T2)
	],
	[
		happens(move_away(Unit1, D), T3, T4)
	],
	98
).

reactive_rule(
	[
		happens(hit_from(Unit, Unit1, H, D), T1, T2),
		holds(person(Unit1, normal), T2),
		holds(in_view(Unit1, D, person), T2)
	],
	[
		happens(turn_and_attack(Unit1, D, Unit), T3, T4)
	],
	98
).

reactive_rule(
	[
		happens(turn_hit(Unit, D, X, Y, Unit1, H, D1), T1, T2),
		holds(person(Unit1, normal), T2),
		holds(in_view(Unit1, D, person), T2)
	],
	[
		happens(turn_and_attack(Unit1, D, Unit), T3, T4)
	],
	98
).

reactive_rule(
	[
		holds(person(Unit, violent), T1),
		holds(in_sight(Unit, person), T1)
	],
	[
		happens(attack(Unit), T2, T3)
	],
	93

).

reactive_rule(
	[
		holds(person(Unit, violent), T1),
		holds(not(in_sight(Unit, person)), T1)
	],
	[
		happens(look(Unit, person), T2, T3)
	],
	92

).

reactive_rule(
	[
		happens(hit_from(Unit, Unit1, H, D), T1, T2),
		holds(person(Unit1, violent), T2),
		holds(in_view(Unit1, D, person), T2)
	],
	[
		happens(turn_and_attack(Unit1, D, Unit), T3, T4)
	],
	98
).

reactive_rule(
	[
		happens(turn_hit(Unit, D, X, Y, Unit1, H, D1), T1, T2),
		holds(person(Unit1, violent), T2),
		holds(in_view(Unit1, D, person), T2)
	],
	[
		happens(turn_and_attack(Unit1, D, Unit), T3, T4)
	],
	98
).

/* Base case if in range of unit, to attack them */

l_events(
	happens(attack(Unit), T1, T2),
	[
		holds(position(Unit, D, X, Y), T1),
		has(Unit, weapon(A)),
		holds(in_range(Unit, Unit1, D, A), T1),
		power(weapon(A), P),
		holds(health(Unit1, H), T1),
		opposite(D, D1),
		happens(hit(Unit, Unit1, H, D1, P), T1, T2)
	]
).

/* Case when unit is in shelter */

l_events(
	happens(hit(Unit, Unit1, H, D1, P), T1, T2),
	[
		holds(in_shelter(Unit1), T1),
		holds(shelter(Unit1, H1, X, Y), T1),
		decrease(H1, H2, P),
		greater_than(H2, 0),
		happens(hit_shelter(Unit, Unit1, H2, D1, X, Y), T1, T2)
	]
).

l_events(
	happens(hit(Unit, Unit1, H, D1, P), T1, T2),
	[
		holds(in_shelter(Unit1), T1),
		holds(shelter(Unit1, H1, X, Y), T1),
		decrease(H1, H2, P),
		less_or_equal(H2, 0),
		happens(hit_shelter(Unit, Unit1, 0, D1, X, Y), T1, T2)
	]
).

/* Case when unit is not inside a shelter */

l_events(
	happens(hit(Unit, Unit1, H, D1, P), T1, T2),
	[
		holds(not(in_shelter(Unit1)), T1),
		decrease(H, H1, P),
		greater_than(H1, 0),
		happens(hit_from(Unit, Unit1, H1, D1), T1, T2)
	]
).

l_events(
	happens(hit(Unit, Unit1, H, D1, P), T1, T2),
	[
		holds(not(in_shelter(Unit1)), T1),
		decrease(H, H1, P),
		less_or_equal(H1, 0),
		happens(hit_from(Unit, Unit1, 0, D1), T1, T2)
	]
).


/* Recursive case when unit is too far away from enemy */

l_events(
	happens(attack(Unit), T1, T3),
	[
		holds(position(Unit, north, X, Y), T1),
		holds(position(Unit1, D1, X, Y1), T1),
		not_equal(Unit, Unit1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(Y, Y2, Y1),
		greater_than(Y2, U),
		decrement(Y, NewY),
		happens(walk(Unit, north, X, NewY), T1, T2),
		happens(attack(Unit), T2, T3)
	]
).

l_events(
	happens(attack(Unit), T1, T3),
	[
		holds(position(Unit, east, X, Y), T1),
		holds(position(Unit1, D1, X1, Y), T1),
		not_equal(Unit, Unit1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(X1, X2, X),
		greater_than(X2, U),
		increment(X, NewX),
		happens(walk(Unit, east, NewX, Y), T1, T2),
		happens(attack(Unit), T2, T3)
	]
).

l_events(
	happens(attack(Unit), T1, T3),
	[
		holds(position(Unit, south, X, Y), T1),
		holds(position(Unit1, D1, X, Y1), T1),
		not_equal(Unit, Unit1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(Y1, Y2, Y),
		greater_than(Y2, U),
		increment(Y, NewY),
		happens(walk(Unit, south, X, NewY), T1, T2),
		happens(attack(Unit), T2, T3)

	]
).

l_events(
	happens(attack(Unit), T1, T3),
	[
		holds(position(Unit, west, X, Y), T1),
		holds(position(Unit1, D1, X1, Y), T1),
		not_equal(Unit, Unit1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(X, X2, X1),
		greater_than(X2, U),
		decrement(X, NewX),
		happens(walk(Unit, west, NewX, Y), T1, T2),
		happens(attack(Unit), T2, T3)
	]
).


/* Recursive case when unit is too close to the enemy unit */

l_events(
	happens(attack(Unit), T1, T3),
	[
		holds(position(Unit, north, X, Y), T1),
		holds(position(Unit1, D1, X, Y1), T1),
		not_equal(Unit, Unit1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(Y, Y2, Y1),
		less_than(Y2, L),
		increment(Y, NewY),
		happens(walk(Unit, north, X, NewY), T1, T2),
		happens(attack(Unit), T2, T3)
	]
).

l_events(
	happens(attack(Unit), T1, T3),
	[
		holds(position(Unit, east, X, Y), T1),
		holds(position(Unit1, D1, X1, Y), T1),
		not_equal(Unit, Unit1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(X1, X2, X),
		less_than(X2, L),
		decrement(X, NewX),
		happens(walk(Unit, east, NewX, Y), T1, T2),
		happens(attack(Unit), T2, T3)
	]
).

l_events(
	happens(attack(Unit), T1, T3),
	[
		holds(position(Unit, south, X, Y), T1),
		holds(position(Unit1, D1, X, Y1), T1),
		not_equal(Unit, Unit1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(Y1, Y2, Y),
		less_than(Y2, L),
		decrement(Y, NewY),
		happens(walk(Unit, south, X, NewY), T1, T2),
		happens(attack(Unit), T2, T3)

	]
).

l_events(
	happens(attack(Unit), T1, T3),
	[
		holds(position(Unit, west, X, Y), T1),
		holds(position(Unit1, D1, X1, Y), T1),
		not_equal(Unit, Unit1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(X, X2, X1),
		less_than(X2, L),
		increment(X, NewX),
		happens(walk(Unit, west, NewX, Y), T1, T2),
		happens(attack(Unit), T2, T3)
	]
).

/* Case for cautious units to walk away when they spot enemies */

l_events(
	happens(walk_away(Unit), T1, T2),
	[
		holds(position(Unit, north, X, Y), T1),
		decrement(X, X1),
		happens(walk(Unit, west, X1, Y), T1, T2)
	]
).

l_events(
	happens(walk_away(Unit), T1, T2),
	[
		holds(position(Unit, east, X, Y), T1),
		decrement(Y, Y1),
		happens(walk(Unit, north, X, Y1), T1, T2)
	]
).

l_events(
	happens(walk_away(Unit), T1, T2),
	[
		holds(position(Unit, south, X, Y), T1),
		increment(X, X1),
		happens(walk(Unit, east, X1, Y), T1, T2)
	]
).

l_events(
	happens(walk_away(Unit), T1, T2),
	[
		holds(position(Unit, west, X, Y), T1),
		increment(Y, Y1),
		happens(walk(Unit, south, X, Y1), T1, T2)
	]
).

/* Case when violent person cannot see another person, then look for one */

/* Case to turn to look */

l_events(
	happens(look(Unit, Type), T1, T2),
	[
		holds(position(Unit, north, X, Y), T1),
		holds(turns(Unit, N), T1),
		less_than(N, 4),
		happens(turn(Unit, N, west, X, Y), T1, T2)
	]
).

l_events(
	happens(look(Unit, Type), T1, T2),
	[
		holds(position(Unit, west, X, Y), T1),
		holds(turns(Unit, N), T1),
		less_than(N, 4),
		happens(turn(Unit, N, south, X, Y), T1, T2)
	]
).

l_events(
	happens(look(Unit, Type), T1, T2),
	[
		holds(position(Unit, south, X, Y), T1),
		holds(turns(Unit, N), T1),
		less_than(N, 4),
		happens(turn(Unit, N, east, X, Y), T1, T2)
	]
).

l_events(
	happens(look(Unit, Type), T1, T2),
	[
		holds(position(Unit, east, X, Y), T1),
		holds(turns(Unit, N), T1),
		less_than(N, 4),
		happens(turn(Unit, N, north, X, Y), T1, T2)
	]
).

/* Case when too many turns are made */

l_events(
	happens(look(Unit, Type), T1, T2),
	[
		holds(position(Unit, north, X, Y), T1),
		holds(turns(Unit, N), T1),
		greater_or_equal(N, 4),
		decrement(Y, Y2),
		greater_than(Y2, 1),
		happens(walk(Unit, north, X, Y2), T1, T2)
	]
).

l_events(
	happens(look(Unit, Type), T1, T2),
	[
		holds(position(Unit, east, X, Y), T1),
		holds(turns(Unit, N), T1),
		greater_or_equal(N, 4),
		increment(X, X2),
		less_or_equal(X2, 20),
		happens(walk(Unit, east, X2, Y), T1, T2)

	]
).

l_events(
	happens(look(Unit, Type), T1, T2),
	[
		holds(position(Unit, south, X, Y), T1),
		holds(turns(Unit, N), T1),
		greater_or_equal(N, 4),
		increment(Y, Y2),
		less_or_equal(Y2, 20),
		happens(walk(Unit, south, X, Y2), T1, T2)

	]
).

l_events(
	happens(find(Unit, Type), T1, T2),
	[
		holds(position(Unit, west, X, Y), T1),
		holds(turns(Unit, N), T1),
		greater_or_equal(N, 4),
		decrement(X, X2),
		greater_or_equal(X2, 1),
		happens(walk(Unit, west, X2, Y), T1, T2)

	]
).

/* Case for cautious units to escape from attack */

l_events(
	happens(move_away(Unit, north), T1, T2),
	[
		holds(position(Unit, D, X, Y), T1),
		decrement(X, X1),
		happens(walk(Unit, west, X1, Y), T1, T2)
	]
).

l_events(
	happens(move_away(Unit, east), T1, T2),
	[
		holds(position(Unit, D, X, Y), T1),
		decrement(Y, Y1),
		happens(walk(Unit, north, X, Y1), T1, T2)
	]
).

l_events(
	happens(move_away(Unit, south), T1, T2),
	[
		holds(position(Unit, D, X, Y), T1),
		increment(X, X1),
		happens(walk(Unit, east, X1, Y), T1, T2)
	]
).

l_events(
	happens(move_away(Unit, west), T1, T2),
	[
		holds(position(Unit, D, X, Y), T1),
		increment(Y, Y1),
		happens(walk(Unit, south, X, Y1), T1, T2)
	]
).

/* Case where the unit would turn and attack at the same time */

l_events(
	happens(turn_and_attack(Unit, D, Unit1), T1, T2),
	[
		holds(position(Unit, OldD, X, Y), T1),
		has(Unit, weapon(A)),
		power(weapon(A), P),
		holds(in_range(Unit, Unit1, D, A), T1),
		opposite(D, D1),
		holds(health(Unit1, H), T1),
		happens(turn_and_hit(Unit, D, X, Y, P, Unit1, H, D1), T1, T2)
	]

).

/* Case when enemy unit is in shelter */

l_events(
	happens(turn_and_hit(Unit, D, X, Y, P, Unit1, H, D1), T1, T2),
	[
		holds(in_shelter(Unit1), T1),
		holds(shelter(Unit1, H1, X1, Y1), T1),
		decrease(H1, H2, P),
		greater_than(H2, 0),
		happens(turn_hit_shelter(Unit, D, X, Y, Unit1, H2, D1, X1, Y1), T1, T2)
	]
).

l_events(
	happens(turn_and_hit(Unit, D, X, Y, P, Unit1, H, D1), T1, T2),
	[
		holds(in_shelter(Unit1), T1),
		holds(shelter(Unit1, H1, X1, Y1), T1),
		decrease(H1, H2, P),
		less_or_equal(H2, 0),
		happens(turn_hit_shelter(Unit, D, X, Y, Unit1, 0, D1, X1, Y1), T1, T2)
	]
).

/* Case when enemy unit is not in shelter */

l_events(
	happens(turn_and_hit(Unit, D, X, Y, P, Unit1, H, D1), T1, T2),
	[
		holds(not(in_shelter(Unit1)), T1),
		decrease(H, H1, P),
		greater_than(H1, 0),
		happens(turn_hit(Unit, D, X, Y, Unit1, H1, D1), T1, T2)
	]
).

l_events(
	happens(turn_and_hit(Unit, D, X, Y, P, Unit1, H, D1), T1, T2),
	[
		holds(not(in_shelter(Unit1)), T1),
		decrease(H, H1, P),
		less_or_equal(H1, 0),
		happens(turn_hit(Unit, D, X, Y, Unit1, 0, D1), T1, T2)
	]
).

/* Recursive case when the enemy unit is too far away */

l_events(
	happens(turn_and_attack(Unit, north, Unit1), T1, T3),
	[
		holds(position(Unit, Direction, X, Y), T1),
		holds(position(Unit1, D1, X, Y1), T1),
		not_equal(Unit, Unit1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(Y, Y2, Y1),
		greater_than(Y2, U),
		decrement(Y, NewY),
		happens(walk(Unit, north, X, NewY), T1, T2),
		happens(attack(Unit), T2, T3)
	]
).

l_events(
	happens(turn_and_attack(Unit, east, Unit1), T1, T3),
	[
		holds(position(Unit, Direction, X, Y), T1),
		holds(position(Unit1, D1, X1, Y), T1),
		not_equal(Unit, Unit1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(X1, X2, X),
		greater_than(X2, U),
		increment(X, NewX),
		happens(walk(Unit, east, NewX, Y), T1, T2),
		happens(attack(Unit), T2, T3)
	]
).

l_events(
	happens(turn_and_attack(Unit, south, Unit1), T1, T3),
	[
		holds(position(Unit, Direction, X, Y), T1),
		holds(position(Unit1, D1, X, Y1), T1),
		not_equal(Unit, Unit1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(Y1, Y2, Y),
		greater_than(Y2, U),
		increment(Y, NewY),
		happens(walk(Unit, south, X, NewY), T1, T2),
		happens(attack(Unit), T2, T3)

	]
).

l_events(
	happens(turn_and_attack(Unit, west, Unit1), T1, T3),
	[
		holds(position(Unit, Direction, X, Y), T1),
		holds(position(Unit1, D1, X1, Y), T1),
		not_equal(Unit, Unit1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(X, X2, X1),
		greater_than(X2, U),
		decrement(X, NewX),
		happens(walk(Unit, west, NewX, Y), T1, T2),
		happens(attack(Unit), T2, T3)
	]
).


/* Recursive case when enemy unit is too close */

l_events(
	happens(turn_and_attack(Unit, north, Unit1), T1, T3),
	[
		holds(position(Unit, Direction, X, Y), T1),
		holds(position(Unit1, D1, X, Y1), T1),
		not_equal(Unit, Unit1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(Y, Y2, Y1),
		less_than(Y2, L),
		increment(Y, NewY),
		happens(walk(Unit, north, X, NewY), T1, T2),
		happens(attack(Unit), T2, T3)
	]
).

l_events(
	happens(turn_and_attack(Unit, east, Unit1), T1, T3),
	[
		holds(position(Unit, Direction, X, Y), T1),
		holds(position(Unit1, D1, X1, Y), T1),
		not_equal(Unit, Unit1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(X1, X2, X),
		less_than(X2, L),
		decrement(X, NewX),
		happens(walk(Unit, east, NewX, Y), T1, T2),
		happens(attack(Unit), T2, T3)
	]
).

l_events(
	happens(turn_and_attack(Unit, south, Unit1), T1, T3),
	[
		holds(position(Unit, Direction, X, Y), T1),
		holds(position(Unit1, D1, X, Y1), T1),
		not_equal(Unit, Unit1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(Y1, Y2, Y),
		less_than(Y2, L),
		decrement(Y, NewY),
		happens(walk(Unit, south, X, NewY), T1, T2),
		happens(attack(Unit), T2, T3)

	]
).

l_events(
	happens(turn_and_attack(Unit, west, Unit1), T1, T3),
	[
		holds(position(Unit, Direction, X, Y), T1),
		holds(position(Unit1, D1, X1, Y), T1),
		not_equal(Unit, Unit1),
		has(Unit, weapon(A)),
		range(weapon(A), L, U),
		decrease(X, X2, X1),
		less_than(X2, L),
		increment(X, NewX),
		happens(walk(Unit, west, NewX, Y), T1, T2),
		happens(attack(Unit), T2, T3)
	]
).


/* Post conditions used to update the state */

terminated(happens(hit_shelter(Unit, Unit1, H, D, X, Y), T1, T2), shelter(Unit1, H1, X, Y), []).
initiated(happens(hit_shelter(Unit, Unit1, H, D, X, Y), T1, T2), shelter(Unit1, H, X, Y), [greater_than(H, 0)]).

terminated(happens(hit_shelter(Unit, Unit1, H, D, X, Y), T1, T2), cycles(T), []).
initiated(happens(hit_shelter(Unit, Unit1, H, D, X, Y), T1, T2), cycles(T), [modu(T, T1)]).

terminated(happens(hit_shelter(Unit, Unit1, H, D, X, Y), T1, T2), has_shelter(Unit1, true), [equal(H, 0)]).
initiated(happens(hit_shelter(Unit, Unit1, H, D, X, Y), T1, T2), has_shelter(Unit1, false), [equal(H, 0)]).

terminated(happens(hit_shelter(Unit, Unit1, H, D, X, Y), T1, T2), turns(Unit, N), []).
initiated(happens(hit_shelter(Unit, Unit1, H, D, X, Y), T1, T2), turns(Unit, 0), []).

terminated(happens(hit_from(Unit, Unit1, H, D), T1, T2), health(Unit1, H1), []).
initiated(happens(hit_from(Unit, Unit1, H, D), T1, T2), health(Unit1, H), [greater_than(H, 0)]).

terminated(happens(hit_from(Unit, Unit1, H, D), T1, T2), cycles(T), []).
initiated(happens(hit_from(Unit, Unit1, H, D), T1, T2), cycles(T), [modu(T, T1)]).

initiated(happens(hit_from(Unit, Unit1, H, D), T1, T2), dead(Unit1), [equal(H, 0)]).
terminated(happens(hit_from(Unit, Unit1, H, D), T1, T2), person(Unit1, Type), [equal(H, 0)]).
terminated(happens(hit_from(Unit, Unit1, H, D), T1, T2), position(Unit1, Direction, X, Y), [equal(H, 0)]).
terminated(happens(hit_from(Unit, Unit1, H, D), T1, T2), hunger(Unit1, N), [equal(H, 0)]).
terminated(happens(hit_from(Unit, Unit1, H, D), T1, T2), has_shelter(Unit1, Boolean), [equal(H, 0)]).
terminated(happens(hit_from(Unit, Unit1, H, D), T1, T2), shelter(Unit1, N, X, Y), [equal(H, 0)]).
terminated(happens(hit_from(Unit, Unit1, H, D), T1, T2), holds_wood(Unit1, N), [equal(H, 0)]).
terminated(happens(hit_from(Unit, Unit1, H, D), T1, T2), holds_food(Unit1, N, A, B, C), [equal(H, 0)]).
terminated(happens(hit_from(Unit, Unit1, H, D), T1, T2), turns(Unit1, N), [equal(H, 0)]).


terminated(happens(hit_from(Unit, Unit1, H, D), T1, T2), turns(Unit, N), []).
initiated(happens(hit_from(Unit, Unit1, H, D), T1, T2), turns(Unit, 0), []).


terminated(happens(turn_hit_shelter(Unit, D, X, Y, Unit1, H, D1, X1, Y1), T1, T2), shelter(Unit1, H1, X1, Y1), []).
initiated(happens(turn_hit_shelter(Unit, D, X, Y, Unit1, H, D1, X1, Y1), T1, T2), shelter(Unit1, H, X1, Y1), [greater_than(H, 0)]).

terminated(happens(turn_hit_shelter(Unit, D, X, Y, Unit1, H, D1, X1, Y1), T1, T2), has_shelter(Unit1, true), [equal(H, 0)]).
initiated(happens(turn_hit_shelter(Unit, D, X, Y, Unit1, H, D1, X1, Y1), T1, T2), has_shelter(Unit1, false), [equal(H, 0)]).


terminated(happens(turn_hit_shelter(Unit, D, X, Y, Unit1, H, D1, X1, Y1), T1, T2), position(Unit, OldD, OldX, OldY), []).
initiated(happens(turn_hit_shelter(Unit, D, X, Y, Unit1, H, D1, X1, Y1), T1, T2), position(Unit, D, X, Y), []).

terminated(happens(turn_hit_shelter(Unit, D, X, Y, Unit1, H, D1, X1, Y1), T1, T2), cycles(T), []).
initiated(happens(turn_hit_shelter(Unit, D, X, Y, Unit1, H, D1, X1, Y1), T1, T2), cycles(T), [modu(T, T1)]).

terminated(happens(turn_hit_shelter(Unit, D, X, Y, Unit1, H, D1, X1, Y1), T1, T2), turns(Unit, N), []).
initiated(happens(turn_hit_shelter(Unit, D, X, Y, Unit1, H, D1, X1, Y1), T1, T2), turns(Unit, 0), []).


terminated(happens(turn_hit(Unit, D, X, Y, Unit1, H, D1), T1, T2), health(Unit1, H1), []).
initiated(happens(turn_hit(Unit, D, X, Y, Unit1, H, D1), T1, T2), health(Unit1, H), [greater_than(H, 0)]).

terminated(happens(turn_hit(Unit, D, X, Y, Unit1, H, D1), T1, T2), position(Unit, OldD, OldX, OldY), []).
initiated(happens(turn_hit(Unit, D, X, Y, Unit1, H, D1), T1, T2), position(Unit, D, X, Y), []).

terminated(happens(turn_hit(Unit, D, X, Y, Unit1, H, D1), T1, T2), cycles(T), []).
initiated(happens(turn_hit(Unit, D, X, Y, Unit1, H, D1), T1, T2), cycles(T), [modu(T, T1)]).

initiated(happens(turn_hit(Unit, D, X, Y, Unit1, H, D1), T1, T2), dead(Unit1), [equal(H, 0)]).
terminated(happens(turn_hit(Unit, D, X, Y, Unit1, H, D1), T1, T2), person(Unit1, Type), [equal(H, 0)]).
terminated(happens(turn_hit(Unit, D, X, Y, Unit1, H, D1), T1, T2), position(Unit1, Direction, X1, Y1), [equal(H, 0)]).
terminated(happens(turn_hit(Unit, D, X, Y, Unit1, H, D1), T1, T2), hunger(Unit1, N), [equal(H, 0)]).
terminated(happens(turn_hit(Unit, D, X, Y, Unit1, H, D1), T1, T2), has_shelter(Unit1, Boolean), [equal(H, 0)]).
terminated(happens(turn_hit(Unit, D, X, Y, Unit1, H, D1), T1, T2), shelter(Unit1, N, X, Y), [equal(H, 0)]).
terminated(happens(turn_hit(Unit, D, X, Y, Unit1, H, D1), T1, T2), holds_wood(Unit1, N), [equal(H, 0)]).
terminated(happens(turn_hit(Unit, D, X, Y, Unit1, H, D1), T1, T2), holds_food(Unit1, N, A, B, C), [equal(H, 0)]).
terminated(happens(turn_hit(Unit, D, X, Y, Unit1, H, D1), T1, T2), turns(Unit1, N), [equal(H, 0)]).

terminated(happens(turn_hit(Unit, D, X, Y, Unit1, H, D1), T1, T2), turns(Unit, N), []).
initiated(happens(turn_hit(Unit, D, X, Y, Unit1, H, D1), T1, T2), turns(Unit, 0), []).

/************************** Logic for reducing hunger and reducing health if hunger is too low **************************/

action(reduce_hunger(_, _)).

action(need_food(_)).

action(check_hunger(_)).

action(lower_health(_,_)).

action(lower_hunger(_, _)).

/* Reactive rules */

reactive_rule(
	[
		holds(cycles(7), T1),
		holds(hunger(alex, N), T1)
	],
	[
		happens(lower_hunger(alex, N), T1, T2)
	],
	99
).

reactive_rule(
	[
		holds(cycles(7), T1),
		holds(hunger(amanda, N), T1)
	],
	[
		happens(lower_hunger(amanda, N), T1, T2)
	],
	99
).

reactive_rule(
	[
		holds(cycles(7), T1),
		holds(hunger(katherine, N), T1)
	],
	[
		happens(lower_hunger(katherine, N), T2, T3)
	],
	99
).

reactive_rule(
	[
		holds(cycles(7), T1),
		holds(hunger(peter, N), T1)
	],
	[
		happens(lower_hunger(peter, N), T2, T3)
	],
	99
).

reactive_rule(
	[
		holds(cycles(7), T1),
		holds(hunger(tom, N), T1)
	],
	[
		happens(lower_hunger(tom, N), T2, T3)
	],
	99
).



reactive_rule(
	[
		happens(reduce_hunger(Unit, N), T1, T2)
	],
	[
		happens(check_hunger(Unit), T3, T4)
	],
	91
).

/* Case where hunger is above zero */

l_events(
	happens(lower_hunger(Unit, N), T1, T2),
	[
		decrement(N, N1),
		greater_than(N1, 0),
		happens(reduce_hunger(Unit, N1), T1, T2)
	]
).

/* Case when hunger is already zero, then initialise zero */

l_events(
	happens(lower_hunger(Unit, N), T1, T2),
	[
		decrement(N, N1),
		less_or_equal(N1, 0),
		happens(reduce_hunger(Unit, 0), T1, T2)
	]
).

/* Base Case */

l_events(
	happens(check_hunger(Unit), T1, T2),
	[
		holds(hunger(Unit, N), T1),
		greater_or_equal(N, 10)
	]
).

/* Recursive as health lowers every cycle */

/* Case where hunger is low, then lower health */


l_events(
	happens(check_hunger(Unit), T1, T3),
	[
		holds(hunger(Unit, N), T1),
		less_than(N, 10),
		holds(health(Unit, H), T1),
		decrement(H, H1),
		greater_than(H1, 0),
		happens(lower_health(Unit, H1), T1, T2),
		happens(check_hunger(Unit), T2, T3)
	]
).

/* Case when hunger is low and health is already zero, then initialise zero */

l_events(
	happens(check_hunger(Unit), T1, T3),
	[
		holds(hunger(Unit, N), T1),
		less_than(N, 10),
		holds(health(Unit, H), T1),
		decrement(H, H1),
		less_or_equal(H1, 0),
		happens(lower_health(Unit, 0), T1, T2),
		happens(check_hunger(Unit), T2, T3)
	]
).

terminated(happens(lower_health(Unit, H), T1, T2), health(Unit, H1), []).
initiated(happens(lower_health(Unit, H), T1, T2), health(Unit, H), [greater_than(H, 0)]).

initiated(happens(lower_health(Unit, H), T1, T2), cycles(T), [modu(T, T1)]).
terminated(happens(lower_health(Unit, H), T1, T2), cycles(T), []).

terminated(happens(lower_health(Unit, H), T1, T2), turns(Unit, N1), []).
initiated(happens(lower_health(Unit, H), T1, T2), turns(Unit, 0), []).

initiated(happens(lower_health(Unit, H), T1, T2), dead(Unit), [equal(H, 0)]).
terminated(happens(lower_health(Unit, H), T1, T2), person(Unit, Type), [equal(H, 0)]).
terminated(happens(lower_health(Unit, H), T1, T2), position(Unit, Direction, X, Y), [equal(H, 0)]).
terminated(happens(lower_health(Unit, H), T1, T2), hunger(Unit, N), [equal(H, 0)]).
terminated(happens(lower_health(Unit, H), T1, T2), has_shelter(Unit, Boolean), [equal(H, 0)]).
terminated(happens(lower_health(Unit, H), T1, T2), shelter(Unit, N, X, Y), [equal(H, 0)]).
terminated(happens(lower_health(Unit, H), T1, T2), holds_wood(Unit, N), [equal(H, 0)]).
terminated(happens(lower_health(Unit, H), T1, T2), holds_food(Unit, N, A, B, C), [equal(H, 0)]).
terminated(happens(lower_health(Unit, H), T1, T2), turns(Unit, N), [equal(H, 0)]).

terminated(happens(reduce_hunger(Unit, N), T1, T2), hunger(Unit, N1), []).
initiated(happens(reduce_hunger(Unit, N), T1, T2), hunger(Unit, N), []).

terminated(happens(reduce_hunger(Unit, N), T1, T2), cycles(T), []).
initiated(happens(reduce_hunger(Unit, N), T1, T2),  cycles(T), [modu(T, T1)]).

terminated(happens(reduce_hunger(Unit, N), T1, T2), turns(Unit, N1), []).
initiated(happens(reduce_hunger(Unit, N), T1, T2), turns(Unit, 0), []).

/********************************************* Logic for eating food *********************************************/

action(consume_food(_)).

action(eat_food(_)).

action(eat(_, _, _, _, _, _, _)).

/* Reactive rule */

reactive_rule(
	[
		happens(reduce_hunger(Unit, N), T1, T2)
	],
	[
		happens(consume_food(Unit), T3, T4)
	],
	96
).

/* Cautious unit case */

l_events(
	happens(consume_food(Unit), T1, T2),
	[
		holds(person(Unit, cautious), T1),
		holds(hunger(Unit, F), T1),
		greater_than(F, 15)
	]
).

l_events(
	happens(consume_food(Unit), T1, T2),
	[
		holds(person(Unit, cautious), T1),
		holds(hunger(Unit, F), T1),
		less_or_equal(F, 15),
		holds(holds_food(Unit, N, A, B, C), T1),
		greater_or_equal(N, 1),
		happens(eat_food(Unit), T1, T2)
	]
).

/* Normal unit case */

l_events(
	happens(consume_food(Unit), T1, T2),
	[
		holds(person(Unit, normal), T1),
		holds(hunger(Unit, F), T1),
		greater_than(F, 12)
	]
).

l_events(
	happens(consume_food(Unit), T1, T2),
	[
		holds(person(Unit, normal), T1),
		holds(hunger(Unit, F), T1),
		less_or_equal(F, 12),
		holds(holds_food(Unit, N, A, B, C), T1),
		greater_or_equal(N, 1),
		happens(eat_food(Unit), T1, T2)
	]
).

/* Violent unit case */
l_events(
	happens(consume_food(Unit), T1, T2),
	[
		holds(person(Unit, violent), T1),
		holds(hunger(Unit, F), T1),
		greater_than(F, 9)
	]
).


l_events(
	happens(consume_food(Unit), T1, T2),
	[
		holds(person(Unit, violent), T1),
		holds(hunger(Unit, F), T1),
		less_or_equal(F, 9),
		holds(holds_food(Unit, N, A, B, C), T1),
		greater_or_equal(N, 1),
		happens(eat_food(Unit), T1, T2)
	]
).



/* Base cases */
l_events(
	happens(eat_food(Unit), T1, T2),
	[
		holds(hunger(Unit, 20), T1)
	]
).

/* Different recursive cases of eating depending on what food points the unit is holding */

l_events(
	happens(eat_food(Unit), T1, T3),
	[
		holds(hunger(Unit, F), T1),
		decrease(20, F1, F),
		greater_or_equal(F1, 5),
		holds(holds_food(Unit, N, A, B, C), T1),
		greater_or_equal(C, 1),
		happens(eat(Unit, 5, F, N, A, B, C), T1, T2),
		happens(eat_food(Unit), T2, T3)
	]
).

l_events(
	happens(eat_food(Unit), T1, T3),
	[
		holds(hunger(Unit, F), T1),
		decrease(20, F1, F),
		greater_or_equal(F1, 5),
		holds(holds_food(Unit, N, A, B, C), T1),
		equal(C, 0),
		greater_or_equal(B, 1),
		happens(eat(Unit, 3, F, N, A, B, C), T1, T2),
		happens(eat_food(Unit), T2, T3)
	]
).

l_events(
	happens(eat_food(Unit), T1, T3),
	[
		holds(hunger(Unit, F), T1),
		decrease(20, F1, F),
		greater_or_equal(F1, 5),
		holds(holds_food(Unit, N, A, B, C), T1),
		equal(C, 0),
		equal(B, 0),
		greater_or_equal(A, 1),
		happens(eat(Unit, 1, F, N, A, B, C), T1, T2),
		happens(eat_food(Unit), T2, T3)
	]
).

l_events(
	happens(eat_food(Unit), T1, T3),
	[
		holds(hunger(Unit, F), T1),
		decrease(20, F1, F),
		less_than(F1, 5),
		greater_or_equal(F1, 3),
		holds(holds_food(Unit, N, A, B, C), T1),
		greater_or_equal(B, 1),
		happens(eat(Unit, 3, F, N, A, B, C), T1, T2),
		happens(eat_food(Unit), T2, T3)
	]
).

l_events(
	happens(eat_food(Unit), T1, T3),
	[
		holds(hunger(Unit, F), T1),
		decrease(20, F1, F),
		less_than(F1, 5),
		greater_or_equal(F1, 3),
		holds(holds_food(Unit, N, A, B, C), T1),
		equal(B, 0),
		greater_or_equal(A, 1),
		happens(eat(Unit, 1, F, N, A, B, C), T1, T2),
		happens(eat_food(Unit), T2, T3)
	]
).

l_events(
	happens(eat_food(Unit), T1, T3),
	[
		holds(hunger(Unit, F), T1),
		decrease(20, F1, F),
		less_than(F1, 3),
		greater_or_equal(F1, 1),
		holds(holds_food(Unit, N, A, B, C), T1),
		greater_or_equal(A, 1),
		happens(eat(Unit, 1, F, N, A, B, C), T1, T2),
		happens(eat_food(Unit), T2, T3)
	]
).

/* Post conditions to update the state */

terminated(happens(eat(Unit, V, F, N, A, B, C), T1, T2), hunger(Unit, F), []).
initiated(happens(eat(Unit, V, F, N, A, B, C), T1, T2), hunger(Unit, F1), [increase(F, F1, V)]).

terminated(happens(eat(Unit, 5, F, N, A, B, C), T1, T2), holds_food(Unit, N, A, B, C), []).
initiated(happens(eat(Unit, 5, F, N, A, B, C), T1, T2), holds_food(Unit, N1, A, B, C1), [decrease(N, N1, 5), decrement(C, C1)]).

terminated(happens(eat(Unit, 3, F, N, A, B, C), T1, T2), holds_food(Unit, N, A, B, C), []).
initiated(happens(eat(Unit, 3, F, N, A, B, C), T1, T2), holds_food(Unit, N1, A, B1, C), [decrease(N, N1, 3), decrement(B, B1)]).

terminated(happens(eat(Unit, 1, F, N, A, B, C), T1, T2), holds_food(Unit, N, A, B, C), []).
initiated(happens(eat(Unit, 1, F, N, A, B, C), T1, T2), holds_food(Unit, N1, A1, B, C), [decrease(N, N1, 1), decrement(A, A1)]).

terminated(happens(eat(Unit, V, F, N, A, B, C), T1, T2), turns(Unit, N1), []).
initiated(happens(eat(Unit, V, F, N, A, B, C), T1, T2), turns(Unit, 0), []).

initiated(happens(eat(Unit, V, F, N, A, B, C), T1, T2), cycles(T), [modu(T, T1)]).
terminated(happens(eat(Unit, V, F, N, A, B, C), T1, T2), cycles(T), []).


/********************************************* Logic for getting more food *********************************************/


action(check_food(_)).

reactive_rule(
	[
		happens(eat(Unit, V, F, N, A, B, C), T1, T2)

	],
	[
		happens(check_food(Unit), T3, T4)
	],
	94
).

/* Base Case */

l_events(
	happens(check_food(Unit), T1, T2),
	[
		holds(person(Unit, cautious), T1),
		holds(holds_food(Unit, N, A, B, C), T1),
		greater_or_equal(N, 10)
	]
).

l_events(
	happens(check_food(Unit), T1, T2),
	[
		holds(person(Unit, normal), T1),
		holds(holds_food(Unit, N, A, B, C), T1),
		greater_or_equal(N, 5)
	]
).

l_events(
	happens(check_food(Unit), T1, T2),
	[
		holds(person(Unit, violent), T1),
		holds(holds_food(Unit, N, A, B, C), T1),
		greater_or_equal(N, 0)
	]
).

/* Recursive Case */

l_events(
	happens(check_food(Unit), T1, T3),
	[
		holds(person(Unit, cautious), T1),
		holds(holds_food(Unit, N, A, B, C), T1),
		less_than(N, 10),
		happens(get_food(Unit), T1, T2),
		happens(check_food(Unit), T2, T3)
	]
).

l_events(
	happens(check_food(Unit), T1, T3),
	[
		holds(person(Unit, normal), T1),
		holds(holds_food(Unit, N, A, B, C), T1),
		less_than(N, 5),
		happens(get_food(Unit), T1, T2),
		happens(check_food(Unit), T2, T3)
	]
).

l_events(
	happens(check_food(Unit), T1, T3),
	[
		holds(person(Unit, violent), T1),
		holds(holds_food(Unit, N, A, B, C), T1),
		less_than(N, 0),
		happens(get_food(Unit), T1, T2),
		happens(check_food(Unit), T2, T3)
	]
).


/********************************************* Logic for fire *********************************************/
event(fire(_,_)).
action(on_fire(_,_)).
action(burn(_,_)).
reactive_rule(
	[
		happens(fire(X, Y), T1, T2)
	],
	[
		happens(on_fire(X, Y), T3, T4)
	],
	99
).

l_events(
	happens(on_fire(X, Y), T1, T2),
	[
		holds(position(Unit, D, X, Y), T1),
		holds(health(Unit, N), T1),
		decrease(N, N1, 10),
		happens(burn(Unit, N1), T1, T2)
	]
).

terminated(happens(burn(Unit, H), T1, T2), health(Unit, N1), []).
initiated(happens(burn(Unit, H), T1, T2), health(Unit, H), [greater_than(H, 0)]).

terminated(happens(burn(Unit, H), T1, T2), turns(Unit, N1), []).
initiated(happens(burn(Unit, H), T1, T2), turns(Unit, 0), []).

initiated(happens(burn(Unit, H), T1, T2), cycles(T), [modu(T, T1)]).
terminated(happens(burn(Unit, H), T1, T2), cycles(T), []).

initiated(happens(burn(Unit, H), T1, T2), dead(Unit), [equal(H, 0)]).
terminated(happens(burn(Unit, H), T1, T2), person(Unit, Type), [equal(H, 0)]).
terminated(happens(burn(Unit, H), T1, T2), position(Unit, Direction, X, Y), [equal(H, 0)]).
terminated(happens(burn(Unit, H), T1, T2), hunger(Unit, N), [equal(H, 0)]).
terminated(happens(burn(Unit, H), T1, T2), has_shelter(Unit, Boolean), [equal(H, 0)]).
terminated(happens(burn(Unit, H), T1, T2), shelter(Unit, N, X, Y), [equal(H, 0)]).
terminated(happens(burn(Unit, H), T1, T2), holds_wood(Unit, N), [equal(H, 0)]).
terminated(happens(burn(Unit, H), T1, T2), holds_food(Unit, N, A, B, C), [equal(H, 0)]).
terminated(happens(burn(Unit, H), T1, T2), turns(Unit, N), [equal(H, 0)]).

/**************************************** Logic for unit to heal ****************************************/


action(check_health(_)).
action(heal(_, _)).

reactive_rule(
	[
		holds(hunger(Unit, 20), T1)
	],
	[
		happens(check_health(Unit), T1, T2)
	],
	90
).


l_events(
	happens(check_health(Unit), T1, T2),
	[
		holds(health(Unit, H), T1),
		less_than(H, 50),
		happens(heal(Unit, H), T1, T2)
	]
).

l_events(
	happens(check_health(Unit), T1, T2),
	[
		holds(health(Unit, 50), T1)
	]

).

terminated(happens(heal(Unit, H), T1, T2), health(Unit, H), []).
initiated(happens(heal(Unit, H), T1, T2), health(Unit, H1), [increment(H, H1)]).

terminated(happens(heal(Unit, H), T1, T2), turns(Unit, N), []).
initiated(happens(heal(Unit, H), T1, T2), turns(Unit, 0), []).

initiated(happens(heal(Unit, H), T1, T2), cycles(T), [modu(T, T1)]).
terminated(happens(heal(Unit, H), T1, T2), cycles(T), []).

/**************************************** Logic for unit to garrison in shelter ****************************************/

reactive_rule(
	[
		happens(successful_shelter(Unit, N, X, Y), T1, T2),
		holds(person(Unit, cautious), T2)
	],
	[
		happens(go_to_shelter(Unit), T3, T4)
	],
	91
).

fluent(in_shelter(_)).

l_events(
	happens(go_to_shelter(Unit), T1, T2),
	[
		holds(in_shelter(Unit), T1)
	]
).

/* Cases when next to shelter */

l_events(
	happens(go_to_shelter(Unit), T1, T3),
	[
		holds(position(Unit, north, X, Y), T1),
		decrement(Y, Y1),
		holds(shelter(Unit, H, X, Y1), T1),
		happens(walk(Unit, north, X, Y1), T1, T2),
		happens(go_to_shelter(Unit), T2, T3)
	]
).

l_events(
	happens(go_to_shelter(Unit), T1, T3),
	[
		holds(position(Unit, south, X, Y), T1),
		increment(Y, Y1),
		holds(shelter(Unit, H, X, Y1), T1),
		happens(walk(Unit, south, X, Y1), T1, T2),
		happens(go_to_shelter(Unit), T2, T3)
	]

).


l_events(
	happens(go_to_shelter(Unit), T1, T3),
	[
		holds(position(Unit, east, X, Y), T1),
		increment(X, X1),
		holds(shelter(Unit, H, X1, Y), T1),
		happens(walk(Unit, east, X1, Y), T1, T2),
		happens(go_to_shelter(Unit), T2, T3)
	]

).

l_events(
	happens(go_to_shelter(Unit), T1, T3),
	[
		holds(position(Unit, west, X, Y), T1),
		decrement(X, X1),
		holds(shelter(Unit, H, X1, Y), T1),
		happens(walk(Unit, west, X1, Y), T1, T2),
		happens(go_to_shelter(Unit), T2, T3)
	]

).

/* Cases when not next to shelter, then go towards it */

l_events(
	happens(go_to_shelter(Unit), T1, T3),
	[
		holds(position(Unit, D, X, Y), T1),
		holds(shelter(Unit, H, X1, Y1), T1),
		decrease(Y, Y2, Y1),
		greater_than(Y2, 1),
		decrement(Y, Y3),		
		happens(walk(Unit, north, X, Y3), T1, T2),
		happens(go_to_shelter(Unit), T2, T3)
	]
).

l_events(
	happens(go_to_shelter(Unit), T1, T3),
	[
		holds(position(Unit, D, X, Y), T1),
		holds(shelter(Unit, H, X1, Y1), T1),
		decrease(Y1, Y2, Y),
		greater_than(Y2, 1),
		increment(Y, Y3),		
		happens(walk(Unit, south, X, Y3), T1, T2),
		happens(go_to_shelter(Unit), T2, T3)
	]
).

l_events(
	happens(go_to_shelter(Unit), T1, T3),
	[
		holds(position(Unit, D, X, Y), T1),
		holds(shelter(Unit, H, X1, Y1), T1),
		decrease(X1, X2, X),
		greater_than(X2, 1),
		increment(X, X3),		
		happens(walk(Unit, east, X3, Y), T1, T2),
		happens(go_to_shelter(Unit), T2, T3)
	]
).

l_events(
	happens(go_to_shelter(Unit), T1, T3),
	[
		holds(position(Unit, D, X, Y), T1),
		holds(shelter(Unit, H, X1, Y1), T1),
		decrease(X, X2, X1),
		greater_than(X2, 1),
		decrement(X, X3),		
		happens(walk(Unit, west, X3, Y), T1, T2),
		happens(go_to_shelter(Unit), T2, T3)
	]
).

l_int(
	holds(in_shelter(Unit), T),
	[
		holds(position(Unit, D, X, Y), T),
		holds(shelter(Unit, H, X, Y), T)
	]
).




/* Observations */


observe(
	[
		start_game(amanda),
		start_game(katherine),
		start_game(peter),
		start_game(tom),
		start_game(alex)
		
	], 
	1

).



observe([], 2).

observe([], 3).

observe([], 4).

observe([], 5).

observe([], T) :- T <15. % down from 30, slowing down around cycles 24 or so; future tests will deal with THAT
