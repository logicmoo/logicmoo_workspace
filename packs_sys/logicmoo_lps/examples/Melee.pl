
:- expects_dialect(lps).


/* output from ?- dumplps*/

observe([start_game(amanda),start_game(katherine),start_game(peter),start_game(tom),start_game(alex)],2).
observe([],3).
observe([],4).
observe([],5).
observe([in_sight(katherine,person)],25).
% observe([],T) :- T<15.

next_to(Unit,Type)at T if
    position(Unit,south,X,Y)at T,
    increment(Y,Y1),
    item(Type,X,Y1)at T.
next_to(Unit,Type)at T if
    position(Unit,north,X,Y)at T,
    decrement(Y,Y1),
    item(Type,X,Y1)at T.
next_to(Unit,Type)at T if
    position(Unit,east,X,Y)at T,
    increment(X,X1),
    item(Type,X1,Y)at T.
next_to(Unit,Type)at T if
    position(Unit,west,X,Y)at T,
    decrement(X,X1),
    item(Type,X1,Y)at T.
in_sight(Unit,Type)at T if
    position(Unit,east,X,Y)at T,
    item(Type,A,Y)at T,
    greater_than(A,X),
    increase(X,X1,6),
    less_or_equal(A,X1).
in_sight(Unit,Type)at T if
    position(Unit,north,X,Y)at T,
    item(Type,X,A)at T,
    less_than(A,Y),
    decrease(Y,Y1,6),
    greater_or_equal(A,Y1).
in_sight(Unit,Type)at T if
    position(Unit,south,X,Y)at T,
    item(Type,X,A)at T,
    greater_than(A,Y),
    increase(Y,Y1,6),
    less_or_equal(A,Y1).
in_sight(Unit,Type)at T if
    position(Unit,west,X,Y)at T,
    item(Type,A,Y)at T,
    less_than(A,X),
    decrease(X,X1,6),
    greater_or_equal(A,X1).
item(tree,X,Y)at T if
    tree(X,Y)at T.
item(tree,X,Y)at T if
    wood(X,Y)at T.
item(animal,X,Y)at T if
    animal(Type,N,D,X,Y)at T.
item(animal,X,Y)at T if
    food(A,B,C,X,Y)at T.
item(food,X,Y)at T if
    food(A,B,C,X,Y)at T.
item(person,X,Y)at T if
    position(P,D,X,Y)at T.
item(wood,X,Y)at T if
    wood(X,Y)at T.
in_view(Unit,east,Type)at T if
    position(Unit,D,X,Y)at T,
    item(Type,A,Y)at T,
    greater_than(A,X),
    increase(X,X1,6),
    less_or_equal(A,X1).
in_view(Unit,north,Type)at T if
    position(Unit,D,X,Y)at T,
    item(Type,X,A)at T,
    less_than(A,Y),
    decrease(Y,Y1,6),
    greater_or_equal(A,Y1).
in_view(Unit,south,Type)at T if
    position(Unit,D,X,Y)at T,
    item(Type,X,A)at T,
    greater_than(A,Y),
    increase(Y,Y1,6),
    less_or_equal(A,Y1).
in_view(Unit,west,Type)at T if
    position(Unit,D,X,Y)at T,
    item(Type,A,Y)at T,
    less_than(A,X),
    decrease(X,X1,6),
    greater_or_equal(A,X1).
in_range(Unit,Unit1,north,A)at T1 if
    position(Unit,D,X,Y)at T1,
    position(Unit1,D1,X,Y1)at T1,
    not_equal(Unit,Unit1),
    range(weapon(A),L,U),
    decrease(Y,Y2,Y1),
    greater_or_equal(Y2,L),
    less_or_equal(Y2,U).
in_range(Unit,Unit1,east,A)at T1 if
    position(Unit,D,X,Y)at T1,
    position(Unit1,D1,X1,Y)at T1,
    not_equal(Unit,Unit1),
    range(weapon(A),L,U),
    decrease(X1,X2,X),
    greater_or_equal(X2,L),
    less_or_equal(X2,U).
in_range(Unit,Unit1,south,A)at T1 if
    position(Unit,D,X,Y)at T1,
    position(Unit1,D1,X,Y1)at T1,
    not_equal(Unit,Unit1),
    range(weapon(A),L,U),
    decrease(Y1,Y2,Y),
    greater_or_equal(Y2,L),
    less_or_equal(Y2,U).
in_range(Unit,Unit1,west,A)at T1 if
    position(Unit,D,X,Y)at T1,
    position(Unit1,D1,X1,Y)at T1,
    not_equal(Unit,Unit1),
    range(weapon(A),L,U),
    decrease(X,X2,X1),
    greater_or_equal(X2,L),
    less_or_equal(X2,U).
in_range(Unit,animal,north,A)at T1 if
    position(Unit,D,X,Y)at T1,
    animal(Type,H,D1,X,Y1)at T1,
    range(weapon(A),L,U),
    decrease(Y,Y2,Y1),
    greater_or_equal(Y2,L),
    less_or_equal(Y2,U).
in_range(Unit,animal,east,A)at T1 if
    position(Unit,D,X,Y)at T1,
    animal(Type,H,D1,X1,Y)at T1,
    range(weapon(A),L,U),
    decrease(X1,X2,X),
    greater_or_equal(X2,L),
    less_or_equal(X2,U).
in_range(Unit,animal,south,A)at T1 if
    position(Unit,D,X,Y)at T1,
    animal(Type,H,D1,X,Y1)at T1,
    range(weapon(A),L,U),
    decrease(Y1,Y2,Y),
    greater_or_equal(Y2,L),
    less_or_equal(Y2,U).
in_range(Unit,animal,west,A)at T1 if
    position(Unit,D,X,Y)at T1,
    animal(Type,H,D1,X1,Y)at T1,
    range(weapon(A),L,U),
    decrease(X,X2,X1),
    greater_or_equal(X2,L),
    less_or_equal(X2,U).
in_shelter(Unit)at T if
    position(Unit,D,X,Y)at T,
    shelter(Unit,H,X,Y)at T.
initially
    person(amanda,cautious),
    person(katherine,normal),
    person(peter,normal),
    person(tom,normal),
    person(alex,violent),
    position(amanda,north,3,3),
    holds_wood(amanda,0),
    holds_food(amanda,0,0,0,0),
    health(amanda,50),
    hunger(amanda,20),
    has_shelter(amanda,true),
    shelter(amanda,100,4,5),
    turns(amanda,0),
    position(katherine,east,4,18),
    holds_wood(katherine,0),
    holds_food(katherine,0,0,0,0),
    health(katherine,50),
    hunger(katherine,20),
    has_shelter(katherine,true),
    shelter(katherine,100,8,19),
    turns(katherine,0),
    position(peter,north,17,3),
    holds_wood(peter,0),
    holds_food(peter,0,0,0,0),
    health(peter,50),
    hunger(peter,20),
    has_shelter(peter,true),
    shelter(peter,100,13,4),
    turns(peter,0),
    position(tom,west,19,18),
    holds_wood(tom,0),
    holds_food(tom,0,0,0,0),
    health(tom,50),
    hunger(tom,20),
    has_shelter(tom,true),
    shelter(tom,100,14,20),
    turns(tom,0),
    position(alex,north,10,9),
    holds_wood(alex,0),
    holds_food(alex,10,0,0,2),
    health(alex,50),
    hunger(alex,20),
    has_shelter(alex,false),
    turns(alex,0),
    animal(rabbit,5,west,5,7),
    animal(rabbit,5,east,12,8),
    animal(rabbit,5,west,4,9),
    animal(rabbit,5,south,1,10),
    animal(rabbit,5,west,6,13),
    animal(rabbit,5,north,14,16),
    animal(rabbit,5,north,9,16),
    animal(chicken,10,north,7,6),
    animal(chicken,10,east,16,6),
    animal(chicken,10,east,8,8),
    animal(chicken,10,west,2,13),
    animal(chicken,10,east,3,15),
    animal(chicken,10,south,12,15),
    animal(chicken,10,south,13,18),
    animal(cow,15,north,9,3),
    animal(cow,15,north,18,8),
    animal(cow,15,south,17,13),
    animal(cow,15,west,6,16),
    food(2,1,0,3,8),
    food(2,1,1,15,9),
    food(2,1,2,20,9),
    food(2,1,0,8,14),
    tree(1,1),
    tree(4,1),
    tree(11,1),
    tree(18,1),
    tree(2,2),
    tree(13,2),
    tree(6,3),
    tree(19,4),
    tree(1,5),
    tree(11,6),
    tree(13,7),
    tree(12,10),
    tree(8,11),
    tree(11,12),
    tree(14,12),
    tree(19,14),
    tree(1,15),
    tree(3,17),
    tree(1,19),
    tree(6,19),
    tree(4,20),
    tree(8,20),
    tree(17,20),
    tree(20,20),
    wood(7,1),
    wood(4,11),
    wood(18,11),
    wood(10,14),
    cycles(0).

has(amanda,weapon(spear)).
power(weapon(spear),6).
range(weapon(spear),1,3).
has(katherine,weapon(bow)).
power(weapon(bow),6).
range(weapon(bow),1,6).
has(peter,weapon(bombs)).
power(weapon(bombs),8).
range(weapon(bombs),6,6).
has(tom,weapon(whip)).
power(weapon(whip),4).
range(weapon(whip),1,3).
has(alex,weapon(sword)).
power(weapon(sword),8).
range(weapon(sword),1,1).
less_than(X,Y) :- X<Y.
less_or_equal(X,Y) :- X=<Y.
greater_or_equal(X,Y) :- X>=Y.
greater_than(X,Y) :- X>Y.
equal(X,Y) :- X==Y.
not_equal(X,Y) :- X\=Y.
decrement(X,X1) :- X1 is X-1.
increment(X,X1) :- X1 is X+1.
decrease(X,X1,N) :- X1 is X-N.
increase(X,X1,N) :- X1 is X+N.
calculate(N,A,B,C) :- N is A*1+B*3+C*5.
modu(X,X1) :- X is X1 mod 8.
opposite(north,south).
opposite(east,west).
opposite(south,north).
opposite(west,east).

action(find(_24248,_24250)).
action(turn(_24248,_24250,_24252,_24254,_24256)).
action(walk_towards(_24248,_24250)).
action(walk(_24248,_24250,_24252,_24254)).
action(make_shelter(_24248)).
action(build_shelter(_24248,_24250)).
action(get_wood(_24248)).
action(successful_shelter(_24248,_24250,_24252,_24254)).
action(break_tree(_24248,_24250,_24252)).
action(collect_wood(_24248,_24250,_24252,_24254)).
action(hit_tree(_24248)).
action(pick_wood(_24248)).
action(get_food(_24248)).
action(hit_animal(_24248)).
action(pick_food(_24248)).
action(pick_up_food(_24248,_24250)).
action(collect_food(_24248,_24250,_24252,_24254,_24256,_24258,_24260,_24262,_24264)).
action(kill_animal(_24248)).
action(reduce_health(_24248,_24250)).
action(check_animal(_24248,_24250,_24252,_24254,_24256,_24258,_24260)).
action(change_animal(_24248,_24250,_24252,_24254,_24256,_24258,_24260)).
action(attack(_24248)).
action(hit_from(_24248,_24250,_24252,_24254)).
action(walk_backward(_24248,_24250,_24252,_24254)).
action(hit(_24248,_24250,_24252,_24254,_24256)).
action(hit_shelter(_24248,_24250,_24252,_24254,_24256,_24258)).
action(move_away(_24248,_24250)).
action(turn_and_hit(_24248,_24250,_24252,_24254,_24256,_24258,_24260,_24262)).
action(turn_hit_shelter(_24248,_24250,_24252,_24254,_24256,_24258,_24260,_24262,_24264)).
action(turn_hit(_24248,_24250,_24252,_24254,_24256,_24258,_24260)).
action(walk_away(_24248)).
action(look(_24248,_24250)).
action(reduce_hunger(_24248,_24250)).
action(need_food(_24248)).
action(check_hunger(_24248)).
action(lower_health(_24248,_24250)).
action(lower_hunger(_24248,_24250)).
action(consume_food(_24248)).
action(eat_food(_24248)).
action(eat(_24248,_24250,_24252,_24254,_24256,_24258,_24260)).
action(check_food(_24248)).
action(on_fire(_24248,_24250)).
action(burn(_24248,_24250)).
action(check_health(_24248)).
action(heal(_24248,_24250)).
find(Unit,Type)from T1 to T2 if
    in_sight(Unit,Type)at T1.
find(Unit,Type)from T1 to T3 if
    position(Unit,north,X,Y)at T1,
    turns(Unit,N)at T1,
    less_than(N,4),
    turn(Unit,N,west,X,Y)from T1 to T2,
    find(Unit,Type)from T2 to T3.
find(Unit,Type)from T1 to T3 if
    position(Unit,west,X,Y)at T1,
    turns(Unit,N)at T1,
    less_than(N,4),
    turn(Unit,N,south,X,Y)from T1 to T2,
    find(Unit,Type)from T2 to T3.
find(Unit,Type)from T1 to T3 if
    position(Unit,south,X,Y)at T1,
    turns(Unit,N)at T1,
    less_than(N,4),
    turn(Unit,N,east,X,Y)from T1 to T2,
    find(Unit,Type)from T2 to T3.
find(Unit,Type)from T1 to T3 if
    position(Unit,east,X,Y)at T1,
    turns(Unit,N)at T1,
    less_than(N,4),
    turn(Unit,N,north,X,Y)from T1 to T2,
    find(Unit,Type)from T2 to T3.
find(Unit,Type)from T1 to T3 if
    position(Unit,north,X,Y)at T1,
    turns(Unit,N)at T1,
    greater_or_equal(N,4),
    decrement(Y,Y2),
    greater_than(Y2,1),
    walk(Unit,north,X,Y2)from T1 to T2,
    find(Unit,Type)from T2 to T3.

find(Unit,Type)from T1 to T3 if
    position(Unit,east,X,Y)at T1,
    turns(Unit,N)at T1,
    greater_or_equal(N,4),
    increment(X,X2),
    less_or_equal(X2,20),
    walk(Unit,east,X2,Y)from T1 to T2,
    find(Unit,Type)from T2 to T3.

find(Unit,Type)from T1 to T3 if
    position(Unit,south,X,Y)at T1,
    turns(Unit,N)at T1,
    greater_or_equal(N,4),
    increment(Y,Y2),
    less_or_equal(Y2,20),
    walk(Unit,south,X,Y2)from T1 to T2,
    find(Unit,Type)from T2 to T3.
find(Unit,Type)from T1 to T3 if
    position(Unit,west,X,Y)at T1,
    turns(Unit,N)at T1,
    greater_or_equal(N,4),
    decrement(X,X2),
    greater_or_equal(X2,1),
    walk(Unit,west,X2,Y)from T1 to T2,
    find(Unit,Type)from T2 to T3.
walk_towards(Unit,Type)from T1 to T2 if
    next_to(Unit,Type)at T1.
walk_towards(Unit,Type)from T1 to T3 if
    position(Unit,north,X,Y)at T1,
    in_sight(Unit,Type)at T1,
    item(Type,X,A)at T1,
    decrease(Y,Y1,A),
    greater_than(Y1,1),
    decrement(Y,Y2),
    greater_or_equal(Y2,1),
    walk(Unit,north,X,Y2)from T1 to T2,
    walk_towards(Unit,Type)from T2 to T3.
walk_towards(Unit,Type)from T1 to T3 if
    position(Unit,east,X,Y)at T1,
    in_sight(Unit,Type)at T1,
    item(Type,A,Y)at T1,
    decrease(A,X1,X),
    greater_than(X1,1),
    increment(X,X2),
    less_or_equal(X2,20),
    walk(Unit,east,X2,Y)from T1 to T2,
    walk_towards(Unit,Type)from T2 to T3.
walk_towards(Unit,Type)from T1 to T3 if
    position(Unit,south,X,Y)at T1,
    in_sight(Unit,Type)at T1,
    item(Type,X,A)at T1,
    decrease(A,Y1,Y),
    greater_than(Y1,1),
    increment(Y,Y2),
    less_or_equal(Y2,20),
    walk(Unit,south,X,Y2)from T1 to T2,
    walk_towards(Unit,Type)from T2 to T3.
walk_towards(Unit,Type)from T1 to T3 if
    position(Unit,west,X,Y)at T1,
    in_sight(Unit,Type)at T1,
    item(Type,A,Y)at T1,
    decrease(X,X1,A),
    greater_than(X1,1),
    decrement(X,X2),
    greater_or_equal(X2,1),
    walk(Unit,west,X2,Y)from T1 to T2,
    walk_towards(Unit,Type)from T2 to T3.
make_shelter(Unit)from T1 to T2 if
    holds_wood(Unit,N)at T1,
    greater_or_equal(N,25),
    build_shelter(Unit,N)from T1 to T2.
make_shelter(Unit)from T1 to T3 if
    holds_wood(Unit,N)at T1,
    less_than(N,25),
    get_wood(Unit)from T1 to T2,
    make_shelter(Unit)from T2 to T3.
build_shelter(Unit,N)from T1 to T2 if
    position(Unit,north,X,Y)at T1,
    decrement(Y,Y1),
    successful_shelter(Unit,N,X,Y1)from T1 to T2.
build_shelter(Unit,N)from T1 to T2 if
    position(Unit,east,X,Y)at T1,
    increment(X,X1),
    successful_shelter(Unit,N,X1,Y)from T1 to T2.
build_shelter(Unit,N)from T1 to T2 if
    position(Unit,south,X,Y)at T1,
    increment(Y,Y1),
    successful_shelter(Unit,N,X,Y1)from T1 to T2.
build_shelter(Unit,N)from T1 to T2 if
    position(Unit,west,X,Y)at T1,
    decrement(X,X1),
    successful_shelter(Unit,N,X1,Y)from T1 to T2.
get_wood(Unit)from T1 to T3 if
    in_sight(Unit,wood)at T1,
    walk_towards(Unit,wood)from T1 to T2,
    pick_wood(Unit)from T2 to T3.
get_wood(Unit)from T1 to T5 if
    position(Unit,Direction,X,Y)at T1,
    find(Unit,tree)from T1 to T2,
    walk_towards(Unit,tree)from T2 to T3,
    hit_tree(Unit)from T3 to T4,
    pick_wood(Unit)from T4 to T5.
hit_tree(Unit)from T1 to T2 if
    position(Unit,north,X,Y)at T1,
    decrement(Y,Y1),
    wood(X,Y1)at T1.
hit_tree(Unit)from T1 to T2 if
    position(Unit,east,X,Y)at T1,
    increment(X,X1),
    wood(X1,Y)at T1.
hit_tree(Unit)from T1 to T2 if
    position(Unit,south,X,Y)at T1,
    increment(Y,Y1),
    wood(X,Y1)at T1.
hit_tree(Unit)from T1 to T2 if
    position(Unit,west,X,Y)at T1,
    decrement(X,X1),
    wood(X1,Y)at T1.
hit_tree(Unit)from T1 to T2 if
    position(Unit,north,X,Y)at T1,
    decrement(Y,Y1),
    tree(X,Y1)at T1,
    break_tree(Unit,X,Y1)from T1 to T2.
hit_tree(Unit)from T1 to T2 if
    position(Unit,east,X,Y)at T1,
    increment(X,X1),
    tree(X1,Y)at T1,
    break_tree(Unit,X1,Y)from T1 to T2.
hit_tree(Unit)from T1 to T2 if
    position(Unit,south,X,Y)at T1,
    increment(Y,Y1),
    tree(X,Y1)at T1,
    break_tree(Unit,X,Y1)from T1 to T2.
hit_tree(Unit)from T1 to T2 if
    position(Unit,west,X,Y)at T1,
    decrement(X,X1),
    tree(X1,Y)at T1,
    break_tree(Unit,X1,Y)from T1 to T2.
pick_wood(Unit)from T1 to T2 if
    position(Unit,north,X,Y)at T1,
    holds_wood(Unit,N)at T1,
    decrement(Y,Y1),
    wood(X,Y1)at T1,
    collect_wood(Unit,N,X,Y1)from T1 to T2.
pick_wood(Unit)from T1 to T2 if
    position(Unit,east,X,Y)at T1,
    holds_wood(Unit,N)at T1,
    increment(X,X1),
    wood(X1,Y)at T3,
    collect_wood(Unit,N,X1,Y)from T1 to T2.
pick_wood(Unit)from T1 to T2 if
    position(Unit,south,X,Y)at T1,
    holds_wood(Unit,N)at T1,
    increment(Y,Y1),
    wood(X,Y1)at T3,
    collect_wood(Unit,N,X,Y1)from T1 to T2.
pick_wood(Unit)from T1 to T2 if
    position(Unit,west,X,Y)at T1,
    holds_wood(Unit,N)at T1,
    decrement(X,X1),
    wood(X1,Y)at T3,
    collect_wood(Unit,N,X1,Y)from T1 to T2.
need_food(Unit)from T1 to T2 if
    person(Unit,cautious)at T1,
    holds_food(Unit,N,A,B,C)at T1,
    greater_or_equal(N,15).
need_food(Unit)from T1 to T3 if
    person(Unit,cautious)at T1,
    holds_food(Unit,N,A,B,C)at T1,
    less_than(N,15),
    get_food(Unit)from T1 to T2,
    need_food(Unit)from T2 to T3.
need_food(Unit)from T1 to T2 if
    person(Unit,normal)at T1,
    holds_food(Unit,N,A,B,C)at T1,
    greater_or_equal(N,10).
need_food(Unit)from T1 to T3 if
    person(Unit,normal)at T1,
    holds_food(Unit,N,A,B,C)at T1,
    less_than(N,10),
    get_food(Unit)from T1 to T2,
    need_food(Unit)from T2 to T3.
need_food(Unit)from T1 to T2 if
    person(Unit,violent)at T1,
    holds_food(Unit,N,A,B,C)at T1,
    greater_or_equal(N,5).
need_food(Unit)from T1 to T3 if
    person(Unit,violent)at T1,
    holds_food(Unit,N,A,B,C)at T1,
    less_than(N,5),
    get_food(Unit)from T1 to T2,
    need_food(Unit)from T2 to T3.
get_food(Unit)from T1 to T2 if
    in_sight(Unit,food)at T1,
    pick_food(Unit)from T1 to T2.
get_food(Unit)from T1 to T5 if
    find(Unit,animal)from T1 to T2,
    kill_animal(Unit)from T2 to T3,
    hit_animal(Unit)from T3 to T4,
    get_food(Unit)from T4 to T5.
kill_animal(Unit)from T1 to T2 if
    position(Unit,D,X,Y)at T1,
    in_sight(Unit,food)at T1.
kill_animal(Unit)from T1 to T2 if
    position(Unit,D,X,Y)at T1,
    has(Unit,weapon(A)),
    in_range(Unit,animal,D,A)at T1.
kill_animal(Unit)from T1 to T3 if
    position(Unit,north,X,Y)at T1,
    animal(Type,H,D1,X,Y1)at T1,
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(Y,Y2,Y1),
    greater_than(Y2,U),
    decrement(Y,NewY),
    walk(Unit,north,X,NewY)from T1 to T2,
    kill_animal(Unit)from T2 to T3.
kill_animal(Unit)from T1 to T3 if
    position(Unit,east,X,Y)at T1,
    animal(Type,H,D1,X1,Y)at T1,
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(X1,X2,X),
    greater_than(X2,U),
    increment(X,NewX),
    walk(Unit,east,NewX,Y)from T1 to T2,
    kill_animal(Unit)from T2 to T3.
kill_animal(Unit)from T1 to T3 if
    position(Unit,south,X,Y)at T1,
    animal(Type,H,D1,X,Y1)at T1,
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(Y1,Y2,Y),
    greater_than(Y2,U),
    increment(Y,NewY),
    walk(Unit,south,X,NewY)from T1 to T2,
    kill_animal(Unit)from T2 to T3.
kill_animal(Unit)from T1 to T3 if
    position(Unit,west,X,Y)at T1,
    animal(Type,H,D1,X1,Y)at T1,
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(X,X2,X1),
    greater_than(X2,U),
    decrement(X,NewX),
    walk(Unit,west,NewX,Y)from T1 to T2,
    kill_animal(Unit)from T2 to T3.
kill_animal(Unit)from T1 to T3 if
    position(Unit,north,X,Y)at T1,
    animal(Type,H,D1,X,Y1)at T1,
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(Y,Y2,Y1),
    less_than(Y2,L),
    increment(Y,NewY),
    walk(Unit,north,X,NewY)from T1 to T2,
    kill_animal(Unit)from T2 to T3.
kill_animal(Unit)from T1 to T3 if
    position(Unit,east,X,Y)at T1,
    animal(Type,H,D1,X1,Y)at T1,
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(X1,X2,X),
    less_than(X2,L),
    decrement(X,NewX),
    walk(Unit,east,NewX,Y)from T1 to T2,
    kill_animal(Unit)from T2 to T3.
kill_animal(Unit)from T1 to T3 if
    position(Unit,south,X,Y)at T1,
    animal(Type,H,D1,X,Y1)at T1,
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(Y1,Y2,Y),
    less_than(Y2,L),
    decrement(Y,NewY),
    walk(Unit,south,X,NewY)from T1 to T2,
    kill_animal(Unit)from T2 to T3.
kill_animal(Unit)from T1 to T3 if
    position(Unit,west,X,Y)at T1,
    animal(Type,H,D1,X1,Y)at T1,
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(X,X2,X1),
    less_than(X2,L),
    increment(X,NewX),
    walk(Unit,west,NewX,Y)from T1 to T2,
    kill_animal(Unit)from T2 to T3.
hit_animal(Unit)from T1 to T2 if
    position(Unit,D,X,Y)at T1,
    in_sight(Unit,food)at T1.
hit_animal(Unit)from T1 to T2 if
    position(Unit,north,X,Y)at T1,
    animal(Type,H,D1,X,Y1)at T1,
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(Y,Y2,Y1),
    greater_or_equal(Y2,L),
    less_or_equal(Y2,U),
    reduce_health(Unit,animal(Type,H,D1,X,Y1))from T1 to T2.
hit_animal(Unit)from T1 to T2 if
    position(Unit,east,X,Y)at T1,
    animal(Type,H,D1,X1,Y)at T1,
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(X1,X2,X),
    greater_or_equal(X2,L),
    less_or_equal(X2,U),
    reduce_health(Unit,animal(Type,H,D1,X1,Y))from T1 to T2.
hit_animal(Unit)from T1 to T2 if
    position(Unit,south,X,Y)at T1,
    animal(Type,H,D1,X,Y1)at T1,
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(Y1,Y2,Y),
    greater_or_equal(Y2,L),
    less_or_equal(Y2,U),
    reduce_health(Unit,animal(Type,H,D1,X,Y1))from T1 to T2.
hit_animal(Unit)from T1 to T2 if
    position(Unit,west,X,Y)at T1,
    animal(Type,H,D1,X1,Y)at T1,
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(X,X2,X1),
    greater_or_equal(X2,L),
    less_or_equal(X2,U),
    reduce_health(Unit,animal(Type,H,D1,X1,Y))from T1 to T2.
reduce_health(Unit,animal(Type,N,D,X,Y))from T1 to T2 if
    person(Unit,Kind)at T1,
    has(Unit,weapon(A)),
    power(weapon(A),P),
    decrease(N,N1,P),
    check_animal(Unit,N,D,X,Y,N1,Type)from T1 to T2.
check_animal(Unit,N,D,X,Y,N1,A)from T1 to T2 if
    less_than(N1,0),
    change_animal(Unit,N,D,X,Y,0,A)from T1 to T2.
check_animal(Unit,N,D,X,Y,N1,A)from T1 to T2 if
    greater_or_equal(N1,0),
    change_animal(Unit,N,D,X,Y,N1,A)from T1 to T2.
pick_food(Unit)from T1 to T3 if
    walk_towards(Unit,food)from T1 to T2,
    pick_up_food(Unit)from T2 to T3.
pick_up_food(Unit)from T1 to T2 if
    position(Unit,north,X,Y)at T1,
    holds_food(Unit,N,A,B,C)at T1,
    decrement(Y,Y1),
    food(K,L,M,X,Y1)at T1,
    collect_food(Unit,A,B,C,K,L,M,X,Y1)from T1 to T2.
pick_up_food(Unit)from T1 to T2 if
    position(Unit,east,X,Y)at T1,
    holds_food(Unit,N,A,B,C)at T1,
    increment(X,X1),
    food(K,L,M,X1,Y)at T1,
    collect_food(Unit,A,B,C,K,L,M,X1,Y)from T1 to T2.
pick_up_food(Unit)from T1 to T2 if
    position(Unit,south,X,Y)at T1,
    holds_food(Unit,N,A,B,C)at T1,
    increment(Y,Y1),
    food(K,L,M,X,Y1)at T1,
    collect_food(Unit,A,B,C,K,L,M,X,Y1)from T1 to T2.
pick_up_food(Unit)from T1 to T2 if
    position(Unit,west,X,Y)at T1,
    holds_food(Unit,N,A,B,C)at T1,
    decrement(X,X1),
    food(K,L,M,X1,Y)at T1,
    collect_food(Unit,A,B,C,K,L,M,X1,Y)from T1 to T2.
attack(Unit)from T1 to T2 if
    position(Unit,D,X,Y)at T1,
    has(Unit,weapon(A)),
    in_range(Unit,Unit1,D,A)at T1,
    power(weapon(A),P),
    health(Unit1,H)at T1,
    opposite(D,D1),
    hit(Unit,Unit1,H,D1,P)from T1 to T2.
hit(Unit,Unit1,H,D1,P)from T1 to T2 if
    in_shelter(Unit1)at T1,
    shelter(Unit1,H1,X,Y)at T1,
    decrease(H1,H2,P),
    greater_than(H2,0),
    hit_shelter(Unit,Unit1,H2,D1,X,Y)from T1 to T2.
hit(Unit,Unit1,H,D1,P)from T1 to T2 if
    in_shelter(Unit1)at T1,
    shelter(Unit1,H1,X,Y)at T1,
    decrease(H1,H2,P),
    less_or_equal(H2,0),
    hit_shelter(Unit,Unit1,0,D1,X,Y)from T1 to T2.
hit(Unit,Unit1,H,D1,P)from T1 to T2 if
    not in_shelter(Unit1)at T1,
    decrease(H,H1,P),
    greater_than(H1,0),
    hit_from(Unit,Unit1,H1,D1)from T1 to T2.
hit(Unit,Unit1,H,D1,P)from T1 to T2 if
    not in_shelter(Unit1)at T1,
    decrease(H,H1,P),
    less_or_equal(H1,0),
    hit_from(Unit,Unit1,0,D1)from T1 to T2.
attack(Unit)from T1 to T3 if
    position(Unit,north,X,Y)at T1,
    position(Unit1,D1,X,Y1)at T1,
    not_equal(Unit,Unit1),
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(Y,Y2,Y1),
    greater_than(Y2,U),
    decrement(Y,NewY),
    walk(Unit,north,X,NewY)from T1 to T2,
    attack(Unit)from T2 to T3.
attack(Unit)from T1 to T3 if
    position(Unit,east,X,Y)at T1,
    position(Unit1,D1,X1,Y)at T1,
    not_equal(Unit,Unit1),
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(X1,X2,X),
    greater_than(X2,U),
    increment(X,NewX),
    walk(Unit,east,NewX,Y)from T1 to T2,
    attack(Unit)from T2 to T3.
attack(Unit)from T1 to T3 if
    position(Unit,south,X,Y)at T1,
    position(Unit1,D1,X,Y1)at T1,
    not_equal(Unit,Unit1),
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(Y1,Y2,Y),
    greater_than(Y2,U),
    increment(Y,NewY),
    walk(Unit,south,X,NewY)from T1 to T2,
    attack(Unit)from T2 to T3.
attack(Unit)from T1 to T3 if
    position(Unit,west,X,Y)at T1,
    position(Unit1,D1,X1,Y)at T1,
    not_equal(Unit,Unit1),
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(X,X2,X1),
    greater_than(X2,U),
    decrement(X,NewX),
    walk(Unit,west,NewX,Y)from T1 to T2,
    attack(Unit)from T2 to T3.
attack(Unit)from T1 to T3 if
    position(Unit,north,X,Y)at T1,
    position(Unit1,D1,X,Y1)at T1,
    not_equal(Unit,Unit1),
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(Y,Y2,Y1),
    less_than(Y2,L),
    increment(Y,NewY),
    walk(Unit,north,X,NewY)from T1 to T2,
    attack(Unit)from T2 to T3.
attack(Unit)from T1 to T3 if
    position(Unit,east,X,Y)at T1,
    position(Unit1,D1,X1,Y)at T1,
    not_equal(Unit,Unit1),
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(X1,X2,X),
    less_than(X2,L),
    decrement(X,NewX),
    walk(Unit,east,NewX,Y)from T1 to T2,
    attack(Unit)from T2 to T3.
attack(Unit)from T1 to T3 if
    position(Unit,south,X,Y)at T1,
    position(Unit1,D1,X,Y1)at T1,
    not_equal(Unit,Unit1),
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(Y1,Y2,Y),
    less_than(Y2,L),
    decrement(Y,NewY),
    walk(Unit,south,X,NewY)from T1 to T2,
    attack(Unit)from T2 to T3.
attack(Unit)from T1 to T3 if
    position(Unit,west,X,Y)at T1,
    position(Unit1,D1,X1,Y)at T1,
    not_equal(Unit,Unit1),
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(X,X2,X1),
    less_than(X2,L),
    increment(X,NewX),
    walk(Unit,west,NewX,Y)from T1 to T2,
    attack(Unit)from T2 to T3.
walk_away(Unit)from T1 to T2 if
    position(Unit,north,X,Y)at T1,
    decrement(X,X1),
    walk(Unit,west,X1,Y)from T1 to T2.
walk_away(Unit)from T1 to T2 if
    position(Unit,east,X,Y)at T1,
    decrement(Y,Y1),
    walk(Unit,north,X,Y1)from T1 to T2.
walk_away(Unit)from T1 to T2 if
    position(Unit,south,X,Y)at T1,
    increment(X,X1),
    walk(Unit,east,X1,Y)from T1 to T2.
walk_away(Unit)from T1 to T2 if
    position(Unit,west,X,Y)at T1,
    increment(Y,Y1),
    walk(Unit,south,X,Y1)from T1 to T2.
look(Unit,Type)from T1 to T2 if
    position(Unit,north,X,Y)at T1,
    turns(Unit,N)at T1,
    less_than(N,4),
    turn(Unit,N,west,X,Y)from T1 to T2.
look(Unit,Type)from T1 to T2 if
    position(Unit,west,X,Y)at T1,
    turns(Unit,N)at T1,
    less_than(N,4),
    turn(Unit,N,south,X,Y)from T1 to T2.
look(Unit,Type)from T1 to T2 if
    position(Unit,south,X,Y)at T1,
    turns(Unit,N)at T1,
    less_than(N,4),
    turn(Unit,N,east,X,Y)from T1 to T2.
look(Unit,Type)from T1 to T2 if
    position(Unit,east,X,Y)at T1,
    turns(Unit,N)at T1,
    less_than(N,4),
    turn(Unit,N,north,X,Y)from T1 to T2.
look(Unit,Type)from T1 to T2 if
    position(Unit,north,X,Y)at T1,
    turns(Unit,N)at T1,
    greater_or_equal(N,4),
    decrement(Y,Y2),
    greater_than(Y2,1),
    walk(Unit,north,X,Y2)from T1 to T2.
look(Unit,Type)from T1 to T2 if
    position(Unit,east,X,Y)at T1,
    turns(Unit,N)at T1,
    greater_or_equal(N,4),
    increment(X,X2),
    less_or_equal(X2,20),
    walk(Unit,east,X2,Y)from T1 to T2.
look(Unit,Type)from T1 to T2 if
    position(Unit,south,X,Y)at T1,
    turns(Unit,N)at T1,
    greater_or_equal(N,4),
    increment(Y,Y2),
    less_or_equal(Y2,20),
    walk(Unit,south,X,Y2)from T1 to T2.
find(Unit,Type)from T1 to T2 if
    position(Unit,west,X,Y)at T1,
    turns(Unit,N)at T1,
    greater_or_equal(N,4),
    decrement(X,X2),
    greater_or_equal(X2,1),
    walk(Unit,west,X2,Y)from T1 to T2.
move_away(Unit,north)from T1 to T2 if
    position(Unit,D,X,Y)at T1,
    decrement(X,X1),
    walk(Unit,west,X1,Y)from T1 to T2.
move_away(Unit,east)from T1 to T2 if
    position(Unit,D,X,Y)at T1,
    decrement(Y,Y1),
    walk(Unit,north,X,Y1)from T1 to T2.
move_away(Unit,south)from T1 to T2 if
    position(Unit,D,X,Y)at T1,
    increment(X,X1),
    walk(Unit,east,X1,Y)from T1 to T2.
move_away(Unit,west)from T1 to T2 if
    position(Unit,D,X,Y)at T1,
    increment(Y,Y1),
    walk(Unit,south,X,Y1)from T1 to T2.
turn_and_attack(Unit,D,Unit1)from T1 to T2 if
    position(Unit,OldD,X,Y)at T1,
    has(Unit,weapon(A)),
    power(weapon(A),P),
    in_range(Unit,Unit1,D,A)at T1,
    opposite(D,D1),
    health(Unit1,H)at T1,
    turn_and_hit(Unit,D,X,Y,P,Unit1,H,D1)from T1 to T2.
turn_and_hit(Unit,D,X,Y,P,Unit1,H,D1)from T1 to T2 if
    in_shelter(Unit1)at T1,
    shelter(Unit1,H1,X1,Y1)at T1,
    decrease(H1,H2,P),
    greater_than(H2,0),
    turn_hit_shelter(Unit,D,X,Y,Unit1,H2,D1,X1,Y1)from T1 to T2.
turn_and_hit(Unit,D,X,Y,P,Unit1,H,D1)from T1 to T2 if
    in_shelter(Unit1)at T1,
    shelter(Unit1,H1,X1,Y1)at T1,
    decrease(H1,H2,P),
    less_or_equal(H2,0),
    turn_hit_shelter(Unit,D,X,Y,Unit1,0,D1,X1,Y1)from T1 to T2.
turn_and_hit(Unit,D,X,Y,P,Unit1,H,D1)from T1 to T2 if
    not in_shelter(Unit1)at T1,
    decrease(H,H1,P),
    greater_than(H1,0),
    turn_hit(Unit,D,X,Y,Unit1,H1,D1)from T1 to T2.
turn_and_hit(Unit,D,X,Y,P,Unit1,H,D1)from T1 to T2 if
    not in_shelter(Unit1)at T1,
    decrease(H,H1,P),
    less_or_equal(H1,0),
    turn_hit(Unit,D,X,Y,Unit1,0,D1)from T1 to T2.
turn_and_attack(Unit,north,Unit1)from T1 to T3 if
    position(Unit,Direction,X,Y)at T1,
    position(Unit1,D1,X,Y1)at T1,
    not_equal(Unit,Unit1),
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(Y,Y2,Y1),
    greater_than(Y2,U),
    decrement(Y,NewY),
    walk(Unit,north,X,NewY)from T1 to T2,
    attack(Unit)from T2 to T3.
turn_and_attack(Unit,east,Unit1)from T1 to T3 if
    position(Unit,Direction,X,Y)at T1,
    position(Unit1,D1,X1,Y)at T1,
    not_equal(Unit,Unit1),
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(X1,X2,X),
    greater_than(X2,U),
    increment(X,NewX),
    walk(Unit,east,NewX,Y)from T1 to T2,
    attack(Unit)from T2 to T3.
turn_and_attack(Unit,south,Unit1)from T1 to T3 if
    position(Unit,Direction,X,Y)at T1,
    position(Unit1,D1,X,Y1)at T1,
    not_equal(Unit,Unit1),
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(Y1,Y2,Y),
    greater_than(Y2,U),
    increment(Y,NewY),
    walk(Unit,south,X,NewY)from T1 to T2,
    attack(Unit)from T2 to T3.
turn_and_attack(Unit,west,Unit1)from T1 to T3 if
    position(Unit,Direction,X,Y)at T1,
    position(Unit1,D1,X1,Y)at T1,
    not_equal(Unit,Unit1),
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(X,X2,X1),
    greater_than(X2,U),
    decrement(X,NewX),
    walk(Unit,west,NewX,Y)from T1 to T2,
    attack(Unit)from T2 to T3.
turn_and_attack(Unit,north,Unit1)from T1 to T3 if
    position(Unit,Direction,X,Y)at T1,
    position(Unit1,D1,X,Y1)at T1,
    not_equal(Unit,Unit1),
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(Y,Y2,Y1),
    less_than(Y2,L),
    increment(Y,NewY),
    walk(Unit,north,X,NewY)from T1 to T2,
    attack(Unit)from T2 to T3.
turn_and_attack(Unit,east,Unit1)from T1 to T3 if
    position(Unit,Direction,X,Y)at T1,
    position(Unit1,D1,X1,Y)at T1,
    not_equal(Unit,Unit1),
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(X1,X2,X),
    less_than(X2,L),
    decrement(X,NewX),
    walk(Unit,east,NewX,Y)from T1 to T2,
    attack(Unit)from T2 to T3.
turn_and_attack(Unit,south,Unit1)from T1 to T3 if
    position(Unit,Direction,X,Y)at T1,
    position(Unit1,D1,X,Y1)at T1,
    not_equal(Unit,Unit1),
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(Y1,Y2,Y),
    less_than(Y2,L),
    decrement(Y,NewY),
    walk(Unit,south,X,NewY)from T1 to T2,
    attack(Unit)from T2 to T3.
turn_and_attack(Unit,west,Unit1)from T1 to T3 if
    position(Unit,Direction,X,Y)at T1,
    position(Unit1,D1,X1,Y)at T1,
    not_equal(Unit,Unit1),
    has(Unit,weapon(A)),
    range(weapon(A),L,U),
    decrease(X,X2,X1),
    less_than(X2,L),
    increment(X,NewX),
    walk(Unit,west,NewX,Y)from T1 to T2,
    attack(Unit)from T2 to T3.
lower_hunger(Unit,N)from T1 to T2 if
    decrement(N,N1),
    greater_than(N1,0),
    reduce_hunger(Unit,N1)from T1 to T2.
lower_hunger(Unit,N)from T1 to T2 if
    decrement(N,N1),
    less_or_equal(N1,0),
    reduce_hunger(Unit,0)from T1 to T2.
check_hunger(Unit)from T1 to T2 if
    hunger(Unit,N)at T1,
    greater_or_equal(N,10).
check_hunger(Unit)from T1 to T3 if
    hunger(Unit,N)at T1,
    less_than(N,10),
    health(Unit,H)at T1,
    decrement(H,H1),
    greater_than(H1,0),
    lower_health(Unit,H1)from T1 to T2,
    check_hunger(Unit)from T2 to T3.
check_hunger(Unit)from T1 to T3 if
    hunger(Unit,N)at T1,
    less_than(N,10),
    health(Unit,H)at T1,
    decrement(H,H1),
    less_or_equal(H1,0),
    lower_health(Unit,0)from T1 to T2,
    check_hunger(Unit)from T2 to T3.
consume_food(Unit)from T1 to T2 if
    person(Unit,cautious)at T1,
    hunger(Unit,F)at T1,
    greater_than(F,15).
consume_food(Unit)from T1 to T2 if
    person(Unit,cautious)at T1,
    hunger(Unit,F)at T1,
    less_or_equal(F,15),
    holds_food(Unit,N,A,B,C)at T1,
    greater_or_equal(N,1),
    eat_food(Unit)from T1 to T2.
consume_food(Unit)from T1 to T2 if
    person(Unit,normal)at T1,
    hunger(Unit,F)at T1,
    greater_than(F,12).
consume_food(Unit)from T1 to T2 if
    person(Unit,normal)at T1,
    hunger(Unit,F)at T1,
    less_or_equal(F,12),
    holds_food(Unit,N,A,B,C)at T1,
    greater_or_equal(N,1),
    eat_food(Unit)from T1 to T2.
consume_food(Unit)from T1 to T2 if
    person(Unit,violent)at T1,
    hunger(Unit,F)at T1,
    greater_than(F,9).
consume_food(Unit)from T1 to T2 if
    person(Unit,violent)at T1,
    hunger(Unit,F)at T1,
    less_or_equal(F,9),
    holds_food(Unit,N,A,B,C)at T1,
    greater_or_equal(N,1),
    eat_food(Unit)from T1 to T2.
eat_food(Unit)from T1 to T2 if
    hunger(Unit,20)at T1.
eat_food(Unit)from T1 to T3 if
    hunger(Unit,F)at T1,
    decrease(20,F1,F),
    greater_or_equal(F1,5),
    holds_food(Unit,N,A,B,C)at T1,
    greater_or_equal(C,1),
    eat(Unit,5,F,N,A,B,C)from T1 to T2,
    eat_food(Unit)from T2 to T3.
eat_food(Unit)from T1 to T3 if
    hunger(Unit,F)at T1,
    decrease(20,F1,F),
    greater_or_equal(F1,5),
    holds_food(Unit,N,A,B,C)at T1,
    equal(C,0),
    greater_or_equal(B,1),
    eat(Unit,3,F,N,A,B,C)from T1 to T2,
    eat_food(Unit)from T2 to T3.
eat_food(Unit)from T1 to T3 if
    hunger(Unit,F)at T1,
    decrease(20,F1,F),
    greater_or_equal(F1,5),
    holds_food(Unit,N,A,B,C)at T1,
    equal(C,0),
    equal(B,0),
    greater_or_equal(A,1),
    eat(Unit,1,F,N,A,B,C)from T1 to T2,
    eat_food(Unit)from T2 to T3.
eat_food(Unit)from T1 to T3 if
    hunger(Unit,F)at T1,
    decrease(20,F1,F),
    less_than(F1,5),
    greater_or_equal(F1,3),
    holds_food(Unit,N,A,B,C)at T1,
    greater_or_equal(B,1),
    eat(Unit,3,F,N,A,B,C)from T1 to T2,
    eat_food(Unit)from T2 to T3.
eat_food(Unit)from T1 to T3 if
    hunger(Unit,F)at T1,
    decrease(20,F1,F),
    less_than(F1,5),
    greater_or_equal(F1,3),
    holds_food(Unit,N,A,B,C)at T1,
    equal(B,0),
    greater_or_equal(A,1),
    eat(Unit,1,F,N,A,B,C)from T1 to T2,
    eat_food(Unit)from T2 to T3.
eat_food(Unit)from T1 to T3 if
    hunger(Unit,F)at T1,
    decrease(20,F1,F),
    less_than(F1,3),
    greater_or_equal(F1,1),
    holds_food(Unit,N,A,B,C)at T1,
    greater_or_equal(A,1),
    eat(Unit,1,F,N,A,B,C)from T1 to T2,
    eat_food(Unit)from T2 to T3.
check_food(Unit)from T1 to T2 if
    person(Unit,cautious)at T1,
    holds_food(Unit,N,A,B,C)at T1,
    greater_or_equal(N,10).
check_food(Unit)from T1 to T2 if
    person(Unit,normal)at T1,
    holds_food(Unit,N,A,B,C)at T1,
    greater_or_equal(N,5).
check_food(Unit)from T1 to T2 if
    person(Unit,violent)at T1,
    holds_food(Unit,N,A,B,C)at T1,
    greater_or_equal(N,0).
check_food(Unit)from T1 to T3 if
    person(Unit,cautious)at T1,
    holds_food(Unit,N,A,B,C)at T1,
    less_than(N,10),
    get_food(Unit)from T1 to T2,
    check_food(Unit)from T2 to T3.
check_food(Unit)from T1 to T3 if
    person(Unit,normal)at T1,
    holds_food(Unit,N,A,B,C)at T1,
    less_than(N,5),
    get_food(Unit)from T1 to T2,
    check_food(Unit)from T2 to T3.
check_food(Unit)from T1 to T3 if
    person(Unit,violent)at T1,
    holds_food(Unit,N,A,B,C)at T1,
    less_than(N,0),
    get_food(Unit)from T1 to T2,
    check_food(Unit)from T2 to T3.
on_fire(X,Y)from T1 to T2 if
    position(Unit,D,X,Y)at T1,
    health(Unit,N)at T1,
    decrease(N,N1,10),
    burn(Unit,N1)from T1 to T2.
check_health(Unit)from T1 to T2 if
    health(Unit,H)at T1,
    less_than(H,50),
    heal(Unit,H)from T1 to T2.
check_health(Unit)from T1 to T2 if
    health(Unit,50)at T1.
go_to_shelter(Unit)from T1 to T2 if
    in_shelter(Unit)at T1.
go_to_shelter(Unit)from T1 to T3 if
    position(Unit,north,X,Y)at T1,
    decrement(Y,Y1),
    shelter(Unit,H,X,Y1)at T1,
    walk(Unit,north,X,Y1)from T1 to T2,
    go_to_shelter(Unit)from T2 to T3.
go_to_shelter(Unit)from T1 to T3 if
    position(Unit,south,X,Y)at T1,
    increment(Y,Y1),
    shelter(Unit,H,X,Y1)at T1,
    walk(Unit,south,X,Y1)from T1 to T2,
    go_to_shelter(Unit)from T2 to T3.
go_to_shelter(Unit)from T1 to T3 if
    position(Unit,east,X,Y)at T1,
    increment(X,X1),
    shelter(Unit,H,X1,Y)at T1,
    walk(Unit,east,X1,Y)from T1 to T2,
    go_to_shelter(Unit)from T2 to T3.
go_to_shelter(Unit)from T1 to T3 if
    position(Unit,west,X,Y)at T1,
    decrement(X,X1),
    shelter(Unit,H,X1,Y)at T1,
    walk(Unit,west,X1,Y)from T1 to T2,
    go_to_shelter(Unit)from T2 to T3.
go_to_shelter(Unit)from T1 to T3 if
    position(Unit,D,X,Y)at T1,
    shelter(Unit,H,X1,Y1)at T1,
    decrease(Y,Y2,Y1),
    greater_than(Y2,1),
    decrement(Y,Y3),
    walk(Unit,north,X,Y3)from T1 to T2,
    go_to_shelter(Unit)from T2 to T3.
go_to_shelter(Unit)from T1 to T3 if
    position(Unit,D,X,Y)at T1,
    shelter(Unit,H,X1,Y1)at T1,
    decrease(Y1,Y2,Y),
    greater_than(Y2,1),
    increment(Y,Y3),
    walk(Unit,south,X,Y3)from T1 to T2,
    go_to_shelter(Unit)from T2 to T3.
go_to_shelter(Unit)from T1 to T3 if
    position(Unit,D,X,Y)at T1,
    shelter(Unit,H,X1,Y1)at T1,
    decrease(X1,X2,X),
    greater_than(X2,1),
    increment(X,X3),
    walk(Unit,east,X3,Y)from T1 to T2,
    go_to_shelter(Unit)from T2 to T3.
go_to_shelter(Unit)from T1 to T3 if
    position(Unit,D,X,Y)at T1,
    shelter(Unit,H,X1,Y1)at T1,
    decrease(X,X2,X1),
    greater_than(X2,1),
    decrement(X,X3),
    walk(Unit,west,X3,Y)from T1 to T2,
    go_to_shelter(Unit)from T2 to T3.
fluent(person(_24248,_24250)).
fluent(position(_24248,_24250,_24252,_24254)).
fluent(health(_24248,_24250)).
fluent(hunger(_24248,_24250)).
fluent(holds_wood(_24248,_24250)).
fluent(holds_food(_24248,_24250,_24252,_24254,_24256)).
fluent(has_shelter(_24248,_24250)).
fluent(shelter(_24248,_24250,_24252,_24254)).
fluent(turns(_24248,_24250)).
fluent(dead(_24248)).
fluent(tree(_24248,_24250)).
fluent(wood(_24248,_24250)).
fluent(animal(_24248,_24250,_24252,_24254,_24256)).
fluent(food(_24248,_24250,_24252,_24254,_24256)).
fluent(cycles(_24248)).
fluent(in_shelter(_24248)).
turn(Unit,N,D,X,Y)from T1 to T2 terminates position(Unit,Direction,X,Y).
turn(Unit,N,D,X,Y)from T1 to T2 terminates turns(Unit,N).
turn(Unit,N,D,X,Y)from T1 to T2 terminates cycles(T).
walk(Unit,D,X,Y)from T1 to T2 terminates position(Unit,D1,X1,Y1).
walk(Unit,D,X,Y)from T1 to T2 terminates turns(Unit,N).
walk(Unit,D,X,Y)from T1 to T2 terminates cycles(T).
successful_shelter(Unit,N,X,Y)from T1 to T2 terminates holds_wood(Unit,N).
successful_shelter(Unit,N,X,Y)from T1 to T2 terminates has_shelter(Unit,false).
successful_shelter(Unit,N,X,Y)from T1 to T2 terminates cycles(T).
successful_shelter(Unit,N,X,Y)from T1 to T2 terminates turns(Unit,N1).
break_tree(Unit,X,Y)from T1 to T2 terminates tree(X,Y).
break_tree(Unit,X,Y)from T1 to T2 terminates turns(Unit,N).
break_tree(Unit,X,Y)from T1 to T2 terminates cycles(T).
break_tree(Unit,X,Y)from T1 to T2 terminates turns(Unit,N).
collect_wood(Unit,N,X,Y)from T1 to T2 terminates wood(X,Y).
collect_wood(Unit,N,X,Y)from T1 to T2 terminates holds_wood(Unit,N).
collect_wood(Unit,N,X,Y)from T1 to T2 terminates cycles(T).
collect_wood(Unit,N,X,Y)from T1 to T2 terminates turns(Unit,N1).
change_animal(Unit,N,D,X,Y,N1,Type)from T1 to T2 terminates animal(Type,N,D,X,Y) if
    greater_than(N1,0).
change_animal(Unit,N,D,X,Y,0,rabbit)from T1 to T2 terminates animal(rabbit,N1,D,X,Y).
change_animal(Unit,N,D,X,Y,0,chicken)from T1 to T2 terminates animal(chicken,N1,D,X,Y).
change_animal(Unit,N,D,X,Y,0,cow)from T1 to T2 terminates animal(cow,N1,D,X,Y).
change_animal(Unit,N,D,X,Y,N1,Type)from T1 to T2 terminates cycles(T).
change_animal(Unit,N,D,X,Y,N1,Type)from T1 to T2 terminates turns(Unit,N2).
collect_food(Unit,A,B,C,K,L,M,X,Y)from T1 to T2 terminates food(K,L,M,X,Y).
collect_food(Unit,A,B,C,K,L,M,X,Y)from T1 to T2 terminates holds_food(Unit,N,A,B,C).
collect_food(Unit,A,B,C,K,L,M,X,Y)from T1 to T2 terminates cycles(T).
collect_food(Unit,A,B,C,K,L,M,X,Y)from T1 to T2 terminates turns(Unit,N).
hit_shelter(Unit,Unit1,H,D,X,Y)from T1 to T2 terminates shelter(Unit1,H1,X,Y).
hit_shelter(Unit,Unit1,H,D,X,Y)from T1 to T2 terminates cycles(T).
hit_shelter(Unit,Unit1,H,D,X,Y)from T1 to T2 terminates has_shelter(Unit1,true) if
    equal(H,0).
hit_shelter(Unit,Unit1,H,D,X,Y)from T1 to T2 terminates turns(Unit,N).
hit_from(Unit,Unit1,H,D)from T1 to T2 terminates health(Unit1,H1).
hit_from(Unit,Unit1,H,D)from T1 to T2 terminates cycles(T).
hit_from(Unit,Unit1,H,D)from T1 to T2 terminates person(Unit1,Type) if
    equal(H,0).
hit_from(Unit,Unit1,H,D)from T1 to T2 terminates position(Unit1,Direction,X,Y) if
    equal(H,0).
hit_from(Unit,Unit1,H,D)from T1 to T2 terminates hunger(Unit1,N) if
    equal(H,0).
hit_from(Unit,Unit1,H,D)from T1 to T2 terminates has_shelter(Unit1,Boolean) if
    equal(H,0).
hit_from(Unit,Unit1,H,D)from T1 to T2 terminates shelter(Unit1,N,X,Y) if
    equal(H,0).
hit_from(Unit,Unit1,H,D)from T1 to T2 terminates holds_wood(Unit1,N) if
    equal(H,0).
hit_from(Unit,Unit1,H,D)from T1 to T2 terminates holds_food(Unit1,N,A,B,C) if
    equal(H,0).
hit_from(Unit,Unit1,H,D)from T1 to T2 terminates turns(Unit1,N) if
    equal(H,0).
hit_from(Unit,Unit1,H,D)from T1 to T2 terminates turns(Unit,N).
turn_hit_shelter(Unit,D,X,Y,Unit1,H,D1,X1,Y1)from T1 to T2 terminates shelter(Unit1,H1,X1,Y1).
turn_hit_shelter(Unit,D,X,Y,Unit1,H,D1,X1,Y1)from T1 to T2 terminates has_shelter(Unit1,true) if
    equal(H,0).
turn_hit_shelter(Unit,D,X,Y,Unit1,H,D1,X1,Y1)from T1 to T2 terminates position(Unit,OldD,OldX,OldY).
turn_hit_shelter(Unit,D,X,Y,Unit1,H,D1,X1,Y1)from T1 to T2 terminates cycles(T).
turn_hit_shelter(Unit,D,X,Y,Unit1,H,D1,X1,Y1)from T1 to T2 terminates turns(Unit,N).
turn_hit(Unit,D,X,Y,Unit1,H,D1)from T1 to T2 terminates health(Unit1,H1).
turn_hit(Unit,D,X,Y,Unit1,H,D1)from T1 to T2 terminates position(Unit,OldD,OldX,OldY).
turn_hit(Unit,D,X,Y,Unit1,H,D1)from T1 to T2 terminates cycles(T).
turn_hit(Unit,D,X,Y,Unit1,H,D1)from T1 to T2 terminates person(Unit1,Type) if
    equal(H,0).
turn_hit(Unit,D,X,Y,Unit1,H,D1)from T1 to T2 terminates position(Unit1,Direction,X1,Y1) if
    equal(H,0).
turn_hit(Unit,D,X,Y,Unit1,H,D1)from T1 to T2 terminates hunger(Unit1,N) if
    equal(H,0).
turn_hit(Unit,D,X,Y,Unit1,H,D1)from T1 to T2 terminates has_shelter(Unit1,Boolean) if
    equal(H,0).
turn_hit(Unit,D,X,Y,Unit1,H,D1)from T1 to T2 terminates shelter(Unit1,N,X,Y) if
    equal(H,0).
turn_hit(Unit,D,X,Y,Unit1,H,D1)from T1 to T2 terminates holds_wood(Unit1,N) if
    equal(H,0).
turn_hit(Unit,D,X,Y,Unit1,H,D1)from T1 to T2 terminates holds_food(Unit1,N,A,B,C) if
    equal(H,0).
turn_hit(Unit,D,X,Y,Unit1,H,D1)from T1 to T2 terminates turns(Unit1,N) if
    equal(H,0).
turn_hit(Unit,D,X,Y,Unit1,H,D1)from T1 to T2 terminates turns(Unit,N).
lower_health(Unit,H)from T1 to T2 terminates health(Unit,H1).
lower_health(Unit,H)from T1 to T2 terminates cycles(T).
lower_health(Unit,H)from T1 to T2 terminates turns(Unit,N1).
lower_health(Unit,H)from T1 to T2 terminates person(Unit,Type) if
    equal(H,0).
lower_health(Unit,H)from T1 to T2 terminates position(Unit,Direction,X,Y) if
    equal(H,0).
lower_health(Unit,H)from T1 to T2 terminates hunger(Unit,N) if
    equal(H,0).
lower_health(Unit,H)from T1 to T2 terminates has_shelter(Unit,Boolean) if
    equal(H,0).
lower_health(Unit,H)from T1 to T2 terminates shelter(Unit,N,X,Y) if
    equal(H,0).
lower_health(Unit,H)from T1 to T2 terminates holds_wood(Unit,N) if
    equal(H,0).
lower_health(Unit,H)from T1 to T2 terminates holds_food(Unit,N,A,B,C) if
    equal(H,0).
lower_health(Unit,H)from T1 to T2 terminates turns(Unit,N) if
    equal(H,0).
reduce_hunger(Unit,N)from T1 to T2 terminates hunger(Unit,N1).
reduce_hunger(Unit,N)from T1 to T2 terminates cycles(T).
reduce_hunger(Unit,N)from T1 to T2 terminates turns(Unit,N1).
eat(Unit,V,F,N,A,B,C)from T1 to T2 terminates hunger(Unit,F).
eat(Unit,5,F,N,A,B,C)from T1 to T2 terminates holds_food(Unit,N,A,B,C).
eat(Unit,3,F,N,A,B,C)from T1 to T2 terminates holds_food(Unit,N,A,B,C).
eat(Unit,1,F,N,A,B,C)from T1 to T2 terminates holds_food(Unit,N,A,B,C).
eat(Unit,V,F,N,A,B,C)from T1 to T2 terminates turns(Unit,N1).
eat(Unit,V,F,N,A,B,C)from T1 to T2 terminates cycles(T).
burn(Unit,H)from T1 to T2 terminates health(Unit,N1).
burn(Unit,H)from T1 to T2 terminates turns(Unit,N1).
burn(Unit,H)from T1 to T2 terminates cycles(T).
burn(Unit,H)from T1 to T2 terminates person(Unit,Type) if
    equal(H,0).
burn(Unit,H)from T1 to T2 terminates position(Unit,Direction,X,Y) if
    equal(H,0).
burn(Unit,H)from T1 to T2 terminates hunger(Unit,N) if
    equal(H,0).
burn(Unit,H)from T1 to T2 terminates has_shelter(Unit,Boolean) if
    equal(H,0).
burn(Unit,H)from T1 to T2 terminates shelter(Unit,N,X,Y) if
    equal(H,0).
burn(Unit,H)from T1 to T2 terminates holds_wood(Unit,N) if
    equal(H,0).
burn(Unit,H)from T1 to T2 terminates holds_food(Unit,N,A,B,C) if
    equal(H,0).
burn(Unit,H)from T1 to T2 terminates turns(Unit,N) if
    equal(H,0).
heal(Unit,H)from T1 to T2 terminates health(Unit,H).
heal(Unit,H)from T1 to T2 terminates turns(Unit,N).
heal(Unit,H)from T1 to T2 terminates cycles(T).
reactive_rule([happens(start_game(Unit),T1,T2),holds(person(Unit,cautious),T2),holds(has_shelter(Unit,false),T2)],[happens(make_shelter(Unit),T3,T4),tc(T2=<T3)],95).
reactive_rule([happens(start_game(Unit),T1,T2),holds(person(Unit,normal),T2),holds(has_shelter(Unit,false),T2)],[happens(make_shelter(Unit),T3,T4),tc(T2=<T3)],95).
reactive_rule([happens(start_game(Unit),T1,T2)],[happens(need_food(Unit),T3,T4),tc(T2=<T3)],94).
reactive_rule([holds(person(Unit,cautious),T1),holds(in_sight(Unit,person),T1)],[happens(walk_away(Unit),T2,T3)],97).
reactive_rule([happens(hit_from(Unit,Unit1,H,D),T1,T2),holds(person(Unit1,cautious),T2),holds(in_view(Unit1,D,person),T2)],[happens(move_away(Unit1,D),T3,T4)],98).
reactive_rule([happens(turn_hit(Unit,D,X,Y,Unit,H,D1),T1,T2),holds(person(Unit1,cautious),T2),holds(in_view(Unit1,D,person),T2)],[happens(move_away(Unit1,D),T3,T4)],98).
reactive_rule([happens(hit_from(Unit,Unit1,H,D),T1,T2),holds(person(Unit1,normal),T2),holds(in_view(Unit1,D,person),T2)],[happens(turn_and_attack(Unit1,D,Unit),T3,T4)],98).
reactive_rule([happens(turn_hit(Unit,D,X,Y,Unit1,H,D1),T1,T2),holds(person(Unit1,normal),T2),holds(in_view(Unit1,D,person),T2)],[happens(turn_and_attack(Unit1,D,Unit),T3,T4)],98).
reactive_rule([holds(person(Unit,violent),T1),holds(in_sight(Unit,person),T1)],[happens(attack(Unit),T2,T3)],93).
reactive_rule([holds(person(Unit,violent),T1),holds(not in_sight(Unit,person),T1)],[happens(look(Unit,person),T2,T3)],92).
reactive_rule([happens(hit_from(Unit,Unit1,H,D),T1,T2),holds(person(Unit1,violent),T2),holds(in_view(Unit1,D,person),T2)],[happens(turn_and_attack(Unit1,D,Unit),T3,T4)],98).
reactive_rule([happens(turn_hit(Unit,D,X,Y,Unit1,H,D1),T1,T2),holds(person(Unit1,violent),T2),holds(in_view(Unit1,D,person),T2)],[happens(turn_and_attack(Unit1,D,Unit),T3,T4)],98).
reactive_rule([holds(cycles(7),T1),holds(hunger(alex,N),T1)],[happens(lower_hunger(alex,N),T1,T2)],99).
reactive_rule([holds(cycles(7),T1),holds(hunger(amanda,N),T1)],[happens(lower_hunger(amanda,N),T1,T2)],99).
reactive_rule([holds(cycles(7),T1),holds(hunger(katherine,N),T1)],[happens(lower_hunger(katherine,N),T2,T3)],99).
reactive_rule([holds(cycles(7),T1),holds(hunger(peter,N),T1)],[happens(lower_hunger(peter,N),T2,T3)],99).
reactive_rule([holds(cycles(7),T1),holds(hunger(tom,N),T1)],[happens(lower_hunger(tom,N),T2,T3)],99).
reactive_rule([happens(reduce_hunger(Unit,N),T1,T2)],[happens(check_hunger(Unit),T3,T4)],91).
reactive_rule([happens(reduce_hunger(Unit,N),T1,T2)],[happens(consume_food(Unit),T3,T4)],96).
reactive_rule([happens(eat(Unit,V,F,N,A,B,C),T1,T2)],[happens(check_food(Unit),T3,T4)],94).
reactive_rule([happens(fire(X,Y),T1,T2)],[happens(on_fire(X,Y),T3,T4)],99).
reactive_rule([holds(hunger(Unit,20),T1)],[happens(check_health(Unit),T1,T2)],90).
reactive_rule([happens(successful_shelter(Unit,N,X,Y),T1,T2),holds(person(Unit,cautious),T2)],[happens(go_to_shelter(Unit),T3,T4)],91).
turn(Unit,N,D,X,Y)from T1 to T2 initiates position(Unit,D,X,Y).
turn(Unit,N,D,X,Y)from T1 to T2 initiates turns(Unit,N1) if
    increment(N,N1).
turn(Unit,N,D,X,Y)from T1 to T2 initiates cycles(T) if
    modu(T,T1).
walk(Unit,D,X,Y)from T1 to T2 initiates position(Unit,D,X,Y).
walk(Unit,D,X,Y)from T1 to T2 initiates turns(Unit,0).
walk(Unit,D,X,Y)from T1 to T2 initiates cycles(T) if
    modu(T,T1).
successful_shelter(Unit,N,X,Y)from T1 to T2 initiates holds_wood(Unit,N1) if
    decrease(N,N1,25).
successful_shelter(Unit,N,X,Y)from T1 to T2 initiates has_shelter(Unit,true).
successful_shelter(Unit,N,X,Y)from T1 to T2 initiates shelter(Unit,100,X,Y).
successful_shelter(Unit,N,X,Y)from T1 to T2 initiates cycles(T) if
    modu(T,T1).
successful_shelter(Unit,N,X,Y)from T1 to T2 initiates turns(Unit,0).
break_tree(Unit,X,Y)from T1 to T2 initiates wood(X,Y).
break_tree(Unit,X,Y)from T1 to T2 initiates turns(Unit,0).
break_tree(Unit,X,Y)from T1 to T2 initiates cycles(T) if
    modu(T,T1).
break_tree(Unit,X,Y)from T1 to T2 initiates turns(Unit,0).
collect_wood(Unit,N,X,Y)from T1 to T2 initiates holds_wood(Unit,N1) if
    increase(N,N1,5).
collect_wood(Unit,N,X,Y)from T1 to T2 initiates cycles(T) if
    modu(T,T1).
collect_wood(Unit,N,X,Y)from T1 to T2 initiates turns(Unit,0).
change_animal(Unit,N,D,X,Y,N1,Type)from T1 to T2 initiates animal(Type,N1,D,X,Y) if
    greater_than(N1,0).
change_animal(Unit,N,D,X,Y,0,rabbit)from T1 to T2 initiates food(2,1,0,X,Y).
change_animal(Unit,N,D,X,Y,0,chicken)from T1 to T2 initiates food(2,1,1,X,Y).
change_animal(Unit,N,D,X,Y,0,cow)from T1 to T2 initiates food(2,1,2,X,Y).
change_animal(Unit,N,D,X,Y,N1,Type)from T1 to T2 initiates cycles(T) if
    modu(T,T1).
change_animal(Unit,N,D,X,Y,N1,Type)from T1 to T2 initiates turns(Unit,0).
collect_food(Unit,A,B,C,K,L,M,X,Y)from T1 to T2 initiates holds_food(Unit,N1,A1,B1,C1) if
    increase(A,A1,K),
    increase(B,B1,L),
    increase(C,C1,M),
    calculate(N1,A1,B1,C1).
collect_food(Unit,A,B,C,K,L,M,X,Y)from T1 to T2 initiates cycles(T) if
    modu(T,T1).
collect_food(Unit,A,B,C,K,L,M,X,Y)from T1 to T2 initiates turns(Unit,0).
hit_shelter(Unit,Unit1,H,D,X,Y)from T1 to T2 initiates shelter(Unit1,H,X,Y) if
    greater_than(H,0).
hit_shelter(Unit,Unit1,H,D,X,Y)from T1 to T2 initiates cycles(T) if
    modu(T,T1).
hit_shelter(Unit,Unit1,H,D,X,Y)from T1 to T2 initiates has_shelter(Unit1,false) if
    equal(H,0).
hit_shelter(Unit,Unit1,H,D,X,Y)from T1 to T2 initiates turns(Unit,0).
hit_from(Unit,Unit1,H,D)from T1 to T2 initiates health(Unit1,H) if
    greater_than(H,0).
hit_from(Unit,Unit1,H,D)from T1 to T2 initiates cycles(T) if
    modu(T,T1).
hit_from(Unit,Unit1,H,D)from T1 to T2 initiates dead(Unit1) if
    equal(H,0).
hit_from(Unit,Unit1,H,D)from T1 to T2 initiates turns(Unit,0).
turn_hit_shelter(Unit,D,X,Y,Unit1,H,D1,X1,Y1)from T1 to T2 initiates shelter(Unit1,H,X1,Y1) if
    greater_than(H,0).
turn_hit_shelter(Unit,D,X,Y,Unit1,H,D1,X1,Y1)from T1 to T2 initiates has_shelter(Unit1,false) if
    equal(H,0).
turn_hit_shelter(Unit,D,X,Y,Unit1,H,D1,X1,Y1)from T1 to T2 initiates position(Unit,D,X,Y).
turn_hit_shelter(Unit,D,X,Y,Unit1,H,D1,X1,Y1)from T1 to T2 initiates cycles(T) if
    modu(T,T1).
turn_hit_shelter(Unit,D,X,Y,Unit1,H,D1,X1,Y1)from T1 to T2 initiates turns(Unit,0).
turn_hit(Unit,D,X,Y,Unit1,H,D1)from T1 to T2 initiates health(Unit1,H) if
    greater_than(H,0).
turn_hit(Unit,D,X,Y,Unit1,H,D1)from T1 to T2 initiates position(Unit,D,X,Y).
turn_hit(Unit,D,X,Y,Unit1,H,D1)from T1 to T2 initiates cycles(T) if
    modu(T,T1).
turn_hit(Unit,D,X,Y,Unit1,H,D1)from T1 to T2 initiates dead(Unit1) if
    equal(H,0).
turn_hit(Unit,D,X,Y,Unit1,H,D1)from T1 to T2 initiates turns(Unit,0).
lower_health(Unit,H)from T1 to T2 initiates health(Unit,H) if
    greater_than(H,0).
lower_health(Unit,H)from T1 to T2 initiates cycles(T) if
    modu(T,T1).
lower_health(Unit,H)from T1 to T2 initiates turns(Unit,0).
lower_health(Unit,H)from T1 to T2 initiates dead(Unit) if
    equal(H,0).
reduce_hunger(Unit,N)from T1 to T2 initiates hunger(Unit,N).
reduce_hunger(Unit,N)from T1 to T2 initiates cycles(T) if
    modu(T,T1).
reduce_hunger(Unit,N)from T1 to T2 initiates turns(Unit,0).
eat(Unit,V,F,N,A,B,C)from T1 to T2 initiates hunger(Unit,F1) if
    increase(F,F1,V).
eat(Unit,5,F,N,A,B,C)from T1 to T2 initiates holds_food(Unit,N1,A,B,C1) if
    decrease(N,N1,5),
    decrement(C,C1).
eat(Unit,3,F,N,A,B,C)from T1 to T2 initiates holds_food(Unit,N1,A,B1,C) if
    decrease(N,N1,3),
    decrement(B,B1).
eat(Unit,1,F,N,A,B,C)from T1 to T2 initiates holds_food(Unit,N1,A1,B,C) if
    decrease(N,N1,1),
    decrement(A,A1).
eat(Unit,V,F,N,A,B,C)from T1 to T2 initiates turns(Unit,0).
eat(Unit,V,F,N,A,B,C)from T1 to T2 initiates cycles(T) if
    modu(T,T1).
burn(Unit,H)from T1 to T2 initiates health(Unit,H) if
    greater_than(H,0).
burn(Unit,H)from T1 to T2 initiates turns(Unit,0).
burn(Unit,H)from T1 to T2 initiates cycles(T) if
    modu(T,T1).
burn(Unit,H)from T1 to T2 initiates dead(Unit) if
    equal(H,0).
heal(Unit,H)from T1 to T2 initiates health(Unit,H1) if
    increment(H,H1).
heal(Unit,H)from T1 to T2 initiates turns(Unit,0).
heal(Unit,H)from T1 to T2 initiates cycles(T) if
    modu(T,T1).

false
    turn(Unit,N,D,X,Y)from T1 to T2,
    turn(Unit,N1,D1,X1,Y1)from T1 to T2.
false
    turn(Unit,N,D,X,Y)from T1 to T2,
    walk(Unit,D1,X1,Y1)from T1 to T2.
false
    turn(Unit,N,D,X,Y)from T1 to T2,
    successful_shelter(Unit,N1,X1,Y1)from T1 to T2.
false
    turn(Unit,N,D,X,Y)from T1 to T2,
    break_tree(Unit,X1,Y1)from T1 to T2.
false
    turn(Unit,N,D,X,Y)from T1 to T2,
    collect_wood(Unit,W,X1,Y1)from T1 to T2.
false
    turn(Unit,N,D,X,Y)from T1 to T2,
    change_animal(Unit,N1,D1,X1,Y1,N2,A)from T1 to T2.
false
    turn(Unit,N,D,X,Y)from T1 to T2,
    collect_food(Unit,A,B,C,K,L,M,X1,Y1)from T1 to T2.
false
    turn(Unit,N,D,X,Y)from T1 to T2,
    hit_shelter(Unit,Unit1,H,D1,X1,Y1)from T1 to T2.
false
    turn(Unit,N,D,X,Y)from T1 to T2,
    hit_from(Unit,Unit1,H,D1)from T1 to T2.
false
    turn(Unit,N,D,X,Y)from T1 to T2,
    turn_hit(Unit,D1,X1,Y1,Unit1,H,D2)from T1 to T2.
false
    turn(Unit,N,D,X,Y)from T1 to T2,
    turn_hit_shelter(Unit,D1,X1,Y1,Unit1,H,D2,X2,Y2)from T1 to T2.
false
    turn(Unit,N,D,X,Y)from T1 to T2,
    eat(Unit,V,F,N1,A,B,C)from T1 to T2.
false
    walk(Unit,D,X,Y)from T1 to T2,
    walk(Unit,D1,X1,Y1)from T1 to T2.
false
    walk(Unit,D,X,Y)from T1 to T2,
    successful_shelter(Unit,N1,X1,Y1)from T1 to T2.
false
    walk(Unit,D,X,Y)from T1 to T2,
    break_tree(Unit,X1,Y1)from T1 to T2.
false
    walk(Unit,D,X,Y)from T1 to T2,
    collect_wood(Unit,W,X1,Y1)from T1 to T2.
false
    walk(Unit,D,X,Y)from T1 to T2,
    change_animal(Unit,N1,D1,X1,Y1,N2,A)from T1 to T2.
false
    walk(Unit,D,X,Y)from T1 to T2,
    collect_food(Unit,A,B,C,K,L,M,X1,Y1)from T1 to T2.
false
    walk(Unit,D,X,Y)from T1 to T2,
    hit_shelter(Unit,Unit1,H,D1,X1,Y1)from T1 to T2.
false
    walk(Unit,D,X,Y)from T1 to T2,
    hit_from(Unit,Unit1,H,D1)from T1 to T2.
false
    walk(Unit,D,X,Y)from T1 to T2,
    turn_hit(Unit,D1,X1,Y1,Unit1,H,D2)from T1 to T2.
false
    walk(Unit,D,X,Y)from T1 to T2,
    turn_hit_shelter(Unit,D1,X1,Y1,Unit1,H,D2,X2,Y2)from T1 to T2.
false
    walk(Unit,D,X,Y)from T1 to T2,
    eat(Unit,V,F,N1,A,B,C)from T1 to T2.
false
    successful_shelter(Unit,N,X,Y)from T1 to T2,
    successful_shelter(Unit,N1,X1,Y1)from T1 to T2.
false
    successful_shelter(Unit,N,X,Y)from T1 to T2,
    break_tree(Unit,X1,Y1)from T1 to T2.
false
    successful_shelter(Unit,N,X,Y)from T1 to T2,
    collect_wood(Unit,W,X1,Y1)from T1 to T2.
false
    successful_shelter(Unit,N,X,Y)from T1 to T2,
    change_animal(Unit,N1,D1,X1,Y1,N2,A)from T1 to T2.
false
    successful_shelter(Unit,N,X,Y)from T1 to T2,
    collect_food(Unit,A,B,C,K,L,M,X1,Y1)from T1 to T2.
false
    successful_shelter(Unit,N,X,Y)from T1 to T2,
    hit_shelter(Unit,Unit1,H,D1,X1,Y1)from T1 to T2.
false
    successful_shelter(Unit,N,X,Y)from T1 to T2,
    hit_from(Unit,Unit1,H,D1)from T1 to T2.
false
    successful_shelter(Unit,N,X,Y)from T1 to T2,
    turn_hit(Unit,D1,X1,Y1,Unit1,H,D2)from T1 to T2.
false
    successful_shelter(Unit,N,X,Y)from T1 to T2,
    turn_hit_shelter(Unit,D1,X1,Y1,Unit1,H,D2,X2,Y2)from T1 to T2.
false
    successful_shelter(Unit,N,X,Y)from T1 to T2,
    eat(Unit,V,F,N1,A,B,C)from T1 to T2.
false
    break_tree(Unit,X,Y)from T1 to T2,
    break_tree(Unit,X1,Y1)from T1 to T2.
false
    break_tree(Unit,X,Y)from T1 to T2,
    collect_wood(Unit,W,X1,Y1)from T1 to T2.
false
    break_tree(Unit,X,Y)from T1 to T2,
    change_animal(Unit,N1,D1,X1,Y1,N2,A)from T1 to T2.
false
    break_tree(Unit,X,Y)from T1 to T2,
    collect_food(Unit,A,B,C,K,L,M,X1,Y1)from T1 to T2.
false
    break_tree(Unit,X,Y)from T1 to T2,
    hit_shelter(Unit,Unit1,H,D1,X1,Y1)from T1 to T2.
false
    break_tree(Unit,X,Y)from T1 to T2,
    hit_from(Unit,Unit1,H,D1)from T1 to T2.
false
    break_tree(Unit,X,Y)from T1 to T2,
    turn_hit(Unit,D1,X1,Y1,Unit1,H,D2)from T1 to T2.
false
    break_tree(Unit,X,Y)from T1 to T2,
    turn_hit_shelter(Unit,D1,X1,Y1,Unit1,H,D2,X2,Y2)from T1 to T2.
false
    break_tree(Unit,X,Y)from T1 to T2,
    eat(Unit,V,F,N1,A,B,C)from T1 to T2.
false
    collect_wood(Unit,W,X,Y)from T1 to T2,
    collect_wood(Unit,W1,X1,Y1)from T1 to T2.
false
    collect_wood(Unit,W,X,Y)from T1 to T2,
    change_animal(Unit,N1,D1,X1,Y1,N2,A)from T1 to T2.
false
    collect_wood(Unit,W,X,Y)from T1 to T2,
    collect_food(Unit,A,B,C,K,L,M,X1,Y1)from T1 to T2.
false
    collect_wood(Unit,W,X,Y)from T1 to T2,
    hit_shelter(Unit,Unit1,H,D1,X1,Y1)from T1 to T2.
false
    collect_wood(Unit,W,X,Y)from T1 to T2,
    hit_from(Unit,Unit1,H,D1)from T1 to T2.
false
    collect_wood(Unit,W,X,Y)from T1 to T2,
    turn_hit(Unit,D1,X1,Y1,Unit1,H,D2)from T1 to T2.
false
    collect_wood(Unit,W,X,Y)from T1 to T2,
    turn_hit_shelter(Unit,D1,X1,Y1,Unit1,H,D2,X2,Y2)from T1 to T2.
false
    collect_wood(Unit,W,X,Y)from T1 to T2,
    eat(Unit,V,F,N1,A,B,C)from T1 to T2.
false
    change_animal(Unit,N,D,X,Y,N1,A)from T1 to T2,
    change_animal(Unit,N2,D1,X1,Y1,N3,A1)from T1 to T2.
false
    change_animal(Unit,N,D,X,Y,N1,A)from T1 to T2,
    collect_food(Unit,A1,B,C,K,L,M,X1,Y1)from T1 to T2.
false
    change_animal(Unit,N,D,X,Y,N1,A)from T1 to T2,
    hit_shelter(Unit,Unit1,H,D1,X1,Y1)from T1 to T2.
false
    change_animal(Unit,N,D,X,Y,N1,A)from T1 to T2,
    hit_from(Unit,Unit1,H,D1)from T1 to T2.
false
    change_animal(Unit,N,D,X,Y,N1,A)from T1 to T2,
    turn_hit(Unit,D1,X1,Y1,Unit1,H,D2)from T1 to T2.
false
    change_animal(Unit,N,D,X,Y,N1,A)from T1 to T2,
    turn_hit_shelter(Unit,D1,X1,Y1,Unit1,H,D2,X2,Y2)from T1 to T2.
false
    change_animal(Unit,N,D,X,Y,N1,A)from T1 to T2,
    eat(Unit,V,F,N2,A1,B,C)from T1 to T2.
false
    collect_food(Unit,A,B,C,K,L,M,X,Y)from T1 to T2,
    collect_food(Unit,A1,B1,C1,K1,L1,M1,X1,Y1)from T1 to T2.
false
    collect_food(Unit,A,B,C,K,L,M,X,Y)from T1 to T2,
    hit_shelter(Unit,Unit1,H,D1,X1,Y1)from T1 to T2.
false
    collect_food(Unit,A,B,C,K,L,M,X,Y)from T1 to T2,
    hit_from(Unit,Unit1,H,D1)from T1 to T2.
false
    collect_food(Unit,A,B,C,K,L,M,X,Y)from T1 to T2,
    turn_hit(Unit,D1,X1,Y1,Unit1,H,D2)from T1 to T2.
false
    collect_food(Unit,A,B,C,K,L,M,X,Y)from T1 to T2,
    turn_hit_shelter(Unit,D1,X1,Y1,Unit1,H,D2,X2,Y2)from T1 to T2.
false
    collect_food(Unit,A,B,C,K,L,M,X,Y)from T1 to T2,
    eat(Unit,V,F,N1,A1,B1,C1)from T1 to T2.
false
    hit_shelter(Unit,Unit1,H,D,X,Y)from T1 to T2,
    hit_shelter(Unit,Unit2,H2,D2,X2,Y2)from T1 to T2.
false
    hit_shelter(Unit,Unit1,H,D,X,Y)from T1 to T2,
    hit_from(Unit,Unit2,H1,D1)from T1 to T2.
false
    hit_shelter(Unit,Unit1,H,D,X,Y)from T1 to T2,
    turn_hit(Unit,D1,X1,Y1,Unit2,H2,D2)from T1 to T2.
false
    hit_shelter(Unit,Unit1,H,D,X,Y)from T1 to T2,
    turn_hit_shelter(Unit,D1,X1,Y1,Unit2,H2,D2,X2,Y2)from T1 to T2.
false
    hit_shelter(Unit,Unit1,H,D,X,Y)from T1 to T2,
    eat(Unit,V,F,N1,A1,B1,C1)from T1 to T2.
false
    hit_from(Unit,Unit1,H,D)from T1 to T2,
    hit_from(Unit,Unit2,H2,D2)from T1 to T2.
false
    hit_from(Unit,Unit1,H,D)from T1 to T2,
    turn_hit(Unit,D1,X1,Y1,Unit2,H2,D2)from T1 to T2.
false
    hit_from(Unit,Unit1,H,D)from T1 to T2,
    turn_hit_shelter(Unit,D1,X1,Y1,Unit2,H2,D2,X2,Y2)from T1 to T2.
false
    hit_from(Unit,Unit1,H,D)from T1 to T2,
    eat(Unit,V,F,N1,A1,B1,C1)from T1 to T2.
false
    turn_hit(Unit,D,X,Y,Unit1,H1,D1)from T1 to T2,
    turn_hit(Unit,D2,X2,Y2,Unit3,H3,D3)from T1 to T2.
false
    turn_hit(Unit,D,X,Y,Unit1,H1,D1)from T1 to T2,
    turn_hit_shelter(Unit,D1,X1,Y1,Unit2,H2,D2,X2,Y2)from T1 to T2.
false
    turn_hit(Unit,D,X,Y,Unit1,H1,D1)from T1 to T2,
    eat(Unit,V,F,N1,A1,B1,C1)from T1 to T2.
false
    turn_hit_shelter(Unit,D,X,Y,Unit1,H1,D1,X1,Y1)from T1 to T2,
    turn_hit_shelter(Unit,D2,X2,Y2,Unit3,H3,D3,X3,Y3)from T1 to T2.
false
    turn_hit_shelter(Unit,D,X,Y,Unit1,H1,D1,X1,Y1)from T1 to T2,
    eat(Unit,V,F,N1,A1,B1,C1)from T1 to T2.
false
    eat(Unit,V,F,N,A,B,C)from T1 to T2,
    eat(Unit,V1,F1,N1,A1,B1,C1)from T1 to T2.
false
    walk(Unit,D,X,Y)from T1 to T2,
    walk(Unit1,D1,X,Y)from T1 to T2,
    not_equal(Unit,Unit1).
false
    walk(Unit,D,X,Y)from T1 to T2,
    successful_shelter(Unit1,N1,X,Y)from T1 to T2,
    not_equal(Unit,Unit1).
false
    walk(Unit,D,X,Y)from T1 to T2,
    turn_hit(Unit1,D1,X,Y,Unit2,H2,D2)from T1 to T2,
    not_equal(Unit,Unit1).
false
    walk(Unit,D,X,Y)from T1 to T2,
    turn_hit_shelter(Unit1,D1,X,Y,Unit2,H,D2,X2,Y2)from T1 to T2,
    not_equal(Unit,Unit1).
false
    successful_shelter(Unit,N,X,Y)from T1 to T2,
    successful_shelter(Unit1,N,X,Y)from T1 to T2,
    not_equal(Unit,Unit1).
false
    successful_shelter(Unit,N,X,Y)from T1 to T2,
    turn_hit(Unit1,D1,X,Y,Unit2,H,D2)from T1 to T2,
    not_equal(Unit,Unit1).
false
    successful_shelter(Unit,N,X,Y)from T1 to T2,
    turn_hit_shelter(Unit1,D1,X,Y,Unit2,H,D2,X2,Y2)from T1 to T2,
    not_equal(Unit,Unit1).
false
    break_tree(Unit,X,Y)from T1 to T2,
    break_tree(Unit1,X,Y)from T1 to T2,
    not_equal(Unit,Unit1).
false
    collect_wood(Unit,W,X,Y)from T1 to T2,
    collect_wood(Unit1,W1,X,Y)from T1 to T2,
    not_equal(Unit,Unit1).
false
    change_animal(Unit,N,D,X,Y,N1,A)from T1 to T2,
    change_animal(Unit1,N2,D2,X,Y,N3,A1)from T1 to T2,
    not_equal(Unit,Unit1).
false
    collect_food(Unit,A,B,C,K,L,M,X,Y)from T1 to T2,
    collect_food(Unit1,A1,B1,C1,K1,L1,M1,X,Y)from T1 to T2,
    not_equal(Unit,Unit1).
false
    hit_shelter(Unit,Unit2,H,D,X,Y)from T1 to T2,
    hit_shelter(Unit1,Unit3,H1,D1,X,Y)from T1 to T2,
    not_equal(Unit,Unit1).
false
    hit_shelter(Unit,Unit2,H,D,X,Y)from T1 to T2,
    turn_hit_shelter(Unit1,D1,X1,Y1,Unit3,H1,D1,X,Y)from T1 to T2,
    not_equal(Unit,Unit1).
false
    hit_from(Unit,Unit2,H,D)from T1 to T2,
    hit_from(Unit1,Unit2,H1,D1)from T1 to T2,
    not_equal(Unit,Unit1).
false
    hit_from(Unit,Unit2,H,D)from T1 to T2,
    turn_hit(Unit1,D1,X1,Y1,Unit2,H2,D2)from T1 to T2,
    not_equal(Unit,Unit1).
false
    turn_hit(Unit,D,X,Y,Unit2,H2,D2)from T1 to T2,
    turn_hit(Unit1,D1,X1,Y1,Unit2,H3,D3)from T1 to T2,
    not_equal(Unit,Unit1).
false
    turn_hit(Unit,D,X,Y,Unit2,H2,D2)from T1 to T2,
    turn_hit(Unit1,D1,X,Y,Unit3,H3,D3)from T1 to T2,
    not_equal(Unit,Unit1).
false
    turn_hit_shelter(Unit,D,X,Y,Unit2,H2,D2,X2,Y2)from T1 to T2,
    turn_hit_shelter(Unit1,D1,X1,Y1,Unit3,H3,D3,X2,Y2)from T1 to T2,
    not_equal(Unit,Unit1).
false
    turn_hit_shelter(Unit,D,X,Y,Unit2,H2,D2,X2,Y2)from T1 to T2,
    turn_hit_shelter(Unit1,D1,X,Y,Unit3,H3,D3,X3,Y3)from T1 to T2,
    not_equal(Unit,Unit1).
false
    turn_hit(Unit,D,X,Y,Unit1,H,D1)from T1 to T2,
    lower_health(Unit1,N)from T1 to T2.
false
    hit_from(Unit,Unit1,H,D)from T1 to T2,
    lower_health(Unit1,N)from T1 to T2.
false
    turn_hit(Unit,D,X,Y,Unit1,H,D1)from T1 to T2,
    burn(Unit1,N)from T1 to T2.
false
    hit_from(Unit,Unit1,H,D)from T1 to T2,
    burn(Unit1,N)from T1 to T2.
false
    turn_hit(Unit,D,X,Y,Unit1,H,D1)from T1 to T2,
    heal(Unit1,N)from T1 to T2.
false
    heal(Unit1,N)from T1 to T2,
    hit_from(Unit,Unit1,H,D)from T1 to T2.
false
    lower_health(Unit,N)from T1 to T2,
    burn(Unit,N1)from T1 to T2.
false
    lower_health(Unit,N)from T1 to T2,
    heal(Unit,N1)from T1 to T2.
false
    burn(Unit,N)from T1 to T2,
    heal(Unit,N1)from T1 to T2.
false
    eat(Unit,V,F,N,A,B,C)from T1 to T2,
    reduce_hunger(Unit,H)from T1 to T2.
false
    walk(Unit,D,X,Y)from T1 to T2,
    position(Unit1,Direction,X,Y)at T2.
false
    walk(Unit,D,X,Y)from T1 to T2,
    animal(Type,N,Direction,X,Y)at T2.
false
    walk(Unit,D,X,Y)from T1 to T2,
    shelter(Unit1,H,X,Y)at T2,
    not_equal(Unit,Unit1).
false
    walk(Unit,D,X,Y)from T1 to T2,
    tree(X,Y)at T2.
false
    walk(Unit,D,X,Y)from T1 to T2,
    wood(X,Y)at T2.
false
    walk(Unit,D,X,Y)from T1 to T2,
    food(A,B,C,X,Y)at T2.
false
    walk(Unit,D,X,Y)from T1 to T2,
    greater_than(X,20).
false
    walk(Unit,D,X,Y)from T1 to T2,
    greater_than(Y,20).
false
    walk(Unit,D,X,Y)from T1 to T2,
    less_than(X,1).
false
    walk(Unit,D,X,Y)from T1 to T2,
    less_than(Y,1).
false
    successful_shelter(Unit,N,X,Y)from T1 to T2,
    position(Unit1,Direction,X,Y)at T2.
false
    successful_shelter(Unit,N,X,Y)from T1 to T2,
    animal(Type,N1,Direction,X,Y)at T2.
false
    successful_shelter(Unit,N,X,Y)from T1 to T2,
    shelter(Unit1,H,X,Y)at T2,
    not_equal(Unit,Unit1).
false
    successful_shelter(Unit,N,X,Y)from T1 to T2,
    tree(X,Y)at T2.
false
    successful_shelter(Unit,N,X,Y)from T1 to T2,
    wood(X,Y)at T2.
false
    successful_shelter(Unit,N,X,Y)from T1 to T2,
    food(A,B,C,X,Y)at T2.
false
    successful_shelter(Unit,N,X,Y)from T1 to T2,
    greater_than(X,20).
false
    successful_shelter(Unit,N,X,Y)from T1 to T2,
    greater_than(Y,20).
false
    successful_shelter(Unit,N,X,Y)from T1 to T2,
    less_than(X,1).
false
    successful_shelter(Unit,N,X,Y)from T1 to T2,
    less_than(Y,1).
false
    turn(Unit,N1,D1,X1,Y1)from T1 to T2,
    dead(Unit)at T2.
false
    walk(Unit,D1,X1,Y1)from T1 to T2,
    dead(Unit)at T2.
false
    successful_shelter(Unit,N1,X1,Y1)from T1 to T2,
    dead(Unit)at T2.
false
    break_tree(Unit,X1,Y1)from T1 to T2,
    dead(Unit)at T2.
false
    collect_wood(Unit,W,X1,Y1)from T1 to T2,
    dead(Unit)at T2.
false
    change_animal(Unit,N1,D1,X1,Y1,N2,A)from T1 to T2,
    dead(Unit)at T2.
false
    collect_food(Unit,A,B,C,K,L,M,X1,Y1)from T1 to T2,
    dead(Unit)at T2.
false
    hit_shelter(Unit,Unit1,H,D1,X1,Y1)from T1 to T2,
    dead(Unit)at T2.
false
    hit_from(Unit,Unit1,H,D1)from T1 to T2,
    dead(Unit)at T2.
false
    turn_hit(Unit,D1,X1,Y1,Unit1,H,D2)from T1 to T2,
    dead(Unit)at T2.
false
    turn_hit_shelter(Unit,D1,X1,Y1,Unit1,H,D2,X2,Y2)from T1 to T2,
    dead(Unit)at T2.
false
    eat(Unit,V,F,N1,A,B,C)from T1 to T2,
    dead(Unit)at T2.

event(start_game(_24248)).

event(fire(_24248,_24250)).


