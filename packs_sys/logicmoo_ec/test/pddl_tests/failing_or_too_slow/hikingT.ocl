% Automatically generated OCl Domain

% This version avoids the use of conditional operators

domain_name(hiking).

% Sorts
sorts(primitive_sorts,[car,person,tent,place,couple]).

% Objects
objects(car,[car1,car2]).
objects(tent,[tent1]).
objects(person,[sue,fred]).
objects(couple,[couple1]).
objects(place,[keswick,helvelyn,fairfield,honister,derwent]).

% Predicates
predicates([
    at_tent(tent,place),
    at_person(person,place),
    at_car(car,place),
    partners(couple,person,person),
    up(tent),
    down(tent),
    walked(couple,place),
    next(place,place)]).

% Object Class Definitions
substate_classes(person,Person,[
    [at_person(Person,Place)]]).
substate_classes(couple,Couple,[
    [walked(Couple,Place),partners(Couple,Person1,Person2)]]).
substate_classes(tent,Tent,[
    [at_tent(Tent,Place),up(Tent)],
    [at_tent(Tent,Place),down(Tent)]]).
substate_classes(car,Car,[
    [at_car(Car,Place)]]).

% Atomic Invariants
atomic_invariants([
    partners(couple1,sue,fred),
    next(keswick,helvelyn),
    next(helvelyn,fairfield),
    next(fairfield,honister),
    next(honister,derwent)]).

% Implied Invariants

% Inconsistent Constraints

% Operators
operator(put_down(Person,Place,Tent),
    % prevail
    [     se(person,Person,[at_person(Person,Place)])],
    % necessary
    [     sc(tent,Tent,[at_tent(Tent,Place),up(Tent)]=>[at_tent(Tent,Place),down(Tent)])],
    % conditional
    []).
operator(put_up(Person,Place,Tent),
    % prevail
    [     se(person,Person,[at_person(Person,Place)])],
    % necessary
    [     sc(tent,Tent,[at_tent(Tent,Place),down(Tent)]=>[at_tent(Tent,Place),up(Tent)])],
    % conditional
    []).
operator(drive_passenger(Person,Place,Place2,Car,Person2),
    % prevail
    [],
    % necessary
    [     sc(person,Person,[at_person(Person,Place)]=>[at_person(Person,Place2)]),
     sc(car,Car,[at_car(Car,Place)]=>[at_car(Car,Place2)]),
     sc(person,Person2,[at_person(Person2,Place),ne(Person,Person2)]=>[at_person(Person2,Place2)])],
    % conditional
    []).
operator(drive(Person,Place,Place2,Car),
    % prevail
    [],
    % necessary
    [     sc(person,Person,[at_person(Person,Place)]=>[at_person(Person,Place2)]),
     sc(car,Car,[at_car(Car,Place)]=>[at_car(Car,Place2)])],
    % conditional
    []).
operator(drive_tent(Person,Place,Place2,Car,Tent),
    % prevail
    [],
    % necessary
    [     sc(person,Person,[at_person(Person,Place)]=>[at_person(Person,Place2)]),
     sc(car,Car,[at_car(Car,Place)]=>[at_car(Car,Place2)]),
     sc(tent,Tent,[at_tent(Tent,Place),down(Tent)]=>[at_tent(Tent,Place2),down(Tent)])],
    % conditional
    []).
operator(drive_tent_passenger(Person,Place,Place2,Car,Tent,Person2),
    % prevail
    [],
    % necessary
    [     sc(person,Person,[at_person(Person,Place)]=>[at_person(Person,Place2)]),
     sc(car,Car,[at_car(Car,Place)]=>[at_car(Car,Place2)]),
     sc(tent,Tent,[at_tent(Tent,Place),down(Tent)]=>[at_tent(Tent,Place2),down(Tent)]),
     sc(person,Person2,[at_person(Person2,Place),ne(Person,Person2)]=>[at_person(Person2,Place2)])],
    % conditional
    []).
operator(walk_together(Tent,Place2,Person1,Place1,Person2,Couple),
    % prevail
    [     se(tent,Tent,[at_tent(Tent,Place2),up(Tent)])],
    % necessary
    [     sc(person,Person1,[at_person(Person1,Place1),next(Place1,Place2)]=>[at_person(Person1,Place2)]),
     sc(person,Person2,[at_person(Person2,Place1),ne(Person1,Person2)]=>[at_person(Person2,Place2)]),
     sc(couple,Couple,[walked(Couple,Place1),partners(Couple,Person1,Person2)]=>[walked(Couple,Place2)])],
    % conditional
    []).

% Methods

% Domain Tasks
planner_task(10,
    % Goals
    [
     se(couple,couple1,[walked(couple1,derwent)])],
    % INIT States
    [
     ss(car,car1,[at_car(car1,keswick)]),
     ss(car,car2,[at_car(car2,keswick)]),
     ss(couple,couple1,[walked(couple1,keswick)]),
     ss(person,sue,[at_person(sue,keswick)]),
     ss(person,fred,[at_person(fred,keswick)]),
     ss(tent,tent1,[at_tent(tent1,keswick),up(tent1)])]).
