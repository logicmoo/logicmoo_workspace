
/*

The Hiking Domain -- Lee McCluskey -- Aug 01

This version uses conditional operators

*/

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
    up(tent,place),
    down(tent,place),
    loaded(tent,car,place),
    in(person,car,place),
    out(person,place),
    at(car,place),
    partners(couple,person,person),
    walked(couple,place),
    next(place,place)]).

% Object Class Definitions
substate_classes(person,Person,[
    [out(Person,Place)],
     [in(Person,Car,Place)] 
 ]).
    
substate_classes(couple,Couple,[
    [walked(Couple,Place),partners(Couple,Person1,Person2)] ]).

substate_classes(tent,Tent,[
    [up(Tent,Place)],
    [down(Tent,Place)],
    [loaded(Tent,Car,Place)] ]).

substate_classes(car,Car,[
    [at(Car,Place)]  
  ]).

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

operator(load(Person,Tent,Place,Car),
   % prevail
    [se(person,Person,[out(Person,Place)]) ],
    % necessary
    [
     sc(tent,Tent,[down(Tent,Place) ] =>
             [loaded(Tent,Car,Place) ])
     ],
    % conditional
    []).
operator(unload(Person,Tent,Place,Car),
   % prevail
    [se(person,Person,[out(Person,Place)]) ],
    % necessary
    [
     sc(tent,Tent,[loaded(Tent,Car,Place) ] =>
             [down(Tent,Place) ])
     ],
    % conditional
    []).

operator(getin(Person,Place,Car),
   % prevail
    [se(car,Car,[at(Car,Place)]) ],
    % necessary
    [    
     sc(person,Person,[out(Person,Place)]=>[in(Person,Car,Place)])
     ],
    % conditional
    []).
operator(getout(Person,Place,Car),
   % prevail
    [se(car,Car,[at(Car,Place)]) ],
    % necessary
    [    
     sc(person,Person,[in(Person,Car,Place)]=>[out(Person,Place)])
     ],
    % conditional
    []).





operator(put_down(Person,Tent,Place),
    % prevail
    [se(person,Person,[out(Person,Place)]) ],
    % necessary
    [
     sc(tent,Tent,[up(Tent,Place)] =>
             [down(Tent,Place) ])
     ],
    % conditional
    []).
operator(put_up(Person,Tent,Place),
    % prevail
    [se(person,Person,[out(Person,Place)]) ],
    % necessary
    [
     sc(tent,Tent,[down(Tent,Place)] =>
             [up(Tent,Place)])
     ],
    % conditional
    []).

operator(drive(Person,Car,Place,Place2,Person2,Tent),
    % prevail
    [],
    % necessary
    [     
     sc(person,Person,[in(Person,Car,Place)]=>[in(Person,Car,Place2)]),
     sc(car,Car,[at(Car,Place)]=>[at(Car,Place2)])
     ],
    % conditional
    [
     sc(person,Person2,[in(Person2,Car,Place)]=>[in(Person2,Car,Place2)]),
     sc(tent, Tent, [loaded(Tent,Car,Place)]=>[loaded(Tent,Car,Place2)])
     ]
     ).

operator(walk_together(Person1,Person2,Couple,Place1,Place2,Tent),
    % prevail
[se(tent,Tent,[up(Tent,Place2)]) ],
    % necessary
    [     
    sc(person,Person1,[out(Person1,Place1),next(Place1,Place2)]=>[out(Person1,Place2)]),
    sc(person,Person2,[out(Person2,Place1),ne(Person1,Person2)]=>[out(Person2,Place2)]),       
    sc(couple,Couple,[walked(Couple,Place1),partners(Couple,Person1,Person2)]=>[walked(Couple,Place2)])
     ],
    % conditional
    []).


planner_task(10,
    % Goals
    [
     se(couple,couple1,[walked(couple1,derwent)])
    ],
    % INIT States
    [

     ss(car,car1,[at(car1,keswick)]),
     ss(car,car2,[at(car2,keswick)]),
     ss(couple,couple1,[walked(couple1,keswick)]),
     ss(person,sue,[out(sue,keswick)]),
     ss(person,fred,[out(fred,keswick)]),
     ss(tent,tent1,[up(tent1,keswick)])
]).

