:- module(myfreeze, [myfreeze/2]).
/*
  Must use dmiles patch of SWI-Prolog for verify_attributes/3 to work
  Otherwise this is more a syntax test of atts.pl
*/

:- set_prolog_flag(runtime_debug,3).
:- set_prolog_flag(runtime_saftey,2).
:- set_prolog_flag(runtime_speed,1).

:- use_module(library(atts)).

:- attribute frozen/1.

verify_attributes(Var, Other, Goals) :-
        get_atts(Var, frozen(Fa)), !,       % are we involved?
        (   var(Other) ->                   % must be attributed then
            (   get_atts(Other, frozen(Fb)) % has a pending goal?
            ->  put_atts(Other, frozen((Fa,Fb))) % rescue conjunction
            ;   put_atts(Other, frozen(Fa)) % rescue the pending goal
            ),
            Goals = []
        ;   Goals = [Fa]
        ).
verify_attributes(_, _, []).

attribute_goal(Var, Goal) :-                % interpretation as goal
        get_atts(Var, frozen(Goal)).

myfreeze(X, Goal) :-
        put_atts(Fresh, frozen(Goal)),
        Fresh = X.
		
/*		
Assuming that this code lives in file `myfreeze.pl', we would use it via:

| ?- use_module(myfreeze).
| ?- myfreeze(X,print(bound(x,X))), X=2.

bound(x,2)                      % side effect
X = 2                           % bindings
The two solvers even work together:

| ?- myfreeze(X,print(bound(x,X))), domain(X,[1,2,3]),
     domain(Y,[2,10]), X=Y.

	 
	 
	 sting(test(_)).
*/