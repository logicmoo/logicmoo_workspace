/*********************** operators ****************************************/










% method(name,precons,transitions,statics,temps,decomposition)
% operator(name,prevail,transitions,cond_transitions)


method(
 % 1. name
      transport(P,O,D),
 % 2. dynamic constraints
      [ ],
 % 3. list of necessary substate changes
      [ sc(package, P, [at(P,O), is_of_sort(P,package)] => 
                       [at(P,D), delivered(P)]) ],
 % 4. static constraints
      [ ne(O,D),in_region(O,R),in_region(D,R)
       % list of static predicates that must be instantiated to
       % be true. Static preds may also appear in 2. and 3. if
       % its clearer that way 
       ],

 % 5.  temporal  constraints
       % list of static predicates before(N1,N2)
      [before(1,2),before(2,3)],
 % 6. decomposition
      [ achieve( ss(package, P,[waiting(P),certified(P)]) ), carry_direct(P,O,D), deliver(P,D)]
 ).

method(
      transport(P,O,D),
      [ ],
      [ sc(package, P, [at(P,O), is_of_sort(P,package)] =>
                       [at(P,D), delivered(P)]) ],
      [ ne(O,D),ne(R1,R2),is_of_sort(AV,aircraft),
        in_region(O,R1),in_region(D,R2),
        serves_region(A1,R1),serves_region(A2,R2)
       ],
      [before(1,2),before(2,3),before(3,4),before(4,5)],
      [ achieve( ss(package, P,[waiting(P),certified(P)]) ), 
        carry_direct(P,O,A1), carry_via_ap(A1,A2,P), 
        carry_direct(P,A2,D), deliver(P,D)]
 ).

method(
   carry_via_ap(O,D,P),
  [ ],
  [ sc(package, P, [at(P,O),waiting(P),certified(P)] =>
                       [at(P,D),waiting(P),certified(P)]) ],
      [ ne(O,D), 
%                ne(O,O1), is_of_sort(O1,airport),
        is_of_sort(O,airport), is_of_sort(D,airport),
        is_of_sort(P,package), is_of_sort(V,aircraft)],
 [before(1,3), before(2,3),before(3,4), before(4,5)],
  [
%       fly(V,O1,O),
       commission(V),
       achieve(ss(aircraft,V,[at(V,O)])),
       load_package(P,V,O),
       fly(V,O,D),
       unload_package(P,V,D)
       ]
).

% carry in one city
method(  
  carry_direct(P,O,D),
 [ ],
  [ sc(package, P, [at(P,O),waiting(P),certified(P)] => 
                       [at(P,D),waiting(P),certified(P)]) ],
 [is_of_sort(P,package),
       is_of_sort(V,truck),
       in_city(O,CY),
       in_city(D,CY)
       ],
 [before(1,2), before(2,3), before(3,4),before(4,5)
       ],
 [   %  achieve(ss(truck,V,[moveable(V), busy(V)])),
       commission(V),
       achieve(ss(truck,V,[at(V,O)])),
       load_package(P,V,O),
       move(V,O,D,local_roads),
       unload_package(P,V,D)
       ]
).

% carry between two cities by traincar
method(
  carry_direct(P,O,D),
 [ ],
  [ sc(package, P, [at(P,O),waiting(P),certified(P)] => 
                       [at(P,D),waiting(P),certified(P)]) ],
 [is_of_sort(P,package),
       is_of_sort(V,traincar),
       is_of_sort(Train,train),
       connects(R,O,D),
       rv_compatible(R,traincar),
       route_available(R)
       ],
 [before(1,2), before(2,3), before(3,4),before(4,5),
       before(5,6),before(6,7),before(7,8) ],
 [     commission(V),
       achieve(ss(train,Train,[at(Train,O),attached(Train,V)])),
       load_package(P,V,O),
       pull_traincar(Train,V,O,D,R),
       detach_traincar(Train,V,D),
       unload_package(P,V,D)
       ]
).

% carry between two cities by truck
method(
  carry_direct(P,O,D),
 [ ],
  [ sc(package, P, [at(P,O),waiting(P),certified(P)] => 
                       [at(P,D),waiting(P),certified(P)]) ],
 [is_of_sort(P,package),
       is_of_sort(V,truck),
       in_city(O,CY),
       in_city(D,CY1),
       ne(CY,CY1),
       connects(R,CY,CY1),
       is_of_sort(R,road_route),
       route_available(R)
       ],
 [before(1,2), before(2,3), before(3,4),before(4,5) ],
 [   %  achieve(ss(truck,V,[moveable(V), busy(V)])),
       commission(V),
       achieve(ss(truck,V,[at(V,O)])),
       load_package(P,V,O),
       move(V,O,D,R),
       unload_package(P,V,D)
       ]
).

method(
  move_traincar(V, O, L, R2),
 [ ],
         [sc(traincar,V,[at(V,O) ]
            =>[at(V,L)] )],
 [is_of_sort(V,traincar),
       connects(R2,O,L),
       is_of_sort(R2,rail_route),
       is_of_sort(Train,train) ],
 [before(1,2), before(2,3), before(3,4),before(4,5) ],
 [   achieve(ss(train,Train,[at(Train,O)])),
          attach_traincar(Train,V,O),
          pull_traincar(Train,V,O,L,R2),
          detach_traincar(Train,V,L) ]
).

/* getting docs ready */
 operator( pay_fees(P),
      [],
      [sc(package,P,[uncertified(P)]
      =>[waiting(P),certified(P)])],
      [ ]).

operator(fly(A,D1,D2),
      [ ],
      [sc(aircraft,A,[at(A,D1)]
            =>[at(A,D2)] )],
         [sc(package,X,[loaded(X,A),certified(X),at(X,D1)]
            => [loaded(X,A),certified(X),at(X,D2)])  ]
).


%move truck
operator( move(V, O, L, R), 
        [ ],
         [sc(truck,V,[at(V,O),
             is_of_sort(R,road_route),
             moveable(V),
             in_city(O,City),
             in_city(L,City1),
             ne(City,City1),
             connects(R,City,City1)]
            =>[at(V,L)] )],
         [sc(package,X,[loaded(X,V),certified(X),at(X,O)] 
            => [loaded(X,V),certified(X),at(X,L)])  ]
).

%move truck inside city
operator( move(V, O, L, local_roads), 
         [],
         [sc(truck,V,[at(V,O),
             moveable(V),
             in_city(O,City),
             in_city(L,City)]
            =>[at(V,L)]  )],
         [ sc(package,X,[loaded(X,V),certified(X),at(X,O)] 
            =>[loaded(X,V),certified(X),at(X,L)])   ]
).

%move traincar
operator( pull_traincar(Train,V1, O, L, Rt), 
         [  ],
         [ sc(train,Train,[at(Train,O),
             attached(Train,V1),
             moveable(Train),
             connects(Rt,O,L),
             is_of_sort(Rt,rail_route)]
            =>[at(Train,L),attached(Train,V1)] ),
           sc(traincar,V1,[at(V1,O),attached(V1,Train)]
            =>[at(V1,L),attached(V1,Train)]) ],
         [sc(package,P,[loaded(P,V1),certified(P),at(P,O)]
            =>[loaded(P,V1),certified(P),at(P,L)]) ]
).

operator( move_train(V, O, L, Rt),
         [ ],
          [sc(train,V,[at(V,O),unattached(V),
             moveable(V),available(V),
             connects(Rt,O,L),
             is_of_sort(Rt,rail_route)]
            =>[at(V,L),unattached(V),moveable(V),available(V)] )],
       [ ]
).

operator(attach_traincar(Train,V,O),
     [  ],
     [sc(train, Train, [at(Train,O),moveable(Train),available(Train),unattached(Train)]
        =>[at(Train,O),attached(Train,V),moveable(Train),busy(Train)] ),
     sc(traincar, V, [at(V,O),unattached(V)]
        =>[at(V,O),attached(V,Train)] ) ],
     [ ]
).

operator(detach_traincar(Train,V,O),
     [ ],
     [sc(train, Train, [attached(Train,V),moveable(Train),busy(Train)]
        =>[unattached(Train),moveable(Train),available(Train)] ),
     sc(traincar, V, [attached(V,Train)]
        =>[unattached(V)] ) ],
     [ ]
).

operator(commission(V),
      [ ],
      [sc(vehicle, V,[moveable(V),available(V)] =>[moveable(V), busy(V)])],
      [ ]).

         

operator( load_package(P,V,L),
   [ss(vehicle,V, [at(V,L),moveable(V),busy(V)])],
   [sc(package, P, [at(P,L),waiting(P),certified(P)]=>
      [at(P,L),loaded(P,V),certified(P)])],
   []
).

operator( unload_package(P,V,L),
 [],
 [sc(package, P, [at(P,L),loaded(P,V),certified(P)]=>[at(P,L),waiting(P),certified(P)]
),
 sc(vehicle,V, [at(V,L), moveable(V), busy(V)] => [at(V,L),moveable(V),available(V)])
],
 []
 ).


operator( deliver(P,L),
        [],
        [sc(package, P, [at(P,L),waiting(P),certified(P)]=>
          [at(P,L),delivered(P)] )],
        []
).







