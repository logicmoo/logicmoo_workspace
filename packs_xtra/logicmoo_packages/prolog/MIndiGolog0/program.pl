%%
%%  program.pl:  MIndiGolog procedure definitions
%%
%%  Copyright 2005, Ryan Kelly
%%
%%  The predicate proc/2 is used to define procedures.  Its first argument
%%  is called with the term representing the procedure call, and it should
%%  bind its second argument to the corresponding procedure body.
%%


%%  Ensure that the agent has control of an object
proc(ensureHas(Agt,Obj),
     if(has_object(Agt,Obj,now),nil,acquire_object(Agt,Obj))
    ).

%%  Carry out the necessary sequence of actions to place one object
%%  inside another, releasing the destination container when done.
proc(doPlaceIn(Agt,Obj,Dest),
     ensureHas(Agt,Obj) // ensureHas(Agt,Dest)
     : place_in(Agt,Obj,Dest)
     : release_object(Agt,Dest)
    ).

%%  Nondeterministically select an object of a given type, gain control
%%  of it, and place it inside a container object.
proc(doPlaceTypeIn(Agt,Type,Dest),
     pi(obj,?and(obj_is_type(obj,Type),not(used(obj,now)))
            : acquire_object(Agt,obj)
            : doPlaceIn(Agt,obj,Dest))
    ).

%%  Carry out the necessary actions to transfer the contents of one
%%  container to another, relasing both when finished.
proc(doTransfer(Agt,Source,Dest),
     ensureHas(Agt,Source) // ensureHas(Agt,Dest)
     : transfer(Agt,Source,Dest)
     : release_object(Agt,Source) // release_object(Agt,Dest)
    ).

%%  Make a simple cake mixture in the specified container.
%%  The agents to perform the various steps are selected
%%  nondeterministically.
proc(makeCakeMix(Dest),
     pi(agt,?agent(agt) : doPlaceTypeIn(agt,egg,Dest))
     : pi(agt,?agent(agt) : doPlaceTypeIn(agt,flour,Dest))
     : pi(agt,?agent(agt) : doPlaceTypeIn(agt,sugar,Dest))
     : pi(agt, ?agent(agt) : acquire_object(agt,Dest)
                           : begin_task(agt,mix(Dest,5))
%                           : end_task(agt,mix(Dest,5))
                           : release_object(agt,Dest))
    ).

%%  Make a cake in the specified container.  This involves
%%  making cake mix in the container, then baking it in an oven.
proc(makeCake(Dest),
     makeCakeMix(Dest)
     : pi(myOven, ?obj_is_type(myOven,oven)
                  : pi(agt, ensureHas(agt,myOven)
                            : ensureHas(agt,Dest)
                            : place_in(agt,Dest,myOven)
                            : set_timer(agt,cakeTimer,35)
                      )
                  : ring_timer(cakeTimer)
                  : pi(agt,pi(myBoard, ?obj_is_type(myBoard,board)
                                       : doTransfer(agt,myOven,myBoard)
                      ))
         )
    ).


%%  Chop the given item then place it in the given container.
%%  Releases control of the container when done.  An empty chopping
%%  board is selected nondeterministically.
proc(doChopInto(Agt,Obj,Dest),
     ensureHas(Agt,Obj)
     : pi(myBoard, ?and(obj_is_type(myBoard,board),neg(contents(myBoard,_,now)))
                   : ensureHas(Agt,myBoard)
                   : place_in(Agt,Obj,myBoard)
                   : begin_task(Agt,chop(myBoard))
%                   : end_task(Agt,chop(myBoard))
                   : ensureHas(Agt,Dest)
                   : transfer(Agt,myBoard,Dest)
                   : release_object(Agt,myBoard) // release_object(Agt,Dest)
         )
    ).


%%  Make a salad in the specified container.  This involves selecting
%%  appropriate vegetables, chopping them, placing them in the container,
%%  and mixing briefly.
proc(makeSalad(Dest),
       pi(agt,pi(obj, ?obj_is_type(obj,lettuce)
                      : acquire_object(agt,obj)
                      : doChopInto(agt,obj,Dest)
                )
         )
       //
       pi(agt,pi(obj, ?obj_is_type(obj,tomato)
                      : acquire_object(agt,obj)
                      : doChopInto(agt,obj,Dest)
                )
         )
      //
      pi(agt,pi(obj, ?obj_is_type(obj,carrot)
                      : acquire_object(agt,obj)
                      : doChopInto(agt,obj,Dest)
                )
        )
    : pi(agt, ensureHas(agt,Dest)
              : begin_task(agt,mix(Dest,1))
%              : end_task(agt,mix(Dest,1))
              : release_object(agt,Dest)
        )
    ).


%%  Main control program - prepare a nice meal
proc(main,
     makeSalad(bowl1)
    ).

%%  Tests the operation of the LNTP condition
proc(timerTest,
     set_timer(thomas,timer1,5)
     : set_timer(richard,timer2,7)
     : ring_timer(timer2)
    ).

%%  Tests the operation of concurrency with nondeterminism
proc(concTest,
     doPlaceTypeIn(thomas,egg,bowl1) // doPlaceTypeIn(richard,egg,bowl2)
    ).

%%  Test the operation of nondeterministic argument selection
proc(piTest,
     acquire_object(thomas,egg1)
     : pi(obj, ?and(obj_is_type(obj,egg),neg(has_object(_,obj,now)))
               : acquire_object(richard,board1)
               : acquire_object(richard,obj)
         )
    ).

%%  A simple little program for testing purposes
proc(simple(Agt),
     pi(obj, ?and(obj_is_type(obj,lettuce),not(used(obj,now)))
             : acquire_object(Agt,obj))).

