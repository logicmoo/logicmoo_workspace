%%
%%  domain.pl:  Axiomatisation of the "Cooking Agents" domain for MIndiGolog
%%
%%  Author:  Ryan Kelly (rfk)
%%
%%  Date Created:  28/07/05
%%
%%    This file contains an axiomatisation of the "Cooking Agents" domain
%%    in the Concurrent, Temporal Situation Calculus with Natural Actions.
%%
%%    The domain consists of several agents and inanimate objects of
%%    different types (indicated by prim_obj/2) which in turn may
%%    be part of super-types (indicated by super_type/2).
%%
%%    Agents may acquire objects, place them inside/on container objects,
%%    and transfer the contents of one container object to another.  There
%%    are also an unlimited supply of timers in the world which may be
%%    set to ring at a specified time in the future.
%%
%%    The agents may also perform several continuous tasks, which have
%%    durations and may span several situations.  Agents may only perform
%%    one task at a time.
%%
%%    Depending on the tasks being performed, the contents of container
%%    objects may evolve from one situation to another.  For example,
%%    if the task mix(bowl1,5) is being performed (mix the contents of
%%    bowl1 for five minutees) then the contents might evolve from
%%    [egg1,flour1,sugar1] to mixed([egg1,flour1,sugar1],5).
%%


%%  
%%  agent(Agt):  specify agents in the system
%%
%%  This predicate is true when Agt is the name of an agent in the world.
%%
agent(thomas).
agent(richard).
agent(harriet).


%% 
%%  task(T):  specify the tasks that can be performed
%%
%%  This predicate is true when T is a task that the agents in the system
%%  can perform.  Tasks are typically parameterised in terms of the
%%  objects on which they operate.
%%

%%  mix(Cont,Dur):  mix the contents of container Cont for duration Dur
task(mix(Cont,_)) :- 
    obj_is_type(Cont,container).

%%  chop(Cont):  chop the contents of container Cont
task(chop(Cont)) :-
    obj_is_type(Cont,container).

%%
%%  task_duration(Agt,Task,Dur):  specify duration of tasks
%%
%%  This predicate is true when Dur is the time taken by agent Agt
%%  to perform the task Task.
%%
task_duration(_,mix(_,D),D).
task_duration(Agt,chop(_),D) :-
    (Agt = richard ->
        D = 5
    ;
        D = 3
    ).


%%
%%  prim_obj(Obj,Type):  specify primitive objects in the world
%%
%%  This predicate is true when Obj is the name of a primitive object
%%  in the world, of type Type.
%%
%%  prim_obj(Obj):  shortcut to check object names
%%
%%  This predicate is true if Obj is the name of a primite object,
%%  regardless of its type.
%%

prim_obj(Obj) :-
    prim_obj(Obj,_).

prim_obj(Obj,knife) :-
    member(Obj,[knife1,knife2,knife3]).
prim_obj(Obj,bowl) :-
    member(Obj,[bowl1,bowl2,bowl3]).
prim_obj(Obj,board) :-
    member(Obj,[board1,board2]).
prim_obj(Obj,oven) :-
    member(Obj,[oven1]).
prim_obj(Obj,flour) :-
    member(Obj,[flour1,flour2,flour3,flour4,flour5]).
prim_obj(Obj,sugar) :-
    member(Obj,[sugar1,sugar2,sugar3,sugar4,sugar5,sugar6]).
prim_obj(Obj,egg) :-
    member(Obj,[egg1,egg2,egg3]).
prim_obj(Obj,tomato) :-
    member(Obj,[tomato1,tomato2]).
prim_obj(Obj,lettuce) :-
    member(Obj,[lettuce1,lettuce2,lettuce3]).
prim_obj(Obj,carrot) :-
    member(Obj,[carrot1,carrot2,carrot3]).


%%
%%  super_type(SubType,SuperType):  specify type hierarchy
%%
%%  This predicate is true when all objects of type SubType are
%%  also of type SuperType.
%%  
super_type(Type,cooking_appliance) :-
    member(Type,[oven]).
super_type(Type,container) :-
    member(Type,[bowl,board,cooking_appliance]).
super_type(Type,ingredient) :-
    member(Type,[flour,egg,tomato,lettuce,sugar]).

%%
%%  obj_is_type(Obj,Type):  check object types
%%
%%  This predicate is true when the object named Obj is of type
%%  Type according to the hierarchy of super-types.
%%
obj_is_type(Obj,Type) :-
    prim_obj(Obj,Type)
    ;
    super_type(SubType,Type), obj_is_type(Obj,SubType).


%%
%%  prim_action(Act):  specify primitive actions
%%
%%  This predicate is true when Act is the name of a primitive action
%%  in the world.  Actions are typically parameterised in terms of the
%%  objects they act on.  See the details of the MIndiGolog situation
%%  calculus for further information.
%%

%%  acquire_object(Agt,Obj):  agent acquires control of an object
prim_action(acquire_object(Agt,Obj)) :-
    agent(Agt), prim_obj(Obj).

%%  release_object(Agt,Obj):  agent releases an object it has acquired
prim_action(release_object(Agt,Obj)) :-
    agent(Agt), prim_obj(Obj).

%%  set_timer(Agt,ID,Dur):  agent sets timer with name ID to ring Dur
%%  minutes in the future
prim_action(set_timer(Agt,_,_)) :-
    agent(Agt).

%%  ring_timer(ID):  timer with name ID rings, a natural action
prim_action(ring_timer(_)).
natural(ring_timer(_)).

%%  place_in(Agt,Conts,Dest):  agent places object Conts in container Dest
prim_action(place_in(Agt,Conts,Dest)) :-
    agent(Agt), prim_obj(Conts), obj_is_type(Dest,container).

%%  transfer(Agt,Source,Dest):  agent transfers contents of container Source
%%  to container Dest
prim_action(transfer(Agt,Source,Dest)) :-
    agent(Agt), obj_is_type(Source,container),
    obj_is_type(Dest,container).

%%  begin_task(Agt,Task):  agent starts performing task Task
prim_action(begin_task(Agt,Task)) :-
    agent(Agt), task(Task).

%%  end_task(Agt,Task):  agent finishes performing task, a natural action
prim_action(end_task(Agt,Task)) :-
    agent(Agt), task(Task).
natural(end_task(_,_)).


%%
%%  poss(A,T,S):  possibility of performing an action
%%
%%  This predicate is true when it is possible to perform action
%%  A at time T in situation S.
%%

%%  Agents can only acquire an object if no agent already has it,
%%  they arent doing a task, and the object hasnt been used.
poss(acquire_object(Agt,Obj),_,S) :-
    \+ has_object(_,Obj,S), \+ doing_task(Agt,_,_,S), \+ used(Obj,S).

%%  Agents may only release objects that they have, when they arent
%%  currently performing a task
poss(release_object(Agt,Obj),_,S) :-
    has_object(Agt,Obj,S), \+ doing_task(Agt,_,_,S).

%%  Agents may set a timer as long as it hasnt already been set, and
%%  they arent currently performing a task
poss(set_timer(Agt,ID,_),_,S) :-
    \+ timer_set(ID,_,S), \+ doing_task(Agt,_,_,S).

%%  It is only possible for a timer to ring once its remaining time
%%  has precisely elapsed
poss(ring_timer(ID),T,S) :-
    timer_set(ID,Dur,S),
    start(S,SStart), {T = SStart + Dur}.

%%  Agents may place an object in a container provided they have possession
%%  of both, and arent currently doing a task
poss(place_in(Agt,Conts,Dest),_,S) :-
    has_object(Agt,Conts,S), has_object(Agt,Dest,S),
    \+ obj_is_type(Conts,cooking_appliance),
    \+ doing_task(Agt,_,_,S).

%%  Agents may transfer contents from one container to another as long
%%  as they have possession of both, and arent doing a task
poss(transfer(Agt,Source,Dest),_,S) :-
    has_object(Agt,Source,S), has_object(Agt,Dest,S),
    \+ doing_task(Agt,_,_,S).

%%  Agents may begin the mix() task as long as they arent doing another
%%  task, and have possession of the container to be mixed in
poss(begin_task(Agt,mix(Obj,_)),_,S) :-
    has_object(Agt,Obj,S), \+ doing_task(Agt,_,_,S).

%%  Agents may begin the chop() task as long as they arent doing another
%%  task, and have possession of the container whose contents to chop
poss(begin_task(Agt,chop(Obj)),_,S) :-
    has_object(Agt,Obj,S), \+ doing_task(Agt,_,_,S).

%%  Agents may end a task only when precisely the remaining amount of
%%  time has elapsed
poss(end_task(Agt,Task),T,S) :-
    doing_task(Agt,Task,Remain,S),
    start(S,SStart), {T = SStart + Remain}.


%%
%%  conflicts(C,T,S):  concurrent actions conflict
%%
%%  This predicate must be true when concurrent actions in C conflict
%%  and cannot be performed at time T in situation S.
%%

%%  Agents cannot do more than one action at a time
conflicts(C,_,_) :-
    member(A1,C), actor(A1,Agt),
    member(A2,C), actor(A2,Agt),
    A2 \= A1.

%%  Two agents cannot acquire the same object
conflicts(C,_,_) :-
    member(acquire_object(A1,Res),C),
    member(acquire_object(A2,Res),C),
    A1 \= A2.
    
%%
%%  Fluents in the Domain
%%
%%  The fluents are specified in terms of their successor state axioms,
%%  of the form "a fluent is true if it became true, or was previously
%%  true did not become false".
%%
%%    fluent_holds(Args,do(A,T,S)) :-
%%        fluent_becomes_true(Args,do(A,T,S))
%%        ;
%%        (
%%          fluent_holds(Args,S),
%%          \+ fluent_becomes_false(Args,do(A,T,S))
%%        )
%%

%%
%%  has_object(Agt,Obj,S):  agent has an object
%%
%%  This fluent is true when agent Agt has possession of the object Obj
%%  in situation S.  It can become true by acquiring the object, and
%%  false by releasing the object or if it has become used.
%%
has_object(Agt,Obj,do(C,T,S)) :-
    member(acquire_object(Agt,Obj),C)
    ;
    has_object(Agt,Obj,S),
    \+ (
       member(release_object(Agt,Obj),C)
       ;
       used(Obj,do(C,T,S))
    ).

%%
%%  used(Obj,S):  object is used in situation S
%%
%%  This fluent is true when an object has been used - for example,
%%  an ingredient has been placed in a container.  Once an object has
%%  been used, it cannot be used again.
%%
used(Obj,do(C,_,S)) :-
    prim_obj(Obj), obj_is_type(Obj,ingredient),
    (
      used(Obj,S)
      ;
      member(place_in(_,Obj,_),C)
    ).

%%
%%  timer_set(ID,Dur,S):  timer is set in situation S
%%
%%  This fluent indicates that the timer named ID is set to ring in Dur
%%  minutes in situation S.  It becomes true as a result of a set_timer()
%%  action, and is updated from situation to situation to reflect
%%  the amount of time remaining.
%%
timer_set(ID,Dur,do(C,T,S)) :-
    member(set_timer(_,ID,Dur),C)
    ;
    timer_set(ID,OldDur,S), start(S,SStart), {Dur = OldDur-(T-SStart)},
    \+ member(ring_timer(ID),C).

%%
%%  contents(Obj,Conts,S):  object contents in a situation
%%
%%  This fluent indicates that object Obj contains the contents Conts
%%  in situation S.  It can become true, become false, and change value
%%  in a variety of ways, each of which is documented with its
%%  implementation.
%%
contents(Obj,Conts,do(C,T,S)) :-
    start(S,SStart),
    ((
      %% --- All the ways it can become true
      %% It was previously empty, and contents were placed or transfered in
      (member(place_in(_,Conts,Obj),C)
         ; member(transfer(_,Obj2,Obj),C), contents(Obj2,Conts,S)),
      \+ contents(Obj,_,S)
      ;
      %% It previously had contents, and more contents were placed or
      %% transfered in.  Contents is then a list.
      (member(place_in(_,NewConts,Obj),C)
         ; member(transfer(_,Obj2,Obj),C), contents(Obj2,NewConts,S)),
      contents(Obj,OldConts,S),
      ( OldConts = [_|_] -> OldContsL = OldConts ; OldContsL = [OldConts]),
      ( NewConts = [_|_] -> NewContsL = NewConts ; NewContsL = [NewConts]),
      union(OldContsL,NewContsL,Conts)
      ;
      %% An agent is mixing the contents.  If they were previously
      %% unmixed, they are encased in a mixed(conts,time) indicator.
      %% If they were previously mixed, the mixing time is increased.
      doing_task(_,mix(Obj,_),_,do(C,T,S)), contents(Obj,OldConts,S),
      (  OldConts = mixed(MixConts,OldP) ->
             {NewP = OldP+(T-SStart)}, Conts = mixed(MixConts,NewP)
         ;
             Conts = mixed(OldConts,0)
      )
      ;
      %% An agent just completed mixing the contents.  The mixing time
      %% must be modified to take its final value.
      member(end_task(_,mix(Obj,_)),C), contents(Obj,mixed(MixConts,OldP),S),
      {NewP = OldP+(T-SStart)}, Conts = mixed(MixConts,NewP)
      ;
      %% An agent just completed chopping the contents.  They are
      %% encased in a chopped() indicator.
      member(end_task(_,chop(Obj)),C), contents(Obj,OldConts,S),
      Conts = chopped(OldConts)
      ;
      %% If the container is in an oven, its contents are baked.
      %% If they are not encapsulated in a baked() indicator then do
      %% so, otherwise update the baking time.
      \+ obj_is_type(Obj,cooking_appliance), obj_is_type(Oven,oven),
      contents(Oven,Obj,do(C,T,S)), contents(Obj,OldConts,S),
      (  OldConts = baked(BakedConts,OldP) ->
             {NewP = OldP+(T-SStart)}, Conts = baked(BakedConts,NewP)
         ;
             Conts = baked(OldConts,0)
      )
      ;
      %% If the container was just taken out of the oven, updated
      %% the content to reflect the total baking time.
      \+ obj_is_type(Obj,cooking_appliance), obj_is_type(Oven,oven),
      contents(Oven,Obj,S), member(transfer(_,Oven,_),C),
      contents(Obj,baked(BakedConts,OldP),S),
      {NewP = OldP+(T-SStart)}, Conts = baked(BakedConts,NewP)
    )
    ;
    %% Or it was true, and didnt become false...
    contents(Obj,Conts,S), \+ (
        %% --- All the ways it can become false
        %% The contents were transfered out
        member(transfer(_,Obj,_),C)
        ;
        %% New contents were transfered in
        member(transfer(_,Obj2,Obj),C), contents(Obj2,_,S)
        ;
        %% New contents were placed in
        member(place_in(_,_,Obj),C)
        ;
        %% The contents are being mixed, hence will change
        doing_task(_,mix(Obj,_),_,do(C,T,S))
        ;
        %% The contents just finished being mixed, hence will change
        member(end_task(_,mix(Obj,_)),C)
        ;
        %% The contents just finished being chopped, hence will change
        member(end_task(_,chop(Obj)),C)
        ;
        %% The object is in an oven, hence will change
        \+ obj_is_type(Obj,cooking_appliance), obj_is_type(Oven,oven),
        contents(Oven,Obj,do(C,T,S))
        ;
        %% The object was just taken out of an oven, hence will change
        \+ obj_is_type(Obj,cooking_appliance), obj_is_type(Oven,oven),
        contents(Oven,Obj,S), member(transfer(_,Oven,_),C)
    )).


%%
%%  doing_task(Agt,Task,Remain,S):  agent is performing a task
%%
%%  This fluent is true when agent Agt is performing task Task in situation
%%  S, and has Remain minutes left before it is completed.  It becomes
%%  true when an agent begins a task, becomes false when an agent
%%  completes a task, and its value is updated from situation to situation
%%  to reflect the remianing time till completion.
%%
doing_task(Agt,Task,Remain,do(C,T,S)) :-
    member(begin_task(Agt,Task),C), task_duration(Agt,Task,Remain)
    ;
    doing_task(Agt,Task,OldRem,S), start(S,SStart),
    {OldRem = Remain-(T-SStart)}, \+ member(end_task(Agt,Task),C).
    

%%
%%  history_length(N,S):  length of the action histoy in a situation
%%
%%  This simple fluent encodes in N the number of actions that have
%%  taken place in the history of situation S.  It is used to make this
%%  information easily available to agents.
%%
history_length(N,do(_,_,S)) :-
    history_length(N1,S),
    N is N1 + 1.
history_length(0,s0).

%%
%%  Intial Conditions for the domain
%%
%%  The initial conditions are specified by additional clauses for
%%  each fluent, with the situation term set to the atom s0.  For
%%  the most part no fluents hold in the initial situation, so 
%%  there arent many clauses here.
%%
start(s0,0).

%%
%%  MIndiGolog procedures
%%
%%  The following are a collection of useful procedures in the domain,
%%  from which larger programs can be built.
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
proc(control,
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

