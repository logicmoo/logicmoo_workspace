

% ---:- include('domain_cake.pl').

% ---
%  domain.pl:  Axiomatisation of the "Cooking Agents" domain for MIndiGolog
% ---
%    This file contains an axiomatisation of the "Cooking Agents" domain
%    in the Concurrent, Temporal Situation Calculus with Natural Actions.
% ---
%    The domain consists of several agents and inanimate objects of
%    different types (indicated by prim_obj/2) which in turn may
%    be part of super-types (indicated by super_type/2).
% ---
%    Agents may acquire objects, place them inside/on container objects,
%    and transfer the contents of one container object to another.  There
%    are also an unlimited supply of timers in the world which may be
%    set to ring at a specified time in the future.
% ---
%    The agents may also perform several continuous tasks, which have
%    durations and may span several situations.  Agents may only perform
%    one task at a time.
% ---
%    Depending on the tasks being performed, the contents of container
%    objects may evolve from one situation to another.  For example,
%    if the task actT(mix,Agt,bowl1,5) is being performed (mix the contents of
%    bowl1 for five minutees) then the contents might evolve from
%    [egg1,flour1,sugar1] to mixed([egg1,flour1,sugar1],5).
% ---


%  
%  agent(Agt):  specify agents in the system
% ---
%  This predicate is true when Agt is the name of an agent in the world.
% ---
agent(joe).
agent(richard).
agent(harriet).


% 
%  task(T):  specify the tasks that can be performed
% ---
%  This predicate is true when T is a task that the agents in the system
%  can perform.  Tasks are typically parameterised in terms of the
%  objects on which they operate.
% ---

%  actT(mix,Agt,Cont,Dur):  mix the contents of container Cont for duration Dur
task(actT(mix,_,Cont,_)) :- 
    obj_is_type(Cont,container).

%  actT(chop,Agt,Cont):  chop the contents of container Cont
task(actT(chop,_,Cont)) :-
    obj_is_type(Cont,container).

% ---
%  task_duration(Agt,Task,Dur):  specify duration of tasks
% ---
%  This predicate is true when Dur is the time taken by agent Agt
%  to perform the task Task.
% ---
task_duration(_,actT(mix,_,_,D),D).
task_duration(Agt,actT(chop,Agt,_),D) :-
    (Agt = richard ->
        D = 5
    ;
        D = 3
    ).


% ---
%  prim_obj1(Obj):  shortcut to check object names
% ---
%  This predicate is true if Obj is the name of a primite object,
%  regardless of its type.
% ---

prim_obj1(Obj) :-
    prim_obj(Obj,_).

% ---
%  prim_obj(Obj,Type):  specify primitive objects in the world
% ---
%  This predicate is true when Obj is the name of a primitive object
%  in the world, of type Type.

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


% ---
%  super_type(SubType,SuperType):  specify type hierarchy
% ---
%  This predicate is true when all objects of type SubType are
%  also of type SuperType.
%  
super_type(Type,cooking_appliance) :-
    member(Type,[oven]).
super_type(Type,container) :-
    member(Type,[bowl,board,cooking_appliance]).
super_type(Type,ingredient) :-
    member(Type,[flour,egg,tomato,lettuce,sugar]).
super_type(Type,makable) :-
    member(Type,[salad,cake]).


type_is_type(Type,Super):- super_type(Type,Super).
type_is_type(Type,Type).

% ---
%  obj_is_type(Obj,Type):  check object types
% ---
%  This predicate is true when the object named Obj is of type
%  Type according to the hierarchy of super-types.
% ---
obj_is_type(Obj,Type) :-
    prim_obj(Obj,Type)
    ;
    super_type(SubType,Type), obj_is_type(Obj,SubType).


% ---
%  prim_action(Act):  specify primitive actions
% ---
%  This predicate is true when Act is the name of a primitive action
%  in the world.  Actions are typically parameterised in terms of the
%  objects they act on.  See the details of the MIndiGolog situation
%  calculus for further information.
% ---

%  actT(acquire_object,Agt,Obj):  agent acquires control of an object
prim_action(actT(acquire_object,Agt,Obj)) :-
    agent(Agt), prim_obj1(Obj).

%  actT(release_object,Agt,Obj):  agent releases an object it has acquired
prim_action(actT(release_object,Agt,Obj)) :-
    agent(Agt), prim_obj1(Obj).

%  actT(set_timer,Agt,ID,Dur):  agent sets timer with name ID to ring Dur
%  minutes in the future
prim_action(actT(set_timer,Agt,_,_)) :-
    agent(Agt).

%  event_ring_timer(ID):  timer with name ID rings, a natural action
prim_action(event_ring_timer(_)).
natural(event_ring_timer(_)).

%  actT(place_in,Agt,Conts,Dest):  agent places object Conts in container Dest
prim_action(actT(place_in,Agt,Conts,Dest)) :-
    agent(Agt), prim_obj1(Conts), obj_is_type(Dest,container).

%  actT(transfer_now,Agt,Source,Dest):  agent transfers contents of container Source
%  to container Dest
prim_action(actT(transfer_now,Agt,Source,Dest)) :-
    agent(Agt), obj_is_type(Source,container),
    obj_is_type(Dest,container).

%  actT(begin_task,Agt,Task):  agent starts performing task Task
prim_action(actT(begin_task,Agt,Task)) :-
    agent(Agt), task(Task).

%  event_end_task(Agt,Task):  agent finishes performing task, a natural action
prim_action(event_end_task(Agt,Task)) :-
    agent(Agt), task(Task).
natural(event_end_task(_,_)).


% ---
%  poss(A,T,S):  possibility of performing an action
% ---
%  This predicate is true when it is possible to perform action
%  A at time T in situation S.
% ---

%  Agents can only acquire an object if no agent already has it,
%  they arent doing a task, and the object hasnt been used.
poss(actT(acquire_object,Agt,Obj),_,S) :-
    \+ state_agent_has_object(_,Obj,S), \+ doing_task(Agt,_,_,S), \+ used(Obj,S).

%  Agents may only release objects that they have, when they arent
%  currently performing a task
poss(actT(release_object,Agt,Obj),_,S) :-
    state_agent_has_object(Agt,Obj,S), \+ doing_task(Agt,_,_,S).

%  Agents may set a timer as long as it hasnt already been set, and
%  they arent currently performing a task
poss(actT(set_timer,Agt,ID,_),_,S) :-
    \+ timer_set(ID,_,S), \+ doing_task(Agt,_,_,S).

%  It is only possible for a timer to ring once its remaining time
%  has precisely elapsed
poss(event_ring_timer(ID),T,S) :-
    timer_set(ID,Dur,S),
    start(S,SStart), {T = SStart + Dur}.

%  Agents may place an object in a container provided they have possession
%  of both, and arent currently doing a task
poss(actT(place_in,Agt,Conts,Dest),_,S) :-
    state_agent_has_object(Agt,Conts,S), state_agent_has_object(Agt,Dest,S),
    \+ obj_is_type(Conts,cooking_appliance),
    \+ doing_task(Agt,_,_,S).

%  Agents may transfer contents from one container to another as long
%  as they have possession of both, and arent doing a task
poss(actT(transfer_now,Agt,Source,Dest),_,S) :-
    state_agent_has_object(Agt,Source,S), state_agent_has_object(Agt,Dest,S),
    \+ doing_task(Agt,_,_,S).

%  Agents may begin the actT(mix,Agt,...) task as long as they arent doing another
%  task, and have possession of the container to be mixed in
poss(actT(begin_task,Agt,actT(mix,Agt,Obj,_)),_,S) :-
    state_agent_has_object(Agt,Obj,S), \+ doing_task(Agt,_,_,S).

%  Agents may begin the actT(chop,Agt,...) task as long as they arent doing another
%  task, and have possession of the container whose contents to chop
poss(actT(begin_task,Agt,actT(chop,Agt,Obj)),_,S) :-
    state_agent_has_object(Agt,Obj,S), \+ doing_task(Agt,_,_,S).

%  Agents may end a task only when precisely the remaining amount of
%  time has elapsed
poss(event_end_task(Agt,Task),T,S) :-
    doing_task(Agt,Task,Remain,S),
    start(S,SStart), {T = SStart + Remain}.


happens(E,L):- member(E,L).

% ---
%  conflicts(C,T,S):  concurrent actions conflict
% ---
%  This predicate must be true when concurrent actions in C conflict
%  and cannot be performed at time T in situation S.
% ---

%  Agents cannot do more than one action at a time
conflicts(C,_,_) :-
    happens(A1,C), actor(A1,Agt),
    happens(A2,C), actor(A2,Agt),
    A2 \= A1.

%  Two agents cannot acquire the same object
conflicts(C,_,_) :-
    happens(actT(acquire_object,A1,Res),C),
    happens(actT(acquire_object,A2,Res),C),
    A1 \= A2.
    
% ---
%  Fluents in the Domain
% ---
%  The fluents are specified in terms of their successor state axioms,
%  of the form "a fluent is true if it became true, or was previously
%  true did not become false".
% ---
%    fluent_holds(Args,do(A,T,S)) :-
%        fluent_becomes_true(Args,do(A,T,S))
%        ;
%        (
%          fluent_holds(Args,S),
%          \+ fluent_becomes_false(Args,do(A,T,S))
%        )
% ---

% ---
%  state_agent_has_object(Agt,Obj,S):  agent has an object
% ---
%  This fluent is true when agent Agt has possession of the object Obj
%  in situation S.  It can become true by acquiring the object, and
%  false by releasing the object or if it has become used.
% ---
state_agent_has_object(Agt,Obj,do(C,T,S)) :-
    happens(actT(acquire_object,Agt,Obj),C)
    ;
    state_agent_has_object(Agt,Obj,S),
    \+ (
       happens(actT(release_object,Agt,Obj),C)
       ;
       used(Obj,do(C,T,S))
    ).

% ---
%  used(Obj,S):  object is used in situation S
% ---
%  This fluent is true when an object has been used - for example,
%  an ingredient has been placed in a container.  Once an object has
%  been used, it cannot be used again.
% ---
used(Obj,do(C,_,S)) :-
    prim_obj1(Obj), obj_is_type(Obj,ingredient),
    (
      used(Obj,S)
      ;
      happens(actT(place_in,_,Obj,_),C)
    ).

% ---
%  timer_set(ID,Dur,S):  timer is set in situation S
% ---
%  This fluent indicates that the timer named ID is set to ring in Dur
%  minutes in situation S.  It becomes true as a result of a actT(set_timer,)
%  action, and is updated from situation to situation to reflect
%  the amount of time remaining.
% ---
timer_set(ID,Dur,do(C,T,S)) :-
    happens(actT(set_timer,_,ID,Dur),C)
    ;
    timer_set(ID,OldDur,S), start(S,SStart), {Dur = OldDur-(T-SStart)},
    \+ happens(event_ring_timer(ID),C).

% ---
%  contents(Obj,Conts,S):  object contents in a situation
% ---
%  This fluent indicates that object Obj contains the contents Conts
%  in situation S.  It can become true, become false, and change value
%  in a variety of ways, each of which is documented with its
%  implementation.
% ---
      % --- All the ways it can become true

%- It was previously empty, and contents were placed or transfered in
contents(Obj,Conts,do(C,_T,S)):- 
      (happens(actT(place_in,_,Conts,Obj),C)
         ; happens(actT(transfer_now,_,Obj2,Obj),C), contents(Obj2,Conts,S)),
      \+ contents(Obj,_,S).


%- It previously had contents, and more contents were placed or
%- transfered in.  Contents is then a list.
contents(Obj,Conts,do(C,_T,S)):- 
      (happens(actT(place_in,_,NewConts,Obj),C)
         ; happens(actT(transfer_now,_,Obj2,Obj),C), contents(Obj2,NewConts,S)),
      contents(Obj,OldConts,S),
      ( OldConts = [_|_] -> OldContsL = OldConts ; OldContsL = [OldConts]),
      ( NewConts = [_|_] -> NewContsL = NewConts ; NewContsL = [NewConts]),
      union(OldContsL,NewContsL,Conts).

%- An agent is mixing the contents.  If they were previously
%- unmixed, they are encased in a mixed(conts,time) indicator.
%- If they were previously mixed, the mixing time is increased.
contents(Obj,Conts,do(C,T,S)):- 
      start(S,SStart),
      doing_task(_,actT(mix,_,Obj,_),_,do(C,T,S)), 
      contents(Obj,OldConts,S),
      (  OldConts = mixed(MixConts,OldP) ->
             {NewP = OldP+(T-SStart)}, Conts = mixed(MixConts,NewP)
         ;
             Conts = mixed(OldConts,0)
      ).


%- An agent just completed mixing the contents.  The mixing time
%- must be modified to take its final value.
contents(Obj,Conts,do(C,T,S)):- start(S,SStart),
      happens(event_end_task(_,actT(mix,_,Obj,_)),C), 
      contents(Obj,mixed(MixConts,OldP),S),
      {NewP = OldP+(T-SStart)}, Conts = mixed(MixConts,NewP).


%- An agent just completed chopping the contents.  They are
%- encased in a chopped() indicator.
contents(Obj,Conts,do(C,_T,S)):- 
      happens(event_end_task(_,actT(chop,_,Obj)),C), 
      contents(Obj,OldConts,S),
      Conts = chopped(OldConts).


%- If the container is in an oven, its contents are baked.
%- If they are not encapsulated in a baked() indicator then do
%- so, otherwise update the baking time.
contents(Obj,Conts,do(C,T,S)):- start(S,SStart),
      \+ obj_is_type(Obj,cooking_appliance), 
      obj_is_type(Oven,oven),
      contents(Oven,Obj,do(C,T,S)), 
      contents(Obj,OldConts,S),
      (  OldConts = baked(BakedConts,OldP) ->
             {NewP = OldP+(T-SStart)}, Conts = baked(BakedConts,NewP)
         ;
             Conts = baked(OldConts,0)
      ).


%- If the container was just taken out of the oven, updated
%- the content to reflect the total baking time.
contents(Obj,Conts,do(C,T,S)):- start(S,SStart),
      \+ obj_is_type(Obj,cooking_appliance), obj_is_type(Oven,oven),
      contents(Oven,Obj,S), happens(actT(transfer_now,_,Oven,_),C),
      contents(Obj,baked(BakedConts,OldP),S),
      {NewP = OldP+(T-SStart)}, Conts = baked(BakedConts,NewP). 

    % Or it was true, and didnt become false...
contents(Obj,Conts,do(C,T,S)):-
    contents(Obj,Conts,S), 
    \+ stateT_becomes_false(contents,Obj,Conts, do(C,T,S)).

%- --- All the ways it can become false

%- The contents were transfered out
stateT_becomes_false(contents,Obj,_,do(C,_,_)) :-  happens(actT(transfer_now,_,Obj,_),C).

%- New contents were transfered in
stateT_becomes_false(contents,Obj,_,do(C,_,S)) :- happens(actT(transfer_now,_,Obj2,Obj),C), contents(Obj2,_,S).        

%- New contents were placed in
stateT_becomes_false(contents,Obj,_,do(C,_,_)) :- happens(actT(place_in,_,_,Obj),C).

%- The contents are being mixed, hence will change
stateT_becomes_false(contents,Obj,_,CTS) :-  doing_task(_,actT(mix,_,Obj,_),_,CTS).

%- The contents just finished being mixed, hence will change
stateT_becomes_false(contents,Obj,_,do(C,_,_)) :- happens(event_end_task(_,actT(mix,_,Obj,_)),C).

%- The contents just finished being chopped, hence will change
stateT_becomes_false(contents,Obj,_,do(C,_,_)) :- happens(event_end_task(_,actT(chop,_,Obj)),C).

%- The object is in an oven, hence will change
stateT_becomes_false(contents,Obj,_Conts,CTS) :- 
  \+ obj_is_type(Obj,cooking_appliance), 
  obj_is_type(Oven,oven), 
  contents(Oven,Obj,CTS).

%- The object was just taken out of an oven, hence will change
stateT_becomes_false(contents,Obj,_,do(C,_,S)) :- 
  \+ obj_is_type(Obj,cooking_appliance), 
  obj_is_type(Oven,oven),
  contents(Oven,Obj,S),
  happens(actT(transfer_now,_,Oven,_),C).


% ---
%  doing_task(Agt,Task,Remain,S):  agent is performing a task
% ---
%  This fluent is true when agent Agt is performing task Task in situation
%  S, and has Remain minutes left before it is completed.  It becomes
%  true when an agent begins a task, becomes false when an agent
%  completes a task, and its value is updated from situation to situation
%  to reflect the remianing time till completion.
% ---
doing_task(Agt,Task,Remain,do(C,T,S)) :-
    happens(actT(begin_task,Agt,Task),C), task_duration(Agt,Task,Remain)
    ;
    doing_task(Agt,Task,OldRem,S), start(S,SStart),
    {OldRem = Remain-(T-SStart)}, \+ happens(event_end_task(Agt,Task),C).
    

% ---
%  history_length(N,S):  length of the action histoy in a situation
% ---
%  This simple fluent encodes in N the number of actions that have
%  taken place in the history of situation S.  It is used to make this
%  information easily available to agents.
% ---
history_length(N,do(_,_,S)) :-
    history_length(N1,S),
    N is N1 + 1.
history_length(0,s0).

% ---
%  Intial Conditions for the domain
% ---
%  The initial conditions are specified by additional clauses for
%  each fluent, with the situation term set to the atom s0.  For
%  the most part no fluents hold in the initial situation, so 
%  there arent many clauses here.
% ---
start(s0,0).

% ---
%  MIndiGolog procedures
% ---
%  The following are a collection of useful procedures in the domain,
%  from which larger programs can be built.
% ---


%  Ensure that the agent has control of an object
proc(ensure_agent_has_object(Agt,Obj),
     if(state_agent_has_object(Agt,Obj,now),nil,actT(acquire_object,Agt,Obj))
    ).

%  Carry out the necessary sequence of actions to place one object
%  inside another, releasing the destination container when done.
proc(doPlaceIn(Agt,Obj,Dest),
     ensure_agent_has_object(Agt,Obj) // ensure_agent_has_object(Agt,Dest)
     : actT(place_in,Agt,Obj,Dest)
     : actT(release_object,Agt,Dest)
    ).

%  Nondeterministically select an object of a given type, gain control
%  of it, and place it inside a container object.
proc(ensure_place_type_in(Agt,Type,Dest),
     exists(obj,?and(obj_is_type(obj,Type),not(used(obj,now)))
            : actT(acquire_object,Agt,obj)
            : doPlaceIn(Agt,obj,Dest))
    ).

%  Carry out the necessary actions to transfer the contents of one
%  container to another, relasing both when finished.
proc(ensure_xfrd(Agt,Source,Dest),
     ensure_agent_has_object(Agt,Source) // ensure_agent_has_object(Agt,Dest)
     : actT(transfer_now,Agt,Source,Dest)
     : actT(release_object,Agt,Source) // actT(release_object,Agt,Dest)
    ).

%  Make a simple cake mixture in the specified container.
%  The agents to perform the various steps are selected
%  nondeterministically.
proc(actT(make,Doer,mixFor(Cake,Doer),Dest),
     exists(agt,?agent(agt) : ensure_place_type_in(agt,egg,Dest))
     : exists(agt,?agent(agt) : ensure_place_type_in(agt,flour,Dest))
     : exists(agt,?agent(agt) : ensure_place_type_in(agt,sugar,Dest))
     : exists(agt, ?agent(agt) : actT(acquire_object,agt,Dest)
                           : actT(begin_task,agt,actT(mix,agt,Dest,5))
%                           : event_end_task(agt,actT(mix,agt,Dest,5))
                           : actT(release_object,agt,Dest))
     ) :- type_is_type(Cake,cake).

%  Make a cake in the specified container.  This involves
%  making cake mix in the container, then baking it in an oven.
proc(actT(make,Doer,Cake,Dest),
exists(agt,actT(make,agt,mixFor(Cake,Doer),Dest))
     : exists(myOven, ?obj_is_type(myOven,oven)
                  : exists(agt, ensure_agent_has_object(agt,myOven)
                            : ensure_agent_has_object(agt,Dest)
                            : actT(place_in,agt,Dest,myOven)
                            : actT(set_timer,agt,timerFor(Cake,Doer),35)
                      )
                  : event_ring_timer(timerFor(Cake,Doer))
                  : exists(agt,exists(myBoard, ?obj_is_type(myBoard,board)
                                       : ensure_xfrd(agt,myOven,myBoard)
                      ))
         )
     ) :- type_is_type(Cake,cake).



%  Chop the given item then place it in the given container.
%  Releases control of the container when done.  An empty chopping
%  board is selected nondeterministically.
proc(ensure_chop_into(Agt,Obj,Dest),
     ensure_agent_has_object(Agt,Obj)
     : exists(myBoard, ?and(obj_is_type(myBoard,board),neg(contents(myBoard,_,now)))
                   : ensure_agent_has_object(Agt,myBoard)
                   : actT(place_in,Agt,Obj,myBoard)
                   : actT(begin_task,Agt,actT(chop,Agt,myBoard))
%                   : event_end_task(Agt,actT(chop,Agt,myBoard))
                   : ensure_agent_has_object(Agt,Dest)
                   : actT(transfer_now,Agt,myBoard,Dest)
                   : actT(release_object,Agt,myBoard) // actT(release_object,Agt,Dest)
         )
    ).


%  Make a salad in the specified container.  This involves selecting
%  appropriate vegetables, chopping them, placing them in the container,
%  and mixing briefly.
proc(actT(make,_Agt,salad,Dest),
       exists(agt,exists(obj, ?obj_is_type(obj,lettuce)
                      : actT(acquire_object,agt,obj)
                      : ensure_chop_into(agt,obj,Dest)
                )
         )
       >>
       exists(agt,exists(obj, ?obj_is_type(obj,tomato)
                      : actT(acquire_object,agt,obj)
                      : ensure_chop_into(agt,obj,Dest)
                )
         )
      //
      exists(agt,exists(obj, ?obj_is_type(obj,carrot)
                      : actT(acquire_object,agt,obj)
                      : ensure_chop_into(agt,obj,Dest)
                )
        )
    : exists(agt, ensure_agent_has_object(agt,Dest)
              : actT(begin_task,agt,actT(mix,agt,Dest,1))
%              : event_end_task(agt,actT(mix,agt,Dest,1))
              : actT(release_object,agt,Dest)
        )
    ).


%  Main control program - prepare a nice meal
proc(control,
     actT(make,joe,salad,bowl1)
    ).

proc(make_cake,
     actT(make,joe,cake,board1)
    ).

%  Tests the operation of the LNTP condition
proc(timerTest,
     actT(set_timer,joe,timer1,5)
     : actT(set_timer,richard,timer2,7)
     : event_ring_timer(timer2)
    ).

%  Tests the operation of concurrency with nondeterminism
proc(concTest,
     ensure_place_type_in(joe,egg,bowl1) // ensure_place_type_in(richard,egg,bowl2)
    ).

%  Test the operation of nondeterministic argument selection
proc(piTest,
     actT(acquire_object,joe,egg1)
     : exists(obj, ?and(obj_is_type(obj,egg),neg(state_agent_has_object(_,obj,now)))
               : actT(acquire_object,richard,board1)
               : actT(acquire_object,richard,obj)
         )
    ).

%  A simple little program for testing purposes
proc(simple(Agt),
     exists(obj, ?and(obj_is_type(obj,lettuce),not(used(obj,now)))
             : actT(acquire_object,Agt,obj))).


proc(control_fails,
     actT(make,_Harriet,salad,bowl1)
    ).


testsit_sanity:- ol_do(control,s0).
testsit_all:- forall((proc(Test,Stuff), ground(Test), writeln(-), 
  wdmsg(proc(Test,Stuff))),
  (ol_do(Test,s0),writeln(-))).

:- fixup_exports.


/*

baseKB: ?-testsit_all.

-
% proc(control,actT(make,joe,salad,bowl1)).
do [actT(acquire_object,joe,lettuce1)] at 1
do [actT(acquire_object,joe,board1)] at 2
do [actT(place_in,joe,lettuce1,board1)] at 3
do [actT(begin_task,joe,actT(chop,joe,board1))] at 4
do [event_end_task(joe,actT(chop,joe,board1))] at 7
do [actT(acquire_object,joe,bowl1)] at 8
do [actT(transfer_now,joe,board1,bowl1)] at 9
do [actT(release_object,joe,board1)] at 10
do [actT(release_object,joe,bowl1)] at 11
do [actT(acquire_object,joe,tomato1),actT(acquire_object,richard,carrot1)] at 12
do [actT(acquire_object,joe,board1),actT(acquire_object,richard,board2)] at 13
do [actT(place_in,joe,tomato1,board1),actT(place_in,richard,carrot1,board2)] at 14
do [actT(begin_task,joe,actT(chop,joe,board1)),actT(begin_task,richard,actT(chop,richard,board2))] at 15
do [event_end_task(joe,actT(chop,joe,board1))] at 18
do [actT(acquire_object,joe,bowl1),event_end_task(richard,actT(chop,richard,board2))] at 26
do [actT(transfer_now,joe,board1,bowl1)] at 27
do [actT(release_object,joe,board1)] at 28
do [actT(release_object,joe,bowl1)] at 29
do [actT(acquire_object,richard,bowl1)] at 30
do [actT(transfer_now,richard,board2,bowl1)] at 31
do [actT(release_object,richard,board2)] at 32
do [actT(release_object,richard,bowl1)] at 33
do [actT(acquire_object,joe,bowl1)] at 34
do [actT(begin_task,joe,actT(mix,joe,bowl1,1))] at 35
do [event_end_task(joe,actT(mix,joe,bowl1,1))] at 36
do [actT(release_object,joe,bowl1)] at 37
SUCCEEDED!-
-
% proc(make_cake,actT(make,joe,cake,board1)).
do [actT(acquire_object,joe,egg1)] at 1
do [actT(acquire_object,joe,board1)] at 2
do [actT(place_in,joe,egg1,board1)] at 3
do [actT(release_object,joe,board1)] at 4
do [actT(acquire_object,joe,flour1)] at 5
do [actT(acquire_object,joe,board1)] at 6
do [actT(place_in,joe,flour1,board1)] at 7
do [actT(release_object,joe,board1)] at 8
do [actT(acquire_object,joe,sugar1)] at 9
do [actT(acquire_object,joe,board1)] at 10
do [actT(place_in,joe,sugar1,board1)] at 11
do [actT(release_object,joe,board1)] at 12
do [actT(acquire_object,joe,board1)] at 13
do [actT(begin_task,joe,actT(mix,joe,board1,5))] at 14
do [event_end_task(joe,actT(mix,joe,board1,5))] at 19
do [actT(release_object,joe,board1)] at 20
do [actT(acquire_object,joe,oven1)] at 21
do [actT(acquire_object,joe,board1)] at 22
do [actT(place_in,joe,board1,oven1)] at 23
do [actT(set_timer,joe,timerFor(cake,joe),35)] at 24
do [event_ring_timer(timerFor(cake,joe))] at 59
do [actT(transfer_now,joe,oven1,board1)] at 60
do [actT(release_object,joe,oven1)] at 61
do [actT(release_object,joe,board1)] at 62
SUCCEEDED!-
-
% proc(timerTest,
%      actT(set_timer,joe,timer1,5):actT(set_timer,richard,timer2,7):event_ring_timer(timer2)).
do [actT(set_timer,joe,timer1,5)] at 1
do [actT(set_timer,richard,timer2,7)] at 2
do [event_ring_timer(timer1)] at 6
do [event_ring_timer(timer2)] at 9
SUCCEEDED!-
-
% proc(concTest,
%      ensure_place_type_in(joe,egg,bowl1)//ensure_place_type_in(richard,egg,bowl2)).
do [actT(acquire_object,joe,egg1),actT(acquire_object,richard,egg2)] at 1
do [actT(acquire_object,joe,bowl1),actT(acquire_object,richard,bowl2)] at 2
do [actT(place_in,joe,egg1,bowl1),actT(place_in,richard,egg2,bowl2)] at 3
do [actT(release_object,joe,bowl1),actT(release_object,richard,bowl2)] at 4
SUCCEEDED!-
-
% proc(piTest,
%      actT(acquire_object,joe,egg1):exists(obj,
%           ?(and(obj_is_type(obj,egg),
%                 neg(state_agent_has_object(_,obj,now)))) : actT(acquire_object,richard,board1):actT(acquire_object,richard,obj))).
do [actT(acquire_object,joe,egg1)] at 1
do [actT(acquire_object,richard,board1)] at 2
do [actT(acquire_object,richard,egg2)] at 3
SUCCEEDED!-
-
% proc(control_fails,actT(make,_223812,salad,bowl1)).
do [actT(acquire_object,joe,lettuce1)] at 1
do [actT(acquire_object,joe,board1)] at 2
do [actT(place_in,joe,lettuce1,board1)] at 3
do [actT(begin_task,joe,actT(chop,joe,board1))] at 4
do [event_end_task(joe,actT(chop,joe,board1))] at 7
do [actT(acquire_object,joe,bowl1)] at 8
do [actT(transfer_now,joe,board1,bowl1)] at 9
do [actT(release_object,joe,board1)] at 10
do [actT(release_object,joe,bowl1)] at 11
do [actT(acquire_object,joe,tomato1),actT(acquire_object,richard,carrot1)] at 12
do [actT(acquire_object,joe,board1),actT(acquire_object,richard,board2)] at 13
do [actT(place_in,joe,tomato1,board1),actT(place_in,richard,carrot1,board2)] at 14
do [actT(begin_task,joe,actT(chop,joe,board1)),actT(begin_task,richard,actT(chop,richard,board2))] at 15
do [event_end_task(joe,actT(chop,joe,board1))] at 18
do [actT(acquire_object,joe,bowl1),event_end_task(richard,actT(chop,richard,board2))] at 26
do [actT(transfer_now,joe,board1,bowl1)] at 27
do [actT(release_object,joe,board1)] at 28
do [actT(release_object,joe,bowl1)] at 29
do [actT(acquire_object,richard,bowl1)] at 30
do [actT(transfer_now,richard,board2,bowl1)] at 31
do [actT(release_object,richard,board2)] at 32
do [actT(release_object,richard,bowl1)] at 33
do [actT(acquire_object,joe,bowl1)] at 34
do [actT(begin_task,joe,actT(mix,joe,bowl1,1))] at 35
do [event_end_task(joe,actT(mix,joe,bowl1,1))] at 36
do [actT(release_object,joe,bowl1)] at 37
SUCCEEDED!-
% with pending residual goals
{_49346=_49370}.

% ircEventNow("##logic","tolarz",say("i understand roughly")).
% unused(ircEventNow("##logic","tolarz",say("i understand roughly"))).

baseKB:  ?- 

*/
