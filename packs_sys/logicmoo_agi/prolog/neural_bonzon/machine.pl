:- module(bonzon_mach,[]).

:- include('lpa_emu.pl').

:- style_check(-singleton).


:- dynamic(bonzon_op/1).


bonzon_op(at). 
bonzon_op(check).
bonzon_op(choice).
bonzon_op(cleaner).
bonzon_op(clear).
bonzon_op(clock).
bonzon_op(decrement).
bonzon_op(detect).
bonzon_op(effector).
bonzon_op(excite).
bonzon_op(fetch).
bonzon_op(fire).
bonzon_op(increment).
bonzon_op(inhibit).
bonzon_op(initial).
bonzon_op(join).
bonzon_op(learn).
bonzon_op(left).
bonzon_op(ltd).
bonzon_op(ltp).
bonzon_op(merge).
bonzon_op(move).
bonzon_op(on).
bonzon_op(recall).
bonzon_op(receive).
bonzon_op(resume).
bonzon_op(right).
bonzon_op(robot).
bonzon_op(scan).
bonzon_op(send).
bonzon_op(sense).
bonzon_op(sensor).
bonzon_op(seq).
bonzon_op(signal).
bonzon_op(step).
bonzon_op(stop).
bonzon_op(sync).
bonzon_op(synchro).
bonzon_op(thread).
bonzon_op(threads).
bonzon_op(weight).
bonzon_op(weights).

compile(Tree,Code) :-
  compileTree(Tree,true,1,Code).

compileTree([],Guard,T,[Guard => T:::end]).
compileTree(Sequence,Guard,T,Code) :-
  compileSequence(Sequence,Guard,T,Code).
compileTree([Alternative],Guard,T,Code) :-
  compileAlternative(Alternative,Guard,T,Code).

compileSequence([Instruction|Tree],Guard,T,Code) :-
  T1 is T+1,
  compileInstruction(Instruction,Guard,T,P1),
  compileTree(Tree,Guard,T1,P2),
  append(P1,P2,Code).

compileAlternative(Branch,Guard,T,Code) :-
  compileBranch(Branch,Guard,T,Code).
 
compileAlternative((Branch;Alternative),Guard,T,Code) :-
  compileBranch(Branch,Guard,T,P1),
  compileAlternative(Alternative,Guard,T,P2),
  append(P1,P2,Code).

compileBranch((Guard1| Tree), Guard2, T, Code) :-
    ( Guard2=true ->  Guard=Guard1 ;   Guard=(Guard1, Guard2)),
    compileTree(Tree, Guard, T, Code).


lpa_functor(PX,F,A):- is_list(PX),PX=[F|Len],length(Len,A).
lpa_functor(PX,F,A):- functor(PX,F,A).

% compileInstruction(P(@X), Guard, T, [Guard => T ::: P(@X)] ) :- \+ atom(PX), lpa_functor(PX,F), instruction(F).
compileInstruction(PX, Guard, T, [Guard => T ::: PX] ) :- \+ atom(PX), lpa_functor(PX,F,_), instruction(F).


			/*
*/

instruction(fire).
instruction(resume).
instruction(end).
instruction(exit).
instruction(send).
instruction(receive).
instruction(join).
instruction(merge).
instruction(increment).
instruction(decrement).
instruction(step).
instruction(on).
instruction(choice).
instruction(check).
instruction(scan).
instruction(effector).





			/* machine_cleaner */

:- set_prolog_flag(allow_variable_name_as_functor,true).

%:-consult(compiler_cleaner).

load(Model) :- new(_), 			/* clear machine */
   for_each(thread(Thread,Tree), 			/* for all threads */
     from(threads(Model(Fiber))), 		/* from model fibers */
     do((compile(Tree,Code), 			    /* compile tree */
       forall(member(P,Code),
       insert(Model(Fiber)o(Thread),P))))), 			/* load code */

   for_each(thread(Thread,Tree), 			/* for all threads */
     from(threads(Model)), 			      /* from model */
     do((compile(Tree,Code), 			    /* compile kernel */
     forall(member(P,Code),
       insert(Model(_)o(Thread),P))))),	/* load kernel */

   for_each(Weight(Thread1,Thread2)o(@(X)), 
       from(weights(Model(Fiber))), 			/* load weights */
       do(insert(Model(Fiber),
   Weight(Thread1,Thread2)o(@(X))))).

run(Model) :- loop((sense(Model), 		/* loop sense */
                react(Model), 			  /* react */
                 reflect(Model))). 		/* reflect */

lpa_call_sense(Model) :- if(interrupt(Stream(Interrupt)), 			/* if interrupt */
    then((remove(Model(Stream)o(_),clock(@(_))), 			/*   then clear stream */
  remove(Model(Stream)o(_),excite(@_)), 
  remove(Model(Stream)o(_),inhibit(@_)), 
  remove(Model(Stream)o(_),signal(@_)),   
  for_each(sensor(@X), 			                    /* for each */
  such_that(member(sensor(@X),Interrupt)), 			/* instruction */
  do((set(Model(Stream)o(sense(@X)),clock(1)), 	/* fire sensor */
  write(0:::sense(@X):::sensor(@X)),nl))), 
  set(Model(Stream),seq(1)), 			              /* reset stream */
  remove(Model(Stream),_:::_:::_)))). 			    /* clear synchro */

react(Model) :- for_each((Stream(Thread),T:::Instruction), 			/* for each */
 such_that(ist(Model(Stream)o(Thread), 			                    /* instruction */
(clock(T), T:::Instruction))), 			                            /* retrieve */
 do(Model(Stream)o(Thread)>>>(T:::Instruction))). 			        /* interpret */

reflect(Model):-for_each((Stream,I:::Thread:::Stimulus), 			/* for each stimulus */
 such_that(ist(Model(Stream),I:::Thread:::Stimulus)), 			  /* retrieve synchro */
 do(Model(Stream)>>>(I:::Thread:::Stimulus))). 			          /* report synchro */

/* virtual machine stream reports */ 

Model(Stream)>>>(I:::Thread:::Stimulus) :-
 write(I:::Thread:::Stimulus),nl,
 remove(Model(Stream),I:::Thread:::Stimulus).

/* virtual machine thread instructions */
  
Model(Stream)o(P(@X))>>>(T:::fire(Q(@Y))) :- 			/* fire thread Q(@Y) */
 T1 is T+1,
 set(Model(Stream)o(Q(@Y)),clock(1)), 			/* reset Q(@Y) clock */
  set(Model(Stream)o(P(@X)),clock(T1)) . 			/* set P(@X) clock */

Model(Stream)o(P(@X))>>>(_T:::resume(P(@X))) :- 			/* reenter thread */
  set(Model(Stream)o(P(@X)),clock(1)). 			/* reset P(@X) clock */

Model(Stream)o(P(@X))>>>(T:::end) :- 			/* end active thread */
  remove(Model(Stream)o(P(@X)),clock(T)). 			/* deactive thread */

Model(Stream)o(P(@X))>>>(T:::send(Q(@Y))) :- 			        /* send to Q(@Y) */
 T1 is T+1,
 if_not(ist(Model(Stream)o(Q(@Y)),clock(_)), 			        /* if not active */
    then(set(Model(Stream)o(Q(@Y)),clock(1)))), 			    /*   then fire Q(@Y) */
 
 if_not(ist(Model(Stream),weight(P(@X),Q(@Y))o(W)), 			/* if no weight */
    then(if(ist(Model(Stream),initial(P(@X),Q(@Y))o(W)),  /*   then if weight */
      then(set(Model(Stream),weight(P(@X),Q(@Y))o(W))), 	/*   then attach */
      else(set(Model(Stream),weight(P(@X),Q(@Y))o(0)))))),/*     else inhibit */

 if_not(ist(Model(Stream)o(P(@X)),signal(send(Q(@Y)))), 	      /* if no send signal */
    then(insert(Model(Stream)o(P(@X)),signal(send(Q(@Y)))))), 	/*   then post signal */

 set(Model(Stream)o(P(@X)),clock(T1)). 			/* set clock */
 
Model(Stream)o(P(@X))>>>(T:::receive(Q(@Y))) :- 			/* receive from Q(@Y)*/
 T1 is T+1,
 if(ist(Model(Stream)o(Q(@Y)),signal(send(P(@X)))), 			      /* if signal posted */
    then(if((ist(Model(Stream),weight(Q(@Y),P(@X))o(K)),K>0), 	/*   then if threshold */
    then(set(Model(Stream)o(P(@X)),clock(T1)))))).
 			/*   then clock */

Model(Stream)o(P(@X))>>>(T:::merge(Q(@Y))) :- 			/* merge with Q(@Y) */
 T1 is T+1,
 if_not(ist(Model(Stream)o(P(@X)),signal(merge(Q(@Y)))), 			  /* if no signal */
    then(insert(Model(Stream)o(P(@X)),signal(merge(Q(@Y)))))), 	/*   then post signal */
 
 set(Model(Stream)o(P(@X)),clock(T1)). 			/* set clock */

Model(Stream)o(P(@X))>>>(T:::join(Q(@Y))) :- 			/* join Q(@Y) */
 T1 is T+1,
 if(ist(Model(Stream)o(Q(@Y)),signal(merge(P(@X)))), 			/* if signal */
    then(set(Model(Stream)o(P(@X)),clock(T1)))).  			/*   then set clock */

Model(Stream)o(Thread)>>>(T:::increment(weight(P(F(X)),Q(Y)))) :- 			/* increment weight */
 T1 is T+1,
 ist(Model(Stream),seq(I)), 			/* get global time */

 if_not(ist(Model(Stream),weight(P(F(X)),Q(Y))o(W)), 			/* if no weight */
     then(if(ist(Model(Stream),initial(P(F(X)),Q(Y))o(W)), 			/*   then if initial */
    then(set(Model(Stream),weight(P(F(X)),Q(Y))o(W))), 			/*   then attach */
     else(set(Model(Stream),weight(P(F(X)),Q(Y))o(0)))))), 			/*     else inhibit */

 if((ist(Model(Stream),weight(P(F(X)),Q(Y))o(W)),W<1), 			/* if no threshold */
      then((W1 is W+1, 			/*      then increment */
  insert(Model(Stream),I:::Thread:::weight(P(F(_)),Q(Y))o(W1)), 			/* report */
  set(Model(Stream),weight(P(F(_)),Q(Y))o(W1))))), 			/* attach */
 
 set(Model(Stream)o(Thread),clock(T1)). 			/* set clock */

Model(Stream)o(Thread)>>>(T:::decrement(weight(P(F(X)),Q(Y)))) :- 			/* decrement weight */
 T1 is T+1,
 ist(Model(Stream),seq(I)), 			/* get global time */
 
 if_not(ist(Model(Stream),weight(P(F(X)),Q(Y))o(W)), 			/* if no weight */
    then(if(ist(Model(Stream),initial(P(F(X)),Q(Y))o(W)), 			/*   then if weight */
    then(set(Model(Stream),weight(P(F(X)),Q(Y))o(W))), 			/*   then attach */
      else(set(Model(Stream),weight(P(F(X)),Q(Y))o(0)))))), 			/*     else inhibit */
 
 if((ist(Model(Stream),weight(P(F(X)),Q(Y))o(W)),W>0), 			/* if no threshold */
          then((W1 is W-1, 			/*      then decrement */
  insert(Model(Stream),I:::Thread:::weight(P(F(_)),Q(Y))o(W1)), 			/* report */
  set(Model(Stream),weight(P(F(_)),Q(Y))o(W1))))), 			/* attach */
 
 set(Model(Stream)o(Thread),clock(T1)). 			/* set clock */

Model(Stream)o(Thread)>>>(T:::step(Y)) :- 			/* step */
 T1 is T+1,
 ist(Model(Stream),seq(I)), 			/* get global time */
 if(Y=forward,              			/* get direction */
      then(F=right),
        else(F=left)),
 if(ist(Model(_),at(F(X))), 			/* get position */
      then(X1 is X+1),
        else(X1 is 1)),
 if(X1<8, 			/* if not limit */
    then(set(Model(_),at(F(X1)))), 			/*   then advance */
        else(remove(Model(_),at(_)))), 			/*        else stop */
 I1 is I+1,
 set(Model(Stream),seq(I1)),
 set(Model(Stream)o(Thread),clock(T1)). 			/* set clock */

Model(Stream)o(Thread)>>>(T:::on(X)) :- 			/* detect */
 T1 is T+1,
 if(ist(Model(_),at(X)), 			/* if position */
      then(set(Model(Stream),on(X))), 			/*      then found */
          else(remove(Model(Stream),on(_)))), 			/*        else not found */
 set(Model(Stream)o(Thread),clock(T1)). 			/* set clock */

Model(Stream)o(Thread)>>>(T:::choice(X)) :- 			/* random selection */
 T1 is T+1,
 ist(Model(Stream),seq(I)), 			/* get global time */
 random_e(R,X),
 set(Model(Stream)o(Thread),fetch(R)), 			/* set fetch */
 insert(Model(Stream),I:::Thread:::fetch(R)),
 set(Model(Stream)o(Thread),clock(T1)). 			/* set clock */

Model(Stream)o(Thread)>>>(T:::check(P(@X))) :- 			/* check if P(@X) */
 T1 is T+1,
 ist(Model(Stream),seq(I)), 			/* get global time */
  remove(Model(Stream)o(Thread),excite(P(@X))), 
 remove(Model(Stream)o(Thread),inhibit(P(@X))), 
 if(ist(Model(Stream),P(@X)), 			/* if P(@X) */
    then((set(Model(Stream)o(Thread),excite(P(@X))), 			/*   then set excite */

 insert(Model,Stream(I):::Thread:::excite(P(@X))), 			/* synchronize */
  insert(Model(Stream),I:::Thread:::excite(P(@X))))), 			/* report */
      else((set(Model(Stream)o(Thread),inhibit(P(@X))), 
 insert(Model(Stream),I:::Thread:::inhibit(P(@X))), 
insert(Model,Stream(I):::Thread:::inhibit(P(@X)))))), 			/*     else set inhibit */
  set(Model(Stream)o(Thread),clock(T1)).
 			/* set clock */

Model(Stream)o(Thread)>>>(T:::scan(detect(F(X)),move(Y))) :- 			/* scan synchro */
 T1 is T+1,
 ist(Model(Stream),seq(I)), 			/* get global time */
  remove(Model(Stream)o(Thread),sync(detect(F(X)),move(Y))), 			/* remove synchro */
  if((ist(Model,Stream(I1):::detect(F(X)):::excite(on(F(X)))), 			/* if synchro */
  ist(Model,Stream(I2):::move(Y):::excite(at(F(X)))), I1=I2), 
   then((set(Model(Stream)o(Thread),sync(detect(F(X)),move(Y))), 			/* set synchro */
  insert(Model(Stream),I:::Thread:::sync(detect(F(X)),move(Y)))))), 			/* report synchro */
 set(Model(Stream)o(Thread),clock(T1)).  			/* set clock */

Model(Stream)o(Thread)>>>(T:::effector(P)) :- 			/* virtual effector */
 T1 is T+1,
 ist(Model(Stream),seq(I)), 			/* get global time */
 insert(Model(Stream),I:::Thread:::effector(P)),
 set(Model(Stream)o(Thread),clock(T1)). 			/* set clock */


			/* data types and macro instructions */

:- dynamic(instance/2). 

new(C) :- retractall(instance(C,_)).

insert(C,P) :- assert(instance(C,P)).

remove(C,P) :- retractall(instance(C,P)).

set(C,F(@(X))) :- remove(C,F(@ _)), insert(C,F(@X)).

ist(_C,true).
ist(C,P) :- instance(C,P); instance(C,Q=>P),ist(C,Q).
ist(C,(P,Q)) :- ist(C,P), ist(C,Q).

loop(P) :- repeat, call((lpa_call(P),!)),fail.

interrupt(P):-get_code(C),(C=13->nl,read(P);false).

lpa_call(G):- wdmsg(lpa_call(G)),
  (is_list(G)->apply(call,G);call(G)).

if(P,then(Q)):-lpa_call(P)->lpa_call(Q);true.

if_not(P,then(Q)):-lpa_call(P)->true;lpa_call(Q).

if(P,then(Q),else(R)):-lpa_call(P)->lpa_call(Q);lpa_call(R).


for_each(X, such_that(F), do(P)) :-
    findall(X, lpa_call(F), L),
    forall(member(X, L), P).
for_each(X, from(F), do(P)) :-
    forall(lpa_call(F:::L), forall(member(X, L), lpa_call(P))).

random_e(X,L):-length(L,N),K is random(N),nth0(K,L,X).
                                                   





			/* cleaner  
 
                                          |inhibit|resume(detect(F(X))) 
             -detect(F(X))-check(on(F(X)))| 
            |                             |excite|clear(F(X))-synchro(detect(F(X)),move(A))-  
            |                                                                                         |             |   ---<----------------------------------------------------------------------------------  
            |  |  
            | LTP 
            | \|/                                      |excite|resume(move(A)) 
            +--*>=>-recall(A)-+-move(A)-check(at(F(X)))| 
            |                 |                        |inhibit|stop(A) 
            |   ---<---------- 
            |  |                                                           |excite|resume(move(A)) 
            | LTD                         |fetch(A)|move(A)-check(at(F(_)))| 
            | \|/                         |                                |inhibit|stop(A) sense(F(X))-+--*>=>-learn(F)-choice([A,B])| 
            | /|\                         |                                |excite|resume(move(B)) 
            | LTD                         |fetch(B)|move(B)-check(at(F(_)))|  
            |  |                                                           |inhibit|stop(A) 
            |   ---<----------  
            |                 |                        |excite|resume(move(A)) 
            +--*>=>-recall(B)-+-move(B)-check(at(F(X)))| 
            | /|\                                      |inhibit|stop(A) 
            | LTP  
            |  | 
            |   ---<----------------------------------------------------------------------------------  
            |                                                                                         | 
            |                              |          
            |                             |excite|clear(F(X))-synchro(detect(F(X)),move(B))- 
             -detect(F(X))-check(on(F(X)))| 
                                          |inhibit|resume(detect(F(X))) 
*/
  

 %:-consult(machine_cleaner).
 
( threads(robot(cleaner(_,_))) :::
  [ thread( sense(F(X)), [
      merge(ltd(sense(F(X)),learn(F))),
      merge(ltp(sense(F(X)),recall(Y))), send(learn(F)),fire(detect(F(X))),
      send(recall(Y))]),
    thread( detect(F(X)), [
      on(F(X)),
      check(on(F(_))),
      ( ( excite(on(F(_))) |
          [ effector(clear(F(X))),
            fire(synchro(detect(F(_)),move(_)))]) ;
        inhibit(on(F(_)))|[resume(detect(F(_)))])]),
    thread( learn(F), [
      receive(sense(F(X))),
      choice([_,_]),
      (fetch(_)|[fire(move(_))]);(fetch(_)|[fire(move(_))])]),
    thread(
       synchro(detect(F(X)),move(Y)),
       [ scan(detect(F(X)),move(Y)),
         sync(detect(F(_)),move(_))|[fire(ltp(sense(F(_)),recall(_)))]]),
    thread( recall(Y), [
      receive(sense(F(X))), fire(ltd(sense(F(_)),learn(_))),fire(move(_))]),
    thread( move(Y), [
      step(Y),
      check(at(F(_))),
      ( excite(at(F(_)))|[resume(move(_))] ;
        inhibit(at(F(_)))|[effector(stop(_))])])]).

( threads(robot) :::
  [ thread(ltp(Q,R),[join(Q),increment(weight(Q,R))]),
    thread(ltd(_,_),[join(_),decrement(weight(_,_))])]).

( weights(robot(cleaner(_,_))) :::
  [ initial(sense(_),learn(_))o 1,
    initial(recall(_),do(_))o 1]).


			/* example run
consult(cleaner).
load(robot).
run(robot).

cleaner(forward,backward)o([sensor(right(3))]).

0 : sense(right(3)) : sensor(right(3))
1 : learn(right) : fetch(backward)
1 : detect(right(3)) : inhibit(on(right(3)))
2 : move(backward) : excite(at(left(1)))
2 : detect(right(3)) : inhibit(on(right(3)))
3 : move(backward) : excite(at(left(2)))
3 : detect(right(3)) : inhibit(on(right(3)))
4 : move(backward) : excite(at(left(3)))
4 : detect(right(3)) : inhibit(on(right(3)))
5 : move(backward) : excite(at(left(4)))
5 : detect(right(3)) : inhibit(on(right(3)))
6 : move(backward) : excite(at(left(5)))
6 : detect(right(3)) : inhibit(on(right(3)))
7 : move(backward) : excite(at(left(6)))
7 : detect(right(3)) : inhibit(on(right(3)))
8 : move(backward) : excite(at(left(7)))
8 : detect(right(3)) : inhibit(on(right(3)))
9 : move(backward) : inhibit(at(_44192(_44198)))
9 : move(backward) : effector(stop(backward))
9 : detect(right(3)) : inhibit(on(right(3)))
9 : detect(right(3)) : inhibit(on(right(3)))
9 : detect(right(3)) : inhibit(on(right(3)))
...
cleaner(forward,backward)o([sensor(right(3))]).
0 : sense(right(3)) : sensor(right(3))
1 : learn(right) : fetch(forward)
1 : detect(right(3)) : inhibit(on(right(3)))
2 : move(forward) : excite(at(right(1)))
2 : detect(right(3)) : inhibit(on(right(3)))
3 : move(forward) : excite(at(right(2)))
3 : detect(right(3)) : inhibit(on(right(3)))
4 : move(forward) : excite(at(right(3)))
4 : detect(right(3)) : excite(on(right(3)))
5 : detect(right(3)) : effector(clear(right(3)))
5 : move(forward) : excite(at(right(4)))
5 : synchro(detect(right(3)),move(forward)) : sync(detect(right(3)),move(forward))
6 : move(forward) : excite(at(right(5)))
6 : ltp(sense(right(3)),recall(forward)) : weight(sense(right(_14222)),recall(forward))o(1)
7 : move(forward) : excite(at(right(6)))
7 : ltd(sense(right(3)),learn(right)) : weight(sense(right(_38444)),learn(right))o(0)
8 : move(forward) : excite(at(right(7)))
9 : move(forward) : inhibit(at(_42122(_42128)))
9 : move(forward) : effector(stop(forward))
cleaner(forward,backward)o([sensor(right(3))]).
   
0 : sense(right(3)) : sensor(right(3))
1 : detect(right(3)) : inhibit(on(right(3)))
1 : detect(right(3)) : inhibit(on(right(3)))
2 : move(forward) : excite(at(right(1)))
2 : detect(right(3)) : inhibit(on(right(3)))
3 : move(forward) : excite(at(right(2)))
3 : detect(right(3)) : inhibit(on(right(3)))
4 : move(forward) : excite(at(right(3)))
4 : detect(right(3)) : excite(on(right(3)))
5 : detect(right(3)) : effector(clear(right(3)))
5 : move(forward) : excite(at(right(4)))
5 : synchro(detect(right(3)),move(forward)) : sync(detect(right(3)),move(forward))
6 : move(forward) : excite(at(right(5)))
7 : move(forward) : excite(at(right(6)))
8 : move(forward) : excite(at(right(7)))
9 : move(forward) : inhibit(at(_22894(_22900)))
9 : move(forward) : effector(stop(forward))
cleaner(forward,backward)o([sensor(right(2))]).
  
0 : sense(right(2)) : sensor(right(2))
1 : detect(right(2)) : inhibit(on(right(2)))
1 : detect(right(2)) : inhibit(on(right(2)))
2 : move(forward) : excite(at(right(1)))
2 : detect(right(2)) : inhibit(on(right(2)))
3 : move(forward) : excite(at(right(2)))
3 : detect(right(2)) : excite(on(right(2)))
4 : detect(right(2)) : effector(clear(right(2)))
4 : move(forward) : excite(at(right(3)))
4 : synchro(detect(right(2)),move(forward)) : sync(detect(right(2)),move(forward))
5 : move(forward) : excite(at(right(4)))
6 : move(forward) : excite(at(right(5)))
7 : move(forward) : excite(at(right(6)))
8 : move(forward) : excite(at(right(7)))
9 : move(forward) : inhibit(at(_41294(_41300)))
9 : move(forward) : effector(stop(forward))
cleaner(forward,backward)o([sensor(left(2))]).
0 : sense(left(2)) : sensor(left(2))
1 : learn(left) : fetch(forward)
1 : detect(left(2)) : inhibit(on(left(2)))
2 : move(forward) : excite(at(right(1)))
2 : detect(left(2)) : inhibit(on(left(2)))
3 : move(forward) : excite(at(right(2)))
3 : detect(left(2)) : inhibit(on(left(2)))
4 : move(forward) : excite(at(right(3)))
4 : detect(left(2)) : inhibit(on(left(2)))
5 : move(forward) : excite(at(right(4)))
5 : detect(left(2)) : inhibit(on(left(2)))
6 : move(forward) : excite(at(right(5)))
6 : detect(left(2)) : inhibit(on(left(2)))
7 : move(forward) : excite(at(right(6)))
7 : detect(left(2)) : inhibit(on(left(2)))
8 : move(forward) : excite(at(right(7)))
8 : detect(left(2)) : inhibit(on(left(2)))
9 : move(forward) : inhibit(at(_43862(_43868)))
9 : move(forward) : effector(stop(forward))
9 : detect(left(2)) : inhibit(on(left(2)))
9 : detect(left(2)) : inhibit(on(left(2)))
9 : detect(left(2)) : inhibit(on(left(2)))
cleaner(forward,backward)o([sensor(left(1))]).
0 : sense(left(1)) : sensor(left(1))
1 : learn(left) : fetch(backward)
1 : detect(left(1)) : inhibit(on(left(1)))
2 : move(backward) : excite(at(left(1)))
2 : detect(left(1)) : excite(on(left(1)))
3 : detect(left(1)) : effector(clear(left(1)))
3 : move(backward) : excite(at(left(2)))
3 : synchro(detect(left(1)),move(backward)) : sync(detect(left(1)),move(backward))
4 : move(backward) : excite(at(left(3)))
4 : ltp(sense(left(1)),recall(backward)) : weight(sense(left(_37664)),recall(backward))o(1)
5 : move(backward) : excite(at(left(4)))
5 : ltd(sense(left(1)),learn(left)) : weight(sense(left(_14100)),learn(left))o(0)
6 : move(backward) : excite(at(left(5)))
7 : move(backward) : excite(at(left(6)))
8 : move(backward) : excite(at(left(7)))
9 : move(backward) : inhibit(at(_37904(_37910)))
9 : move(backward) : effector(stop(backward))
*/

:- set_prolog_flag(allow_variable_name_as_functor,false).
%:- listing(bonzon_op/1).
