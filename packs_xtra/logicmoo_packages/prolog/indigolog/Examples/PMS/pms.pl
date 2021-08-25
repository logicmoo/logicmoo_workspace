%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: aPMS/pms.pl
%
%  AUTHORS : Massimiliano de Leoni, Andrea Marrella and Stefano Valentini
%  EMAIL  : deleoni@dis.uniroma1.it,marrella@dis.uniroma1.it,stefano_valentini82@libero.it
%  TYPE   : system independent code
%  TESTED : SWI Prolog 5.id_.1id_ http://www.swi-prolog.org
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%  A basic action theory (BAT) is described with:
%
% -- fun_fluent(fluent)     : for each functional fluent (non-ground)
% -- rel_fluent(fluent)     : for each relational fluent (non-ground)
%
%           e.g., rel_fluent(painted(C)).
%           e.g., fun_fluent(color(C)).
%
% -- prim_action(action)    : for each primitive action (ground)
% -- exog_action(action)    : for each exogenous action (ground)
%
%           e.g., prim_action(clean(C)) :- domain(C,country).
%           e.g., exog_action(painte(C,B)):- domain(C,country), domain(B,color).
%
% -- senses(action,fluent)  : for each sensing action
%
%           e.g, poss(check_painted(C),  painted(C)).
%
% -- poss(action,cond)      : when cond, action is executable
%
%           e.g, poss(clean(C),   and(painted(C),holding(cleanear))).
%
% -- initially(fluent,value): fluent has value in Sid_ (ground)
%
%          e.g., initially(painted(C), false):- domain(C,country), C\=3.
%                initially(painted(3), true).
%                initially(color(3), blue).
%
% -- causes_val(action,fluent,value,cond)
%          when cond holds, doing act causes functional fluent to have value
%
%            e.g., causes_val(paint(C2,V), color(C), V, C = C2).
%               or causes_val(paint(C,V), color(C), V, true).
%
% -- causes_true(action,fluent,cond)
%          when cond holds, doing act causes relational fluent to hold
% -- causes_false(action,fluent,cond)
%          when cond holds, doing act causes relational fluent to not hold
%
%            e.g., causes_true(paint(C2,_), painted(C), C = C2).
%               or causes_true(paint(C,_), painted(C), true).
%            e.g., causes_false(clean(C2),  painted(C), C = C2).
%               or causes_false(clean(C),  painted(C), true).
%
% A high-level program-controller is described with:
%
% -- proc(name,P): for each procedure P 
% -- simulator(N,P): P is the N exogenous action simulator
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic controller/1.


/* SOME DOMAIN-INDEPENDENT PREDICATES TO DENOTE THE VARIOUS OBJECTS OF INTEREST IN THE FRAMEWORK */

/* Available services */
services([1,2,3,4,5]).
service(S) :- domain(S,services).

/* Tasks defined in the Process specification */
tasks([takephoto,evaluatephoto,compilequest,sendbygprs,go]).
task(T) :- domain(T,tasks).

/* Capabilities relevant for the process of interest */
capabilities([camera,evaluation,compile,gprs]).
capability(C) :- domain(C,capabilities).

/* The list of identifiers that may be used to distinguish different istances of the same task */ 
task_identifiers([id_1,id_2,id_3,id_4,id_5,id_6,id_7,id_8,id_9,id_10,id_11,id_12,id_13,id_14,id_15,id_16,id_17,id_18,id_19,id_20,id_21,
id_22,id_23,id_24,id_25,id_26,id_27,id_28,id_29,id_30,id_31,id_32,id_33,id_34,id_35,id_36,id_37,id_38,id_39,id_40,id_41,
id_42,id_42,id_44,id_45,id_46,id_47,id_48,id_49,id_50]).

id(D) :- domain(D,task_identifiers).

/* Definition of predicate workitem(T,D,I). It identifies a task TASK with id ID and input INPUT */
listelem(workitem(go,ID,INPUT)) :- id(ID),location(INPUT).
listelem(workitem(compilequest,ID,INPUT)) :- id(ID),location(INPUT).
listelem(workitem(evaluatephoto,ID,INPUT)) :- id(ID),location(INPUT).
listelem(workitem(takephoto,ID,INPUT)) :- id(ID),number(Nadd,2),location(LOC),INPUT = [LOC,Nadd].
listelem(workitem(sendbygprs,ID,input)) :- id(ID).

worklist([]).
worklist([ELEM | TAIL]) :- worklist(TAIL),listelem(ELEM).


/* The capabilities required for each task */ 
required(takephoto,camera).
required(evaluatephoto,evaluation).
required(compilequest,compile).
required(sendbygprs,gprs).

/* The capabilities provided by each service */
provide(1,camera).
provide(2,evaluation).
provide(2,camera).
provide(3,compile).
provide(4,evaluation).
provide(4,camera).
provide(5,compile).


/* There is nothing to do caching on (required because cache 1 is static) */
cache(_):-fail.

/* Definition of predicate loc(i,j) identifying the current location of a service */
gridsize(2).
gridindex(V) :- 
	gridsize(S),
	get_integer(0,V,S).
location(loc(I,J)) :- gridindex(I), gridindex(J).

/* The definition of integer numbers */
number(N,M) :- get_integer(0,N,M).

/* square(X,Y): Y is the square of X */
square(X,Y) :- Y is X * X.

/* member(ELEM,LIST): returns true if ELEM is contained in LIST */
member(ELEM,[HEAD|_]) :- ELEM=HEAD.
member(ELEM,[_|TAIL]) :- member(ELEM,TAIL).
listEqual(L1,L2) :- subset(L1,L2),subset(L2,L1).

/*  DOMAIN-INDEPENDENT FLUENT */

/* Basically, there has to be some definition for predicates causes_true and causes_false, at least one 
for each.  We have added  the following dummy code:  */ 
causes_true(_,_,_) :- false.
causes_false(_,_,_) :- false.

/* Indicates that a list LWRK of workitems has been assigned to service SRVC */ 
rel_fluent(assigned(_,SRVC)) :- service(SRVC).

/* assigned(LWRK,SRVC) holds after action assign(LWrk,Srvc) */         
causes_val(assign(LWRK,SRVC),assigned(LWRK,SRVC),true,true).        

/* assigned(LWRK,SRVC) holds no longer after action release(LWRK,SRVC) */ 
causes_val(release(LWRK,SRVC),assigned(LWRK,SRVC),false,true).

/* Indicates that task TASK with id ID has been begun by service SRVC */ 
rel_fluent(enabled(TASK,ID,SRVC)) :- task(TASK), service(SRVC), id(ID).

/* enabled(T,D,N) becomes true if the service N calls the exogenous action readyToStart(T,D,N), indicating the 
starting of the task T with id D */ 
causes_val(readyToStart(TASK,ID,SRVC),enabled(TASK,ID,SRVC),true,true).

/* enabled(T,D,N) becomes false if the service N calls the exogenous action finishedTask(T,D,N,V), indicating the 
ending of the task T with id D and output value V */ 
causes_val(finishedTask(TASK,ID,SRVC,_),enabled(TASK,ID,SRVC),false,true).

/* free(N) indicates that service N is not currently executing any other task */ 
rel_fluent(free(SRVC)) :- service(SRVC).

/* free(N) becomes true if the PMS engine calls the action release(LWRK,SRVC). LWRK is a worklist, SRVC is a service */ 
causes_val(release(_,SRVC),free(SRVC),true,true).

/* free(N) becomes false if the PMS engine calls the action assign(LWRK,SRVC). LWRK is a worklist, SRVC is a service */           
causes_val(assign(_,SRVC),free(SRVC),false,true).


/*  ACTIONS and PRECONDITIONS*/

/* Every task execution is the sequence of four actions:  
    (i)     The assignment of the task to a service.  
    (ii)    The notification to the service N to start executing the task T. It happens when the service N 
            calls the exogenous action readyToStart(T,D,N)).  
    (iii)   The PMS stops the service acknowledging the successful termination of its task. It happens 
            when the service N calls the exogenous action finishedTask(T,D,N,V).   
    (iv)    Finally, the PMS releases the service, which becomes free again.  
     
    We formalize these four actions as follows (these are the only actions used in our formalization):    */

prim_action(assign(LWRK,SRVC)) :- worklist(LWRK),service(SRVC).
poss(assign(LWRK,SRVC),true) :- worklist(LWRK),service(SRVC).

prim_action(ackTaskCompletion(TASK,ID,SRVC)) :- task(TASK), service(SRVC), id(ID).
poss(ackTaskCompletion(TASK,ID,SRVC), neg(enabled(TASK,ID,SRVC))).

prim_action(start(TASK,ID,SRVC,INPUT)) :- listelem(workitem(TASK,ID,INPUT)), service(SRVC).

poss(start(TASK,ID,SRVC,_INPUT),true) :- task(TASK),id(ID),service(SRVC).

prim_action(release(LWRK,SRVC)) :- worklist(LWRK),service(SRVC).
poss(release(_LWRK,_SRVC), true).

/*  DOMAIN-DEPENDENT FLUENT */

/* at(SRVC) indicates that service SRVC is in position P */
fun_fluent(at(SRVC)) :- service(SRVC).

/* at(N) assumes the value loc(I,J) if service N calls the exogenous action finishedTask(T,D,N,V) and V=loc(I,J) */  
causes_val(finishedTask(go,_ID,SRVC,V),at(SRVC),loc(I,J),V=loc(I,J)).



fun_fluent(photoBuild(LOC)) :- location(LOC).
causes_val(finishedTask(TASK,_ID,SRVC,V),photoBuild(LOC),N,
    							and(TASK=takephoto,
							   			and(number(Nadd,2),
							      		and(V=[LOC,Nadd],
								  		and(at(SRVC)=LOC,
     									and(Nold=photoBuild(LOC),
								N is Nold+Nadd)))))
).


rel_fluent(evaluationOK(LOC)) :- location(LOC).
causes_val(finishedTask(TASK,_ID,_SRVC,V),evaluationOK(loc(I,J)), true,
    							and(TASK=evaluatephoto,
     								and(V=(loc(I,J),ok),
      									 and(N=photoBuild(loc(I,J)),
      											  	N>3)))).


rel_fluent(infoSent).
causes_val(finishedTask(TASK,_ID,_SRVC,V),infoSent, true, and(TASK=sendByGPRS, V=ok)).

proc(hasConnection(SRVC),and(service(SRVC),hasConnectionHelper(SRVC,[SRVC]))).

proc(hasConnectionHelper(SRVC,M), or(neigh(SRVC,1),
					some(n,
					     and(service(n),
					     and(neg(member(n,M)),
					     and(neigh(n,SRVC),
					     hasConnectionHelper(n,[n|M]))))))).

proc(neigh(SRVC1,SRVC2),  
	some(x1,
	some(x2,
	some(y1,
	some(y2,
	some(k1,
	some(k2,
	and(at(SRVC1)=loc(x1,y1),
	and(at(SRVC2)=loc(x2,y2),
	and(square(x1-x2,k1),
	and(square(y1-y2,k2),
	sqrt(k1+k2)<7))))))))))).


/* INITIAL STATE:  */

/*initially(free(SRVC),true) :- service(SRVC).*/

/*TEMPORARY CODE*/
initially(free(1),true).
initially(free(2),false).
initially(free(3),false).
initially(free(4),false).
initially(free(5),false).
/*END TEMPORARY CODE*/

initially(at(SRVC),loc(0,0)) :- service(SRVC).
initially(at_prev(SRVC),loc(0,0)) :- service(SRVC).

initially(photoBuild(LOC),0) :- location(LOC).
initially(photoBuild_prev(LOC),0) :- location(LOC).

initially(evaluationOK(LOC),false) :- location(LOC).
initially(evaluationOK_prev(LOC),false) :- location(LOC).

initially(infoSent,false).
initially(infoSent_prev,false).

initially(enabled(TASK,ID,SRVC),false) :- task(TASK), service(SRVC), id(ID).
initially(assigned(_LWRK,SRVC),false) :- service(SRVC).

initially(finished,false).

/* EXOGENOUS ACTIONS EXECUTED BY SERVICES */

exog_action(readyToStart(TASK,ID,SRVC)) :- task(TASK), service(SRVC), id(ID).
exog_action(finishedTask(TASK,ID,SRVC,_V)) :- task(TASK), service(SRVC), id(ID).

/* exogenous action which target is to reduce of a fixed number V the overall pics taken in location LOC */
exog_action(photoLost(V,LOC)) :- location(LOC),number(V,2).
causes_val(photoLost(_,_),exogenous,true,true).

exog_action(disconnect(SRVC,loc(I,J))) :- service(SRVC), gridindex(I), gridindex(J).
causes_val(disconnect(_,_),exogenous,true,true).


/* PREDICATES AND ACTIONS FOR MONITORING ADAPTATION */

/* at(N) assumes the value loc(I,J) if the exogenous action disconnect(N,loc(I,J)) is called */  
causes_val(disconnect(SRVC,loc(I,J)),at(SRVC),loc(I,J),true).

/* photoBuild(LOC,N) reduces of a fixed number N the overall pics taken in the location LOC */  
causes_val(photoLost(N,LOC),photoBuild(LOC),M,and(photoBuild(LOC)=H,M is H-N)).

prim_action(A) :- exog_action(A).
poss(A,true) :- exog_action(A).

/* ADAPTATION FEATURES */

fun_fluent(at_prev(SRVC)) :- service(SRVC).
fun_fluent(photoBuild_prev(LOC)) :- location(LOC).
rel_fluent(evaluationOK_prev(LOC)) :- location(LOC).
rel_fluent(infoSent_prev).

causes_val(disconnect(_,_),at_prev(SRVC),LOC,at(SRVC)=LOC) :- service(SRVC),location(LOC).
causes_val(disconnect(_,_),photoBuild_prev(LOC),X,photoBuild(LOC)=X) :- location(LOC).
causes_val(disconnect(_,_),evaluationOK_prev(Loc),X,evaluationOK(Loc)=X) :- location(Loc).
causes_val(disconnect(_,_),infoSent_prev,X,infoSent=X).

causes_val(photoLost(_,_),at_prev(SRVC),LOC,at(SRVC)=LOC) :- service(SRVC),location(LOC).
causes_val(photoLost(_,_),photoBuild_prev(LOC),X,photoBuild(LOC)=X) :- location(LOC).
causes_val(photoLost(_,_),evaluationOK_prev(Loc),X,evaluationOK(Loc)=X) :- location(Loc).
causes_val(photoLost(_,_),infoSent_prev,X,infoSent=X).

proc(hasConnection_prev(SRVC), hasConnectionHelper_prev(SRVC,[SRVC])).

proc(hasConnectionHelper_prev(SRVC,M), 
	or(neigh_prev(SRVC,1),
		some(n,and(service(n),
					and(neg(member(n,M)),
					and(neigh_prev(n,SRVC),
							hasConnectionHelper_prev(n,[n|M]))))))
).

proc(neigh_prev(Srvc1,Srvc2),  some(x1,some(x2,some(y1,some(y2,some(k1,some(k2,and(at_prev(Srvc1)=loc(x1,y1),and(at_prev(Srvc2)=loc(x2,y2),and(square(x1-x2,k1),and(square(y1-y2,k2),sqrt(k1+k2)<7))))))))))).

/* ADAPTATION DOMAIN-INDEPENDENT FEATURES */

prim_action(finish).
poss(finish,true).

rel_fluent(finished).
causes_val(finish,finished,true,true).

rel_fluent(exogenous).
initially(exogenous,false).

rel_fluent(adapted).

prim_action(resetExo).
poss(resetExo,true).

causes_val(resetExo,exogenous,false,true).
causes_val(adaptStart,adapted,false,true).
causes_val(adaptFinish,adapted,true,true).

prim_action(adaptFinish).
poss(adaptFinish,true).
prim_action(adaptStart).
poss(adaptStart,true).

%proc(relevant,
%  and(writeln('IS IT RELEVANT?'),
%    or(some(Srvc,and(service(Srvc),
%		  and(hasConnection_prev(Srvc), 
%                     neg(hasConnection(Srvc))))),
%       some(Loc,and(location(Loc),
%		  and(photoBuild_prev(Loc)=Y, 
%                      neg(photoBuild(Loc)=Y))))))).

proc(relevant,
    		or(some(srvc,	and(service(srvc),
		  							and(hasConnection_prev(srvc), 
                      						neg(hasConnection(srvc))))
       					), % some
       			some(loc,and(location(loc),
		       						   neg(photoBuild_prev(loc)=photoBuild(loc))
		       							  )
		       			) % some
		       	) % or
).


proc(goalReached,neg(relevant)).


proc(adapt,[adaptStart, ?(report_message(user, 'About to adapt...')),
	  	 pconc([adaptingProgram, adaptFinish],
	 		while(neg(adapted), [?(writeln('waiting')),wait]))
	   ]
).	


proc(adaptingProgram,  
	searchn([?(true),searchProgram, ?(report_message(user, 'Adaptation program found!'))], 
				[ assumptions([ 	[ assign([workitem(T,D,_I)],N), readyToStart(T,D,N) ],
				 		       			 	[ start(T,D,N,I), finishedTask(T,D,N,I) ]
						      			])
				]
				)
). 

proc(searchProgram,plans(2,2)).

proc(plans(M,N),[?(M<(N+1)),ndet(
				[actionSequence(M),?(goalReached)],
				[?(SUCCM is M+1),plans(SUCCM,N)]
			    )]).

proc(actionSequence(N),ndet(
				[?(N=0)],
				[?(N>0),pi([t,i,n], 
				 [ ?(isPickable([workitem(t,id_30,i)],n)),
				   assign([workitem(t,id_30,i)],n),
				   start(t,id_30,n,i),
				   ackTaskCompletion(t,id_30,n),
				   release([workitem(t,id_30,i)],n)
				  ]
			     ), 
				?(PRECN is N-1), actionSequence(PRECN)]
			   )).



/* report_message(user,[]) */

/* ABBREVIATIONS - BOOLEAN FUNCTIONS */

proc(isPickable(X,N), 
	or(X=[], 
		and(free(N),
		and(X=[A|TAIL],
		and(listelem(A),
		and(A=workitem(T,_D,_I),
		and(isExecutable(T,N),
		isPickable(TAIL,N))))))
	)
).


/* Particular attention is needed with respect to the function isExecutable(X,N). The target of this function
is to verify if task X can be executed by service N. It means that all capabilities needed by task X must
be provided by service N. The function follows three steps:
    1) It captures all the capabilities requested by task X recording them in a list A
    2) It captures all the capabilities performed by service N recording them in a list C
    3) It verifies if capabilities of list A are contained in list C; in that case it returns true */     

proc(isExecutable(T,S), and(findall(B,required(T,B),A),and(findall(B,provide(S,B),C),subset(A,C)))).


/* THIS IS THE MAIN PROCEDURE FOR INDIGOLOG */

proc(main,  mainControl(N)) :- controller(N), !.
proc(main,  mainControl(3)). % default one

proc(manageAssignment(X), [atomic([pi(n,[?(isPickable(X,n)), assign(X,n)])])]).

proc(manageExecution(X), pi(n,[?(assigned(X,n)=true),manageExecutionHelper(X,n)])).

proc(manageExecutionHelper([],_N),[]).

proc(manageExecutionHelper([workitem(T,D,I)|TAIL],N), 
	[start(T,D,N,I), ackTaskCompletion(T,D,N), manageExecutionHelper(TAIL,N)]).

proc(manageTermination(X), [atomic([pi(n,[?(assigned(X,n)=true), release(X,n)])])]).

proc(manageTask(X), [manageAssignment(X),manageExecution(X),manageTermination(X)]).

/* This is the process represented by the activity diagram */

proc(mainControl(5), prioritized_interrupts(
	[interrupt(and(neg(finished),exogenous), monitor),
	 interrupt(true, [process,finish]),
	 interrupt(neg(finished), wait)
	])).


proc(monitor,[?(writeln('Monitor')),
		  ndet(
			[?(neg(relevant)),?(writeln('NonRelevant'))],
			[?(relevant),?(writeln('Relevant')),adapt]
		  ), resetExo
		]).

proc(process, 
[rrobin([[manageTask([workitem(compilequest,id_1,loc(1,1))]),rrobin([manageTask([workitem(takephoto,id_2,loc(1,1))]),manageTask([workitem(takephoto,id_3,loc(1,1))])])],
[rrobin([
	manageTask(
		[workitem(go,id_4,loc(2,2)),
		 workitem(takephoto,id_5,loc(2,2))]),
        manageTask(
		[workitem(go,id_6,loc(2,2)),
		 workitem(compilequest,id_7,loc(2,2))])]),
	manageTask([workitem(takephoto,id_8,loc(2,2))])
	]])]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  INFORMATION FOR THE EXECUTOR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translations of domain actions to real actions (one-to-one)
actionNum(X,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
