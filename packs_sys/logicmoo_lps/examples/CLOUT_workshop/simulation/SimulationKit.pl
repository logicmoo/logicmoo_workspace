
:- expects_dialect(lps).

% A collection of "classes", related groups of fluents and other predicates representing some real world "object",
% for simple simulations on a 2D pixel space
% By Miguel Calejo and Bob Kowalski, 2019; all rights reserved; academic usage allowed;
% Please contact mc@interprolog.com or r.kowalski@imperial.ac.uk for rights and licensing information

% Object: realized as the "primary key" (and first argument) of some predicates,
%	its identifier is a classname(id) term
% TODO: add pseudo random noise to the simulation and sensors
% TODO: conveyor should "drop off" transported objects when they get to the end

%%% First, some generic object facets
%
fluents location(_Ob,_Point). % Point is point(X,Y)
% Although some locations do not change, objects are assumed to be created over time, making their "timeless"
% attributes - such as some locations - as effectively... fluents

actions setLocation(_Ob,_Point).
setLocation(Ob,L) updates Old to L in location(Ob,Old) if L=point(X,Y),number(X),number(Y). % some basic type checking

fluents working(_Ob,_Yes). % whether an object is on (on/off)

actions start(_Object1), stop(_Object2).

start(Ob) updates Old to on in working(Ob,Old).
stop(Ob) updates Old to off in working(Ob,Old).

working(Ob) at T if working(Ob,on) at T.

% LPS hacking invoked to duplicate an object:
clone(Ob,New) from T1 to T2 if
	Ob=..[Class,_], objectName(X), New=..[Class,X],
	findall(NF,(holds(F,T1), F=..[FF,Ob|Args], NF=..[FF,New|Args]),NewFluents),
	initiateAll(NewFluents) from T1 to T2.

initiateAll([F|Fluents]) from T1 to T2 if 
	initiate F from T1, initiateAll(Fluents) from T1 to T2.
initiateAll([]) from T1 to T2 if 
	T2 =:= T1+1.

% Make N clones of a prototype object in a single cycle
clone(N,Proto,[New|Clones]) from T1 to T2 if 
	N>0, clone(Proto,New) from T1 to T2,
	NewN is N-1, clone(NewN,Proto,Clones) from T1 to T2.
clone(0,_,[]).

%%% Conveyor: an object continuously changing some objects' locations, 
% in a straight line from the start point to the endpoint, at Speed (per LPS cycle) as measured on the line vector

% the conveyor's location is the start point
fluents transports(_ConveyorId,_ObjectId), conveyorEndpoint(_C,_Point), conveyorSpeed(_C,_Speed). % Speed may be negative
actions setConveyorSpeed(_ObjectID,_Speed).
setConveyorSpeed(Ob,S) updates Old to S in conveyorSpeed(Ob,Old).

placeOnConveyor(Ob,ConveyorOb,DistanceFromStart) from T1 to T2 if
	not transports(ConveyorOb,Ob) at T1, 
	location(ConveyorOb,Start), conveyorEndpoint(ConveyorOb,End), 
	interpolate(Start,End,DistanceFromStart,Point),
	update Old to Point in location(Ob,Old) from T1 to T2,
	initiate transports(ConveyorOb,Ob) from T1 to T2.

% Ob will be the new object ID; Name is optional (can be var), and somehow redundant with Ob; it's useful mostly
% for debugging
createConveyor(Name,Start,End,Ob) from T1 to T2 if
	Ob=conveyor(Name), objectName(Name),
	initiate location(Ob,Start) from T1 to T2,
	initiate conveyorEndpoint(Ob,End) from T1 to T2,
	initiate conveyorSpeed(Ob,0) from T1 to T2,
	initiate working(Ob,off) from T1 to T2.

conveyor(C,Start,End,Working,Speed) at T if
	conveyorEndpoint(C,End) at T, location(C,Start) at T, 
	working(C,Working) at T, conveyorSpeed(C,Speed) at T.

% The conveyor's active behavior: for all transported objects still far from the end, move them closer
if 
	conveyorSpeed(C,Speed) at T1, working(C,on) at T1, transports(C,Ob) at T1, conveyorEndpoint(C,End) at T1, 
	location(Ob,Current) at T1, location(C,Start) at T1, notReachingConveyorEnds(Speed,Start,End,Current)
then
	newPosition(Current,Start,End,Speed,NewPoint),
	update Current to NewPoint in location(Ob,Current) from T1. 

% notGoingOverConveyorEnds(Speed,Start,End,Current).
notReachingConveyorEnds(0,_,_,_).
notReachingConveyorEnds(Speed,_,End,Current):- Speed>0, distance(Current,End,Delta), Delta>=abs(Speed).
notReachingConveyorEnds(Speed,Start,_End,Current):- Speed<0, distance(Start,Current,Delta), Delta>=abs(Speed).

% makeLocation(Conveyor,Object,DestinationObject)
% transport Object to the closest possible point to DestinationObject's location
% Object must be already placed on the conveyor, DestinationObject can NOT be on the conveyor
% The conveyor must be stopped, and a speed must be set (its sign may get reversed)
% Find perpendicular to conveyor vector closest to DO...

makeLocation(C,Ob,DestinationOb) from T1 to T3 if
	transports(C,Ob) at T1, not transports(C,DestinationOb) at T1,
	not working(C) at T1, conveyorSpeed(C,Speed_) at T1,
	conveyorEndpoint(C,End) at T1, location(C,Start) at T1,
	location(Ob,Current) at T1, location(DestinationOb,To) at T1,
	perpendicular_intersection(Start,End,To,Destination),
	% check proper direction of the speed vector needed; 
	% which point is closer to the conveyor origin?
	distance(Start,Current,DC), distance(Start,Destination,DD),
	mayToggleSpeed(DC,DD,Speed_,Speed),
	setConveyorSpeed(C,Speed) from T1, start(C) from T1,
	location(Ob,L) at T2, distance(L,Destination,Delta), Delta=<abs(Speed),
	stop(C) from T2 to T3.

mayToggleSpeed(DC,DD,S,NS) :- DD>=DC, !, (S>=0->  NS=S;NS is -S).  
mayToggleSpeed(_DC,_DD,S,NS) :- S>0-> NS is -S; NS = S.  

	
display(conveyor(C,point(SX,SY_),point(EX,EY_),_Working,Speed),[
%	type:line, strokeWidth: 2, strokeColor:black, from:[SX,SY], to:[EX, EY] ] ).
	type:Type, arrow:Speed, headLength:10, strokeWidth: 2, strokeColor:black, from:[RSX,RSY], to:[REX, REY]|Label ] ) :-
    SY is SY_-5, EY is EY_-5, % hack to draw our line a bit under the conveyed objects
    format(string(Sp),"~w px/cycle",[Speed]),
    (Speed=0 -> Type=line, RSX=SX, RSY=SY, REX=EX, REY=EY, label=[] ; 
    	Speed>0 -> Type=arrow, RSX=SX, RSY=SY, REX=EX, REY=EY, Label=[label:Sp] ;
    	Type=arrow, RSX=EX, RSY=EY, REX=SX, REY=SY, Label=[label:Sp] ).


%%% Container: something containing a quantity and a location, and that's it...but it is known to pumps/valves

fluents container(_ID,_Level).

createContainer(Name,Level,Ob) from T1 to T2 if
	Ob=container(Name), objectName(Name), number(Level),
	initiate container(Ob,Level) from T1 to T2,
	initiate location(Ob,point(0,0)) from T1 to T2.

container(C,Level,Where) at T if % visualisation helper
	container(C,Level) at T, location(C,Where) at T.

display(container(C,Level,point(X,Y)),[from:[X,Y], to:[RightX,RightY], label:(Name:Level), type:rectangle,  fontSize:13, fillColor:'#85bb65']) :-
    RightX is X+10, RightY is Y+Level, C=..[_,Name].

%%% Heater; transfers its temperature instantaneously to all heatable(Ob,_Temperature) objects inside its rectangle
% when they leave, their temperature is instantaneously reset to what it was prior to entering
% heatable objects should have a location(Ob,Point)

fluents heater(_ID,_BottomLeft,_TopRight,_Temperature), heatable(_ID,_Temperature), initialTemperature(_Heater,_Ob,_Temp).

createHeater(Name,BL,TR,InitialTemp,Ob) from T1 to T2 if
	Ob=heater(Name), objectName(Name),
	initiate heater(Ob,BL,TR,InitialTemp) from T1 to T2,
	initiate working(Ob,off) from T1 to T2.

if heater(H,BL,TR,_Temp) at T, working(H,on) at T, heatable(Ob,InitialTemp) at T, 
	location(Ob,L1) at T, \+ inside(L1,BL,TR), location(Ob,L2) at T+1, inside(L2,BL,TR)
then initiate initialTemperature(H,Ob,InitialTemp) from T+1.

if heater(H,BL,TR,Temp) at T, working(H,on) at T, heatable(Ob,_) at T, location(Ob,L) at T, inside(L,BL,TR)
then update Old to Temp in heatable(Ob,Old) from T.

if heater(H,BL,TR,_) at T, working(H,on) at T, heatable(Ob,Temp) at T, 
	location(Ob,L1) at T, inside(L1,BL,TR), location(Ob,L2) at T+1, \+ inside(L2,BL,TR), initialTemperature(H,Ob,InitialTemp)
then terminate initialTemperature(H,Ob,InitialTemp) from T+1, update Temp to InitialTemp in heatable(Ob,Temp) from T+1.

display(heater(ID,point(BLX,BLY),point(TRX,TRY),Temp),[
    type:rectangle,label:TS,from:[BLX,BLY],to:[TRX,TRY],strokeColor:red ]) :-
    format(string(TS),"~wo",[Temp]).

%%% Cookable; an object with a location that gets cooked when exposed to heat
% Doneness is a nonnegative number reflecting how well cooked the item is versus its initial state, 
% and depends on temperature (Celsius)
% This model could be improved, e.g. https://opentextbc.ca/physicstestbook2/chapter/temperature-change-and-heat-capacity/
fluents cookable(_ID,_InitialTemperature,_Doneness). 

createCookable(Name,InitialTemp,Ob) from T1 to T2 if
	Ob=cookable(Name), objectName(Name),
	initiate location(Ob,point(0,0)) from T1 to T2,
	initiate heatable(Ob,InitialTemp) from T1 to T2,
	initiate cookable(Ob,InitialTemp,0) from T1 to T2.

if cookable(Ob,Initial,D) at T, heatable(Ob,Current) at T, Current>Initial
then NewD is D+(Current-Initial)*0.01, update D to NewD in cookable(Ob,Initial,D).

cookable(Ob,InitialTemp,Doneness,Where) at T if % visualisation helper
	cookable(Ob,InitialTemp,Doneness) at T, location(Ob,Where) at T.

display(cookable(C,_,D,point(X,Y)),[type:circle,center:[X,Y], radius:5, label:Ds, fontSize:13, fillColor:red]) :- format(string(Ds),"~2f",[D]).


%%% Pump, a generic version of valve, with a flow and ways to impact it
% It has one input and one output, which must be containers

fluents pump(_ID,_Input,_Output), pumpFlow(_ID,_Flow). % flow is units per LPS cycle from Input to Output

createPump(Name,Input,Output,Ob) from T1 to T2 if 
	Input \= Output, Input=container(_), Output=container(_),
	Ob=pump(Name), objectName(Name),
	initiate pump(Ob,Input,Output) from T1 to T2,
	initiate pumpFlow(Ob,0) from T1 to T2,
	initiate working(Ob,off) from T1 to T2.

actions setPumpFlow(_Pump,_Flow), switchPumpOutputTo(_Pump,_Output), switchPumpInputTo(_Pump,_Input), pumpIt(_Pump,_Delta).
setPumpFlow(P,Flow) updates Old to Flow in pumpFlow(P,Old) if number(Flow).
switchPumpOutputTo(P,New) updates Old to New in pump(P,_I,Old) if New=container(_).
switchPumpInputTo(P,New) updates Old to New in pump(P,Old,_) if New=container(_).

if pump(P,I,O) at T, working(P,on) at T, container(I,InputLevel) at T
then
	pumpFlow(P,Flow) at T, InputLevel-Flow>=0, 
	% assuming this pump to "explode" (nuking the simulation) if no input available
	pumpIt(I,-Flow) from T, pumpIt(O,Flow) from T.

% This postcondition must be used, rather than a simple update "anonymous action", for serializability,
% e.g. integrating symetric output and input to/from each container in a pair
pumpIt(C,Delta) updates Old to New in container(C,Old) if New is Old+Delta.

pour(P, Quantity) from T1 to T3 if
	pump(P,Receptacle1, Receptacle2) at T1,
	working(P,off) at T1,
	container(Receptacle2, OldLevel) at T1, 
	container(Receptacle1, Supply) at T1, Supply >= Quantity,
	DesiredLevel is  Quantity + OldLevel,
	pumpFlow(P,R) at T1,
	StopLevel is DesiredLevel - R,
	start(P) from T1,
	container(Receptacle2, NewLevel) at T2, NewLevel>=StopLevel, % DO WE NEED TO LIMIT TIME to T1+Quantity/R...?
	stop(P) from T2 to T3.
	
%%% Dropper, a maker of objects from a given prototype, that places them on a conveyor at position 0
% a fractional speed will space droppings over cycles; displayed above the conveyor's start point

fluents dropper(_ID,_Speed,_PrototypeObject,_Conveyor,_LastDropAt).

createDropper(Name,Speed,Prototype,Conveyor,Ob) from T1 to T2 if
	Ob=dropper(Name), objectName(Name), number(Speed), Conveyor=conveyor(_),
	location(Conveyor,point(CX,CY)) at T1, Y is CY+25,
	initiate working(Ob,off) from T1 to T2,
	initiate location(Ob,point(CX,Y)) from T1 to T2,
	initiate dropper(Ob,Speed,Prototype,Conveyor,T1) from T1 to T2.

actions setDroppingSpeed(_Dropper,_Speed).
setDroppingSpeed(D,S) updates Old to S in dropper(D,Old,_,_,_) if number(S).

if dropper(D,Speed,Prototype,Conveyor,LastDrop) at T1, working(D,on) at T1, Speed*(T1-LastDrop)>=1
then 
	update Old to T1 in dropper(D,Speed,Prototype,Conveyor,Old) from T1 to T1+1,
	N is round(Speed*(T1-LastDrop)), 
	clone(N,Prototype,Clones) from T1 to T1+1, 
	drop(Clones,Conveyor) from T1+1 to T1+2.

drop([Ob|Objects],Conveyor) from T1 to T2 if
	placeOnConveyor(Ob,Conveyor,0) from T1 to T2,
	drop(Objects,Conveyor) from T1 to T2.
drop([],_).

dropper(D,Speed,Where) at T if % helper for displaying
	dropper(D,Speed,_Proto,_,_) at T, location(D,Where) at T.

display(dropper(dropper(Name),Speed,point(X,Y)),[
    [type:line,from:[X,Y],to:[TLX,TY],strokeColor:black], [type:line,from:[X,Y],to:[TRX,TY],strokeColor:black]
    ]) :- TLX is X-10, TY is Y+15, TRX is X+10.

%% geometry and other utilities

%display(timeless,Props) :- findall(P,timeless_element(P),Props). % TODO: some other scene elements?

% interpolate(+StartPoint,+EndPoint,+DistanceFromStart,-Point)
interpolate(point(SX,Y),point(EX,Y),D,point(X,Y)) :- !, (EX>=SX ->   X is SX+D ; X is SX-D).
interpolate(point(X,SY),point(X,EY),D,point(X,Y)) :- !, (EY>=SY ->   Y is SY+D ; Y is SY-D).
interpolate(point(SX,SY),point(EX,EY),D,point(X,Y)) :-
    M is (EY-SY)/(EX-SX),
    DX is D/sqrt(1+M*M), DY is DX*M, X is round(SX+DX), Y is round(SY+DY).
   
% newPosition(CurrentPoint,EndPoint,Delta,NewPoint)
newPosition(Current,_Start,End,Delta,NewPoint) :- Delta>=0, interpolate(Current,End,Delta,NewPoint).
newPosition(Current,Start,_End,Delta,NewPoint) :- Delta<0, interpolate(Current,Start,Delta,NewPoint).

distance(point(X1,Y1),point(X2,Y2),D) :- D is sqrt((X2-X1)*(X2-X1)+(Y2-Y1)*(Y2-Y1)).

% perpendicular_intersection(Start,End,To_Other,IntersectionPoint)
% Given a line and a point To, finds the intersection point in the line whose perpendicular passes by To
perpendicular_intersection(point(SX,_SY),point(SX,_EY),point(_TX,TY),point(SX,TY)) :- !.
perpendicular_intersection(point(_SX,SY),point(_EX,SY),point(TX,_TY),point(TX,SY)) :- !.
perpendicular_intersection(point(SX,SY),point(EX,EY),point(TX,TY),point(X,Y)) :-
    MC is (EY-SY)/(EX-SX), BC is SY-MC*SX,
    MP is -1/MC, BP is TY+TX/MC,
    X is (BC-BP)/(MP-MC), Y is BC+MC*X.
    
% inside(Point,BottomLeft,TopRight) whether a point is inside a rectangle defined by two points
inside(point(X,Y),point(BLX,BLY),point(TRX,TRY)) :- X>BLX,X<TRX,Y>BLY,Y<TRY.
    
objectName(Name) :- nonvar(Name) -> true ; gensym(object,Name).
