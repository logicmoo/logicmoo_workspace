
:- expects_dialect(lps).


:- include(example('simulation/SimulationKit.pl')).

% "if true at T then ..." is a way to "observe" (e.g. request to perform) a composite event/action
% test pump
/*if true at 2 then
	createContainer(bob,100,Bob), createContainer(fariba,200,Fariba),
	createPump(payments,Bob,Fariba,P), setPumpFlow(Pump,20) to T, start(Pump) to T,
	stop(Pump) from T+3. % ... otherwise the program would fail, to protect the pump! */

% a sliding variation on the bank transfer example; all conveyed upwards until Fariba's funding ends
/* if true at 2 then
	createContainer(bob,50,Bob), createContainer(fariba,100,Fariba), createContainer(miguel,10,Miguel) to TC,
	createPump(payments,Fariba,Bob,Pump), 
	createCookable(shrimp,20,Shrimp),
	createHeater(_,point(0,0),point(300,200),90,Heater), start(Heater),
	createConveyor(_SomeName,point(10,10),point(250,100),Conveyor) to T0, 
	createDropper(_,0.25,Shrimp,Conveyor,Dropper) from T0 to T1, 
	% T0 is needed, otherwise Bob may backtrack prematurely! 
	% Truth pursuing can be rather subtle, when in doubt make time explicit!
	placeOnConveyor(Shrimp,Conveyor,220) from T0 to T1,
	placeOnConveyor(Bob,Conveyor,0) from T0 to T1, 
	placeOnConveyor(Fariba,Conveyor,100) from T0 to T1, 
	placeOnConveyor(Miguel,Conveyor,200) from T0 to T1, 
	start(Dropper) from T1 to T2,
	start(Conveyor) from T1 to T2, setConveyorSpeed(Conveyor,10) from T1 to T2,
	setPumpFlow(Pump,10) from T2, start(Pump) from T2,
	container(Fariba,Left) at T3, Left=<20,
	setPumpFlow(Pump,-10) from T3, switchPumpInputTo(Pump,Miguel) from T3,
	% stop(Pump) from T3, % ... otherwise the program would fail, to protect the pump!
	setConveyorSpeed(Conveyor,-10) from T3. */

if true at 1 then
	createContainer(hotOil,50,Hot) from T1 to T2, 
	createContainer(usedOil,5,Used) from T1 to T2, 
	createContainer(newOil,30,New) from T1 to T2, 
	setLocation(Hot,point(300,200)) from T2 to T3, setLocation(Used,point(200,0)) from T2 to T3,
	setLocation(New,point(200,80)) from T2 to T3,
	createPump(outward,Hot,Used,Outward) from T2 to T3, createPump(inward,Used,Hot,Inward) from T2 to T3,
	setPumpFlow(Outward,1) from T3, setPumpFlow(Inward,1) from T3, start(Outward) from T3, start(Inward) from T3 to T4,
	createConveyor(feeding,point(550,140),point(50,140),Feeding) from T4 to T5, 
	setConveyorSpeed(Feeding,10) from T5 to T6, start(Feeding) from T5 to T6,
	createHeater(_,point(100,75),point(500,290),150,Heater) from T6 to T7, start(Heater) from T7,
	createCookable(shrimp,4,ProtoShrimp) from T7 to T8,
	setLocation(ProtoShrimp,point(-20,-20)) from T8, % hack to hide prototype
	createDropper(_,0.25,ProtoShrimp,Feeding,Dropper) from T8 to T9,
	start(Dropper) from T9.
	
%%% KitchenSummary; a singleton object aggregating some useful statistics
:- use_module(library(lists)).
kitchenSummary(CookableCount,AvgDoneness ,Min,Max) at _ if
	findall(D,cookable(_,_,D),L), length(L,CookableCount), CookableCount>0,
	sum_list(L,Total), AvgDoneness is Total/CookableCount, 
	min_list(L,Min), max_list(L,Max).

display(kitchenSummary(Count,Avg,Min,Max),[
    from:[400,300], to:[600,350], label:S, type:rectangle,  fillColor:salmon]) :- 
	format(string(S),"~w items, ~ndoneness ~2f (~2f-~2f)",[Count,Avg,Min,Max]).
    
display(timeless, [[type:rectangle,from:[0,0],to:[600,350],strokeColor:green]]). % bounds for our display

maxTime(100). % minCycleTime(0).
/** <examples> 
?- go(Timeline).
*/
