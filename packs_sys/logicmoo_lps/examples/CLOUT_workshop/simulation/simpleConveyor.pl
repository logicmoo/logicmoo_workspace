
:- expects_dialect(lps).

% Based on https://www.youtube.com/watch?v=5ms_idvN8Tg 

%:- include(example('SimulationKit.pl')).
:- include(('./SimulationKit.pl')).

% Simple conveyor revisited:
if true at 1 then
	createContainer(tank1,100,Tank1) from 1 to T0, createContainer(tank2,100,Tank2) from 1 to T0, 
	createContainer(nowhere,0,Nothing) from 1 to T0, % hack for disconnected pump outputs
	createConveyor(simple,point(10,10),point(210,10),Conveyor) from 1 to T0, 
	createContainer(bottle,0,Bottle) from 1 to T0, 
	setConveyorSpeed(Conveyor,5) from T0 to T1,
	placeOnConveyor(Bottle,Conveyor,0) from T0 to T1, 
	setLocation(Tank1,point(50,150)) from T1 to T2, setLocation(Tank2,point(150,150)) from T1 to T2,
	createPump(tank1output,Tank1,Nothing,Tank1Out) from T1 to T2,
	createPump(tank2output,Tank2,Nothing,Tank2Out) from T1 to T2,
	createPump(bottleOutput,Bottle,Nothing,BottleOut) from T1 to T2,
	switchPumpOutputTo(Tank1Out,Bottle) from T2 to T3, setPumpFlow(Tank1Out,10) from T2 to T3,
	switchPumpOutputTo(Tank2Out,Bottle) from T2 to T3, setPumpFlow(Tank2Out,5) from T2 to T3,
	makeLocation(Conveyor,Bottle,Tank1) from T3 to T4,
	pour(Tank1Out,30) from T4 to T5 /*, % BUGGY!!!!
	makeLocation(Conveyor,Bottle,Tank2) from T5 to T6,
	pour(Tank2Out,60) from T6*/.

/*
 from T1, 
	start(Conveyor) from T1 to T2, setConveyorSpeed(Conveyor,5) from T1 to T2
	
pour(From,To,Qty)  implies atarting/stopping the pump! and a given rate/flow
a pump method
start, check after a while, stop
is level sampling expensive?

makeLocation(bottle, tank2) 
a conveyor method...

 */
    
display(timeless, [[type:rectangle,from:[0,0],to:[600,350],strokeColor:green]]). % bounds for our display

maxTime(70). % minCycleTime(0).
/** <examples> 
?- go(Timeline).
*/
