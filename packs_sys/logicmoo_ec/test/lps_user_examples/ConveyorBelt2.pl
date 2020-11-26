:- expects_dialect(lps).

% Example for https://www.youtube.com/watch?v=5ms_idvN8Tg
maxTime(60).
fluents 
	contents(_Object,_Units), empty/1, location(_What,_Where).
initially normalOperation, 
	contents(bottle, 0), contents(container, 0), contents(tank1, 200), contents(tank2, 200),
	location(bottle, 0), location(tank1, 25), location(tank2, 65), location(container, 100).
actions pourChunk(_From,_To), turnConveyor(_Direction).

%TODO: This and the following macroactions are currently specific to bottles, but they should probably be generalised
makeLocation(bottle, Station) from T1 to T2 if 
	location(bottle, Place1) at T1, location(Station, Place2) at T1, 
	Vector is Place2 - Place1, 
	moveBottle(Vector,Place2) from T1 to T2.

moveBottle(Vector,Desired) from T to T if
	Vector>=0, location(bottle,L) at T, L>=Desired.
moveBottle(Vector,Desired) from T to T if
	Vector<0, location(bottle,L) at T, L=<Desired.
moveBottle(Vector,Desired) from T1 to T3 if 
	Vector > 0, location(bottle,L) at T1, L<Desired,
	turnConveyor(clockwise) from T1 to T2,
	moveBottle(Vector,Desired) from T2 to T3.
moveBottle(Vector,Desired) from T1 to T3 if 
	Vector < 0, location(bottle,L) at T1, L>Desired,
	turnConveyor(counterClockwise) from T1 to T2,
	moveBottle(Vector,Desired) from T2 to T3.

turnConveyor(counterClockwise) updates Place to NewPlace in location(bottle, Place) if
	conveyorSpeed(S), NewPlace is Place-S.
turnConveyor(clockwise) updates Place to NewPlace in location(bottle, Place) if
	conveyorSpeed(S), NewPlace is Place+S.

conveyorSpeed(5). % our conveyor moves this distance per LPS cycle

empty(Receptacle) if contents(Receptacle, 0).

pourChunk(_, Receptacle2) updates Old to New in contents(Receptacle2, Old) if 
	valveRate(Quantity), New is Old + Quantity.
pourChunk(Receptacle1, _) updates Old to New in contents(Receptacle1, Old) if 
	valveRate(Quantity), New is Old - Quantity.

valveRate(10). % our receptacle's output valves debit quantity of liquid per LPS cycle

pour(From,To,Qty) from T1 to T2 if
	contents(From,InitialLevel) at T1, Desired is InitialLevel - Qty,
	pourUntil(From,To,Desired) from T1 to T2.

pourUntil(From,_,Desired) from T to T if 
	contents(From,X) at T, X=<Desired.
pourUntil(From,To,Desired) from T1 to T2 if
	contents(From,X) at T1, X>Desired,
	pourChunk(From,To) from T1 to T,
	pourUntil(From,To,Desired) from T to T2.

dump(From,To) from T1 to T2 if 
	contents(From,X) at T1, pour(From,To,X) from T1 to T2.

if empty(bottle) at T1, location(bottle,0) at T1
then  
	makeLocation(bottle, tank1) from T1 to T3, pour(tank1, bottle, 50) from T3 to T4,
	makeLocation(bottle, tank2) from T4 to T5, pour(tank2, bottle, 50) from T5 to T6, 
	makeLocation(bottle, container) from T6 to T7, dump(bottle,container) from T7 to T8, 
	makeLocation(bottle, tank1) from T8.

% helper intensional fluent for 2D display; you need to use the sample(..) option to force LPS to remember it
locatedContents(Item,Location,Level) at T if 
	location(Item,Location) at T, contents(Item,Level) at T.

d(locatedContents(bottle,Location,Level),[Props,[type:rectangle,fillColor:blue,from:[X1,Y1],to:[X2,Y]]]) :- 
    % objects are drawn i order, so we put the liquid rectangle second so it appears
    d_(location(bottle,Location),Props),
    member(from:[X1,Y1],Props), member(to:[X2,Y2],Props),
    Yrange is Y2-Y1, Y is round(Level/100*Yrange+Y1).

d_(location(bottle,Pos),[type:rectangle,fillColor:yellow, 
    from:[X1,100], to:[X2,150], strokeColor:blue]) :- 
    locationToPixels(Pos,X1), X2 is X1+10.

locationToPixels(L,P) :- P is 25+L*4. % assumes the frame rectangle in d(timeless,...)

d(timeless,[
             [type:rectangle, from:[0,0], to:[450,300]], % frame for our whole scene
			 [type:line, strokeWidth: 2, strokeColor:black, from:[Start,100], to:[End, 100]],
             [type:circle, strokeWidth: 2, strokeColor:black, center:[Start,80], radius:20],
			 [type:circle, strokeWidth: 2, strokeColor:black, center:[End,80], radius:20], 
			[type:ellipse, fillColor:white, from:[Tank1Left,200], to:[Tank1Right,300], strokeColor:blue],
			[type:ellipse, fillColor:white, from:[Tank2Left,200], to:[Tank2Right,300], strokeColor:blue],
			 [type:line, strokeWidth: 2, strokeColor:black, from:[Start,60], to:[End, 60]] ]) :-
    locationToPixels(0,Start), locationToPixels(100,End), 
    locationToPixels(25,Tank1Left), Tank1Right is Tank1Left+30, 
    locationToPixels(65,Tank2Left), Tank2Right is Tank2Left+30.

/** <examples>
?- go(Timeline,[sample([locatedContents(_,_,_)])]).
?- go.
*/

