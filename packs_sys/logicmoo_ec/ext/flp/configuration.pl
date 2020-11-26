:- dynamic option/2.

setOption(Option,Value) :-
	retractall(option(_,_)),
	assertz(option(Option,Value)).

:- setOption(iemConfiguration,1).

%% Goals = ['isReplete'(andrewDougherty)],
%% Goals = ['directly-holding'(andrewDougherty,bluetoothKeyboard)],
%% Goals = ['directly-holding'(meredithMcGhan,bluetoothKeyboard)],

iemConfiguration(1,[
		    currentPlanningCapsule('flp/flp'),
		    currentPlanner('LPG'),
		    currentPlanningGoals(['directly-holding'(andrewDougherty,bluetoothKeyboard)])
		   ]).
iemConfiguration(2,[
		    currentPlanningCapsule('mealplanning/caloriesingle/current/caloriesingle'),
		    currentPlanner('LPG'),
		    currentPlanningGoals(['isReplete'(andrewDougherty)])
		   ]).
iemConfiguration(3,[
		    currentPlanningCapsule('finance/current/tsimpleopticclp'),
		    currentPlanner('OPTIC_CLP'),
		    currentPlanningGoals([])
		   ]).

currentPlanningCapsule(Capsule) :-
	option(iemConfiguration,IEMConfiguration),
	iemConfiguration(IEMConfiguration,Arguments),
	argt(Arguments,[currentPlanningCapsule(Capsule)]).

currentPlanner(Planner) :-
	option(iemConfiguration,IEMConfiguration),
	iemConfiguration(IEMConfiguration,Arguments),
	argt(Arguments,[currentPlanner(Planner)]).

currentPlanningGoals(Goals) :-
	option(iemConfiguration,IEMConfiguration),
	iemConfiguration(IEMConfiguration,Arguments),
	argt(Arguments,[currentPlanningGoals(Goals)]).
