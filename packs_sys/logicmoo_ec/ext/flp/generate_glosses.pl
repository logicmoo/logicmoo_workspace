thingsToFix('generate_glosses.pl',
	    [
	     ['the andy should ...','Andy should ...']
	    ]).

%% [preconditions,['at start'(locked('$VAR'('Lo'))),'at start'(at('$VAR'('P'),'$VAR'('L'))),'at start'(at('$VAR'('Lo'),'$VAR'('L')))],effects,['at end'(not(locked('$VAR'('Lo')))),'at end'([assign(actions,op(+,actions,1))])]]

%% [pairs,[,[['$VAR'('Ob'),object],bookbag],[['$VAR'('Lo'),container],'laptop-backpack'],[['$VAR'('L'),location],'cathedral-of-learning']]]

%% :- convert_precondition('at start'(locked('$VAR'('Lo'))),[[['$VAR'('Lo'),container],'laptop-backpack']],Gloss),view([gloss,Gloss]).

%% convert_pregd('at start'(not(inaccessible('$VAR'('L0')))),[[['$VAR'('Ob0'),object],andy],[['$VAR'('L0'),location],'doherty-4201'],[['$VAR'('L1'),location],'cathedral-of-learning']],Temp).

preconditionNLGTemplates([
			  %% ['arm',[[A,must,be,P],
			  %% 	  [A,will,be,P],
			  %% 	  [A,must,be,P,for,the,duration,of,the,action]]],
			 ]).

%% replace this to be a proper noun, for instance, 'The chicago' is incorrect. 
getDeterminerAndGloss(A,DT,AG) :-
	(   (	isa(A,person)) ->
	    (	DT = '') ;
	    (	DT = the )),
	hasEnglishGlosses(A,Tmp),
	Tmp =.. [_,AG|_].

formatSentence(Template,Sentence) :-
	findall(Item,(member(Item,Template),not(Item = '')),[First|Rest]),
	capitalize(First,FirstCapitalized),
	Sentence = [FirstCapitalized|Rest].

get_sentence_for_precondition_predicate(PTmp,ObjectNames,Condition,Sentence) :-
	(   (	PTmp = location) ->
	    (	P = at) ;
	    (	P = PTmp)),
	view([get_sentence_for_precondition_predicate(P,ObjectNames,Condition,Sentence)]),
	preconditionNLGTemplates(List),
	kmax_get_index_of_first_item_in_list(Condition,['at start','at end','over all'],Index),
	view([index,Index]),
	(   member([P,Templates],List) ->
	    (	nth1(Index,Templates,Template)) ; 
	    (	view([condition,Condition,index,Index,templates,Templates,objectnames,ObjectNames]), 
		(   
		    ObjectNames = [A] ->
		    (	
			getDeterminerAndGloss(A,DT1,AG),
			nth1(Index,[[DT1,AG,must,be,P],
				    [DT1,AG,will,be,P],
				    [DT1,AG,must,be,P,for,the,duration,of,the,action]],
			     Template)) ;
		    (	ObjectNames = [A,B] ->
			(   
			    getDeterminerAndGloss(A,DT1,AG),
			    getDeterminerAndGloss(B,DT2,BG),
			    nth1(Index,[[DT1,AG,must,be,P,DT2,BG],
					[DT1,AG,will,be,P,DT2,BG],
					[DT1,AG,must,be,P,DT2,BG,for,the,duration,of,the,action]],
				 Template)) ;
			(   ObjectNames = [A,B,C] ->
			    (	
				getDeterminerAndGloss(A,DT1,AG),
				nth1(Index,[[DT1,AG,must,be,P,B,C],
					    [DT1,AG,will,be,P,B,C],
					    [DT1,AG,must,be,P,B,C,for,the,duration,of,the,action]],
				     Template)) ;
			    (	ObjectNames = [A,B,C,D] ->
				(   
				    getDeterminerAndGloss(A,DT1,AG),
				    nth1(Index,[[DT1,AG,must,be,P,B,C,D],
						[DT1,AG,will,be,P,B,C,D],
						[DT1,AG,must,be,P,B,C,D,for,the,duration,of,the,action]],
					 Template)) ;
				(   ObjectNames = [A,B,C,D,E] ->
				    (	
					getDeterminerAndGloss(A,DT1,AG),
					nth1(Index,[[DT1,AG,must,be,P,B,C,D,E],
						    [DT1,AG,will,be,P,B,C,D,E],
						    [DT1,AG,must,be,P,B,C,D,E,for,the,duration,of,the,action]],
					     Template)) ; fail) ; fail) ; fail) ; fail) ; fail))),
	formatSentence(Template,Sentence).

get_sentence_for_precondition_predicate(P,ObjectNames,Condition,Sentence) :-
	view([get_sentence_for_precondition_predicate(P,ObjectNames,Condition,Sentence)]),
	preconditionNLGTemplates(List),
	kmax_get_index_of_first_item_in_list(Condition,['at start','at end','over all'],Index),
	view([index,Index]),
	(   member([P,Templates],List) ->
	    (	nth1(Index,Templates,Template)) ; 
	    (	view([condition,Condition,index,Index,templates,Templates,objectnames,ObjectNames]), 
		(   
		    ObjectNames = [A] ->
		    (	
			getDeterminerAndGloss(A,DT1,AG),
			nth1(Index,[[DT1,AG,must,be,P],
				    [DT1,AG,will,be,P],
				    [DT1,AG,must,be,P,for,the,duration,of,the,action]],
			     Template)) ;
		    (	ObjectNames = [A,B] ->
			(   
			    getDeterminerAndGloss(A,DT1,AG),
			    getDeterminerAndGloss(B,DT2,BG),
			    nth1(Index,[[DT1,AG,must,be,P,DT2,BG],
					[DT1,AG,will,be,P,DT2,BG],
					[DT1,AG,must,be,P,DT2,BG,for,the,duration,of,the,action]],
				 Template)) ;
			(   ObjectNames = [A,B,C] ->
			    (	
				getDeterminerAndGloss(A,DT1,AG),
				nth1(Index,[[DT1,AG,must,be,P,B,C],
					    [DT1,AG,will,be,P,B,C],
					    [DT1,AG,must,be,P,B,C,for,the,duration,of,the,action]],
				     Template)) ;
			    (	ObjectNames = [A,B,C,D] ->
				(   
				    getDeterminerAndGloss(A,DT1,AG),
				    nth1(Index,[[DT1,AG,must,be,P,B,C,D],
						[DT1,AG,will,be,P,B,C,D],
						[DT1,AG,must,be,P,B,C,D,for,the,duration,of,the,action]],
					 Template)) ;
				(   ObjectNames = [A,B,C,D,E] ->
				    (	
					getDeterminerAndGloss(A,DT1,AG),
					nth1(Index,[[DT1,AG,must,be,P,B,C,D,E],
						    [DT1,AG,will,be,P,B,C,D,E],
						    [DT1,AG,must,be,P,B,C,D,E,for,the,duration,of,the,action]],
					     Template)) ; fail) ; fail) ; fail) ; fail) ; fail))),
	formatSentence(Template,Sentence).

get_sentence_for_precondition_predicate(P,ObjectNames,Condition,Sentence).


convert_pregd(Item,Pairs,Gloss) :-
	nl,
	length(Pairs,Length),
	view([pairs,Pairs]),
	(   Item =.. [Condition,not(PreGD)] -> (Concat = '*not* ') ;
	    (	Item =.. [Condition,PreGD] -> (Concat = '') ; true )),
	view([preGd,PreGD]),
	PreGD =.. [P|A],
	view([p,P,a,A]),
	findall(ObjectName,(member(Pair,Pairs),member('$VAR'(VarName),A),Pair = [['$VAR'(VarName),_],ObjectName]),ObjectNames),
	get_sentence_for_precondition_predicate(P,ObjectNames,Condition,Sentence),
	view([sentence,Sentence]),
	(   nonvar(Sentence) -> atomic_list_concat(Sentence,' ',Gloss) ; (print_to_atom(PreGD,Gloss),view([glossBaby,Gloss]))),
	!.

%% convert_pregd(Item,Pairs,Gloss) :-
%% 	length(Pairs,Length),
%% 	view([pairs,Pairs]),
%% 	(   Item =.. [Condition,not(PreGD)] -> (Concat = '*not* ') ;
%% 	    (	Item =.. [Condition,PreGD] -> (Concat = '') ; true )),
%% 	view([preGd,PreGD]),
%% 	PreGD =.. [P|A],
%% 	length(A,1),
%% 	A = ['$VAR'(VarName)],
%% 	findall(ObjectName,(member(Pair,Pairs),Pair = [['$VAR'(VarName),_],ObjectName]),ObjectNames),
%% 	ObjectNames = [NewObjectName],
%% 	(   Condition = 'at start' ->
%% 	    Sentence = [NewObjectName,must,be,P] ;
%% 	    (	Condition = 'at end' ->
%% 		Sentence = [NewObjectName,will,be,P] ;
%% 		(   Condition = 'over all' ->
%% 		    Sentence = [NewObjectName,must,be,Concat,P,for,the,duration,of,the,action] ;
%% 		    fail ) ) ),
%% 	atomic_list_concat(Sentence,' ',Gloss),
%% 	view([gloss,Gloss]),!.

%% convert_pregd(Item,Pairs,Gloss) :-
%% 	view([pairs,Pairs]),
%% 	Item =.. [Condition,PreGD],
%% 	view([preGd,PreGD]),
%% 	PreGD =.. [P|A],
%% 	length(A,1),
%% 	A = ['$VAR'(VarName)],
%% 	findall(ObjectName,(member(Pair,Pairs),Pair = [['$VAR'(VarName),_],ObjectName]),ObjectNames),
%% 	ObjectNames = [NewObjectName],
%% 	(   Condition = 'at start' ->
%% 	    Sentence = [NewObjectName,must,be,P] ;
%% 	    (	Condition = 'at end' ->
%% 		Sentence = [NewObjectName,will,be,P] ;
%% 		(   Condition = 'over all' ->
%% 		    Sentence = [NewObjectName,must,be,P,for,the,duration,of,the,action] ;
%% 		    fail ) ) ),
%% 	atomic_list_concat(Sentence,' ',Gloss),
%% 	view([gloss,Gloss]),!.

convert_pregd(Statement,Pairs,Gloss) :-
	with_output_to(atom(Gloss),write_term(Statement,[quoted(true)])),!.
	
%%   (locked ?lo - lockable-container)

%% the Laptop Backpack must be *not* locked
%% Andrew Dougherty must be in the Den
%% the Laptop Backpack must be in the Den

%% the Laptop Backpack will be locked


%%  (:types 
%%   object location outlet stuff tool
%%   person - object
%%   battery-powered-device - object
%%   electric-razor - battery-powered-device
%%   collection - object
%%   meals - collection
%%   office - location
%%   building - location
%%   store - building
%%   headset - object
%%   container - object
%%   lockable-container - container
%%   locker - lockable-container
%%   bag - container
%%   tool - object
%%   hygiene-tool - tool
%%   laptop - battery-powered-device
%%   )

%%  (:predicates
%%   (socially-acceptable ?p - person)
%%   (isolated ?l - location)
%%   (inaccessible ?l - location)
%%   (at ?ob - object ?l - location)
%%   (autonomous ?ob - object)
%%   (mobile ?ob - object)
%%   (holding ?ob0 ?ob1 - object)
%%   (is-contained-by ?ob1 - object ?c - container)
%%   (plugged-in ?l - laptop)
%%   (all-pending-work-accomplished ?p - person)
%%   (shaved ?p - person)
%%   (showered ?p - person)
%%   (clean ?la - laundry)
%%   (wet ?la - laundry)
%%   (tired ?p - person)
%%   (hungry ?p - person)
%%   (locked ?lo - lockable-container)
%%   (use-is-required ?t - tool)
%%   (ship-shape)
%%   )

%%  (:functions
%%   (actions)
%%   (quantity ?c - collection)
%%   (hourly-wage-net ?p - person)
%%   (total-walking-distance)
%%   (charge-rate ?r - battery-powered-device)
%%   (charge-level ?r - battery-powered-device)
%%   (speed ?ob - object)
%%   (cash ?p - person)
%%   )

%% (:objects
%%   andy justin - person
%%   electric-razor0 - electric-razor
%%   laundry - laundry
%%   towel - towel
%%   basement-laundry-machines forbes-ave-laundromat - laundromat
%%   food-store - meals
%%   sleeping-bag - bed
%%   hidden-sleeping-spot - bed
%%   outlet0 - outlet
%%   UC-mens-locker-room-shower - shower
%%   IBM-R30 - laptop
%%   bookbag laptop-backpack duffel-bag - container
%%   doherty-4201 casos-office - office
%%   doherty-hall wean-hall baker-hall fouroseven-craig-st-hall - building
%%   flagstaff-hill cs-lounge wean-hall-bathroom doherty-hall-bathroom - location
%%   UC-gym forbes-and-chesterfield cathedral-of-learning - location
%%   baker-locker-18 baker-locker-67 baker-locker-69 doherty-locker-161 - locker
%%   doherty-locker-1 doherty-locker-4 doherty-locker-20 - locker


%%   squirrel-hill-giant-eagle oakland-giant-eagle greenville-giant-eagle - store
%%   water-front-giant-eagle airport-walmart indiana-walmart - store
%%   forbes-ave-cvs - store
%%   finger-clippers towel shampoo - hygiene-tool
%%   hair-trimmers food-cans shirts can-opener hair-brush - stuff
%%   padlock shampoo headset wallet sleeping-bag camouflage - stuff

%%   svrs-1 - location
%%   )
