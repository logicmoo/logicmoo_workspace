:- multifile generation_grammar/4.

%RESPONSES TO INTERACTION WITH FOUND OBJECT COMMANDS

generation_grammar(german, [rotate, Object]) -->
	['We are turning toward'],
	generation_grammar(german, Object).

	
generation_grammar(german, [surround, Object]) -->
	['We are surrounding'],
	generation_grammar(german, Object).
	
generation_grammar(german, [circle, Object]) -->
	['We are surrounding'],
	generation_grammar(german, Object).
	
generation_grammar(german, [guard, Object]) -->
	['Wir bewachen'],
	generation_grammar(german, Object).
	
generation_grammar(german, [rescue, Object]) -->
	['We are rescuing'],
	generation_grammar(german, Object).
	
generation_grammar(german, [stalk, Object]) -->
	['We are stalking'],
	generation_grammar(german, Object).



generation_grammar(german, [reset]) -->
	['Wir gehen nach hause'].

	

%RESPONSES TO MAKING SHAPES

generation_grammar(german, [make,Shape]) -->
	['Wir stellen sich in'],
	generation_grammar(german, Shape).



%RESPONSES TO MOTION COMMANDS

generation_grammar(german, [rotate,Direction]) -->
	['Wir biegen'],
	generation_grammar(german, Direction),
  ['ab'].

generation_grammar(german, [move, Direction]) -->
	['Wir gehen nach'],
	generation_grammar(german, Direction).
	
generation_grammar(german, [move, Cardinal]) -->
	['Wir gehen nach'],
	generation_grammar(german, Cardinal).	


generation_grammar(german, [move_out]) -->
	['Spreading out'].

generation_grammar(german, [move_in]) -->
	['Coming together'].

generation_grammar(german, [stop]) -->
	['Wir halten'].

%RESPONSES
generation_grammar(german, [fire,Weapon]) -->
	['Wir feueren'],
  generation_grammar(german, Weapon).





/*
generation_grammar(german, [attack_formation_delta]) -->
	['all your base are belong to us'].
*/

generation_grammar(german, ambiguous) --> 
  ['Es tut mir leid. Das war zweideutig.'].








generation_grammar(german, friend) --> ['a friend'].
generation_grammar(german, human) --> ['einen menschen'].
generation_grammar(german, victim) --> ['a victim'].


generation_grammar(german, dev(lost)) --> ['lost'].
generation_grammar(german, dev(found)) --> ['found'].

generation_grammar(german, triangle) -->['ein dreieck'].
generation_grammar(german, delta) -->['attack formation delta'].

generation_grammar(german, iso_triangle) -->['an isosceles triangle'].
generation_grammar(german, equi_triangle) -->['an equilateral triangle'].
generation_grammar(german, circle) -->['a circle'].
generation_grammar(german, right_triangle) -->['a right triangle'].
generation_grammar(german, trick) --> ['einen trick'].
generation_grammar(german, delta) --> ['attack formation delta'].
generation_grammar(german, laser) --> ['laser'].

generation_grammar(german, south) --> ['suden'].
generation_grammar(german, north) --> ['norden']. 
generation_grammar(german, east) --> ['osten'].
generation_grammar(german, west) --> ['westen'].
%generation_grammar(german, northeast) --> ['northeast'].
%generation_grammar(german, northwest) --> ['northwest'].
%generation_grammar(german, southeast) --> ['southeast'].
%generation_grammar(german, southwest) --> ['southwest'].

%generation_grammar(german, forward) --> ['forward'].
%generation_grammar(german, backward) --> ['backward'].

generation_grammar(german, left) --> ['links'].
generation_grammar(german, right) --> ['rechts'].
generation_grammar(german, around) --> ['herum'].
generation_grammar(german, backward) --> ['hinten']
