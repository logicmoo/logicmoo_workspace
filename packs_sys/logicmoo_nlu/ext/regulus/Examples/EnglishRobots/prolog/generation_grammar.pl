
:- multifile generation_grammar/4.

%RESPONSES TO INTERACTION WITH FOUND OBJECT COMMANDS

generation_grammar(english, [rotate, Object]) -->
	['We are turning toward'],
	generation_grammar(english, Object).

	
generation_grammar(english, [surround, Object]) -->
	['We are surrounding'],
	generation_grammar(english, Object).
	
generation_grammar(english, [circle, Object]) -->
	['We are surrounding'],
	generation_grammar(english, Object).
	
generation_grammar(english, [guard, Object]) -->
	['We are guarding'],
	generation_grammar(english, Object).
	
generation_grammar(english, [rescue, Object]) -->
	['We are rescuing'],
	generation_grammar(english, Object).
	
generation_grammar(english, [stalk, Object]) -->
	['We are stalking'],
	generation_grammar(english, Object).



generation_grammar(english, [reset]) -->
	['We are going home'].

	

%RESPONSES TO MAKING SHAPES

generation_grammar(english, [make,Shape]) -->
	['We are making'],
	generation_grammar(english, Shape).



%RESPONSES TO MOTION COMMANDS

generation_grammar(english, [rotate,Direction]) -->
	['We turned'],
	generation_grammar(english, Direction).

generation_grammar(english, [move, Direction]) -->
	['Moving'],
	generation_grammar(english, Direction).
	
generation_grammar(english, [move, Cardinal]) -->
	['Moving'],
	generation_grammar(english, Cardinal).	


generation_grammar(english, [move_out]) -->
	['Spreading out'].

generation_grammar(english, [move_in]) -->
	['Coming together'].

generation_grammar(english, [stop]) -->
	['stop'].

%RESPONSES
generation_grammar(english, [fire,Weapon]) -->
	['firing'],
  generation_grammar(english, Weapon).





/*
generation_grammar(english, [attack_formation_delta]) -->
	['all your base are belong to us'].
*/

generation_grammar(english, ambiguous) --> 
  ['sorry, that\'s ambiguous'].

generation_grammar(english, unable_to_interpret) --> 
  ['sorry, I don\'t understand'].








generation_grammar(english, friend) --> ['a friend'].
generation_grammar(english, human) --> ['a human'].
generation_grammar(english, victim) --> ['a victim'].


generation_grammar(english, dev(lost)) --> ['lost'].
generation_grammar(english, dev(found)) --> ['found'].

generation_grammar(english, triangle) -->['a triangle'].
generation_grammar(english, delta) -->['attack formation delta'].

generation_grammar(english, iso_triangle) -->['an isosceles triangle'].
generation_grammar(english, equi_triangle) -->['an equilateral triangle'].
generation_grammar(english, circle) -->['a circle'].
generation_grammar(english, right_triangle) -->['a right triangle'].
generation_grammar(english, trick) --> ['a trick'].
generation_grammar(english, delta) --> ['attack formation delta'].
generation_grammar(english, laser) --> ['lasers'].

generation_grammar(english, south) --> ['south'].
generation_grammar(english, north) --> ['north']. 
generation_grammar(english, east) --> ['east'].
generation_grammar(english, west) --> ['west'].
%generation_grammar(english, northeast) --> ['northeast'].
%generation_grammar(english, northwest) --> ['northwest'].
%generation_grammar(english, southeast) --> ['southeast'].
%generation_grammar(english, southwest) --> ['southwest'].

generation_grammar(english, forward) --> ['forward'].
generation_grammar(english, back) --> ['backward'].

generation_grammar(english, left) --> ['left'].
generation_grammar(english, right) --> ['right'].
generation_grammar(english, around) --> ['around'].

