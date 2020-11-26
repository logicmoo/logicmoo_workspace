:- module(interpret, [interpret/0,init/0,quit/0]).

:- use_module( library(tis_access), [check_conditions/1,
				     check_condition/1,
				     apply_update/1] ).
:-use_module(library(lists),[delete/3]).
:-use_module(library(charsio),[write_to_chars/3,read_from_chars/2]).

:- dynamic type/2.

init:-
	check_condition( $latest_moves = LM ),
	functor(LM, Type, _),
	assert( type(latest_moves, Type ) ),
	error:report(['Initialized interpret_simple']).
quit.

interpret:-
	check_condition($input=InputStr),
	interpret(InputStr,Moves).

interpret("",[]):- !.
interpret(InputStr,Moves):-
	check_conditions([$domain=Domain,$language=Lang]),
	gf_properties_file(Domain,PropFile),
	gf_grammar_file(usr,Domain,Lang,UsrNLGrammar),
	gf_grammar_file(usr,Domain,sem,UsrSemGrammar),
	atom_chars(InputAtom,InputStr),
	solve( translate(PropFile,
			 UsrNLGrammar,InputAtom,
			 UsrSemGrammar,MovesAtom)),
	atom2moves(MovesAtom,Moves).

atom2moves(MovesAtom,Moves):-
	write_to_chars(MovesAtom,Chars,[0'.]),
	remove_space(Chars,CharsNoSpace),
	
	map_underscore_to_space(CharsNoSpace,MovesStr),
	read_from_chars(Moves, MovesStr).

remove_space(Chars,CharsNoSpace):-
	delete(Chars,0' ,CharsNoSpace).


