:- assert('$package_name'('shef.nlp.supple.prolog.cafe')).

:- compile('supple_utils.pl').

:- op(700, xfx, \=). 
X \= Y :- \+(X=Y).
not(Goal) :- \+(Goal).

:- op(900, fy, once).
once(Goal) :- call(Goal), !.

tell(File) :- atom(File),open(File,'write',_A),set_output(_A).
tell(Stream) :- set_output(Stream).

told :- current_output(_A), close(_A), set_output('user_output').
telling(Stream) :- current_output(Stream).

see(File) :- open(File,'read',_A), set_input(_A).
see(Stream) :- set_input(Stream).

seen :- current_input(_A), close(_A), set_input('user_input').

error(A,B) :- write('user_error',A).

member(Element, [Head|Tail]) :-
	member_(Tail, Head, Element).

% auxiliary to avoid choicepoint for last element
member_(_, Element, Element).
member_([Head|Tail], _, Element) :-
	member_(Tail, Head, Element).


memberchk(Element, [Element|_]) :- !.
memberchk(Element, [_|Rest]) :-
	memberchk(Element, Rest).

is_list(X) :- var(X), !, fail.
is_list([]).
is_list([_|Tail]) :- is_list(Tail).

lower(Text, Lower) :-
	atom(Text),
	atom_chars(Text, TextChars),
	lower_chars(TextChars, LowerChars),
	atom_chars(Lower, LowerChars).