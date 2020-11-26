% earley.pl
% An Earley-Style Active Chart Parser
% Matt Voss
%	Based on programs by Michael Covington
% 	in his book Natural Language Processing 
%	for Prolog Programmers
% 	05/07/04
%
%
% Tested using SWI Prolog 5.2.13
% Availible at http://www.swi-prolog.org/
%
%
% Requires a grammar of rule/2 and word/2 clauses
% Ways to use these are discussed in the documentation
%

:- ensure_loaded('subsumes.pl').

% :- unknown(_,fail).

:- dynamic chart/4.
:- dynamic c/3.
:- dynamic l/2.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A MODIFIED EARLEY-STYLE PARSER % 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




% parse(+Constituent,+Sentence)
%
% 	Constituent is the type of constituent 
%     we are looking for. Sentence is a list of words.
% 	traverse the list in Sentence and succeed if
%     we have a Constituent at the end of the traversal.
% 	this is the main engine of the parser.
parse(C,S1) :-
	clear_chart,		% Clear out the previous chart
	convert(S1,0,End),	% Convert the input list into predicates that give the positions of the words
	get_links,			% Make links between the rules
	store(chart(0,0,start,[C])),
	process(0,End),
	chart(End,0,C,[]).



% process(+Position,-End)
% 
%	Start at the current position in the input string.
%     Run through the predictor, scanner, and completer until
%     you reach the end of the input list.
%	Return the index of the end of the input list.
process(End,End) :- !.


process(Position,End) :-
	predictor(Position),
	scanner(Position,NewPosition),
	completer(NewPosition),
	process(NewPosition,End).


% predictor(+Position)
%
%	Position is the current position
% 	of the parser in the input string.
%	use the chart to find a goal at the 
% 	current position in the input string.  
%	find the word corresponding to that 
%	position, and use it to make predictions
%	about which rules will be used to 
% 	parse the current goals.  
predictor(Position) :-
	chart(Position,_,_,[Goal|_]),
	c(Position,_,W),
	predict(W,Goal,Position),
	fail.

predictor(_).



% predict(+Word,+Category,+Position)
%	
%	Word is the word at the current
% 	position in the input stream.  
%	Category is the lexical category of
%	Word.  Position is the position of
%	that word in the input stream.  
%	Use the word beginning at the current
% 	position in the input stream to make 
%	predictions about what rules will be required 
% 	to parse the current goals.
predict(W,Cat,_) :-
	word(Cat,W),!.

predict(W,Goal1,Position) :-
	word(Cat,W),
	predict_all(Goal1,Cat,Position),
	predict(W,Goal1,Position),
	fail.

predict(_,_,_).


% predict_al(+Goal,+Cat,+Position)
%  
%	For the current goal and the current subgoal and
% 	current position, assert chart entries for all subgoals
%     from the input word to the main goal.
% 	Also back up one step and assert a chart entry
% 	for a null constituent if the current word is not
%	the first subgoal of any rule
predict_all(Goal,Goal,_).

% Get all of the regular rules taken care of
predict_all(Goal1,Cat,Position) :-
	rule(Goal,[Cat|T]),
	l(Goal,Goal1),               % predicates like this are the links
	store(chart(Position,Position,Goal,[Cat|T])),
	predict_all(Goal1,Goal,Position).

% And also do the null constituents
predict_all(Goal1,Cat,Position) :-
	rule(Goal,[D,Cat|T]),
	rule(D,[]),
	l(Goal,Goal1),
	store(chart(Position,Position,Goal,[D,Cat|T])),
	predict_all(Goal1,Goal,Position),
	store(chart(D,Position,[],Position)),
	complete(D,Position,Position).
	


% scanner(+Before,-After)
%
%	Accept a word and update the
% 	chart appropriately
scanner(End,End2) :-
	chart(End,Start,C,[G|Goals]),
	c(End,End2,Word),
	word(G,Word),
	store(chart(End2,Start,C,Goals)),
	fail.

scanner(End,End2) :-
	End2 is End+1.


% completer(+Position)
%
%	Find every chart entry that has no subgoals left
%	at the current position.
%	Use these chart entries to complete other higher 
%	level subgoals.
completer(Position) :-
	chart(Position,PC,C,[]),
	complete(C,PC,Position),
	fail.

completer(_).

complete(C,PC,Position) :-
	chart(PC,PC0,C0,[C|Goals]),
	store(chart(Position,PC0,C0,Goals)),
	Goals == [],
	complete(C0,PC0,Position),
	fail.

complete(_,_,_).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Various Predicates Used in the Parser. %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% get rid of all chart, links, and word positions.
clear_chart :- abolish(chart/4), abolish(c/3),abolish(l/2),
               dynamic(chart/4), dynamic(c/3),dynamic(l/2).


store(chart(A,B,C,D)) :-
	\+ ( chart(A,B,C1,D1),subsumes_chk(C1,C),subsumes_chk(D1,D) ),
	asserta(chart(A,B,C,D)).

% get_links
% 	create a table of links from grammar rules
get_links :-
	link(_,_),
	fail.

get_links :- assert(l(X,X)),assert(l(_,start)).

% Create Links 

% for the null determiner
link([],Y) :-
	rule(Y,[]),
	\+ l([],Y),
	assert(l([],Y)).

% for any immediate subgoals
link(X,Y) :-
	rule(Y,[X|_]),
	\+ l(X,Y),
	assert(l(X,Y)).

% for any subsubgoals, etc.
link(X,Z) :-
	l(Y,Z),   
	rule(Y,[X|_]),
	\+ l(X,Z),
	assert(l(X,Z)).

link(_,_).

% convert(+List,+Begin,-End)
%	convert a list to clauses in the knowledge base.
%	clauses are of the form c(+Before,+After,+Word),
%	And reflect the positions before and after a word.
convert([],End,End) :- !.

convert([H|T],Begin,End2) :-
	End is Begin+1,
	%write('here'),nl,
	assertz(c(Begin,End,H)),
	% write(End2),nl,
	convert(T,End,End2).








%%%%%%%%%%%%%%%%%%
% Some Utilities %
%%%%%%%%%%%%%%%%%%


% get_constituents_list(+Constituent,+Sentence,-CompletedConstituents)
%
%	Constituent is the constituent to parse in Sentence.  Make a list
%	of all the completed constituents and return it
get_constituents_list(C,List,CList) :-
	parse(C,List),!,
	findall(T,((chart(_,_,X,[]),X =.. [_,T])),CList).


% get_constituents(+Constituent,+List)
%
%	Constituent is the constituent to parse in Sentence.
%	Print each completed chart entry to the console.
get_constituents(C,List) :-
	parse(C,List),!,
	forall((chart(_,_,X,[]),X =.. [_,T]),(write(T),nl)).




