% scp.pl
% SCP: A Simple Chunk Parser
% Philip Brooks
%  Based on programs by Michael Covington given in his
%  Natural Language Processing class in Spring 2003
%  and in his book Natural Language Processing for
%  Prolog Programmers.
% 05/08/03

%
% Tested using SWI Prolog 5.0.10
% Available at http://www.swi-prolog.org/
%

%
% Requires a grammar of rule/2 and word/2 clauses:
%  word(Category, Word)
%  rule(Phrase, Constituents)
% where Constituents is a list of the phrase's constituents,
% either word categories or other phrases.  A constituent
% may be enclosed in brackets indicating that it is optional.
%
% The order of the rules in the grammar matters, as this
% parser will return the first rule that completely matches.
%


% chunk_parse(+Stream,-Rest,-Tree)
%  Parses a constituent of type 'chunk' and peforms a cut.
%  Stream is the input, Tree the tree of the chunk, and
%  Rest the rest of the Steam once the chunk is removed.

chunk_parse(Stream,Rest,Tree) :-
	parse(chunk,Stream,Rest,Tree),
	!.


% chunk_parse_text(+Stream)
%  Calls chunk_parse over and over, displaying the tree each
%  time, until all the text is used up.

chunk_parse_text(S) :-
	chunk_parse(S,Rest,Tree),
	show_tree(Tree),
	nl,
	chunk_parse_text(Rest).
chunk_parse_text([]).


% chunk_parse_text(+Stream,-List)
%  Calls chunk_parse over and over, and returns a list of
%  all the chunks.

chunk_parse_text(Stream,[Chunk|List]) :-
	chunk_parse(Stream,Rest,Chunk),
	chunk_parse_text(Rest,List).
chunk_parse_text([],[]).


% show_tree(+Tree)
%  Displays a tree as an indented structure

show_tree(Tree) :-
	show_tree_x(Tree,0).

show_tree_x(Atom,N) :-
	atomic(Atom),
	!,
	write_spaces(N),
	write(Atom),
	nl.

show_tree_x(Tree,N) :-
	NewN is N + 3,
	functor(Tree,Functor,Args),
	show_tree_x(Functor,N),
	between(1,Args,X),
	arg(X,Tree,Item),
	show_tree_x(Item,NewN),
	fail.
show_tree_x(_,_).


% write_spaces(+Num)
%  Writes Num spaces.  Called by show_tree/1.

write_spaces(X) :-
	X >= 0,                % Sanity check
	write_spaces_x(X).

write_spaces_x(0) :- !.
write_spaces_x(N) :-
	write(' '),
	NewN is N - 1,
	write_spaces_x(NewN).


% parse(?Category, +Stream, ?R, -Tree)
%  Parses a word/phrase of type Category from the beginning
%  of Stream with R remaining.  Tree is the tree of the
%  parsed item.

% Category is a word
parse(C,[Word|S],S,PWord) :-
	word(C,Word),
	functor(PWord,C,1),
	arg(1,PWord,Word).

% Category is a phrase
parse(C,S,R,T) :-
	rule(C,Cs),
	parse_list(Cs,S,R,List),
	T =.. [C|List].

% Optional constituent
parse_list([[C]|Cs],S,R,[Tree|List]) :-
	parse(C,S,S1,Tree),
	parse_list(Cs,S1,R,List).
parse_list([[_]|Cs],S,R,List) :-
	!,
	parse_list(Cs,S,R,List).

% Ordinary constituent
parse_list([C|Cs],S,R,[Tree|List]) :-
	parse(C,S,S1,Tree),
	parse_list(Cs,S1,R,List).

% Done with this list
parse_list([],S,S,[]).


% flatten_chunks(+Chunks,-List)
%  Given a list of Chunks, flattens them all 
%  and returns them in List.

flatten_chunks(Chunks,List) :-
	maplist(flatten_chunk,Chunks,List).


% flatten_chunk(+Chunk,-Flattened)
%  Extract the words from the Chunk in a flat list.

flatten_chunk(Chunk,Flat) :-
	flatten_chunk(Chunk,Flat,_).


% flatten_chunk(+Chunk,-Flattened,-Type)
%  Extract the words from the Chunk into a Flattened
%  list and return its Type.

flatten_chunk(chunk(Struc),Flat,Type) :-
	Struc =.. [Type|List],
	flatten_chunk_x(List,Flat).

flatten_chunk_x([H|T], [H|FT]) :-
	atomic(H),
	!,
	flatten_chunk_x(T,FT).
flatten_chunk_x([H|T], Flat) :-
	H =.. [_|List],
	flatten_chunk_x(List,FH),
	flatten_chunk_x(T,FT),
	append(FH,FT,Flat).
flatten_chunk_x([],[]).
