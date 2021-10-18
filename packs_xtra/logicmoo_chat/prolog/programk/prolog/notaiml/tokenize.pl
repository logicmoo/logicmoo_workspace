:- module(tokenize, [token_stream_of/2, detokenize/2]).
/*
             A tokenizer
	     converts an input string into a list of tokens.
    Stuff that's a token:
    any C language symbol is the token word('foo73_bar')
    any sequence of digits that didn't get sucked into a C symbol is number(42)
    any other non whitespace is special('&')
*/

% unifies if NewSoFar is SoFar with the token created from Word appended
% to it
% word is single character atoms of the word in reverse order
add_word(Word, SoFar, [word(Atom)|SoFar]) :-
	reverse(Word, WordLR),
	atomic_list_concat(WordLR, Atom).

add_num(Word, SoFar, [number(Val)|SoFar]) :-
	base_ten_value(Word, 1 , 0, Val).

base_ten_value([], _ , X, X).
base_ten_value([C|T], Place, ValSoFar, Val) :-
	char_type(C, digit(V)),
	NPlace is 10 * Place,
	NValSoFar is ValSoFar + Place * V,
	base_ten_value(T, NPlace, NValSoFar, Val).

add_special(C, SoFar, [special(C)|SoFar]).

% unifies if InToken is the token list of the input
% tokenize/5
%     -State, this is a state machine lexer, this is the state name
%     -Chars, remaining chars as a list of one char
%      atoms
%     -PartialToken, list of chars accumulated for this token in
%      backwards order
%     -TokensSoFar,  list of tokens accumulated so far
%     +InToken)       final output list of tokens
%
%     done
tokenize(_, [], [], Tokens, Tokens) :- !.
% done with part of a word
tokenize(in_word, [], Word, SoFar, Tokens) :-
	 add_word(Word, SoFar, Tokens).
tokenize(in_num, [], Word, SoFar, Tokens) :-
	add_num(Word, SoFar, Tokens).

%all processing once we're in a word
tokenize(in_word, [C|T], Word, SoFar, Tokens) :-
	char_type(C, csym),
	tokenize(in_word, T, [C|Word], SoFar, Tokens).
tokenize(in_word, [C|T], Word, SoFar, Tokens) :-
	add_word(Word, SoFar, NewSoFar),
	tokenize(not_in_word, [C|T], [], NewSoFar, Tokens).

%all processing once we're in a number
tokenize(in_num, [C|T], Word, SoFar, Tokens) :-
	char_type(C, digit),
	tokenize(in_num, T, [C|Word], SoFar, Tokens).
tokenize(in_num, [C|T], Word, SoFar, Tokens) :-
	add_num(Word, SoFar, NewSoFar),
	tokenize(not_in_word, [C|T], [], NewSoFar, Tokens).

% encounter a csymf, start a word
tokenize(not_in_word, [C|T], [], SoFar, Tokens) :-
	char_type(C, csymf),
	tokenize(in_word, T, [C], SoFar, Tokens).

% encounter a digit, start a number
tokenize(not_in_word, [C|T], [], SoFar, Tokens) :-
	char_type(C, digit),
	tokenize(in_num, T, [C], SoFar, Tokens).

% eject whitespace
tokenize(not_in_word, [C|T], [], SoFar, Tokens) :-
	char_type(C, space),
	tokenize(not_in_word, T, [], SoFar, Tokens).

% handle as a special
tokenize(not_in_word, [C|T], [], SoFar, Tokens) :-
	add_special(C, SoFar, NewSoFar),
	tokenize(not_in_word, T, [], NewSoFar, Tokens).

token_stream_of('', []).
token_stream_of(Intext, InTokens) :-
	atom_chars(Intext, Chars),
	tokenize(not_in_word, Chars, [] , [] , InTokensBackwards),
	reverse(InTokensBackwards, InTokens).

% -TokenList, +Atom   turn a list of tokens into an atom
detokenize([], '').
detokenize(TokenList, Atom) :- detokenize(TokenList, '', Atom).
detokenize(List, AtomSoFar, Atom) :-
	elements_to_atoms(List, [], ListOfAtoms),
	atom_concat(ListOfAtoms, ' ',  Atom).

elements_to_atoms([] , A, A).
elements_to_atoms([word(X)|T], [X|SoFar], Final) :-
	atom(X),
	elements_to_atoms(T, SoFar, Final).
elements_to_atoms([nt(_)|T], SoFar, Final) :-
	elements_to_atoms(T, SoFar, Final).
elements_to_atoms([special(X)|T], [X|SoFar], Final) :-
	atom(X),
	elements_to_atoms(T, [X|SoFar], Final).










