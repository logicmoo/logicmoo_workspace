% number_form( +WordList, -Number )
% e.g. number_form( [three,one,five], '315' )
%      number_form( ['315'], '315' )

/*
number_form( [Word|Rest], N ) :-
	digit_word( Word, Char ),
	digit_words( Rest, Chars ),
	atom_chars( N, [Char|Chars] ).

number_form( [Word], Word ) :-
	atom_chars( Word, WordCs ),
	number_chars( _, WordCs ).

digit_words( [Word|Rest], [Char|Chars] ) :-
	digit_word( Word, Char ),
	digit_words( Rest, Chars ),
	!.
*/

number_form( [Word|Rest], A ) :-
	digit_word( Word, _ ),
	words2number( [Word|Rest], N ),
	number2atom(N,A).

words2number(W,N):-
	words2numberstring(W,S,10),
	name(N,S).

words2numberstring( [], [], _ ).
words2numberstring( [Word|Words], S, C ):-
	C > 0,
	digit_word(Word, _),
	word2numberstring(Word, S1),
	C1 is C-1,
	words2numberstring( Words, S2, C1 ),
	append(S1,S2,S).


word2numberstring( W, S ):-
	digit_word( W, S  ).

number2atom(N,A):-
	number_chars(N,C),
	atom_chars(A,C).

% number_to_digits( +Number, -DigitString )
% e.g. number_to_digits( '315', "3 1 5" )

number_to_digits(Number,NumberStr) :-
	atom_chars(Number,NumberS),
	add_spaces(NumberS,NumberStr).

add_spaces( [], [] ).
add_spaces( [C], [C] ) :- !.
add_spaces( [C|Cs], [C,0' |Cs1] ) :-
	add_spaces( Cs, Cs1 ).
