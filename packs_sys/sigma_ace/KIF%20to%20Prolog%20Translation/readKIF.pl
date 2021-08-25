% CELT KIF to Prolog Translator

:- style_check(-singleton).      % no singletons warnings
:- style_check(-discontiguous).  % allows more creative predicate placement

:-write('Loading CELT Parser Version 2(b) ...KIF to Prolog Translator'),nl.

%-------------------------------
% INTERFACE CODE
%-------------------------------

translate :- read_kif('kif.txt','kif.pl').

read_kif(FILE1,FILE2) :- see(FILE1),
	tell(FILE2),
	read_all_lines(20000000000),
	seen,
	told.

read_all_lines(0) :- !,write('\%Done.'),nl.

read_all_lines(N) :- at_end_of_stream,!,write('\%End of file'),nl.

read_all_lines(N) :-
	M is N - 1,!,
	read_next_line_or_sexp,
	read_all_lines(M).

read_all_lines(N) :- write('Fell through read_all_lines ??'),nl.

% read_next_line_or_sexp skips white space as necessary
% to find the first left parens or semi-colon, then calls
% either the read comment procedure or the read sexp procedure
% as appropriate.

read_next_line_or_sexp :-
	at_end_of_stream,!.

read_next_line_or_sexp :-
	peek_code(Code),
	comment_character(Code),
	!,
	read_comment_line([],Comment).

read_next_line_or_sexp :-
	peek_code(Code),
	left_parenthesis(Code),
	!,
	read_sexp(Sexp),
	translate_KIF_to_Prolog(Sexp).

% translate_KIF_to_Prolog(+Sexp) writes
% out sexp, taking care to make the following
% replacements:
%
%     not    -> is_not
%     forall -> for_all
%
% to avoid conflicts with predefined Prolog fns.

translate_KIF_to_Prolog([not|Rest]) :-
	!,
	FOR_PROLOG =.. [is_not|Rest],
	write(FOR_PROLOG :- true),write('.'),nl.

translate_KIF_to_Prolog([forall|Rest]) :-
	!,
	FOR_PROLOG =.. [for_all|Rest],
	write(FOR_PROLOG :- true),write('.'),nl.

translate_KIF_to_Prolog(Sexp) :-
	FOR_PROLOG =.. Sexp,
	write(FOR_PROLOG :- true),write('.'),nl.
	
read_next_line_or_sexp :-
	peek_code(Code),
	end_of_line(Code),
	!,
	get0(Code).

read_next_line_or_sexp :-
	peek_code(Code),
	whitespace(Code),
	!,
	get0(Code).

read_next_line_or_sexp :-
	peek_code(Code),
	double_quote(Code),
	!,
	read_string(String),
	write(String),nl.

read_next_line_or_sexp :-
	peek_code(Code),
	write('Character Code Not Covered: '),
	write(Code),
	nl,
	true.

read_next_line_or_sexp :- write('Fell through read_next_line_or_sexp??').

comment_character(59). /* ; */

% read_comment_line reads characters until an end of line
% marker is found. It is used for skipping over comments.

read_comment_line(Chars, Comment) :-
	at_end_of_stream,write('End of file found while reading a comment'),!.

read_comment_line(Chars, Comment) :-
	peek_code(Code),
	end_of_line(Code),
	!,
	get0(Code),
	reverse(Chars,InOrder),
	name(Comment,InOrder).

read_comment_line(Chars, Comment) :-
	get0(Code),
	read_comment_line([Code|Chars],Comment).

end_of_line(10).    /* \n  */

% read_token reads the current string of characters until
% a space, newline, or period is found. It returns the token
% read.

read_token(Token) :- read_one_token([],Token).

read_one_token(CharsSoFar,Token) :-
	at_end_of_stream,!,
	make_token_from_char_list(CharsSoFar,Token).

read_one_token(CharsSoFar,Token) :-
	peek_code(Code),
	separator(Code),!,
	make_token_from_char_list(CharsSoFar,Token).

read_one_token(CharsSoFar,Token) :-
	peek_code(Code),
	token_char(Code),!,
        get0(Code),
        read_one_token([Code|CharsSoFar],Token).

/* make_token_from_char_list just reverses the characters
in Chars to form Token, unless Token would start with a
decimal point (e.g., .5) in which case a leading 0 is added
to satisfy Prolog (e.g., .5 would become 0.5).

One more change for Prolog is that single-quotes are added
around the token to preserve any capitalization from KIF,
if there are any capital letters in the token anywhere.

39 is single quote.
46 is decimal point.
48 is zero.

*/

make_token_from_char_list(Chars,Token) :-
	reverse(Chars,InOrder),
	((InOrder = [46|Others]) -> name(Token,[48|InOrder]);
	    (all_lowercase(Chars) -> name(Token,InOrder); reverse([39|Chars],WithCaps),name(Token,[39|WithCaps]))).

separator(32).    /* ' ' */
separator(10).    /* \n  */
separator(9).     /* \t  */
separator(40).    /* '(' */
separator(41).    /* ')' */

% read_sexp reads from the next sexp

read_sexp(Sexp) :- find_left,read_tokens_in_sexp([],Sexp),find_right.

% find left parenthesis

find_left :- peek_code(Code),
	left_parenthesis(Code),
        get_code(Code),
	!.

find_left :- peek_code(Code),
	whitespace(Code),
	get_code(Code),
	!,
	find_left.

find_left :- fail.

whitespace(32).    /* ' ' space */
whitespace(9).     /* \t  tab */
whitespace(10).    /* \n  newline */

% find right parenthesis

find_right :- peek_code(Code),
	right_parenthesis(Code),
        get_code(Code),
	!.

find_right :- peek_code(Code),
	whitespace(Code),
	get_code(Code),
	!,
	find_right.

find_right :- fail.

% next char is a '(', indicating an embedded sexp.
read_tokens_in_sexp(TokensSoFar,Sexp) :-
	peek_code(Code),
	left_parenthesis(Code),
	!,
        read_sexp(Embedded),
	read_tokens_in_sexp([Embedded|TokensSoFar],Sexp).

% next char is a ')', indicating end of the current sexp.
read_tokens_in_sexp(TokensSoFar,Sexp) :-
	peek_code(Code),
	right_parenthesis(Code),
	!,
	reverse(TokensSoFar,Sexp).

% next char is double-quote, indicating the beginning of a string
read_tokens_in_sexp(TokensSoFar,Sexp) :-
	peek_code(Code),
	double_quote(Code),
	!,
	read_string(String),
	read_tokens_in_sexp([String|TokensSoFar],Sexp).

% next char is whitespace (space, tab, or newline) and we are between tokens
read_tokens_in_sexp(TokensSoFar,Sexp) :-
	peek_code(Code),
	separator(Code),
	!,
        get_code(Code),
	read_tokens_in_sexp(TokensSoFar,Sexp).



% next char is part of a token.
read_tokens_in_sexp(TokensSoFar,Sexp) :-
	peek_code(Code),
	token_char(Code),
	!,
        read_token(Token),
	read_tokens_in_sexp([Token|TokensSoFar],Sexp).

% next char is part of a token.
read_tokens_in_sexp(TokensSoFar,Sexp) :-
	peek_code(Code),
	write('Unidentified Code in an SEXP: '),
	write(Code),nl.

token_char(Code) :- Code > 96, Code < 123.  /* a b ... z */
token_char(Code) :- Code > 64, Code < 91.   /* A B ... Z */
token_char(Code) :- Code > 47, Code < 58.   /* 0 1 ... 9 */
token_char(Code) :- Code > 59, Code < 65.   /* <, =, >, and ? */
token_char(43).                             /* plus sign, as in +8 */
token_char(45).                             /* minus sign, as in -2 */
token_char(46).                             /* decimal point, as in .5 */
token_char(37).                             /* percent sign, %, as in &%Minute */
token_char(38).                             /* ampersand sign, &, as in &%Minute */

left_parenthesis(40).
right_parenthesis(41).
new_line(10).
new_line(13).

% read_string reads all characters from one double-quote to
% another. It assumes that there are no double-quotes embedded
% in the string, i.e., no special escape characters to allow
% embedded quotes.

read_string(String) :- find_double_quote([],_),find_double_quote([],String).

% find double quote

find_double_quote(Chars,String) :-
	peek_code(Code),
	double_quote(Code),
        get_code(Code),
	!,
	reverse([34|Chars],InOrder), /* the 34's here and the next line add double-quotes around the string */
	name(String,[34|InOrder]).

find_double_quote(Chars,String) :-
	get_code(Code), /* skip chars between double quotes */
	!,
	find_double_quote([Code|Chars],String).

double_quote(34).

uppercase(Code) :- Code > 64, Code < 91.   /* A B ... Z */

% all_lowercase(+Chars) succeeds only if there are no upper case character
% codes in Chars.

all_lowercase([]).

all_lowercase([Char|Chars]) :- uppercase(Char), !, fail.

all_lowercase([Char|Chars]) :- all_lowercase(Chars).