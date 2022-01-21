%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% tokenizer.pl
%%%
%%% Read from pascal-like syntax, into list.
%%% Reading is CASE-INSENSITIVE:  all alpha characters
%%% are converted to lower case when read in.   
%%% Also ignores extra blanks, tabs, and linefeeds.
%%% Comments occur from some point in a line started by %,
%%% and will be ignored to end of line.
%%% Ignores spaces between operators and atoms,
%%% so squares can be written (X,Y) instead of ( X , Y ).  

% READ_TOKENS_FROM_FILE(+File,-Tokens)
read_tokens_from_file(File,Tokens) :-
	see(File),
	read_tokens(Tokens), !,
	seen.
read_tokens_from_file(File,_Tokens) :-
	format("~nError: Couldn't read tokens from file ~w~n",[File]),
	seen,
	fail.


% READ_TOKENS(-Tokens).
% First reads input literally (as number codes of characters),
% then converts to tokens.
% After this, the tokens can be parsed into a higher-level structure.
read_tokens(Tokens) :-
	read_chars(Chars),
	tokenize_chars(Chars,Tokens).

% READ_KEYBOARD_TOKENS(-Tokens).
% First reads input literally (as number codes of characters),
% then converts to tokens.
% After this, the tokens can be parsed into a higher-level structure.
read_keyboard_tokens(Tokens) :-
	read_chars_period_include(Chars),
	tokenize_chars(Chars,Tokens).


% READ_CHARS(-Chars)
% Reads in a list of numerical character codes, until encountering
% the end_of_file (returns code -1).
read_chars(Chars) :-
	read_chars(-1,n,Chars).

% READ_LINE(-Chars)
% Reads in a list of numerical character codes, until encountering
% a new line (char code 10).
read_line(Chars) :-
	read_chars(10,n,Chars).


% READ_CHARS_PERIOD_INCLUDE(-Chars)
% Reads in a list of numerical character codes, until encountering
% a period, which will be the last char.
read_chars_period_include(Chars) :-
	read_chars(0'.,y,Chars).

% READ_CHARS_PERIOD(-Chars)
% Reads in a list of numerical character codes, until encountering
% a period, which will be omitted from the string.
read_chars_period(Chars) :-
	read_chars(0'.,n,Chars).


% READ_CHARS(+EndChar,+Include,-Chars)
% Reads in a list of numerical character codes, until encountering
% either:
% a. end_of_file [-1],  or 
% b. EndChar, unless in a quotation ('...') context.
% c. EndChar, unless in a comment  (% ...) context. 
%    EndChar will be included in Chars if Include=y.
% 
read_chars(End,Include,Chars) :-
	read_chars(End,n,n,Include,Chars).

read_chars(End,Quote,Comment,Include,Chars) :-
	read_char(C),
	read_chars(C,End,Quote,Comment,Include,Chars).

read_chars(End,End,n,n,y,[End]) :- !.
read_chars(End,End,n,n,n,[]) :- !.
read_chars(-1,_,_,_,_,[]) :- !.
read_chars(C,End,Quote1,Comment1,Include,[C|Cs]) :-
	toggle_contexts(C,Quote1,Comment1,Quote,Comment),
	read_chars(End,Quote,Comment,Include,Cs).

toggle_contexts(C,Quote1,Comment1,Quote,Comment) :- 
	toggle_quote(C,Quote1,Quote),
	toggle_comment(C,Comment1,Comment).

% toggle_quote(Char,OldQuote,NewQuote)
% Quote char is: 0''  (ie.:  "'").
toggle_quote(0'',n,y).
toggle_quote(0'',y,n).
toggle_quote(_,Q,Q).

% toggle_comment(Char,OldComment,NewComment)
% Start Comment char is: 0'%  (ie.:  "%").
% End Comment char is newline.
toggle_comment(0'%,n,y).
toggle_comment(0'
,y,n).
toggle_comment(_,C,C).

%read_char(Char) :- get0(N), name(Char,[N]).
read_char(Char) :- get0(Char).


% READ_TOKENS_FROM_STRING(+String,-Tokens).
% Reads input from String (list of  number codes of characters),
% then converts to tokens.
% After this, the tokens can be parsed into a higher-level structure.
% This is just an alias for the procedure below.
read_tokens_from_string(String,Tokens) :-
	tokenize_chars(String,Tokens).

% TOKENIZE_CHARS(+Chars,-Tokens)
% Given a string of characters (like "barney." or
% equivalently [0'b,0'a,0'r,0'n,0'e,0'y,0'.]
% or the corresponding numbers [98,97,114,110,101,121,46] 
% Tokens is a list of atomic tokens read from this 
% string.  
% For the simple requirements of game and move grammars, this also
% gives us the ability to read directly from strings.
% Note that the tokens in the string ends when we read the 0'.
% character. If a string doesn't have this, we fail.

tokenize_chars(Chars,Tokens) :- 
	tokens(Tokens,[],Chars,_).


%tokenize_chars(Chars,Tokens) :- 
%	tokens(Tokens,[],Chars,[]).

tokens(['.'],[]) --> [0'.].
tokens(In,Out) --> token(In,S), tokens(S,Out).


token([C|Rest],Rest) --> identifier(C), !.
token(Rest,Rest)  --> layout_string(_C), !.
token(Rest,Rest) --> comment(_C), !.
token([C|Rest],Rest) --> operator(C).
token([C|Rest],Rest) --> quote_atom(C).


%----------------------------------------
% Quoted atom
%----------------------------------------

quote_atom(C) --> quote_char(_), non_quote_chars(Cs), quote_char(_), {string_chars(Cs,C)}.

non_quote_chars([]) --> [].
non_quote_chars([C|Cs]) --> non_quote_char(C), more_non_quote_chars(Cs).

more_non_quote_chars(Cs) --> non_quote_chars(Cs).
more_non_quote_chars([]) --> [].

non_quote_char(C) --> quote_char(C), !, {fail}.
non_quote_char(C) --> [C].

quote_char(39) --> [0''].

%----------------------------------------
% Comment
%----------------------------------------


comment(C) --> [0'%], non_lf_chars(Cs), linefeed(_), {string_chars(Cs,C)}.

non_lf_chars([]) --> [].
non_lf_chars([C|Cs]) --> non_lf_char(C), more_non_lf_chars(Cs).

more_non_lf_chars(Cs) --> non_lf_chars(Cs).
more_non_lf_chars([]) --> [].

non_lf_char(C) --> linefeed(C), !, {fail}.
non_lf_char(C) --> [C].

linefeed(10) --> [0'
]. 

%----------------------------------------
% Operator
%----------------------------------------


operator(X) --> comparison_operator(X), !.
operator(X) --> transfer_operator(X), !.
operator(X) --> [Y], {terminal_char(Y,X)}.

comparison_operator('>=') --> [0'>],[0'=].
comparison_operator('<=') --> [0'<],[0'=].

transfer_operator('->') --> [0'-,0'>].


terminal_char(X,Y) :- 
	terminal(X),
	name(Y,[X]).

%terminal(0'.).
terminal(0'{).
terminal(0'}).
terminal(0',).
terminal(0'<).
terminal(0'>).
terminal(0'[).
terminal(0']).
terminal(0'().
terminal(0')).
terminal(0'=).

terminal(0'-).
terminal(0'/).
terminal(0';).

%----------------------------------------
% Layout String
%----------------------------------------

layout_string([X|Xs]) --> layout_char(X), more_layout_chars(Xs).
more_layout_chars(Xs) -->  layout_string(Xs), !.
more_layout_chars([]) --> [].


layout_char(X) --> [X], {layout_char(X)}.

layout_char(9).
layout_char(10).
layout_char(32).
	

%----------------------------------------
% Identifier
%----------------------------------------

identifier(Id) --> alphanumchars(Chars),
	{string_chars(Chars,Id)}.


string_chars(Chars,Id) :- 
	name(Id,Chars).

/* not used */
chars_to_nums([],[]).
chars_to_nums([H|T],[NH|NT]) :- 
	name(H,[NH]),
	chars_to_nums(T,NT).

alphanumchars([X|Xs]) --> alphanumchar(X), more_alphanumchars(Xs).

more_alphanumchars(X) --> alphanumchars(X), !.
more_alphanumchars([]) --> [].

alphanumchar(Y) --> [X], {alphanumchar(X,Y)}.

% converts to lower-case
alphanumchar(X,X) :- X>=0'a,X=<0'z.
alphanumchar(X,X) :- X>=0'0,X=<0'9.
alphanumchar(X,Y) :- X>=0'A,X=<0'Z,
	downcase_char(X,Y).
alphanumchar(0'_,0'_).


downcase_char(Upper,Lower) :-
	downcase_char_num(Upper,Lower).

downcase_char_num(Upper,Lower) :-
	uppercase(Upper), !,
	Lower is Upper + "a" - "A".
downcase_char_num(X,X).

uppercase(N) :- N >= "A", N=< "Z".



%================================================================================
% Pretty-Printing Token Lists
%================================================================================
%
%  When tokens are parsed, they can also be generated back with
% special printing information, such as insertion of lines and tabs.
% These routines print these out nicely, destroying the list notation.
% However, they can always be read back in again, by the tokenizer.

%----------------------------------------
% PARSING_MODE(-Mode)
% Mode should be: PARSING in general, 
% but PRINTING, when using grammars to generate strings which will
% then be pretty-printed.

parsing_mode(M) :- parameter(parsing_mode,M).

% SET_PARSING_MODE(Mode)
set_parsing_mode(Mode) :- 
	set_parameter(parsing_mode,Mode).

set_parsing_mode :- set_parsing_mode(parsing).
set_printing_mode :- set_parsing_mode(printing).

%----------------------------------------

% ALPHA_SQUARES_MODE(-Mode)
%% Mode values: {on,off}
%% description:  whether can use (a,1) notation to denote
%%               squares in move and game grammars
%%   on:   use (a,1) notation
%%   off:  use (1,1) notation
%% used in: grammar.pl, notation.pl, interface.pl, tokenizer.pl
%% In general, this param is off, except for humans 
%% entering moves requiring completion.  It could be
%% used to generate pretty initial assignments for games,
%% but CAUTION, it will not parse games or moves in the
%% other mode.

set_alpha_squares_mode(M) :- set_parameter(alpha_squares_mode,M).

alpha_squares_mode(M) :- parameter(alpha_squares_mode,M).


% WITH_ALPHA_SQUARES(+Goal)
% Calls Goal with alpha squares mode on, then
% resets the mode to what it was before.
% Goal must be deterministic for this to work properly.

with_alpha_squares(Goal) :-
	alpha_squares_mode(Mode),
	set_alpha_squares_mode(on),
	with_alpha_squares(Goal,Mode).

with_alpha_squares(Goal,Mode) :-
	call(Goal), !,
	set_alpha_squares_mode(Mode).
with_alpha_squares(_Goal,Mode) :-
	set_alpha_squares_mode(Mode),
	fail.

recover_grammar :- 
	add_parameter(alpha_squares_mode,off).

%----------------------------------------




print_token(line) :- !, nl.
print_token(tab(T)) :- !, tab(T).
print_token(X) :- write(X), tab(1).

print_tokens([]).
print_tokens([H|T]) :- 
	print_token(H), 
	print_tokens(T).

print_tokens_to_file(Tokens,File) :-
	set_printing_mode,
	tell(File),
	print_tokens(Tokens),
	told.


print_token_to_string(line,[]) :- !.
print_token_to_string(tab(_T),[]) :- !.
print_token_to_string(X,String) :- name(X,String).

% PRINT_TOKENS_TO_STRING(Tokens,String)
% Prints out Tokens (a list of atoms) into a String which.
% If String were then printed, the printout would
% look just like printing each of the tokens, with
% 1 space between each token.  
% This string is of the right form to be read in later by 
% READ_TOKENS_FROM_STRING/2 :
%
%      | ?- print_tokens_to_string([barney,is,happy,'.'],S), read_tokens_from_string(S,T).
%
%      S = [98,97,114,110,101,121,32,105,115,32,104,97,112,112,121,32,46,32],
%      T = [barney,is,happy,'.'] ? 


print_tokens_to_string([],[]).
print_tokens_to_string([H|T],String) :- 
	print_token_to_string(H,StrH), 
	print_tokens_to_string(T,StrT),
	append(StrH,[0' |StrT],String).


