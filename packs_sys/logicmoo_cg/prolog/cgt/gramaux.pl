/* COPYRIGHT ************************************************************

Conceptual Graph Tools (CGT) - a partial implementation of Sowa's CS Theory
Copyright (C) 1990 Miguel Alexandre Wermelinger

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

************************************************************************/

/* AUTHOR(S) ************************************************************

Michel Wermelinger
Dept. de Informatica, Univ. Nova de Lisboa, Quinta da Torre
P - 2825 Monte da Caparica, PORTUGAL
Phone: (+351) (1) 295 44 64 ext. 1360  Internet: mw@fct.unl.pt

************************************************************************/

/* GENERALITIES *********************************************************
 
File Name	: GRAMAUX.PL
Creation Date	: 90/06/26 	By: mw
Abbreviations	: mw - Michel Wermelinger 
Description	: Reads the linear notation
Notes		: the arity of the DCG predicates doesn't include the lists
 
************************************************************************/

/* HISTORY **************************************************************
 
1.0	90/07/08  mw	initial version
1.1	90/10/23  mw	a word may now contain '-' and start/end with "
1.2	90/12/08  mw	rewrote the tokeniser thereby eliminating some bugs
1.3     91/01/04  mw    added accent/2

************************************************************************/

/* CONTENTS *************************************************************

message/1		displays a message (works with CGE) 
get_token/1		reads user input and returns the list of tokens
tokenise/1              transforms a list of ASCII codes into a list of tokens

************************************************************************/

/* message/1 ************************************************************

Usage		: message(+Message)
Argument(s)	: atom, list or Prolog goal
Description	: writes the Message
Notes		: if Message is a Prolog goal, executes it
	          if Message is a list, writes each element separately

************************************************************************/

% the following clause(s) is(are) to be used iff CGE is available
message(Msg) :- acknowledge(Msg).

/* the following clause(s) is(are) to be used iff CGE isn't available
message([]) :- !.
message([H|T]) :- message(H), message(T), !.
message(Atom) :- atomic(Atom), write(Atom), !.
message(G) :- call(G), !.
message(_).
*/

/* get_token/1 **********************************************************

Usage		: get_token(-Tokens)
Argument(s)	: 	      list
Description	: reads user input from the keyboard returning a list of tokens
Notes		: 

************************************************************************/

get_token(T) :- read_in(L), tokenise(T, L, []).

/* read_in/1 ************************************************************

Usage		: read_in(-Characters)
Argument(s)	: 	      list
Description	: reads Characters from the keyboard, stopping with '.'
Notes		: just used with Arity-Prolog

************************************************************************/

read_in(L) :-
	get0(C),
	( C = 13, nl, read_in(T), join_bs(13, T, L)
	; C = 10, nl, read_in(T), join_bs(10, T, L)
	%; C = 9, read_in(T), join_bs(9, T, L)
	; C = 8, read_in(T), L = [8|T]
	; name('.', [C]), L = [C]
	; read_in(T), join_bs(C, T, L)
	).

/* join_bs/3 ************************************************************

Usage		: join_bs(+Character, +Rest, -List)
Argument(s)	: 	     atom      list   list
Description	: treats backspaces
Notes		: 

************************************************************************/

join_bs(10, [8|T], L) :- join_bs(10, T, L).
join_bs(13, [8|T], L) :- join_bs(13, T, L).
join_bs(_, [8|T], T).
join_bs(H, T, [H|T]).

/* tokenise/1 ***********************************************************

Usage		: tokenise(-Tokens)
Argument(s)	: 	     list
Description	: DCG predicate which returns a list of atoms (tokens)
Notes		: the input list contains the ASCII codes of the characters read

************************************************************************/

tokenise(['(', 'NEG', ')', '-', '>', '['|L]) --> 
	skip_blanks, char(_, '~'), skip_blanks, char(_, '['),
        tokenise(L).
tokenise([T|L]) --> skip_blanks, token(T), tokenise(L).
tokenise([]) --> [].

/* skip_blanks/0 ********************************************************

Usage		: skip_blanks
Argument(s)	:
Description	: DCG predicate which skips the following white space
Notes		: succeeds always

************************************************************************/

skip_blanks --> blank(_, _), skip_blanks.
skip_blanks --> [].

/* token/1 **************************************************************

Usage		: token(-Token)
Argument(s)	: 	 atom
Description	: DCG predicate which returns a token
Notes		: a token is an integer, a word or a single character

************************************************************************/

token(I) --> digit(_, D), integer(I, D).
token(W) --> letter(C, _), word(W, [C]).
token(W) --> char(C, '"'), word(P, []), char(C, '"'), 
             { name(P, L1), conc(['"'|L1], ['"'], L2), name(W, L2) }.
token(C) --> char(_, C).

/* integer/2 ************************************************************

Usage		: integer(-Integer, +Partial)
Argument(s)	: integers
Description	: DCG predicate which returns an Integer
Notes		: Partial is the integer parsed so far

************************************************************************/

integer(I, N) --> digit(_, D), { J is 10*N+D }, integer(I, J).
integer(I, I) --> [].

/* word/2 ***************************************************************

Usage		: word(-Word, +Partial)
Argument(s)	:       atom    list
Description	: DCG predicate which returns a Word
Notes		: Partial is the list of characters parsed so far
	          Word is given by the following regular expression:
			letter ( letter | digit | "-" | "_" )*

************************************************************************/

word(W, R) --> letter(C, _), word(W, [C|R]).
word(W, R) --> digit(C, _), word(W, [C|R]).
word(W, R) --> char(C, '-'), word(W, [C|R]).
word(W, R) --> char(C, '_'), word(W, [C|R]).
word(W, R) --> accent(C, _), word(W, [C|R]).
word(W, R) --> { reverse(R, L), name(W, L) }.

/* char/2 ***************************************************************

Usage		: char(-Ascii, -Character)
Argument(s)	:      integer    atom
Description	: DCG predicate returning the next Character and its Ascii code
Notes		:

************************************************************************/

char(A, C) --> [A], { name(C, [A]) }.

/* letter/2 *************************************************************

Usage		: letter(-Ascii, -Letter)
Argument(s)	:      integer     atom
Description	: DCG predicate returning the next Letter and its Ascii code
Notes		: fails iff the next character in the input list isn't a letter

************************************************************************/

letter(C, L) --> [C], { letter(C), name(L, [C]) }.

/* accent/2 *************************************************************

Usage		: accent(-Ascii, -Accent)
Argument(s)	:        integer   atom
Description	: DCG predicate returning the next accent and its Ascii code
Notes		: fails iff the next character in the input list isn't an accent
                  an accent is "'", "`", "^" or "~"

************************************************************************/

accent(C, A) --> 
        char(C, A), { A = '''' ; A = '`' ; A = '^' ; A = '~' }.

/* digit/2 **************************************************************

Usage		: digit(-Ascii, -Digit)
Argument(s)	: integers
Description	: DCG predicate returning the next Digit and its Ascii code
Notes		: fails iff the next character in the input list isn't a digit

************************************************************************/

digit(C, D) --> [C], { digit(C), D is C - 48 }.

/* blank/2 **************************************************************

Usage		: blank(-Ascii, -Blank)
Argument(s)	: 	integer  atom
Description	: DCG predicate returning the next Blank and its Ascii code
Notes		: fails iff the next character in the input list isn't a blank
		  Blank is 'space', 'tab', 'nl', 'cr' or 'ff'
		  the information in blank might be important to update the
			current line and column numbers

************************************************************************/

blank(9, tab) --> [9].
blank(32, space) --> [32].
blank(10, nl) --> [10].
blank(13, cr) --> [13].
blank(12, ff) --> [12].

/* letter/1 *************************************************************

Usage		: letter(+Ascii)
Argument(s)	: 	 integer
Description	: succeeds iff Ascii is the ASCII code of a letter
Notes		:

************************************************************************/

letter(L) :- upper(L), !.
letter(L) :- lower(L), !.

/* upper/1 **************************************************************

Usage		: upper(+Ascii)
Argument(s)	: 	integer
Description	: succeeds iff Ascii is the ASCII code of an uppercase letter
Notes		:

************************************************************************/

upper(C) :- C >= 65, C =<  90.

/* lower/1 **************************************************************

Usage		: lower(+Ascii)
Argument(s)	: 	integer
Description	: succeeds iff Ascii is the ASCII code of a lowercase letter
Notes		:

************************************************************************/

lower(C) :- C >= 97, C =< 122.

/* digit/1 **************************************************************

Usage		: digit(+Ascii)
Argument(s)	: 	integer
Description	: succeeds iff Ascii is the ASCII code of a digit
Notes		:

************************************************************************/

digit(D) :- D >= 48, D =< 57.
