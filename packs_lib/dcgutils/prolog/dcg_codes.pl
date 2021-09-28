/* Part of dcgutils
	Copyright 2012-2015 Samer Abdallah (Queen Mary University of London; UCL)
	 
	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:- module(dcg_codes, [
		writedcg/1
   ,  phrase_string/2
   ,  phrase_atom/2

   % Types
   ,  ctype//1

   % Constants
	,	null//0
   ,	cr//0
   ,  sp//0
   ,  fs//0
	,	fssp/2
   ,	tb/2
   ,	comma/2
   ,  commasp/2

   % Writing Prolog data
	,	at//1
   ,	wr//1
   ,	str//1
   ,  fmt//2
	,	padint/5

   % Brackets
	,	brace//1
   ,	paren//1
   ,	sqbr//1

   % Quoting and escaping
	,	q//1
   ,	qq//1
	,	escape//2 % escape Char by doubling up 
   ,  escape_with//3 % escape Char1 with Char2
   ,  esc//2  % predicate based, most flexible
]).

/** <module> DCG utilities for list of character codes representation.
 
This module contains predicates for working with DCGs defined over
sequences of character codes. Some of the predicates can only
be used to generate sequences, not parse them.

*/

:- meta_predicate 
		writedcg(2)
   ,  phrase_string(//,-)
   ,  phrase_atom(//,-)
	,	brace(//,?,?)
	,	paren(//,?,?)
	,	sqbr(//,?,?)
	,	qq(//,?,?)
	,	q(//,?,?)
   ,  esc(4,?,?,?)
	.

:- set_prolog_flag(double_quotes, codes).


%% writedcg(+P:phrase) is nondet.
%
%  Run the phrase P, which must be a standard list-of-codes DCG,
%  and print the output.
writedcg(Phrase) :-
	phrase(Phrase,Codes),
	format('~s',[Codes]).
		

%% phrase_string(+P:phrase,-S:string) is nondet.
%% phrase_string(+P:phrase,+S:string) is nondet.
%
%  Use list-of-codes DCG phrase P to parse or generate a string S.
phrase_string(Phrase,String) :-
   (  var(String)
   -> phrase(Phrase,Codes), string_codes(String,Codes)
   ;  string_codes(String,Codes), phrase(Phrase,Codes)
   ).

%% phrase_atom(+P:phrase,-A:atom) is nondet.
%% phrase_atom(+P:phrase,+A:atom) is nondet.
%
%  Use list-of-codes DCG phrase P to parse or generate a atom A.
phrase_atom(Phrase,Atom) :-
   (  var(Atom)
   -> phrase(Phrase,Codes), atom_codes(Atom,Codes)
   ;  atom_codes(Atom,Codes), phrase(Phrase,Codes)
   ).

%% ctype(Type)// is nondet.
%  Matches a code C that satisfies code_type(C,Type). See char_type/2 
%  for listing of types.
ctype(T) --> [X], {code_type(X,T)}.

%% null// is det.
%  Empty string.
null  --> "".

%% cr// is det.
%  Carriage return "\n".
cr    --> "\n".

%% sp// is det.
%  Space " ".
sp    --> " ".

%% fs// is det.
%  Full stop (period) ".".
fs    --> ".".

%% fssp// is det.
%  Full stop (period) followed by space.
fssp  --> ". ".

%% tb// is det.
%  Tab "\t".
tb    --> "\t".

%% comma// is det.
%  Comma ",".
comma   --> ",".

%% commasp// is det.
%  Comma and space ", ".
commasp --> ", ".

%% at(+X:atom)// is det.
%  Generate code list for textual representation of atom X.
at(A,C,T) :- atomic(A), with_output_to(codes(C,T),write(A)).

%% wr(+X:term)// is det.
%  Generate the list of codes for term X, as produced by write/1.
wr(X,C,T) :- ground(X), with_output_to(codes(C,T),write(X)).

%% wq(+X:term)// is det.
%  Generate the list of codes for term X, as produced by writeq/1.
wq(X,C,T) :- ground(X), with_output_to(codes(C,T),writeq(X)).

%% str(+X:term)// is det.
%  Generate the list of codes for string X, as produced by writeq/1.
str(X,C,T):- string(X), with_output_to(codes(C,T),write(X)).

%% fmt(+F:atom,+Args:list)// is det
%  Generate list of codes using format/3.
fmt(F,A,C,T) :- format(codes(C,T),F,A).

%% padint( +N:integer, +Range, +X:integer)// is det.
%% padint( +N:integer, +Range, -X:integer)// is nondet.
%
%  Write integer X padded with zeros ("0") to width N.
padint(N,..(L,H),X,C,T) :- 
	between(L,H,X), 
	format(codes(C,T),'~`0t~d~*|',[X,N]).

%% brace(+P:phrase)// is nondet.
%  Generate "{" before and "}" after the phrase P.
brace(A) --> "{", phrase(A), "}".

%% paren(+P:phrase)// is nondet.
%  Generate "(" before and ")" after the phrase P.
paren(A) --> "(", phrase(A), ")".

%% sqbr(+P:phrase)// is nondet.
%  Generate "[" before and "]" after the phrase P.
sqbr(A)  --> "[", phrase(A), "]".

%% q(+P:phrase)// is nondet.
%  Generate list of codes from phrase P, surrounds it with single quotes,
%  and escapes (by doubling up) any internal quotes so that the
%  generated string is a valid quoted string. Must be list of codes DCG.
q(X,[0''|C],T)  :- T1=[0''|T], escape_with(0'',0'',X,C,T1). 

%% qq(+P:phrase)// is nondet.
%  Generate list of codes from phrase P, surrounds it with double quotes,
%  and escapes (by doubling up) any double quotes so that the
%  generated string is a valid double quoted string.
qq(X,[0'"|C],T) :- T1=[0'"|T], escape_with(0'",0'",X,C,T1). 

%% escape(+Q:C, +P:phrase)// is nondet.
%
%  Runs phrase P to generate a list of elements of type C and
%  then escapes any occurrences of Q by doubling them up, e.g.,
%  =|escape(39,"some 'text' here")|= doubles up the single quotes
%  yielding =|"some ''text'' here"|=.
:- meta_predicate escape(+,//,?,?).
escape(Q,A) --> escape_with(Q,Q,A).

%% escape_with(+E:C, +Q:C, +P:phrase)// is nondet.
%
%  Runs phrase P to generate a list of elements of type C and
%  then escapes any occurrences of Q by prefixing them with E, e.g.,
%  =|escape_with(92,39,"some 'text' here")|= escapes the single quotes
%  with backslashes, yielding =|"some \'text\' here"|=.
:- meta_predicate escape_with(+,+,//,?,?).
escape_with(E,Q,Phrase,L1,L2) :-
	phrase(Phrase,L0,L2),
	escape_codes(E,Q,L0,L1,L2).

% escape difference list of codes with given escape character
escape_codes(_,_,A,A,A).
escape_codes(E,Q,[Q|X],[E,Q|Y],T) :-escape_codes(E,Q,X,Y,T).
escape_codes(E,Q,[A|X],[A|Y],T)   :- Q\=A, escape_codes(E,Q,X,Y,T).

%% esc(+Esc:esc,+Codes:list(code))// is det.
%% esc(+Esc:esc,-Codes:list(code))// is nondet.
%
%  Parser for a sequence of characters involving escape sequences. 
%  These are recognised by the predicate Esc, whose type is
%  ==
%  esc == pred(list(codes),list(codes))//.
%  ==
%  The DCG goal esc(H,T) matches an escaped sequence in the string
%  and unifies H-T with a difference list representing it's internal
%  or semantic form. Esc  must not place any constraints on the 
%  difference list tail T.
%
%  Starts with the longest possible match and retrieves shorter
%  matches on backtracking.

esc(Esc,C1) --> call(Esc,C1,C2), !, esc(Esc,C2). 
esc(_,[]) --> [].

% Not used, apparently.
% difflength(A-B,N) :- unify_with_occurs_check(A,B) -> N=0; A=[_|T], difflength(T-B,M), succ(M,N).

% % tail recursive version
% difflength_x(A-B,M)       :- difflength_x(A-B,0,M).
% difflength_x(A-B,M,M)     :- unify_with_occurs_check(A,B).
% difflength_x([_|T]-A,M,N) :- succ(M,L), difflength_x(T-A,L,N).

% These are some more escape/quoting mechanisms, disabled for now.
% escape_codes_with(Special,E,C) --> [E,C], {member(C,Special)}.
% escape_codes_with(Special,_,C) --> [C], {\+member(C,Special)}.
