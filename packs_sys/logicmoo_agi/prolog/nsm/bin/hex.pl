:- module(hex,[
	       hex2dec/2,
	       hex2decs/2,
	       get_uni_char/3,
	       num2str/2
	      ]).

/** <module> Hexadecimal converter

Conversion from decimal to hexadecimal and vice-versa is needed
by the markup routines (see mark_up.pl), mainly for
unicode character conversion. For example, rtf unicode character
escapes need a decimal number, while HTML and XML use hexadecimal
codes
*/


%%	hex2dec(+Hex:string,-Dec:int) is det
%
%	This predicate converts an hexadecimal number 
%       (represented as a string, e.g. "32AB", into a decimal number,
%       which variable Dec gets instantiated to.
%       
%       Example:
%       
%       ==
%       2 ?- hex:hex2dec("AB",X).
%       X = 171 .
%       ==
%       
hex2dec(Hex,Dec) :-
	hex2dec(Hex,0,Dec).


/** <module> Hexadecimal converter

Conversion from decimal to hexadecimal and vice-versa is needed
by the markup routines (see mark_up.pl), mainly for
unicode character conversion. For example, rtf unicode character
escapes need a decimal number, while HTML and XML use hexadecimal
codes
*/


%%	hex2decs(+Hex:string,-Dec:string) is det
%
%	This predicate converts an hexadecimal number 
%       (represented as a string, e.g. "32AB", into a decimal number,
%       represented as a string.
%       
%       Example:
%       
%       ==
%       4 ?- hex:hex2decs("AB",X),name(Num,X).
%       X = [49, 55, 49],
%       Num = 171 .
%       ==
%       

hex2decs(Hex,Str) :-
	hex2dec(Hex,0,Dec),
	num2str(Dec,Str).

get_uni_char([59|Rest],[],Rest) :- !.
get_uni_char([],[],[]).
get_uni_char([C|String],[C|Char],Rest) :-
	get_uni_char(String,Char,Rest).

hex2dec([],Dec,Dec).
hex2dec([HexDigit|HexNum],Dec,Res) :-
	Dec1 is Dec * 16,
	hex_val([HexDigit],Val),
	Dec2 is Dec1 + Val,
	hex2dec(HexNum,Dec2,Res).
	
	
num2str(Num,Str) :-
	name(Num,Str).


hex_val("0",0).
hex_val("1",1).
hex_val("2",2).
hex_val("3",3).
hex_val("4",4).
hex_val("5",5).
hex_val("6",6).
hex_val("7",7).
hex_val("8",8).
hex_val("9",9).
hex_val("A",10).
hex_val("B",11).
hex_val("C",12).
hex_val("D",13).
hex_val("E",14).
hex_val("F",15).
hex_val("a",10).
hex_val("b",11).
hex_val("c",12).
hex_val("d",13).
hex_val("e",14).
hex_val("f",15).

