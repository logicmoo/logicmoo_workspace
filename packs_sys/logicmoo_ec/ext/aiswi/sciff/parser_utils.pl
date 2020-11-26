:-module(parser_utils,
	 [read_file_to_string/2,
	 atomic_constant_list/3,
	 atomic_constant/3,
	 elementList/3, %PAY ATTENTION: HERE THE ORIGINAL PRED IS /1, BUT SINCE IT'S DCG, IT BECOMES /3 !!!
	 drop_whites/2,
	 drop_comments/2,
	 opening_parenthesis/2,
	 term/3,
	 term_list/3,
	 comma/2,
	 time/3,
	 closing_parenthesis/2,
	 funct/3,
	 number/3,
	 variable/3,
	 full_stop/2,
     integer/3,
     signed_int/3,
     alphanumeric_string/3]).


:-use_module(library(lists)).
%	     [append/3,
%	      member/2, memberchk/2]).

% If the path is a URL, then download the file
read_file_to_string(Path,String):-
    atom_concat('http://',_,Path),!,
    ensure_loaded(library(pillow)), % load the library only if needed
    url_info(Path,Info),
    fetch_url(Info,[],Response),
    
    memberchk(status(Type,Code,Message),Response),
    (Type = success
        -> memberchk(content(String),Response)
        ;  write(Type), write(' '), write(Code), write(': '), print_resp(Message), nl, fail
    ).
% Otherwise, if the file is local, open it
read_file_to_string(File,String):-
	open(File,read,Stream),
	%write('Open OK'),nl,
	read_stream_to_string(Stream,String),
	close(Stream).

print_resp([]):-!.
print_resp([C|S]):- !,format('~s',[[C]]), print_resp(S).



read_stream_to_string(Stream,[]):-
	at_end_of_stream(Stream),
	!.
read_stream_to_string(Stream,[C|T]):-
	get_code(Stream,C),
	read_stream_to_string(Stream,T).

drop_whites([],[]).
drop_whites(Codes,NoWhiteCodes):-
	whites(Codes,MoreCodes),
	!,
	drop_whites(MoreCodes,NoWhiteCodes).
drop_whites([Code|MoreCodes],[Code|MoreNoWhiteCodes]):-
	drop_whites(MoreCodes,MoreNoWhiteCodes).


full_stop -->
	".".
comma -->
	",".
opening_parenthesis -->
	"(".
closing_parenthesis -->
	")".

whites(Codes,MoreCodes):-
	white_codes(WhiteCodes),
	append(WhiteCodes,MoreCodes,Codes).

white_codes(WhiteCodes):-
	white_atom(Atom),
	atom_codes(Atom,WhiteCodes).
white_codes([13]).
white_codes([-1]). % Sometimes in Linux the end-of-file has code -1
	
white_atom(' ').
white_atom('\t').
white_atom('\n').

funct(Functor) -->
	atomic_constant(Functor),
	!.
funct(',')-->
	"".

atomic_constant(Const) -->
	lowercase(LowerCase),
	alphanumeric_string_tail(Alpha),
	{atom_codes(Const,[LowerCase|Alpha])}.
atomic_constant(Const) -->
	"'",
	any_char_tail(Alpha),
	"'",
	{
		char_code('\'', A),
		append(Alpha, [A], L1),
		append([A], L1, L2),
		atom_codes(Const,L2)
	}.
	
any_char_tail(String) -->
	any_char(String),
	!.
any_char_tail([]) -->
	[].
any_char([Code|MoreCodes]) -->
	any_char_excluded_apice(Code),
	!,
	any_char_tail(MoreCodes).

any_char_excluded_apice(Code) -->
	[Code],
	{
		char_code('\'', A),
		Code =\= A
	}.



lowercase(C) -->
	[C],
	{char_code('a',A),
	 char_code('z',Z),
	 A =< C,
	 C =< Z}.

uppercase(C) -->
	[C],
	{char_code('A',A),
	 char_code('Z',Z),
	 A =< C,
	 C =< Z}.
underscore(C) -->
	[C],
	{char_code('_',C)}.

uppercase_or_underscore(C)-->
	uppercase(C),
	!.
uppercase_or_underscore(C)-->
	underscore(C).

alphanumeric_string([Code|MoreCodes]) -->
	alphanumeric(Code),
	!,
	alphanumeric_string_tail(MoreCodes).

alphanumeric_string_tail(String) -->
	alphanumeric_string(String),
	!.
alphanumeric_string_tail([]) -->
	[].

alphanumeric(Code) -->
	lowercase(Code).
alphanumeric(Code) -->
	uppercase(Code).
alphanumeric(Code) -->
	digit(Code).
alphanumeric(Code) -->
	[Code],
	{char_code('_',Code)}.

integer(Integer) -->
	digit_string(DigitString),
	{number_codes(Integer,DigitString)}.
signed_int(Integer) -->
    "+", integer(Integer).
signed_int(Integer) -->
    "-", integer(Neg),
    {Integer is 0-Neg}.

digit_string([Digit|MoreDigits]) -->
	digit(Digit),
	!,
	digit_string_tail(MoreDigits).

digit_string_tail(DigitStringTail) -->
	digit_string(DigitStringTail),
	!.
digit_string_tail([]) -->
	[].

digit(Digit) -->
	[Digit],
	{char_code('0',Zero),
	 char_code('9',Nine),
	 Zero =< Digit,
	 Digit =< Nine}.

float(Float) -->
	digit_string(DigitString1),
	".",
	digit_string(DigitString2),
	{char_code('.',FullStopCode),
	 append(DigitString1,[FullStopCode|DigitString2],DigitString),
	 number_codes(Float,DigitString)}.

number(Number) -->
	float(Number),
	!.
number(Number) -->
	integer(Number).


term(Term) -->
	funct(Functor),
	opening_parenthesis,
	!,
	term_list(Arguments),
	closing_parenthesis,
	{Term=..[Functor|Arguments]}.



term(TermList) -->
	"[",
	!,
	term_list(TermList),
	"]".
	
term(Term) -->
	constant(Term),
	!.
term(Term) -->
	variable(Term),
	!.

term1(Term) -->
	term(Term).
term1(Term) -->
	unary_operator_term(Term).
term1(Term) -->
	binary_operator_term(Term).

unary_operator_term(Term) -->
	oper(Operator),
	term(Term1),
	{concat_atoms(Operator,Term1,Term)}.

binary_operator_term(Term) -->
	term(Term1),
	oper(Operator),
	term(Term2),
	{concat_atoms(Term1,Operator,Term3),
	concat_atoms(Term3,Term2,Term)}.

term_list([Term|MoreTerms]) -->
	term1(Term),
	term_list_tail(MoreTerms).
term_list([]) -->
	[].

term_list_tail(TermListTail) -->
	comma,
	!,
	term_list(TermListTail).
term_list_tail([]) -->
	[].

atomic_constant_list([Const|MoreConsts]) -->
	atomic_constant(Const),
	!,
	atomic_constant_list(MoreConsts).
atomic_constant_list([]) -->
	[].


constant(Const) -->
	atomic_constant(Const).
constant(Const) -->
	number(Const).

oper(Operator) -->
	{member(Operator,[+,-,*,/]),
	 atom_codes(Operator,Codes)},
	Codes.
	 

time(Time) -->
	variable(Time).
time(Time) -->
	number(Time).


variable(Variable) -->
	uppercase_or_underscore(UpperCase),
	alphanumeric_string_tail(Alpha),
	{atom_codes(Variable,[UpperCase|Alpha])}.

concat_atoms(A1,A2,A3) :-
	atomo_codes(A1,S1),
	atomo_codes(A2,S2),
	append(S1,S2,S3),
	atomo_codes(A3,S3).

atomo_codes(A,S):-
	number(A),
	!,
	number_codes(A,S).
atomo_codes(A,S):-
	atom_codes(A,S).



%----------------------------------------------------------
% COMMENTS DCG
%----------------------------------------------------------
drop_comments(InputList, OutputList) :-
	phrase(elementList(OutputList),InputList).


elementList(MoreElements) -->
	comment,
	!,
	elementList(MoreElements).
elementList([Element|MoreElements]) --> 
	anElement(Element),
	!,
	elementList(MoreElements).
elementList([]) -->
	[].

anElement(Element) -->
	[Element].

comment -->
	commentStarter_sl, !, commentContent_sl.
comment -->
	commentStarter_ml, commentContent_ml.


%%% Comments on single-line

commentStarter_sl -->
	"//", !.
commentStarter_sl -->
	"%".

commentContent_sl -->
	commentEnder_sl, !.
commentContent_sl -->
	[_], commentContent_sl.

commentEnder_sl -->
	{atom_codes('\n', X)}, X.


%%% Comments on multi-lines...

commentStarter_ml -->
	"/*".

commentContent_ml -->
	commentEnder_ml, !.
commentContent_ml -->
	[_], commentContent_ml.

commentEnder_ml -->
	"*/".

