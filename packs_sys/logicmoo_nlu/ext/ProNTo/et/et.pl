% et.pl - M. Covington      2003 February 12

% ET the Efficient Tokenizer

% Measured speed: On a 1-GHz Pentium III,
% about 6 seconds per megabyte of text tokenized,
% or about three lines of text per millisecond.

% New in this version: Special handling of apostrophes.
% The apostrophe is a whitespace character except in
% the sequence 't which is treated as just t,
% making morphological analysis easier.


%%
%% User-callable routines
%%

% tokens_words(+Tokens,-Words)
%  From the output of the other routines, extracts just
%  the word tokens and converts them to atoms.

tokens_words([],[]).

tokens_words([w(Chars)|Tokens],[Atom|Atoms]) :-
   !,
   atom_chars(Atom,Chars),
   tokens_words(Tokens,Atoms).

tokens_words([_|Tokens],Atoms) :-
   % skip non-word tokens
   tokens_words(Tokens,Atoms).



% tokenize_file(+Filename,-Tokens)
%  Reads and entire file and tokenizes it.

tokenize_file(Filename,Tokens) :-
   open(Filename,read,Stream),
   tokenize_stream(Stream,Tokens),
   close(Stream).


% tokenize_stream(+Stream,-Tokens)
%  Reads an entire stream and tokenizes it.

tokenize_stream(Stream,[]) :-
   at_end_of_stream(Stream),
   !.

tokenize_stream(Stream,Tokens) :-
   tokenize_line_dl(Stream,Tokens/Tail),
   tokenize_stream(Stream,Tail).
   

% tokenize_line(+Stream,-Tokens)
%  Reads a line of input and returns a list of tokens.

tokenize_line(Stream,Tokens) :-
   tokenize_line_dl(Stream,Tokens/[]).


% tokenize_line_dl(+Stream,-Tokens/Tail)
%  Like tokenize_line, but uses a difference list.
%  This makes it easier to append the results of successive calls.

tokenize_line_dl(Stream,Tail/Tail) :-
   at_end_of_stream(Stream),                        % unnecessary test?
   !.

tokenize_line_dl(Stream,Dlist) :-
   get_char_and_type(Stream,Char,Type),
   tokenize_line_x(Type,Char,Stream,Dlist).


%%
%% Auxiliary predicates for tokenization
%%

% get_char_and_type(+Stream,-Char,-Type)
%  Reads a character, determines its type, and translates
%  it as specified in char_type_char.

get_char_and_type(Stream,Char,Type) :-
   get_char(Stream,C),
   char_type_char(C,Type,Char).


% tokenize_line_x(+Type,+Char,+Stream,-Tokens/Tail)
%  Tokenizes (the rest of) a line of input.
%  Type and Char describe the character that has been read ahead.

tokenize_line_x(eol,_,_,Tail/Tail) :-               % end of line mark; terminate
   !.

tokenize_line_x(whitespace,_,Stream,Dlist) :-       % whitespace, skip it
   !,
   tokenize_line_dl(Stream,Dlist).


% Word tokens and number tokens have to be completed,
% maintaining 1 character of read-ahead as this is done.
% NewChar and NewType are the character read ahead
% after completing the token.

tokenize_line_x(letter,Char,Stream,[w(T)|Tokens]/Tail) :-
   !,
   tokenize_letters(letter,Char,Stream,T,NewType,NewChar),
   tokenize_line_x(NewType,NewChar,Stream,Tokens/Tail).

tokenize_line_x(digit,Char,Stream,[n(T)|Tokens]/Tail) :-
   !,
   tokenize_digits(digit,Char,Stream,T,NewType,NewChar),
   tokenize_line_x(NewType,NewChar,Stream,Tokens/Tail).


% A period is handled like a digit if it is followed by a digit.
% This handles numbers that are written with the decimal point first.

tokenize_line_x(_, '.', Stream,Dlist) :-
   peek_char(Stream,P),
   char_type_char(P,digit,_),
   !,
   % Start over, classifying '.' as a digit
   tokenize_line_x(digit, '.', Stream,Dlist).


% Special characters and unidentified characters are easy:
% they stand by themselves, and the next token begins with
% the very next character.

tokenize_line_x(special,Char,Stream,[s(Char)|Tokens]/Tail) :-   % special char
   !,
   tokenize_line_dl(Stream,Tokens/Tail).

tokenize_line_x(_,Char,Stream,[other(Char)|Tokens]/Tail) :-     % unidentified char
   !,
   tokenize_line_dl(Stream,Tokens/Tail).



% tokenize_letters(+Type,+Char,+Stream,-Token,-NewChar,-NewType)
%   Completes a word token beginning with Char, which has
%   been read ahead and identified as type Type.
%   When the process ends, NewChar and NewType are the
%   character that was read ahead after the token.

tokenize_letters(letter,Char,Stream,[Char|Rest],NewType,NewChar) :-
   % It's a letter, so process it, read another character ahead, and recurse.
   !,
   get_char_and_type(Stream,Char2,Type2),
   tokenize_letters(Type2,Char2,Stream,Rest,NewType,NewChar).

tokenize_letters(_,'''',Stream,Rest,NewType,NewChar) :-
   %
   % Absorb an apostrophe, but only when it precedes t.
   % This keeps words together like doesn't, won't.
   %
   peek_char(Stream,t),
   !,
   get_char(Stream,_),
   tokenize_letters(letter,t,Stream,Rest,NewType,NewChar).

tokenize_letters(Type,Char,_,[],Type,Char).
   % It's not a letter, so don't process it; pass it to the calling procedure.


% tokenize_digits(+Type,+Char,+Stream,-Token,-NewChar,-NewType)
%   Like tokenize_letters, but completes a number token instead.
%   Additional subtleties for commas and decimal points.

tokenize_digits(digit,Char,Stream,[Char|Rest],NewType,NewChar) :-
   % It's a digit, so process it, read another character ahead, and recurse.
   !,
   get_char_and_type(Stream,Char2,Type2),
   tokenize_digits(Type2,Char2,Stream,Rest,NewType,NewChar).

tokenize_digits(_, ',', Stream,Rest,NewType,NewChar) :-
   peek_char(Stream,P),
   char_type_char(P,digit,Char2),
   !,
   % It's a comma followed by a digit, so skip it and continue.
   get_char(Stream,_),
   tokenize_digits(digit,Char2,Stream,Rest,NewType,NewChar).

tokenize_digits(_, '.', Stream,['.'|Rest],NewType,NewChar) :-
   peek_char(Stream,P),
   char_type_char(P,digit,Char2),
   !,
   % It's a period followed by a digit, so include it and continue.
   get_char(Stream,_),
   tokenize_digits(digit,Char2,Stream,Rest,NewType,NewChar).

tokenize_digits(Type,Char,_,[],Type,Char).
   % It's not any of those, so don't process it;
   % pass it to the calling procedure.





%%
%% Character classification
%%

% char_type_char(+Char,-Type,-TranslatedChar)
%   Classifies all characters as letter, digit, special, etc.,
%   and also translates each character into the character that
%   will represent it, converting upper to lower case.

char_type_char(Char,Type,Tr) :-
   char_table(Char,Type,Tr),
   !.

char_type_char(Char,special,Char).

% End of line marks
char_table(end_of_file, eol, end_of_file).
char_table('\n',        eol, '\n'       ).

% Whitespace characters
char_table(' ',     whitespace,  ' ').     % blank
char_table('\t',    whitespace,  ' ').     % tab
char_table('\r',    whitespace,  ' ').     % return
char_table('''',    whitespace, '''').     % apostrophe does not translate to blank

% Letters 
char_table(a,     letter,    a ).
char_table(b,     letter,    b ).
char_table(c,     letter,    c ).
char_table(d,     letter,    d ).
char_table(e,     letter,    e ).
char_table(f,     letter,    f ).
char_table(g,     letter,    g ).
char_table(h,     letter,    h ).
char_table(i,     letter,    i ).
char_table(j,     letter,    j ).
char_table(k,     letter,    k ).
char_table(l,     letter,    l ).
char_table(m,     letter,    m ).
char_table(n,     letter,    n ).
char_table(o,     letter,    o ).
char_table(p,     letter,    p ).
char_table(q,     letter,    q ).
char_table(r,     letter,    r ).
char_table(s,     letter,    s ).
char_table(t,     letter,    t ).
char_table(u,     letter,    u ).
char_table(v,     letter,    v ).
char_table(w,     letter,    w ).
char_table(x,     letter,    x ).
char_table(y,     letter,    y ).
char_table(z,     letter,    z ).
char_table('A',   letter,    a ).
char_table('B',   letter,    b ).
char_table('C',   letter,    c ).
char_table('D',   letter,    d ).
char_table('E',   letter,    e ).
char_table('F',   letter,    f ).
char_table('G',   letter,    g ).
char_table('H',   letter,    h ).
char_table('I',   letter,    i ).
char_table('J',   letter,    j ).
char_table('K',   letter,    k ).
char_table('L',   letter,    l ).
char_table('M',   letter,    m ).
char_table('N',   letter,    n ).
char_table('O',   letter,    o ).
char_table('P',   letter,    p ).
char_table('Q',   letter,    q ).
char_table('R',   letter,    r ).
char_table('S',   letter,    s ).
char_table('T',   letter,    t ).
char_table('U',   letter,    u ).
char_table('V',   letter,    v ).
char_table('W',   letter,    w ).
char_table('X',   letter,    x ).
char_table('Y',   letter,    y ).
char_table('Z',   letter,    z ).

% Digits
char_table('0',   digit,     '0' ).
char_table('1',   digit,     '1' ).
char_table('2',   digit,     '2' ).
char_table('3',   digit,     '3' ).
char_table('4',   digit,     '4' ).
char_table('5',   digit,     '5' ).
char_table('6',   digit,     '6' ).
char_table('7',   digit,     '7' ).
char_table('8',   digit,     '8' ).
char_table('9',   digit,     '9' ).

% Everything else is a special character.
