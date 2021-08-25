/* -*- Mode: Prolog -*- */

:- module(clif_parser,
	  [atom_cltext/2,
           file_to_cltext/2]).

:- use_module(cl_io).

:- multifile cl_io:parse_cltext_hook/4.
cl_io:parse_cltext_hook(File,clif,Text,_Opts) :-
        file_to_cltext(File,Text).

%% DGC Utility Predicates

zeroOrMore(F,In,Rest):-
        Head =.. [F,In,Rest1],
        Head,
        !,
        zeroOrMore(F,Rest1,Rest).
zeroOrMore(_,In,In):- !.

%% zeroOrMore( +F, ?List, +InToks, ?Rest )
zeroOrMore(F,[X|L],In,Rest):-
        Head =.. [F,X,In,Rest1],
        Head,
        !,
        zeroOrMore(F,L,Rest1,Rest).
zeroOrMore(_,[],In,In):- !.
oneOrMore(F,[X|L],In,Rest):-
        Head =.. [F,X,In,Rest1],
        Head,
        !,
        zeroOrMore(F,L,Rest1,Rest).
zeroOrOne(F,[X],In,Rest):-
        Head =.. [F,X,In,Rest],
        Head,
        !.
zeroOrOne(_,[],In,In):- !.

user:term_expansion( ( autoarg(F)),
                     [ ( Head :- Goal,append(V,Y,X) ) ]):-
        Goal =.. [F,X,Y],
        Head =.. [F,V,X,Y].
user:term_expansion( ( autoatom(F)),
                     [ ( Head :- Goal , append(V,Y,X),atom_codes( A,V) ) ]):-
        Goal =.. [F,X,Y],
        Head =.. [F,A,X,Y].
user:term_expansion( ( autocode(F)),
                     [ ( Head :- Goal , [V|Y]=X, atom_codes( A,[V]) ) ]):-
        Goal =.. [F,X,Y],
        Head =.. [F,A,X,Y].

%% A.2.2 Lexical syntax 
%% We make a distinction between lexical and syntactic constructs for convenience in dividing up the presentation 
%% into two parts. This sub-clause may help implementers in identifying logical tokens that make up syntactic 
%% expressions, as shown in the next sub-clause A.2.3. Implementations are not required to adhere to this 
%% distinction. 
%% A.2.2.1 White space 
%% whitechar = space U+0020 | tab U+0009 | line U+000A | page U+000C | return U+000D
%whitechar --> ['\u0020']. % '
%whitechar --> ['\u0009'].
%whitechar --> ['\u000A'].
%whitechar --> ['\u000C'].
%whitechar --> ['\u000D'].
whitechar --> " ".
whitechar --> "\u0020". % whitechar --> "\u0020". % '0
whitechar --> "\u0009".
whitechar --> "\u000A".
whitechar --> "\u000C".
whitechar --> "\u000D".

%% white = whitechar | 
%%  '/*' , {char - '*' | '*' , char - '/' | open | close | namequote | stringquote |
%% backslash | whitechar }, ['*'] , '*/' | 
%%   '//' {char | open | close | namequote | stringquote | backslash | space | tab }, 
%% (page | line | return)  ;
white --> whitechar.
white --> "/*",zeroOrMore(white_1),opt_asterisk,"*/".
white --> "//",zeroOrMore(white_2), (page ; line ; return).

white_1 --> char(C),{C\="*"}.
white_1 --> "*",char(C),{C\="/"}.
white_1 --> open ; close_t ; namequote ; stringquote ; backslash ; whitechar.

white_2 --> char ; open ; close_t ; namequote ; stringquote ; backslash ; space ; ws_tab.

space --> " ".
page --> [12].
return --> "\n".
line --> "\r".
ws_tab --> "\t".

opt_asterisk --> "*".
opt_asterisk --> [].

%% This allows temporary comments to be inserted into CLIF text, following C++/Java conventions. Text on a line 
%% after '//', and entire text blocks surrounded by '/* ... */', are treated as whitespace by any CLIF parser. 
%% The quoting sequences '//',  '/*' and '*/'  trigger this production only when they occur outside a quoted 
%% string or enclosed name. Names in CLIF text which contain the character sequences '//', '/*' or '*/' 
%% should therefore be written as enclosed names. 
%% Temporary comments are distinct from CL comments, which are a permanent part of the CLIF parsed text. 
%% Since they are counted as whitespace, temporary comments act as lexical break characters.

%% A.2.2.2 Delimiters 
%% Single quote (apostrophe) is used to delimit quoted strings, and double quote to delimit enclosed names, 
%% which obey special lexicalization rules. Quoted strings and enclosed names are the only CLIF lexical items 
%% which can contain whitespace and parentheses. Parentheses elsewhere are self-delimiting; they are 
%% considered to be lexical tokens in their own right. Parentheses are the primary grouping device in CLIF syntax. 
%% open = '(' ;
open --> "(".

%% close = ')' ; 
close_t --> ")".

%% stringquote = ''' ;
stringquote --> "'".

%% namequote =  '"' ; 
namequote --> "\"".

%% backslash =  '\' ; 
backslash --> "\\".

%% A.2.2.3 Characters 
%% char is all the remaining ASCII non-control characters, which can all be used to form lexical tokens (with some 
%% restrictions based on the first character of the lexical token). This includes all the alphanumeric characters. 
%% char = digit | '~' | '!' | '#' | '$' | '%' | '^' | '&' | '*' | '_' | '+' | '{' | '}' | 
%% '|' | ':' |   '<' | '>' | '?' | '`' | '-' | '=' | '[' | ']' |  ';'| ',' | '.' | 
%% '/' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' 
%% | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | 
%% 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' 
%% | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' ;
char --> digit ; "~" ; "!" ; "#" ; "$" ; "%" ; "^" ; "&" ; "*" ; "_" ; "+" ; "{" ; "}" ; 
 "|" ; ":" ;   "<" ; ">" ; "?" ; "`" ; "-" ; "=" ; "[" ; "]" ;  ";"; "," ; "." ; 
 "/" ; "A" ; "B" ; "C" ; "D" ; "E" ; "F" ; "G" ; "H" ; "I" ; "J" ; "K" ; "L" ; "M" 
 ; "N" ; "O" ; "P" ; "Q" ; "R" ; "S" ; "T" ; "U" ; "V" ; "W" ; "X" ; "Y" ; "Z" ; 
 "a" ; "b" ; "c" ; "d" ; "e" ; "f" ; "g" ; "h" ; "i" ; "j" ; "k" ; "l" ; "m" ; "n" 
 ; "o" ; "p" ; "q" ; "r" ; "s" ; "t" ; "u" ; "v" ; "w" ; "x" ; "y" ; "z" .
%% CJM: allow @ in char
char --> "@".
autocode(char).


%% digit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ; 
digit --> "0" ; "1" ; "2" ; "3" ; "4" ; "5" ; "6" ; "7" ; "8" ; "9" .   % "0
autocode(digit).


%% hexa = digit | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' ;
hexa --> digit ; "A" ; "B" ; "C" ; "D" ; "E" ; "F" ; "a" ; "b" ; "c" ; "d" ; "e" ; "f" .
autocode(hexa).

%% A.2.2.4 Quoting within strings 
%% Certain character sequences are used to indicate the presence of a single character. nonascii is the set of 
%% characters or character sequences which indicate a Unicode character outside the ASCII range. 
%% NOTE For input using a full Unicode character encoding, this production should be ignored and nonascii should be 
%% understood instead to be the set of all non-control characters of Unicode outside the ASCII range which are supported by 
%% the character encoding. The use of the \uxxxx and \Uxxxxxx sequences in text encoded using a full Unicode character 
%% repertoire is deprecated. 
%% innerstringquote is used to indicate the presence of a single-quote character inside a quoted string. A quoted 
%% string can contain any character, including whitespace; however, a single-quote character can occur inside a 
%% quoted string only as part of an innerstringquote, i.e. when immediately preceded by a backslash character. 
%% The occurrence of a single-quote character in the character stream of a quoted string marks the end of the 
%% quoted string lexical token unless it is immediately preceded by a backslash character. Inside enclosed name 
%% strings, double quotes are treated exactly similarly. Innernamequote is used to indicate the presence of a 
%% double-quote character inside an enclosed name. 
%% nonascii = '\u' , hexa, hexa, hexa, hexa | '\U' , hexa, hexa, hexa, hexa, hexa, hexa ;
nonascii --> "\\u", hexa, hexa, hexa, hexa ; "\\U" , hexa, hexa, hexa, hexa, hexa, hexa .
autoatom(nonascii).

%% innerstringquote = '\'' ; 
innerstringquote --> "\\'" .

%% innernamequote = '\"' ; 
innernamequote --> "\\\""  .

%% innerbackslash = ‘\\’ 
innerbackslash --> "\\\\" .

%% numeral = digit , { digit } ;
numeralseq([D|L])--> digit(D), zeroOrMore(digit,L).
numeral( numeral(Num) ) --> numeralseq(L),{concat_atom(L,A),atom_number(A,Num)}.

%% Sequence markers are a distinctive syntactic form with a special meaning in Common Logic. Note that a bare 
%% ellipsis without any text (i.e., '...') is itself a sequence marker. 
%% seqmark = '...' , { char } ;
seqmark( seqmark(A) ) --> "...", zeroOrMore(char,Chars),{concat_atom(Chars,A)}.


%% Single quotes are delimiters for quoted strings; double quotes for enclosed names. 
%% An enclosed name is simply a name which may contain characters which would break the lexicalization, such 
%% as “Mrs Norah Jones” or “Girl(interrupted)”; like any other name, it may denote anything. The surrounding 
%% double-quote marks are not considered part of the discourse name, which is defined to be the character string 
%% obtained by removing the enclosing double-quote marks and replacing any internal occurrences of an 
%% innernamequote by a single double-quote character. It is recommended to use the enclosed-name syntax 
%% when writing URIs, URI references and IRIs as names, since these Web identifiers may contain characters 
%%  
%% which would otherwise break CLIF lexicalization: in particular, Xpath-compliant URI references will often end 
%% in a closing parenthesis. 
%% A quoted string, in contrast, is an expression with a fixed semantic meaning: it denotes a text string similarly 
%% related to the string inside the quotes.


%% A.2.2.5 Quoted strings 
%% Quoted strings and enclosed names require a different lexicalization algorithm than other parts of CLIF text, 
%% since parentheses and whitespace do not break a quoted text stream into lexical tokens. 
%% When CLIF text is enclosed inside a text or document which uses character escaping conventions, the 
%% Common Logic quoted string conventions here described are understood to apply to the text described or 
%% indicated by the conventions in use, which should be applied first. Thus for example the content of the XML 
%% element: <cl-text>&apos;a\&apos;b&lt;c&apos</cl-text> is the CLIF syntax quoted string 'a\'b<c' 
%% which denotes the five-character text string a'b<c . Considered as bare CLIF text, however, 
%% &apos;a\&apos;b&lt;c&apos would simply be a rather long name. 
%% quotedstring = stringquote, { white | open | close | char | nonascii | namequote | 
%% innerstringquote | innerbackslash }, stringquote ;
quotedstring( quotedstring(S) ) --> stringquote, zeroOrMore(quotedstring_char,Codes), stringquote,{atom_codes(S,Codes)}.
quotedstring_char -->  white ; open ; close_t ; char ; nonascii ; namequote ; innerstringquote ; innerbackslash.
autoatom(quotedstring_char).

%% enclosedname  = namequote, { white | open | close | char | nonascii | stringquote | 
enclosedname( enclosedname(N) )--> namequote, zeroOrMore(enclosedname_char,Codes),  namequote,{atom_codes(N,Codes)}.
enclosedname_char  --> white ; open ; close_t ; char ; nonascii ; stringquote ; innernamequote.
autoatom(enclosedname_char).

%% A.2.2.6 Reserved tokens 
%% reservedelement consists of the lexical tokens which are used to indicate the syntactic structure of Common 
%% Logic expressions. These may not be used as names in CLIF text. 
%% reservedelement = '='  |  'and'  |  'or'  |  'iff'  |  'if'  |  'forall'  |  'exists'  |  'not'  |  'roleset:'  |  
%% ‘cl-text‘  |  'cl-imports'  |  'cl-excludes'  |  'cl-module'  |  'cl-comment' ; 

reservedelement --> "="  ;  "and"  ;  "or"  ;  "iff"  ;  "if"  ;  "forall"  ;  "exists"  ;  "not"  ;  "roleset:"  ;  
 "cl-text"  ;  "cl-imports"  ;  "cl-excludes"  ;  "cl-module"  ;  "cl-comment" .

autoatom(reservedelement).
% NOTE TODO: why backticks for cl-text

%% A.2.2.7 Name character sequence 
%% A namecharsequence is a lexical token which does not start with any of the special characters. Note that 
%% namecharsequences may not contain whitespace or parentheses, and may not start with a quote mark 
%% although they may contain them. Numerals and sequence markers are not namecharsequences. 
%% namecharsequence = ( char , { char | stringquote | namequote | backslash } ) - ( reservedelement  | numeral | seqmark ) ;
namecharsequence( namecharsequence(A) ) --> char(C),zeroOrMore(namecharsequence_char,L),{flatten([C,L],L2),atom_codes(A,L2)}.
namecharsequence_char --> stringquote ; namequote ; backslash.
namecharsequence_char(C) --> char(C).
autoatom(namecharsequence_char).

 % TODO
        
%% A.2.2.8 Lexical categories 
%% The task of a lexical analyser is to parse the character stream into consecutive, non-overlapping lexbreak and 
%% nonlexbreak strings, and to deliver the lexical tokens it finds as a stream of tokens to the next stage of 
%% syntactic processing. Lexical tokens are divided into eight mutually disjoint categories: the open and closing 
%% parentheses, numerals, quoted strings (which begin and end with '''), sequence markers (which begin with 
%% '...'), enclosed names (which begin and end with '"') , and namesequences and reserved elements. 
%% lexbreak  = open | close | white , { white } ; 
%lexbreak  --> open ; close_t ; white ;  zeroOrMore(white). % CHECK
lexbreak(X) --> (open,{X=open} | close_t,{X=close} | white,{X=white} ),  zeroOrMore(white).

%% nonlexbreak = numeral | quotedstring | seqmark | reservedelement | namecharsequence | 
nonlexbreak( X)--> numeral( X); quotedstring( X); seqmark( X); reservedelement( X); namecharsequence( X); enclosedname(X).

%% lexicaltoken  = open | close | nonlexbreak ; 
lexicaltoken(open) --> open.
lexicaltoken(close) --> close_t.
lexicaltoken(  X) --> nonlexbreak(X).

%% charstream = { white } , { lexicaltoken, lexbreak }  ; 
charstream( L) -->  zeroOrMore(white) , zeroOrMore(charstream_tokens, L).
% charstream_tokens( T)-->  lexicaltoken(T), lexbreak.
charstream_tokens( T)-->  lexicaltoken(T). % CHECK ME

% e.g.: "(cl-comment 'xx')
% any nonlexbreak must be followed by a lexbreak

lexicaltokens(X) --> white,lexicaltokens(X).
lexicaltokens([X,Y|L]) --> nonlexbreak(X),lexbreak(Y),lexicaltokens(L).
lexicaltokens([open|L]) --> open,lexicaltokens(L).
lexicaltokens([close|L]) --> close_t,zeroOrMore(white),lexicaltokens(L).
lexicaltokens([]) --> [].


%% END OF LEXICAL SYNTAX


% for expressions, open and close are tokens not strings
open_tok --> [open].
close_tok --> [close].


%% A.2.3  Expression syntax 
%% This part of the syntax is written so as to apply to a sequence of Common Logic lexical tokens rather than a 
%% character stream. 
%% A.2.3.1 Term sequence 
%% Both terms and atomic sentences use the notion of a sequence of terms representing a vector of arguments to 
%% a function or relation. Sequence markers are used to indicate a subsequence of a term sequence; terms 
%% indicate single elements. 
%% termseq = { term  | seqmark } ;
termseq(L) --> zeroOrMore(termseq_token,L).
termseq_token( T)--> term( T) ; seqmark(T).

%% A.2.3.2 Name 
%% A name is any lexical token which is understood to denote. We distinguish the names which have a fixed 
%% meaning from those which are given a meaning by an interpretation. 
%% interpretedname =  numeral | quotedstring ; 
interpretedname( N) -->  [numeral(N)] ; [quotedstring( N)].

%% interpretablename =  namecharsequence | enclosedname ; 
interpretablename( N) -->  [namecharsequence(N)] ; [enclosedname(N)].

%% name = interpretedname | interpretablename ; 
name( N) --> interpretedname( N) ; interpretablename(N).

%% A.2.3.3 Term 
%% Names count as terms, and a complex (application) term consists of an operator, which is itself a term, 
%% together with a vector of arguments. Terms may also have an associated comment, represented as a quoted 
%% string (in order to allow text which would otherwise break the lexicalization). Comment wrappers syntactically 
%% enclose the term they comment upon. 
%% term = name | ( open_tok, operator, termseq, close ) | ( open_tok, 'cl-comment', quotedstring 
%% , term, close ) ; 
term(T) --> name( T).
term(T) --> open_tok, operator(OpT), termseq(Ts), close_tok,{T=..[OpT|Ts]}.
term( '$comment'(C,T) ) --> open_tok, ['cl-comment'], [quotedstring(C)], term(T), close_tok.

%% operator = term  ; 
operator(Op) --> term(Op).

%% A.2.3.4 Equation 
%% Equations are distinguished as a special category because of their special semantic role, and special handling 
%% by many applications. The equality sign is not a name. 
%% equation = open_tok, '=', term, term, close ;
equation( A=B) --> open_tok, ['='], term(A), term(B), close_tok .


%% A.2.3.5 Sentence 
%% Like terms, sentences may have enclosing comments. Note that comments may be applied to sentences 
%% which are subexpressions of larger sentences. 
%% sentence = atomsent | boolsent | quantsent | commentsent ; 
sentence( S)--> atomsent( S) ; boolsent( S) ; quantsent( S) ; commentsent( S).


%% A.2.3.6 Atomic sentence 
%% Atomic sentences are similar in structure to terms, but in addition the arguments to an atomic sentence may be 
%% represented using role-pairs consisting of a role-name and a term. Equations are considered to be atomic 
%% sentences, and an atomic sentence may be represented using role-pairs consisting of a role-name and a term. 
%% atomsent = equation | atom ; 
atomsent( S)--> equation( S) ; atom( S).


%% atom = ( open_tok, predicate , termseq, close ) | ( open_tok, term, open_tok, 'roleset:' , { open_tok,  name, term, close }, close, close ) ; 
atom( A) --> ( open_tok, predicate( P), termseq(S),{A=..[P|S]}, close_tok ); ( open_tok, term(T), open_tok, ['roleset:'] , zeroOrMore(roleset,Ts),{A=..[roleset,T|Ts]},close_tok, close_tok ) .
roleset(N-T) --> open_tok, name(N), term(T), close_tok.

%% predicate =  term ;
predicate( T)-->  term( T).
               
%%  
%% A.2.3.7 Boolean sentence 
%% Boolean sentences require implication and biconditional to be binary, but allow conjunction and disjunction to 
%% have any number of arguments, including zero; the sentences (and) and (or) can be used as the truth-values 
%% true and false respectively. 
%% boolsent = ( open_tok, ('and' | 'or') , { sentence }, close ) | ( open_tok, ('if' | 'iff') , 
%% sentence , sentence, close ) | ( open_tok, 'not' , sentence, close ;
% MISSING CLOSING BRACKET IN SPEC
boolsent( S ) -->  open_tok, boolean_binary_operator(Op), zeroOrMore(sentence,SL), close_tok, {S=..[Op|SL]}.
boolsent( S ) --> open_tok, implication( Op), sentence( A), sentence(B), close_tok, {S=..[Op,A,B]}.
boolsent( not(S) ) --> (   open_tok, [not] , sentence(S), close_tok ).

% supporting named clauses:
boolean_binary_operator(and) --> [and].
boolean_binary_operator(or) --> [or].
implication(iff) --> [iff].
implication(if) --> [if].



%% A.2.3.8 Quantified sentence 
%% Quantifiers may bind any number of variables and may be guarded; and bound variables may be restricted to 
%% a category indicated by a term. 
%% quantsent  = open_tok, ('forall' | 'exists') , [ interpretablename ] , boundlist, 
%% sentence, close ; 
quantsent(QS) --> open_tok, quantifier(Q), zeroOrOne(interpretablename,_NamesTODO) , boundlist(BL), sentence(S), close_tok,  {QS=.. [Q,BL,S]}.

quantifier(forall) --> [forall].
quantifier(exists) --> [exists].

%% boundlist  = open_tok, { interpretablename |  seqmark | ( open_tok, (interpretablename | seqmark), term, close )} , close ; 
boundlist(  BL) --> open_tok, zeroOrMore(boundlist_internal,BL), close_tok.
boundlist_internal(B) --> interpretablename( B).
boundlist_internal(B) --> seqmark(B).
boundlist_internal(named(N,T)) --> open_tok, ( interpretablename(N) ; seqmark(N) ), term(T), close_tok. % ?? TODO

%% A.2.3.9 Commented sentence 
%% A comment may be applied to any sentence; so comments may be attached to sentences which are 
%% subexpressions of larger sentences. 
%% commentsent = open_tok, 'cl-comment', quotedstring , sentence , close ; 
% commentsent(comment(Str,S)) --> open_tok, ['cl-comment'], [quotedstring( Str)], sentence( S), close_tok .
%% SUGGESTION Dec1 2008: commentsent = open_tok, 'cl-comment', ( quotedstring | enclosedname) , sentence , close ;    
commentsent('$comment'(Str,S)) --> open_tok, ['cl-comment'], ( [quotedstring( Str)] ; [enclosedname(Str)] ), sentence( S), close_tok .

%% A.2.3.10 Module 
%% Modules are named text segments which represent a text intended to be understood in a ‘local’ context, where 
%% the name indicates the domain of the quantifiers in the text. The module name shall not be a numeral or a 
%% quoted string. A module may optionally have an exclusion list of names whose denotations are considered to 
%% be excluded from the domain. Note that text and module are mutually recursive categories, so that modules 
%% may be nested. 
%module =  open_tok, 'cl-module' , interpretablename ,  [open_tok, 'cl-excludes' , {name}  , close ], cltext, close.
module( module(N,Excls,Txt) ) -->  open_tok, ['cl-module'] , interpretablename( N),  module_excludes_opt(Excls), cltext(Txt), close_tok.
module_excludes_opt( Names) --> open_tok, ['cl-excludes'] , zeroOrMore(name,Names), close_tok .
module_excludes_opt( []) --> [].


%% A module without an exclusion list is not identical to a named text. 
%% A.2.3.11 Phrase 
%% CLIF text is a sequence of phrases, each of which is either a sentence, a module, an importation or a plain 
%% text with an attached comment. The commented text may be empty, or may be a single sentence. Text may 
%% be assigned a name in the same way as a module, but in this case the name serves only to identify the text 
%% and does not restrict the universe of discourse. A single module may also be treated as a text. Any name 
%% assigned to a named text or a module, and any name occurring inside an importation, shall be a network 
%% identifier. For Web applications at the time of writing, it should be an IRI [2]. Particular applications may 
%% impose additional conditions on names used as identifiers. The only nonterminal character for this grammar is 
%% <code>cltext</code>. 
%% phrase = sentence | module | (open_tok, 'cl-imports' , interpretablename , close) | (open_tok, 'cl-comment', quotedstring, cltext, close);
phrase(P) --> sentence( P) ; module(P).
phrase(imports(N)) --> open_tok, ['cl-imports'] , interpretablename( N), close_tok.
phrase('$comment'(C,T)) --> open_tok, ['cl-comment'], [quotedstring(C)], cltext(T), close_tok.
%phrase(_,In,_Rest) :- throw(parse('cannot parse phrase: ~w',[In])).


%% ORIGINAL: cltext = { phrase }  ; 
%% CORRIGENDUM: text = { phrase }  ; 
text(cltext(L)) --> zeroOrMore(phrase,L).

%% namedtext = open_tok, 'cl-text' interpretablename, text, close ;
% MISSING COMMA IN SPEC
namedtext(namedtext(N,T)) --> open_tok, ['cl-text'], interpretablename(N), text(T), close_tok.
% not clear where this fits in in figure 1
% in XCL?

%% cltext = module | namedtext | text ; 
cltext( T) --> module( T) ; namedtext( T) ; text( T).


% CJM
%text(L) --> zeroOrMore(phrase,L).

% -------------------- UTILITY PREDICATES --------------------

%% parse
% utility predicate for REPL
parse:-
	read_stream_to_codes(user_input,Codes),
	codes_cltext(Codes,Text),
	writeln(Text).

%% atom_cltext(+Atom,?CLText) is semidet
%
% parses CLIF
%
% @param Atom text in CLIF syntax
% @param CLText text is prolog term
atom_cltext(A,Text):-
	atom_codes(A,Codes),
	codes_cltext(Codes,Text).
codes_cltext(Codes,Text):-
	(   lexicaltokens(Tokens,Codes,[])
	->  true
	;   atom_codes(A,Codes),
	    throw(cannot_tokenize(A))),
	remove_white(Tokens,Tokens2), % TODO - should not be necessary
	debug(commonlogic,'Tokens=~w',[Tokens2]),
	cltext(Text,Tokens2,[]).

file_to_cltext(File,Text) :-
        read_file_to_codes(File,Codes,[]),
        codes_cltext(Codes,Text).


remove_white([],[]).
remove_white([white|L],L2):- !,remove_white(L,L2).
remove_white([H|L],[H|L2]):- !,remove_white(L,L2).


/** <module> parser for ISO Common Logic

  ---+ Synopsis

==
:- use_module(library('cltools/clif_parser')).

% 
demo:-
  nl.
  

==

---+ Details

----+ Common Logic Standard

http://common-logic.org/

Implements:

Technical Corrigendum ISO/IEC IS 24707:2007/DCOR:1

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
