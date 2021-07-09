% ===================================================================
% File 'parser_all.pl'
% Purpose: English to KIF conversions from SWI-Prolog  
% This implementation is incomplete
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'parser_all.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2002/06/06 15:43:15 $
% ===================================================================

:- module(parser_tokenize,[into_text80/2,into_acetext/2,any_nb_to_atom/2,foc_framevar/3,foc_framevar2/2]).

%into_acetext(Input,AceText):- atomic(Input), !, tokenizer:tokenize(Input, Tokens), into_acetext(Tokens,AceText).
%into_acetext(Input,AceText):- into_acetext(Input,AceText).

:- use_module(parser_sharing).

:- use_module('../../ext/ape/prolog/parser/tokenizer').

into_acetext(Tokens,AceText):- 
   notrace((into_text80(Tokens,TokensP),tokens_to_acetext0(TokensP,AceText))).

tokens_to_acetext0([],'').
tokens_to_acetext0(ListIn,Out):-  notrace((member(T,ListIn), \+ atom(T))), !, maplist(any_nb_to_atom,ListIn,List),tokens_to_acetext0(List,Out).
tokens_to_acetext0([T],T):-!.
tokens_to_acetext0([T,':',P|Tokens],AceText):- atomic_list_concat([T,(:),P],'',TP),!,tokens_to_acetext0([TP|Tokens],AceText).
tokens_to_acetext0([T,P|Tokens],AceText):- atom_length(P,1),char_type_punct(P),!,atom_concat(T,P,TP),tokens_to_acetext0([TP|Tokens],AceText).
tokens_to_acetext0([T,P],AceText):- atomic_list_concat([T,P],' ',AceText),!.
tokens_to_acetext0([T,P|Tokens],AceText):- atomic_list_concat([T,P],' ',TP),!,tokens_to_acetext0([TP|Tokens],AceText).


into_text80(I,O):- into_text80_atoms(I,O).
   
into_text80_atoms(I,O):- nonvar(I), parser_tokenize:(init_to_tokens(I,T),!,fast_break_atom_symbols(T,O)),!.

into_text80_string_list(I,O):- into_text80_atoms(I,M),maplist(any_to_string,M,O),!.

into_text80_string(I,O):- into_text80_string_list(I,M), any_to_string(M, O).

%number_to_nb(nb(N),nb(N)):-!.
%number_to_nb(A,nb(N)):- atom(A),atom_number(A,N),!.
%number_to_nb(N,nb(N)):- number(N),!.
number_to_nb(A,A).


unquoted(I,S):- atom_concat('"',R,I),atom_concat(S,'"',R).
requoted(I,S):- format(atom(S),'"~w"',I).

%keep_unbroken(I):- atom_ upcase_atom(I),!.
keep_unbroken(I):- \+ atom(I),!.
keep_unbroken(I):- atom_concat('#$',_,I).

fast_break_atom_symbols(I,O):- break_atom_symbols(I,O),!.
%fast_break_atom_symbols(I,I).

break_atom_symbols([],[]).
break_atom_symbols([I,'\'','t'|List],[S,'n\'t'|ListO]):- \+ keep_unbroken(I), !, 
   atom_concat(S,'n',I),
   break_atom_symbols(List,ListO).

break_atom_symbols(['\'',I|List],[S|ListO]):- \+ keep_unbroken(I), !, 
   atom_concat('\'',I,S),
   break_atom_symbols(List,ListO).

break_atom_symbols(['?',I|List],[S|ListO]):- \+ keep_unbroken(I), !, 
   atom_concat('XVAR',I,S),
   break_atom_symbols(List,ListO).

break_atom_symbols([I|List],[I|ListO]):- keep_unbroken(I), !, 
   break_atom_symbols(List,ListO).

break_atom_symbols([I|List],[O|ListO]):-  unquoted(I,S),!,
  into_acetext(S,A),
  requoted(A,O),!,
  break_atom_symbols(List,ListO).
break_atom_symbols([Pos,':',Word|List],[O|ListO]):-  atomic_list_concat([Pos,':',Word],'',O), !,
  break_atom_symbols(List,ListO).
break_atom_symbols([I|List],ListO):- atom_length(I,N), N>1, split_symbols(I,O),!, 
  append(O,List,ListM), 
  break_atom_symbols(ListM,ListO).
break_atom_symbols([I|List],[I|ListO]):- !, break_atom_symbols(List,ListO).

split_symbols(I,[O]):- char_type_space(S),(atom_concat(S,O,I);atom_concat(O,S,I)),!.
split_symbols(I,[O1,O2|List]):- char_type_space(S),atomic_list_concat([O1,O2|List],S,I),!.
split_symbols(I,[S,O]):- split_from_start(S),atom_concat(S,O,I),!.
split_symbols(I,[S,O]):- split_from_end(O),atom_concat(S,O,I),!.
split_symbols(I,[O1,S,O2]):- split_from_mid(S),atomic_list_concat([O1,O2],S,I),!.


char_type_space( ' '). char_type_space('\r'). char_type_space('\n').
char_type_space('\t'). char_type_space('\v'). char_type_space('\f').

% split_symbol(S):- char_type(S, punct), \+ char_type(S, quote).
char_type_period('?'). char_type_period('.'). char_type_period('!').

char_type_punct(P  ):- char_type_period(P).
char_type_punct(',').  char_type_punct(':').
char_type_punct('$').  char_type_punct(';'). 
char_type_punct('%'). 
 
split_symbol(P  ):- char_type_punct(P).
split_symbol('_'). split_symbol('-').   split_symbol('#'). 
split_symbol('='). split_symbol('+'). split_symbol('/').
split_symbol('\\'). %split_symbol('#$').

split_from_start(S):- split_symbol(S), S \== '#'.
split_from_mid(S):- split_symbol(S), S \== '-', S \== ':'.
split_from_end(S):- split_symbol(S), S \== '#'.

:-share_mp(into_text80/2).

init_to_tokens(I,C):- is_list(I),!,into_control80(I,T),rejoin_pronouns(T,C),!.
init_to_tokens(I,C):- any_to_string(I,S),atom_string(A,S),!,tokenizer_tokenize(A,T),into_control80(T,C).

tokenizer_tokenize(A,T):- tokenizer:tokenize(A,M),!, rejoin_pronouns(M,T),!.

rejoin_pronouns([],[]).
rejoin_pronouns([A,Thing|List],[S|ListO]):- 
    ace_niceace:pronoun_split(S, lower, (A,Thing)),!,
    break_atom_symbols(List,ListO).
rejoin_pronouns([I|List],[I|ListO]):- 
  rejoin_pronouns(List,ListO).


% into_text80(I,O):- notrace((into_control80(I,M),!,break_atom_symbols(M,N),maplist(number_to_nb,N,O))).

%into_control80(W,Out):- throw(into_control80(W,Out)).
into_control80(W,_Out):- var(W), throw(var_into_control80(W)).
into_control80(NotList,Out):- string(NotList),string_to_atom(NotList,Atom),!, into_control80(Atom,Out).
%into_control80(W,Out):- atom(W), \+ atom_contains(W, ' '),!, Out = [W].
%into_control80(W,_Out):- format(user_error,"~Ninto_control80: ~q",[W]),flush_output(user_error),fail.
into_control80(NotList,Out):-  atom(NotList),
   on_x_fail(tokenizer_tokenize(NotList,Tokens)), 
   (Tokens\=[NotList] -> into_control80(Tokens,Out); Out=Tokens),!.
into_control80(W,[Out]):- number(W), atom_number(Out,W).
into_control80([W|ListIn],Out):- !, into_control80(W,H), into_control80(ListIn,T),!,append(H,T,Out).
into_control80(w(_,L),Out):- member(txt(W),L),!,into_control80(W,Out).
into_control80(w(W,_),Out):- !,listify(W,M),!,into_control80(M,Out).
into_control80(span(_),[]):- !.
into_control80(NotList,Out):-  findall(E,((sub_term(E,NotList),compound(E),E=w(_,_))), EL), EL\==[],!,into_control80(EL,Out).
into_control80(NotList,Out):-  
   \+ is_list(NotList), 
   convert_to_atoms_list(NotList,List), !,
   into_control80(List,Out).

into_control80(ListIn,Out):- fail,
   append(Left,[Last],ListIn), 
 ( \+ atom_length(Last,1),
   char_type_period(P), % covers Q, ! , etc
   atom_concat(Word,P,Last)),
   append(Left,[Word,P],ListMid),!,
   into_control80(ListMid,Out).

/*
into_control80(ListIn,Out):- 
   append(Left,[P],ListIn), 
 (\+ atom_length(P,1); \+ char_type(P,period)),!,
   append(ListIn,[('.')],ListMid),!,
   into_control80(ListMid,Out).
*/
into_control80([W,A,B|More],Out):- fail,
   downcase_atom(W,D),
   Out=[D,A,B|More],!.
into_control80(Out,Out):- !.


any_nb_to_atom(nb(N),A):- nonvar(N),!,any_to_atom(N,A),!.
any_nb_to_atom(N,A):- any_to_atom(N,A).


foc_framevar(VarName,VarFn,X):- atom_concat('?',VN,VarName),!,foc_framevar(VN,VarFn,X).
foc_framevar(VarName,VarFn,X):- toPropercase(VarName,VarFn),!,foc_framevar2(VarFn,X).

foc_framevar2(VarName,X):- atom_concat('?',VN,VarName),!,foc_framevar2(VN,X).
foc_framevar2(VarFn,X):- nb_current('$frame_variable_names',Vs),member(N=V,Vs),VarFn==N,!,must(X=V).
foc_framevar2(VarFn,X):- (nb_current('$frame_variable_names',Vs);Vs=[]),!,nb_setval('$frame_variable_names',[VarFn=X|Vs]).

:- fixup_exports.

