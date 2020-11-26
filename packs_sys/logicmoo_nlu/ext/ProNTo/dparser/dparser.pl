% dparser.pl - M. Covington 2003    http://www.ai.uga.edu/mc
%
% A simple free-word-order dependency parser in plain Prolog
% (not requiring GULP, and using only relatively simple techniques).

%
% SAMPLE GRAMMAR AND LEXICON  (LATIN)
%

% Lexicon
%   Each word is represented as:
%   word(Form,[Category,Gloss,Gender,Number,Case,Person]).

word(canis,   [n,'''dog''',masc,sg,nom,3]).
word(canem,   [n,'''dog''',masc,sg,acc,3]).
word(canes,   [n,'''dog''',masc,pl,nom,3]).
word(canes,   [n,'''dog''',masc,pl,acc,3]).

word(felis,   [n,'''cat''',masc,sg,nom,3]).
word(felem,   [n,'''cat''',masc,sg,acc,3]).
word(feles,   [n,'''cat''',masc,pl,nom,3]).
word(feles,   [n,'''cat''',masc,pl,acc,3]).

word(video,   [v,'''(I) see''', _,sg,_,1]).   
word(videmus, [v,'''(we) see''',_,pl,_,1]).  
word(videt,   [v,'''sees''',    _,sg,_,3]).  
word(vident,  [v,'''see''',     _,pl,_,3]).   

word(parvus,  [adj,'''small''',masc,sg,nom,_]).
word(parvum,  [adj,'''small''',masc,sg,acc,_]).
word(parvi,   [adj,'''small''',masc,pl,nom,_]).
word(parvos,  [adj,'''small''',masc,pl,acc,_]).

% Dependency rules
%   Each rule is of the form: dh(Dependent,Head)
%   where Dependent and Head are like the lexical entries above.

%   Subject and verb

dh([n,_,_,Number,nom,Person], [v,_,_,Number,_,Person]).

%   Object and verb

dh([n,_,_,_,acc,_], [v,_,_,_,_,_]).

%   Adjective and noun

dh([adj,_,Gender,Number,Case,_], [n,_,Gender,Number,Case,_]).

% LIMITATION:
% There is nothing to prevent a verb from having multiple subjects
% or multiple objects.  Thus we get 4 parses for 'feles vident canes'
% which is actually only 2 ways ambiguous in Latin.



%
% PARSER
%

% We maintain 2 lists, WordList and HeadList.
% WordList contains all words prior to the current one.
% HeadList contains all words that do not have heads.
% Eventually HeadList will have only one word in it.

% In WordList and HeadList, each word is represented as a "node"
% of the form:
% [Number,Dependents,Category,Gloss,Gender,Number,Case,Person].
% This is just like the lexical entry, but with 2 items added
% at the beginning:
% Number, a unique identifier;
% Dependents, an open list (initially empty) of dependents.

% We build the list of Dependents for each word by instantiating
% its initially uninstantiated tail.  That way, we can work on the
% word in either list (WordList or HeadList) and its representation
% in the other list will change with it, because it contains the
% same variable.


% parse(+InputList,-Result)
%  Parses a list of words, giving a HeadList.

parse(InputList,Result) :-
   parse_loop(1,InputList,[],[],Result).


% parse_loop(+Count,+InputList,+WordList,+HeadList,-Result)
%  Called by parse/2, to loop through the list.

parse_loop(Count,[Word|InputList],WordList,HeadList,Result) :-
   word(Word,WordFeatures),                        % Look up the next word
   Node = [Count, _ | WordFeatures],               % Build a node for it
   parse_node(Node,WordList,HeadList,NewHeadList),   % Try to attach it
   NewCount is Count + 1,
   NewWordList = [Node|WordList],                    % Add Node to WordList
   parse_loop(NewCount,InputList,NewWordList,NewHeadList,Result).

parse_loop(_,[],_,Result,Result).
   % No more words to parse; so Result := HeadList.



% parse_node(+Node,+WordList,+HeadList,-NewHeadList)
%  Try to attach Node to the dependency tree.
%  HeadList gets modified here; WordList does not
%  (except by instantiation).

parse_node(Node,[],[],[Node]) :- !.
   % If this is the first word, just add it to HeadList.

parse_node(Node,WordList,HeadList,NewHeadList) :-
   try_inserting_as_head(Node,HeadList,HeadList2),
   try_inserting_as_dependent(Node,WordList,HeadList2,NewHeadList).




% try_inserting_as_head(+Node,+HeadList,-NewHeadList)
%  Try to insert Node as the head above a word presently in HeadList.
%  This entails deleting an element from HeadList.

try_inserting_as_head(_,[],[]).
   % No more elements of HeadList to look at.

try_inserting_as_head(Node,[Head|HeadList],NewHeadList) :-
   % Insert Node above Head, and remove Head from HeadList.
   Node = [_,D|NodeFeatures],
   Head = [_,_|HeadFeatures],
   dh(HeadFeatures,NodeFeatures),  % Head is the dependent here
   add_item(D,Head),
   % Now recurse down HeadList and see if it can be done again.
   % (Multiple elements of HeadList may be dependents of Node.)
   try_inserting_as_head(Node,HeadList,NewHeadList).
   
try_inserting_as_head(Node,[Head|HeadList],[Head|NewHeadList]) :-
   % Couldn't use Head, so skip it.
   try_inserting_as_head(Node,HeadList,NewHeadList).


% try_inserting_as_dependent(+Node,+WordList,+HeadList,-NewHeadList)
%  Try to insert Node as a dependent of a word already encountered,
%  i.e., of an element of WordList.
%  Alternatively, add Node to HeadList.

try_inserting_as_dependent(Node,WordList,HeadList,HeadList) :-
   member(Word,WordList),
   Word = [_,D|WordFeatures],
   Node = [_,_|NodeFeatures],
   dh(NodeFeatures,WordFeatures),
   add_item(D,Node).


try_inserting_as_dependent(Node,_,HeadList,[Node|HeadList]).
   % couldn't attach it, so add it to HeadList


%
% LIST UTILITIES
%


% add_item(?OpenList,+Item)
%   adds Item just before the tail of OpenList

add_item(X,Item) :- var(X), !, X = [Item|_].

add_item([_|X],Item) :- add_item(X,Item).


% Output utilities


write_list([First|Rest]) :-
   var(First),
   !,
   write('_ '),
   write_list(Rest).

write_list([First|Rest]) :-
   write(First),
   write(' '),
   write_list(Rest).

write_list([]) :-
   nl.



write_dep(Node) :-
   write_dep(Node,0).

write_dep([N,Dependents|Rest],Indentation) :-
   tab(Indentation),
   write(N), write(' '), write_list(Rest),
   NewIndentation is Indentation + 3,
   write_dep_open_list(Dependents,NewIndentation).

write_dep_open_list(X,_) :-
   var(X),
   !.

write_dep_open_list([First|Rest],N) :-
   write_dep(First,N),
   write_dep_open_list(Rest,N).

   



% tests

try(List) :- write_list(List),nl,
             parse(List,[Head]),
             write_dep(Head),
	     nl,
	     fail.

try(_) :- write('No (more) parses.'), nl.


test1 :- try([canis,videt,felem]).
test2 :- try([videt,felem,canis]).
test3 :- try([canes,vident,felem]).
test4 :- try([canes,vident,feles]).  % ambiguous
test5 :- try([canis,parvus,videt,felem]).
test6 :- try([felem,canis,videt,parvum]).






