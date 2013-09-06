:- module(re, [ re//1
              , rematch1/4
              ]).

% DCG parser for regular expressions
re(Z) --> basicRE(W), reTail(W, Z).
reTail(W, Z) --> "|", basicRE(X), reTail(union(W,X), Z).
reTail(W, W) --> {true}.
basicRE(Z) --> simpleRE(W), basicREtail(W, Z).
basicREtail(W, Z) --> simpleRE(X), basicREtail(conc(W,X), Z).
basicREtail(W, W) --> {true}.
simpleRE(Z) --> elementalRE(W), simpleREtail(W, Z).
simpleREtail(W, star(W)) --> "*".
simpleREtail(W, plus(W)) --> "+".
simpleREtail(W, W) --> {true}.
elementalRE(any) --> ".".
elementalRE(group(X)) --> "(", re(X), ")".
elementalRE(eos) --> "$".
elementalRE(char(C)) --> [C], {\+(re_metachar([C]))}.
elementalRE(char(C)) --> "\\", [C], {re_metachar([C])}.
%  For sets, first try the negative set syntax.  If the "[^" recognition
%  succeeds, use cut to make sure that any subsequent failure does not
%  cause the positive set interpretation to be used.
elementalRE(negSet(X)) --> "[^", {!}, setItems(X), "]".
elementalRE(posSet(X)) --> "[", setItems(X), "]".
re_metachar("\\").
re_metachar("|").
re_metachar("*").
re_metachar("+").
re_metachar(".").
re_metachar("[").
re_metachar("$").
re_metachar("(").
re_metachar(")").
setItems([Item1|MoreItems]) --> setItem(Item1), setItems(MoreItems).
setItems([Item1]) --> setItem(Item1).
setItem(char(C)) --> [C], {\+(set_metachar([C]))}.
setItem(char(C)) --> "\\", [C], {set_metachar([C])}.
setItem(range(A,B)) --> setItem(char(A)), "-", setItem(char(B)).
set_metachar("\\").
set_metachar("]").
set_metachar("-").


% regular expression interpreter
% rematch1(RE, S, Unmatched, Selected) is true if RE matches
% a string Prefix such that S = [Prefix|Unmatched], and
% Selected is the list of substrings of Prefix that matched
% the parenthesized components of RE.

rematch1(union(RE1, _RE2), S, U, Selected) :-
  rematch1(RE1, S, U, Selected).
rematch1(union(_RE1, RE2), S, U, Selected) :-
  rematch1(RE2, S, U, Selected).
rematch1(conc(RE1, RE2), S, U, Selected) :-
  rematch1(RE1, S, U1, Sel1),
  rematch1(RE2, U1, U, Sel2),
  append(Sel1, Sel2, Selected).
% Try longest match first.
rematch1(star(RE), S, U, Selected) :-
  rematch1(RE, S, U1, Sel1),
  rematch1(star(RE), U1, U, Sel2),
  append(Sel1, Sel2, Selected).
rematch1(star(_RE), S, S, []).
rematch1(plus(RE), S, U, Selected) :-
  rematch1(RE, S, U1, Sel1),
  rematch1(star(RE), U1, U, Sel2),
  append(Sel1, Sel2, Selected).
% Match a group and add it to the end of
% list of selected items from the submatch.
rematch1(group(RE), S, U, Selected) :-
  rematch1(RE, S, U, Sel1),
  append(P, U, S),
  append(Sel1, [P], Selected).

rematch1(any, [_C1|U], U, []).
% Note that the following works for matching both regular
% characters and metacharacters.
rematch1(char(C), [C|U], U, []).

rematch1(eos, [], [], []).

rematch1(negSet(Set), [C|U], U, []) :-
  \+(charSetMember(C, Set)).

rematch1(posSet(Set), [C|U], U, []) :-
  charSetMember(C, Set).

charSetMember(C, [char(C) | _]).
charSetMember(C, [range(C1, C2) | _]) :-
  C1 =< C,
  C =< C2.
charSetMember(C, [_|T]) :- charSetMember(C, T).
