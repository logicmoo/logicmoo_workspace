:- module(re, [ re//1
              , rematch1/4
              ]).

% DCG parser for regular expressions
re(Z) -->
    basic_re(W),
    re_tail(W, Z).

re_tail(W, Z) -->
    "|",
    basic_re(X),
    re_tail(union(W,X), Z).
re_tail(W, W) -->
    { true }.


basic_re(Z) -->
    simple_re(W),
    basic_re_tail(W, Z).

basic_re_tail(W, Z) -->
    simple_re(X),
    basic_re_tail(conc(W,X), Z).
basic_re_tail(W, W) -->
    { true }.


simple_re(Z) -->
    elemental_re(W),
    simple_re_tail(W, Z).

simple_re_tail(W, star(W)) -->
    "*".
simple_re_tail(W, plus(W)) -->
    "+".
simple_re_tail(W, W) -->
    { true }.


elemental_re(any) -->
    ".".
elemental_re(group(X)) -->
    "(",
    re(X),
    ")".
elemental_re(eos) -->
    "$".
elemental_re(char(C)) -->
    [C],
    { \+ re_metachar([C]) }.
elemental_re(char(C)) -->
    "\\",
    [C],
    { re_metachar([C]) }.
elemental_re(neg_set(X)) -->
    "[^",
    !,  % don't backtrack into pos_set/1 clause below
    set_items(X),
    "]".
elemental_re(pos_set(X)) -->
    "[",
    set_items(X),
    "]".


re_metachar("\\").
re_metachar("|").
re_metachar("*").
re_metachar("+").
re_metachar(".").
re_metachar("[").
re_metachar("$").
re_metachar("(").
re_metachar(")").


set_items([Item1|MoreItems]) -->
    set_item(Item1),
    set_items(MoreItems).
set_items([Item1]) -->
    set_item(Item1).

set_item(char(C)) -->
    [C],
    { \+ set_metachar([C]) }.
set_item(char(C)) -->
    "\\",
    [C],
    { set_metachar([C]) }.
set_item(range(A,B)) -->
    set_item(char(A)),
    "-",
    set_item(char(B)).


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
rematch1(star(RE), S, U, Selected) :-
    % Try longest match first.
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

rematch1(neg_set(Set), [C|U], U, []) :-
    \+ char_set_member(C, Set).

rematch1(pos_set(Set), [C|U], U, []) :-
    char_set_member(C, Set).


char_set_member(C, [char(C) | _]).
char_set_member(C, [range(C1, C2) | _]) :-
    C1 =< C,
    C =< C2.
char_set_member(C, [_|T]) :-
    char_set_member(C, T).
