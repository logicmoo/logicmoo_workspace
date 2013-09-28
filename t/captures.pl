:- use_module(library(regex)).

:- use_module(library(tap)).

% no captures
regex('\\d+', [], '932', []).
regex('.*', [], howdy, []).

'single capture (entire)' :-
    regex('(\\d+)', [], '932', ["932"]).
'single capture (partial)' :-
    regex('a (\\d+)rd', [], 'a 3rd', ["3"]).

'two captures' :-
    regex('(\\S+) (\\S+)', [], 'hello world', ["hello", "world"]).


'one, explicit named capture' :-
    regex('Hi (?<Name>\\w+)', i, 'Hi Ed', ['Name'=Name]),
    Name == "Ed".

'two, explicit named captures' :-
    regex('(?<A>\\d) (?<B>\\d)', [], 'a 1 2 b', ['B'=B,'A'=A]),
    A == "1",
    B == "2".

'two named captures, only one is used'(todo) :-
    regex('(?<A>\\d) (?<B>\\d)', [], 'a 1 2 b', ['A'=A]),
    A == "1".

'one named capture, two names given'(todo) :-
    regex('(?<A>a)bcd', [], 'abcd', ['B'=B,'A'=A]),
    A == "a",
    var(B).

'numbered, constrained captures' :-
    when(ground(A),(number_codes(N,A), 1 =:= N mod 2)),
    regex("odd: ([0-9]+)", [], "odd: 77", [A]).

'named, constrained captures' :-
    when(ground(A),(number_codes(N,A), 0 =:= N mod 2)),
    regex("even: (?<A>[0-9]+)", [], "even: 42", ['A'=A]).

'named capture treated as a numbered capture' :-
    regex('(?<A>a)bcd', [], 'abcd', Captures),
    Captures == ["a"].

'all captures initially unbound' :-
    regex('hello (\\w+)', i, 'Hello Sue', Captures),
    Captures == ["Sue"].

'single capture initially unbound' :-
    regex('hello (\\w+)', i, 'Hello Joe', [Whom]),
    Whom == "Joe".

'pattern matches but captures fail unification'(fail) :-
    Whom = "Thomas",
    regex('hello ([a-z]+)', i, 'Hello Tom', [Whom]).
