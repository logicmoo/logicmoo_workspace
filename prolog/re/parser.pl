:- module(re_parser, [re//1]).

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
simple_re_tail(W, optional(W)) -->
    "?".
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
re_metachar("?").
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

