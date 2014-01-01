:- module(regex_parser, [re//2]).
:- use_module(library(dcg/basics), [integer//1, string//1]).
:- use_module(library(regex/state), [adjust_case/3]).

:- set_prolog_flag(double_quotes, string).

% DCG parser for regular expressions
re(Opt, Z) -->
    basic_re(Opt,W),
    re_tail(Opt,W,Z).


re_tail(Opt, W, Z) -->
    "|",
    basic_re(Opt,X),
    re_tail(Opt,union(W,X), Z).
re_tail(_Opt, W, W) -->
    { true }.


basic_re(Opt, Z) -->
    simple_re(Opt,W),
    basic_re_tail(Opt,W,Z).

basic_re_tail(Opt, W, Z) -->
    simple_re(Opt,X),
    basic_re_tail(Opt,conc(W,X), Z).
basic_re_tail(_Opt, W, W) -->
    { true }.


simple_re(Opt, Z) -->
    elemental_re(Opt,W),
    simple_re_tail(Opt,W,Z).

simple_re_tail(_Opt, W, count(W,0,999_999_999)) -->
    "*".
simple_re_tail(_Opt, W, count(W,1,999_999_999)) -->
    "+".
simple_re_tail(_Opt, W, count(W,0,1)) -->
    "?".
simple_re_tail(_Opt, W, count(W,N,N)) -->
    % {n}
    "{",
    integer(N),
    { N >= 0 },
    "}".
simple_re_tail(_Opt, W, count(W,N,999_999_999)) -->
    % {n,}
    "{",
    integer(N),
    { N >= 0 },
    ",",
    "}".
simple_re_tail(_Opt, W, count(W,N,M)) -->
    % {n,m}
    "{",
    integer(N),
    { N >= 0 },
    ",",
    integer(M),
    { M >= N },
    "}".
simple_re_tail(_Opt, W, W) -->
    { true }.


elemental_re(_Opt, any) -->
    ".".
%elemental_re(_Opt, caret) -->
%    "^".
elemental_re(Opt, group(X)) -->
    "(",
    re(Opt, X),
    ")".
elemental_re(Opt, named_group(Name, X)) -->
    "(?<",
    string(NameCodes),
    { atom_codes(Name, NameCodes) },
    ">",
    re(Opt, X),
    ")".
elemental_re(_Opt, eos) -->
    "$".
elemental_re(State, char(C)) -->
    [C0],
    { \+ re_metachar(C0) },
    { adjust_case(State, C0, C) }.
elemental_re(Opt, RE) -->
    "\\",
    [C],
    { perl_character_class(C, Opt, RE) }.
elemental_re(_Opt, char(C)) -->
    "\\",
    [C],
    { re_metachar(C) }.
elemental_re(Opt, neg_set(X)) -->
    "[^",
    !,  % don't backtrack into pos_set/1 clause below
    set_items(Opt,X),
    "]".
elemental_re(Opt, pos_set([char(0'-)|X])) -->
    "[-",
    !,  % don't backtrack into pos_set/1 clause below
    set_items(Opt,X),
    "]".
elemental_re(Opt, pos_set(X)) -->
    "[",
    set_items(Opt,X),
    "]".
elemental_re(Opt, pos_set([char(0'-)|X])) -->
    "[",
    set_items(Opt,X),
    "-]".


% true if argument is a code for a regular expression meta character
re_metachar(0'^).
re_metachar(0'\\).
re_metachar(0'|).
re_metachar(0'*).
re_metachar(0'+).
re_metachar(0'.).
re_metachar(0'?).
re_metachar(0'[).
re_metachar(0'$).
re_metachar(0'().
re_metachar(0')).


% define Perl character classes as character sets
perl_character_class(0'd, Opt, pos_set(X)) :-
    string_codes("0-9", Codes),
    set_items(Opt, X,Codes,[]).
perl_character_class(0'w, Opt, pos_set(X)) :-
    string_codes("0-9A-Za-z_", Codes),
    set_items(Opt, X,Codes,[]).
perl_character_class(0's, _Opt, pos_set([ char(0'\t)  % tab
                                  , char(0'\n)  % newline
                                  , char(0'\f)  % form feed
                                  , char(0'\r)  % carriage return
                                  , char(0' )   % space
                                  ])).
perl_character_class(Upper, Opt, neg_set(Set)) :-
    code_type(Lower, lower(Upper)),
    perl_character_class(Lower, Opt, pos_set(Set)).


set_items(Opt, [Item1|MoreItems]) -->
    set_item(Opt, Item1),
    set_items(Opt, MoreItems).
set_items(Opt, [Item1]) -->
    set_item(Opt, Item1).

set_item(State, char(C)) -->
    [C0],
    { \+ set_metachar(C0) },
    { adjust_case(State,C0,C) }.
set_item(_Opt, char(C)) -->
    "\\",
    [C],
    { set_metachar(C) }.
set_item(Opt, range(A,B)) -->
    set_item(Opt, char(A)),
    "-",
    set_item(Opt, char(B)).


set_metachar(0'\\).
set_metachar(0']).
set_metachar(0'-).

