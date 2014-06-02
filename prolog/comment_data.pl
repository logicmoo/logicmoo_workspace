:- module(comment_data, [comment_data/2, enable/0, disable/0]).

:- dynamic comment_data/2.

%% process_comment_data(+Comments, +Term) is semidet
%
% This comment_hook hack allow us to write copy-pasteable data as
% comment, to facilitate output comparisons:

process_comment_data(Comments, Term) :-
    Term = (test(_Test) :- _),
    ( member(_-Comment, Comments),
      get_comment_data(Comment, Name, Out),
      retractall(comment_data(Name, _)),
      assertz(comment_data(Name, Out)),
      fail
    ; true
    ).

get_comment_data(Comment, Name, Out) :-
    string_concat("/* $", Out0, Comment),
    sub_string(Out0, Before, Length, After, "$\n"),
    sub_string(Out0, 0, Before, _, SName),
    atom_string(Name, SName),
    OutPos is Before + Length,
    sub_string(Out0, OutPos, After, _, Out1),
    string_concat(Out, "*/", Out1).

:- dynamic enabled/0.

enable :-
    retractall(enabled),
    assertz(enabled).

disable :-
    retractall(enabled).

:- multifile prolog:comment_hook/3.
% :- dynamic prolog:comment_hook/3.

prolog:comment_hook(Comments, _TermPos, Term) :-
    enabled,
    process_comment_data(Comments, Term).
