:- module(comment_data, [comment_data/2, enable/0, disable/0]).

:- dynamic comment_data/2.

%% process_comment_data(+Comments, +Term) is semidet
%
% This comment_hook hack allow us to write copy-pasteable data as
% comment, to facilitate output comparisons:

process_comment_data(Comments, Term) :-
    Term = (test(Test) :- _),
    format(string(Header), '/* $~w$~n', [Test]), % */
    member(_-Comment, Comments),
    string_concat(Header, Out0, Comment),
    string_concat(Out, '*/', Out0),
    retractall(comment_data(Test, _)),
    assertz(comment_data(Test, Out)).

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
