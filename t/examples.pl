:- use_module(library(re)).

% macro to make it easier to write tests.
% eventually, the library will support this syntax natively
:- op(1050,xfx,=~).
term_expansion(String =~ Pattern, (Head :- Test)) :-
    format(atom(Head), '~s =~~ ~s', [String, Pattern]),
    Test = (
        once(phrase(re(Re), Pattern)),
        once(rematch1(Re, String, _, _))
    ),
    tap:register_test(Head).

:- use_module(library(tap)).

"deadbeef" =~ "[a-f0-9]+".
"1973c" =~ "[a-f0-9]+".
"abbba" =~ "ab+a".
