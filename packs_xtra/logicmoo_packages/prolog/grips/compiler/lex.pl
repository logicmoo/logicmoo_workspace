/*  LEX.PL  */


read_list <-
    read_rest( eval(get0) ).


read_rest( C ) <- [] if is_eof(C).

read_rest( C ) <- read_rest( eval(get0) ) if is_nl( C ).

read_rest( C ) <- read_rest( skip_spaces(C) ) if is_space(C).

read_rest( Alpha ) <-
    is_letter( Alpha )
    =>
    ( [ Id ] ++ read_rest( CPost )
      where
      read_identifier( Alpha, CPost, Id )
    ).

read_rest( Digit ) <-
    is_digit( Digit )
    =>
    ( [ Int ] ++ read_rest( CPost )
      where
      read_integer( Digit, CPost, Int )
    ).

read_rest( Q ) <-
    is_single_quote( Q )
    =>
    ( [ q(string(S)) ] ++ read_rest( CPost )
      where
      read_string( Q, CPost, S )
    ).

read_rest( C ) <-
    (
        double_token( [C,CNext] )
    =>
        ( [N] where name(N,[C,CNext]) ) ++ read_rest( eval(get0) )
    else
        ( [N] where name(N,[C]) ) ++ read_rest( CNext )
    )
    where get0( CNext ).

read_rest( C ) <-
    ([A] where name(A,[C])) ++ read_rest( eval(get0) ).


skip_spaces( C ) <-
    skip_spaces( eval(get0) ) if is_space( C ).

skip_spaces( C ) <- C.


read_identifier( C1, CPost, Id ) does
    read_identifier_1( C1, CPost, Chars ) and
    name( Id, Chars ).


read_identifier_1( C, C, [] ) does
    nothing
    if
    not( is_letter_or_digit(C) ).

read_identifier_1( C, CPost, [C|Rest] ) does
    read_identifier_1( eval(get0), CPost, Rest ).


read_integer( C1, CPost, Int ) does
    read_integer_1( C1, CPost, 0, Int ).


read_integer_1( C, C, Sofar, Sofar ) does
    nothing
    if
    not( is_digit(C) ).

read_integer_1( C, CPost, Sofar, Result ) does
    read_integer_1( eval(get0), CPost, Sofar*10+char_to_int(C), Result ).


read_string( C1, CPost, S ) does
    read_string_1( C1, eval(get0), Chars ) and
    name( S, Chars ) and
    get0( CPost ).


read_string_1( Q, Q, [] ) does nothing.

read_string_1( Q, C, [C|Rest] ) does
    read_string_1( Q, eval(get0), Rest ).


double_token( ":=" ).
double_token( "<>" ).    


is_space( 32 ).


is_nl( 13 ).
is_nl( 10 ).


is_eof( 26 ).


is_single_quote( 39 ).


is_letter( C ) :-
    ( C >= 65, C =< 90 )
    ;
    ( C >= 97, C =< 122 ).


is_digit( C ) :-
    C >= 48, C =< 57.


is_letter_or_digit( C ) :-
    is_letter( C )
    ;
    is_digit( C ).


char_to_int( C ) <- C-48.
