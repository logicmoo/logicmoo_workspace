:- module(camel_snake, [camel_snake/2]).

camel_snake(Camel, Snake) :-
    atom(Camel), !,
    atom_codes(Camel, CamelS),
    camel_snake_s(CamelS, SnakeS),
    atom_codes(Snake, SnakeS).
camel_snake(Camel, Snake) :-
    atom(Snake),
    atom_codes(Snake, SnakeS),
    camel_snake_s(CamelS, SnakeS),
    atom_codes(Camel, CamelS).    

camel_snake_s([U|CL], [L|SL]) :-
    ( upper_lower(U, L)
    ->true
    ; U = L
    ),
    camel_snake_(CL, SL).
camel_snake_s([], []).

camel_snake_([U|CL], [0'_, L|SL]) :-
    upper_lower(U, L), !,
    camel_snake_(CL, SL).
camel_snake_([C|CL], [C|SL]) :-
    camel_snake_(CL, SL).
camel_snake_([], []).
