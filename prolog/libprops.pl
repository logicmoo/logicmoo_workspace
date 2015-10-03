:- module(libprops, []).

% Properties of library predicates

:- use_module(library(assertions)).
:- use_module(library(basicprops)).
:- use_module(library(termtyping), []).
:- use_module(library(plprops)).

:- pred functor(+, -atm, -nnegint) is det.
:- pred functor(-, +atm, +nnegint) is det.
:- pred functor(+, +atm, ?nnegint) is semidet.
:- pred functor(+, -atm, +nnegint) is semidet.

:- pred (+arithexpression> +arithexpression) is semidet.
:- pred (+arithexpression< +arithexpression) is semidet.
:- pred (+arithexpression>= +arithexpression) is semidet.
:- pred (+arithexpression=< +arithexpression) is semidet.
:- pred (+arithexpression=\= +arithexpression) is semidet.
:- pred (+arithexpression=:= +arithexpression) is semidet.
:- pred ((is)/2) : (var*arithexpression) => (num*arithexpression) is det.
:- pred ((is)/2) : (num*arithexpression) is semidet.

:- pred atomic_list_concat(+list(constant), +constant).
:- pred atomic_list_concat(+list(constant), -constant).

:- pred atomic_list_concat(+list(constant), +constant, +constant).
:- pred atomic_list_concat(+list(constant), +constant, -constant).
:- pred atomic_list_concat(-list(constant), +constant, +constant).

:- pred atom_number(+atm,-num) is semidet.
:- pred atom_number(?atm,+num) is det.

:- pred atom_codes(+atm, -list) is det.
:- pred atom_codes(-atm, +list) is det.
:- pred atom_codes(+atm, +list) is semidet.

:- pred sub_atom(+atm,?int,?int,?int,?atm).

:- use_module(library(apply)).
:- comp maplist/2 is multi.
:- comp maplist/3 is multi.
:- comp maplist/4 is multi.
:- comp maplist/5 is multi.

:- pred maplist(A, list)                   : callable(call(A, _)).
:- pred maplist(A, list, list)             : callable(call(A, _, _)).
:- pred maplist(A, list, list, list)       : callable(call(A, _, _, _)).
:- pred maplist(A, list, list, list, list) : callable(call(A, _, _, _, _)).

:- pred memberchk(?, ?list) is semidet.
