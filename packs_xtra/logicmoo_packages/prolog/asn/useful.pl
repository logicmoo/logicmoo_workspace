/*  USEFUL.PL  */

/*  Shelved on the 14th of November 1989.  */


/*
    These predicates provide predicates called, but not defined,
    in the source of ASN:
        append( ?List1, ?List2, ?List3 )
        member( ?Member, ?List )
        checklist( +Pred, +List )
        concat_atom( +List, ?Atom )
        aname( A+, AN? ), where A must be atom or integer.
        aname( A?, AN+ ), where AN must be a list of character codes.
        is_list( Term+ )
        portray_clause( Clause+ )

    As with IO.PL, I have had to guess at their function, though most
    were obvious. "checklist" came from the Edinburgh Tools library.
                            Jocelyn Paine
*/


/*  append( ?List1, ?List2, ?List3) :-
    As standard.
*/
append([], L, L).
append([H|R], S, [H|T]) :-
    append(R, S, T).


/*  member( ?Member, ?List) :-
    As standard.
*/
member(X, [X|_]).
member(X, [_|Y]) :-
    member(X, Y).


/*  checklist( +Pred, +List) :-
    Pred(Elem) succeeds for each Elem in List.
    This is normally useful for side-effects.
*/
checklist(_, []).
checklist(Pred, [Head|Tail]) :-
    Goal =.. [ Pred, Head ],
    call( Goal ),
    checklist(Pred, Tail).


/*  concat_atom( +List, ?Atom) :-
    The elements of List, when concatenated, form Atom.
    Assumes each element is an atom or an integer.
*/
concat_atom( L, A ) :-
    concat_atom( L, [], AN ),
    aname( A, AN ).

concat_atom( [], Sofar, Sofar ) :- !.
concat_atom( [H|T], Sofar, Result ) :-
    aname( H, HN ),
    append( Sofar, HN, H_and_Sofar ),
    concat_atom( T, H_and_Sofar, Result ).


/*  aname( A+, AN? ), where A must be atom or integer.
    aname( A?, AN+ ), where AN must be a list of character codes.

    If A is an integer, converts it to a list of character codes
    representing the digits, and matches with AN.
    If A is an atom, acts like name(A,AN).
    If AN is a list of character codes, which are all between the codes
    for '0' to '9', matches A with the corresponding integer.
    Otherwise, acts like name(A,AN).

    So, this is like name/2 with the extension that it converts between
    integers and lists of their digits. This is what Quintus name does.
*/
aname( A, AN ) :-
    var( AN ), atom(A),
    !,
    name( A, AN ).

aname( A, AN ) :-
    var( AN ), integer(A),
    !,
    iname( A, AN ).

aname( A, AN ) :-
    nonvar( AN ),
    is_digits( AN ),
    !,
    iname( A, AN ).

aname( A, AN ) :-
    name( A, AN ).


iname( A, AN ) :-
    var( AN ),
    !,
    int_to_list( A, AN ).

iname( A, AN ) :-
    !,
    list_to_int( AN, A ).


int_to_list( A, [C] ) :-
    A =< 9,
    !,
    int_to_char( A, C ).

int_to_list( A, L ) :-
    int_to_list( A, [], L ).


int_to_list( 0, Sofar, Sofar ) :- !.

int_to_list( I, Sofar, L ) :-
    Rem is I mod 10,
    Div is I div 10,
    int_to_char( Rem, C ),
    int_to_list( Div, [C|Sofar], L ).


int_to_char( I, C ) :-
    C is I + 48.


char_to_int( C, I ) :-
    I is C - 48.


is_digits( L ) :-
    L \= [],
    checklist( is_digit, L ).

is_digit( D ) :-
    D >= 48, D =< 57.


list_to_int( L, I ) :-
    list_to_int( L, 0, I ).

list_to_int( [], Sofar, Sofar ) :- !.

list_to_int( [H|T], Sofar, Result ) :-
    !,
    char_to_int( H, IH ),
    HSofar is Sofar * 10 + IH,
    list_to_int( T, HSofar, Result ).


/*  is_list( Thing+ ) :-
    Thing is a list.
*/
is_list( [] ) :- !.
is_list( [_|_] ).


/*  portray_clause( C+) :
        C: clause.
    Writes C, pretty-printed.
    (Actually, writeq probably won't, but it's good enough to get ASN
    running.
*/
portray_clause( C ) :-
    writeq(C), nl.
