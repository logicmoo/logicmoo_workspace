/*  GRIPSTOP.PL  */



/*
Introduction - interfacing GRIPS.
---------------------------------

This file exports three predicates:
1. grips           - a top-level interpreter for GRIPS.
2. grips_consult   - to load GRIPS definitions, imitating Prolog 'consult'.
2. grips_reconsult - to load GRIPS definitions, imitating Prolog 'reconsult'.
*/



/*
The top-level interpreter.
--------------------------

The main predicate defined is 'grips'. This reads a term, evaluates it
in some way, and then repeats (unless the term was 'halt', in which case
it stops the program, or unless the term was 'prolog', in which case it
returns to Prolog).

For the form of terms, see the section on 'The top-level interpreter' in
the GRIPS documentation.


Implementation.
---------------

Terms are recognised and processed by 'process'. This takes three
arguments. The first is the term to be evaluated. The second is a list
of names of the variables occurring in the term (see below for more on
this). The third will be instantiated to 'yes' if 'grips' is to read
another term; 'false' if it is to exit.

To translate a term, 'process' calls 'trans_guard' or 'trans_expr' as
appropriate. To evaluate the result, it calls either 'prolog_tli' (which
simulates the Prolog top-level interpreter) or 'try' (for GRIPS
expressions).

Note that the version of 'read' called takes two arguments. The second
is assumed to return a representation of the names of the variables in
the first argument. This is used when answers are to be displayed by the
Prolog top-level interpreter, but ignored for all other terms. Most
systems now have some such version of read/2.
*/


(grips) :-
    read( Term, Vars ),
    process_tli_term( Term, Vars, Continue ),
    (
        Continue = yes
    ->
        (grips)
    ;
        true
    ).


process_tli_term( V, _, yes ) :-
    var( V ),
    !,
    write( 'Result is a variable' ), nl.

process_tli_term( halt, _, _ ) :-
    !,
    halt.

process_tli_term( prolog, _, no ) :-
    !.

process_tli_term( pr(G), Vars, yes ) :-
    !,
    prolog_tli( G, Vars ).

process_tli_term( test(G), Vars, yes ) :-
    !,
    trans_guard( G, G0 ),
    prolog_tli( G0, Vars ).

process_tli_term( do(G), _, yes ) :-
    !,
    trans_guard( G, G0 ),
    (
        call( G )
    ->
        write( 'Done' )
    ;
        write( 'Failed' )
    ),
    nl.

process_tli_term( echo(V), _, yes ) :-
    var(V),
    !,
    write('Argment of echo is a variable.'), nl.

process_tli_term( echo(E), _, yes ) :-                    
    !,
    trans_expr( G, ResultVar, Goals ),
    write( 'GOALS: ' ), write( Goals ), nl,
    write( 'RESULT VAR: ' ), write( ResultVar ), nl,
    try( Goals, ResultVar ).

process_tli_term( E, _, yes ) :-
    !,
    trans_expr( E, ResultVar, Goals ),
    try( Goals, ResultVar ).


try( G, V ) :-
    call( G ),
    write( 'Result = ' ), write( V ), write( '.' ), nl, !.
try( _, _ ) :-
    write( 'Failed.' ), nl, !.



/*
Simulating the Prolog top-level interpreter.
--------------------------------------------

The main predicate is 'prolog_tli', which takes a goal and a
representation of its variable names, calls the goal, and displays the
result. If the goal contains at least one variable, 'prolog_tli' then
displays the 'More (y/n)' message and reads your answer to see whether
you want another solution. This is the way the TLI behaves on my system:
IT MAY BE DIFFERENT ON YOURS.

If your system allows you to call the TLI directly, then you can
dispense with most of this section.


Implementation.
---------------

'prolog_tli' calls 'prolog_goal' to answer each goal, after setting the
current input and output streams.

'prolog_goal' calls its goal, and then calls 'display_answer'. This is a
bit messy. If the goal contains no variables, or only an anonymous
variable, 'display_answer' succeeds without doing anything else, so
'prolog_goal' just writes 'yes' and itself suceeds.

If the goal does contain variables, then 'display_answer' displays the
values of all non-anonymous variables in the goal and then asks the user
whether he wants more solutions. It fails if the answer is 'yes', thus
causing 'prolog_goal' to backtrack into the goal and try it again.

These predicates rely on the way that the variable names are represented
(see note about read/2 above). I have assumed them to be represented as
in Poplog: that is, as a list of elements, each of which is a term whose
functor is the variable name and whose only argument is the variable.
Thus, if read/2 were called on the term
    f( g( A ), B, _, C )
the variable names would be the list
    [ 'A'(A), 'B'(B), '_'(_), 'C'(C) ]
(though the order is undefined). IN YOUR SYSTEM, THE CONVENTION IS
PROBABLY DIFFERENT.
*/


prolog_tli( Goal, Vars ) :-
    see(user), tell(user),
    prolog_goal( Goal, Vars ),
    see(user), tell(user).


prolog_goal( V, _ ) :-
    var(V),
    !,
    write( 'Result is a variable' ), nl.

prolog_goal( X, Vars ) :-
    call( X ),
    display_answer( Vars ),
    tell(user),
    write(yes), nl.

prolog_goal( _, _ ) :-
    tell(user),
    write(no), nl.


display_answer(Vars) :-
    /* if Vars contains a non-anonymous variable... */
    member( X, Vars ),
    functor( X, Name, 1 ),
    Name \= '_',
    !,
    writevars(Vars), nl,
    write('More (y/n)? '),
    see(user),
    read_line_as_charlist( Reply ),
    ( Reply = [] ; Reply = [N|_], is_n(N) ).

display_answer(Vars).


writevars([]).

writevars([T|Rest]) :-
    writevars(Rest),
    nl,
    writevar(T).


writevar(T) :-
    functor( T, '_', 1 ),
    !.

writevar(T) :-
    functor( T, F, 1 ),
    arg( 1, T, X ),
    write(F), write(' = '), print(X).



/*
Reading the user's reply.
-------------------------

Prolog I/O being what it is, this bit is almost as big as the rest of
the interpreter. It's the standard method. YOU MAY NEED TO CHANGE THE
CHARACTER DEFINITIONS AT THE END.
*/


read_line_as_charlist( List ) :-
    get0( C ),
    read_rest_as_charlist( C, [], List ).


read_rest_as_charlist( C, CharsSoFar, CharsSoFar ) :-
    is_newline_char( C ),
    !.

read_rest_as_charlist( C, CharsSoFar, CharsSoFar ) :-
    is_eof_char( C ),
    !.

read_rest_as_charlist( C, CharsSoFar, [C|Tail] ) :-
    get0( CNext ),
    read_rest_as_charlist( CNext, CharsSoFar, Tail ).


is_newline_char( 13 ).
is_newline_char( 10 ).

is_eof_char( 26 ).

is_n( 110 ).    /*  Little n.  */
is_n( 78 ).     /*  Big N.     */



/*
Consulting files.
-----------------

This section defines grips_consult and grips_reconsult. Both take a file
name as argument, and imitate consult or reconsult. The file can contain
the following:
1. GRIPS definitions - these will be translated into Prolog and then
                       asserted. If you're reconsulting, any definitions
                       with the same functor and arity will be deleted.
2. GRIPS directives  - these will be translated into Prolog and called.
3. Prolog directives - these will be called.
4. Prolog definitions- these will be asserted. If you're reconsulting,
                       old definitions will be deleted.
5. Grammar rules     - these will be translated into Prolog and treated
                       as above.


Implementation.
---------------

Both predicates call grips_consult_or_reconsult, which in turn calls the
inferior predicates for reading, translating and asserting terms. This
predicate is passes a flag which is either 'consult' or 'reconsult'.
This tells it whether to delete old clauses when it meets the start of
a new set for the same functor and arity.

The decision whether to do so is made by assert_consulted_clause. Each
call of this is passed a structure of the form
    clause( Functor, Arity )
specifying the functor and arity of the previous clause. (If there was
no previous clause, because it's the first call, or bcause the last thing
was a directive, this argument is '$none'.) If assert_consulted_clause
is in reconsult mode, and its current clause has a different functor
and arity, then it deletes the old clauses before asserting the new. This
is neater than the method usually found in Prolog implementations, which
is to assert a flag.

Grammar rules cause a problem, because there is no system-independent way
to translate them. I assume that there's a predicate 'expand_term(A,B)'
which, if A is a grammar rule, expands it into a clause in B. This is
true on my system (because I added it to the implementation): IT MAY NOT
BE ON YOURS.
*/


grips_consult( File ) :-
    grips_reconsult_or_consult( File, consult ).


grips_reconsult( File ) :-
    grips_reconsult_or_consult( File, reconsult ).


grips_reconsult_or_consult( File, ConsultOrReconsult ) :-
    seeing( CIS ),
    see( File ), seen,
    see( File ),
    grips_reconsult_or_consult_1( '$none', ConsultOrReconsult ),
    seen,
    see( CIS ).


grips_reconsult_or_consult_1( Previous, ConsultOrReconsult ) :-
    read( Term ),
    (
        Term = end_of_file
    ->
        true
    ;
        process_consulted_term( Term, Previous, ConsultOrReconsult, Next ),
        grips_reconsult_or_consult_1( Next, ConsultOrReconsult )
    ).


process_consulted_term( Term, Previous, _, '$none' ) :-
    trans_directive( Term, Goal ),
    !,
    ( call( Goal ) -> true ; true ).

process_consulted_term( Term, Previous, ConsultOrReconsult, Next ) :-
    trans_def( Term, Clause ),
    !,
    assert_consulted_clause( Clause, Previous, ConsultOrReconsult, Next ).

process_consulted_term( Term, Previous, ConsultOrReconsult, Next ) :-
    expand_term( Term, Clause ),
    !,
    assert_consulted_clause( Clause, Previous, ConsultOrReconsult, Next ).

process_consulted_term( Clause, Previous, ConsultOrReconsult, Next ) :-
    assert_consulted_clause( Clause, Previous, ConsultOrReconsult, Next ).


assert_consulted_clause( (Head:-Tail), Previous, consult, _ ) :-
    !,
    assert( (Head:-Tail) ).

assert_consulted_clause( (Head:-Tail), Previous, reconsult, clause(Functor,Arity) ) :-
    !,
    functor( Head, Functor, Arity ),
    (
        Previous \= clause(Functor,Arity)
    ->
        functor( Head1, Functor, Arity ),
        retractall( Head1 )
    ;
        true
    ),
    assert( (Head:-Tail) ).

assert_consulted_clause( Head, Previous, ConsultOrReconsult, Next ) :-
    assert_consulted_clause( (Head:-true), Previous, ConsultOrReconsult, Next ).
