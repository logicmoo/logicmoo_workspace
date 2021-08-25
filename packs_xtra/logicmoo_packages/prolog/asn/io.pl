/*  IO.PL  */

/*  Shelved on the 14th of November 1989.  */


/*
    I have provided this file for compatibility with Quintus Prolog.
    It defines the predicates
        is_eof_char( C+ )
        open( F+, D+, S- )
        set_output( OS+ )
        current_output( OS- )
        close( S+ )
        read_from( IS+, Term- )
        get0_from( IS+, Integer- )
    where
        C is a character code (integer)
        F is a filename (atom)
        D is 'read' or 'write'
        S is a stream
        OS and IS are output and input streams.
    These were used in the ASN translator. I have only attempted to
    imitate the functions needed for this program. Not having a Quintus
    manual, my guess about what they should do may be wrong, but ASN
    seems to work with them.

    It appears that Quintus I/O works on a current input or output
    "stream": some kind of term representing an open input or
    output file, which can be passed as an argument to get0, read, etc.
    (This is obviously neater than having it as global in the way
    Edinburgh Prolog does).

    I have implemented the streams as structures
        output(Filename)
    and
        input(Filename)
    I also use assertions
        '$output'(Filename)
    and
        '$input'(Filename)
    to indicate which files are currently opened by open/3, and
        '$current_output'( Filename )
    to indicate the current Quintus output stream.                  
                            JNP
*/


/*  is_eof_char( C+):
        True if C, read by get0_from, denotes end-of-file.
*/
is_eof_char( 26 ).


/*  open( F+, D+, S- ):
        F: filename (atom);
        D: 'read' or 'write';
        S: stream.
    Opens F.
    Instantiates S to a stream representing the open file - this can be
    passed as an argument to the other stream predicates.
    Action undefined if F is already open, or can't be opened.
*/
open( F, write, output(F) ) :-
    !,
    (
        '$output'(F)
    ->
        write(F), write(' is already open for output.'),
        nl,
        abort
    ;
        telling( COS ),
        tell( F ),
        tell( COS ),
        asserta( '$output'(F) )
    ).

open( F, read, input(F) ) :-
    (
        '$input'(F)
    ->
        write(F), write(' is already open for input.'),
        nl,
        abort
    ;
        seeing( CIS ),
        see( F ),
        see( CIS ),
        asserta( '$input'(F) )
    ).


/*  set_output( OS+ ):
        OS: output stream.
    Makes the file referenced by OS the COS (in the Edinburgh Prolog sense:
    i.e. "tell" it).
    Action undefined if OS points at a not-open file.
*/
set_output( output(F) ) :-
    (
        '$output'(F)
    ->
        tell( F ),
        retractall( '$current_output'(_) ),
        assert( '$current_output'(F) )

    ;
        write(F), write(' is not open for output.'), nl, abort
    ).


/*  current_output( OS- ):
        OS: output stream.
    Instantiates OS to the current output stream (in the Quintus,
    not the Edinburgh, sense).
    Action undefined if there is no such stream.
*/
current_output( output(F) ) :-
    (
        '$current_output'(F)
    ->
        true
    ;
        write('There is no stream open for output.'), nl, abort
    ).


/*  close( S+ ):
        S: stream.
    Closes the file referred to by S.
    Action undefined if S points at a not-open file.
*/
close( output(F) ) :-
    (
        retract( '$output'(F) ),
        retractall( '$current_output'(F) )
    ->
        telling( COS ),
        tell( F ),
        told,
        tell( COS )
    ;
        tell(user),
        write('Attempt to close (for output) '), write(F),
        write(' when it isn\'t open.'),
        nl, abort
    ).

close( input(F) ) :-
    (
        retract( '$input'(F) ),
        retractall( '$current_input'(F) )
    ->
        seeing( CIS ),
        see( F ),
        seen,
        see( CIS )
    ;
        tell(user),
        write('Attempt to close (for input) '), write(F),
        write(' when it isn\'t open.'),
        nl, abort
    ).


/*  read_from( IS+, Term- ):
        IS:     input stream.
        Term:   a term.
    Reads Term from the file pointed at by IS.
    Action undefined if IS points at a not-open file.
*/
read_from( input(F), Term ) :-
    (
        '$input'(F)
    ->
        seeing( CIS ),
        see( F ),
        read( Term ),
        see( CIS )
    ;
        write(F), write(' is not open for input.'), nl, abort
    ).


/*  get0_from( IS+, C- ):
        IS: input stream.
        C:  a character code.
    Calls get0 to read C from the file pointed at by IS.           
    Action undefined if IS points at a not-open file.
*/
get0_from( input(F), Char ) :-
    (
        '$input'(F)
    ->
        seeing( CIS ),
        see( F ),
        get0( Char ),
        see( CIS )
    ;
        write(F), write(' is not open for input.'), nl, abort
    ).
