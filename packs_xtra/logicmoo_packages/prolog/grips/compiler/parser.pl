/*  PARSER.PL  */


/*
The parser.
-----------

This file exports the function
    parse( Tokens ) -> Tree
which converts a list of tokens into a syntax tree.

It also exports the command
    test_parser
which reads a list, parses it, displays the tree, and repeats.

The implementation is straightforward, using definite clause grammars.
*/


parse( Tokens, Tree ) :-
    program( Tree, Tokens, [] ).


program( 'PROG'(Name,Declarations,Body) ) -->
    [program], identifier(Name), [';'],
    declarations( Declarations ),
    [begin], statement_list( Body ), [end], ['.'].


declarations( 'DECLARATIONS'(Labels,Consts,Vars) ) -->
    label_declarations( Labels ),
    constant_declarations( Consts ),
    variable_declarations( Vars ).


label_declarations( [] ) --> [].
label_declarations( Labels ) -->
    [label],
    label_part( Labels ),
    [;].


constant_declarations( [] ) --> [].
constant_declarations( Constants ) -->
    [const],
    constant_part( Constants ),
    [;].


variable_declarations( [] ) --> [].
variable_declarations( Variables ) -->
    [var],
    variable_part( Variables ),
    [;].


label_part( [ L | Rest ] ) -->
    unsigned_integer( L ),
    label_parts( Rest ).


label_parts( [] ) --> [].
label_parts( C ) -->
    [','],
    label_part( C ).


constant_part( [ 'C'(Name,Value) | Rest ] ) -->
    identifier(Name), [=], constant(Value),
    constant_parts( Rest ).


constant_parts( [] ) --> [].
constant_parts( C ) -->
    [;],
    constant_part( C ).


variable_part( [ 'V'(Name,Type) | Rest ] ) -->                   
    identifier(Name), [':'], type(Type),
    variable_parts( Rest ).


variable_parts( [] ) --> [].
variable_parts( V ) -->
    [;],
    variable_part( V ).


type( integer ) --> [integer].
type( boolean ) --> [boolean].


statement_list( [] ) --> [].

statement_list( [ S | Rest ] ) -->
    statement( S ),
    statement_list2( Rest ).


statement_list2( [] ) --> [].

statement_list2( [ S | Rest ] ) -->
    [';'], statement( S ),
    statement_list2( Rest ).


statement( 'LABELLED'(L,S) ) -->
    unsigned_integer( L ), [':'], statement( S ).

statement( 'ASSIGN'(Left,Right) ) -->
    identifier( Left ),
    [':='],
    expression( Right ).

statement( 'IF'( Cond, Then ) ) -->
    [if], expression( Cond ),
    [then], statement_list( Then ).

statement( 'GOTO'(L) ) -->
    [goto], unsigned_integer(L).

statement( 'READ'(V) ) -->
    [read], ['('], identifier(V), [')'].

statement( 'WRITE'(E) ) -->
    [write], ['('], writeable_expression(E), [')'].

statement( 'LIST'(S) ) -->
    [begin], statement_list(S), [end].



writeable_expression( S ) -->
    string( S ).

writeable_expression( E ) -->
    expression( E ).


expression( E ) -->
    simple_expression( E ).

expression( 'E'(O,E1,E2) ) -->
    simple_expression( E1 ), comparison_op(O), expression( E2 ).


comparison_op( = ) --> [=].
comparison_op( <> ) --> [<>].


simple_expression( SE ) -->
    term( SE ).

simple_expression( 'E'(O,E1,E2) ) -->
    term( E1 ), add_op( O ), simple_expression( E2 ).


add_op( + ) --> ['+'].
add_op( - ) --> ['-'].
add_op( or ) --> ['or'].


term( T ) -->
    factor( T ).

term( 'E'(O,T1,T2) ) -->
    factor( T1 ), mult_op( O ), term( T2 ).


mult_op( * ) --> ['*'].
mult_op( / ) --> ['/'].
mult_op( and ) --> [and].


factor( F ) -->
    identifier( F ).

factor( F ) -->
    ['('], expression(F), [')'].

factor( F ) -->
    constant( F ).

unsigned_integer( I ) -->
    [I], { integer(I), I >= 0 }.


integer( I ) -->
    [I], { integer(I) }.


identifier( N ) -->
    [N], { atom(N) }.


constant( C ) -->
    integer( C ).
constant( C ) -->
    [true].
constant( C ) -->
    [false].


string( 'STR'(S) ) -->
    [ string(S) ].


test_parser does
    do write( 'Program? ' ) and do nl and
    do read_list( L ) and
    do write( parse(L) ) and
    do nl and
    do test_parser.
                      
