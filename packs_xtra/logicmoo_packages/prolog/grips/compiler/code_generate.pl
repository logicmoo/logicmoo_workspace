/*  CODE_GENERATE.PL  */


code_generate( 'PROG'(Name,Declarations,Body) ) <-
    'CODE'( Code, InitialStore )
    where
    ( 'ENV'( _, _, VarEnv ) = environment( Declarations ) and          
      Code = fixup( cg( Name, E , Body ) ) and
      InitialStore = fill( VarEnv )
    ).


test_code_generate( 'PROG'(Name,Declarations,Body), 'CODE'(Code,InitialStore) ) does
    do environment( Declarations, E ) and
    do write( 'Environment      : ') and do write_environment(E) and do nl and
    do cg( Name, E, Body, Unfixed ) and
    do write( 'Unfixedup code   : ') and do write(Unfixed) and do nl and
    do fixup( Unfixed, Code ) and
    do E = 'ENV'( _, _, VarEnv ) and do fill( VarEnv, InitialStore ).


environment( 'DECLARATIONS'( Labels, Consts, Vars ) ) <-
    'ENV'(
           label_environment( Labels ),
           constant_environment( Consts ),
           variable_environment( Vars, 1000 )
    ).


write_environment( 'ENV'( Labels, Consts, Vars ) ) does
    do write( Labels ) and do write('  ') and
    do portray_map( Consts ) and do write('  ') and
    do portray_map( Vars ).


label_environment( _ ) <- 0.


constant_environment( [] ) <- map.
constant_environment( [ 'C'(Name,Value) | Rest ] ) <-
    map_update( constant_environment( Rest ),
                Name, Value
    ).


variable_environment( [], Start ) <- map.
variable_environment( [ 'V'(Name,boolean) | Rest ], Start ) <-
    map_update( variable_environment( Rest, Start+1 ),
                Name, 'LOC'(Start,1)
    ).
variable_environment( [ 'V'(Name,integer) | Rest ], Start ) <-
    map_update( variable_environment( Rest, Start+2 ),
                Name, 'LOC'(Start,2)
    ).


lookup_var( 'ENV'( LabelEnv, ConstEnv, VarEnv ), Var ) <-
    map_value( VarEnv, Var ).


location( Env, Var ) <-
    Start where ( 'LOC'(Start,Width) = lookup_var( Env, Var ) )
    if
    is_var( Env, Var ).

location( Env, Var ) <-
    'ERROR'.


width( Env, Var ) <-
    Width where ( 'LOC'(Start,Width) = lookup_var( Env, Var ) ).


lookup_const( 'ENV'( LabelEnv, ConstEnv, VarEnv ), Const ) <-
    map_value( ConstEnv, Const ).


is_var( 'ENV'( LabelEnv, ConstEnv, VarEnv ), Var ) :-
    map_defined( VarEnv, Var ).


is_const( 'ENV'( LabelEnv, ConstEnv, VarEnv ), Const ) :-
    map_defined( ConstEnv, Const ).


cg( Name, Environment, Body ) <-
    cg_statementlist( Environment, Body ).


cg_statementlist( Environment, [] ) <-
    [ ].

cg_statementlist( Environment, [ Statement | Rest ] ) <-
    cg_statement( Environment, Statement ) ++ cg_statementlist( Environment, Rest ).

cg_statementlist( Environment, Statement ) <-
    cg_statement( Environment, Statement ).


cg_statement( Environment, 'ASSIGN'(Variable,Expression) ) <-
    cg_expression( Environment, Expression ) ++
    [ 'ERRORASSIGN' ]
    if
    ( atom(Variable) and not( is_var( Environment, Variable ) ) ).

cg_statement( Environment, 'ASSIGN'(Variable,Expression) ) <-
    cg_expression( Environment, Expression ) ++
    [ 'OP'( store, location(Environment,Variable) ) ]
    if
    ( atom(Variable) and width( Environment, Variable ) = 2 ).

cg_statement( Environment, 'ASSIGN'(Variable,Expression) ) <-
    cg_expression( Environment, Expression ) ++
    [ 'OP'( storebyte, location(Environment,Variable) ) ]
    if
    ( atom(Variable) and width( Environment, Variable ) = 1 ).

cg_statement( Environment, 'GOTO'(L) ) <-
    [ 'OP'( goto, L ) ].

cg_statement( Environment, 'LABELLED'(L,S) ) <-
    [ 'LABEL'(L) ] ++ cg_statement( Environment, S ).

cg_statement( Environment, 'IF'(Cond,Then) ) <-
    (
        cg_expression( NewEnvironment, Cond ) ++ [ 'OP'( jump_if_false, L ) ]                  
        ++ cg_statementlist( NewEnvironment, Then )
        ++ [ 'LABEL'(L) ]
    )
    where new_label( Environment, NewEnvironment, L ).

cg_statement( Environment, 'READ'(Variable) ) <-
    [ 'OP'( read ), 'OP'( storebyte, location(Environment,Variable) ) ]
    if
    ( atom(Variable) and width( Environment, Variable ) = 1 ).

cg_statement( Environment, 'READ'(Variable) ) <-
    [ 'OP'( read ), 'OP'( store, location(Environment,Variable) ) ]
    if
    ( atom(Variable) and width( Environment, Variable ) = 2 ).

cg_statement( Environment, 'READ'(Variable) ) <-
    [ 'ERRREAD' ].

cg_statement( Environment, 'WRITE'('STR'(V)) ) <-
    cg_string( Environment, V )    
    ++
    [ 'OP'( writestring ) ].

cg_statement( Environment, 'WRITE'(V) ) <-
    cg_expression( Environment, V )
    ++
    [ 'OP'( write ) ].

cg_statement( Environment, 'LIST'(S) ) <-
    cg_statementlist( Environment, S ).

cg_statement( Environment, _ ) <-
    [ 'ERRSTMT' ].


cg_expression( Environment, true ) <-
    [ 'OP'( loadconst, 1 ) ].

cg_expression( Environment, false ) <-
    [ 'OP'( loadconst, 0 ) ].

cg_expression( Environment, I ) <-
    [ 'OP'( loadconst, I ) ]
    if
    integer( I ).

cg_expression( Environment, Var ) <-
    [ 'OP'( loadbyte, location( Environment, Var ) ) ]
    if
    ( atom(Var) and width( Environment, Var ) = 1 ).

cg_expression( Environment, Var ) <-
    [ 'OP'( load, location( Environment, Var ) ) ]
    if
    ( atom(Var) and width( Environment, Var ) = 2 ).

cg_expression( Environment, Const ) <-
    cg_expression( Environment, lookup_const(Environment,Const) )
    if
    ( atom(Const) and is_const( Environment, Const ) ).

cg_expression( Environment, Const ) <-
    [ 'ERRID' ]
    if
    atom(Const).

cg_expression( Environment, 'E'((=),E1,E2) ) <-
    cg_expression(Environment,E1) ++
    cg_expression(Environment,E2) ++
    [  'OP'( eq ) ].

cg_expression( Environment, 'E'((<>),E1,E2) ) <-
    cg_expression(Environment,E1) ++
    cg_expression(Environment,E2) ++
    [ 'OP'( eq ),
      'OP'( not )
    ] .

cg_expression( Environment, 'E'(+,E1,E2) ) <-
    cg_expression(Environment,E1) ++
    cg_expression(Environment,E2) ++
    [ 'OP'( add ) ].

cg_expression( Environment, 'E'(-,E1,E2) ) <-
    cg_expression(Environment,E1) ++
    cg_expression(Environment,E2) ++
    [  'OP'( sub ) ].

cg_expression( Environment, 'E'(*,E1,E2) ) <-
    cg_expression(Environment,E1) ++
    cg_expression(Environment,E2) ++
    [  'OP'( mult ) ].

cg_expression( Environment, 'E'(/,E1,E2) ) <-
    cg_expression(Environment,E1) ++
    cg_expression(Environment,E2) ++
    [  'OP'( div ) ].

cg_expression( Environment, 'E'(and,E1,E2) ) <-
    cg_expression(Environment,E1) ++
    cg_expression(Environment,E2) ++
    [  'OP'( logand ) ].

cg_expression( Environment, 'E'(or,E1,E2) ) <-
    cg_expression(Environment,E1) ++
    cg_expression(Environment,E2) ++
    [  'OP'( logor ) ].

cg_expression( Environment, _ ) <-
    [ 'ERREXPR' ].


cg_string( Environment, Str ) <-
    cg_string_1( Environment, name(Str) ) ++
    [ 'OP'( loadconst, listlength(name(Str)) ) ].                    


cg_string_1( Environment, [] ) <- [].

cg_string_1( Environment, [C1|Cn] ) <-
    cg_string_1( Environment, Cn ) ++
    [ 'OP'( loadconst, C1 ) ].


listlength( [] ) <- 0.

listlength( [_|T] ) <-
    listlength( T ) + 1.


/*  For generating internal labels.  */
new_label( 'ENV'(LastInternal,Consts,Vars), 'ENV'(NextInternal,Consts,Vars), NextInternal ) if
    NextInternal = LastInternal - 1.


fixup( Code ) <-
    fixup_labels( Code, allocate_addresses( Code ) ).


allocate_addresses( Code ) <-
    allocate1( Code, map, 0 ).


allocate1( [], Map, Loc ) <-
    Map.

allocate1( [ 'LABEL'(L) | Rest ], Map, Loc ) <-
    allocate1( Rest, map_update( Map, L, Loc ), Loc ).

allocate1( [ Instruction | Rest ], Map, Loc ) <-
    allocate1( Rest, Map, Loc+width(Instruction) ).


width( 'OP'(_) ) <-
    2.

width( 'OP'(_,_) ) <-
    4.

width( _ ) <-
    0.


fixup_labels( [], Addresses ) <-
    [].

fixup_labels( [ 'OP'(GOTO,L) | Rest ], Addresses ) <-
    [ 'OP'( GOTO, map_value(Addresses,L) ) ]
    ++
    fixup_labels( Rest, Addresses )
    if
    is_jump( GOTO ) and
    map_defined( Addresses, L ).

fixup_labels( [ 'OP'(GOTO,L) | Rest ], Addresses ) <-
    [ 'ERRGOTO' ]
    ++
    fixup_labels( Rest, Addresses )
    if
    is_jump( GOTO ).      

fixup_labels( [ 'LABEL'(L) | Rest ], Addresses ) <-
    [ 'LABEL'( map_value(Addresses,L) ) ]
    ++
    fixup_labels( Rest, Addresses )
    if
    map_defined( Addresses, L ).

fixup_labels( [ 'LABEL'(L) | Rest ], Addresses ) <-
    [ 'ERRLBL' ]
    ++
    fixup_labels( Rest, Addresses ).

fixup_labels( [ Other | Rest ], Addresses ) <-
    [ Other ]
    ++
    fixup_labels( Rest, Addresses ).


is_jump( goto ).
is_jump( jump_if_false ).


fill( VarEnv ) <-
    fill1( map_range( VarEnv ) ).


fill1( [] ) <-
    map.

fill1( [ 'LOC'(Start,1) | Rest ] ) <-
    map_update( fill1( Rest ), Start, 255 ).

fill1( [ 'LOC'(Start,2) | Rest ] ) <-
    map_update( map_update( fill1(Rest), Start, 255 ), Start+1, 255 ).


'OP'( Op ) <-
    q( 'OP'( Op ) ).


'OP'( Op, Operand ) <-
    q( 'OP'( Op, Operand ) ).


'LABEL'( Label ) <-
    q( 'LABEL'( Label ) ).


'ENV'( E1, E2, E3 ) <-
    q( 'ENV'( E1, E2, E3 ) ).


'LOC'( Start, Width ) <-
    q( 'LOC'( Start, Width ) ).


'CODE'( Code, Store ) <-
    q( 'CODE'( Code, Store ) ).
