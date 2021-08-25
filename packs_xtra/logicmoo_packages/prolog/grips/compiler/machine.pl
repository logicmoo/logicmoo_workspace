/*  MACHINE.PL  */


/*
The machine-code interpreter.
-----------------------------

This file exports the function 'run' and the command 'test_run'. These
are described below. They both act on a machine state, represented
as a septuple:
    'MACHINE'( Stack, Store, Code, PC, In, Out )
where
    Stack is its evaluation stack;
    Store is its memory (a mapping from locations to values);
    Code  is the runnable code;
    PC    is the program pointer;
    In    is the input file;
    Out   is the output file.

The components are represented as follows:            
    Stack : a list of elements, with the top at the front.
    Store : a 'map' from location to value. These maps are implemented
            by library MAP.PL.
    Code  : a 'map' from address to instruction.
    PC    : an integer.
    In    : a list of elements, with the first at the front.
    Out   : as for In.


The main function exported is
    run( Machine ) -> Machine
which takes a machine-state and runs it until a stop instruction is
encountered, or until an instruction can't be found at the machine's PC.

'run' calls 'step' which in turn calls
    next_state( Instruction, Machine ) -> Machine.
This is the state-transition function and the thing that does all the
instruction decoding.


There is a command analogue of 'run'. This is 'test_run' which writes
out the successive states.
*/


run( 'MACHINE'( Stack, Store, Code, PC, In, Out ) ) <-
    step( 'MACHINE'( Stack, Store, Code, PC, In, Out ), geti( Code, PC ) ).


test_run( 'MACHINE'( Stack, Store, Code, PC, In, Out ) ) does
    do test_step( 'MACHINE'( Stack, Store, Code, PC, In, Out ), geti( Code, PC ) ).


geti( Code, PC ) <-
    stop
    if
    not( map_defined( Code, PC ) ).

geti( Code, PC ) <-
    map_value( Code, PC ).


step( Machine, stop ) <-
    Machine.

step( Machine, Op ) <-
    run( next_state( Op, Machine ) ).


test_step( Machine, stop ) does
    do write_machine( Machine ) and do nl and
    do write( 'Finished.' ) and do nl.

test_step( Machine, Op ) does
    do write_machine( Machine ) and do nl and
    do write( 'About to do ' ) and do write( Op ) and do nl and
    do test_run( next_state( Op, Machine ) ).


next_state( 'OP'( storebyte, Loc ), 'MACHINE'( [ Top | Rest ], Store, Code, PC, In, Out ) ) <-
    'MACHINE'( Rest, map_update( Store, Loc, Top ), Code, PC+4, In, Out ).

next_state( 'OP'( store, Loc ), 'MACHINE'( [ Top | Rest ], Store, Code, PC, In, Out ) ) <-
    'MACHINE'( Rest,
               map_update(
                            map_update( Store, Loc, byte(Top,1) ),
                            Loc+1, byte(Top,2)
               ),
               Code, PC+4, In, Out
    ).

next_state( 'OP'( loadconst, Const ), 'MACHINE'( Rest, Store, Code, PC, In, Out ) ) <-
    'MACHINE'( [ Const | Rest ], Store, Code, PC+4, In, Out ).

next_state( 'OP'( load, Loc ), 'MACHINE'( Rest, Store, Code, PC, In, Out ) ) <-
    'MACHINE'( [ Top | Rest ], Store, Code, PC+4, In, Out )
    where
    ( Top = pack( map_value(Store,Loc), map_value(Store,Loc+1) ) ).

next_state( 'OP'( loadbyte, Loc ), 'MACHINE'( Rest, Store, Code, PC, In, Out ) ) <-
    'MACHINE'( [ Top | Rest ], Store, Code, PC+4, In, Out )
    where
    ( Top = map_value( Store, Loc ) ).

next_state( 'OP'( goto, Label ), 'MACHINE'( Rest, Store, Code, PC, In, Out ) ) <-
    'MACHINE'( Rest, Store, Code, Label, In, Out ).

next_state( 'OP'( jump_if_false, Label ), 'MACHINE'( [ Top | Rest ], Store, Code, PC, In, Out ) ) <-
    'MACHINE'( Rest, Store, Code, Label, In, Out )
    if Top = 0.

next_state( 'OP'( jump_if_false, Label ), 'MACHINE'( [ Top | Rest ], Store, Code, PC, In, Out ) ) <-
    'MACHINE'( Rest, Store, Code, PC+4, In, Out )
    if Top \= 0.

next_state( 'OP'( read ), 'MACHINE'( Rest, Store, Code, PC, [ F1 | Fn ], Out ) ) <-
    'MACHINE'( [ F1 | Rest ], Store, Code, PC+2, Fn, Out ).

next_state( 'OP'( write ), 'MACHINE'( [ Top | Rest ], Store, Code, PC, In, Out ) ) <-
    'MACHINE'( Rest, Store, Code, PC+2, In, (Out ++ [Top]) ).

next_state( 'OP'( writestring ), 'MACHINE'( [ Length | Rest ], Store, Code, PC, In, Out ) ) <-
    writestring( Length, 'MACHINE'( Rest, Store, Code, PC+2, In, Out ), [] ).

next_state( 'OP'( eq ), 'MACHINE'( [ Top1, Top1 | Rest ], Store, Code, PC, In, Out ) ) <-
    'MACHINE'( [ 1 | Rest ], Store, Code, PC+2, In, Out ).

next_state( 'OP'( eq ), 'MACHINE'( [ Top1, Top2 | Rest ], Store, Code, PC, In, Out ) ) <-
    'MACHINE'( [ 0 | Rest ], Store, Code, PC+2, In, Out )
    if Top1 \= Top2.

next_state( 'OP'( not ), 'MACHINE'( [ 0 | Rest ], Store, Code, PC, In, Out ) ) <-
    'MACHINE'( [ 1 | Rest ], Store, Code, PC+2, In, Out ).

next_state( 'OP'( not ), 'MACHINE'( [ _ | Rest ], Store, Code, PC, In, Out ) ) <-
    'MACHINE'( [ 0 | Rest ], Store, Code, PC+2, In, Out ).

next_state( 'OP'( add ), 'MACHINE'( [ Top1, Top2 | Rest ], Store, Code, PC, In, Out ) ) <-
    'MACHINE'( [ Top1+Top2 | Rest ], Store, Code, PC+2, In, Out ).

next_state( 'OP'( sub ), 'MACHINE'( [ Top1, Top2 | Rest ], Store, Code, PC, In, Out ) ) <-
    'MACHINE'( [ Top1-Top2 | Rest ], Store, Code, PC+2, In, Out ).

next_state( 'OP'( mult ), 'MACHINE'( [ Top1, Top2 | Rest ], Store, Code, PC, In, Out ) ) <-
    'MACHINE'( [ Top1*Top2 | Rest ], Store, Code, PC+2, In, Out ).

next_state( 'OP'( div ), 'MACHINE'( [ Top1, Top2 | Rest ], Store, Code, PC, In, Out ) ) <-
    'MACHINE'( [ Top1 div Top2 | Rest ], Store, Code, PC+2, In, Out ).

next_state( 'OP'( logand ), 'MACHINE'( [ 0,0 | Rest ], Store, Code, PC, In, Out ) ) <-
    'MACHINE'( [ 0 | Rest ], Store, Code, PC+2, In, Out ).

next_state( 'OP'( logand ), 'MACHINE'( [ 0,_ | Rest ], Store, Code, PC, In, Out ) ) <-
    'MACHINE'( [ 0 | Rest ], Store, Code, PC+2, In, Out ).

next_state( 'OP'( logand ), 'MACHINE'( [ _,0 | Rest ], Store, Code, PC, In, Out ) ) <-
    'MACHINE'( [ 0 | Rest ], Store, Code, PC+2, In, Out ).

next_state( 'OP'( logand ), 'MACHINE'( [ _,_ | Rest ], Store, Code, PC, In, Out ) ) <-
    'MACHINE'( [ 1 | Rest ], Store, Code, PC+2, In, Out ).

next_state( 'OP'( logor ), 'MACHINE'( [ 0,0 | Rest ], Store, Code, PC, In, Out ) ) <-
    'MACHINE'( [ 0 | Rest ], Store, Code, PC+2, In, Out ).

next_state( 'OP'( logor ), 'MACHINE'( [ _,_ | Rest ], Store, Code, PC, In, Out ) ) <-
    'MACHINE'( [ 1 | Rest ], Store, Code, PC+2, In, Out ).


byte( Int, 1 ) <-
    Int mod 256.

byte( Int, 2 ) <-
    Int div 256.


pack( Byte1, Byte2 ) <-
    Byte1 + Byte2 * 256.


writestring( 0, 'MACHINE'( Rest, Store, Code, PC, In, Out ), Chars ) <-
    'MACHINE'( Rest, Store, Code, PC, In, Out++[String] )
    where
    name( String, Chars ).

writestring( Left, 'MACHINE'( [ C | Rest ], Store, Code, PC, In, Out ), Chars ) <-
    writestring( Left-1, 'MACHINE'( Rest, Store, Code, PC, In, Out ), Chars++[C] ).


write_machine( 'MACHINE'( Stack, Store, Code, PC, In, Out ) ) does
    do write( 'Stack: ' ) and do write( Stack ) and do nl and
    do write( 'Store: ' ) and do portray_map( Store ) and do nl and
    do write( 'PC   : ' ) and do write( PC ) and do nl and
    do write( 'In   : ' ) and do write( In ) and do nl and
    do write( 'Out  : ' ) and do write( Out ) and do nl.


'MACHINE'( Stack, Store, Code, PC, In, Out ) <-
    q( 'MACHINE'( Stack, Store, Code, PC, In, Out ) ).
