/*  COMPILE.PL  */


/*
    This and its associated files (except for the two Edinburgh Tools
    libraries) were written by Jocelyn Paine, Department of Experimental
    Psychology, Oxford University in 1989.
    Use them as you wish, provided that you retain this acknowledgement.
*/


/*
Demonstration compiler.
-----------------------

This is the master file for a demonstration compiler written in GRIPS.

It compiles programs written in a (very small) subset of Pascal. For
the syntax (which should also make the semantics obvious) see PARSER.PL.
An example program is in PROGRAM. The compiler generates stack-code for
a simple virtual machine. This can then be run by an interpreter.

The compiler consists of:
1.  A lexical analyser:                         LEX.PL.
2.  A parser:                                   PARSER.PL.
3.  A code-generator:                           CODE_GENERATE.PL.
4.  A loader (which fixes up label references): LOAD.PL.
5.  An interpreter:                             MACHINE.PL.

There are two auxiliary files, ORDSET.PL and MAP.PL. These are from the
Edinburgh Tools library, and define a representation for sets and mappings.

As far as possible, I have written the compiler in a functional style
(but without using higher-order functions). The functions model concepts
familiar in theoretical semantics, such as environments: I have used
the maps library here to represent some of the functions (such as the
store). I originally wrote this demonstration for a mathematician
who was starting a computer science MSc, to give him a simple model, in
his idiom, of how compilers work and what they do. Thank you Hendrik
for giving me something big to demonstrate GRIPS on.
*/


do reconsult( 'ordset.pl' ).
do reconsult( 'map.pl' ).
do grips_reconsult( 'lex.pl' ).
do grips_reconsult( 'parser.pl' ).
do grips_reconsult( 'code_generate.pl' ).
do grips_reconsult( 'load.pl' ).
do grips_reconsult( 'machine.pl' ).


compile( Program ) <-
    code_generate( parse( Program ) ).


demo does
    do read_tokens( Tokens ) and
    do write( 'Tokens           : ' ) and do write( Tokens ) and do nl and
    do parse(Tokens,Tree) and
    do write( 'Tree             : ' ) and do write( Tree ) and do nl and
    do test_code_generate( Tree, 'CODE'(Code,InitialStore) ) and
    do write( 'Code             : ' ) and do write( Code ) and do nl and
    do loadcode( Code, LoadedCode ) and
    do write( 'Loaded code      : ' ) and do portray_map( LoadedCode ) and do nl and
    do write( 'Initial store    : ' ) and do portray_map( InitialStore ) and do nl and
    do write( 'Input? ' ) and do nl and
    do read_list( In ) and
    do test_run( 'MACHINE'( [], InitialStore, LoadedCode, 0, In, [] ) ).


read_tokens( L ) does
    seeing( CIS ) and
    see( 'program.' ) and seen and
    see( 'program.' ) and
    read_list( L ) and
    see( CIS ).
