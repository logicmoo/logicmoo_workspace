/*

Script for analysing result of a make trace.
Invoke as follows:

sicstus -l $REGULUS/Prolog/summarise_make_trace_top.pl -a <TraceFile>

The script reads the file and prints a summary of errors

Example:

sicstus -l $REGULUS/Prolog/summarise_make_trace.pl -a $CALLSLT/Fre/scripts/make_trace_2011_08_09-13_41_50.txt

*/

:- compile('$REGULUS/Prolog/summarise_make_trace.pl').

:- prolog_flag(argv, Args), summarise_make_trace_from_command_line(Args).

:- halt.

	