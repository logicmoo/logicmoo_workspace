:- use_module(library(regex)).

:- use_module(library(tap)).

'one two' =~ '\\s'.
'^ with \\S' :- 'one two' =~ '^\\S'.

'^ with \\d' :- '123 main' =~ '^\\d+'.
'^ with \\D' :- '123 main' \~ '^\\D+'.

'123 main' =~ '\\w'.
'^ with \\W' :- '123 main' \~ '^\\W'.

'9876' =~ '\\d'.
'9876' \~ '\\D'.
' \t' \~ '\\w'.
' \t' =~ '\\W'.
' \t' =~ '\\s'.
' \t' \~ '\\S'.
