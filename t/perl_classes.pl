:- use_module(library(regex)).

:- use_module(library(tap)).

'one two' =~ '\\s'.
'one two' =~ '^\\S'.

'123 main' =~ '^\\d+'.
'123 main' \~ '^\\D+'.

'123 main' =~ '\\w'.
'123 main' \~ '^\\W'.

'9876' =~ '\\d'.
'9876' \~ '\\D'.
' \t' \~ '\\w'.
' \t' =~ '\\W'.
' \t' =~ '\\s'.
' \t' \~ '\\S'.
