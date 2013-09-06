:- use_module(library(re)).

:- use_module(library(tap)).

deadbeef =~ '[a-f0-9]+'.
"1973c" =~ '[a-f0-9]+'.
abbba =~ 'ab+a'.
aa =~ "ab*a".
cat =~ 'cat|dog'.
dog =~ 'cat|dog'.
pig \~ 'cat|dog'.
dog =~ 'dog$'.
doggie \~ 'dog$'.
cat =~ 'c.t'.
cot =~ 'c.t'.
'c-t' =~ 'c.t'.

% TODO
% "hello world" =~ "world".  % match anywhere inside string
