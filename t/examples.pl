:- use_module(library(re)).

:- use_module(library(tap)).

% character ranges
deadbeef =~ '[a-f0-9]+'.
"1973c" =~ '[a-f0-9]+'.

% + meta character
abbba =~ 'ab+a'.

% * meta character
aa =~ "ab*a".

% union
cat =~ 'cat|dog'.
dog =~ 'cat|dog'.
pig \~ 'cat|dog'.

% $ meta character
dog =~ 'dog$'.
doggie \~ 'dog$'.

% . meta character
cat =~ 'c.t'.
cot =~ 'c.t'.
'c-t' =~ 'c.t'.

% TODO
% "hello world" =~ "world".  % match anywhere inside string
