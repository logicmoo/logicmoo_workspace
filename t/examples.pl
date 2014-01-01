:- use_module(library(regex)).

:- use_module(library(tap)).

% character ranges
deadbeef =~ '[a-f0-9]+'.
"1973c" =~ '[a-f0-9]+'.
`feed` =~ '[a-f0-9]+'.
'C' =~ '[A-Z0-9]'.
'trailing-dash' =~ '[a-zA-Z0-9-]+'.
'leading-dash' =~ '[-a-zA-Z0-9]+'.

% + meta character
abbba =~ 'ab+a'.

% * meta character
aa =~ "ab*a".

% union
cat =~ 'cat|dog'.
dog =~ 'cat|dog'.
pig \~ 'cat|dog'.

% ^ meta character
'begin with ^'(todo) :- begin =~ '^beg'.
'beggar with ^'(todo) :- beggar =~ '^beg'.
'i beg with ^'(todo) :- 'i beg' \~ '^beg'.
'hello world' =~ world.  % no anchor matches anywhere in string
'i beg' =~ beg.

% $ meta character
dog =~ 'dog$'.
doggie \~ 'dog$'.

% . meta character
cat =~ 'c.t'.
cot =~ 'c.t'.
'c-t' =~ 'c.t'.

% ? meta character
https =~ 'https?'.
http =~ 'https?'.
bot =~ 'boo?t'.
boot =~ 'boo?t'.

% quantification
http =~ 'ht{2}p'.
bot \~ 'bo{2,}t'.
boot =~ 'bo{2,}t'.
booot =~ 'bo{2,}t'.
boooot =~ 'bo{2,}t'.
bot \~ 'bo{2,4}t'.
boot =~ 'bo{2,4}t'.
booot =~ 'bo{2,4}t'.
boooot =~ 'bo{2,4}t'.
booooot \~ 'bo{2,4}t'.
