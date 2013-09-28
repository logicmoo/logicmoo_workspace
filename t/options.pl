:- use_module(library(regex)).

:- use_module(library(tap)).

% i option succeeds
'ABC' =~ abc/i.
abc =~ abc/i.
'ABC' =~ 'ABC'/i.
abc =~ 'ABC'/i.
'ABC' =~ aBc/i.
abc =~ aBc/i.
aBc =~ aBC/i.

% i option affects character classes too
'E' =~ '[a-z]'/i.
'E' =~ '[A-Z]'/i.
'e' =~ '[a-z]'/i.
'e' =~ '[A-Z]'/i.

% i option doesn't help
foo \~ abc/i.


% s option (single-line mode)
'abc\ndef' \~ 'c.d'.
'abc\ndef' =~ 'c.d'/s.
