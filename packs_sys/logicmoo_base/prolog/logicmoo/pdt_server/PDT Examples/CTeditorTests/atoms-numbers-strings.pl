
/* 
   Testcases of all syntax variations.
   Everything that does not require cross-file scoping.
*/

%============== Facts with all kinds of terms as parameters ==========

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Integers and decimal numbers:
a(0).    % integer
a(1.0).  % decimal number
a(1,0).  % two arguments!

% 2.15.1.5 Using digit groups in large integers
% 
% SWI-Prolog supports splitting long integers into digit groups.
% Digit groups can be separated with the sequence <underscore>, 
% <optional white space>. If the <radix> is 10 or lower, they 
% may also be separated with exactly one space. 
% http://www.swi-prolog.org/pldoc/man?section=digitgroupsyntax
% The following all express the integer 1 million:

a(1_000_000).
a(1 000 000).
a(1_000_/*more*/000).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2.15.1.4 Syntax for non-decimal numbers
% Edinburgh syntax:
%   <radix>'<number>, where <radix> is a number between 2 and 36. 
% ISO syntax: 
%   0[bxo]<number>. For example: A is 0b100 \/ 0xf00 is a valid expression.  
% http://www.swi-prolog.org/pldoc/man?section=nondecsyntax

a(2'1010). % Edinburgh: binary representation of 10
a(8'12).   % Edinburgh: octal representation of 10
a(16'A).   % Edinburgh: hexadecimal representation of 10

%a(1'1).    % Edinburgh: Illegal, radix must be above 1
% <-- Syntax error: Operator expected (is not the most helpful
% <-- message but at least it is an error message
%a(37'1).   % Edinburgh: Illegal, radix must be below 37
% <-- Throws no error at this point but many lines below it will
% <-- say: Syntax error: String too long (see style_check/1)
 
a(0b1010). % ISO: binary representation of 10
a(0o12).   % ISO: octal representation of 10
a(0xA).    % ISO: hexadecimal representation of 10

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2.15.1.2 Nested comments
/* 
   SWI-Prolog allows for nesting /* ... */ comments like this one 
*/
% http://www.swi-prolog.org/pldoc/man?section=nestedcomments

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2.15.1.3 Character Escape Syntax
% Within quoted atoms special characters are represented using escape 
% sequences. An escape sequence is led in by the backslash (\) character.
% Character escaping is only available if 
% current_prolog_flag(character_escapes, true) is active (default). 
% http://www.swi-prolog.org/pldoc/man?section=charescapes

a('abc-\a\b\c\
\e\f\n\r\s\t\v-def').
% The code \xa\3 emits the character 10 (hexadecimal `a') followed by `3'. 
% Characters specified this way are interpreted as Unicode characters. See also \u.
a('abc-\xa\3-def'). 
a('abc-\u1111-def').
%a('abc-\U11111111-def').  % Legal unicode sign in the Hangul character set
%a('\U11111111').          % Legal unicode sign in the Hangul character set
a('\40').
a('\\').
a('\"').
a('\'').
a('\`').
