
/*

Grammar for tagging surface strings - applied before surface patterns.

The point is that "surface" patterns are something of a misnomer. If we
really just matched surface strings, we would have terribly specific patterns.
We definitely want the patterns to generalise over things like numbers. So
we apply a chunking grammar first, to e.g. replace [twenty, five] with 25.
The grammar is structured as a DCG.

Things to watch out for:

- Collisions between chunking rules are resolved by using the earlier rule.

- The rules have to be able to work in both directions

*/

tagging_grammar(N) -->
	quantity_number(N).

quantity_number(number(N)) -->
	number_0_99(N).

number_0_99(0) --> [zero].
number_0_99(0) --> [zero, zero].
number_0_99(0) --> [oh].
number_0_99(0) --> [oh, oh].
number_0_99(1) --> [one].
number_0_99(2) --> [two].
number_0_99(3) --> [three].
number_0_99(4) --> [four].
number_0_99(5) --> [five].
number_0_99(5) --> [fiver].
number_0_99(6) --> [six].
number_0_99(7) --> [seven].
number_0_99(8) --> [eight].
number_0_99(9) --> [nine].
number_0_99(9) --> [niner].
number_0_99(10) --> [ten].
number_0_99(11) --> [eleven].
number_0_99(12) --> [twelve].
number_0_99(13) --> [thirteen].
number_0_99(14) --> [fourteen].
number_0_99(15) --> [fifteen].
number_0_99(16) --> [sixteen].
number_0_99(17) --> [seventeen].
number_0_99(18) --> [eighteen].
number_0_99(19) --> [nineteen].
number_0_99(20) --> [twenty].
number_0_99(21) --> [twenty,one].
number_0_99(22) --> [twenty,two].
number_0_99(23) --> [twenty,three].
number_0_99(24) --> [twenty,four].
number_0_99(25) --> [twenty,five].
number_0_99(25) --> [twenty,fiver].
number_0_99(26) --> [twenty,six].
number_0_99(27) --> [twenty,seven].
number_0_99(28) --> [twenty,eight].
number_0_99(29) --> [twenty,nine].
number_0_99(29) --> [twenty,niner].
number_0_99(30) --> [thirty].
