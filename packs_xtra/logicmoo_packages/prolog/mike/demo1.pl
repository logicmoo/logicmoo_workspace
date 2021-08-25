/* PD624 code file: DEMO1.PL

Code to put backward-chaining rule tracer through its paces.
To try it, reconsult this file, and invoke tracing options 7 and 8
as follows:
   ?- tracing([7,8]).
Or else just invoke
   ?- tracing.
and respond to the menu of choices, paying particular attention to
options 7 and 8.

Then, invoke MIKE's backward-chainer as follows:

   ?- deduce [What, is, fun].

----------------------------------------------------------------------
An alternative, forward-chaining proof of the same conclusion uses
rule 'fun3' defined below.  It is invoked by typing

    ?- go.

(Do not use the normal ?- fc.  as explained in the comment below)
-----------------------------------------------------------------------
*/


/* Rules 'fun1' and 'fun2' correspond intuitively to the notion that
"Red cars or blue bikes are fun".  To deduce that something is fun, it
is therefore sufficient to deduce that the thing in question is both red
and that it is a car. Alternatively, it is sufficient to deduce that the
thing in question is both blue and that it is a bike */

rule fun1 backward
  if
    [X,is,red] &
    [X,isa,car]
  then
    [X,is,fun].

rule fun2 backward
  if
    [X,is,blue] &
    [X,isa,bike]
  then
    [X,is,fun].


/* ============= alternative forward-chaining variation ================ */

/* N.B.  DO NOT USE  ?- fc. TO TEST THIS OUT!!!   USE  ?- go.  INSTEAD.
THE REASON IS THAT WORKING MEMORY IS MANUALLY 'SEEDED' AT THE END OF THIS
FILE, AND ?- fc. WOULD CLEAR IT OUT (see Appendix B, section 3.3.1)
*/

rule fun3 forward
 if
   [X, is, red] & [X, isa, car] or    /* more intuitive use of disjunction */
   [X, is, blue] & [X, isa, bike]
 then
   add [X, is, fun] &
   announce ['I have concluded what is fun by forward-chaining: ', X] &
   halt.

/* Seed working memory when this file is loaded: */



?- initialise.          /* clear working memory first */
?- add [block1,is,red].
?- add [block2,is,red].
?- add [block3,is,red].
?- add [car1,isa,car].
?- add [car2,isa,car].
?- add [house1,is,blue].
?- add [triangle39,is,blue].
?- add [bike3,is,blue].
?- add [bike1,isa,bike].
?- add [bike2,isa,bike].
?- add [bike3,isa,bike].

?- nl, write('If you decide to use forward chaining to test DEMO1.PL'),nl,
   write('be sure to use  ?- go.  rather than  ?- fc.'),nl,
   write('because working memory has already been "seeded" manually.'),nl.

/* notice that it would have been just as possible to have an 'initialisation
rule' which looked like this:

(remember, the next two rules are enclosed in this large comment....)

rule begin forward
 if
   start
 then
   remove start &
   add [block1,is,red] &
   add [block2,is,red] &
   add [block3,is,red]
   <etc. etc.>

rule make_deduction forward
 if
   deduce [X,is,fun]
 then
   announce ['I have just deduced what is fun: ', X] &
   halt.

There are thus two styles of usage:
  (a) you can seed working memory 'manually', as above, and invoke
'deduce' yourself from top-level;
  (b) you can seed working memory on the right-hand side of a rule, as
in the commented-out rule above, and use a second rule to invoke
'deduce' within a forward-chaining sequence.

Which style you use is a matter of personal taste.

If you opt for the latter style, you can copy and paste the rules
'begin' and 'make_deduction' (you must of course complete the
definition of 'begin'), so that they reside contiguously with the
other rules in this file.

*/

