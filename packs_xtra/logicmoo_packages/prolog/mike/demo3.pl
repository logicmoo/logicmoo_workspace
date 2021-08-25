/* PD624 code file: DEMO3.PL
Test cases taken from Appendix B, sections 4.1 and 4.2.
Example top-level interactions are presented within those sections,
and may be entered directly by you once this file is reconsulted.
An example of how to run the rule augment_fillers is presented at
the end of this file.
*/

man subclass_of person with
    sex: male.

tom instance_of man with
    age: 34,
    hobbies: [skiing, photography].

person subclass_of animate_things with
    species: 'homo sapiens',
    vocabulary_item: 'the word PERSON',
    number_of_legs: 2.

'the word PERSON' instance_of english_word with
    number_of_letters: 6,
    first_letter: p,
    last_letter: n,
    other_letters: [e, r, s, o],
    name_for: person.

joseph instance_of person with
    age: 38,
    hobbies: [swimming, tennis].

mary instance_of person with
    age: 37,
    hobbies: [badmintion, opera].

/* N.B. if the following rule is the ONLY rule, it will cycle
endlessly... even refractoriness wont prevent this because the
'note' in the THEN part of the rule ensures that H1 and H2 have
new (ever-lengthening) instantiations on each cycle!!! Therefore,
a halt is added at the end of the RHS actions.  */

rule augment_fillers forward
    if
      all hobbies of joseph are H1 &
      all hobbies of mary are H2 &
      prolog(append(H1,H2,NewHobbies))
    then
      note the hobbies of joseph is NewHobbies &
      note the hobbies of mary is NewHobbies &
      halt.  /* just to end this example run */


/* You can run the above rule after reconsulting this file just by
typing in
    ?- fc.

To see how joseph has changed, you should then type in:

    ?- describe joseph.

*/
