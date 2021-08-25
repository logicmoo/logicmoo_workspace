/* PD624 code file: DEMO4.PL
Code based on Appendix B section 4.3, to test
inheritance, type-checking, cardinality-checking, change_rule demons,
and access_rule demons.  The top-level queries shown in that section
can be typed in directly by you once this file is reconsulted
(but notice that the description of tweetie is already included in this file,
so you don't have to bother entering it).  Alternatively, the test cases
at the bottom of the file can be invoked as follows:

      ?- run_demo4.

All parts of this file can co-exist, even though they come from separate
sections of the reference manual.  Therefore, you can just reconsult
this entire file and try out the examples from the different sections.
*/

/* ===== inheritance, type, and cardinality (sections B4.3.2-B4.3.4) == */

man subclass_of person with   /* old sense of man as 'mankind' */
  sex:
    [value: male,
     type: [male, female]].   /* must be one of the specified items */

tom instance_of man with
  age:
    [value: 34,
     type: integer],
  hobbies: [skiing, photography].

animals subclass_of beings with
  has_skin:
    [value: yes,
     type: [yes, no],
     cardinality: 1],
  can_breathe: yes.

bird subclass_of animals with
  can_fly:
    [value: yes,
     type: [yes, no],
     cardinality: 1],
  eats:
    [value: [worms, seeds],
     inheritance: merge,
     cardinality: 1-5].

ostrich subclass_of bird with
  can_fly: no,
  eats:
    [value: [slugs, snails],
     inheritance: merge].

canary subclass_of bird with
  colour: yellow,
  eats:
    [value: sainsbury_bird_food,
     inheritance: merge].

tweetie instance_of canary with  /* included for you... */
  size: small.                   /* so you don't have to use 'note' */

oscar instance_of ostrich with  /* an extra one, not in course text */
  age: 34.

ollie instance_of ostrich with
  eats: [worms, pellets].  /* notice merge not specified here */



/* ============ change_rule example (section B4.3.5) ==================== */

/* this single frame for person below combines two examples in one, by
containing slots for both husband and wife (these are presented as
separate examples in section B4.3.5, but work perfectly well together)
*/

/* The example assumes the case when someone already has a child, and
then marries (or re-marries).  Of course, the change_rule demon would also
be triggered when we happened first to HEAR about an already-existing husband,
in which case it would (over-zealously) assert that husband was the
step_father of the child (when it might be the original father instead).
This is because there is no notion of temporal sequence in this simple
representation, and therefore the child's existence is assumed to pre-date
the 'wedding', thereby making the husband (logically) a step-father.
*/

person subclass_of animate_things with
  husband:
    [value: X,                           /* <-- this variable is crucial */
     change_rule:                        /* when some person (re)marries... */
       (if
          the child of ?self is Kid      /* if they already have a child.. */
        then
          note the step_father of Kid is X)], /* inform it of new step father */
  wife:
    [value: [],     /* variable not needed here, as it is not referred to */
     change_rule:
       (if
          true
        then
          note the marital_status of ?self is married)].


jane instance_of person with
  child: [jackie],  /* use list in case of many children! */
  husband: joe.

jackie instance_of person with
  father: joe.

joe instance_of person with
  marital_status: single.


/* ========= access_rule demons (section B4.3.6) ===================== */
/* The text uses two consecutive examples with the same names,
but in this file we give them different names so that they can be loaded
together. */

tank subclass_of vessel with
  volume:
   [value: unknown,
    access_rule:
     (if
       the height of ?self is Height &
       the width of ?self is Width &
       the depth of ?self is Depth &
       prolog(Volume is Height*Width*Depth)
      then
       make_value Volume)].     /* key-word 'make_value' is critical */

small_tank instance_of tank with
  height: 10,
  width: 10,
  depth: 10.

tall_tank instance_of tank with
  height: 20,
  width: 10,
  depth: 10.

tank2 subclass_of vessel with
  volume:
   [value: unknown,
    access_rule:
     (if
       the height of ?self is Height &
       the width of ?self is Width &
       the depth of ?self is Depth &
       prolog(Volume is Height*Width*Depth)
      then
       the volume of ?self is Volume)]. /* run-time deduction, not stored */

small_tank2 instance_of tank2 with
  height: 10,
  width: 10,
  depth: 10.

tall_tank2 instance_of tank2 with
  height: 20,
  width: 10,
  depth: 10.


/* ===================== examples to try: ============================== */
/* any of the following can be tried directly from top-level, e.g.
  ?- the can_fly of ostrich is X.

alternatively, just invoke

  ?- run_demo4.

to set the whole thing going.
*/


pause :-   /* one utility to allow pauses at convenient spots */
  nl,      /* output new-line */
  nl,      /* and again */
  write('Please press RETURN key.'),
  get0(X).   /* reads character from keyboard (any one will do) */

run_demo4 :-  /* uses Prolog to invoke lots of separate MIKE calls */
  announce ['Testing section B4.3.2. (inheritance)', nl],
  /* section B4.3.2 */
  the can_fly of ostrich is X,
  announce ['ostrich can_fly... ', X, nl],
  the can_fly of canary is Y,
  announce ['canary can_fly... ', Y, nl],
  the eats of canary is worms,
  announce ['Here are the descriptions of canary and ostrich:',nl],
  describe canary,
  describe ostrich,
  announce ['But notice what happens with inheritance : merge ...', nl],
  all eats of canary are What1,
  announce ['all eats of canary are... ', What1, nl],
  all eats of ostrich are What2,
  announce ['all eats of ostrich are... ', What2, nl],
  all can_fly of ostrich are What3,
  announce ['all can_fly of ostrich are ', What3, nl],
  pause,
  /* section B4.3.3 & B4.3.4 */
  /* the next three uses of 'note' should generate warning messages */
  announce ['Testing sections B4.3.3/4. (type and cardinality checking)', nl],
  describe tom,
  describe tweetie,
  pause,
  announce [nl, 'The next examples illustrate violation of type or', nl,
     '  cardinality restrictions, leading to WARNING messages, e.g.',nl],
  note the age of tom is old,
  note the sex of tom is neuter,
  note the eats of tweetie is [dog_food, cat_food, chicken, fish, liver,
                                  hot_dogs],
  pause,
  describe tom,
  describe tweetie,
  pause,
  /* section B4.3.5 */
  announce ['Testing section B4.3.5. (change_rule demons)', nl],
  describe jane,
  describe jackie,
  describe joe,
  announce ['New husband for jane will now invoke a change_rule demon...', nl],
  pause,
  note the husband of jane is fred,
  describe jane,
  describe jackie,
  announce [nl, 'Marriage of joe to jane will invoke a change_rule demon...',
            nl],
  note the wife of joe is jane,
  describe joe,
  pause,
  /* section B4.3.6 */
  announce ['Testing section B4.3.6 (access_rule demons)', nl],
  announce ['access_rule will side-effect the following frames...', nl],
  describe small_tank,
  describe tall_tank,
  the volume of small_tank is What4,
  announce ['access_rule computed volume of small_tank: ', What4, nl],
  the volume of tall_tank is What5,
  announce ['access_rule computed volume of tall_tank: ', What5, nl],
  describe small_tank,
  describe tall_tank,
  pause,
  announce ['About to perform similar task, but without side-effect...', nl],
  describe small_tank2,
  describe tall_tank2,
  the volume of small_tank2 is What6,
  announce ['access_rule computed volume of small_tank2: ', What6, nl],
  the volume of tall_tank2 is What7,
  announce ['access_rule computed volume of tall_tank2: ', What7, nl],
  describe small_tank2,
  describe tall_tank2.




