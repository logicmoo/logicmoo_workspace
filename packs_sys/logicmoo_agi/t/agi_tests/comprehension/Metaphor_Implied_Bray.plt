test_e2c([

  nlu_assert('There are several types of metaphor'),
  nlu_assert(
    [ 
    'A metaphor is an expression whose meaning is not literal and has a figurative meaning.',
  'The expression of a metaphor and its meaning are sometimes related by 
    an analogy or other similarity.',
    'unsual word choice can be an indicator of a type of figurative meaning called an implied metaphor.',
    'An implied metaphor is a more subtle form of comparison;
   the terms being compared are not so specifically explained. ',
     'In implied metaphor the word choice will imply some additional meaning, similar to an analogy.'
    ]),
%oy, definition feels incomplete here ^^^

  nlu_assert(['linda asked jim to leave',
             'jim brayed his refusal and sat down on the porch instead',
              'linda went inside and closed the door'])

  nlu_ask_answer(
   'How many agents are here?',
   'At least two.', [infer_unique_agents]),

  nlu_ask_answer(
   'did jim leave when linda asked?',
   'no.', [infer_jim_didnt_leave]),

 nlu_assert(['bray as a noun means the loud, harsh cry of a donkey or mule.',
            'bray as a verb is to utter a bray.', 'jim is a human and no other animal.']).

 nlu_ask_answer(
    'is jim a human?',
    'yes',
     [jim_is_human]).

 nlu_ask_answer(
    'Is jim a donkey or mule?',
    'No',
    [jim_not_a_literal_ass]).

  nlu_ask_answer(
    'was jim compared to a donkey or mule?',
    'yes.'
    [notice_jim_is_compared_to_ass]),

  nlu_ask_answer(
    'which word compared jim to a donkey or mule?',
    'brayed.'
    [notice_jim_brayed]),

nlu_assert(['"jim brayed his refusal" is a implied metaphor meaning he acted similarly with a donkey or mule.']).

% implied metaphors are tricky and often its perfectly alright for the reader to
% miss the meaning even though they help to infer consequences.
% however extended and controling metaphors are often not explicitly stated,
 % the meaning relies on the identification of implied metaphor.
% should implied metaphor fail, expect extended and controling metaphor to fail.

 nlu_ask_answer(
    'could Jim be like a mule?',
    'if mule is a implied metaphor.',
    [is_mule_implied_metaphor]) ]),

['implied metaphor "brayed". If failed, expect fail of extended metaphor. CasAm']).



