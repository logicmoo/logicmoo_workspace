test_e2c([
  nlu_assert('There are several types of metaphor'),
  nlu_assert(
    [' A standard metaphor compares two unlike things using the basic construction X is Y.',
    'A metaphor is an expression whose meaning is not literal and has a figurative meaning.',
    'A common metaphor is an expression whose meaning is not literal
     and has a fixed figurative meaning.',
    'A common metaphor is an expression whose meaning is fixed and likely known to others',
  'The expression of a metaphor and its meaning are sometimes related by 
    an analogy or other similarity.' ]),

  nlu_assert(['Jim is a human','Jim is a night owl']),

  nlu_ask_answer(
   'Is Jim a human?',
   'Yes.', [assert_jim_human]),

  nlu_ask_answer(
   'Is Jim an owl?',
   'Yes.', [assert_jim_night_owl]),

 nlu_assert([ 'humans are not owls','night owl is a common metaphor',

  nlu_ask_answer(
    'Is Jim an owl?',
    'no.',
    [jim_is_not_literally_an_owl]),

  nlu_ask_answer(
    'Why is Jim not an owl?',
    'Jim is a human.','Humans are not owls.',
    [why_isnt_jim_an_owl]),


  nlu_ask_answer(
    'What is a night owl?',
    'a common metaphor',
     [night_owl_is_common_metaphor]),

  nlu_assert('night owl is a common metaphor that means an agent is frequently awake at night'),

  nlu_ask_answer(
   "Is Jim frequently awake at night?",
   'Yes.',[meaning_of_common_metaphor]),
 
  nlu_ask_answer(
    'Why is Jim frequently awake at night?',
    'Jim is a night owl.',
    [used_metaphor_night_owl]),
  ])
  nlu_ask_answer(
    'What is a night owl?',
    'a common metaphor for an agent that is frequently awake at night',
     [combined_metaphor_with_meaning])

 nlu_assert(['Linda is awake when Jim is awake.'])

   nlu_ask_answer(
    'Is Linda a night owl?',
    'Yes.',
     [learned_common_metaphor, linda_is_a_night_owl, nightowl, metaphor]),
    ])
),
['common standard metaphor "night owl" is not unique to subject CasAm']).
