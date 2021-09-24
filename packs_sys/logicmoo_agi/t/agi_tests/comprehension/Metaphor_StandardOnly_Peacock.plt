test_e2c([

  nlu_assert(['Jim is a human','Jim is a peacock'])

  nlu_ask_answer(
   'Is Jim a human?',
   'Yes.', [assert_jim_human]),

  nlu_ask_answer(
   'Is Jim a peacock?',
   'Yes.', [assert_jim_peacock]),

 nlu_assert(['humans cannot be another animal','peacock is an animal']).

 nlu_ask_answer(
    'What is a peacock?',
    'a peacock is an animal',
     [peacock_is_animal]).

 nlu_ask_answer(
    'Is Jim a peacock?',
    'No',
    [jim_not_literal_peacock]).
  nlu_ask_answer(
    'Why is Jim not a peacock?',
    'Jim is a human.','Humans cannot be another animal.','a peacock is an animal.'
    [why_jim_is_not_peacock]),
  
 nlu_assert('There are several types of metaphor'),
  nlu_assert(
    [' A standard metaphor compares two unlike things using the basic construction X is Y.',
    'A metaphor is an expression whose meaning is not literal and has a figurative meaning.',
  'The expression of a metaphor and its meaning are sometimes related by 
    an analogy or other similarity.' ]),

nlu_assert(['"jim is a peacock" is a metaphor meaning to show off']).
% This is different than nlu_assert(["'peacock' is a metaphor meaning to show off"]). 
% which would mean peacock is a common metaphor others would know.
% might be good to write in a propting for metaphors as a common expression or not anyway

 nlu_ask_answer(
    'Is Jim a peacock?',
    'If peacock is a metaphor.',
    [is_peacock_a_metaphor]),
  nlu_ask_answer(
    'What is a peacock metaphorically?',
    '"jim is a peacock" is a metaphor meaning to show off',
     [peacock_metaphor_for_jim_unique]),

  nlu_ask_answer(
   "Does Jim show off?",
   'Yes.',[standard_metaphor_understood, jim_is_a_show_off]),
 
  nlu_ask_answer(
    'Why does Jim show off?',
    'Jim is a peacock.','"jim is a peacock" is a metaphor meaning to show off',
    [learned_standard_metaphor]),


nlu_assert(["Kiki is a peacock"]).
 nlu_ask_answer(
   "Is Kiki an animal?",
   'Kiki is a peacock,a peacock is an animal.',[kiki_is_literal_peacock]),

  nlu_ask_answer(
   "Does kiki show off?",
   'unknown',[kiki_is_not_a_show_off, standard_metaphor_not_common]),
   

['standard metaphor "peacock" is unique to subject. CasAm']]).



