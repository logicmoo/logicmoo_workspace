test_e2c([
  nlu_assert('Jim said "Linda is sharp as a tack."'),
  
  nlu_ask_answer(
   'How many agents are here?',
   'At least one.', [infer_unique_agents]),

  nlu_ask_answer(
    'Who was Jim talking about?',
    'Linda.',
    [who_jim_talks_about]).

  nlu_ask_answer(
    'What did he say about Linda?',
    'she is sharp as a tack.',
    [recall_jim_said]),


nlu_assert(
    ["A metaphor is an expression whose meaning is not literal and has a figurative meaning.",  
  "The expression of a metaphor and its figurative meaning are sometimes related by 
    an analogy or other similarity.",
    '"sharp as a tack" is a metaphor.',
    '"sharp as a tack" is a metaphor about/meaning being intelligent.']),


nlu_ask_answer(
    'What did Jim say about Linda?',
    'she is sharp as a tack.',
    [recall_jim_said_metaphor_added]),

  nlu_ask_answer(
    'What does that mean?',
    'She is intelligent.',
     [understood_metaphor]).

  % i want it to by itself: 
  % nlu_assert('Jim said a metaphor that meant linda is intelligent'), will it?

  nlu_ask_answer(
   "What did Jim mean when he talked about Linda?",
   'Linda is intelligent.',[metaphor_meaning_intention]),
 %  
  nlu_ask_answer(
    'What did Jim mean to say about Linda?',
    'She is intelligent.',
    [metaphor_meaning_intention2]),

['metaphor "sharp as a tack" used to infer intentioned statement, CasAm']]).
