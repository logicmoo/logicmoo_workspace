test_e2c([
  nlu_assert("Jim said 'Linda is sharp as a tack.'"),
  
  
 nlu_assert(
    ["A metaphor is a pre-existing phrase whose meaning is not literal and has a different fixed meaning.  
  The word choice of a particular metaphor and the meaning are sometimes related by an analogy or similarity.",
    "sharp as a tack is a metaphor",
    "sharp as a tack is a metaphor about being intelligent"]),

  nlu_ask_answer(
   "How many agents are here?",
   'At least one.', [infer_unique_objects]),

  nlu_ask_answer(
    'Who was Jim talking about?',
    'Linda.',
    [Linda]).

  nlu_ask_answer(
    'What did he say about Linda?',
    'she is sharp as a tack.',
    [false_positives]),


  nlu_ask_answer(
    'What does that mean?',
    'She is intelligent.',
     [it]).

  nlu_assert("Jim said something that meant linda is intelligent"),

  nlu_ask_answer(
   "What did Jim mean when he talked about Linda?",
   'Linda is intelligent.',[infer_unique_objects]),

 
  nlu_ask_answer(
    'What did Jim mean to say about Linda?',
    'She is intelligent.',
    [learned,it]),

  []]).
