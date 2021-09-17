  %% okay. so we will need a non-standard hyperbole like these:
% “it took forever” “she could kill with a look”)
test_e2c([

 nlu_assert(['some sentances include hyperboles']),

  nlu_assert(
           [' a hyperbole is an exaggerated statement or claim not meant to be taken 
              literally.',
           'hyperboles exaggerate extremes to call attention
            to the attribute they exaggerate'                
            ]),
nlu_assert(['the man said he would have to stand in line for several million years
 to keep his drivers liscense. ',
          'The human lifespan does not currently exceed 300 years']).
       nlu_ask_answer(
           'how long was the man in the line?',
           'i dont know.', [not_literal_million years]),
       nlu_ask_answer(
            'how long did the man live?',
            'i dont know', [sanity_check_lifespan]),
       nlu_ask_answer(
           'how long was the man in the line?',
           'i dont know.', [not_literal_million years]),
 nlu_ask_answer(
    'was hyperbole used?',
    'yes/probably',
     [hyperbole_yn]),
     nlu_ask_answer(
        'what was the man expressing by using hyperbole?',
        'that it would take a long time',
         [hyperbole_meaning_long_time]),
       ])
),
['non-standard hyperbole. CasAm']).
