test_e2c([

  nlu_assert(['some sentances include hyperboles']),

  nlu_assert(
           [' a hyperbole is an exaggerated statement or claim not meant to be taken literally.',
           'hyperboles exaggerate to extremes to call attention
            to the attribute they exaggerate'                
            ]),
nlu_assert(['jim is helping linda move into her new apartment, as they carry the boxes 
from the truck to the apartment jim complains to linda "these boxes weigh a ton!"',
'moving boxes are usually made of corrugated cardboard.',
'cardboard boxes are rated to carry up to 300 lbs safely.',
           ]),
nlu_assert( 'most humans cannot lift more than 50lbs safely',  
 'Andy Bolton set the heaviest lift record when he lifted 
     457.5 kilograms from the floor to his thigh.', 
         'one imperial ton is 1016.047 kilograms.'
           ]),

  nlu_ask_answer(
   'do the boxes weigh exactly ton?',
   'no', [not_exactly_one_ton]),

  nlu_ask_answer(
   'do we know the weight of the boxes?',
   'no', [not_literal_weight]),


  nlu_ask_answer(
   'does each box weigh 1 ton?',
   'no', [each_box_exactly_one_ton]),

 nlu_ask_answer(
    'What was jim calling attention to when he complained?',
    'the weight of the boxes',
     [jim_complained_why]),

 nlu_ask_answer(
    'would jim be able to lift a box weighing a ton?',
    'No',
    [jim_not_superman]),
  ])
),
['hyperbole emphasizes which feature. CasAm']).



