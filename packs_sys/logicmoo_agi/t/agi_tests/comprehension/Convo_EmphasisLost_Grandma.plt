% convo flo misinterp spaces, emphasis,
%Let's eat grandma. 

test_convo_flo([
  convo_assert('There are many types of potential misunderstandings that
   can happen in a conversation and alter the usual flow of the conversation.
   One is when one agent mishears or misunderstands another agents
   timing or emphasis. 
    '),
  convo_assert(
    [p1'It look like the buffet is running out of food.',
     p2'lets go eat, grandma',
     p1'no. we cant eat grandma, i love her.'
     ,]),
 
  convo_ask_answer(
    'what did p1 think p2 said?',
    'they should eat grandma',
    [p2_heard_eat_grandma]).

  convo_ask_answer(
    'What did p1 mean',
    'that p1 and grandma should go eat',
    [p1_said_lets_eat]),

   ])
 ),
 ['convo lost emphasis or punctuation "lets_eat_grandma". CasAm']).


