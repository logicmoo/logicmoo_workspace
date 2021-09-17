% convo flow involving letter mistyped
%Hook vs look: He’s been looking for money.



test_convo_flo([
  convo_assert('There are many types of potential misunderstandings that
   can happen in a conversation and alter the usual flow of the conversation.
Misunderstandings can alter the usual flow of conversation, there are many 
  potential causes of misunderstandings.
One cause is when one agent mishears and/or misunderstands another agents
   timing or emphasis. Another cause of potential misunderstandings in 
   conversations can be a typo. A typo is when one letter or word is 
   accidentally substitued for another, especially in text.
    '),
  convo_assert(
    [p1'How is Jim lately?',
     p2'worried. He owes money to the mafia. He has been hooking for money but i 
     dont think he has enough to pay them',
     p1'i didnt see jim as the type to sell his body but i suppose desperate
     times call for desperate measures.'
     p2'what?',
      p2'oh! i meant looking, not hooking!', 
       p1'okay that makes sense.',]),

  convo_ask_answer(
   "'How many agents are here?',
   'At least two.', [infer_unique_objects]),

  convo_ask_answer(
    'what did p1 think p2 said?',
    'that jim had been hooking for money',
    [jim_hooking]).

  convo_ask_answer(
    'What did p2 mean?',
    'that jim had been looking for money',
    [p2_meaning]),

  convo_ask_answer(
    'so is jim hooking for money?',
    'no',
    [jim_not_hooking]),

   convo_ask_answer(
    'why did p2 say jim was hooking for money?',
    'a typo',
    [mistyped_substitution]),

 ])
),
['convo included typo "jim_hooking". CasAm']).


