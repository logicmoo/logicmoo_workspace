    %  Synecdoche

test_e2c([
     nlu_assert(
    ['The "National Football League" is often shortened to the "NFL".',
    'The Super Bowl is the annual championship game of the National Football League.',
    'It has served as the final game of every NFL season since 1966, 
            replacing the NFL Championship Game. 
    Since 2004, the game has been played on the first Sunday in February.'
    ]),

  nlu_assert('This year the Texans won the Super Bowl.'),
  nlu_assert('29 million people live in Texas.'),
  nlu_ask_answer(
   'Who won the super bowl?',
   'The Texans.', [superbowl])

  nlu_ask_answer(
    'How many people won the Super Bowl?',
    '29 million',
    [expected_failure_synecdoche]),

  nlu_assert('A metaphor is an expression whose meaning is not literal.',
             'Synecdoche is a kind of metaphor in which a part of something is used 
         to signify the whole, or the whole of something is used to signify a part.'),


nlu_assert('Members of NFL are players on teams which compete to play in the Super Bowl.'),
nlu_assert('NFL Teams are from geographical locations and each has a mascot.'),
nlu_assert('All NFL teams were required to have exactly 53 players.'),
%this is not a real fact, dont let any of this get out in the public knowledge. 

 nlu_ask_answer(
   'Who won the super bowl this year?',
   'The Texans', [who_won_superbowl]),

 nlu_ask_answer(
    'How many teams do we know won the Super Bowl this year?',
    '1 team',
    [teams_synecdoche]),

 nlu_ask_answer(
    'How many people do we know won the Super Bowl?',
    '53 players',
    [success_synecdoche]),
 

% now we go backward - use a part to signify whole concept

  nlu_assert(
    ['The Vince Lombardi Trophy is the trophy awarded each year to the winning team
     of the National Football League championship game, the Super Bowl.
      The trophy is named in honor of NFL coach Vince Lombardi,
       who led the Green Bay Packers to victories in the first two Super Bowl games.', ]),

  nlu_ask_answer(
   'Who got the Vince Lombardi trophy this year?',
   'The Texans',[texas_got_trophy]),

  nlu_ask_answer(
    'Why did they get the Vince Lombardi trophy?',
    'They won the Super Bowl.',
    [vince_trophy]).

 nlu_assert('Last year Chicago took home the vince lombardi trophy.'),

  nlu_ask_answer(
    'who won the super bowl last year?',
    'the Chicago team',
    [synecdoche_learned]),

['double synecdoche vince_trophy. CasAm']]).


