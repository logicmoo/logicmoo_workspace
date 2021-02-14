
```prolog
baseKB:  ?- do_nars_example_tests.
% not_add_history(run_nars_example_tests).
```

```prolog
TEST: ?- revision([inheritance(bird,swimmer),[1,0.8]],[inheritance(bird,swimmer),[0,0.5]],_35114).
```
EXPECTED: `_35114=[inheritance(bird,swimmer),[0.8,0.83]]`
SUCCESS!
```prolog
% [ '='(
%      [ inheritance(bird,swimmer),
%        [0.8,0.8333333333333334] ],
%      [ inheritance(bird,swimmer),
%        [0.8,0.83] ]) ].
```


```prolog
TEST: ?- choice([inheritance(swan,bird),[1,0.8]],[inheritance(swan,bird),[0,0.5]],_39832).
```
EXPECTED: `_39832=[inheritance(swan,bird),[1,0.8]]`
SUCCESS!
```prolog
% [ '='(
%      [ inheritance(swan,bird),
%        [1,0.8] ],
%      [ inheritance(swan,bird),
%        [1,0.8] ]) ].
```


```prolog
TEST: ?- choice([inheritance(swan,bird),[1,0.5]],[inheritance(penguin,bird),[0.8,0.9]],_44306).
```
EXPECTED: `_44306=[inheritance(penguin,bird),[0.8,0.9]]`
SUCCESS!
```prolog
% [ '='(
%      [ inheritance(penguin,bird),
%        [0.8,0.9] ],
%      [ inheritance(penguin,bird),
%        [0.8,0.9] ]) ].
```


```prolog
TEST: ?- inference([inheritance(bird,animal),[1,0.9]],[inheritance(robin,bird),[1,0.9]],[inheritance(robin,animal),_48956]).
```
EXPECTED: `_48956=[1,0.81]`
SUCCESS!
```prolog
% [ '='(
%      [1,0.81],
%      [1,0.81]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,animal),[1,0.9]],[inheritance(robin,bird),[1,0.9]],[inheritance(bird,animal),_51670]).
```
EXPECTED: `_51670=[1,0.45]`
SUCCESS!
```prolog
% [ '='(
%      [1,0.44751381215469616],
%      [1,0.45]) ].
```


```prolog
TEST: ?- inference([inheritance(bird,animal),[1,0.9]],[inheritance(robin,animal),[1,0.9]],[inheritance(robin,bird),_54426]).
```
EXPECTED: `_54426=[1,0.45]`
SUCCESS!
```prolog
% [ '='(
%      [1,0.44751381215469616],
%      [1,0.45]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,bird),[1,0.9]],[inheritance(bird,animal),[1,0.9]],[inheritance(animal,robin),_57182]).
```
EXPECTED: `_57182=[1,0.45]`
SUCCESS!
```prolog
% [ '='(
%      [1,0.44751381215469616],
%      [1,0.45]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,bird),[0.9,0.8]],[inheritance(bird,swan),_59926]).
```
EXPECTED: `_59926=[1,0.42]`
SUCCESS!
```prolog
% [ '='(
%      [1,0.4186046511627907],
%      [1,0.42]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,robin),[0.9,0.8]],[inheritance(robin,swan),[0.9,0.8]],[similarity(swan,robin),_62674]).
```
EXPECTED: `_62674=[0.81,0.64]`
SUCCESS!
```prolog
% [ '='(
%      [0.81,0.6400000000000001],
%      [0.81,0.64]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,swimmer),[1,0.9]],[inheritance(swan,bird),[1,0.9]],[similarity(bird,swimmer),_65440]).
```
EXPECTED: `_65440=[1,0.45]`
SUCCESS!
```prolog
% [ '='(
%      [1,0.44751381215469616],
%      [1,0.45]) ].
```


```prolog
TEST: ?- inference([inheritance(sport,competition),[1,0.9]],[inheritance(chess,competition),[1,0.9]],[similarity(chess,sport),_68242]).
```
EXPECTED: `_68242=[1,0.45]`
SUCCESS!
```prolog
% [ '='(
%      [1,0.44751381215469616],
%      [1,0.45]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,swimmer),[1,0.9]],[similarity(gull,swan),[0.9,0.9]],[inheritance(gull,swimmer),_71050]).
```
EXPECTED: `_71050=[0.9,0.73]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7290000000000001],
%      [0.9,0.73]) ].
```


```prolog
TEST: ?- inference([inheritance(chess,competition),[1,0.9]],[similarity(sport,competition),[0.9,0.9]],[inheritance(chess,sport),_73842]).
```
EXPECTED: `_73842=[0.9,0.73]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7290000000000001],
%      [0.9,0.73]) ].
```


```prolog
TEST: ?- inference([similarity(swan,robin),[0.8,0.9]],[similarity(gull,swan),[0.9,0.8]],[similarity(gull,robin),_76640]).
```
EXPECTED: `_76640=[0.72,0.71]`
SUCCESS!
```prolog
% [ '='(
%      [0.7200000000000001,0.7056000000000001],
%      [0.72,0.71]) ].
```


```prolog
TEST: ?- inference([instance(tweety,bird),[1,0.9]],[inheritance(_79422,_79424),_79428]).
```
EXPECTED: `_79422=ext_set([tweety])`
EXPECTED: `_79424=bird`
EXPECTED: `_79428=[1,0.9]`
SUCCESS!
```prolog
% [ '='(
%      ext_set([tweety]),
%      ext_set([tweety])),
%   bird=bird,
%   '='(
%      [1,0.9],
%      [1,0.9]) ].
```


```prolog
TEST: ?- inference([property(raven,black),[1,0.9]],[inheritance(_83980,_83982),_83986]).
```
EXPECTED: `_83980=raven`
EXPECTED: `_83982=int_set([black])`
EXPECTED: `_83986=[1,0.9]`
SUCCESS!
```prolog
% [ raven=raven,
%   '='(
%      int_set([black]),
%      int_set([black])),
%   '='(
%      [1,0.9],
%      [1,0.9]) ].
```


```prolog
TEST: ?- inference([inst_prop(tweety,yellow),[1,0.9]],[inheritance(_88538,_88540),_88544]).
```
EXPECTED: `_88538=ext_set([tweety])`
EXPECTED: `_88540=int_set([yellow])`
EXPECTED: `_88544=[1,0.9]`
SUCCESS!
```prolog
% [ '='(
%      ext_set([tweety]),
%      ext_set([tweety])),
%   '='(
%      int_set([yellow]),
%      int_set([yellow])),
%   '='(
%      [1,0.9],
%      [1,0.9]) ].
```


```prolog
TEST: ?- inference([inheritance(ext_set([tweety]),ext_set([birdie])),[1,0.8]],[similarity(_94612,_94614),_94618]).
```
EXPECTED: `_94612=ext_set([tweety])`
EXPECTED: `_94614=ext_set([birdie])`
EXPECTED: `_94618=[1,0.8]`
SUCCESS!
```prolog
% [ '='(
%      ext_set([tweety]),
%      ext_set([tweety])),
%   '='(
%      ext_set([birdie]),
%      ext_set([birdie])),
%   '='(
%      [1,0.8],
%      [1,0.8]) ].
```


```prolog
TEST: ?- inference([inheritance(int_set([smart]),int_set([bright])),[1,0.8]],[similarity(_100676,_100678),_100682]).
```
EXPECTED: `_100676=int_set([smart])`
EXPECTED: `_100678=int_set([bright])`
EXPECTED: `_100682=[1,0.8]`
SUCCESS!
```prolog
% [ '='(
%      int_set([smart]),
%      int_set([smart])),
%   '='(
%      int_set([bright]),
%      int_set([bright])),
%   '='(
%      [1,0.8],
%      [1,0.8]) ].
```


```prolog
TEST: ?- inference([similarity(ext_set([tweety]),ext_set([birdie])),[1,0.9]],[similarity(tweety,birdie),_106746]).
```
EXPECTED: `_106746=[1,0.9]`
SUCCESS!
```prolog
% [ '='(
%      [1,0.9],
%      [1,0.9]) ].
```


```prolog
TEST: ?- inference([similarity(int_set([smart]),int_set([bright])),[0.8,0.9]],[similarity(smart,bright),_109370]).
```
EXPECTED: `_109370=[0.8,0.9]`
SUCCESS!
```prolog
% [ '='(
%      [0.8,0.9],
%      [0.8,0.9]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,swimmer),[0.9,0.9]],[inheritance(swan,bird),[0.8,0.9]],_111936).
```
EXPECTED: `_111936=[inheritance(swan,ext_intersection([swimmer,bird])),[0.72,0.81]];_111936=[inheritance(swan,int_intersection([swimmer,bird])),[0.98,0.81]];_111936=[inheritance(swan,ext_difference(swimmer,bird)),[0.18,0.81]]`
SUCCESS!
```prolog
% [ ('='(
%       [ inheritance(swan,
%            ext_intersection([swimmer,bird])),
%         [0.7200000000000001,0.81] ],
%       [ inheritance(swan,
%            ext_intersection([swimmer,bird])),
%         [0.72,0.81] ]) ;
%     '='(
%        [ inheritance(swan,
%             ext_intersection([swimmer,bird])),
%          [0.7200000000000001,0.81] ],
%        [ inheritance(swan,
%             int_intersection([swimmer,bird])),
%          [0.98,0.81] ]) ;
%    '='(
%       [ inheritance(swan,
%            ext_intersection([swimmer,bird])),
%         [0.7200000000000001,0.81] ],
%       [ inheritance(swan,
%            ext_difference(swimmer,bird)),
%         [0.18,0.81] ])) ].
```


```prolog
TEST: ?- inference([inheritance(sport,competition),[0.9,0.9]],[inheritance(chess,competition),[0.8,0.9]],_21082).
```
EXPECTED: `_21082=[inheritance(int_intersection([sport,chess]),competition),[0.72,0.81]];_21082=[inheritance(ext_intersection([sport,chess]),competition),[0.98,0.81]];_21082=[inheritance(int_difference(sport,chess),competition),[0.18,0.81]]`
SUCCESS!
```prolog
% [ ('='(
%       [ inheritance(
%            int_intersection([sport,chess]),
%            competition),
%         [0.7200000000000001,0.81] ],
%       [ inheritance(
%            int_intersection([sport,chess]),
%            competition),
%         [0.72,0.81] ]) ;
%     '='(
%        [ inheritance(
%             int_intersection([sport,chess]),
%             competition),
%          [0.7200000000000001,0.81] ],
%        [ inheritance(
%             ext_intersection([sport,chess]),
%             competition),
%          [0.98,0.81] ]) ;
%    '='(
%       [ inheritance(
%            int_intersection([sport,chess]),
%            competition),
%         [0.7200000000000001,0.81] ],
%       [ inheritance(
%            int_difference(sport,chess),
%            competition),
%         [0.18,0.81] ])) ].
```


```prolog
TEST: ?- inference([inheritance(swan,swimmer),[0.9,0.8]],[inheritance(swan,ext_intersection([swimmer,bird])),_37460]).
```
EXPECTED: `_37460=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,swimmer),[0.9,0.8]],[inheritance(swan,int_intersection([swimmer,bird])),_40262]).
```
EXPECTED: `_40262=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,swimmer),[0.9,0.8]],[inheritance(swan,ext_difference(swimmer,bird)),_43060]).
```
EXPECTED: `_43060=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,swimmer),[0.9,0.8]],[negation(inheritance(swan,ext_difference(bird,swimmer))),_45850]).
```
EXPECTED: `_45850=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(sport,competition),[0.9,0.8]],[inheritance(int_intersection([sport,chess]),competition),_48652]).
```
EXPECTED: `_48652=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(sport,competition),[0.9,0.8]],[inheritance(ext_intersection([sport,chess]),competition),_51454]).
```
EXPECTED: `_51454=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(sport,competition),[0.9,0.8]],[inheritance(int_difference(sport,chess),competition),_54252]).
```
EXPECTED: `_54252=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(sport,competition),[0.9,0.8]],[negation(inheritance(int_difference(chess,sport),competition)),_57042]).
```
EXPECTED: `_57042=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,bird),[1,0.8]],[inheritance(swan,ext_intersection([swimmer,bird])),[0,0.8]],[inheritance(swan,swimmer),_59876]).
```
EXPECTED: `_59876=[0,0.64]`
SUCCESS!
```prolog
% [ '='(
%      [0,0.6400000000000001],
%      [0,0.64]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,bird),[0,0.8]],[inheritance(swan,int_intersection([swimmer,bird])),[1,0.8]],[inheritance(swan,swimmer),_62676]).
```
EXPECTED: `_62676=[1,0.64]`
SUCCESS!
```prolog
% [ '='(
%      [1,0.6400000000000001],
%      [1,0.64]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,swimmer),[1,0.8]],[inheritance(swan,ext_difference(swimmer,bird)),[0,0.8]],[inheritance(swan,bird),_65458]).
```
EXPECTED: `_65458=[1,0.64]`
SUCCESS!
```prolog
% [ '='(
%      [1,0.6400000000000001],
%      [1,0.64]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,bird),[0,0.8]],[inheritance(swan,ext_difference(swimmer,bird)),[0,0.8]],[inheritance(swan,swimmer),_68228]).
```
EXPECTED: `_68228=[0,0.64]`
SUCCESS!
```prolog
% [ '='(
%      [0,0.6400000000000001],
%      [0,0.64]) ].
```


```prolog
TEST: ?- inference([inheritance(sport,competition),[1,0.8]],[inheritance(int_intersection([sport,chess]),competition),[0,0.8]],[inheritance(chess,competition),_71024]).
```
EXPECTED: `_71024=[0,0.64]`
SUCCESS!
```prolog
% [ '='(
%      [0,0.6400000000000001],
%      [0,0.64]) ].
```


```prolog
TEST: ?- inference([inheritance(sport,competition),[0,0.8]],[inheritance(ext_intersection([sport,chess]),competition),[1,0.8]],[inheritance(chess,competition),_73824]).
```
EXPECTED: `_73824=[1,0.64]`
SUCCESS!
```prolog
% [ '='(
%      [1,0.6400000000000001],
%      [1,0.64]) ].
```


```prolog
TEST: ?- inference([inheritance(sport,competition),[1,0.8]],[inheritance(int_difference(sport,chess),competition),[0,0.8]],[inheritance(chess,competition),_76606]).
```
EXPECTED: `_76606=[1,0.64]`
SUCCESS!
```prolog
% [ '='(
%      [1,0.6400000000000001],
%      [1,0.64]) ].
```


```prolog
TEST: ?- inference([inheritance(chess,competition),[0,0.8]],[inheritance(int_difference(sport,chess),competition),[0,0.8]],[inheritance(sport,competition),_79376]).
```
EXPECTED: `_79376=[0,0.64]`
SUCCESS!
```prolog
% [ '='(
%      [0,0.6400000000000001],
%      [0,0.64]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,ext_intersection([swimmer,bird])),[0.9,0.8]],[inheritance(swan,swimmer),_82140]).
```
EXPECTED: `_82140=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,int_intersection([swimmer,bird])),[0.9,0.8]],[inheritance(swan,swimmer),_84948]).
```
EXPECTED: `_84948=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,ext_difference(swimmer,bird)),[0.9,0.8]],[inheritance(swan,swimmer),_87740]).
```
EXPECTED: `_87740=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,ext_difference(swimmer,bird)),[0.9,0.8]],[negation(inheritance(swan,bird)),_90536]).
```
EXPECTED: `_90536=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(int_intersection([sport,chess]),competition),[0.9,0.8]],[inheritance(sport,competition),_93338]).
```
EXPECTED: `_93338=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(ext_intersection([sport,chess]),competition),[0.9,0.8]],[inheritance(sport,competition),_96146]).
```
EXPECTED: `_96146=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(int_difference(sport,chess),competition),[0.9,0.8]],[inheritance(sport,competition),_98938]).
```
EXPECTED: `_98938=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(int_difference(sport,chess),competition),[0.9,0.8]],[negation(inheritance(chess,competition)),_101734]).
```
EXPECTED: `_101734=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(bird,animal),[0.9,0.8]],[inheritance(ext_intersection([swimmer,bird]),ext_intersection([swimmer,animal])),_104552]).
```
EXPECTED: `_104552=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(ext_intersection([swimmer,bird]),ext_intersection([swimmer,animal])),[0.9,0.8]],[inheritance(bird,animal),_107406]).
```
EXPECTED: `_107406=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(bird,animal),[0.9,0.8]],[inheritance(int_intersection([swimmer,bird]),int_intersection([swimmer,animal])),_110254]).
```
EXPECTED: `_110254=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(int_intersection([swimmer,bird]),int_intersection([swimmer,animal])),[0.9,0.8]],[inheritance(bird,animal),_113108]).
```
EXPECTED: `_113108=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([similarity(bird,animal),[0.9,0.8]],[similarity(ext_intersection([swimmer,bird]),ext_intersection([swimmer,animal])),_115956]).
```
EXPECTED: `_115956=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([similarity(ext_intersection([swimmer,bird]),ext_intersection([swimmer,animal])),[0.9,0.8]],[similarity(bird,animal),_118810]).
```
EXPECTED: `_118810=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([similarity(bird,animal),[0.9,0.8]],[similarity(int_intersection([swimmer,bird]),int_intersection([swimmer,animal])),_121658]).
```
EXPECTED: `_121658=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([similarity(int_intersection([swimmer,bird]),int_intersection([swimmer,animal])),[0.9,0.8]],[similarity(bird,animal),_124512]).
```
EXPECTED: `_124512=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(bird,animal),[0.9,0.8]],[inheritance(ext_difference(bird,swimmer),ext_difference(animal,swimmer)),_127340]).
```
EXPECTED: `_127340=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(ext_difference(bird,swimmer),ext_difference(animal,swimmer)),[0.9,0.8]],[inheritance(bird,animal),_130138]).
```
EXPECTED: `_130138=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(bird,animal),[0.9,0.8]],[inheritance(int_difference(bird,swimmer),int_difference(animal,swimmer)),_21514]).
```
EXPECTED: `_21514=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(int_difference(bird,swimmer),int_difference(animal,swimmer)),[0.9,0.8]],[inheritance(bird,animal),_24312]).
```
EXPECTED: `_24312=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([similarity(bird,animal),[0.9,0.8]],[similarity(ext_difference(bird,swimmer),ext_difference(animal,swimmer)),_27104]).
```
EXPECTED: `_27104=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([similarity(ext_difference(bird,swimmer),ext_difference(animal,swimmer)),[0.9,0.8]],[similarity(bird,animal),_29902]).
```
EXPECTED: `_29902=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([similarity(bird,animal),[0.9,0.8]],[similarity(int_difference(bird,swimmer),int_difference(animal,swimmer)),_32694]).
```
EXPECTED: `_32694=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([similarity(int_difference(bird,swimmer),int_difference(animal,swimmer)),[0.9,0.8]],[similarity(bird,animal),_35492]).
```
EXPECTED: `_35492=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(bird,animal),[0.9,0.8]],[inheritance(ext_difference(swimmer,animal),ext_difference(swimmer,bird)),_38284]).
```
EXPECTED: `_38284=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(ext_difference(swimmer,animal),ext_difference(swimmer,bird)),[0.9,0.8]],[inheritance(bird,animal),_41082]).
```
EXPECTED: `_41082=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(bird,animal),[0.9,0.8]],[inheritance(int_difference(swimmer,animal),int_difference(swimmer,bird)),_43874]).
```
EXPECTED: `_43874=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(int_difference(swimmer,animal),int_difference(swimmer,bird)),[0.9,0.8]],[inheritance(bird,animal),_46672]).
```
EXPECTED: `_46672=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([similarity(bird,animal),[0.9,0.8]],[similarity(ext_difference(swimmer,animal),ext_difference(swimmer,bird)),_49464]).
```
EXPECTED: `_49464=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([similarity(ext_difference(swimmer,animal),ext_difference(swimmer,bird)),[0.9,0.8]],[similarity(bird,animal),_52262]).
```
EXPECTED: `_52262=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([similarity(bird,animal),[0.9,0.8]],[similarity(int_difference(swimmer,animal),int_difference(swimmer,bird)),_55054]).
```
EXPECTED: `_55054=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([similarity(int_difference(swimmer,animal),int_difference(swimmer,bird)),[0.9,0.8]],[similarity(bird,animal),_57852]).
```
EXPECTED: `_57852=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(ext_set([earth]),ext_set([venus,mars,pluto])),[0.9,0.8]],[inheritance(ext_set([earth]),ext_set([pluto,saturn])),[0.7,0.8]],_60576).
```
EXPECTED: `_60576=[inheritance(ext_set([earth]),ext_set([pluto])),[0.63,0.64]];_60576=[inheritance(ext_set([earth]),ext_set([venus,mars,pluto,saturn])),[0.97,0.64]];_60576=[inheritance(ext_set([earth]),ext_set([venus,mars])),[0.27,0.64]]`
SUCCESS!
```prolog
% [ ('='(
%       [ inheritance(
%            ext_set([earth]),
%            ext_set([pluto])),
%         [0.63,0.6400000000000001] ],
%       [ inheritance(
%            ext_set([earth]),
%            ext_set([pluto])),
%         [0.63,0.64] ]) ;
%     '='(
%        [ inheritance(
%             ext_set([earth]),
%             ext_set([pluto])),
%          [0.63,0.6400000000000001] ],
%        [ inheritance(
%             ext_set([earth]),
%             ext_set([venus,mars,pluto,saturn])),
%          [0.97,0.64] ]) ;
%    '='(
%       [ inheritance(
%            ext_set([earth]),
%            ext_set([pluto])),
%         [0.63,0.6400000000000001] ],
%       [ inheritance(
%            ext_set([earth]),
%            ext_set([venus,mars])),
%         [0.27,0.64] ])) ].
```


```prolog
TEST: ?- inference([inheritance(int_set([red,green,blue]),int_set([colorful])),[0.9,0.8]],[inheritance(int_set([purple,green]),int_set([colorful])),[0.7,0.8]],_82204).
```
EXPECTED: `_82204=[inheritance(int_set([green]),int_set([colorful])),[0.63,0.64]];_82204=[inheritance(int_set([red,blue,purple,green]),int_set([colorful])),[0.97,0.64]];_82204=[inheritance(int_set([red,blue]),int_set([colorful])),[0.271,0.64]]`
SUCCESS!
```prolog
% [ ('='(
%       [ inheritance(
%            int_set([green]),
%            int_set([colorful])),
%         [0.63,0.6400000000000001] ],
%       [ inheritance(
%            int_set([green]),
%            int_set([colorful])),
%         [0.63,0.64] ]) ;
%     '='(
%        [ inheritance(
%             int_set([green]),
%             int_set([colorful])),
%          [0.63,0.6400000000000001] ],
%        [ inheritance(
%             int_set([red,blue,purple,green]),
%             int_set([colorful])),
%          [0.97,0.64] ]) ;
%    '='(
%       [ inheritance(
%            int_set([green]),
%            int_set([colorful])),
%         [0.63,0.6400000000000001] ],
%       [ inheritance(
%            int_set([red,blue]),
%            int_set([colorful])),
%         [0.271,0.64] ])) ].
```


```prolog
TEST: ?- inference([inheritance(product([acid,base]),reaction),[1,0.9]],_103830).
```
EXPECTED: `_103830=[inheritance(acid,ext_image(reaction,[nil,base])),[1,0.9]];_103830=[inheritance(base,ext_image(reaction,[acid,nil])),[1,0.9]]`
SUCCESS!
```prolog
% [ ('='(
%       [ inheritance(acid,
%            ext_image(reaction,
%               [nil,base])),
%         [1,0.9] ],
%       [ inheritance(acid,
%            ext_image(reaction,
%               [nil,base])),
%         [1,0.9] ]) ;
%    '='(
%       [ inheritance(acid,
%            ext_image(reaction,
%               [nil,base])),
%         [1,0.9] ],
%       [ inheritance(base,
%            ext_image(reaction,
%               [acid,nil])),
%         [1,0.9] ])) ].
```


```prolog
TEST: ?- inference([inheritance(acid,ext_image(reaction,[nil,base])),[1,0.9]],_115526).
```
EXPECTED: `_115526=[inheritance(product([acid,base]),reaction),[1,0.9]]`
SUCCESS!
```prolog
% [ '='(
%      [ inheritance(
%           product([acid,base]),
%           reaction),
%        [1,0.9] ],
%      [ inheritance(
%           product([acid,base]),
%           reaction),
%        [1,0.9] ]) ].
```


```prolog
TEST: ?- inference([inheritance(acid,ext_image(reaction,[acid,nil])),[1,0.9]],_121678).
```
EXPECTED: `_121678=[inheritance(product([acid,acid]),reaction),[1,0.9]]`
SUCCESS!
```prolog
% [ '='(
%      [ inheritance(
%           product([acid,acid]),
%           reaction),
%        [1,0.9] ],
%      [ inheritance(
%           product([acid,acid]),
%           reaction),
%        [1,0.9] ]) ].
```


```prolog
TEST: ?- inference([inheritance(neutralization,product([acid,base])),[1,0.9]],_127836).
```
EXPECTED: `_127836=[inheritance(int_image(neutralization,[nil,base]),acid),[1,0.9]];_127836=[inheritance(int_image(neutralization,[acid,nil]),base),[1,0.9]]`
SUCCESS!
```prolog
% [ ('='(
%       [ inheritance(
%            int_image(neutralization,
%               [nil,base]),
%            acid),
%         [1,0.9] ],
%       [ inheritance(
%            int_image(neutralization,
%               [nil,base]),
%            acid),
%         [1,0.9] ]) ;
%    '='(
%       [ inheritance(
%            int_image(neutralization,
%               [nil,base]),
%            acid),
%         [1,0.9] ],
%       [ inheritance(
%            int_image(neutralization,
%               [acid,nil]),
%            base),
%         [1,0.9] ])) ].
```


```prolog
TEST: ?- inference([inheritance(int_image(neutralization,[nil,base]),acid),[1,0.9]],_28480).
```
EXPECTED: `_28480=[inheritance(neutralization,product([acid,base])),[1,0.9]]`
SUCCESS!
```prolog
% [ '='(
%      [ inheritance(neutralization,
%           product([acid,base])),
%        [1,0.9] ],
%      [ inheritance(neutralization,
%           product([acid,base])),
%        [1,0.9] ]) ].
```


```prolog
TEST: ?- inference([inheritance(int_image(neutralization,[acid,nil]),base),[1,0.9]],_34632).
```
EXPECTED: `_34632=[inheritance(neutralization,product([acid,base])),[1,0.9]]`
SUCCESS!
```prolog
% [ '='(
%      [ inheritance(neutralization,
%           product([acid,base])),
%        [1,0.9] ],
%      [ inheritance(neutralization,
%           product([acid,base])),
%        [1,0.9] ]) ].
```


```prolog
TEST: ?- inference([inheritance(bird,animal),[0.9,0.8]],[inheritance(product([bird,plant]),product([animal,plant])),_40880]).
```
EXPECTED: `_40880=[0.9,0.8]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.8],
%      [0.9,0.8]) ].
```


```prolog
TEST: ?- inference([inheritance(product([plant,bird]),product([plant,animal])),[0.9,0.8]],[inheritance(bird,animal),_43534]).
```
EXPECTED: `_43534=[0.9,0.8]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.8],
%      [0.9,0.8]) ].
```


```prolog
TEST: ?- inference([inheritance(neutralization,reaction),[0.9,0.8]],[inheritance(ext_image(neutralization,[acid,nil]),ext_image(reaction,[acid,nil])),_46212]).
```
EXPECTED: `_46212=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(ext_image(neutralization,[acid,nil]),ext_image(reaction,[acid,nil])),[0.9,0.8]],[inheritance(neutralization,reaction),_49034]).
```
EXPECTED: `_49034=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(neutralization,reaction),[0.9,0.8]],[inheritance(int_image(neutralization,[acid,nil]),int_image(reaction,[acid,nil])),_51850]).
```
EXPECTED: `_51850=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(int_image(neutralization,[acid,nil]),int_image(reaction,[acid,nil])),[0.9,0.8]],[inheritance(neutralization,reaction),_54672]).
```
EXPECTED: `_54672=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(soda,base),[0.9,0.8]],[inheritance(ext_image(reaction,[nil,base]),ext_image(reaction,[nil,soda])),_57488]).
```
EXPECTED: `_57488=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(ext_image(reaction,[nil,base]),ext_image(reaction,[nil,soda])),[0.9,0.8]],[inheritance(soda,base),_60330]).
```
EXPECTED: `_60330=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(soda,base),[0.9,0.8]],[inheritance(int_image(neutralization,[nil,base]),int_image(neutralization,[nil,soda])),_63166]).
```
EXPECTED: `_63166=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(int_image(neutralization,[nil,base]),int_image(neutralization,[nil,soda])),[0.9,0.8]],[inheritance(soda,base),_66008]).
```
EXPECTED: `_66008=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- revision([implication(inheritance(robin,flyer),inheritance(robin,bird)),[1,0.8]],[implication(inheritance(robin,flyer),inheritance(robin,bird)),[0,0.5]],_68752).
```
EXPECTED: `_68752=[implication(inheritance(robin,flyer),inheritance(robin,bird)),[0.8,0.83]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           inheritance(robin,flyer),
%           inheritance(robin,bird)),
%        [0.8,0.8333333333333334] ],
%      [ implication(
%           inheritance(robin,flyer),
%           inheritance(robin,bird)),
%        [0.8,0.83] ]) ].
```


```prolog
TEST: ?- revision([equivalence(inheritance(robin,flyer),inheritance(robin,bird)),[1,0.8]],[equivalence(inheritance(robin,flyer),inheritance(robin,bird)),[0,0.5]],_74450).
```
EXPECTED: `_74450=[equivalence(inheritance(robin,flyer),inheritance(robin,bird)),[0.8,0.83]]`
SUCCESS!
```prolog
% [ '='(
%      [ equivalence(
%           inheritance(robin,flyer),
%           inheritance(robin,bird)),
%        [0.8,0.8333333333333334] ],
%      [ equivalence(
%           inheritance(robin,flyer),
%           inheritance(robin,bird)),
%        [0.8,0.83] ]) ].
```


```prolog
TEST: ?- choice([implication(inheritance(robin,flyer),inheritance(robin,bird)),[1,0.8]],[implication(inheritance(robin,flyer),inheritance(robin,bird)),[0,0.5]],_80148).
```
EXPECTED: `_80148=[implication(inheritance(robin,flyer),inheritance(robin,bird)),[1,0.8]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           inheritance(robin,flyer),
%           inheritance(robin,bird)),
%        [1,0.8] ],
%      [ implication(
%           inheritance(robin,flyer),
%           inheritance(robin,bird)),
%        [1,0.8] ]) ].
```


```prolog
TEST: ?- choice([implication(inheritance(robin,flyer),inheritance(robin,bird)),[0.8,0.9]],[implication(inheritance(robin,swimmer),inheritance(robin,bird)),[1,0.5]],_85530).
```
EXPECTED: `_85530=[implication(inheritance(robin,flyer),inheritance(robin,bird)),[0.8,0.9]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           inheritance(robin,flyer),
%           inheritance(robin,bird)),
%        [0.8,0.9] ],
%      [ implication(
%           inheritance(robin,flyer),
%           inheritance(robin,bird)),
%        [0.8,0.9] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[implication(inheritance(robin,flyer),inheritance(robin,bird)),[1,0.5]],_91000).
```
EXPECTED: `_91000=[implication(inheritance(robin,flyer),inheritance(robin,animal)),[0.9,0.36]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           inheritance(robin,flyer),
%           inheritance(robin,animal)),
%        [0.9,0.36000000000000004] ],
%      [ implication(
%           inheritance(robin,flyer),
%           inheritance(robin,animal)),
%        [0.9,0.36] ]) ].
```


```prolog
TEST: ?- inference([equivalence(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[equivalence(inheritance(robin,flyer),inheritance(robin,bird)),[1,0.5]],_96642).
```
EXPECTED: `_96642=[equivalence(inheritance(robin,flyer),inheritance(robin,animal)),[0.9,0.4]]`
SUCCESS!
```prolog
% [ '='(
%      [ equivalence(
%           inheritance(robin,flyer),
%           inheritance(robin,animal)),
%        [0.9,0.39999999999999997] ],
%      [ equivalence(
%           inheritance(robin,flyer),
%           inheritance(robin,animal)),
%        [0.9,0.4] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[implication(inheritance(robin,bird),inheritance(robin,flyer)),[1,0.5]],_102324).
```
EXPECTED: `_102324=[implication(inheritance(robin,flyer),inheritance(robin,animal)),[0.9,0.29]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           inheritance(robin,flyer),
%           inheritance(robin,animal)),
%        [0.9,0.28571428571428575] ],
%      [ implication(
%           inheritance(robin,flyer),
%           inheritance(robin,animal)),
%        [0.9,0.29] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[implication(inheritance(robin,flyer),inheritance(robin,animal)),[1,0.5]],_107960).
```
EXPECTED: `_107960=[implication(inheritance(robin,flyer),inheritance(robin,bird)),[1,0.26]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           inheritance(robin,flyer),
%           inheritance(robin,bird)),
%        [1,0.2647058823529412] ],
%      [ implication(
%           inheritance(robin,flyer),
%           inheritance(robin,bird)),
%        [1,0.26] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,flyer),inheritance(robin,bird)),[0.9,0.8]],[implication(inheritance(robin,bird),inheritance(robin,animal)),[1,0.5]],_113572).
```
EXPECTED: `_113572=[implication(inheritance(robin,animal),inheritance(robin,flyer)),[1,0.26]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           inheritance(robin,animal),
%           inheritance(robin,flyer)),
%        [1,0.2647058823529412] ],
%      [ implication(
%           inheritance(robin,animal),
%           inheritance(robin,flyer)),
%        [1,0.26] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,flyer),inheritance(robin,animal)),[0.9,0.8]],_119202).
```
EXPECTED: `_119202=[implication(inheritance(robin,animal),inheritance(robin,flyer)),[1,0.42]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           inheritance(robin,animal),
%           inheritance(robin,flyer)),
%        [1,0.4186046511627907] ],
%      [ implication(
%           inheritance(robin,animal),
%           inheritance(robin,flyer)),
%        [1,0.42] ]) ].
```


```prolog
TEST: ?- inference([equivalence(inheritance(robin,flyer),inheritance(robin,bird)),[0.9,0.8]],[implication(inheritance(robin,flyer),inheritance(robin,bird)),_124826]).
```
EXPECTED: `_124826=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,flyer),inheritance(robin,bird)),[0.9,0.8]],[implication(inheritance(robin,bird),inheritance(robin,flyer)),[0.9,0.8]],_127556).
```
EXPECTED: `_127556=[equivalence(inheritance(robin,flyer),inheritance(robin,bird)),[0.81,0.64]]`
SUCCESS!
```prolog
% [ '='(
%      [ equivalence(
%           inheritance(robin,flyer),
%           inheritance(robin,bird)),
%        [0.81,0.6400000000000001] ],
%      [ equivalence(
%           inheritance(robin,flyer),
%           inheritance(robin,bird)),
%        [0.81,0.64] ]) ].
```


```prolog
TEST: ?- inference([similarity(swan,bird),[0.9,0.8]],[inheritance(swan,bird),_22054]).
```
EXPECTED: `_22054=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,bird),[0.9,0.8]],[similarity(swan,bird),_24840]).
```
EXPECTED: `_24840=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,bird),[1,0.8]],[inheritance(bird,swan),[0.1,0.8]],_27564).
```
EXPECTED: `_27564=[similarity(swan,bird),[0.1,0.64]]`
SUCCESS!
```prolog
% [ '='(
%      [ similarity(swan,bird),
%        [0.1,0.6400000000000001] ],
%      [ similarity(swan,bird),
%        [0.1,0.64] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[implication(inheritance(robin,bird),inheritance(robin,flyer)),[0.9,0.8]],[equivalence(_32324,_32326),_32330]).
```
EXPECTED: `_32324=inheritance(robin,flyer)`
EXPECTED: `_32326=inheritance(robin,animal)`
EXPECTED: `_32330=[0.82,0.39]`
SUCCESS!
```prolog
% [ '='(
%      inheritance(robin,flyer),
%      inheritance(robin,flyer)),
%   '='(
%      inheritance(robin,animal),
%      inheritance(robin,animal)),
%   '='(
%      [0.8181818181818182,0.3878550440744369],
%      [0.82,0.39]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[implication(inheritance(robin,flyer),inheritance(robin,animal)),[0.9,0.8]],[equivalence(_36346,_36348),_36352]).
```
EXPECTED: `_36346=inheritance(robin,flyer)`
EXPECTED: `_36348=inheritance(robin,bird)`
EXPECTED: `_36352=[0.82,0.39]`
SUCCESS!
```prolog
% [ '='(
%      inheritance(robin,flyer),
%      inheritance(robin,flyer)),
%   '='(
%      inheritance(robin,bird),
%      inheritance(robin,bird)),
%   '='(
%      [0.8181818181818182,0.3878550440744369],
%      [0.82,0.39]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[equivalence(inheritance(robin,flyer),inheritance(robin,animal)),[0.9,0.8]],_40250).
```
EXPECTED: `_40250=[implication(inheritance(robin,bird),inheritance(robin,flyer)),[0.81,0.58]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           inheritance(robin,bird),
%           inheritance(robin,flyer)),
%        [0.81,0.5760000000000001] ],
%      [ implication(
%           inheritance(robin,bird),
%           inheritance(robin,flyer)),
%        [0.81,0.58] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[equivalence(inheritance(robin,flyer),inheritance(robin,bird)),[0.9,0.8]],_45898).
```
EXPECTED: `_45898=[implication(inheritance(robin,flyer),inheritance(robin,animal)),[0.81,0.58]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           inheritance(robin,flyer),
%           inheritance(robin,animal)),
%        [0.81,0.5760000000000001] ],
%      [ implication(
%           inheritance(robin,flyer),
%           inheritance(robin,animal)),
%        [0.81,0.58] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[implication(inheritance(robin,bird),inheritance(robin,flyer)),[0.9,0.8]],_51546).
```
EXPECTED: `_51546=[implication(inheritance(robin,bird),conjunction([inheritance(robin,animal),inheritance(robin,flyer)])),[0.81,0.64]];_51546=[implication(inheritance(robin,bird),disjunction([inheritance(robin,animal),inheritance(robin,flyer)])),[0.99,0.64]]`
SUCCESS!
```prolog
% [ ('='(
%       [ implication(
%            inheritance(robin,bird),
%            conjunction([ inheritance(robin,animal),
%                          inheritance(robin,flyer) ])),
%         [0.81,0.6400000000000001] ],
%       [ implication(
%            inheritance(robin,bird),
%            conjunction([ inheritance(robin,animal),
%                          inheritance(robin,flyer) ])),
%         [0.81,0.64] ]) ;
%    '='(
%       [ implication(
%            inheritance(robin,bird),
%            conjunction([ inheritance(robin,animal),
%                          inheritance(robin,flyer) ])),
%         [0.81,0.6400000000000001] ],
%       [ implication(
%            inheritance(robin,bird),
%            disjunction([ inheritance(robin,animal),
%                          inheritance(robin,flyer) ])),
%         [0.99,0.64] ])) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[implication(inheritance(robin,flyer),inheritance(robin,animal)),[0.9,0.8]],_66388).
```
EXPECTED: `_66388=[implication(disjunction([inheritance(robin,bird),inheritance(robin,flyer)]),inheritance(robin,animal)),[0.81,0.64]];_66388=[implication(conjunction([inheritance(robin,bird),inheritance(robin,flyer)]),inheritance(robin,animal)),[0.99,0.64]]`
SUCCESS!
```prolog
% [ ('='(
%       [ implication(
%            disjunction([ inheritance(robin,bird),
%                          inheritance(robin,flyer) ]),
%            inheritance(robin,animal)),
%         [0.81,0.6400000000000001] ],
%       [ implication(
%            disjunction([ inheritance(robin,bird),
%                          inheritance(robin,flyer) ]),
%            inheritance(robin,animal)),
%         [0.81,0.64] ]) ;
%    '='(
%       [ implication(
%            disjunction([ inheritance(robin,bird),
%                          inheritance(robin,flyer) ]),
%            inheritance(robin,animal)),
%         [0.81,0.6400000000000001] ],
%       [ implication(
%            conjunction([ inheritance(robin,bird),
%                          inheritance(robin,flyer) ]),
%            inheritance(robin,animal)),
%         [0.99,0.64] ])) ].
```


```prolog
TEST: ?- inference([inheritance(robin,animal),[0.9,0.9]],[inheritance(robin,flyer),[0.9,0.9]],[conjunction([inheritance(robin,animal),inheritance(robin,flyer)]),_81352]).
```
EXPECTED: `_81352=[0.81,0.81]`
SUCCESS!
```prolog
% [ '='(
%      [0.81,0.81],
%      [0.81,0.81]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,animal),[0.9,0.8]],[inheritance(robin,flyer),[0.9,0.8]],[disjunction([inheritance(robin,animal),inheritance(robin,flyer)]),_84122]).
```
EXPECTED: `_84122=[0.99,0.64]`
SUCCESS!
```prolog
% [ '='(
%      [0.99,0.6400000000000001],
%      [0.99,0.64]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[implication(inheritance(robin,bird),conjunction([inheritance(robin,animal),inheritance(robin,flyer)])),_86932]).
```
EXPECTED: `_86932=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[implication(inheritance(robin,bird),disjunction([inheritance(robin,animal),inheritance(robin,flyer)])),_89758]).
```
EXPECTED: `_89758=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[implication(disjunction([inheritance(robin,bird),inheritance(robin,flyer)]),inheritance(robin,animal)),_92590]).
```
EXPECTED: `_92590=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[implication(conjunction([inheritance(robin,bird),inheritance(robin,flyer)]),inheritance(robin,animal)),_95416]).
```
EXPECTED: `_95416=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,animal),[0.9,0.8]],[conjunction([inheritance(robin,animal),inheritance(robin,flyer)]),_98224]).
```
EXPECTED: `_98224=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,animal),[0.9,0.8]],[disjunction([inheritance(robin,animal),inheritance(robin,flyer)]),_101026]).
```
EXPECTED: `_101026=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,flyer)),[1,0.8]],[implication(inheritance(robin,bird),conjunction([inheritance(robin,animal),inheritance(robin,flyer)])),[0,0.8]],[implication(inheritance(robin,bird),inheritance(robin,animal)),_103902]).
```
EXPECTED: `_103902=[0,0.64]`
SUCCESS!
```prolog
% [ '='(
%      [0,0.6400000000000001],
%      [0,0.64]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,flyer)),[0,0.8]],[implication(inheritance(robin,bird),disjunction([inheritance(robin,animal),inheritance(robin,flyer)])),[1,0.8]],[implication(inheritance(robin,bird),inheritance(robin,animal)),_106744]).
```
EXPECTED: `_106744=[1,0.64]`
SUCCESS!
```prolog
% [ '='(
%      [1,0.6400000000000001],
%      [1,0.64]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[1,0.8]],[implication(disjunction([inheritance(robin,bird),inheritance(robin,flyer)]),inheritance(robin,animal)),[0,0.8]],[implication(inheritance(robin,flyer),inheritance(robin,animal)),_109578]).
```
EXPECTED: `_109578=[0,0.64]`
SUCCESS!
```prolog
% [ '='(
%      [0,0.6400000000000001],
%      [0,0.64]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0,0.8]],[implication(conjunction([inheritance(robin,bird),inheritance(robin,flyer)]),inheritance(robin,animal)),[1,0.8]],[implication(inheritance(robin,flyer),inheritance(robin,animal)),_112420]).
```
EXPECTED: `_112420=[1,0.64]`
SUCCESS!
```prolog
% [ '='(
%      [1,0.6400000000000001],
%      [1,0.64]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,bird),[1,0.8]],[conjunction([inheritance(robin,bird),inheritance(robin,flyer)]),[0,0.8]],_115108).
```
EXPECTED: `_115108=[inheritance(robin,flyer),[0,0.64]]`
SUCCESS!
```prolog
% [ '='(
%      [ inheritance(robin,flyer),
%        [0,0.6400000000000001] ],
%      [ inheritance(robin,flyer),
%        [0,0.64] ]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,bird),[0,0.8]],[disjunction([inheritance(robin,bird),inheritance(robin,flyer)]),[1,0.8]],_119778).
```
EXPECTED: `_119778=[inheritance(robin,flyer),[1,0.64]]`
SUCCESS!
```prolog
% [ '='(
%      [ inheritance(robin,flyer),
%        [1,0.6400000000000001] ],
%      [ inheritance(robin,flyer),
%        [1,0.64] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),conjunction([inheritance(robin,animal),inheritance(robin,flyer)])),[0.9,0.8]],[implication(inheritance(robin,bird),inheritance(robin,animal)),_124542]).
```
EXPECTED: `_124542=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),disjunction([inheritance(robin,animal),inheritance(robin,flyer)])),[0.9,0.8]],[implication(inheritance(robin,bird),inheritance(robin,animal)),_127374]).
```
EXPECTED: `_127374=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([implication(disjunction([inheritance(robin,bird),inheritance(robin,flyer)]),inheritance(robin,animal)),[0.9,0.8]],[implication(inheritance(robin,bird),inheritance(robin,animal)),_130200]).
```
EXPECTED: `_130200=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([inheritance(robin,bird),inheritance(robin,flyer)]),inheritance(robin,animal)),[0.9,0.8]],[implication(inheritance(robin,bird),inheritance(robin,animal)),_21566]).
```
EXPECTED: `_21566=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([conjunction([inheritance(robin,bird),inheritance(robin,flyer)]),[0.9,0.8]],[inheritance(robin,bird),_24368]).
```
EXPECTED: `_24368=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([disjunction([inheritance(robin,bird),inheritance(robin,flyer)]),[0.9,0.8]],[inheritance(robin,bird),_27176]).
```
EXPECTED: `_27176=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([implication(p,q),[0.9,0.8]],[implication(conjunction([p,r]),conjunction([q,r])),_29988]).
```
EXPECTED: `_29988=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([p,r]),conjunction([q,r])),[0.9,0.8]],[implication(p,q),_32836]).
```
EXPECTED: `_32836=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([implication(p,q),[0.9,0.8]],[implication(disjunction([p,r]),disjunction([q,r])),_35678]).
```
EXPECTED: `_35678=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([implication(disjunction([p,r]),disjunction([q,r])),[0.9,0.8]],[implication(p,q),_38526]).
```
EXPECTED: `_38526=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([equivalence(p,q),[0.9,0.8]],[equivalence(conjunction([p,r]),conjunction([q,r])),_41368]).
```
EXPECTED: `_41368=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([equivalence(conjunction([p,r]),conjunction([q,r])),[0.9,0.8]],[equivalence(p,q),_44216]).
```
EXPECTED: `_44216=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([equivalence(p,q),[0.9,0.8]],[equivalence(disjunction([p,r]),disjunction([q,r])),_47058]).
```
EXPECTED: `_47058=[0.9,0.72]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([equivalence(disjunction([p,r]),disjunction([q,r])),[0.9,0.8]],[equivalence(p,q),_49906]).
```
EXPECTED: `_49906=[0.9,0.44]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([negation(inheritance(robin,bird)),[0.9,0.8]],_52658).
```
EXPECTED: `_52658=[inheritance(robin,bird),[0.1,0.8]]`
SUCCESS!
```prolog
% [ '='(
%      [ inheritance(robin,bird),
%        [0.09999999999999998,0.8] ],
%      [ inheritance(robin,bird),
%        [0.1,0.8] ]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,bird),[0.2,0.8]],[negation(inheritance(robin,bird)),_57282]).
```
EXPECTED: `_57282=[0.8,0.8]`
SUCCESS!
```prolog
% [ '='(
%      [0.8,0.8],
%      [0.8,0.8]) ].
```


```prolog
TEST: ?- inference([implication(negation(inheritance(penguin,flyer)),inheritance(penguin,swimmer)),[0.1,0.8]],[implication(negation(inheritance(penguin,swimmer)),inheritance(penguin,flyer)),_59978]).
```
EXPECTED: `_59978=[0,0.42]`
SUCCESS!
```prolog
% [ '='(
%      [0,0.4186046511627907],
%      [0,0.42]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[inheritance(robin,bird),[1,0.5]],_62640).
```
EXPECTED: `_62640=[inheritance(robin,animal),[0.9,0.36]]`
SUCCESS!
```prolog
% [ '='(
%      [ inheritance(robin,animal),
%        [0.9,0.36000000000000004] ],
%      [ inheritance(robin,animal),
%        [0.9,0.36] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[inheritance(robin,animal),[1,0.5]],_67308).
```
EXPECTED: `_67308=[inheritance(robin,bird),[1,0.26]]`
SUCCESS!
```prolog
% [ '='(
%      [ inheritance(robin,bird),
%        [1,0.2647058823529412] ],
%      [ inheritance(robin,bird),
%        [1,0.26] ]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,animal),[0.9,0.8]],[inheritance(robin,flyer),[1,0.5]],[implication(inheritance(robin,flyer),inheritance(robin,animal)),_72052]).
```
EXPECTED: `_72052=[0.9,0.29]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.28571428571428575],
%      [0.9,0.29]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,animal),[1,0.5]],[equivalence(inheritance(robin,flyer),inheritance(robin,animal)),[0.9,0.8]],_74750).
```
EXPECTED: `_74750=[inheritance(robin,flyer),[0.9,0.36]]`
SUCCESS!
```prolog
% [ '='(
%      [ inheritance(robin,flyer),
%        [0.9,0.36000000000000004] ],
%      [ inheritance(robin,flyer),
%        [0.9,0.36] ]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,animal),[0.9,0.8]],[inheritance(robin,flyer),[1,0.5]],[equivalence(inheritance(robin,flyer),inheritance(robin,animal)),_79524]).
```
EXPECTED: `_79524=[0.9,0.29]`
SUCCESS!
```prolog
% [ '='(
%      [0.9000000000000001,0.2857142857142857],
%      [0.9,0.29]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([a1,a2,a3]),c),[0.9,0.9]],[a2,[0.9,0.9]],_82280).
```
EXPECTED: `_82280=[implication(conjunction([a1,a3]),c),[0.81,0.66]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           conjunction([a1,a3]),
%           c),
%        [0.81,0.6561000000000001] ],
%      [ implication(
%           conjunction([a1,a3]),
%           c),
%        [0.81,0.66] ]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([a1,a2,a3]),c),[0.9,0.9]],[implication(conjunction([a1,a3]),c),[0.9,0.9]],[a2,_88836]).
```
EXPECTED: `_88836=[0.9,0.42]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.42163100057836905],
%      [0.9,0.42]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([a1,a3]),c),[0.9,0.9]],[a2,[0.9,0.9]],[implication(conjunction([a2,a1,a3]),c),_91684]).
```
EXPECTED: `_91684=[0.9,0.42]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.42163100057836905],
%      [0.9,0.42]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([a1,a2,a3]),c),[0.9,0.9]],[implication(b2,a2),[0.9,0.9]],_94392).
```
EXPECTED: `_94392=[implication(conjunction([a1,b2,a3]),c),[0.81,0.66]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           conjunction([a1,b2,a3]),
%           c),
%        [0.81,0.6561000000000001] ],
%      [ implication(
%           conjunction([a1,b2,a3]),
%           c),
%        [0.81,0.66] ]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([a1,a2,a3]),c),[0.9,0.9]],[implication(conjunction([a1,b2,a3]),c),[0.9,0.9]],[implication(b2,a2),_101082]).
```
EXPECTED: `_101082=[0.9,0.42]`
SUCCESS!
```prolog
% [ '='(
%      [0.9,0.42163100057836905],
%      [0.9,0.42]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([a1,b2,a3]),c),[0.9,0.9]],[implication(b2,a2),[0.9,0.9]],_103774).
```
EXPECTED: `_103774=[implication(conjunction([a1,a2,a3]),c),[0.9,0.42]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           conjunction([a1,a2,a3]),
%           c),
%        [0.9,0.42163100057836905] ],
%      [ implication(
%           conjunction([a1,a2,a3]),
%           c),
%        [0.9,0.42] ]) ].
```


```prolog
TEST: ?- revision([implication(inheritance(_110330,bird),inheritance(_110330,flyer)),[0.9,0.8]],[implication(inheritance(_110384,bird),inheritance(_110384,flyer)),[1,0.5]],_110314).
```
EXPECTED: `_110314=[implication(inheritance(_110384,bird),inheritance(_110384,flyer)),[0.92,0.83]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           inheritance(_118330,bird),
%           inheritance(_118330,flyer)),
%        [0.9200000000000002,0.8333333333333334] ],
%      [ implication(
%           inheritance(_118330,bird),
%           inheritance(_118330,flyer)),
%        [0.92,0.83] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(_118754,bird),inheritance(_118754,animal)),[1,0.9]],[implication(inheritance(_118802,robin),inheritance(_118802,bird)),[1,0.9]],_118738).
```
EXPECTED: `_118738=[implication(inheritance(_118802,robin),inheritance(_118802,animal)),[1,0.81]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           inheritance(_126506,robin),
%           inheritance(_126506,animal)),
%        [1,0.81] ],
%      [ implication(
%           inheritance(_126506,robin),
%           inheritance(_126506,animal)),
%        [1,0.81] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(_126930,bird),inheritance(_126930,animal)),[1,0.9]],[implication(inheritance(_126978,robin),inheritance(_126978,animal)),[1,0.9]],_126914).
```
EXPECTED: `_126914=[implication(inheritance(_126978,robin),inheritance(_126978,bird)),[1,0.45]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           inheritance(_23716,robin),
%           inheritance(_23716,bird)),
%        [1,0.44751381215469616] ],
%      [ implication(
%           inheritance(_23716,robin),
%           inheritance(_23716,bird)),
%        [1,0.45] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(_24140,robin),inheritance(_24140,animal)),[1,0.9]],[implication(inheritance(_24188,robin),inheritance(_24188,bird)),[1,0.9]],_24124).
```
EXPECTED: `_24124=[implication(inheritance(_24188,bird),inheritance(_24188,animal)),[1,0.45]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           inheritance(_32042,bird),
%           inheritance(_32042,animal)),
%        [1,0.44751381215469616] ],
%      [ implication(
%           inheritance(_32042,bird),
%           inheritance(_32042,animal)),
%        [1,0.45] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(_32466,feathered),inheritance(_32466,bird)),[1,0.9]],[equivalence(inheritance(_32514,flyer),inheritance(_32514,bird)),[1,0.9]],_32450).
```
EXPECTED: `_32450=[implication(inheritance(_32514,feathered),inheritance(_32514,flyer)),[1,0.81]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           inheritance(_40218,feathered),
%           inheritance(_40218,flyer)),
%        [1,0.81] ],
%      [ implication(
%           inheritance(_40218,feathered),
%           inheritance(_40218,flyer)),
%        [1,0.81] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(_40642,feathered),inheritance(_40642,flyer)),[1,0.9]],[implication(inheritance(_40690,feathered),inheritance(_40690,bird)),[1,0.9]],_40626).
```
EXPECTED: `_40626=[implication(inheritance(_40690,bird),inheritance(_40690,flyer)),[1,0.45]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           inheritance(_48544,bird),
%           inheritance(_48544,flyer)),
%        [1,0.44751381215469616] ],
%      [ implication(
%           inheritance(_48544,bird),
%           inheritance(_48544,flyer)),
%        [1,0.45] ]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([inheritance(_48978,feathered),inheritance(_48978,flyer)]),inheritance(_48978,bird)),[1,0.9]],[implication(inheritance(_49038,swimmer),inheritance(_49038,feathered)),[1,0.9]],_48952).
```
EXPECTED: `_48952=[implication(conjunction([inheritance(_49038,swimmer),inheritance(_49038,flyer)]),inheritance(_49038,bird)),[1,0.81]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           conjunction([ inheritance(_60402,swimmer),
%                         inheritance(_60402,flyer) ]),
%           inheritance(_60402,bird)),
%        [1,0.81] ],
%      [ implication(
%           conjunction([ inheritance(_60402,swimmer),
%                         inheritance(_60402,flyer) ]),
%           inheritance(_60402,bird)),
%        [1,0.81] ]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([inheritance(_60934,feathered),inheritance(_60934,flyer)]),inheritance(_60934,bird)),[1,0.9]],[implication(conjunction([inheritance(_60934,swimmer),inheritance(_60934,flyer)]),inheritance(_60934,bird)),[1,0.9]],_60908).
```
EXPECTED: `_60908=[implication(inheritance(_60934,swimmer),inheritance(_60934,feathered)),[1,0.45]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           inheritance(_68870,swimmer),
%           inheritance(_68870,feathered)),
%        [1,0.44751381215469616] ],
%      [ implication(
%           inheritance(_68870,swimmer),
%           inheritance(_68870,feathered)),
%        [1,0.45] ]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([inheritance(_69304,swimmer),inheritance(_69304,flyer)]),inheritance(_69304,bird)),[1,0.9]],[implication(inheritance(_69364,swimmer),inheritance(_69364,feathered)),[1,0.9]],_69278).
```
EXPECTED: `_69278=[implication(conjunction([inheritance(_69364,feathered),inheritance(_69364,flyer)]),inheritance(_69364,bird)),[1,0.45]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           conjunction([ inheritance(_80938,feathered),
%                         inheritance(_80938,flyer) ]),
%           inheritance(_80938,bird)),
%        [1,0.44751381215469616] ],
%      [ implication(
%           conjunction([ inheritance(_80938,feathered),
%                         inheritance(_80938,flyer) ]),
%           inheritance(_80938,bird)),
%        [1,0.45] ]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([inheritance(_81470,feathered),inheritance(_81470,flyer)]),inheritance(_81470,bird)),[1,0.9]],[implication(inheritance(_81530,swimmer),inheritance(_81530,feathered)),[1,0.9]],_81444).
```
EXPECTED: `_81444=[implication(conjunction([inheritance(_81530,swimmer),inheritance(_81530,flyer)]),inheritance(_81530,bird)),[1,0.81]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           conjunction([ inheritance(_92894,swimmer),
%                         inheritance(_92894,flyer) ]),
%           inheritance(_92894,bird)),
%        [1,0.81] ],
%      [ implication(
%           conjunction([ inheritance(_92894,swimmer),
%                         inheritance(_92894,flyer) ]),
%           inheritance(_92894,bird)),
%        [1,0.81] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(_93416,bird),inheritance(_93416,animal)),[1,0.9]],[inheritance(robin,bird),[1,0.9]],_93400).
```
EXPECTED: `_93400=[inheritance(robin,animal),[1,0.81]]`
SUCCESS!
```prolog
% [ '='(
%      [ inheritance(robin,animal),
%        [1,0.81] ],
%      [ inheritance(robin,animal),
%        [1,0.81] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(_97970,bird),inheritance(_97970,animal)),[1,0.9]],[inheritance(robin,animal),[1,0.9]],_97954).
```
EXPECTED: `_97954=[inheritance(robin,bird),[1,0.45]]`
SUCCESS!
```prolog
% [ '='(
%      [ inheritance(robin,bird),
%        [1,0.44751381215469616] ],
%      [ inheritance(robin,bird),
%        [1,0.45] ]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,animal),[1,0.9]],[equivalence(inheritance(_102638,bird),inheritance(_102638,animal)),[1,0.9]],_102586).
```
EXPECTED: `_102586=[inheritance(robin,bird),[1,0.81]]`
SUCCESS!
```prolog
% [ '='(
%      [ inheritance(robin,bird),
%        [1,0.81] ],
%      [ inheritance(robin,bird),
%        [1,0.81] ]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([inheritance(_107166,feathered),inheritance(_107166,flyer)]),inheritance(_107166,bird)),[1,0.9]],[inheritance(swan,feathered),[1,0.9]],_107140).
```
EXPECTED: `_107140=[implication(inheritance(swan,flyer),inheritance(swan,bird)),[1,0.81]]`
SUCCESS!
```prolog
% [ '='(
%      [ implication(
%           inheritance(swan,flyer),
%           inheritance(swan,bird)),
%        [1,0.81] ],
%      [ implication(
%           inheritance(swan,flyer),
%           inheritance(swan,bird)),
%        [1,0.81] ]) ].
```


```prolog
TEST: ?- inference([conjunction([inheritance(var(_112650,[]),bird),inheritance(var(_112650,[]),swimmer)]),[1,0.9]],[inheritance(swan,bird),[1,0.9]],[inheritance(swan,swimmer),_112746]).
```
EXPECTED: `_112746=[1,0.42]`
SUCCESS!
```prolog
% [ '='(
%      [1,0.42631578947368426],
%      [1,0.42]) ].
```


```prolog
TEST: ?- inference([conjunction([inheritance(var(_115550,[]),flyer),inheritance(var(_115550,[]),bird),inheritance(var(_115550,[]),swimmer)]),[1,0.9]],[inheritance(swan,bird),[1,0.9]],_115524).
```
EXPECTED: `_115524=[conjunction([inheritance(swan,flyer),inheritance(swan,swimmer)]),[1,0.42]]`
SUCCESS!
```prolog
% [ '='(
%      [ conjunction([ inheritance(swan,flyer),
%                      inheritance(swan,swimmer) ]),
%        [1,0.42631578947368426] ],
%      [ conjunction([ inheritance(swan,flyer),
%                      inheritance(swan,swimmer) ]),
%        [1,0.42] ]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,animal),[1,0.9]],[inheritance(robin,bird),[1,0.9]],_122718).
```
EXPECTED: `_122718=[implication(inheritance(_122824,bird),inheritance(_122824,animal)),[1,0.45]];_122718=[equivalence(inheritance(_122824,bird),inheritance(_122824,animal)),[1,0.45]];_122718=[conjunction([inheritance(var(_122948,[]),bird),inheritance(var(_122948,[]),animal)]),[1,0.81]]`
FAILED!
```prolog
% [ ('='(Term_List,
%       [ implication(
%            inheritance(_30500,bird),
%            inheritance(_30500,animal)),
%         [1,0.45] ]) ;
%     '='(Term_List,
%        [ equivalence(
%             inheritance(_30500,bird),
%             inheritance(_30500,animal)),
%          [1,0.45] ]) ;
%    '='(Term_List,
%       [ conjunction([ inheritance(
%                          var(_30576,[]),
%                          bird),
%                       inheritance(
%                          var(_30576,[]),
%                          animal) ]),
%         [1,0.81] ])) ].
```


```prolog
TEST: ?- inference([inheritance(sport,competition),[1,0.9]],[inheritance(chess,competition),[1,0.9]],_31096).
```
EXPECTED: `_31096=[implication(inheritance(sport,_31204),inheritance(chess,_31204)),[1,0.45]];_31096=[equivalence(inheritance(sport,_31204),inheritance(chess,_31204)),[1,0.45]];_31096=[conjunction([inheritance(chess,var(_31326,[])),inheritance(sport,var(_31326,[]))]),[1,0.81]]`
FAILED!
```prolog
% [ ('='(Term_List,
%       [ implication(
%            inheritance(sport,Inheritance),
%            inheritance(chess,Inheritance)),
%         [1,0.45] ]) ;
%     '='(Term_List,
%        [ equivalence(
%             inheritance(sport,Inheritance),
%             inheritance(chess,Inheritance)),
%          [1,0.45] ]) ;
%    '='(Term_List,
%       [ conjunction([ inheritance(chess,
%                          var(_50916,[])),
%                       inheritance(sport,
%                          var(_50916,[])) ]),
%         [1,0.81] ])) ].
```


```prolog
TEST: ?- inference([inheritance(key1,ext_image(open,[nil,lock1])),[1,0.9]],[inheritance(key1,key),[1,0.9]],_51452).
```
EXPECTED: `_51452=[implication(inheritance(_51576,key),inheritance(_51576,ext_image(open,[nil,lock1]))),[1,0.45]];_51452=[conjunction([inheritance(var(_51658,[]),key),inheritance(var(_51658,[]),ext_image(open,[nil,lock1]))]),[1,0.81]]`
FAILED!
```prolog
% [ ('='(Term_List,
%       [ implication(
%            inheritance(_67132,key),
%            inheritance(_67132,
%               ext_image(open,
%                  [nil,lock1]))),
%         [1,0.45] ]) ;
%    '='(Term_List,
%       [ conjunction([ inheritance(
%                          var(_67172,[]),
%                          key),
%                       inheritance(
%                          var(_67172,[]),
%                          ext_image(open,
%                             [nil,lock1])) ]),
%         [1,0.81] ])) ].
```


```prolog
TEST: ?- inference([implication(inheritance(_67708,key),inheritance(lock1,ext_image(open,[_67708,nil]))),[1,0.9]],[inheritance(lock1,lock),[1,0.9]],_67692).
```
EXPECTED: `_67692=[implication(conjunction([inheritance(_67708,key),inheritance(_67850,lock)]),inheritance(_67850,ext_image(open,[_67708,nil]))),[1,0.45]];_67692=[conjunction([implication(inheritance(_67708,key),inheritance(var(_67850,[]),ext_image(open,[_67708,nil]))),inheritance(var(_67850,[]),lock)]),[1,0.81]]`
FAILED!
```prolog
% [ ('='(Term_List,
%       [ implication(
%            conjunction([ inheritance(_86756,key),
%                          inheritance(_86768,lock) ]),
%            inheritance(_86768,
%               ext_image(open,
%                  [_86756,nil]))),
%         [1,0.45] ]) ;
%    '='(Term_List,
%       [ conjunction([ implication(
%                          inheritance(CAR,key),
%                          inheritance(
%                             var(_86768,[]),
%                             ext_image(open,
%                                [CAR,nil]))),
%                       inheritance(
%                          var(_86768,[]),
%                          lock) ]),
%         [1,0.81] ])) ].
```


```prolog
TEST: ?- inference([conjunction([inheritance(var(_87422,[]),key),inheritance(lock1,ext_image(open,[var(_87422,[]),nil]))]),[1,0.9]],[inheritance(lock1,lock),[1,0.9]],_87396).
```
EXPECTED: `_87396=[implication(inheritance(_87554,lock),conjunction([inheritance(_87554,ext_image(open,[var(_87422,[_87554]),nil])),inheritance(var(_87422,[_87554]),key)])),[1,0.45]];_87396=[conjunction([inheritance(var(_87682,[]),lock),inheritance(var(_87682,[]),ext_image(open,[var(_87422,[]),nil])),inheritance(var(_87422,[]),key)]),[1,0.81]]`
FAILED!
```prolog
% [ ('='(Term_List,
%       [ implication(
%            inheritance(_109914,lock),
%            conjunction([ inheritance(CAR,
%                             ext_image(open,
%                                [ var(_109948,
%                                     [CAR]),
%                                  nil ])),
%                          inheritance(
%                             var(_109948,
%                                [CAR]),
%                             key) ])),
%         [1,0.45] ]) ;
%    '='(Term_List,
%       [ conjunction([ inheritance(
%                          var(_110012,[]),
%                          lock),
%                       inheritance(
%                          var(_110012,[]),
%                          ext_image(open,
%                             [ var(_109948,[]),
%                               nil ])),
%                       inheritance(
%                          var(_109948,[]),
%                          key) ]),
%         [1,0.81] ])) ].
```

true.

baseKB:  ?-

