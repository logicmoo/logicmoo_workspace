`baseKB:  ?- do_nars_example_tests.`
`% not_add_history(run_nars_example_tests).`

```prolog
TEST: ?- revision([inheritance(bird,swimmer),[1,0.8]],[inheritance(bird,swimmer),[0,0.5]],_119084).
```
EXPECTED: `_119084=[inheritance(bird,swimmer),[0.8,0.83]]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [ inheritance(bird,swimmer),
%        [0.8,0.8333333333333334] ],
%      [ inheritance(bird,swimmer),
%        [0.8,0.83] ]) ].
```


```prolog
TEST: ?- choice([inheritance(swan,bird),[1,0.8]],[inheritance(swan,bird),[0,0.5]],_123796).
```
EXPECTED: `_123796=[inheritance(swan,bird),[1,0.8]]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [ inheritance(swan,bird),
%        [1,0.8] ],
%      [ inheritance(swan,bird),
%        [1,0.8] ]) ].
```


```prolog
TEST: ?- choice([inheritance(swan,bird),[1,0.5]],[inheritance(penguin,bird),[0.8,0.9]],_128270).
```
EXPECTED: `_128270=[inheritance(penguin,bird),[0.8,0.9]]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [ inheritance(penguin,bird),
%        [0.8,0.9] ],
%      [ inheritance(penguin,bird),
%        [0.8,0.9] ]) ].
```


```prolog
TEST: ?- inference([inheritance(bird,animal),[1,0.9]],[inheritance(robin,bird),[1,0.9]],[inheritance(robin,animal),_21256]).
```
EXPECTED: `_21256=[1,0.81]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [1,0.81],
%      [1,0.81]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,animal),[1,0.9]],[inheritance(robin,bird),[1,0.9]],[inheritance(bird,animal),_23970]).
```
EXPECTED: `_23970=[1,0.45]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [1,0.44751381215469616],
%      [1,0.45]) ].
```


```prolog
TEST: ?- inference([inheritance(bird,animal),[1,0.9]],[inheritance(robin,animal),[1,0.9]],[inheritance(robin,bird),_26726]).
```
EXPECTED: `_26726=[1,0.45]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [1,0.44751381215469616],
%      [1,0.45]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,bird),[1,0.9]],[inheritance(bird,animal),[1,0.9]],[inheritance(animal,robin),_29482]).
```
EXPECTED: `_29482=[1,0.45]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [1,0.44751381215469616],
%      [1,0.45]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,bird),[0.9,0.8]],[inheritance(bird,swan),_32226]).
```
EXPECTED: `_32226=[1,0.42]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [1,0.4186046511627907],
%      [1,0.42]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,robin),[0.9,0.8]],[inheritance(robin,swan),[0.9,0.8]],[similarity(swan,robin),_34974]).
```
EXPECTED: `_34974=[0.81,0.64]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.81,0.6400000000000001],
%      [0.81,0.64]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,swimmer),[1,0.9]],[inheritance(swan,bird),[1,0.9]],[similarity(bird,swimmer),_37740]).
```
EXPECTED: `_37740=[1,0.45]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [1,0.44751381215469616],
%      [1,0.45]) ].
```


```prolog
TEST: ?- inference([inheritance(sport,competition),[1,0.9]],[inheritance(chess,competition),[1,0.9]],[similarity(chess,sport),_40542]).
```
EXPECTED: `_40542=[1,0.45]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [1,0.44751381215469616],
%      [1,0.45]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,swimmer),[1,0.9]],[similarity(gull,swan),[0.9,0.9]],[inheritance(gull,swimmer),_43350]).
```
EXPECTED: `_43350=[0.9,0.73]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7290000000000001],
%      [0.9,0.73]) ].
```


```prolog
TEST: ?- inference([inheritance(chess,competition),[1,0.9]],[similarity(sport,competition),[0.9,0.9]],[inheritance(chess,sport),_46142]).
```
EXPECTED: `_46142=[0.9,0.73]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7290000000000001],
%      [0.9,0.73]) ].
```


```prolog
TEST: ?- inference([similarity(swan,robin),[0.8,0.9]],[similarity(gull,swan),[0.9,0.8]],[similarity(gull,robin),_48940]).
```
EXPECTED: `_48940=[0.72,0.71]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.7200000000000001,0.7056000000000001],
%      [0.72,0.71]) ].
```


```prolog
TEST: ?- inference([instance(tweety,bird),[1,0.9]],[inheritance(_51722,_51724),_51728]).
```
EXPECTED: `_51722=ext_set([tweety])`
EXPECTED: `_51724=bird`
EXPECTED: `_51728=[1,0.9]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([property(raven,black),[1,0.9]],[inheritance(_56280,_56282),_56286]).
```
EXPECTED: `_56280=raven`
EXPECTED: `_56282=int_set([black])`
EXPECTED: `_56286=[1,0.9]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([inst_prop(tweety,yellow),[1,0.9]],[inheritance(_60838,_60840),_60844]).
```
EXPECTED: `_60838=ext_set([tweety])`
EXPECTED: `_60840=int_set([yellow])`
EXPECTED: `_60844=[1,0.9]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([inheritance(ext_set([tweety]),ext_set([birdie])),[1,0.8]],[similarity(_66912,_66914),_66918]).
```
EXPECTED: `_66912=ext_set([tweety])`
EXPECTED: `_66914=ext_set([birdie])`
EXPECTED: `_66918=[1,0.8]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([inheritance(int_set([smart]),int_set([bright])),[1,0.8]],[similarity(_72976,_72978),_72982]).
```
EXPECTED: `_72976=int_set([smart])`
EXPECTED: `_72978=int_set([bright])`
EXPECTED: `_72982=[1,0.8]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([similarity(ext_set([tweety]),ext_set([birdie])),[1,0.9]],[similarity(tweety,birdie),_79046]).
```
EXPECTED: `_79046=[1,0.9]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [1,0.9],
%      [1,0.9]) ].
```


```prolog
TEST: ?- inference([similarity(int_set([smart]),int_set([bright])),[0.8,0.9]],[similarity(smart,bright),_81670]).
```
EXPECTED: `_81670=[0.8,0.9]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.8,0.9],
%      [0.8,0.9]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,swimmer),[0.9,0.9]],[inheritance(swan,bird),[0.8,0.9]],_84236).
```
EXPECTED: `_84236=[inheritance(swan,ext_intersection([swimmer,bird])),[0.72,0.81]];_84236=[inheritance(swan,int_intersection([swimmer,bird])),[0.98,0.81]];_84236=[inheritance(swan,ext_difference(swimmer,bird)),[0.18,0.81]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([inheritance(sport,competition),[0.9,0.9]],[inheritance(chess,competition),[0.8,0.9]],_100530).
```
EXPECTED: `_100530=[inheritance(int_intersection([sport,chess]),competition),[0.72,0.81]];_100530=[inheritance(ext_intersection([sport,chess]),competition),[0.98,0.81]];_100530=[inheritance(int_difference(sport,chess),competition),[0.18,0.81]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([inheritance(swan,swimmer),[0.9,0.8]],[inheritance(swan,ext_intersection([swimmer,bird])),_116896]).
```
EXPECTED: `_116896=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,swimmer),[0.9,0.8]],[inheritance(swan,int_intersection([swimmer,bird])),_119698]).
```
EXPECTED: `_119698=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,swimmer),[0.9,0.8]],[inheritance(swan,ext_difference(swimmer,bird)),_122496]).
```
EXPECTED: `_122496=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,swimmer),[0.9,0.8]],[negation(inheritance(swan,ext_difference(bird,swimmer))),_125286]).
```
EXPECTED: `_125286=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(sport,competition),[0.9,0.8]],[inheritance(int_intersection([sport,chess]),competition),_128088]).
```
EXPECTED: `_128088=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(sport,competition),[0.9,0.8]],[inheritance(ext_intersection([sport,chess]),competition),_19458]).
```
EXPECTED: `_19458=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(sport,competition),[0.9,0.8]],[inheritance(int_difference(sport,chess),competition),_22256]).
```
EXPECTED: `_22256=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(sport,competition),[0.9,0.8]],[negation(inheritance(int_difference(chess,sport),competition)),_25046]).
```
EXPECTED: `_25046=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,bird),[1,0.8]],[inheritance(swan,ext_intersection([swimmer,bird])),[0,0.8]],[inheritance(swan,swimmer),_27880]).
```
EXPECTED: `_27880=[0,0.64]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0,0.6400000000000001],
%      [0,0.64]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,bird),[0,0.8]],[inheritance(swan,int_intersection([swimmer,bird])),[1,0.8]],[inheritance(swan,swimmer),_30680]).
```
EXPECTED: `_30680=[1,0.64]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [1,0.6400000000000001],
%      [1,0.64]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,swimmer),[1,0.8]],[inheritance(swan,ext_difference(swimmer,bird)),[0,0.8]],[inheritance(swan,bird),_33462]).
```
EXPECTED: `_33462=[1,0.64]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [1,0.6400000000000001],
%      [1,0.64]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,bird),[0,0.8]],[inheritance(swan,ext_difference(swimmer,bird)),[0,0.8]],[inheritance(swan,swimmer),_36232]).
```
EXPECTED: `_36232=[0,0.64]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0,0.6400000000000001],
%      [0,0.64]) ].
```


```prolog
TEST: ?- inference([inheritance(sport,competition),[1,0.8]],[inheritance(int_intersection([sport,chess]),competition),[0,0.8]],[inheritance(chess,competition),_39028]).
```
EXPECTED: `_39028=[0,0.64]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0,0.6400000000000001],
%      [0,0.64]) ].
```


```prolog
TEST: ?- inference([inheritance(sport,competition),[0,0.8]],[inheritance(ext_intersection([sport,chess]),competition),[1,0.8]],[inheritance(chess,competition),_41828]).
```
EXPECTED: `_41828=[1,0.64]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [1,0.6400000000000001],
%      [1,0.64]) ].
```


```prolog
TEST: ?- inference([inheritance(sport,competition),[1,0.8]],[inheritance(int_difference(sport,chess),competition),[0,0.8]],[inheritance(chess,competition),_44610]).
```
EXPECTED: `_44610=[1,0.64]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [1,0.6400000000000001],
%      [1,0.64]) ].
```


```prolog
TEST: ?- inference([inheritance(chess,competition),[0,0.8]],[inheritance(int_difference(sport,chess),competition),[0,0.8]],[inheritance(sport,competition),_47380]).
```
EXPECTED: `_47380=[0,0.64]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0,0.6400000000000001],
%      [0,0.64]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,ext_intersection([swimmer,bird])),[0.9,0.8]],[inheritance(swan,swimmer),_50144]).
```
EXPECTED: `_50144=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,int_intersection([swimmer,bird])),[0.9,0.8]],[inheritance(swan,swimmer),_52952]).
```
EXPECTED: `_52952=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,ext_difference(swimmer,bird)),[0.9,0.8]],[inheritance(swan,swimmer),_55744]).
```
EXPECTED: `_55744=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,ext_difference(swimmer,bird)),[0.9,0.8]],[negation(inheritance(swan,bird)),_58540]).
```
EXPECTED: `_58540=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(int_intersection([sport,chess]),competition),[0.9,0.8]],[inheritance(sport,competition),_61342]).
```
EXPECTED: `_61342=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(ext_intersection([sport,chess]),competition),[0.9,0.8]],[inheritance(sport,competition),_64150]).
```
EXPECTED: `_64150=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(int_difference(sport,chess),competition),[0.9,0.8]],[inheritance(sport,competition),_66942]).
```
EXPECTED: `_66942=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(int_difference(sport,chess),competition),[0.9,0.8]],[negation(inheritance(chess,competition)),_69738]).
```
EXPECTED: `_69738=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(bird,animal),[0.9,0.8]],[inheritance(ext_intersection([swimmer,bird]),ext_intersection([swimmer,animal])),_72556]).
```
EXPECTED: `_72556=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(ext_intersection([swimmer,bird]),ext_intersection([swimmer,animal])),[0.9,0.8]],[inheritance(bird,animal),_75410]).
```
EXPECTED: `_75410=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(bird,animal),[0.9,0.8]],[inheritance(int_intersection([swimmer,bird]),int_intersection([swimmer,animal])),_78258]).
```
EXPECTED: `_78258=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(int_intersection([swimmer,bird]),int_intersection([swimmer,animal])),[0.9,0.8]],[inheritance(bird,animal),_81112]).
```
EXPECTED: `_81112=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([similarity(bird,animal),[0.9,0.8]],[similarity(ext_intersection([swimmer,bird]),ext_intersection([swimmer,animal])),_83960]).
```
EXPECTED: `_83960=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([similarity(ext_intersection([swimmer,bird]),ext_intersection([swimmer,animal])),[0.9,0.8]],[similarity(bird,animal),_86814]).
```
EXPECTED: `_86814=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([similarity(bird,animal),[0.9,0.8]],[similarity(int_intersection([swimmer,bird]),int_intersection([swimmer,animal])),_89662]).
```
EXPECTED: `_89662=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([similarity(int_intersection([swimmer,bird]),int_intersection([swimmer,animal])),[0.9,0.8]],[similarity(bird,animal),_92516]).
```
EXPECTED: `_92516=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(bird,animal),[0.9,0.8]],[inheritance(ext_difference(bird,swimmer),ext_difference(animal,swimmer)),_95344]).
```
EXPECTED: `_95344=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(ext_difference(bird,swimmer),ext_difference(animal,swimmer)),[0.9,0.8]],[inheritance(bird,animal),_98142]).
```
EXPECTED: `_98142=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(bird,animal),[0.9,0.8]],[inheritance(int_difference(bird,swimmer),int_difference(animal,swimmer)),_100934]).
```
EXPECTED: `_100934=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(int_difference(bird,swimmer),int_difference(animal,swimmer)),[0.9,0.8]],[inheritance(bird,animal),_103732]).
```
EXPECTED: `_103732=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([similarity(bird,animal),[0.9,0.8]],[similarity(ext_difference(bird,swimmer),ext_difference(animal,swimmer)),_106524]).
```
EXPECTED: `_106524=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([similarity(ext_difference(bird,swimmer),ext_difference(animal,swimmer)),[0.9,0.8]],[similarity(bird,animal),_109322]).
```
EXPECTED: `_109322=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([similarity(bird,animal),[0.9,0.8]],[similarity(int_difference(bird,swimmer),int_difference(animal,swimmer)),_112114]).
```
EXPECTED: `_112114=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([similarity(int_difference(bird,swimmer),int_difference(animal,swimmer)),[0.9,0.8]],[similarity(bird,animal),_114912]).
```
EXPECTED: `_114912=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(bird,animal),[0.9,0.8]],[inheritance(ext_difference(swimmer,animal),ext_difference(swimmer,bird)),_117704]).
```
EXPECTED: `_117704=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(ext_difference(swimmer,animal),ext_difference(swimmer,bird)),[0.9,0.8]],[inheritance(bird,animal),_120502]).
```
EXPECTED: `_120502=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(bird,animal),[0.9,0.8]],[inheritance(int_difference(swimmer,animal),int_difference(swimmer,bird)),_123294]).
```
EXPECTED: `_123294=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(int_difference(swimmer,animal),int_difference(swimmer,bird)),[0.9,0.8]],[inheritance(bird,animal),_126092]).
```
EXPECTED: `_126092=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([similarity(bird,animal),[0.9,0.8]],[similarity(ext_difference(swimmer,animal),ext_difference(swimmer,bird)),_128884]).
```
EXPECTED: `_128884=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([similarity(ext_difference(swimmer,animal),ext_difference(swimmer,bird)),[0.9,0.8]],[similarity(bird,animal),_19822]).
```
EXPECTED: `_19822=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([similarity(bird,animal),[0.9,0.8]],[similarity(int_difference(swimmer,animal),int_difference(swimmer,bird)),_22614]).
```
EXPECTED: `_22614=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([similarity(int_difference(swimmer,animal),int_difference(swimmer,bird)),[0.9,0.8]],[similarity(bird,animal),_25412]).
```
EXPECTED: `_25412=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(ext_set([earth]),ext_set([venus,mars,pluto])),[0.9,0.8]],[inheritance(ext_set([earth]),ext_set([pluto,saturn])),[0.7,0.8]],_28136).
```
EXPECTED: `_28136=[inheritance(ext_set([earth]),ext_set([pluto])),[0.63,0.64]];_28136=[inheritance(ext_set([earth]),ext_set([venus,mars,pluto,saturn])),[0.97,0.64]];_28136=[inheritance(ext_set([earth]),ext_set([venus,mars])),[0.27,0.64]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([inheritance(int_set([red,green,blue]),int_set([colorful])),[0.9,0.8]],[inheritance(int_set([purple,green]),int_set([colorful])),[0.7,0.8]],_49746).
```
EXPECTED: `_49746=[inheritance(int_set([green]),int_set([colorful])),[0.63,0.64]];_49746=[inheritance(int_set([red,blue,purple,green]),int_set([colorful])),[0.97,0.64]];_49746=[inheritance(int_set([red,blue]),int_set([colorful])),[0.271,0.64]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([inheritance(product([acid,base]),reaction),[1,0.9]],_71354).
```
EXPECTED: `_71354=[inheritance(acid,ext_image(reaction,[nil,base])),[1,0.9]];_71354=[inheritance(base,ext_image(reaction,[acid,nil])),[1,0.9]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([inheritance(acid,ext_image(reaction,[nil,base])),[1,0.9]],_83050).
```
EXPECTED: `_83050=[inheritance(product([acid,base]),reaction),[1,0.9]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([inheritance(acid,ext_image(reaction,[acid,nil])),[1,0.9]],_89202).
```
EXPECTED: `_89202=[inheritance(product([acid,acid]),reaction),[1,0.9]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([inheritance(neutralization,product([acid,base])),[1,0.9]],_95360).
```
EXPECTED: `_95360=[inheritance(int_image(neutralization,[nil,base]),acid),[1,0.9]];_95360=[inheritance(int_image(neutralization,[acid,nil]),base),[1,0.9]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([inheritance(int_image(neutralization,[nil,base]),acid),[1,0.9]],_107056).
```
EXPECTED: `_107056=[inheritance(neutralization,product([acid,base])),[1,0.9]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([inheritance(int_image(neutralization,[acid,nil]),base),[1,0.9]],_113208).
```
EXPECTED: `_113208=[inheritance(neutralization,product([acid,base])),[1,0.9]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([inheritance(bird,animal),[0.9,0.8]],[inheritance(product([bird,plant]),product([animal,plant])),_119456]).
```
EXPECTED: `_119456=[0.9,0.8]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.8],
%      [0.9,0.8]) ].
```


```prolog
TEST: ?- inference([inheritance(product([plant,bird]),product([plant,animal])),[0.9,0.8]],[inheritance(bird,animal),_122110]).
```
EXPECTED: `_122110=[0.9,0.8]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.8],
%      [0.9,0.8]) ].
```


```prolog
TEST: ?- inference([inheritance(neutralization,reaction),[0.9,0.8]],[inheritance(ext_image(neutralization,[acid,nil]),ext_image(reaction,[acid,nil])),_124788]).
```
EXPECTED: `_124788=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(ext_image(neutralization,[acid,nil]),ext_image(reaction,[acid,nil])),[0.9,0.8]],[inheritance(neutralization,reaction),_127610]).
```
EXPECTED: `_127610=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(neutralization,reaction),[0.9,0.8]],[inheritance(int_image(neutralization,[acid,nil]),int_image(reaction,[acid,nil])),_130426]).
```
EXPECTED: `_130426=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(int_image(neutralization,[acid,nil]),int_image(reaction,[acid,nil])),[0.9,0.8]],[inheritance(neutralization,reaction),_21368]).
```
EXPECTED: `_21368=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(soda,base),[0.9,0.8]],[inheritance(ext_image(reaction,[nil,base]),ext_image(reaction,[nil,soda])),_24184]).
```
EXPECTED: `_24184=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(ext_image(reaction,[nil,base]),ext_image(reaction,[nil,soda])),[0.9,0.8]],[inheritance(soda,base),_27026]).
```
EXPECTED: `_27026=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(soda,base),[0.9,0.8]],[inheritance(int_image(neutralization,[nil,base]),int_image(neutralization,[nil,soda])),_29862]).
```
EXPECTED: `_29862=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(int_image(neutralization,[nil,base]),int_image(neutralization,[nil,soda])),[0.9,0.8]],[inheritance(soda,base),_32704]).
```
EXPECTED: `_32704=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- revision([implication(inheritance(robin,flyer),inheritance(robin,bird)),[1,0.8]],[implication(inheritance(robin,flyer),inheritance(robin,bird)),[0,0.5]],_35448).
```
EXPECTED: `_35448=[implication(inheritance(robin,flyer),inheritance(robin,bird)),[0.8,0.83]]`

```diff
+SUCCESS!
```

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
TEST: ?- revision([equivalence(inheritance(robin,flyer),inheritance(robin,bird)),[1,0.8]],[equivalence(inheritance(robin,flyer),inheritance(robin,bird)),[0,0.5]],_41128).
```
EXPECTED: `_41128=[equivalence(inheritance(robin,flyer),inheritance(robin,bird)),[0.8,0.83]]`

```diff
+SUCCESS!
```

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
TEST: ?- choice([implication(inheritance(robin,flyer),inheritance(robin,bird)),[1,0.8]],[implication(inheritance(robin,flyer),inheritance(robin,bird)),[0,0.5]],_46808).
```
EXPECTED: `_46808=[implication(inheritance(robin,flyer),inheritance(robin,bird)),[1,0.8]]`

```diff
+SUCCESS!
```

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
TEST: ?- choice([implication(inheritance(robin,flyer),inheritance(robin,bird)),[0.8,0.9]],[implication(inheritance(robin,swimmer),inheritance(robin,bird)),[1,0.5]],_52190).
```
EXPECTED: `_52190=[implication(inheritance(robin,flyer),inheritance(robin,bird)),[0.8,0.9]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[implication(inheritance(robin,flyer),inheritance(robin,bird)),[1,0.5]],_57660).
```
EXPECTED: `_57660=[implication(inheritance(robin,flyer),inheritance(robin,animal)),[0.9,0.36]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([equivalence(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[equivalence(inheritance(robin,flyer),inheritance(robin,bird)),[1,0.5]],_63284).
```
EXPECTED: `_63284=[equivalence(inheritance(robin,flyer),inheritance(robin,animal)),[0.9,0.4]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[implication(inheritance(robin,bird),inheritance(robin,flyer)),[1,0.5]],_68948).
```
EXPECTED: `_68948=[implication(inheritance(robin,flyer),inheritance(robin,animal)),[0.9,0.29]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[implication(inheritance(robin,flyer),inheritance(robin,animal)),[1,0.5]],_74566).
```
EXPECTED: `_74566=[implication(inheritance(robin,flyer),inheritance(robin,bird)),[1,0.26]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([implication(inheritance(robin,flyer),inheritance(robin,bird)),[0.9,0.8]],[implication(inheritance(robin,bird),inheritance(robin,animal)),[1,0.5]],_80160).
```
EXPECTED: `_80160=[implication(inheritance(robin,animal),inheritance(robin,flyer)),[1,0.26]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([implication(inheritance(robin,flyer),inheritance(robin,animal)),[0.9,0.8]],_85772).
```
EXPECTED: `_85772=[implication(inheritance(robin,animal),inheritance(robin,flyer)),[1,0.42]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([equivalence(inheritance(robin,flyer),inheritance(robin,bird)),[0.9,0.8]],[implication(inheritance(robin,flyer),inheritance(robin,bird)),_91378]).
```
EXPECTED: `_91378=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,flyer),inheritance(robin,bird)),[0.9,0.8]],[implication(inheritance(robin,bird),inheritance(robin,flyer)),[0.9,0.8]],_94108).
```
EXPECTED: `_94108=[equivalence(inheritance(robin,flyer),inheritance(robin,bird)),[0.81,0.64]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([similarity(swan,bird),[0.9,0.8]],[inheritance(swan,bird),_99774]).
```
EXPECTED: `_99774=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,bird),[0.9,0.8]],[similarity(swan,bird),_102560]).
```
EXPECTED: `_102560=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(swan,bird),[1,0.8]],[inheritance(bird,swan),[0.1,0.8]],_105284).
```
EXPECTED: `_105284=[similarity(swan,bird),[0.1,0.64]]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [ similarity(swan,bird),
%        [0.1,0.6400000000000001] ],
%      [ similarity(swan,bird),
%        [0.1,0.64] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[implication(inheritance(robin,bird),inheritance(robin,flyer)),[0.9,0.8]],[equivalence(_110038,_110040),_110044]).
```
EXPECTED: `_110038=inheritance(robin,flyer)`
EXPECTED: `_110040=inheritance(robin,animal)`
EXPECTED: `_110044=[0.82,0.39]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[implication(inheritance(robin,flyer),inheritance(robin,animal)),[0.9,0.8]],[equivalence(_114060,_114062),_114066]).
```
EXPECTED: `_114060=inheritance(robin,flyer)`
EXPECTED: `_114062=inheritance(robin,bird)`
EXPECTED: `_114066=[0.82,0.39]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[equivalence(inheritance(robin,flyer),inheritance(robin,animal)),[0.9,0.8]],_117964).
```
EXPECTED: `_117964=[implication(inheritance(robin,bird),inheritance(robin,flyer)),[0.81,0.58]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[equivalence(inheritance(robin,flyer),inheritance(robin,bird)),[0.9,0.8]],_123594).
```
EXPECTED: `_123594=[implication(inheritance(robin,flyer),inheritance(robin,animal)),[0.81,0.58]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[implication(inheritance(robin,bird),inheritance(robin,flyer)),[0.9,0.8]],_129224).
```
EXPECTED: `_129224=[implication(inheritance(robin,bird),conjunction([inheritance(robin,animal),inheritance(robin,flyer)])),[0.81,0.64]];_129224=[implication(inheritance(robin,bird),disjunction([inheritance(robin,animal),inheritance(robin,flyer)])),[0.99,0.64]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[implication(inheritance(robin,flyer),inheritance(robin,animal)),[0.9,0.8]],_32468).
```
EXPECTED: `_32468=[implication(disjunction([inheritance(robin,bird),inheritance(robin,flyer)]),inheritance(robin,animal)),[0.81,0.64]];_32468=[implication(conjunction([inheritance(robin,bird),inheritance(robin,flyer)]),inheritance(robin,animal)),[0.99,0.64]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([inheritance(robin,animal),[0.9,0.9]],[inheritance(robin,flyer),[0.9,0.9]],[conjunction([inheritance(robin,animal),inheritance(robin,flyer)]),_47402]).
```
EXPECTED: `_47402=[0.81,0.81]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.81,0.81],
%      [0.81,0.81]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,animal),[0.9,0.8]],[inheritance(robin,flyer),[0.9,0.8]],[disjunction([inheritance(robin,animal),inheritance(robin,flyer)]),_50172]).
```
EXPECTED: `_50172=[0.99,0.64]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.99,0.6400000000000001],
%      [0.99,0.64]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[implication(inheritance(robin,bird),conjunction([inheritance(robin,animal),inheritance(robin,flyer)])),_52982]).
```
EXPECTED: `_52982=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[implication(inheritance(robin,bird),disjunction([inheritance(robin,animal),inheritance(robin,flyer)])),_55808]).
```
EXPECTED: `_55808=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[implication(disjunction([inheritance(robin,bird),inheritance(robin,flyer)]),inheritance(robin,animal)),_58640]).
```
EXPECTED: `_58640=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[implication(conjunction([inheritance(robin,bird),inheritance(robin,flyer)]),inheritance(robin,animal)),_61466]).
```
EXPECTED: `_61466=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,animal),[0.9,0.8]],[conjunction([inheritance(robin,animal),inheritance(robin,flyer)]),_64274]).
```
EXPECTED: `_64274=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,animal),[0.9,0.8]],[disjunction([inheritance(robin,animal),inheritance(robin,flyer)]),_67076]).
```
EXPECTED: `_67076=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,flyer)),[1,0.8]],[implication(inheritance(robin,bird),conjunction([inheritance(robin,animal),inheritance(robin,flyer)])),[0,0.8]],[implication(inheritance(robin,bird),inheritance(robin,animal)),_69952]).
```
EXPECTED: `_69952=[0,0.64]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0,0.6400000000000001],
%      [0,0.64]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,flyer)),[0,0.8]],[implication(inheritance(robin,bird),disjunction([inheritance(robin,animal),inheritance(robin,flyer)])),[1,0.8]],[implication(inheritance(robin,bird),inheritance(robin,animal)),_72794]).
```
EXPECTED: `_72794=[1,0.64]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [1,0.6400000000000001],
%      [1,0.64]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[1,0.8]],[implication(disjunction([inheritance(robin,bird),inheritance(robin,flyer)]),inheritance(robin,animal)),[0,0.8]],[implication(inheritance(robin,flyer),inheritance(robin,animal)),_75628]).
```
EXPECTED: `_75628=[0,0.64]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0,0.6400000000000001],
%      [0,0.64]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0,0.8]],[implication(conjunction([inheritance(robin,bird),inheritance(robin,flyer)]),inheritance(robin,animal)),[1,0.8]],[implication(inheritance(robin,flyer),inheritance(robin,animal)),_78470]).
```
EXPECTED: `_78470=[1,0.64]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [1,0.6400000000000001],
%      [1,0.64]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,bird),[1,0.8]],[conjunction([inheritance(robin,bird),inheritance(robin,flyer)]),[0,0.8]],_81158).
```
EXPECTED: `_81158=[inheritance(robin,flyer),[0,0.64]]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [ inheritance(robin,flyer),
%        [0,0.6400000000000001] ],
%      [ inheritance(robin,flyer),
%        [0,0.64] ]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,bird),[0,0.8]],[disjunction([inheritance(robin,bird),inheritance(robin,flyer)]),[1,0.8]],_85822).
```
EXPECTED: `_85822=[inheritance(robin,flyer),[1,0.64]]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [ inheritance(robin,flyer),
%        [1,0.6400000000000001] ],
%      [ inheritance(robin,flyer),
%        [1,0.64] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),conjunction([inheritance(robin,animal),inheritance(robin,flyer)])),[0.9,0.8]],[implication(inheritance(robin,bird),inheritance(robin,animal)),_90580]).
```
EXPECTED: `_90580=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),disjunction([inheritance(robin,animal),inheritance(robin,flyer)])),[0.9,0.8]],[implication(inheritance(robin,bird),inheritance(robin,animal)),_93412]).
```
EXPECTED: `_93412=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([implication(disjunction([inheritance(robin,bird),inheritance(robin,flyer)]),inheritance(robin,animal)),[0.9,0.8]],[implication(inheritance(robin,bird),inheritance(robin,animal)),_96238]).
```
EXPECTED: `_96238=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([inheritance(robin,bird),inheritance(robin,flyer)]),inheritance(robin,animal)),[0.9,0.8]],[implication(inheritance(robin,bird),inheritance(robin,animal)),_99070]).
```
EXPECTED: `_99070=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([conjunction([inheritance(robin,bird),inheritance(robin,flyer)]),[0.9,0.8]],[inheritance(robin,bird),_101872]).
```
EXPECTED: `_101872=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([disjunction([inheritance(robin,bird),inheritance(robin,flyer)]),[0.9,0.8]],[inheritance(robin,bird),_104680]).
```
EXPECTED: `_104680=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([implication(p,q),[0.9,0.8]],[implication(conjunction([p,r]),conjunction([q,r])),_107492]).
```
EXPECTED: `_107492=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([p,r]),conjunction([q,r])),[0.9,0.8]],[implication(p,q),_110340]).
```
EXPECTED: `_110340=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([implication(p,q),[0.9,0.8]],[implication(disjunction([p,r]),disjunction([q,r])),_113182]).
```
EXPECTED: `_113182=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([implication(disjunction([p,r]),disjunction([q,r])),[0.9,0.8]],[implication(p,q),_116030]).
```
EXPECTED: `_116030=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([equivalence(p,q),[0.9,0.8]],[equivalence(conjunction([p,r]),conjunction([q,r])),_118872]).
```
EXPECTED: `_118872=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([equivalence(conjunction([p,r]),conjunction([q,r])),[0.9,0.8]],[equivalence(p,q),_121720]).
```
EXPECTED: `_121720=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([equivalence(p,q),[0.9,0.8]],[equivalence(disjunction([p,r]),disjunction([q,r])),_124562]).
```
EXPECTED: `_124562=[0.9,0.72]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.7200000000000001],
%      [0.9,0.72]) ].
```


```prolog
TEST: ?- inference([equivalence(disjunction([p,r]),disjunction([q,r])),[0.9,0.8]],[equivalence(p,q),_127410]).
```
EXPECTED: `_127410=[0.9,0.44]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.4444444444444445],
%      [0.9,0.44]) ].
```


```prolog
TEST: ?- inference([negation(inheritance(robin,bird)),[0.9,0.8]],_130162).
```
EXPECTED: `_130162=[inheritance(robin,bird),[0.1,0.8]]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [ inheritance(robin,bird),
%        [0.09999999999999998,0.8] ],
%      [ inheritance(robin,bird),
%        [0.1,0.8] ]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,bird),[0.2,0.8]],[negation(inheritance(robin,bird)),_22966]).
```
EXPECTED: `_22966=[0.8,0.8]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.8,0.8],
%      [0.8,0.8]) ].
```


```prolog
TEST: ?- inference([implication(negation(inheritance(penguin,flyer)),inheritance(penguin,swimmer)),[0.1,0.8]],[implication(negation(inheritance(penguin,swimmer)),inheritance(penguin,flyer)),_25662]).
```
EXPECTED: `_25662=[0,0.42]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0,0.4186046511627907],
%      [0,0.42]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[inheritance(robin,bird),[1,0.5]],_28324).
```
EXPECTED: `_28324=[inheritance(robin,animal),[0.9,0.36]]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [ inheritance(robin,animal),
%        [0.9,0.36000000000000004] ],
%      [ inheritance(robin,animal),
%        [0.9,0.36] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(robin,bird),inheritance(robin,animal)),[0.9,0.8]],[inheritance(robin,animal),[1,0.5]],_32986).
```
EXPECTED: `_32986=[inheritance(robin,bird),[1,0.26]]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [ inheritance(robin,bird),
%        [1,0.2647058823529412] ],
%      [ inheritance(robin,bird),
%        [1,0.26] ]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,animal),[0.9,0.8]],[inheritance(robin,flyer),[1,0.5]],[implication(inheritance(robin,flyer),inheritance(robin,animal)),_37724]).
```
EXPECTED: `_37724=[0.9,0.29]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.28571428571428575],
%      [0.9,0.29]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,animal),[1,0.5]],[equivalence(inheritance(robin,flyer),inheritance(robin,animal)),[0.9,0.8]],_40422).
```
EXPECTED: `_40422=[inheritance(robin,flyer),[0.9,0.36]]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [ inheritance(robin,flyer),
%        [0.9,0.36000000000000004] ],
%      [ inheritance(robin,flyer),
%        [0.9,0.36] ]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,animal),[0.9,0.8]],[inheritance(robin,flyer),[1,0.5]],[equivalence(inheritance(robin,flyer),inheritance(robin,animal)),_45190]).
```
EXPECTED: `_45190=[0.9,0.29]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9000000000000001,0.2857142857142857],
%      [0.9,0.29]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([a1,a2,a3]),c),[0.9,0.9]],[a2,[0.9,0.9]],_47946).
```
EXPECTED: `_47946=[implication(conjunction([a1,a3]),c),[0.81,0.66]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([implication(conjunction([a1,a2,a3]),c),[0.9,0.9]],[implication(conjunction([a1,a3]),c),[0.9,0.9]],[a2,_54490]).
```
EXPECTED: `_54490=[0.9,0.42]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.42163100057836905],
%      [0.9,0.42]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([a1,a3]),c),[0.9,0.9]],[a2,[0.9,0.9]],[implication(conjunction([a2,a1,a3]),c),_57338]).
```
EXPECTED: `_57338=[0.9,0.42]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.42163100057836905],
%      [0.9,0.42]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([a1,a2,a3]),c),[0.9,0.9]],[implication(b2,a2),[0.9,0.9]],_60046).
```
EXPECTED: `_60046=[implication(conjunction([a1,b2,a3]),c),[0.81,0.66]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([implication(conjunction([a1,a2,a3]),c),[0.9,0.9]],[implication(conjunction([a1,b2,a3]),c),[0.9,0.9]],[implication(b2,a2),_66724]).
```
EXPECTED: `_66724=[0.9,0.42]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [0.9,0.42163100057836905],
%      [0.9,0.42]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([a1,b2,a3]),c),[0.9,0.9]],[implication(b2,a2),[0.9,0.9]],_69416).
```
EXPECTED: `_69416=[implication(conjunction([a1,a2,a3]),c),[0.9,0.42]]`

```diff
+SUCCESS!
```

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
TEST: ?- revision([implication(inheritance(_75960,bird),inheritance(_75960,flyer)),[0.9,0.8]],[implication(inheritance(_76014,bird),inheritance(_76014,flyer)),[1,0.5]],_75944).
```
EXPECTED: `_75944=[implication(inheritance(_76014,bird),inheritance(_76014,flyer)),[0.92,0.83]]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [ implication(
%           inheritance(_83942,bird),
%           inheritance(_83942,flyer)),
%        [0.9200000000000002,0.8333333333333334] ],
%      [ implication(
%           inheritance(_83942,bird),
%           inheritance(_83942,flyer)),
%        [0.92,0.83] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(_84366,bird),inheritance(_84366,animal)),[1,0.9]],[implication(inheritance(_84414,robin),inheritance(_84414,bird)),[1,0.9]],_84350).
```
EXPECTED: `_84350=[implication(inheritance(_84414,robin),inheritance(_84414,animal)),[1,0.81]]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [ implication(
%           inheritance(_92118,robin),
%           inheritance(_92118,animal)),
%        [1,0.81] ],
%      [ implication(
%           inheritance(_92118,robin),
%           inheritance(_92118,animal)),
%        [1,0.81] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(_92542,bird),inheritance(_92542,animal)),[1,0.9]],[implication(inheritance(_92590,robin),inheritance(_92590,animal)),[1,0.9]],_92526).
```
EXPECTED: `_92526=[implication(inheritance(_92590,robin),inheritance(_92590,bird)),[1,0.45]]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [ implication(
%           inheritance(_100426,robin),
%           inheritance(_100426,bird)),
%        [1,0.44751381215469616] ],
%      [ implication(
%           inheritance(_100426,robin),
%           inheritance(_100426,bird)),
%        [1,0.45] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(_100850,robin),inheritance(_100850,animal)),[1,0.9]],[implication(inheritance(_100898,robin),inheritance(_100898,bird)),[1,0.9]],_100834).
```
EXPECTED: `_100834=[implication(inheritance(_100898,bird),inheritance(_100898,animal)),[1,0.45]]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [ implication(
%           inheritance(_108734,bird),
%           inheritance(_108734,animal)),
%        [1,0.44751381215469616] ],
%      [ implication(
%           inheritance(_108734,bird),
%           inheritance(_108734,animal)),
%        [1,0.45] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(_109158,feathered),inheritance(_109158,bird)),[1,0.9]],[equivalence(inheritance(_109206,flyer),inheritance(_109206,bird)),[1,0.9]],_109142).
```
EXPECTED: `_109142=[implication(inheritance(_109206,feathered),inheritance(_109206,flyer)),[1,0.81]]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [ implication(
%           inheritance(_116910,feathered),
%           inheritance(_116910,flyer)),
%        [1,0.81] ],
%      [ implication(
%           inheritance(_116910,feathered),
%           inheritance(_116910,flyer)),
%        [1,0.81] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(_117334,feathered),inheritance(_117334,flyer)),[1,0.9]],[implication(inheritance(_117382,feathered),inheritance(_117382,bird)),[1,0.9]],_117318).
```
EXPECTED: `_117318=[implication(inheritance(_117382,bird),inheritance(_117382,flyer)),[1,0.45]]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [ implication(
%           inheritance(_125218,bird),
%           inheritance(_125218,flyer)),
%        [1,0.44751381215469616] ],
%      [ implication(
%           inheritance(_125218,bird),
%           inheritance(_125218,flyer)),
%        [1,0.45] ]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([inheritance(_125652,feathered),inheritance(_125652,flyer)]),inheritance(_125652,bird)),[1,0.9]],[implication(inheritance(_125712,swimmer),inheritance(_125712,feathered)),[1,0.9]],_125626).
```
EXPECTED: `_125626=[implication(conjunction([inheritance(_125712,swimmer),inheritance(_125712,flyer)]),inheritance(_125712,bird)),[1,0.81]]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [ implication(
%           conjunction([ inheritance(_25646,swimmer),
%                         inheritance(_25646,flyer) ]),
%           inheritance(_25646,bird)),
%        [1,0.81] ],
%      [ implication(
%           conjunction([ inheritance(_25646,swimmer),
%                         inheritance(_25646,flyer) ]),
%           inheritance(_25646,bird)),
%        [1,0.81] ]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([inheritance(_26178,feathered),inheritance(_26178,flyer)]),inheritance(_26178,bird)),[1,0.9]],[implication(conjunction([inheritance(_26178,swimmer),inheritance(_26178,flyer)]),inheritance(_26178,bird)),[1,0.9]],_26152).
```
EXPECTED: `_26152=[implication(inheritance(_26178,swimmer),inheritance(_26178,feathered)),[1,0.45]]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [ implication(
%           inheritance(_34096,swimmer),
%           inheritance(_34096,feathered)),
%        [1,0.44751381215469616] ],
%      [ implication(
%           inheritance(_34096,swimmer),
%           inheritance(_34096,feathered)),
%        [1,0.45] ]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([inheritance(_34530,swimmer),inheritance(_34530,flyer)]),inheritance(_34530,bird)),[1,0.9]],[implication(inheritance(_34590,swimmer),inheritance(_34590,feathered)),[1,0.9]],_34504).
```
EXPECTED: `_34504=[implication(conjunction([inheritance(_34590,feathered),inheritance(_34590,flyer)]),inheritance(_34590,bird)),[1,0.45]]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [ implication(
%           conjunction([ inheritance(_46134,feathered),
%                         inheritance(_46134,flyer) ]),
%           inheritance(_46134,bird)),
%        [1,0.44751381215469616] ],
%      [ implication(
%           conjunction([ inheritance(_46134,feathered),
%                         inheritance(_46134,flyer) ]),
%           inheritance(_46134,bird)),
%        [1,0.45] ]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([inheritance(_46666,feathered),inheritance(_46666,flyer)]),inheritance(_46666,bird)),[1,0.9]],[implication(inheritance(_46726,swimmer),inheritance(_46726,feathered)),[1,0.9]],_46640).
```
EXPECTED: `_46640=[implication(conjunction([inheritance(_46726,swimmer),inheritance(_46726,flyer)]),inheritance(_46726,bird)),[1,0.81]]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [ implication(
%           conjunction([ inheritance(_58090,swimmer),
%                         inheritance(_58090,flyer) ]),
%           inheritance(_58090,bird)),
%        [1,0.81] ],
%      [ implication(
%           conjunction([ inheritance(_58090,swimmer),
%                         inheritance(_58090,flyer) ]),
%           inheritance(_58090,bird)),
%        [1,0.81] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(_58612,bird),inheritance(_58612,animal)),[1,0.9]],[inheritance(robin,bird),[1,0.9]],_58596).
```
EXPECTED: `_58596=[inheritance(robin,animal),[1,0.81]]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [ inheritance(robin,animal),
%        [1,0.81] ],
%      [ inheritance(robin,animal),
%        [1,0.81] ]) ].
```


```prolog
TEST: ?- inference([implication(inheritance(_63166,bird),inheritance(_63166,animal)),[1,0.9]],[inheritance(robin,animal),[1,0.9]],_63150).
```
EXPECTED: `_63150=[inheritance(robin,bird),[1,0.45]]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [ inheritance(robin,bird),
%        [1,0.44751381215469616] ],
%      [ inheritance(robin,bird),
%        [1,0.45] ]) ].
```


```prolog
TEST: ?- inference([inheritance(robin,animal),[1,0.9]],[equivalence(inheritance(_67828,bird),inheritance(_67828,animal)),[1,0.9]],_67776).
```
EXPECTED: `_67776=[inheritance(robin,bird),[1,0.81]]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [ inheritance(robin,bird),
%        [1,0.81] ],
%      [ inheritance(robin,bird),
%        [1,0.81] ]) ].
```


```prolog
TEST: ?- inference([implication(conjunction([inheritance(_72356,feathered),inheritance(_72356,flyer)]),inheritance(_72356,bird)),[1,0.9]],[inheritance(swan,feathered),[1,0.9]],_72330).
```
EXPECTED: `_72330=[implication(inheritance(swan,flyer),inheritance(swan,bird)),[1,0.81]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([conjunction([inheritance(var(_77840,[]),bird),inheritance(var(_77840,[]),swimmer)]),[1,0.9]],[inheritance(swan,bird),[1,0.9]],[inheritance(swan,swimmer),_77936]).
```
EXPECTED: `_77936=[1,0.42]`

```diff
+SUCCESS!
```

```prolog
% [ '='(
%      [1,0.42631578947368426],
%      [1,0.42]) ].
```


```prolog
TEST: ?- inference([conjunction([inheritance(var(_80740,[]),flyer),inheritance(var(_80740,[]),bird),inheritance(var(_80740,[]),swimmer)]),[1,0.9]],[inheritance(swan,bird),[1,0.9]],_80714).
```
EXPECTED: `_80714=[conjunction([inheritance(swan,flyer),inheritance(swan,swimmer)]),[1,0.42]]`

```diff
+SUCCESS!
```

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
TEST: ?- inference([inheritance(robin,animal),[1,0.9]],[inheritance(robin,bird),[1,0.9]],_87890).
```
EXPECTED: `_87890=[implication(inheritance(_87996,bird),inheritance(_87996,animal)),[1,0.45]];_87890=[equivalence(inheritance(_87996,bird),inheritance(_87996,animal)),[1,0.45]];_87890=[conjunction([inheritance(var(_88120,[]),bird),inheritance(var(_88120,[]),animal)]),[1,0.81]]`

```diff
-FAILED!
```

```prolog
% [ ('='(Term_List,
%       [ implication(
%            inheritance(_106594,bird),
%            inheritance(_106594,animal)),
%         [1,0.45] ]) ;
%     '='(Term_List,
%        [ equivalence(
%             inheritance(_106594,bird),
%             inheritance(_106594,animal)),
%          [1,0.45] ]) ;
%    '='(Term_List,
%       [ conjunction([ inheritance(
%                          var(_106670,[]),
%                          bird),
%                       inheritance(
%                          var(_106670,[]),
%                          animal) ]),
%         [1,0.81] ])) ].
```


```prolog
TEST: ?- inference([inheritance(sport,competition),[1,0.9]],[inheritance(chess,competition),[1,0.9]],_107190).
```
EXPECTED: `_107190=[implication(inheritance(sport,_107298),inheritance(chess,_107298)),[1,0.45]];_107190=[equivalence(inheritance(sport,_107298),inheritance(chess,_107298)),[1,0.45]];_107190=[conjunction([inheritance(chess,var(_107420,[])),inheritance(sport,var(_107420,[]))]),[1,0.81]]`

```diff
-FAILED!
```

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
%                          var(_20126,[])),
%                       inheritance(sport,
%                          var(_20126,[])) ]),
%         [1,0.81] ])) ].
```


```prolog
TEST: ?- inference([inheritance(key1,ext_image(open,[nil,lock1])),[1,0.9]],[inheritance(key1,key),[1,0.9]],_20308).
```
EXPECTED: `_20308=[implication(inheritance(_20432,key),inheritance(_20432,ext_image(open,[nil,lock1]))),[1,0.45]];_20308=[conjunction([inheritance(var(_20514,[]),key),inheritance(var(_20514,[]),ext_image(open,[nil,lock1]))]),[1,0.81]]`

```diff
-FAILED!
```

```prolog
% [ ('='(Term_List,
%       [ implication(
%            inheritance(_35988,key),
%            inheritance(_35988,
%               ext_image(open,
%                  [nil,lock1]))),
%         [1,0.45] ]) ;
%    '='(Term_List,
%       [ conjunction([ inheritance(
%                          var(_36028,[]),
%                          key),
%                       inheritance(
%                          var(_36028,[]),
%                          ext_image(open,
%                             [nil,lock1])) ]),
%         [1,0.81] ])) ].
```


```prolog
TEST: ?- inference([implication(inheritance(_36564,key),inheritance(lock1,ext_image(open,[_36564,nil]))),[1,0.9]],[inheritance(lock1,lock),[1,0.9]],_36548).
```
EXPECTED: `_36548=[implication(conjunction([inheritance(_36564,key),inheritance(_36706,lock)]),inheritance(_36706,ext_image(open,[_36564,nil]))),[1,0.45]];_36548=[conjunction([implication(inheritance(_36564,key),inheritance(var(_36706,[]),ext_image(open,[_36564,nil]))),inheritance(var(_36706,[]),lock)]),[1,0.81]]`

```diff
-FAILED!
```

```prolog
% [ ('='(Term_List,
%       [ implication(
%            conjunction([ inheritance(_55612,key),
%                          inheritance(_55624,lock) ]),
%            inheritance(_55624,
%               ext_image(open,
%                  [_55612,nil]))),
%         [1,0.45] ]) ;
%    '='(Term_List,
%       [ conjunction([ implication(
%                          inheritance(CAR,key),
%                          inheritance(
%                             var(_55624,[]),
%                             ext_image(open,
%                                [CAR,nil]))),
%                       inheritance(
%                          var(_55624,[]),
%                          lock) ]),
%         [1,0.81] ])) ].
```


```prolog
TEST: ?- inference([conjunction([inheritance(var(_56278,[]),key),inheritance(lock1,ext_image(open,[var(_56278,[]),nil]))]),[1,0.9]],[inheritance(lock1,lock),[1,0.9]],_56252).
```
EXPECTED: `_56252=[implication(inheritance(_56410,lock),conjunction([inheritance(_56410,ext_image(open,[var(_56278,[_56410]),nil])),inheritance(var(_56278,[_56410]),key)])),[1,0.45]];_56252=[conjunction([inheritance(var(_56538,[]),lock),inheritance(var(_56538,[]),ext_image(open,[var(_56278,[]),nil])),inheritance(var(_56278,[]),key)]),[1,0.81]]`

```diff
-FAILED!
```

```prolog
% [ ('='(Term_List,
%       [ implication(
%            inheritance(_78770,lock),
%            conjunction([ inheritance(CAR,
%                             ext_image(open,
%                                [ var(_78804,
%                                     [CAR]),
%                                  nil ])),
%                          inheritance(
%                             var(_78804,
%                                [CAR]),
%                             key) ])),
%         [1,0.45] ]) ;
%    '='(Term_List,
%       [ conjunction([ inheritance(
%                          var(_78868,[]),
%                          lock),
%                       inheritance(
%                          var(_78868,[]),
%                          ext_image(open,
%                             [ var(_78804,[]),
%                               nil ])),
%                       inheritance(
%                          var(_78804,[]),
%                          key) ]),
%         [1,0.81] ])) ].
```

true.

baseKB:  ?-

