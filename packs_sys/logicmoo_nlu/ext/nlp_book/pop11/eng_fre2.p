;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; eng_fre2.p [Chapter  3] Simple PT for English-French translation

vars networks abbreviations;

[ S
  [  [Initial 0]
     [Final   2]
     [From 0 to 1 by NP]
     [From 1 to 2 by VP]]
  NP
  [  [Initial 0]
     [Final   2]
     [From 0 to 1 by DETFEMN]
     [From 1 to 2 by NFEMN]
     [From 0 to 4 by DETMASC]
     [From 4 to 2 by NMASC]
     [From 2 to 3 by WH]
     [From 3 to 2 by VP]]
  VP
   [  [Initial 0]
      [Final 1 2]
      [From 0 to 1 by V]
      [From 1 to 2 by NP]
      [From 1 to 3 by [that que]]
      [From 3 to 2 by S]]
] -> networks;

[  [NMASC abbreviates [man homme] [horse cheval] ]
   [NFEMN abbreviates [house maison] [table table] ]
   [NP abbreviates [John Jean] [Mary Marie] [Jean Jeanne] ]
   [DETMASC abbreviates [a un] [the le] [this ce] ]
   [DETFEMN abbreviates [a une] [the la] [this cette] ]
   [V abbreviates [sees voit] [hits frappe] [sings chante] [lacks manque] ]
   [WH abbreviates [who qui] [which qui] [that qui] ]
] -> abbreviations;

vars eng_fre2; true -> eng_fre2;
