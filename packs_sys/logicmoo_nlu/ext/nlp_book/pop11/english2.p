;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; english2.p [Chapter  3] RTN for a fragment of English

vars networks abbreviations;

[ S
  [  [Initial  0]
     [Final   2]
     [From 0 to 1 by NP]
     [From 1 to 2 by VP]]
  NP
  [  [Initial  0]
     [Final   2]
     [From 0 to 1 by DET]
     [From 1 to 2 by N]
     [From 2 to 3 by WH]
     [From 3 to 2 by VP]]
  VP
  [  [Initial  0]
     [Final  1 2]
     [From 0 to 1 by V]
     [From 1 to 2 by NP]
     [From 1 to 3 by that]
     [From 3 to 2 by S]]
] -> networks;

[  [N woman house table mouse man]
   [NP Mayumi Maria Washington John Mary]
   [DET a the that]
   [V sees hits sings lacks saw]
   [WH who which that]
] -> abbreviations;

vars english_2; true -> english_2;
