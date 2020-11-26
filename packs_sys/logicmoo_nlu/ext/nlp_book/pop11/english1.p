;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; english1.p [Chapter  2] FSTN for a fragment of English

vars english_1;
[  [Initial 1]
   [Final 9]
    [From 1 to 3 by NP]
    [From 1 to 2 by DET]
    [From 2 to 3 by N]
    [From 3 to 4 by BV]
    [From 4 to 5 by ADV]
    [From 4 to 5 by #]
    [From 5 to 6 by DET]
    [From 5 to 7 by DET]
    [From 5 to 8 by #]
    [From 6 to 6 by MOD]
    [From 6 to 7 by ADJ]
    [From 7 to 9 by N]
    [From 8 to 8 by MOD]
    [From 8 to 9 by ADJ]
    [From 9 to 4 by CNJ]
    [From 9 to 1 by CNJ]] -> english_1;

vars abbreviations;
  [[NP abbreviates kim sandy lee]
   [DET abbreviates a the her]
   [N abbreviates consumer man woman]
   [BV abbreviates is was]
   [CNJ abbreviates and or]
   [ADJ abbreviates happy stupid]
   [MOD abbreviates very]
   [ADV abbreviates often always sometimes]] -> abbreviations;
