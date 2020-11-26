;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; eng_fre1.p [Chapter  2] Simple FST for English-French translation

vars eng_fre1;
 [[Initial 1]
   [Final 5]
   [From 1 to 2 by WHERE]
   [From 2 to 3 by BE]
   [From 3 to 4 by DET]
   [From 4 to 5 by NOUN]] -> eng_fre1;

[[WHERE abbreviates [where ou]]
 [BE abbreviates [is est]]
 [DET abbreviates [the #]]
 [NOUN abbreviates [exit 'la sortie'] [policeman 'le gendarme']
     [shop 'la boutique'] [toilet 'la toilette']]] -> abbreviations;
