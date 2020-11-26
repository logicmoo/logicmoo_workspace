;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; eng_fre1.lsp [Chapter  2] simple FST for English-French translation

(setq eng_fre1
 '((Initial (1))
   (Final (5))
    (From 1 to 2 by WH)
    (From 2 to 3 by BE)
    (From 3 to 4 by DET)
    (From 4 to 5 by NOUN)))

(setq abbreviations
 '((WH (where ou))
   (BE (is est))
   (DET (the |#|))
   (NOUN (exit la_sortie) (policeman le_gendarme)
         (shop la_boutique) (toilet la_toilette))))                
