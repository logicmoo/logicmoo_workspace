;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; eng_fre2.lsp [Chapter  3] simple PT for English-French translation

(defvar networks)

(setq networks
 '((S
       ((Initial (0))
        (Final   (2))
        (From 0 to 1 by NP)
        (From 1 to 2 by VP)))
   (NP
       ((Initial (0))
        (Final   (2))
        (From 0 to 1 by DET-FEMN)
        (From 1 to 2 by N-FEMN)
        (From 0 to 4 by DET-MASC)
        (From 4 to 2 by N-MASC)
        (From 2 to 3 by WH)
        (From 3 to 2 by VP)))
   (VP
       ((Initial (0))
        (Final (1 2))
        (From 0 to 1 by V)
        (From 1 to 2 by NP)
        (From 1 to 3 by (that que))
        (From 3 to 2 by S)))))

(setq abbreviations
 '((N-MASC (man homme) (horse cheval))
   (N-FEMN (house maison) (table table))
   (NP (John Jean) (Mary Marie) (Jean Jeanne))
   (DET-MASC (a un) (the le) (this ce))
   (DET-FEMN (a une) (the la) (this cette))
   (V (sees voit) (hits frappe) (sings chante) (lacks manque))
   (WH (who qui) (which qui) (that qui))))
