;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; atnarcs2.lsp [Chapter  3] extended ATN

(setq networks

 '(

 (S
   (
     (Registers (pps auxs mood mainverb arg0 arg1))
     (Initial (0)          t     ((setq pps ())(setq auxs ())(setq hold ())))
     (Final (3)  (null hold)     ((list mood
                                          (append
                                            (list mainverb
                                                  (list (quote arg0) arg0)
                                                  (list (quote arg1) arg1)
                                            )
                                            pps))))
     (From 0 to 4 by V t     ((setq mainverb star)))
     (From 4 to 2 by NP t     ((setq arg0 star) (setq mood (quote search))))
     (From 0 to 1 by NP t     ((setq arg0 star) (setq mood (quote add))))
     (From 1 to 4 by V (equal arg0 (quote ?))
                               ((setq mainverb star) (setq hold t)))
     (From 1 to 2 by V t     ((setq mainverb star)))
     (From 2 to 2 by V t     ((setq auxs (cons mainverb auxs))
                                        (setq mainverb star)))
     (From 2 to 3 by NP t     ((setq arg1 star)))
     (From 2 to 3 by |#| t     ((setq arg1 nil)))
     (From 3 to 3 by PP t     ((setq pps (cons star pps))))
   )
 )

 (NP
   (
     (Registers (res))
     (Initial (0)          t     ())
     (Final (1)            t     (res))
     (From 0 to 1 by WH t     ((setq res (quote ?))))
     (From 0 to 1 by PN t     ((setq res star)))
     (From 0 to 1 by |#| hold ((setq res (quote ?))(setq hold nil)))
   )
 )

 (PP
   (
     (Registers (p arg))
     (Initial (0)          t     ())
     (Final (2)            t     ((list p arg)))
     (From 0 to 1 by P t     ((setq p star)))
     (From 1 to 2 by NP t     ((setq arg star)))
   )
 )
 ))

(setq abbreviations
 '(
 (PN abbreviates john mary susan peter)
 (P abbreviates with behind)
 (V abbreviates will see)
 (WH abbreviates who what)
 ))
