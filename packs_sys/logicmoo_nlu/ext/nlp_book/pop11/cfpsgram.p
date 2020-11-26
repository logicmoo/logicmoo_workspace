;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; cfpsgram.p [Chapter  4] Example context free grammar in list format

  vars rules;
   [[[S] [NP] [VP]]
    [[VP] [V]]
    [[VP] [V] [NP]]
    [[V] died]
    [[V] employed]
    [[NP] nurses]
    [[NP] patients]
    [[NP] MediCenter]
    [[NP] Dr Chan]] -> rules;
