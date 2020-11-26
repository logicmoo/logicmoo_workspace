;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; fsrecog.p [Chapter  2] Finite state recognition

uses finite;
uses fstape;

vars network abbreviations;

vars recognize_next recognize_move;

define recognize(network, tape);
   vars i;
   for i in initial_nodes(network) do
       recognize_next(i, tape, network)
   endfor;
   false
enddefine;

define recognize_next(node, tape, network);
   vars newnode label newtape;
   if tape = [] and member(node, final_nodes(network)) then
      true;
      exitfrom(recognize)
   else
      foreach [From ^node to ?newnode by ?label] in transitions(network) do
         for newtape in [%recognize_move(label,tape)%] do
            recognize_next(newnode, newtape, network)
         endfor
      endforeach
   endif
enddefine;

vars fsrecog; true -> fsrecog;
