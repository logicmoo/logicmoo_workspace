;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; fsgen.p [Chapter  2] Exhaustive generation of sentences from a FSTN

uses finite;
uses fstape;

vars network;

define generate_next(node, tape, network);
   vars newnode label newtape;
   if member(node, final_nodes(network)) then
      tape =>
   endif;
   foreach [From ^node to ?newnode by ?label] in transitions(network) do
      for newtape in [%generate_move(label, tape)%] do
         generate_next(newnode, newtape, network)
      endfor
   endforeach
enddefine;

define generate(network);
   vars i;
   for i in initial_nodes(network) do
      generate_next(i, [], network)
   endfor
enddefine;

vars fsgen; true -> fsgen;
