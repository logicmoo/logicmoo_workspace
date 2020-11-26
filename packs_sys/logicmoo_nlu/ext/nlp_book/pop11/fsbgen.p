;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; fsbgen.p [Chapter  2] Breadth-first generation of sentences from a FSTN

uses finite;
uses fstape;

define generate(network);
   vars s states pos tape newpos label newtape;
   [% for s in initial_nodes(network) do
         [^s []]
      endfor %] -> states;
   until states = [] do
      [% for s in states do
            s --> [?pos ?tape];
            if member(pos,final_nodes(network)) then
               tape =>
            endif;
            foreach [From ^pos to ?newpos by ?label]
               in transitions(network) do
               for newtape in [%generate_move(label,tape)%] do
                  [^newpos ^newtape]
               endfor
            endforeach
         endfor %] -> states
   enduntil
enddefine;

vars fsbgen; true -> fsbgen;
