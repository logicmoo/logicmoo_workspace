;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; fstrans.p [Chapter  2] Finite state transduction

uses finite;
uses fstape;

vars transduce network;

define transduce_move(label, tape);
   vars input output newinput newoutput l1 l2 e exps;
   tape --> [?input ?output];
   if label matches [?l1 ?l2] then
      [% recognize_move(l1, input) %] -> newinput;
      unless newinput = [] then
         hd(newinput) -> newinput;
         ;;; there is only one possibility
         for newoutput in [% generate_move(l2, output) %] do
            [^newinput ^newoutput]
         endfor
      endunless
   elseif label = "#" then
      tape
   elseif abbreviations matches [== [^label abbreviates ??exps] ==] then
      for e in exps do
         transduce_move(e, tape)
      endfor
   endif
enddefine;

define transduce_next(node, tape, network);
   vars newnode label newtape output;
   if tape matches [[] ?output] and member(node, final_nodes(network)) then
      output;
      exitfrom(transduce)
   else
      foreach [From ^node to ?newnode by ?label] in transitions(network) do
         for newtape in [%transduce_move(label, tape)%] do
             transduce_next(newnode, newtape, network)
         endfor
      endforeach
   endif
enddefine;

define transduce(network, tape);
   vars i;
   for i in initial_nodes(network) do
      transduce_next(i, [^tape []], network)
   endfor;
   false
enddefine;

vars fstrans; true -> fstrans;
