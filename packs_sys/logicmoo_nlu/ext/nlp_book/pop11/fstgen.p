;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; fstgen.p [Chapter  2] Exhaustive generation of sentence pairs from a FST

uses finite;
uses fstape;

vars network;

define generate2_move(lab,tape);
   vars output1 output2 newoutput1 newoutput2 l1 l2 e exps;
   tape --> [?output1 ?output2];
   if lab matches [?l1 ?l2] then
      for newoutput1 in [% generate_move(l1,output1) %] do
         for newoutput2 in [% generate_move(l2,output2) %] do
            [^newoutput1 ^newoutput2]
         endfor
      endfor
   elseif lab = "#" then
      [^output1 ^output2]
   elseif abbreviations matches [== [^label abbreviates ??exps] ==] then
      for e in exps do
         generate2_move(e,tape)
      endfor
   endif
enddefine;

define generate2_next(node,tape,network);
   vars newnode label newtape output;
   if member(node,final_nodes(network)) then
      tape =>
   endif;
   foreach [From ^node to ?newnode by ?label]
      in transitions(network) do
      for newtape in [%generate2_move(label,tape)%] do
         generate2_next(newnode,newtape,network)
      endfor
   endforeach
enddefine;

define generate2(network);
   vars i;
   for i in initial_nodes(network) do
      generate2_next(i,[[] []],network)
   endfor
enddefine;

vars fstgen; true -> fstgen;
