;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; rtnrecog.p [Chapter  3] Recognition using RTNs

uses finite;
uses fstape;

vars rtn_recognize_move rtn_recognize_next networks network get_network;
vars rtn_recognize_pop rtn_recognize_push rtn_recognize_traverse;

define rtn_recognize(networkname, tape);
   vars i;
   for i in initial_nodes(get_network(networkname)) do
       rtn_recognize_next(networkname, i, tape,[])
   endfor;
   false
enddefine;

define get_network(name) -> network;
   networks --> [== ^name ?network ==]
enddefine;

define rtn_recognize_next(networkname, node, tape, stack);
   vars newnode label;
   if member(node,final_nodes(get_network(networkname))) then
      rtn_recognize_pop(networkname, node, tape, stack);
   endif;
   foreach [From ^node to ?newnode by ?label] in transitions(get_network(networkname)) do
      if member(label, networks) then
         rtn_recognize_push(label, networkname, newnode, tape, stack)
      endif;
      rtn_recognize_traverse(label, networkname, newnode, tape, stack)
   endforeach
enddefine;

define rtn_recognize_traverse(label,networkname,newnode,tape,stack);
   vars newtape;
   for newtape in [%rtn_recognize_move(label,tape)%] do
      rtn_recognize_next(networkname,newnode,newtape,stack)
   endfor
enddefine;

define rtn_recognize_pop(networkname,node,tape,stack);
   vars newnetworkname newnode newstack;
   if stack = [] and tape = [] then
      true;
      exitfrom(rtn_recognize)
   elseif stack matches [[?newnetworkname ?newnode] ??newstack] then
      rtn_recognize_next(newnetworkname,newnode,tape,newstack)
   endif
enddefine;

define rtn_recognize_push(label,networkname,newnode,tape,stack);
   vars i;
   for i in initial_nodes(get_network(label)) do
      rtn_recognize_next(label,i,tape,[[^networkname ^newnode] ^^stack])
   endfor
enddefine;

recognize_move -> rtn_recognize_move;

vars rtnrecog; true -> rtnrecog;
