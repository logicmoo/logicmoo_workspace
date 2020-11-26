;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; pdtransd.p [Chapter  3] Pushdown transduction

uses fstrans;   ;;; for transduce_move

vars rtn_transduce rtn_transduce_move networks;
transduce_move -> rtn_transduce_move;

define get_network(name) -> network;
   networks --> [== ^name ?network ==]
enddefine;

define rtn_transduce_next(networkname,node,tape,stack);
   vars newnode label newtape output;
   vars newnetworkname newstack i;
   if member(node,final_nodes(get_network(networkname))) then
      if stack = [] and tape matches [[] ?output] then
         output;
         exitfrom(rtn_transduce)
      elseif stack matches [[?newnetworkname ?newnode] ??newstack] then
         rtn_transduce_next(newnetworkname,newnode,tape,newstack)
      endif
   endif;
   foreach [From ^node to ?newnode by ?label]
      in transitions(get_network(networkname)) do
      if member(label,networks) then
         for i in initial_nodes(get_network(label)) do
            rtn_transduce_next(label,i,tape,
                [[^networkname ^newnode] ^^stack])
         endfor
      endif;
      for newtape in
         [%rtn_transduce_move(label,tape)%] do
         rtn_transduce_next(networkname,
                       newnode,newtape,stack)
      endfor
   endforeach
enddefine;

define rtn_transduce(networkname,tape);
   vars i;
   for i in initial_nodes(get_network(networkname)) do
      rtn_transduce_next(networkname,i,[^tape []],[])
   endfor;
   false
enddefine;

vars pdtransd; true -> pdtransd;
