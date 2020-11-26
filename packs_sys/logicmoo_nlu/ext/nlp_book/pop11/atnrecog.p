;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; atnrecog.p [Chapter  3] Parsing using an ATN

vars doactions atn_recognize_next dotests dopopactions diff_tape networks;
vars atn_recognize_pop atn_recognize_push atn_recognize_traverse;

uses fstape;    ;;; for tape-moving procedures

;;; network accessing

define initial_nodes(n) -> s;
   n --> [== [Initial ??s = =] ==]
enddefine;

define initial_tests(n) -> t;
   n --> [== [Initial == ?t =] ==]
enddefine;

define initial_actions(n) -> a;
   n --> [== [Initial == ?a] ==]
enddefine;

define final_nodes(n) -> s;
   n --> [== [Final ??s = =] ==]
enddefine;

define final_tests(n) -> t;
   n --> [== [Final == ?t =] ==]
enddefine;

define final_actions(n) -> a;
   n --> [== [Final == ?a] ==]
enddefine;

define transitions(n) -> t;
   n --> [= = = ??t]
enddefine;

define get_network(name) -> network;
   networks --> [== ^name ?network ==]
enddefine;

define regs_used(network) -> r;
   network --> [== [Registers ??r] ==]
enddefine;

define initial_regs(network);
   [% regs_used(network),
      [% for r in regs_used(network) do false endfor %]
   %]
enddefine;

define atn_recognize(networkname,tape);
   vars i regs hold;
   initial_regs(get_network(networkname)) -> regs;
   doactions(regs,initial_actions(get_network(networkname)),
                         false,false) -> regs -> hold;
   if regs then
      for i in initial_nodes(get_network(networkname)) do
         atn_recognize_next(networkname,i,tape,[],regs,hold)
      endfor
   endif;
   false
enddefine;

vars atn_recognize_move;
recognize_move -> atn_recognize_move;

;;; Try all ATN traversals starting at a given node

define atn_recognize_next(networkname, node, tape, stack, regs, hold);
   vars newnode label newtape star newregs tests actions newhold;
   if member(node,final_nodes(get_network(networkname))) then
      atn_recognize_pop(networkname, node, tape, stack, regs, hold)
   endif;
   foreach [From ^node to ?newnode by ?label ?tests ?actions]
    in transitions(get_network(networkname)) do
      if member(label,networks) then
         atn_recognize_push(label, networkname ,newnode, tape, stack, regs,
                                   hold, tests, actions)
      endif;
      atn_recognize_traverse(label, networkname, newnode, tape, stack, regs,
                                   hold, tests, actions)
   endforeach
enddefine;

;;; Try to POP from the current network and continue from the higher network

define atn_recognize_pop(networkname, node, tape, stack, regs, hold);
   vars star newhold newnetworkname newnode newregs tests actions newstack;
   if dotests(regs, final_tests(get_network(networkname)), hold, false) then
      dopopactions(regs, final_actions(get_network(networkname)),
                                              hold, false)
        -> star -> newhold;
      if stack == [] and tape == [] then
         star;
         exitfrom(atn_recognize)
      elseif stack matches
       [[?newnetworkname ?newnode ?newregs ?tests ?actions] ??newstack] then
         if dotests(newregs, tests, newhold, star) then
            doactions(newregs, actions, newhold, star)
                -> newregs -> newhold;
            atn_recognize_next(newnetworkname, newnode,
                 tape, newstack, newregs, newhold)
         endif
      endif
   endif
enddefine;

;;; Try to PUSH to a subnetwork and continue traversing from there

define atn_recognize_push(label, networkname, newnode, tape, stack, regs,
                               hold, tests, actions);
   vars newregs newhold i;
   if dotests(initial_regs(get_network(label)),
              initial_tests(get_network(label)),
              hold,false) then
      ;;; postpone tests in calling net to POP time
      initial_regs(get_network(label)) -> newregs;
      doactions(newregs, initial_actions(get_network(label)), hold, false)
          -> newregs -> newhold;
      for i in initial_nodes(get_network(label)) do
         atn_recognize_next(label, i,
            tape, [[^networkname ^newnode ^regs ^tests
                   ^actions] ^^stack], newregs, newhold)
      endfor
   endif
enddefine;

;;; Try to traverse a simple ATN arc and continue from its destination

define atn_recognize_traverse(label, networkname, newnode, tape, stack, regs,
                                       hold, tests, actions);
   vars newtape;
   for newtape in [%atn_recognize_move(label, tape)%] do
      diff_tape(newtape, tape) -> star;
      if dotests(regs, tests, hold, star) then
         doactions(regs, actions, hold, star) -> newregs -> newhold;
         atn_recognize_next(networkname, newnode, newtape, stack, newregs,
                                       newhold)
      endif
   endfor
enddefine;

define diff_tape(t1,t2);
   if t2 /= [] and tl(t2) = t1 then hd(t2)
   else false
   endif
enddefine;

define dopopactions(regs,actions,hold,star) -> result -> newhold;
   vars regnames regvalues;
   regs --> [?regnames ?regvalues];
   popval([procedure hold star ^^regnames; ^^actions; hold; endprocedure])
      (hold,star,dl(regvalues)) -> newhold -> result
enddefine;

define doactions(regs, actions, hold, star) -> newregs -> newhold;
   vars regnames regvalues newregvalues;
   regs --> [?regnames ?regvalues];
   popval([procedure hold star ^^regnames; ^^actions;
              maplist(^regnames,valof); hold; endprocedure])
     (hold,star,dl(regvalues)) -> newhold -> newregvalues;
   [^regnames ^newregvalues] -> newregs
enddefine;

define dotests(regs,tests,hold,star);
   vars regnames regvalues;
   regs --> [?regnames ?regvalues];
   popval([procedure hold star ^^regnames; ^^tests; endprocedure])
    (hold,star,dl(regvalues))
enddefine;

vars atnrecog; true -> atnrecog;
