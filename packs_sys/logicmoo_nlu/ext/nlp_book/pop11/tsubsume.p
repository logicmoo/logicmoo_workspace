;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; tsubsume.p [Chapter  7] Subsumption for terms

vars tersubs1;

define termsubsumes(t1,t2);
   vars var_map;
   [] -> var_map;
   tersubs1(t1,t2)
enddefine;

define tersubs1(t1,t2);
   vars e1 e2;
   if t1 = t2 then
      true
   elseif isvar(t1) then
      if var_map matches [== [^t1 ?val1] ==] then
         val1 = t2
      else
         [^t1 ^t2]::var_map -> var_map;
         true
      endif
   elseif islist(t1) and islist(t2) and length(t1) = length(t2) then
      until t1 = [] do
         t1 --> [?e1 ??t1];
         t2 --> [?e2 ??t2];
         unless tersubs1(e1,e2) then return(false) endunless
      enduntil;
      true
   else
      false
   endif
enddefine;

vars tsubsume; true -> tsubsume;
