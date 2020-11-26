;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; finite.p [Chapter  2] Utilities for finite state networks

define initial_nodes(n) -> s;
   n --> [== [Initial ??s] ==]
enddefine;

define final_nodes(n) -> s;
   n --> [== [Final ??s] ==]
enddefine;

define transitions(n) -> t;
   n --> [= = ??t]
enddefine;

vars finite; true -> finite;
