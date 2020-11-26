;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; fstape.p [Chapter  2] Tape-moving procedures for finite state networks

;;; single tape moving for recognition

vars abbreviations;

define recognize_move(label, tape);
   if tape matches [^label ==] then tl(tape)
   elseif tape /= []
    and abbreviations matches [== [^label == ^(hd(tape)) ==] ==] then
      tl(tape)
   elseif label = "#" then tape
   else
      ;;; return nothing
   endif
enddefine;

;;; single tape moving for generation

define generate_move(label, tape);
   vars x exps;
   if label = "#" then tape
   elseif abbreviations matches [== [^label abbreviates ??exps] ==] then
      for x in exps do [^^tape ^x] endfor
   else
      [^^tape ^label]
   endif
enddefine;
vars fstape;
true -> fstape;
