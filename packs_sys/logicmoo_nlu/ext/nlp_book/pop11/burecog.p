;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; burecog.p [Chapter  5] Bottom-up recognition for a CF-PSG

define next(string);
   vars left rule head body this right others;
   if string matches [[S]] then
      [yes] =>
   else
      [] -> left;
      while string matches [?this ??right] do
         for rule in rules do
            rule --> [?head ??body];
            if string matches [^^body ??others] then
               next([^^left ^head ^^others])
            endif
         endfor;
         [^^left ^this] -> left;
         right -> string
      endwhile
   endif
enddefine;

vars burecog; true -> burecog;
