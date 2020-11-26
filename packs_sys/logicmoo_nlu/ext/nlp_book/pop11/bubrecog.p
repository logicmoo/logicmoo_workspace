;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; bubrecog.p [Chapter  5] Bottom-up, breadth-first recognition for a CF-PSG

define recognize(string);
   vars alternatives left rule head body this right others;
   [^string] -> alternatives;
   until alternatives = [] do
      [%
         for string in alternatives do
            if length(string) = 1 then
               string =>
            else
               [] -> left;
               while string matches [?this ??right] do
                  for rule in rules do
                     rule --> [?head ??body];
                     if string matches [^^body ??others] then
                        [^^left ^head ^^others]
                     endif
                  endfor;
                  [^^left ^this] -> left;
                  right -> string
               endwhile
            endif
         endfor
      %] -> alternatives;
  enduntil
enddefine;

vars bubrecog; true -> bubrecog;
