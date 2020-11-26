;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; buparse2.p [Chapter  5] Bottom-up CF-PSG parser with less redundancy

define initial_segment(goals,string) -> needed -> others;
   vars goal restgoals item restitems goalcat;
   [] -> needed;
   while goals matches [?goal ??restgoals]
     and string matches [?item ??restitems]
     and ((goal matches [?goalcat] and item matches [^goalcat ==]) or
            item matches goal) do
         [^^needed ^item] -> needed;
         restgoals -> goals;
         restitems -> string
   endwhile;
   if goals = [] then
      string -> others
   else
      false -> others
   endif
enddefine;

define next(string,pos);
   vars left rule head body this right c others len;
   if length(string) = 1 then
      string(1) =>
      string(1)
   else
      [] -> left;
      length(string) -> len;
      while string matches [?this ??right] do
         min(pos,len) -> pos;
         for rule in rules do
               rule --> [[?head] ??body];
               initial_segment(body,string) -> needed -> others;
               if others and length(others) < pos then
                  next([^^left [^head ^^needed] ^^others],length(others)+1)
               endif
         endfor;
         [^^left ^this] -> left;
         right -> string;
         len-1 -> len;
         ;;; words heuristic
         if isword(this) then quitloop endif
      endwhile
   endif
enddefine;

define parse(string);
   [% next(string,length(string)) %]
enddefine;

vars buparse2; true -> buparse2;
