;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; buparse1.p [Chapter  5] Bottom-up parsing for a CF-PSG

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

define next(string);
   vars left rule head body this right c others;
   if length(string) = 1 then
      string(1) =>
      string(1)
   else
      [] -> left;
      while string matches [?this ??right] do
         for rule in rules do
               rule --> [[?head] ??body];
               initial_segment(body,string) -> needed -> others;
               if others then
                  next([^^left [^head ^^needed] ^^others])
               endif
         endfor;
         [^^left ^this] -> left;
         right -> string
      endwhile
   endif
enddefine;

define parse(string);
   [% next(string) %]
enddefine;

vars buparse1; true -> buparse1;
