;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; fburecog.p [Chapter  7] Bottom-up recognition for PATR grammars

uses pop_patr;

vars category rules lexical_rules;

define next(string,pos);
   vars left r LHS RHS this right c needed others substitution len r words;
   if length(string) = 1 then
      [found a ^^string] =>
      category(string(1),empty_subst)
   else
      [] -> left;
      length(string) -> len;
      while string matches [?this ??right] do
         min(pos,len) -> pos;
         if isword(this) then
            if length(right) < pos then
               for r in lexical_rules do
                  rhs_match(string,r) -> substitution -> others ;
                  if substitution then
                     next([^^left ^^others],length(others))
                  endif
               endfor
            endif;
            quitloop
         else
            for r in rules do
               rhs_match(string,r) -> substitution -> others;
               if substitution and length(others)-1 < pos then
                  next(apply_subst(substitution,[^^left ^^others]),
                       length(others))
               endif
            endfor
         endif;
         [^^left ^this] -> left;
         right -> string;
         len-1 -> len
      endwhile
   endif
enddefine;

;;; return the list of categories for the successful recognitions

define recognize(string);
   [% next(string,length(string)) %]
enddefine;

vars fburecog; true -> fburecog;
