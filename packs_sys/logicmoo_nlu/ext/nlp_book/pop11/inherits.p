;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; inherits.p [Chapter  9] Simple semantic net with default inheritance

      [[attr club_member sex male]
       [attr club_member over_50 yes]
       [attr club_member citizenship US]

     [isa associate club_member]
       [attr associate associate_member yes]
       [attr associate citizenship non_US]

     [isa life_member club_member]
       [attr life_member life_member yes]
       [attr life_member over_50 no]

     [isa kim associate]
       [attr kim over_50 no]

     [isa jean associate]
       [attr jean sex female]
       [attr jean citizenship US]

     [isa mayumi life_member]
       [attr mayumi sex female]
       [attr mayumi over_50 yes]
       [attr mayumi citizenship non_US]

     [isa beryl life_member]
       [attr beryl sex female]] -> database;

define get_attr(entity1, attribute);
   vars entity2 value;
   if present([attr ^entity1 ^attribute ?value]) then
      value
   else
      foreach [isa ^entity1 ?entity2] do
         get_attr(entity2, attribute) -> value;
         if value then return(value) endif
      endforeach;
      false
   endif
enddefine;

vars inherits; true -> inherits;
