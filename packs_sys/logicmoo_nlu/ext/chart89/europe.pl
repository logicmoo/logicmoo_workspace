% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% europe.pl [Chapter  9] Example rulebase for inference, etc.
%

border(portugal,spain).
border(spain,andorra).
border(spain,france).
border(andorra,france).
border(france,luxembourg).
border(france,belgium).
border(france,germany).
border(france,switzerland).
border(france,italy).
border(belgium,netherlands).
border(luxembourg,belgium).
border(belgium,germany).
border(luxembourg,germany).
border(germany,switzerland).
border(germany,austria).
border(switzerland,austria).
border(switzerland,italy).
border(austria,italy).
borders(Country1,Country2)  :- border(Country1,Country2);
                               border(Country2,Country1).
