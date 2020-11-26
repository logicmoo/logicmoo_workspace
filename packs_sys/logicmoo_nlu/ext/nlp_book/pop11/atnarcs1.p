;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; atnarcs1.p [Chapter  3] Example ATN

vars networks;
[
S
[[Registers pps auxs mood mainverb arg0 arg1]
 [Initial 0           [true] [[] -> pps; [] -> auxs]]
 [Final 3             [true]
                      [list(mood,
                            list(mainverb,
                                 list("arg0", arg0, 2),
                                 list("arg1", arg1, 2),
                                 3) <> pps,
                            2)]]
 [From 0 to 1 by NP   [true] [star -> arg0; "add" -> mood]]
 [From 1 to 2 by V    [true] [star -> mainverb]]
 [From 2 to 2 by V    [true] [mainverb :: auxs -> auxs; star -> mainverb]]
 [From 2 to 3 by NP   [true] [star -> arg1]]
 [From 2 to 3 by #    [true] [[] -> arg1]]
 [From 3 to 3 by PP   [true]  [star :: pps -> pps]]]
NP
[[Registers res]
 [Initial 0           [true] []]
 [Final 1             [true] [res]]
 [From 0 to 1 by PN   [true] [star -> res]]]
PP
[[Registers p arg]
 [Initial 0           [true] []]
 [Final 2             [true] [list(p, arg, 2)]]
 [From 0 to 1 by P    [true] [star -> p]]
 [From 1 to 2 by NP   [true] [star -> arg]]]] -> networks;
[[PN abbreviates john mary susan peter]
 [P  abbreviates with behind]
 [V  abbreviates will see]] -> abbreviations;

define list(n) -> res;
   [] -> res;
   repeat n times conspair(res)->res endrepeat
enddefine;

vars atnarcs1; true -> atnarcs1;
