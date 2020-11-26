;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; scripts.p [Chapter 10] Script matching

;;; the global variable SCRIPTS holds the list of scripts

;;; Example value:

vars scripts;
[
 [[auto_buy _Customer _Auto1 _Auto2 _Driver _Garage]
  [goes _Customer _Garage]
  [test_drives _Customer _Auto1]
  [orders _Customer _Auto2 _Driver]
  [delivers _Driver _Auto2 _Customer]
  [drives _Customer _Auto2]]

 [[hat_buy _Customer _Hat _Assistant _Store]
  [goes _Customer _Store]
  [tries_on _Customer _Hat]
  [buys _Customer _Hat _Assistant]
  [delivers _Assistant _Hat _Customer]
  [wears _Customer _Hat]]
] -> scripts;

uses subst;
uses tunify;

vars predict_all_ways;

;;; SCRIPT_MATCH is given a list of propositions, eg.
;;;    [[tries_on fred hat7][wears _x _y]]
;;; It tries to match them in the order given to the
;;; expectations expressed in a script
;;; SCRIPT_MATCH stops after finding a first solution

define script_match(story);
   vars script substitutions header predictions subst;
   for script in scripts do
      script --> [?header ??predictions];
      predict_all_ways(predictions, story, empty_subst) -> substitutions;
      if substitutions matches [?subst ==] then
         return(apply_subst(subst, script))
      endif
   endfor;
   false;
   [no script matches]=>
enddefine;

;;; test to see whether a list of predictions can be matched with a story.
;;; For each possible way, a corresponding substitution is returned

define predict_all_ways(predictions, story, current_subst);
   vars subst event reststory predict restpredictions successes;
   if story matches [?event ??reststory] then
      [] -> successes;
      predictions -> restpredictions;
      while restpredictions matches [?predict ??restpredictions] do
         termunify(event, predict) -> subst;
         if subst then
            predict_all_ways(apply_subst(subst, restpredictions),
                             apply_subst(subst, reststory),
                             compose_substs(subst, current_subst))
            <> successes -> successes
         endif
      endwhile;
      successes
   else
      [^current_subst]
   endif
enddefine;
