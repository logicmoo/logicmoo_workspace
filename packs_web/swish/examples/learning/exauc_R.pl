:-use_module(library(auc)).
:-use_module(library(cplint_r)).

main(AUCROC, AUCPR) :-
  compute_areas_diagrams_r(
    % list of couples prob-example where example is an atom for positive 
    % examples and \+(atom) for negative examples
    [0.7 - a, 0.7 - a, 0.7 - \+(a), 0.6 - a, 
     0.6 - \+(a), 0.5 - a, 0.4 - \+(a)],
    AUCROC, AUCPR).


/** <examples>

?- main(AUCROC, AUCPR). 

*/
