% Empty with phil loaded
:-use_module(library(phil)).

:-sc.

% Settings
:- set_sc_hplp(verbosity,1).

:- begin_in.

% Input theory here


:- end_in.  

:- begin_bg.

% Background knowledge here

:- end_bg.

% Fold definition
% fold_hplp(train,[folds list]).
% fold_hplp(test,[[folds list]).


% Language bias


% Models / Examples



/** <examples> Your example queries go here, e.g.

?- induce_hplp_par([train],P),test_hplp(P,[test],LL,AUCROC,ROC,AUCPR,PR).

?- induce_hplp([train],P),test_hplp(P,[test],LL,AUCROC,ROC,AUCPR,PR).

*/
