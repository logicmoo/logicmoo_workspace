% Empty with slipcover loaded
:-use_module(library(slipcover)).

:-sc.

% Settings
:- set_sc(verbosity,1).

:- begin_in.

% Input theory here


:- end_in.  

:- begin_bg.

% Background knowledge here

:- end_bg.

% Fold definition
% fold(train,[folds list]).
% fold(test,[[folds list]).


% Language bias


% Models / Examples



/** <examples> Your example queries go here, e.g.

?- induce_par([train],P),test(P,[test],LL,AUCROC,ROC,AUCPR,PR).

?- induce([train],P),test(P,[test],LL,AUCROC,ROC,AUCPR,PR).

*/
