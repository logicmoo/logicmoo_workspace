:- include(test_header).

:- process_this_script.

% Initially Dmiles thought LEM was the culprit, it was not.
% this is a more generalized problem in Nomics


*/
% ==============================================================
% Section 3: Decidablity?
% ==============================================================

% Is the existence of cute puppies really even knowable/decidable in an "Open World"?

% Rule 4: Cute puppies exists or not (LEM)
nesc(exists(X,cute_puppy(X)) v ~exists(X,cute_puppy(X))).

% ~exists(X,



