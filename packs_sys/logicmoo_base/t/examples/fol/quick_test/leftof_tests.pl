

% There are five houses in a row.
:- mpred_test(
    exists(H1,exists(H2,exists(H3,exists(H4,exists(H5,
     (leftof(H1, H2) & leftof(H2, H3) & leftof(H3, H4) & leftof(H4, H5)))))))).

:- mpred_test(
    (leftof(H1, H2) & leftof(H2, H3) & leftof(H3, H4) & leftof(H4, H5))).


:- mpred_test(
    (leftof(h1, h2) & leftof(h2, h3) & leftof(h3, h4) & leftof(h4, h5))).


end_of_file.

% commented out in this_01 case but this_02  has it uncommented
:- if(false).
% This is the real test we care about here
:- interactive_test(pfclog_recompile).
:- endif.

:- interactive_test(listing(nesc)).



% ensure our rule worked
:- test_pfc(nesc(house(h1))).

% ensure we are being nice
:- test_pfc(poss(house(false_positive))).
% but not "too" nice
:- test_pfc(\+ nesc(house(false_positive))).

% lets invalidate at least one pair
~poss(house(h2)).

% if the above took effect
:- test_pfc(\+ nesc(house(h2))).

% we did invalidate the pair ?
:- test_pfc(\+ nesc(house(h1))).

% @TODO not sure what we want to invalidate the rest ?
:- test_pfc(ignore(\+ nesc(house(h5)))).

% ensure our rule worked
:- test_pfc(nesc(house(h1))).

% ensure we are being nice
:- test_pfc(poss(house(false_positive))).
% but not "too" nice
:- test_pfc(\+ nesc(house(false_positive))).

% lets invalidate at least one pair
~poss(house(h2)).

% if the above took effect
:- test_pfc(\+ nesc(house(h2))).

% we did invalidate the pair ?
:- test_pfc(\+ nesc(house(h1))).

% @TODO not sure what we want to invalidate the rest ?
:- test_pfc(ignore(\+ nesc(house(h5)))).

:- interactive_test(listing(nesc)).


