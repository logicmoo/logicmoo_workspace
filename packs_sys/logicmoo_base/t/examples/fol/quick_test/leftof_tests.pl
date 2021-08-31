

% There are five houses in a row.
:- mpred_test(
    exists(H1,exists(H2,exists(H3,exists(H4,exists(H5,
     (leftof(H1, H2) & leftof(H2, H3) & leftof(H3, H4) & leftof(H4, H5)))))))).

:- mpred_test(
    (leftof(H1, H2) & leftof(H2, H3) & leftof(H3, H4) & leftof(H4, H5))).


:- mpred_test(
    (leftof(h1, h2) & leftof(h2, h3) & leftof(h3, h4) & leftof(h4, h5))).


