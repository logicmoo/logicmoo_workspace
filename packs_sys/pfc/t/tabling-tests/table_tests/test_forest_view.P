

:- import start_forest_view/1, stop_forest_view/0  from tables.
?- start_forest_view(test_forest_view_new).
%?- start_forest_view(userout).

:- table p1/0, p2/0, p3/0, p4/0.

test:- p1,fail.
test:- writeln('%-------------'),fail.
test:- r1,fail.
test:- writeln('%-------------'),fail.
test:- delay_test,fail.
test:- writeln('%-------------'),fail.
test:- ar1(_X),fail.
test:- writeln('%-------------'),fail.
test:- dar1(_X),fail.
test:- writeln('%-------------'),fail.
test:- del1(1),fail.
test:- writeln('%-------------'),fail.
test:- del1(3.123456789),fail.
test:- writeln('%-------------'),fail.
test:- puc_simpl_1(1),fail.
test:- writeln('%-------------'),fail.
test:- ns_simpl_1(1),fail.
%test:- writeln('%-------------'),fail.
%test:- ans_compl_1(1),fail.
test:- stop_forest_view.

p1:- tnot(p2),p3.

p2:- p4.
p3.
p4:- fail.

:- table r1/0, r2/0, r3/0.

r1:- tnot(r2).
r2:- tnot(r3).
r3.

:- table delay_test/0,delay_test_1/2.
delay_test:- delay_test_1(_A,_B).

delay_test_1(1,_X):- undefined.

:- table ar1/1, ar2/1.
ar1(X):- ar2(X).
ar1(1).
ar2(X):- ar1(X).
ar2(2).

:- table dar1/1, dar2/1.
dar1(X):- dar2(X).
dar1(1):- undefined.
dar2(X):- dar1(X).
dar2(2):- undefined.

:- table del1/1, del2/1.

del1(X):- tnot(del2(X)).
del2(X):- tnot(del1(X)).

:- table puc_simpl_1/1, puc_simpl_2/1, puc_simpl_3/1.

puc_simpl_1(X):- tnot(puc_simpl_2(X)),fail.

puc_simpl_2(X):- tnot(puc_simpl_1(X)).
puc_simpl_2(X):- puc_simpl_3(X).

puc_simpl_3(X):- puc_simpl_2(X).

%-------------------------

:- table ns_simpl_1/1, ns_simpl_2/1, ns_simpl_3/1, ns_simpl_4/1.

ns_simpl_1(X):- tnot(ns_simpl_2(X)),fail.

ns_simpl_2(X):- ns_simpl_3(X).
ns_simpl_2(X):- tnot(ns_simpl_1(X)).
ns_simpl_2(X):- ns_simpl_4(X).

ns_simpl_3(X):- tnot(ns_simpl_2(X)).

ns_simpl_4(X):- ns_simpl_3(X).
%-------------------------
% Test of answer support.

:- table ans_compl_1/1, ans_compl_2/1, ans_compl_3/1, ans_compl_4/1.

ans_compl_1(X):- tnot(ans_compl_2(X)),fail.

ans_compl_2(X):- ans_compl_3(X).
ans_compl_2(X):- tnot(ans_compl_1(X)).

ans_compl_3(X):- ans_compl_4(X).
ans_compl_3(X):- tnot(ans_compl_2(X)).

ans_compl_4(X):- ans_compl_3(X).

end_of_file.

