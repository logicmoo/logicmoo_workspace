
:- use_module(tfv_1,[p_m/1]).

:- import start_forest_view/1, stop_forest_view/0, set_forest_logging_for_pred/2  from tables.
%?- start_forest_view(test_forest_view_new).
?- start_forest_view(userout).

:- table p/1, q/1, r/1.

p(X):- q(X).
q(X):- r(X).

r(1). 

test:- p(X),fail.
test:- writeln('---------------------------'),fail.
test:- set_forest_logging_for_pred(q/1,off),fail.
%test:- abolish_all_tables,fail.
test:- abolish_table_pred(p/1),abolish_table_pred(q/1),abolish_table_pred(r/1),fail.
test:- writeln('---------------------------'),fail.
test:- p(X),fail.
test:- writeln('---------------------------'),fail.
test:- p(X),fail.
test:- writeln('---------------------------'),fail.
test:- p_m(X),fail.
test:- writeln('---------------------------'),fail.
test:- set_forest_logging_for_pred(tfv_1:q_m/1,off),fail.
test:- abolish_table_pred(tfv_1:p_m/1),abolish_table_pred(tfv_1:q_m/1),abolish_table_pred(tfv_1:r_m/1),fail.
%test:- abolish_all_tables,fail.
test:- writeln('---------------------------'),fail.
test:- p_m(X),fail.
test.
