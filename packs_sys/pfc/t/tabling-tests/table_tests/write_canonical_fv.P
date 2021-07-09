:- import numbervars/1 from num_vars.

rewrite_for_diff:- 
       [test_forest_view_new],
       fail.
rewrite_for_diff:-
	tell(temp),
	fail.
rewrite_for_diff:- 
       tc(A,B,C,D),
       numbervars(tc(A,B,C,D)),
       writeq(tc(A,B,C,D)),nl,
       fail.
rewrite_for_diff:- 
       nc(A,B,C,D),
       numbervars(nc(A,B,C,D)),
       writeq(nc(A,B,C,D)),nl,
       fail.
%rewrite_for_diff:- 
%       ar(A,B,C),
%       numbervars(ar(A,B,C)),
%       writeq(ar(A,B,C)),nl,
%       fail.
rewrite_for_diff:- 
       na(A,B,C),
       numbervars(na(A,B,C)),
       writeq(na(A,B,C)),nl,
       fail.
rewrite_for_diff:- 
       nda(A,B,C,D),
       numbervars(nda(A,B,C,D)),
       writeq(nda(A,B,C,D)),nl,
       fail.
rewrite_for_diff:- 
       cmp(A,B,C),
       numbervars(cmp(A,B,C)),
       writeq(cmp(A,B,C)),nl,
       fail.
rewrite_for_diff:- told.