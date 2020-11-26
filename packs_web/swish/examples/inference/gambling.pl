:- use_module(library(mcintyre)).
:- table comb/2.
:- mc.

:- begin_lpad.

black(_):0.5.
angle(_,A):uniform_dens(A,0,2*pi).

comb(0,a):- black(0), angle(0,A), A < pi.
comb(0,b):- black(0), angle(0,A), A >= pi.
comb(0,c):- \+black(0), angle(0,A), A < pi.
comb(0,d):- \+black(0), angle(0,A), A >= pi.

comb(s(X),a):-comb(X,_), black(X),black(s(X)), angle(s(X),A), A < pi.
comb(s(X),b):-comb(X,_), black(X),black(s(X)), angle(s(X),A), A >= pi.
comb(s(X),c):-comb(X,_), black(X),\+black(s(X)), angle(s(X),A), A < pi.
comb(s(X),d):-comb(X,_),black(X),\+black(s(X)), angle(s(X),A), A >= pi.

at_least_one_comb_a:- abolish_all_tables,comb(_,a).
never_comb_a:- \+ at_least_one_comb_a. 


:- end_lpad.

/** <examples>
?-mc_sample(at_least_one_comb_a,1000,P).
?-mc_sample(never_comb_a,1000,P).

*/
