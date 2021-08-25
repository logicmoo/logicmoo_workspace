


feature_test :-
	backinit,
	r 	:< range(anything),
	f1 	:< range(anything) type feature,
	f2	:< f1,
	f3	:< range(anything) type feature,
	f_23	:= f2.f3,
	rf	:= r.f1,
	range   :< anything,
	c_r	:= all(r,range),
	c_f1	:= all(f1,range),
	c_f2	:= all(f2,range),
	\+ c_r ?< atmost(1,r),
	c_f1 ?< atmost(1,f1),
	c_f2 ?< atmost(1,f2),
	all(f_23,range) ?< atmost(1,f_23),
	\+ all(rf,range) ?< atmost(1,rf),
	!,
	write('+++ feature test succeeded'),nl.


feature_test :-
	write('--- feature test failed'),nl.

