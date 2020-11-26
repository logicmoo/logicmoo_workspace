
:- include(test_header).

:- begin_tests(mpred_pfc_test_02_plt).

:- include('mpred_pfc_test_03').

test:- mpred_reset.

:- defaultAssertMt(M),dynamic((M:current_ooQ1/1,M:default_ooQ1/1,M:if_mooQ1/2)).

test:- mpred_trace.

test:- mpred_watch.

test:- mpred_reset.


test:- notrace((mpred_ain(default_ooQ1(whenMissingQ1)))).

test:- must(call_u(default_ooQ1(whenMissingQ1))).

test:- defaultAssertMt(M),M:must((default_ooQ1(whenMissingQ1))).

test:- mpred_why(default_ooQ1(whenMissingQ1)).

test:- %rtrace
   (mpred_test(default_ooQ1(whenMissingQ1))).

test:- %rtrace
   (mpred_ain(\+default_ooQ1(whenMissingQ1))).
% this should have been ok
% (if_mooQ1(Missing,Create) ==> ((\+ Missing/(Missing\==Create), \+ Create , \+ ~(Create)) ==> Create)).

test:- ((mpred_ain((if_mooQ1(Missing,Create) ==> 
 ( ( \+ Missing/(Missing\=@=Create)) ==> Create))))).

test:- mpred_ain((default_ooQ1(X) ==> if_mooQ1(current_ooQ1(_),current_ooQ1(X)))).

test:- mpred_ain(default_ooQ1(whenMissingQ1)).

test:- mpred_test(current_ooQ1(whenMissingQ1)).
   
% :- pp_DB.

test:- (mpred_ain(current_ooQ1(fooQ1))).

test:- mpred_test(\+current_ooQ1(whenMissingQ1)).

test:- (mpred_ain(\+ current_ooQ1(fooQ1))).

test:- mpred_test(current_ooQ1(whenMissingQ1)).

test:- (mpred_withdraw( default_ooQ1(whenMissingQ1) )).

test:- listing([current_ooQ1,default_ooQ1]).

test:- mpred_test( \+current_ooQ1(whenMissingQ1)).

test:- mpred_ain(~ current_ooQ1(fooQ1)).

% :- pp_DB.

test:- mpred_test(~current_ooQ1(fooQ1)).

test:- mpred_ain(default_ooQ1(whenMissingQ1)).
 
test:- mpred_test(current_ooQ1(whenMissingQ1)).


test:- defaultAssertMt(M),dynamic((M:current_ooTt/1,M:default_ooTt/1,M:if_mooTt/2)).

test:- mpred_trace.
test:- mpred_watch.

% this should have been ok
% (if_mooTt(Missing,Create) ==> ((\+ Missing/(Missing\==Create), \+ Create , \+ ~(Create)) ==> Create)).
test:- mpred_ain((if_mooTt(Missing,Create) ==> 
 ( ( \+ Missing/(Missing\=@=Create)) ==> Create))).

test:- mpred_ain((default_ooTt(X) ==> if_mooTt(current_ooTt(_),current_ooTt(X)))).

test:- mpred_ain(default_ooTt(defaultValueTt)).

% :- make,check,use_module(library(pfc)),make,check.
:- end_tests(mpred_pfc_test_02_plt).

