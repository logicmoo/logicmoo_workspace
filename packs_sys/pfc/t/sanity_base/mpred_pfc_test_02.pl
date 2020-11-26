
:- include(test_header).

% :- use_listing_vars.

% :- set_prolog_flag(umt_local,true).


% :- set_prolog_flag(umt_local,false).
% :- ensure_loaded(library(logicmoo/mpred/mpred_core)).
%:- include(test_header).
% :- include(test_header).
%:- include(test_header).

:- include('mpred_pfc_test_03').

:- mpred_reset.

:- defaultAssertMt(M),dynamic((M:current_ooQ1/1,M:default_ooQ1/1,M:if_mooQ1/2)).
:- mpred_trace.
:- mpred_watch.
:- mpred_reset.


:- notrace(rtrace(mpred_ain(default_ooQ1(whenMissingQ1)))).

:- must(call_u(default_ooQ1(whenMissingQ1))).

:- defaultAssertMt(M),M:must((default_ooQ1(whenMissingQ1))).

:- mpred_why(default_ooQ1(whenMissingQ1)).

:- %rtrace
   (mpred_test(default_ooQ1(whenMissingQ1))).

:- %rtrace
   (mpred_ain(\+default_ooQ1(whenMissingQ1))).
% this should have been ok
% (if_mooQ1(Missing,Create) ==> ((\+ Missing/(Missing\==Create), \+ Create , \+ ~(Create)) ==> Create)).
:- ((mpred_ain((if_mooQ1(Missing,Create) ==> 
 ( ( \+ Missing/(Missing\=@=Create)) ==> Create))))).

:- mpred_ain((default_ooQ1(X) ==> if_mooQ1(current_ooQ1(_),current_ooQ1(X)))).

:- mpred_ain(default_ooQ1(whenMissingQ1)).

:- mpred_test(current_ooQ1(whenMissingQ1)).
   
% :- pp_DB.

:- (mpred_ain(current_ooQ1(fooQ1))).

:- mpred_test(\+current_ooQ1(whenMissingQ1)).

:- (mpred_ain(\+ current_ooQ1(fooQ1))).

:- mpred_test(current_ooQ1(whenMissingQ1)).

:- (mpred_withdraw( default_ooQ1(whenMissingQ1) )).

:- listing([current_ooQ1,default_ooQ1]).

:- mpred_test( \+current_ooQ1(whenMissingQ1)).

:- mpred_ain(~ current_ooQ1(fooQ1)).

% :- pp_DB.

:- mpred_test(~current_ooQ1(fooQ1)).

:- mpred_ain(default_ooQ1(whenMissingQ1)).
 
:- mpred_test(current_ooQ1(whenMissingQ1)).


:- defaultAssertMt(M),dynamic((M:current_ooTt/1,M:default_ooTt/1,M:if_mooTt/2)).

:- mpred_trace.
:- mpred_watch.

% this should have been ok
% (if_mooTt(Missing,Create) ==> ((\+ Missing/(Missing\==Create), \+ Create , \+ ~(Create)) ==> Create)).
:- mpred_ain((if_mooTt(Missing,Create) ==> 
 ( ( \+ Missing/(Missing\=@=Create)) ==> Create))).

:- mpred_ain((default_ooTt(X) ==> if_mooTt(current_ooTt(_),current_ooTt(X)))).

:- mpred_ain(default_ooTt(defaultValueTt)).

% :- make,check,use_module(library(pfc)),make,check.

:- make,check.

:- wdmsg(warning(outdated_tests)).

end_of_file.


:- mpred_test(current_ooTt(defaultValueTt)).

:- (mpred_ain(current_ooTt(fooTt))).

:- mpred_test(\+current_ooTt(defaultValueTt)).

:- (mpred_ain(\+ current_ooTt(fooTt))).

:- mpred_test(current_ooTt(defaultValueTt)).

:- (mpred_withdraw( default_ooTt(defaultValueTt) )).

:- listing([current_ooTt,default_ooTt]).

:- mpred_test( \+current_ooTt(defaultValueTt)).

:- mpred_ain(~ current_ooTt(fooTt)).

%:- pp_DB.

:- mpred_test(~current_ooTt(fooTt)).

:- mpred_ain(default_ooTt(defaultValueTt)).



:- mpred_test(current_ooTt(defaultValueTt)).






:- must(mpred_reset).


:- if(current_module(mpred_loader)).
:- wdmsg("Already loaded mpred_loader!").
:- endif.


% local_testing


:- make,check,use_module(test_header),make,check.

