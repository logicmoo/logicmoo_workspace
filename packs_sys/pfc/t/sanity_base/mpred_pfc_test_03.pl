%  was_module(red_test03,[]).

:- include(test_header).
% :- use_listing_vars.


:- begin_pfc.


% :- (( must((defaultAssertMt(M)->M==red_test01)))).

% :- set_prolog_flag(umt_local,true).


% :- set_prolog_flag(umt_local,false).
% :- ensure_loaded(library(logicmoo/mpred/mpred_core)).
% :- include(test_header).
%:- include(test_header).
% :- include(test_header).

% :- rtrace(mpred_reset).


:- defaultAssertMt(M),dynamic((M:current_ooQ2/1,M:default_ooQ2/1,M:if_mooQ2/2)).
:- mpred_trace.
:- mpred_watch.
%:- mpred_reset.


:- mpred_ain(default_ooQ2(booQ2)).

:- must(call_u(default_ooQ2(booQ2))).

:- mpred_why(default_ooQ2(booQ2)).

:- mpred_test(default_ooQ2(booQ2)).

:- mpred_ain(\+default_ooQ2(booQ2)).
% this should have been ok
% (if_mooQ2(Missing,Create) ==> ((\+ Missing/(Missing\==Create), \+ Create , \+ ~(Create)) ==> Create)).
:- ((mpred_ain((if_mooQ2(Missing,Create) ==> 
 ( ( \+ Missing/(Missing\=@=Create)) ==> Create))))).

:- mpred_ain((default_ooQ2(X) ==> if_mooQ2(current_ooQ2(_),current_ooQ2(X)))).

:- mpred_ain(default_ooQ2(booQ2)).

:- mpred_test(current_ooQ2(booQ2)).
   
% :- pp_DB.

:- (mpred_ain(current_ooQ2(fooQ2))).

:- mpred_test(\+current_ooQ2(booQ2)).

:- (mpred_ain(\+ current_ooQ2(fooQ2))).

:- mpred_test(current_ooQ2(booQ2)).

:- (mpred_withdraw( default_ooQ2(booQ2) )).

:- listing_u([current_ooQ2,default_ooQ2]).

:- mpred_test( \+current_ooQ2(booQ2)).

:- mpred_ain(~ current_ooQ2(fooQ2)).

% :- pp_DB.

:- mpred_test(~current_ooQ2(fooQ2)).

:- mpred_ain(default_ooQ2(booQ2)).
 
:- mpred_test(current_ooQ2(booQ2)).


:- defaultAssertMt(M),dynamic((M:current_ooTt/1,M:default_ooTt/1,M:if_mooTt/2)).

:- mpred_trace.
:- mpred_watch.

% this should have been ok
% (if_mooTt(Missing,Create) ==> ((\+ Missing/(Missing\==Create), \+ Create , \+ ~(Create)) ==> Create)).
:- mpred_ain((if_mooTt(Missing,Create) ==> 
 ( ( \+ Missing/(Missing\=@=Create)) ==> Create))).

:- mpred_ain((default_ooTt(X) ==> if_mooTt(current_ooTt(_),current_ooTt(X)))).

:- mpred_ain(default_ooTt(defaultValueTt)).

:- mpred_test(current_ooTt(defaultValueTt)).

:- (mpred_ain(current_ooTt(fooTt))).

:- mpred_test(\+current_ooTt(defaultValueTt)).

:- (mpred_ain(\+ current_ooTt(fooTt))).

:- mpred_test(current_ooTt(defaultValueTt)).

:- (mpred_withdraw( default_ooTt(defaultValueTt) )).

:- listing_u([current_ooTt,default_ooTt]).

:- mpred_test( \+current_ooTt(defaultValueTt)).

:- mpred_ain(~ current_ooTt(fooTt)).

%:- pp_DB.

:- mpred_test(~current_ooTt(fooTt)).

:- mpred_ain(default_ooTt(defaultValueTt)).

:- mpred_test(current_ooTt(defaultValueTt)).





:- on_x_rtrace(mpred_reset).


:- if(current_module(mpred_loader)).
:- wdmsg("Already loaded mpred_loader!").
:- endif.


% local_testing


