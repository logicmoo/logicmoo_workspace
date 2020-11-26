:- include(test_header).




% =================================================================================
% Load the system
% =================================================================================
:- profile(ensure_loaded(library(logicmoo_user))).
:- make.

% :- set_prolog_flag(gc,false).

% =================================================================================
% Load the system options
% =================================================================================

:- set_lang(clif).
:- begin_pfc.


% deduce instances from usages in args having the effect of deducing human,dwelling,beverage_class are classes

==> feature_setting(make_wff,true).

==> feature_setting(add_admitted_arguments,true).

% set truth maintainance system to remove previous assertions that new assertions disagree with 
==> feature_setting(tms_mode,remove_conflicting).


:- set_prolog_flag(runtime_debug,3). % mention it when we remove previous assertions

:- set_prolog_flag_until_eof(do_renames,mpred_expansion).


:- kif_compile.



% =================================================================================
% Define a couple classes
% =================================================================================

% this creates the servant datatype
==> all(X, if(isa(X,servant),livesAt(X, green_house))).

% this creates the human datatype as well as functions that in the future that are create to accept humans must also accept servants 
==> all(X, if(livesAt(X, green_house),isa(X,human))).

% =================================================================================
% Test 1
% =================================================================================

:- mpred_trace_exec.


% ?- mpred_test(all(X,if(isa(X,servant),isa(X,human)))).

==> isa(fred,servant).  

:- mpred_test(isa(fred,human))

%?- all(X,t(OP,isa(X,human),~isa(X,servant))).
%OP = {A,B}/( if(~A,B) ).

:- mpred_test(all(X,if(isa(X,servant),isa(X,human)))). 
:- mpred_test(all(X,if(~isa(X,human),~isa(X,servant)))). 