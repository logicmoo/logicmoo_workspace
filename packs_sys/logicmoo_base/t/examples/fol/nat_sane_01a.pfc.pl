

% :- module(kbii,[]).

:- if(set_prolog_flag(os_argv,[swipl, '-f', '/dev/null','--nonet'])).
:- endif.

:- include(test_header).

%:- set_prolog_flag(retry_undefined,false).

%:- ensure_loaded(library(script_files)).


%:- ensure_abox(kbii).
%:- set_fileAssertMt(kbii).


% :- process_this_script.

:- break.

:- set_kif_option(+assert).


:- cls.      

% ============================================================
% English:  "For all ?x that is a Nat, there exists a successor ?y which is a Nat"
% Also: "No ?y may not be Nat if it is the successor of an ?x Nat"
%
% {?x a Nat} => {?x suc ?y. ?y a Nat}
% ============================================================
:- test_boxlog([+assert,+comingle],
   all(X,
     exists(Y,
      implies(isa(X,nat),
        and(succ(X,Y),isa(Y,nat)))))).

% ============================================================
% English: Zero is a Nat
%
% {0 a Nat} 
% ============================================================
:- test_boxlog([+assert],isa(0,nat)).

