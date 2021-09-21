#!env swipl

% :- module(kbii,[]).

:- include(test_header).

:- set_prolog_flag(retry_undefined,false).

:- ensure_loaded(library(script_files)).


%:- ensure_abox(kbii).
:- set_fileAssertMt(kbii).


% :- process_this_script.


:- set_prolog_flag(os_argv,[swipl, '-f', '/dev/null','--nonet']).


:- cls.



% autoloading script_files:read_line_to_string/2 from /home/prologmud_server/lib/swipl-7.5.15/library/readutil
% ================================================================================================================
% Exactly 1 puppy
% ================================================================================================================



:- test_boxlog(exactly(1, X, puppy(X))).

 /*
% autoloading make:maplist/2 from /home/prologmud_server/lib/swipl-7.5.15/library/apply
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/boxlog_sanity_04.pfc.pl:32
% :- test_boxlog(exactly(1, X_VAR, puppy(X_VAR))).

proven_neg(puppy(X)) :-
        dif_objs(X, Puppy1),
        nesc(puppy(Puppy1)).
proven_neg(different(X, Puppy1)) :-
        nesc(puppy(Puppy1)),
        nesc(puppy(X)).
proven_tru(puppy(Puppy1)) :-
        skolem(Puppy1, count(1, inf, skF(skIsPuppyExists_0FnSk, vv(KB, Puppy1))), 1).
make_existential(Puppy1, count(1, inf, skF(skIsPuppyExists_0FnSk, vv(KB, Puppy1))), 1) :-
        ensure_cond(Puppy1, puppy(Puppy1)).
% c_success(kbii, kbii:call_u(kbii:test_boxlog(exactly(1, X, puppy(X))))).
*/


% ================================================================================================================
% At most 1 puppy
% ================================================================================================================



:- test_boxlog(atmost(1, X, puppy(X))).

 /*
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/boxlog_sanity_04.pfc.pl:55
% :- test_boxlog(atmost(1, X_VAR, puppy(X_VAR))).
proven_neg(puppy(X)) :-
        dif_objs(X, Puppy0),
        nesc(puppy(Puppy0)).
proven_neg(different(X, Puppy0)) :-
        nesc(puppy(X)),
        nesc(puppy(Puppy0)).
% c_success(kbii, kbii:call_u(kbii:test_boxlog(atmost(1, X, puppy(X))))).
*/



% ================================================================================================================
% At Least 1 puppy
% ================================================================================================================



:- test_boxlog(atleast(1, X, puppy(X))).

 /*
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/boxlog_sanity_04.pfc.pl:75
% :- test_boxlog(atleast(1, X_VAR, puppy(X_VAR))).
proven_tru(puppy(X)) :-
        skolem(X, count(1, inf, skF(skIsPuppyX_0FnSk, vv(KB, X))), 1).
make_existential(X, count(1, inf, skF(skIsPuppyX_0FnSk, vv(KB, X))), 1) :-
        ensure_cond(X, puppy(X)).
% c_success(kbii, kbii:call_u(kbii:test_boxlog(atleast(1, X, puppy(X))))).
*/




% ================================================================================================================
% Exactly 5 puppies
% ================================================================================================================



:- test_boxlog(exactly(5, X, puppy(X))).

 /*
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/boxlog_sanity_04.pfc.pl:93
% :- test_boxlog(exactly(5, X_VAR, puppy(X_VAR))).
proven_neg(puppy(Puppy1)) :-
        nesc(puppy(Puppy2)),
        dif_objs(Puppy1, Puppy2),
        nesc(puppy(Puppy3)),
        dif_objs(Puppy1, Puppy3),
        nesc(puppy(Puppy4)),
        dif_objs(Puppy1, Puppy4),
        nesc(puppy(Puppy5)),
        dif_objs(Puppy1, Puppy5),
        dif_objs(Puppy1, Puppy6),
        nesc(puppy(Puppy6)).
proven_neg(different(Puppy1, Puppy6)) :-
        nesc(puppy(Puppy6)),
        nesc(puppy(Puppy5)),
        dif_objs(Puppy1, Puppy5),
        nesc(puppy(Puppy4)),
        dif_objs(Puppy1, Puppy4),
        nesc(puppy(Puppy1)),
        nesc(puppy(Puppy2)),
        dif_objs(Puppy1, Puppy2),
        dif_objs(Puppy1, Puppy3),
        nesc(puppy(Puppy3)).
proven_tru(puppy(X)) :-
        skolem(X, count(5, inf, skF(skIsPuppyX_0FnSk, vv(KB, X))), _38323838).
make_existential(X, count(5, inf, skF(skIsPuppyX_0FnSk, vv(KB, X))), _38323838) :-
        ensure_cond(X, puppy(X)).
% c_success(kbii, kbii:call_u(kbii:test_boxlog(exactly(5, X, puppy(X))))).
*/



% ================================================================================================================
% At most 5 puppies
% ================================================================================================================



:- test_boxlog(atmost(5, X, puppy(X))).

 /*
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/boxlog_sanity_04.pfc.pl:132
% :- test_boxlog(atmost(5, X_VAR, puppy(X_VAR))).
proven_neg(puppy(X)) :-
        nesc(puppy(Puppy0)),
        dif_objs(X, Puppy0),
        nesc(puppy(Puppy1)),
        dif_objs(X, Puppy1),
        nesc(puppy(Puppy2)),
        dif_objs(X, Puppy2),
        nesc(puppy(Puppy3)),
        dif_objs(X, Puppy3),
        dif_objs(X, Puppy4),
        nesc(puppy(Puppy4)).
proven_neg(different(X, Puppy4)) :-
        nesc(puppy(Puppy4)),
        nesc(puppy(Puppy3)),
        dif_objs(X, Puppy3),
        nesc(puppy(Puppy2)),
        dif_objs(X, Puppy2),
        nesc(puppy(X)),
        nesc(puppy(Puppy0)),
        dif_objs(X, Puppy0),
        dif_objs(X, Puppy1),
        nesc(puppy(Puppy1)).
% c_success(kbii, kbii:call_u(kbii:test_boxlog(atmost(5, X, puppy(X))))).
*/



% ================================================================================================================
% At Least 5 puppies
% ================================================================================================================



:- test_boxlog(atleast(5, X, puppy(X))).

 /*
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/boxlog_sanity_04.pfc.pl:168
% :- test_boxlog(atleast(5, X_VAR, puppy(X_VAR))).
proven_tru(puppy(X)) :-
        skolem(X, count(5, inf, skF(skIsPuppyX_0FnSk, vv(KB, X))), _41398982).
make_existential(X, count(5, inf, skF(skIsPuppyX_0FnSk, vv(KB, X))), _41398982) :-
        ensure_cond(X, puppy(X)).
% c_success(kbii, kbii:call_u(kbii:test_boxlog(atleast(5, X, puppy(X))))).
*/






% ================================================================================================================
% Exactly 1 cute puppy
% ================================================================================================================



:- test_boxlog(exactly(1, X, puppy(X)&cute(X))).

 /*
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/boxlog_sanity_04.pfc.pl:191
% :- test_boxlog(exactly(1, X_VAR, puppy(X_VAR)&cute(X_VAR))).
proven_neg(cute(X)) :-
        nesc(puppy(X)),
        nesc(puppy(Puppy1)),
        dif_objs(X, Puppy1),
        nesc(cute(Puppy1)).
proven_neg(puppy(X)) :-
        nesc(cute(X)),
        nesc(puppy(Puppy1)),
        dif_objs(X, Puppy1),
        nesc(cute(Puppy1)).
proven_neg(different(X, Puppy1)) :-
        nesc(puppy(Puppy1)),
        nesc(cute(Puppy1)),
        nesc(puppy(X)),
        nesc(cute(X)).
proven_tru(cute(Puppy1)) :-
        skolem(Puppy1,
               count(1, inf, skF(skIsCuteIsPuppyExists_0FnSk, vv(KB, Puppy1))),
               1).
proven_tru(puppy(Puppy1)) :-
        skolem(Puppy1,
               count(1, inf, skF(skIsCuteIsPuppyExists_0FnSk, vv(KB, Puppy1))),
               1).
make_existential(Puppy1, count(1, inf, skF(skIsCuteIsPuppyExists_0FnSk, vv(KB, Puppy1))), 1) :-
        ensure_cond(Puppy1, puppy(Puppy1)),
        ensure_cond(Puppy1, cute(Puppy1)).
% c_success(kbii, kbii:call_u(kbii:test_boxlog(exactly(1, X, puppy(X)&cute(X))))).
*/



% ================================================================================================================
% At most 1 cute puppy
% ================================================================================================================



:- test_boxlog(atmost(1, X, puppy(X)&cute(X))).

 /*
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/boxlog_sanity_04.pfc.pl:230
% :- test_boxlog(atmost(1, X_VAR, puppy(X_VAR)&cute(X_VAR))).
proven_neg(cute(X)) :-
        nesc(puppy(X)),
        nesc(puppy(Puppy0)),
        dif_objs(X, Puppy0),
        nesc(cute(Puppy0)).
proven_neg(puppy(X)) :-
        nesc(cute(X)),
        nesc(puppy(Puppy0)),
        dif_objs(X, Puppy0),
        nesc(cute(Puppy0)).
proven_neg(different(X, Puppy0)) :-
        nesc(puppy(X)),
        nesc(cute(X)),
        nesc(puppy(Puppy0)),
        nesc(cute(Puppy0)).
% c_success(kbii, kbii:call_u(kbii:test_boxlog(atmost(1, X, puppy(X)&cute(X))))).
*/



% ================================================================================================================
% At Least 1 cute puppy
% ================================================================================================================



:- test_boxlog(atleast(1, X, puppy(X)&cute(X))).

 /*
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/boxlog_sanity_04.pfc.pl:259
% :- test_boxlog(atleast(1, X_VAR, puppy(X_VAR)&cute(X_VAR))).
proven_tru(cute(X)) :-
        skolem(X, count(1, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X))), 1).
proven_tru(puppy(X)) :-
        skolem(X, count(1, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X))), 1).
make_existential(X, count(1, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X))), 1) :-
        ensure_cond(X, puppy(X)),
        ensure_cond(X, cute(X)).
% c_success(kbii, kbii:call_u(kbii:test_boxlog(atleast(1, X, puppy(X)&cute(X))))).
*/




% ================================================================================================================
% Exactly 5 cute puppies
% ================================================================================================================



:- test_boxlog(exactly(5, X, puppy(X)&cute(X))).

 /*
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/boxlog_sanity_04.pfc.pl:281
% :- test_boxlog(exactly(5, X_VAR, puppy(X_VAR)&cute(X_VAR))).
proven_neg(cute(Cute1)) :-
        nesc(puppy(Cute1)),
        nesc(puppy(Puppy2)),
        nesc(cute(Puppy2)),
        dif_objs(Cute1, Puppy2),
        nesc(puppy(Puppy3)),
        nesc(cute(Puppy3)),
        dif_objs(Cute1, Puppy3),
        nesc(puppy(Puppy4)),
        nesc(cute(Puppy4)),
        dif_objs(Cute1, Puppy4),
        nesc(puppy(Puppy5)),
        nesc(cute(Puppy5)),
        dif_objs(Cute1, Puppy5),
        nesc(puppy(Puppy6)),
        dif_objs(Cute1, Puppy6),
        nesc(cute(Puppy6)).
proven_neg(puppy(Cute1)) :-
        nesc(cute(Cute1)),
        nesc(puppy(Puppy2)),
        nesc(cute(Puppy2)),
        dif_objs(Cute1, Puppy2),
        nesc(puppy(Puppy3)),
        nesc(cute(Puppy3)),
        dif_objs(Cute1, Puppy3),
        nesc(puppy(Puppy4)),
        nesc(cute(Puppy4)),
        dif_objs(Cute1, Puppy4),
        nesc(puppy(Puppy5)),
        nesc(cute(Puppy5)),
        dif_objs(Cute1, Puppy5),
        nesc(puppy(Puppy6)),
        dif_objs(Cute1, Puppy6),
        nesc(cute(Puppy6)).
proven_neg(different(Cute1, Puppy6)) :-
        nesc(puppy(Puppy6)),
        nesc(cute(Puppy6)),
        nesc(puppy(Puppy5)),
        nesc(cute(Puppy5)),
        dif_objs(Cute1, Puppy5),
        nesc(puppy(Puppy4)),
        nesc(cute(Puppy4)),
        dif_objs(Cute1, Puppy4),
        nesc(puppy(Cute1)),
        nesc(cute(Cute1)),
        nesc(puppy(Puppy2)),
        nesc(cute(Puppy2)),
        dif_objs(Cute1, Puppy2),
        nesc(puppy(Puppy3)),
        dif_objs(Cute1, Puppy3),
        nesc(cute(Puppy3)).
proven_tru(cute(X)) :-
        skolem(X, count(5, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X))), _42671860).
proven_tru(puppy(X)) :-
        skolem(X, count(5, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X))), _42671860).
make_existential(X, count(5, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X))), _42671860) :-
        ensure_cond(X, puppy(X)),
        ensure_cond(X, cute(X)).
% c_success(kbii, kbii:call_u(kbii:test_boxlog(exactly(5, X, puppy(X)&cute(X))))).
*/


% ================================================================================================================
% At most 5 cute puppies
% ================================================================================================================



:- test_boxlog(atmost(5, X, puppy(X)&cute(X))).

 /*
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/boxlog_sanity_04.pfc.pl:352
% :- test_boxlog(atmost(5, X_VAR, puppy(X_VAR)&cute(X_VAR))).
proven_neg(cute(X)) :-
        nesc(puppy(X)),
        nesc(puppy(Puppy0)),
        nesc(cute(Puppy0)),
        dif_objs(X, Puppy0),
        nesc(puppy(Puppy1)),
        nesc(cute(Puppy1)),
        dif_objs(X, Puppy1),
        nesc(puppy(Puppy2)),
        nesc(cute(Puppy2)),
        dif_objs(X, Puppy2),
        nesc(puppy(Puppy3)),
        nesc(cute(Puppy3)),
        dif_objs(X, Puppy3),
        nesc(puppy(Puppy4)),
        dif_objs(X, Puppy4),
        nesc(cute(Puppy4)).
proven_neg(puppy(X)) :-
        nesc(cute(X)),
        nesc(puppy(Puppy0)),
        nesc(cute(Puppy0)),
        dif_objs(X, Puppy0),
        nesc(puppy(Puppy1)),
        nesc(cute(Puppy1)),
        dif_objs(X, Puppy1),
        nesc(puppy(Puppy2)),
        nesc(cute(Puppy2)),
        dif_objs(X, Puppy2),
        nesc(puppy(Puppy3)),
        nesc(cute(Puppy3)),
        dif_objs(X, Puppy3),
        nesc(puppy(Puppy4)),
        dif_objs(X, Puppy4),
        nesc(cute(Puppy4)).
proven_neg(different(X, Puppy4)) :-
        nesc(puppy(Puppy4)),
        nesc(cute(Puppy4)),
        nesc(puppy(Puppy3)),
        nesc(cute(Puppy3)),
        dif_objs(X, Puppy3),
        nesc(puppy(Puppy2)),
        nesc(cute(Puppy2)),
        dif_objs(X, Puppy2),
        nesc(puppy(X)),
        nesc(cute(X)),
        nesc(puppy(Puppy0)),
        nesc(cute(Puppy0)),
        dif_objs(X, Puppy0),
        nesc(puppy(Puppy1)),
        dif_objs(X, Puppy1),
        nesc(cute(Puppy1)).
% c_success(kbii, kbii:call_u(kbii:test_boxlog(atmost(5, X, puppy(X)&cute(X))))).
*/



% ================================================================================================================
% At Least 5 cute puppies
% ================================================================================================================



:- test_boxlog(atleast(5, X, puppy(X)&cute(X))).

 /*
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/boxlog_sanity_04.pfc.pl:417
% :- test_boxlog(atleast(5, X_VAR, puppy(X_VAR)&cute(X_VAR))).
proven_tru(cute(X)) :-
        skolem(X, count(5, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X))), _50455988).
proven_tru(puppy(X)) :-
        skolem(X, count(5, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X))), _50455988).
make_existential(X, count(5, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X))), _50455988) :-
        ensure_cond(X, puppy(X)),
        ensure_cond(X, cute(X)).
% c_success(kbii, kbii:call_u(kbii:test_boxlog(atleast(5, X, puppy(X)&cute(X))))).
*/








% ================================================================================================================
% Exactly 1 possibly cute puppy
% ================================================================================================================



:- test_boxlog(exactly(1, X, puppy(X)&poss(cute(X)))).

 /*
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/boxlog_sanity_04.pfc.pl:444
% :- test_boxlog(exactly(1, X_VAR, puppy(X_VAR)&poss(cute(X_VAR)))).
proven_neg(puppy(X)) :-
        poss(cute(X)),
        nesc(puppy(Puppy1)),
        poss(cute(Puppy1)),
        dif_objs(X, Puppy1).
proven_neg(different(X, Puppy1)) :-
        nesc(puppy(Puppy1)),
        poss(cute(Puppy1)),
        poss(cute(X)),
        nesc(puppy(X)).
proven_tru(poss(cute(Puppy1))) :-
        skolem(Puppy1,
               count(1, inf, skF(skIsCuteIsPuppyExists_0FnSk, vv(KB, Puppy1, KB))),
               1).
proven_tru(puppy(Puppy1)) :-
        skolem(Puppy1,
               count(1, inf, skF(skIsCuteIsPuppyExists_0FnSk, vv(KB, Puppy1, KB))),
               1).
proven_tru(~cute(X)) :-
        nesc(puppy(X)),
        nesc(puppy(Puppy1)),
        poss(cute(Puppy1)),
        dif_objs(X, Puppy1).
make_existential(Puppy1, count(1, inf, skF(skIsCuteIsPuppyExists_0FnSk, vv(KB, Puppy1, KB))), 1) :-
        ensure_cond(Puppy1, puppy(Puppy1)),
        ensure_cond(Puppy1, poss(cute(Puppy1))).
% c_success(kbii, kbii:call_u(kbii:test_boxlog(exactly(1, X, puppy(X)&poss(cute(X)))))).
*/


% ================================================================================================================
% At most 1 possibly cute puppy
% ================================================================================================================



:- test_boxlog(atmost(1, X, puppy(X)&poss(cute(X)))).

 /*
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/boxlog_sanity_04.pfc.pl:483
% :- test_boxlog(atmost(1, X_VAR, puppy(X_VAR)&poss(cute(X_VAR)))).
proven_neg(puppy(X)) :-
        poss(cute(X)),
        nesc(puppy(Puppy0)),
        poss(cute(Puppy0)),
        dif_objs(X, Puppy0).
proven_neg(different(X, Puppy0)) :-
        nesc(puppy(X)),
        poss(cute(X)),
        poss(cute(Puppy0)),
        nesc(puppy(Puppy0)).
proven_tru(~cute(X)) :-
        nesc(puppy(X)),
        nesc(puppy(Puppy0)),
        poss(cute(Puppy0)),
        dif_objs(X, Puppy0).
% c_success(kbii, kbii:call_u(kbii:test_boxlog(atmost(1, X, puppy(X)&poss(cute(X)))))).
*/


% ================================================================================================================
% At Least 1 possibly cute puppy
% ================================================================================================================



:- test_boxlog(atleast(1, X, puppy(X)&poss(cute(X)))).

 /*
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/boxlog_sanity_04.pfc.pl:512
% :- test_boxlog(atleast(1, X_VAR, puppy(X_VAR)&poss(cute(X_VAR)))).
proven_tru(poss(cute(X))) :-
        skolem(X, count(1, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X, KB))), 1).
proven_tru(puppy(X)) :-
        skolem(X, count(1, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X, KB))), 1).
make_existential(X, count(1, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X, KB))), 1) :-
        ensure_cond(X, puppy(X)),
        ensure_cond(X, poss(cute(X))).
% c_success(kbii, kbii:call_u(kbii:test_boxlog(atleast(1, X, puppy(X)&poss(cute(X)))))).
*/




% ================================================================================================================
% Exactly 5 possibly cute puppies
% ================================================================================================================



:- test_boxlog(exactly(5, X, puppy(X)&poss(cute(X)))).

 /*
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/boxlog_sanity_04.pfc.pl:534
% :- test_boxlog(exactly(5, X_VAR, puppy(X_VAR)&poss(cute(X_VAR)))).
proven_neg(puppy(Puppy1)) :-
        poss(cute(Puppy1)),
        nesc(puppy(Puppy2)),
        poss(cute(Puppy2)),
        dif_objs(Puppy1, Puppy2),
        nesc(puppy(Puppy3)),
        poss(cute(Puppy3)),
        dif_objs(Puppy1, Puppy3),
        nesc(puppy(Puppy4)),
        poss(cute(Puppy4)),
        dif_objs(Puppy1, Puppy4),
        nesc(puppy(Puppy5)),
        poss(cute(Puppy5)),
        dif_objs(Puppy1, Puppy5),
        nesc(puppy(Puppy6)),
        poss(cute(Puppy6)),
        dif_objs(Puppy1, Puppy6).
proven_neg(different(Puppy1, Puppy6)) :-
        nesc(puppy(Puppy6)),
        poss(cute(Puppy6)),
        nesc(puppy(Puppy5)),
        poss(cute(Puppy5)),
        dif_objs(Puppy1, Puppy5),
        nesc(puppy(Puppy4)),
        poss(cute(Puppy4)),
        dif_objs(Puppy1, Puppy4),
        nesc(puppy(Puppy1)),
        poss(cute(Puppy1)),
        nesc(puppy(Puppy2)),
        poss(cute(Puppy2)),
        dif_objs(Puppy1, Puppy2),
        nesc(puppy(Puppy3)),
        poss(cute(Puppy3)),
        dif_objs(Puppy1, Puppy3).
proven_tru(poss(cute(X))) :-
        skolem(X,
               count(5, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X, KB))),
               _52310992).
proven_tru(puppy(X)) :-
        skolem(X,
               count(5, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X, KB))),
               _52310992).
proven_tru(~cute(Puppy1)) :-
        nesc(puppy(Puppy1)),
        nesc(puppy(Puppy2)),
        poss(cute(Puppy2)),
        dif_objs(Puppy1, Puppy2),
        nesc(puppy(Puppy3)),
        poss(cute(Puppy3)),
        dif_objs(Puppy1, Puppy3),
        nesc(puppy(Puppy4)),
        poss(cute(Puppy4)),
        dif_objs(Puppy1, Puppy4),
        nesc(puppy(Puppy5)),
        poss(cute(Puppy5)),
        dif_objs(Puppy1, Puppy5),
        nesc(puppy(Puppy6)),
        poss(cute(Puppy6)),
        dif_objs(Puppy1, Puppy6).
make_existential(X, count(5, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X, KB))), _52310992) :-
        ensure_cond(X, puppy(X)),
        ensure_cond(X, poss(cute(X))).
% c_success(kbii, kbii:call_u(kbii:test_boxlog(exactly(5, X, puppy(X)&poss(cute(X)))))).
*/

       

% ================================================================================================================
% At most 5 possibly cute puppies
% ================================================================================================================



:- test_boxlog(atmost(5, X, puppy(X)&poss(cute(X)))).

 /*
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/boxlog_sanity_04.pfc.pl:609
% :- test_boxlog(atmost(5, X_VAR, puppy(X_VAR)&poss(cute(X_VAR)))).
proven_neg(puppy(X)) :-
        poss(cute(X)),
        nesc(puppy(Puppy0)),
        poss(cute(Puppy0)),
        dif_objs(X, Puppy0),
        nesc(puppy(Puppy1)),
        poss(cute(Puppy1)),
        dif_objs(X, Puppy1),
        nesc(puppy(Puppy2)),
        poss(cute(Puppy2)),
        dif_objs(X, Puppy2),
        nesc(puppy(Puppy3)),
        poss(cute(Puppy3)),
        dif_objs(X, Puppy3),
        nesc(puppy(Puppy4)),
        poss(cute(Puppy4)),
        dif_objs(X, Puppy4).
proven_neg(different(X, Puppy4)) :-
        nesc(puppy(Puppy4)),
        poss(cute(Puppy4)),
        nesc(puppy(Puppy3)),
        poss(cute(Puppy3)),
        dif_objs(X, Puppy3),
        nesc(puppy(Puppy2)),
        poss(cute(Puppy2)),
        dif_objs(X, Puppy2),
        nesc(puppy(X)),
        poss(cute(X)),
        nesc(puppy(Puppy0)),
        poss(cute(Puppy0)),
        dif_objs(X, Puppy0),
        nesc(puppy(Puppy1)),
        poss(cute(Puppy1)),
        dif_objs(X, Puppy1).
proven_tru(~cute(X)) :-
        nesc(puppy(X)),
        nesc(puppy(Puppy0)),
        poss(cute(Puppy0)),
        dif_objs(X, Puppy0),
        nesc(puppy(Puppy1)),
        poss(cute(Puppy1)),
        dif_objs(X, Puppy1),
        nesc(puppy(Puppy2)),
        poss(cute(Puppy2)),
        dif_objs(X, Puppy2),
        nesc(puppy(Puppy3)),
        poss(cute(Puppy3)),
        dif_objs(X, Puppy3),
        nesc(puppy(Puppy4)),
        poss(cute(Puppy4)),
        dif_objs(X, Puppy4).
% c_success(kbii, kbii:call_u(kbii:test_boxlog(atmost(5, X, puppy(X)&poss(cute(X)))))).
*/
 

% ================================================================================================================
% At Least 5 possibly cute puppies
% ================================================================================================================



:- test_boxlog(atleast(5, X, puppy(X)&poss(cute(X)))).

 /*
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/boxlog_sanity_04.pfc.pl:674
% :- test_boxlog(atleast(5, X_VAR, puppy(X_VAR)&poss(cute(X_VAR)))).
proven_tru(poss(cute(X))) :-
        skolem(X,
               count(5, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X, KB))),
               _64786032).
proven_tru(puppy(X)) :-
        skolem(X,
               count(5, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X, KB))),
               _64786032).
make_existential(X, count(5, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X, KB))), _64786032) :-
        ensure_cond(X, puppy(X)),
        ensure_cond(X, poss(cute(X))).
% c_success(kbii, kbii:call_u(kbii:test_boxlog(atleast(5, X, puppy(X)&poss(cute(X)))))).
*/





% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/427 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/boxlog_sanity_04.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/BOXLOG_SANITY_04/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ABOXLOG_SANITY_04 

