````prolog

(base) root@gitlab:/opt/logicmoo_workspace/packs_sys/pfc/t/sanity_base# swipl
Welcome to SWI-Prolog (threaded, 64 bits, version 8.3.2-27-gca64f340a)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- module(foo).
Warning: foo is not a current module (created)
true.

foo:  ?- use_module(library(pfc)).
% /opt/logicmoo_workspace/packs_sys/pfc/prolog/pfc.pl:84
% add_pfc_to_module(using_pfc(baseKB, foo, baseKB, pfc_load)).
% debugm(system, show_success(system, delete_import_module(baseKB, user))).
% debugm(not_is_pfc_file, show_success(not_is_pfc_file, ensure_abox_hybrid(baseKB))).
% add_pfc_to_module(using_pfc(foo, foo, foo, pfc_load)).
% debugm(system, show_success(system, delete_import_module(foo, user))).
%  /opt/logicmoo_workspace/packs_sys/pfc/prolog/pfclib/system_each_module.pfc.pl compiled into foo 0.00 sec, 2 clauses
% debugm(not_is_pfc_file, show_success(not_is_pfc_file, ensure_abox_hybrid(foo))).
true.

foo:  ?- ain(start==>{writeln(started)}).
% make_as_dynamic(do_call_inherited(baseKB, start), baseKB, start, 0).
true.

foo:  ?- ain(start).
started
true.

foo:  ?- repropagate(start).
% moving_to_last_clause(foo, start) :-
%     zwc,
%     inherit_above(foo, start).
started
% debugm(foo, show_success(foo, mpred_fwc(start))).
true.

foo:  ?- repropagate(start).
started
% debugm(foo, show_success(foo, mpred_fwc(start))).
true.

foo:  ?- ain(start).
true.

foo:  ?- ain(\+ start).
% debugm(pfc_lib, show_success(pfc_lib, mpred_withdraw(start,  (mfl4(_1006996, foo, user_input, 47), ax)))).
% debugm(pfc_lib, show_success(pfc_lib, mpred_withdraw_fail_if_supported(start,  (mfl4(_1006996, foo, user_input, 47), ax)))).
true.

foo:  ?- ain(start).
started
true.

foo:  ?-

````

