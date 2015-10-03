:- module(rtchecks_flags, []).

define_flag(runtime_checks,          [yes, no],        no).
define_flag(rtchecks_level,          [inner, exports], inner).
define_flag(rtchecks_check,          [yes, no],        yes).
define_flag(rtchecks_true,           [yes, no],        no).
define_flag(rtchecks_false,          [yes, no],        no).
define_flag(rtchecks_debug,          [yes, no],        no).
define_flag(rtchecks_trace,          [yes, no],        no).
define_flag(rtchecks_trust,          [yes, no],        yes).
define_flag(rtchecks_entry,          [yes, no],        yes).
define_flag(rtchecks_exit,           [yes, no],        yes).
define_flag(rtchecks_test,           [yes, no],        no).
define_flag(rtchecks_namefmt,        [short, long],    long).
define_flag(rtchecks_abort_on_error, [yes, no],        no).

:- ( define_flag(Flag, _Values, Default),
     create_prolog_flag(Flag, Default, [type(atom)]),
     fail
   ; true
   ).
