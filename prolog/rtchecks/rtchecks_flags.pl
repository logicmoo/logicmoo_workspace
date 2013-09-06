:- multifile define_flag/3.

define_flag(runtime_checks,          [yes, no],                no).
define_flag(rtchecks_level,          [inner, exports],         inner).
define_flag(rtchecks_check,          [yes, no],                yes).
define_flag(rtchecks_debug,          [yes, no],                no).
define_flag(rtchecks_trace,          [yes, no],                no).
define_flag(rtchecks_trust,          [yes, no],                yes).
define_flag(rtchecks_entry,          [yes, no],                yes).
define_flag(rtchecks_exit,           [yes, no],                yes).
define_flag(rtchecks_test,           [yes, no],                no).
define_flag(rtchecks_inline,         [yes, no],                no).
define_flag(rtchecks_asrloc,         [yes, no],                yes).
define_flag(rtchecks_predloc,        [yes, no],                yes).
define_flag(rtchecks_callloc,        [no, literal, predicate], predicate).
define_flag(rtchecks_namefmt,        [short, long],            long).
define_flag(rtchecks_abort_on_error, [yes, no],                no).
