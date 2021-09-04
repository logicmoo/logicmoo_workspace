% This file is mostly all inside if/endifs so it doesnt interfere with `module/2`

:- if((
   keep_going,
   redefine_system_predicate(system:break/0),
   abolish(system:break,0),
   assert(system:break :- (dumpST, sleep(1))))).
:- endif.
