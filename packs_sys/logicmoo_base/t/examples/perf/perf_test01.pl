

:- ensure_loaded('../../../prolog/logicmoo_utils').
:- statistics.
:- use_module(library(pfc)).
:- statistics.
:- include('/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo_user.pl').
:- statistics.
% Started at Sat Sep 24 00:12:29 2016
% 66.750 seconds cpu time for 83,358,106 inferences
% 675,725 atoms, 17,331 functors, 22,431 predicates, 346 modules, 4,674,991 VM-codes
%
%                        Limit    Allocated       In use
% Local  stack:  6,000,004,096      126,976        1,936 Bytes
% Global stack:  6,000,004,096    1,044,464      795,384 Bytes
% Trail  stack:  6,000,002,048      129,016        3,472 Bytes
%
% 173 garbage collections gained 196,051,736 bytes in 0.064 seconds.
% 18 atom garbage collections gained 180,137 atoms in 0.165 seconds.
% 2,155 clause garbage collections gained 147,164 clauses in 0.237 seconds.
% Stack shifts: 5 local, 7 global, 6 trail in 0.001 seconds
% 2 threads, 0 finished threads used 0.000 seconds

:- baseKB:file_begin(pfc).

:- module(baseKB).

==>(aaa,bbb(1)).
aaa.
