/* <module>
%
%  PFC is a language extension for prolog.
%
%  It adds a new type of module inheritance
%
% Dec 13, 2035
% Douglas Miles
*/
%  was_module(mt_04,[]).

:- include(test_header).

%:- file_begin(pfc).

%:- set_defaultAssertMt(myMt).

baseKB:mtProlog(code1).
baseKB:mtHybrid(kb2).
baseKB:mtHybrid(kb3).

baseKB:genlMt(kb3,code1).
baseKB:genlMt(kb3,kb2).

% code1: (a <- b).
code1:a:- kb2:b.
:- export(code1:a/0).

kb2:b.

baseKB:genlMt(code1,baseKB).
kb3: (a==>c).




