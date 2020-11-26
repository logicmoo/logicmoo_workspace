:- include(test_header).
% =============================================
% File 'mpred_builtin.pfc'
% Purpose: Agent Reactivity for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net %
% Version: 'interface' 1.0.0
% Revision: $Revision: 1.9 $
% Revised At: $Date: 2002/06/27 14:13:20 $
% =============================================
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%

% exists(X, lives(X, green) & drinks(X, coffee)).

:- listing(vn:attr_unify_hook/2).

:- set_prolog_flag(gc,false).

:- test_boxlog([+assert],exists(H1,exists(H2,
 (leftof(H1, H2))))).

:- break.

:- test_boxlog(exists(H1,exists(H2,
 (leftof(H1, H2) & different(H1, H2))))).


:- test_boxlog(exactly(1,H1,exactly(1,H2,
 (leftof(H1, H2))))).

end_of_file.

:- test_boxlog(exactly(1,H1,exactly(1,H2,
 (leftof(H1, H2))))).

:- break.

:- test_boxlog(
 exactly(1,H1,exactly(1,H2,
  (leftof(H1, H2) & different(H1, H2))))).


:- break.

:- test_boxlog(exists(H1,exists(H2,
 (leftof(H1, H2) & different(H1, H2))))).

:- break.

:- test_boxlog(exactly(1,H1,exactly(1,H2,exactly(1,H3,
 (leftof(H1, H2) & leftof(H2, H3)))))).

:- break.

:- test_boxlog(clif(exists(H1,exists(H2,exists(H3,
 (leftof(H1, H2) & leftof(H2, H3))))))).

:- break.


% leftof(H1, H2)=>different(H1, H2).


% TODO unbreak this
% There are five houses in a row.
clif(exists(H1,exists(H2,exists(H3,
 (leftof(H1, H2) & leftof(H2, H3)))))).

:- break.

% TODO unbreak this
% There are five houses in a row.
exists(H1,exists(H2,exists(H3,
 (leftof(H1, H2) & different(H1, H2) & different(H1, H3) & different(H3, H2) & leftof(H2, H3))))).

:- break.


% There are five houses in a row.
exists(H1,exists(H2,exists(H3,exists(H4,exists(H5,
 (leftof(H1, H2) & leftof(H2, H3) & leftof(H3, H4) & leftof(H4, H5))))))).
       
