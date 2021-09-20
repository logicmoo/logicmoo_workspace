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
:- expects_dialect(clif).
:- set_prolog_flag(gc,false).
  
% There are five houses in a row.



(leftof(h1, h2) & leftof(h2, h3) & leftof(h3, h4) & leftof(h4, h5)).

% leftof(h3,h4)
% leftof(h4,h5)
% poss(leftof(h1,h2))==>leftof(h2,h3).
% poss(leftof(h2,h3))==>leftof(h1,h2).
% ~leftof(h1,h2)==>nesc(~leftof(h2,h3)).
% ~leftof(h2,h3)==>nesc(~leftof(h1,h2)).



% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/fiveof/five_leftof_insts_02.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.fol.fiveof/FIVE_LEFTOF_INSTS_02/logicmoo_base_fol_fiveof_FIVE_LEFTOF_INSTS_02_JUnit/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3AFIVE_LEFTOF_INSTS_02 

% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/621
