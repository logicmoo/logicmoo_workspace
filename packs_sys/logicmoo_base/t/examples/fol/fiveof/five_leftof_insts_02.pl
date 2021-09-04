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


