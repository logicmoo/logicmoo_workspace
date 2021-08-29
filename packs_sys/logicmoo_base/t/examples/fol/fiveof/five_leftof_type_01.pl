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
  
% There are five houses in a row.

leftof(h1, h2).
leftof(h2, h3).
leftof(h3, h4).
leftof(h4, h5).

leftof(H1, H2) => house(H1) & house(H2).

