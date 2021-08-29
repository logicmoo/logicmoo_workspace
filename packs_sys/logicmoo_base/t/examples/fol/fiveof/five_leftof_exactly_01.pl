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


exists(H1,exists(H2,exists(H3,exists(H4,exists(H5,
 (leftof(H1, H2) & leftof(H2, H3) & leftof(H3, H4) & leftof(H4, H5))))))).



