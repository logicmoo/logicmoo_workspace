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

:- include('../test_header').

interactive_test(G):- add_history(G), fmt("?- ~q.",[G]), nop(mpred_why(G)).

:- set_prolog_flag(gc,true).

%:- if(qsave:qsave_program(bar,[])). :-endif.

%:- break.

:- if(set_prolog_flag(gc,false)). :-endif.
:- expects_dialect(clif).

