% =============================================
% File 'mpred_builtin.pfc'
% Purpose: Agent Reactivity for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net %
% Version: 'interface' 1.0.0
% Revision: $Revision: 1.9 $
% Revised At: $Date: 2002/06/27 14:13:20 $
% =============================================
%:- if(use_module(library(logicmoo_utils))). :- endif.
%:- if(autoload_all). :-endif.
%:- if(use_module(library(qsave))). :-endif.

%:- if(qsave_program(foo,[])). :-endif.

:- if(set_prolog_flag(runtime_testing_module,baseKB)).
:- if(set_prolog_flag(test_module,baseKB)).
:- include('../test_header.pl').
:- endif.
:- endif.


interactive_test(G):- add_history(G), fmt(?-(G)), call(G).

:- set_prolog_flag(gc,true).

%:- if(qsave:qsave_program(bar,[])). :-endif.

%:- break.

:- if(set_prolog_flag(gc,false)). :-endif.
:- expects_dialect(clif).

