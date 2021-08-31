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

:- mpred_test(true).

% There are five houses in a row.
:- module(baseKB).

% makes the KB monotonic
:- set_kif_option(qualify_modality,simple_nesc).

leftof(h1, h2).
leftof(h2, h3).
leftof(h3, h4).
leftof(h4, h5).

% this should cause h1-h5 to become houses
leftof(H1, H2) => house(H1) & house(H2).

:- mpred_test(kif_compile).

% intractive_test/1 means only run if interactive
:- interactive_test(listing(pfclog)).

% mpred_test/1 each become a Junit test that must succeed
:- mpred_test(pfclog_compile).


% :- mpred_test(nesc(leftof(h4, h5))).


