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

:- module(baseKB).
:- process_script_file.
:- dynamic house/1.

:- kif_compile.

% makes the KB monotonic
:- set_kif_option(qualify_modality,false).

% There are five houses in a row.
leftof(h1, h2).
leftof(h2, h3).
leftof(h3, h4).
leftof(h4, h5).

% uncomment the next line and this will fail the test
% :- set_kif_option(qualify_modality,simple_nesc).
% this should cause h1-h5 to become houses
leftof(H1, H2) => house(H1) & house(H2).

% intractive_test/1 means only run if interactive
:- interactive_test(listing(kif_show)).

% ensure our rule worked
% mpred_test/1 each become a Junit test that must succeed
:- mpred_test((house(h1))).
:- mpred_test((house(h2))).
:- mpred_test((house(h3))).
:- mpred_test((house(h4))).
:- mpred_test((house(h5))).

% ensure we are being nice
:- mpred_test(poss(house(false_positive))).
% but not "too" nice
:- mpred_test(\+ nesc(house(false_positive))).

:- listing([house/1,nesc/1]).