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
:- nop(module( baseKB)).

% makes the KB monotonic
:- set_kif_option(qualify_modality,simple_nesc).

leftof(h1, h2).
leftof(h2, h3).
leftof(h3, h4).
leftof(h4, h5).

% this should cause h1-h5 to become houses
leftof(H1, H2) => house(H1) & house(H2).

:- kif_compile.

% intractive_test/1 means only run if interactive
:- interactive_test(listing(pfclog)).

% mpred_test/1 each become a Junit test that must succeed
:- mpred_test(pfclog_compile).

:- mpred_test(pfclog_uncompile).
% This is the real test we care about here
:- interactive_test(pfclog_recompile).

:- interactive_test(listing(nesc)).

% ensure our rule worked
:- mpred_test(nesc(house(h1))).

% ensure we are being nice
:- mpred_test(poss(house(false_positive))).
% but not "too" nice
:- mpred_test(\+ nesc(house(false_positive))).

% lets invalidate at least one pair
~poss(house(h2)).

% if the above took effect
:- mpred_test(\+ nesc(house(h2))).

% we did invalidate the pair ?
:- mpred_test(\+ nesc(house(h1))).

% @TODO not sure what we want to invalidate the rest ?
:- mpred_test(ignore(\+ nesc(house(h5)))).

:- interactive_test(listing(nesc)).



% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/469 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/fiveof/five_leftof_type_02.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.fol.fiveof/FIVE_LEFTOF_TYPE_02/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3AFIVE_LEFTOF_TYPE_02 

