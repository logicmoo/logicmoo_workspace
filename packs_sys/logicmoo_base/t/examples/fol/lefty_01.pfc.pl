:- include(test_header).



:- include('test_header.pfc').
% :- process_this_script.

%=  setup pfc
:- file_begin(pfc).


==> clif(lefty(P)  => ~righty(P)).
%=%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%= kif = 
%=       all(P, (lefty(P)=> ~righty(P))).
%=
%= pkif = 
%=       all(P, (lefty(P)=>not(righty(P)))).
%=
%= cnf = 
%=       not(lefty(P))v not(righty(P)).
%=
%= horn = 
%=       [ (not(righty(P)):-lefty(P)), (not(lefty(P)):-righty(P))].
%=
%=
%= succeed(user:boxlog_to_pfc((not(righty(P)):-lefty(P)), (lefty(P), {is_unit(P)}==> ~(righty(P))))).
%=
%= succeed(user:boxlog_to_pfc((not(lefty(P)):-righty(P)), (righty(P), {is_unit(P)}==> ~(lefty(P))))).
%=
%=%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%= Notice we do not have the evidence to prove anyone lefty to righty!
%= Only the ability to "disprove" right now


%= Humans are lefty or righty
==> clif(handwriter(P) => (righty(P) v lefty(P))).

%=%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%= kif = 
%=       all(P, (handwriter(P)=>righty(P)v lefty(P))).
%=
%= pkif = 
%=       all(P, (handwriter(P)=>righty(P)v lefty(P))).
%=
%= cnf = 
%=       not(handwriter(P))v (righty(P)v lefty(P)).
%=
%= horn = 
%=
%=       [ (righty(P):-handwriter(P), not(lefty(P))),
%=         (lefty(P):-handwriter(P), not(righty(P))),
%=         (not(handwriter(P)):-not(righty(P)), not(lefty(P)))
%=       ].
%=
%=
%= succeed(user:boxlog_to_pfc((righty(P):-handwriter(P), not(lefty(P))), (handwriter(P),  ~(lefty(P)), {is_unit(P)}==>righty(P)))).
%=
%= succeed(user:boxlog_to_pfc((lefty(P):-handwriter(P), not(righty(P))), (handwriter(P),  ~(righty(P)), {is_unit(P)}==>lefty(P)))).
%=
%= succeed(user:boxlog_to_pfc((not(handwriter(P)):-not(righty(P)), not(lefty(P))), ( ~(righty(P)),  ~(lefty(P)), {is_unit(P)}==> ~(handwriter(P))))).
%=
%=%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%= joe is lefty
==> lefty(joe).

:- show_call(example_proven_true( lefty(joe ))).

%= Logical Negation (not by failure)

%= pat is not righty
==>  ~(righty(pat)).

%= We check that we cannot prove Pat is lefty.
%= Thus a query to ?- lefty(pat ). 
:- show_call(example_known_is_failure( lefty(pat ))).

%= Assert pat is handwriter
==> handwriter(pat).

%= Thus we can deduce he is lefty now 
:- show_call(example_known_is_success( lefty(pat ))).



% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/75 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/lefty_01.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/LEFTY_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ALEFTY_01 

