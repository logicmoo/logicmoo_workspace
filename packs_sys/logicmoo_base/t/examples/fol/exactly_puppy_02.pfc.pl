
% test dealing  with instances of type counts with modality

:- module(cute6,[]).

:- include(test_header).
:- user:use_module(library(editline)).
:- use_module(library(occurs)). % sub_term/2
:- use_module(library(sort)). % predsort/3
:- use_module(library(backcomp)). % concat_atom/2
:- user:autoload.

:- module_transparent(system: = /2).
:- module_transparent('$attvar':'$wakeup'/1).
:- module_transparent('$attvar':'call_all_attr_uhooks'/2).
:- module_transparent('$attvar':'begin_call_all_attr_uhooks'/2).
:- module_transparent('$attvar':'uhook'/3).

:- '$current_source_module'(M),install_retry_undefined(M,error).
:- install_retry_undefined(user,error).
:- install_retry_undefined(kbii,error).
:- install_retry_undefined(kbi,error).
% :- set_prolog_flag(autoload,false).
:- set_prolog_flag(retry_undefined, false).
:- set_prolog_flag(access_level, system).

% Option Examples: nesc($sentence),  poss($sentence),  poss($sentence)=>nesc($sentence).
% ==> feature_setting(default_modality,nesc($sentence)).

/*

Feature Notes:

P.  % P happens to be the feature_setting default_modality
poss(P).  % possibly P
nesc(P).  % necessarily P
~nesc(P).  % not necessarily P
nesc(~P).  % necessarily not P
~poss(P).  % not possibly P
poss(~P).  % possibly not P

poss(P)=>nesc(P).  % P is true by default (allows other axioms to override)
poss(P)&~nesc(P).  % possibly, but not necessarily P

~naf(P).  % P is default
naf(~P).  % possibly P
naf(P).  % possibly not P

there are many Logically equivalent settings like   

~poss(P)    == nesc(~P)

falsify(~P) ==  poss(P) v nesc(P).

*/

test_sanity(G):- sanity(mpred_test(G)).

:- kbi_define(cute/1).
:- kbi_define(ugly/1).

:- kb_shared(baseKB:cute/1).
:- kb_local(ugly/1).
:- kb_local(isa/2).
:- kbi_define(poss/1).


%===== axioms =======

% there are  exactly 3 puppies total
:- test_boxlog([+assert],exactly(3, X, puppy(X))).

% Ensure we can see them
:- test_sanity((findall(X,puppy(X),L),length(L,3))).


% There is a puppy we call puppy1
:- test_boxlog([+assert,+exist],puppy(puppy1)).

% There is a puppy we call puppy3
:- test_boxlog([+assert,+exist],puppy(puppy3)).

% there is exactly two objects that are pupplies that are possibly cute
:- test_boxlog([+assert],exactly(2, X, puppy(X) & poss(cute(X)))).

end_of_file.

% Ensure we still only see 3
:- test_sanity((findall(X,puppy(X),L),length(L,3))).

% Ensure we can get 3x3
:- test_sanity((findall(G,(puppy(X),puppy(Y),G =( X=Y), writeln(G),ignore(G)),L),length(L,Len),Len==9)).

end_of_file.


% Ensure we can see them
:- test_sanity((findall(X,poss(cute(X)),L),length(L,2))).

% Ensure only 2 pairs are equal instances
:- test_sanity((findall(G,(puppy(X),puppy(Y),G =( X=Y), writeln(G),G),L),length(L,Len),Len==2)).

% Ensure when two are picked out via conjunction the system tries to serve out 2 different values
:- test_sanity(( puppy(X),puppy(Y), !, X\=Y )).

% Ensure when two are picked out consecutively they are different object instances
:- test_sanity(( puppy(X),puppy(Y), !, dif_objs(X,Y) )).

% Ensure 12 naive obj dif pairs
:- test_sanity(( findall(X\=Y,(puppy(X),puppy(Y), dif_objs(X,Y)),L),length(L,Len),Len==12)).

% Ensure only 6 total obj dif pairs
:- test_sanity(( findall(X\=Y,(pred1_to_unique_pairs(puppy,X,Y), dif_objs(X,Y)),L),length(L,Len),Len==6)).

% Ensure 20 naive obj dif pairs
:- test_sanity(( findall(X\=Y,(puppy(X),puppy(Y), dif_objs(X,Y)),L),length(L,Len),Len==20)).

% Ensure only 10 total obj dif pairs
:- test_sanity(( findall(X\=Y,(pred1_to_unique_pairs(puppy,X,Y), dif_objs(X,Y)),L),length(L,Len),Len==10)).

% there are 2 (for sure) cute puppies
:- test_boxlog([+assert],exactly(2, X, puppy(X) & cute(X))).

% cute things cannot be ugly things and visa versa
:- test_boxlog([+assert],forall( X, iff(cute(X),~ugly(X)))).

% all puppies are cute or ugly
:- test_boxlog([+assert],forall( X, if(puppy(X),(ugly(X) v cute(X))))).


% there is at least one puppy
:- test_boxlog([+assert],atleast(1, X, puppy(X))).

% there is at least one puppy
:- test_boxlog([+assert],exists(X, puppy(X))).


end_of_file.


%===== tests =======

:- if(kif_option_value(unit_tests,true)).

% means total 5 puppies 
:- test_sanity( { findall(X,puppy(X),L),length(L,5) }).

% 2 are for sure actually cute
:- test_sanity( { findall(X,(puppy(X),cute(X)),L),length(L,2)}).

% leaving 3 more as _only_ possibly cute
:- test_sanity( { findall(X,(puppy(X),poss(cute(X)),naf(cute(X))),L),length(L,3)}).

% the last puppy is not for sure known cute or or not cute so it may be ugly
% :- test_sanity( { findall(X,(puppy(X),poss(ugly(X))),L),length(L,1)}).

:- endif.


:- if(kif_option_value(extras(test_fwc),true)).
puppy(X) ==> known(puppy(X)).
cute(X) ==> known(cute(X)).
ugly(X) ==> known(ugly(X)).
~ugly(X) ==> known(~ugly(X)).
poss(cute(X)) ==> known(poss(cute(X))).
:- endif.


end_of_file.

%===== debugging notes =======

:- 
  test_sanity((nesc(puppy(Puppy1)),nesc(puppy(Puppy2)),Puppy1=Puppy2)).
/*
Puppy1 = Puppy2,
add_cond(Puppy2, [puppy, poss(cute(Puppy2)), skF(skF(skIsCuteIsPuppyX_0FnSk, vv), 4)]) ;
Puppy1 = Puppy2,
add_cond(Puppy2, [puppy, poss(cute(Puppy2)), skF(skF(skIsCuteIsPuppyX_0FnSk, vv), 2)]) ;
Puppy1 = Puppy2,
add_cond(Puppy2, [puppy, poss(cute(Puppy2)), skF(skF(skIsCuteIsPuppyX_0FnSk, vv), 3)]) ;
Puppy1 = Puppy2,
add_cond(Puppy2, [puppy, poss(cute(Puppy2)), skF(skF(skIsCuteIsPuppyX_0FnSk, vv), 1)]).
*/

go_test:- forall(no_repeats(proven_neg(G)),writeln(proven_neg(G))).



?- 
   nesc(puppy(Puppy1)),nesc(puppy(Puppy2)),nesc(puppy(Puppy3)),nesc(puppy(Puppy4),nesc(puppy(Puppy5)),
   comingle_vars([Puppy1,Puppy2,Puppy3,Puppy4],NewP).



cute6:  ?- 

  nl,puppy(Puppy),get_typeinfos(Puppy,TYPEINFO),writeln(Puppy=TYPEINFO),fail.

_488=[poss(cute(_488)),puppy(_488)]
_488=[poss(cute(_488)),puppy(_488)]
_488=[poss(cute(_488)),puppy(_488)]
_488=[poss(cute(_488)),puppy(_488)]
_3360=[cute(_3360),puppy(_3360)]
_3360=[cute(_3360),puppy(_3360)]
_4836=[puppy(_4836)]
_4836=[puppy(_4836)]
_4836=[puppy(_4836)]
_4836=[puppy(_4836)]
_4836=[puppy(_4836)]





% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/exactly_poss_cute_06.pfc.pl:11
% :- test_boxlog(exactly(4, X_VAR, puppy(X_VAR)&poss(cute(X_VAR)))).
% autoloading common_logic_snark:gensym/2 from /home/prologmud_server/lib/swipl-7.5.15/library/gensym
% autoloading dmsg:maplist/2 from /home/prologmud_server/lib/swipl-7.5.15/library/apply
% autoloading attvar_serializer:maplist/3 from /home/prologmud_server/lib/swipl-7.5.15/library/apply
% kif :-
%       exactly(4, X, puppy(X)&poss(cute(X))).
% autoloading cute6:sub_term/2 from /home/prologmud_server/lib/swipl-7.5.15/library/occurs
% autoloading cute6:concat_atom/2 from /home/prologmud_server/lib/swipl-7.5.15/library/backcomp
% autoloading common_logic_compiler:sub_term/2 from /home/prologmud_server/lib/swipl-7.5.15/library/occurs
% nnf :-
%       ~puppy(X)v nesc(~cute(X))v skolem(X, count(4, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X, KB))), _38009592)&(puppy(X)&poss(cute(X))v~skolem(X, count(4, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X, KB))), _38009592))&(~puppy(_38033330)v nesc(~cute(_38033330))v~different(_38006508, _38033330)v(~puppy(_38037090)v nesc(~cute(_38037090))v~different(_38006508, _38037090)v(~puppy(_38006508)v nesc(~cute(_38006508))v(~puppy(_38044610)v nesc(~cute(_38044610)))v~different(_38006508, _38044610)v(~puppy(_38040850)v nesc(~cute(_38040850))v~different(_38006508, _38040850))))).
% to_tnot :-
%       ( (   (   neg(puppy(X))
%             ;   nesc(nesc(~cute(X)))
%             )
%         ;   nesc(skolem(X,
%                        count(4, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X, KB))),
%                        _38009592))
%         ),
%         (   nesc(puppy(X)),
%             nesc(poss(cute(X)))
%         ;   neg(skolem(X,
%                        count(4, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X, KB))),
%                        _38009592))
%         )
%       ),
%       (   (   (   neg(puppy(_38033330))
%               ;   nesc(nesc(~cute(_38033330)))
%               )
%           ;   neg(different(_38006508, _38033330))
%           )
%       ;   (   (   neg(puppy(_38037090))
%               ;   nesc(nesc(~cute(_38037090)))
%               )
%           ;   neg(different(_38006508, _38037090))
%           )
%       ;   (   (   (   neg(puppy(_38006508))
%                   ;   nesc(nesc(~cute(_38006508)))
%                   )
%               ;   neg(puppy(_38044610))
%               ;   nesc(nesc(~cute(_38044610)))
%               )
%           ;   neg(different(_38006508, _38044610))
%           )
%       ;   (   neg(puppy(_38040850))
%           ;   nesc(nesc(~cute(_38040850)))
%           )
%       ;   neg(different(_38006508, _38040850))
%       ).
% tlog_nnf :-
%       (   (   ( n(neg(puppy(X))),
%                 n(nesc(nesc(~cute(X))))
%               ),
%               n(nesc(skolem(X,
%                            count(4,
%                                  inf,
%                                  skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X, KB))),
%                            _38009592)))
%           ;   (   n(nesc(puppy(X)))
%               ;   n(nesc(poss(cute(X))))
%               ),
%               n(neg(skolem(X,
%                            count(4,
%                                  inf,
%                                  skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X, KB))),
%                            _38009592)))
%           )
%       ;   ( ( n(neg(puppy(_38033330))),
%               n(nesc(nesc(~cute(_38033330))))
%             ),
%             n(neg(different(_38006508, _38033330)))
%           ),
%           ( ( n(neg(puppy(_38037090))),
%               n(nesc(nesc(~cute(_38037090))))
%             ),
%             n(neg(different(_38006508, _38037090)))
%           ),
%           ( ( ( n(neg(puppy(_38006508))),
%                 n(nesc(nesc(~cute(_38006508))))
%               ),
%               n(neg(puppy(_38044610))),
%               n(nesc(nesc(~cute(_38044610))))
%             ),
%             n(neg(different(_38006508, _38044610)))
%           ),
%           ( n(neg(puppy(_38040850))),
%             n(nesc(nesc(~cute(_38040850))))
%           ),
%           n(neg(different(_38006508, _38040850)))
%       ).
% tlog_nnf_out_negated :-
%       n(~puppy(X))&n(nesc(nesc(~cute(X))))&n(nesc(skolem(X, count(4, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X, KB))), _38009592)))v(n(nesc(puppy(X)))v n(nesc(poss(cute(X))))&n(~skolem(X, count(4, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X, KB))), _38009592)))v(n(~puppy(_38033330))&n(nesc(nesc(~cute(_38033330))))&n(~different(_38006508, _38033330))&(n(~puppy(_38037090))&n(nesc(nesc(~cute(_38037090))))&n(~different(_38006508, _38037090))&(n(~puppy(_38006508))&n(nesc(nesc(~cute(_38006508))))&(n(~puppy(_38044610))&n(nesc(nesc(~cute(_38044610)))))&n(~different(_38006508, _38044610))&(n(~puppy(_38040850))&n(nesc(nesc(~cute(_38040850))))&n(~different(_38006508, _38040850)))))).
% autoloading common_logic_modalization:sub_term/2 from /home/prologmud_server/lib/swipl-7.5.15/library/occurs
% autoloading cute6:predsort/3 from /home/prologmud_server/lib/swipl-7.5.15/library/sort
proven_neg(puppy(Puppy1)) :-
        falsify(~cute(Puppy1)),
        nesc(puppy(Puppy2)),
        falsify(~cute(Puppy2)),
        dif_objs(Puppy1, Puppy2),
        nesc(puppy(Puppy3)),
        falsify(~cute(Puppy3)),
        dif_objs(Puppy1, Puppy3),
        nesc(puppy(Puppy4)),
        falsify(~cute(Puppy4)),
        dif_objs(Puppy1, Puppy4),
        nesc(puppy(Puppy5)),
        dif_objs(Puppy1, Puppy5),
        falsify(~cute(Puppy5)).
proven_neg(different(Puppy1, Puppy5)) :-
        nesc(puppy(Puppy5)),
        falsify(~cute(Puppy5)),
        nesc(puppy(Puppy4)),
        falsify(~cute(Puppy4)),
        dif_objs(Puppy1, Puppy4),
        nesc(puppy(Puppy1)),
        falsify(~cute(Puppy1)),
        nesc(puppy(Puppy2)),
        falsify(~cute(Puppy2)),
        dif_objs(Puppy1, Puppy2),
        nesc(puppy(Puppy3)),
        dif_objs(Puppy1, Puppy3),
        falsify(~cute(Puppy3)).
proven_tru(poss(cute(X))) :-
        skolem(X, count(4, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X, KB))), KB).
proven_tru(puppy(X)) :-
        skolem(X, count(4, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X, KB))), KB).
proven_tru(~cute(Puppy1)) :-
        nesc(puppy(Puppy1)),
        nesc(puppy(Puppy2)),
        falsify(~cute(Puppy2)),
        dif_objs(Puppy1, Puppy2),
        nesc(puppy(Puppy3)),
        falsify(~cute(Puppy3)),
        dif_objs(Puppy1, Puppy3),
        nesc(puppy(Puppy4)),
        falsify(~cute(Puppy4)),
        dif_objs(Puppy1, Puppy4),
        nesc(puppy(Puppy5)),
        dif_objs(Puppy1, Puppy5),
        falsify(~cute(Puppy5)).
make_existential(X, count(4, inf, skF(skIsCuteIsPuppyX_0FnSk, vv(KB, X, KB))), KB) :-
        ensure_cond(X, puppy(X)),
        never_cond(X, ~cute(X)).
% kbi_define(cute6:puppy/1).
% kbi_define(cute6:different/2).
% kbi_define(cute6:poss/1).
% kbi_define(cute6:(~)/1).
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/exactly_poss_cute_06.pfc.pl:14
% :- test_boxlog(exactly(2, X_VAR, puppy(X_VAR)&cute(X_VAR))).
% kif :-
%       exactly(2, X, puppy(X)&cute(X)).
% nnf :-
%       ~puppy(X)v~cute(X)v skolem(X, count(2, inf, skF(skIsCuteIsX_0FnSk, vv(KB, X))), _39514024)&(puppy(X)&cute(X)v~skolem(X, count(2, inf, skF(skIsCuteIsX_0FnSk, vv(KB, X))), _39514024))&(~puppy(_39511688)v~cute(_39511688)v(~puppy(_39538616)v~cute(_39538616))v~different(_39511688, _39538616)v(~puppy(_39535680)v~cute(_39535680)v~different(_39511688, _39535680))).
% to_tnot :-
%       ( (   (   neg(puppy(X))
%             ;   neg(cute(X))
%             )
%         ;   nesc(skolem(X,
%                        count(2, inf, skF(skIsCuteIsX_0FnSk, vv(KB, X))),
%                        _39514024))
%         ),
%         (   nesc(puppy(X)),
%             nesc(cute(X))
%         ;   neg(skolem(X,
%                        count(2, inf, skF(skIsCuteIsX_0FnSk, vv(KB, X))),
%                        _39514024))
%         )
%       ),
%       (   (   (   (   neg(puppy(_39511688))
%                   ;   neg(cute(_39511688))
%                   )
%               ;   neg(puppy(_39538616))
%               ;   neg(cute(_39538616))
%               )
%           ;   neg(different(_39511688, _39538616))
%           )
%       ;   (   neg(puppy(_39535680))
%           ;   neg(cute(_39535680))
%           )
%       ;   neg(different(_39511688, _39535680))
%       ).
% tlog_nnf :-
%       (   (   ( n(neg(puppy(X))),
%                 n(neg(cute(X)))
%               ),
%               n(nesc(skolem(X,
%                            count(2, inf, skF(skIsCuteIsX_0FnSk, vv(KB, X))),
%                            _39514024)))
%           ;   (   n(nesc(puppy(X)))
%               ;   n(nesc(cute(X)))
%               ),
%               n(neg(skolem(X,
%                            count(2, inf, skF(skIsCuteIsX_0FnSk, vv(KB, X))),
%                            _39514024)))
%           )
%       ;   ( ( ( n(neg(puppy(_39511688))),
%                 n(neg(cute(_39511688)))
%               ),
%               n(neg(puppy(_39538616))),
%               n(neg(cute(_39538616)))
%             ),
%             n(neg(different(_39511688, _39538616)))
%           ),
%           ( n(neg(puppy(_39535680))),
%             n(neg(cute(_39535680)))
%           ),
%           n(neg(different(_39511688, _39535680)))
%       ).
% tlog_nnf_out_negated :-
%       n(~puppy(X))&n(~cute(X))&n(nesc(skolem(X, count(2, inf, skF(skIsCuteIsX_0FnSk, vv(KB, X))), _39514024)))v(n(nesc(puppy(X)))v n(nesc(cute(X)))&n(~skolem(X, count(2, inf, skF(skIsCuteIsX_0FnSk, vv(KB, X))), _39514024)))v(n(~puppy(_39511688))&n(~cute(_39511688))&(n(~puppy(_39538616))&n(~cute(_39538616)))&n(~different(_39511688, _39538616))&(n(~puppy(_39535680))&n(~cute(_39535680))&n(~different(_39511688, _39535680)))).
proven_neg(cute(Cute1)) :-
        nesc(puppy(Cute1)),
        nesc(puppy(Puppy2)),
        nesc(cute(Puppy2)),
        dif_objs(Cute1, Puppy2),
        nesc(puppy(Puppy3)),
        dif_objs(Cute1, Puppy3),
        nesc(cute(Puppy3)).
proven_neg(puppy(Cute1)) :-
        nesc(cute(Cute1)),
        nesc(puppy(Puppy2)),
        nesc(cute(Puppy2)),
        dif_objs(Cute1, Puppy2),
        nesc(puppy(Puppy3)),
        dif_objs(Cute1, Puppy3),
        nesc(cute(Puppy3)).
proven_neg(different(Cute1, Puppy3)) :-
        nesc(puppy(Puppy3)),
        nesc(cute(Puppy3)),
        nesc(puppy(Cute1)),
        nesc(cute(Cute1)),
        nesc(puppy(Puppy2)),
        dif_objs(Cute1, Puppy2),
        nesc(cute(Puppy2)).
proven_tru(cute(X)) :-
        skolem(X, count(2, inf, skF(skIsCuteIsX_0FnSk, vv(KB, X))), KB).
proven_tru(puppy(X)) :-
        skolem(X, count(2, inf, skF(skIsCuteIsX_0FnSk, vv(KB, X))), KB).
make_existential(X, count(2, inf, skF(skIsCuteIsX_0FnSk, vv(KB, X))), KB) :-
        ensure_cond(X, puppy(X)),
        ensure_cond(X, cute(X)).
% kbi_define(cute6:cute/1).
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/exactly_poss_cute_06.pfc.pl:17
% :- test_boxlog(exactly(5, X_VAR, puppy(X_VAR))).
% kif :-
%       exactly(5, X, puppy(X)).
% nnf :-
%       ~puppy(X)v skolem(X, count(5, inf, skF(skIsX_0FnSk, vv(KB, X))), _40260990)&(puppy(X)v~skolem(X, count(5, inf, skF(skIsX_0FnSk, vv(KB, X))), _40260990))&(~puppy(_40274182)v~different(_40259278, _40274182)v(~puppy(_40276474)v~different(_40259278, _40276474)v(~puppy(_40278766)v~different(_40259278, _40278766)v(~puppy(_40259278)v~puppy(_40283350)v~different(_40259278, _40283350)v(~puppy(_40281058)v~different(_40259278, _40281058)))))).
% to_tnot :-
%       ( (   neg(puppy(X))
%         ;   nesc(skolem(X, count(5, inf, skF(skIsX_0FnSk, vv(KB, X))), _40260990))
%         ),
%         (   nesc(puppy(X))
%         ;   neg(skolem(X, count(5, inf, skF(skIsX_0FnSk, vv(KB, X))), _40260990))
%         )
%       ),
%       (   (   neg(puppy(_40274182))
%           ;   neg(different(_40259278, _40274182))
%           )
%       ;   (   neg(puppy(_40276474))
%           ;   neg(different(_40259278, _40276474))
%           )
%       ;   (   neg(puppy(_40278766))
%           ;   neg(different(_40259278, _40278766))
%           )
%       ;   (   (   neg(puppy(_40259278))
%               ;   neg(puppy(_40283350))
%               )
%           ;   neg(different(_40259278, _40283350))
%           )
%       ;   neg(puppy(_40281058))
%       ;   neg(different(_40259278, _40281058))
%       ).
% tlog_nnf :-
%       (   (   n(neg(puppy(X))),
%               n(nesc(skolem(X,
%                            count(5, inf, skF(skIsX_0FnSk, vv(KB, X))),
%                            _40260990)))
%           ;   n(nesc(puppy(X))),
%               n(neg(skolem(X,
%                            count(5, inf, skF(skIsX_0FnSk, vv(KB, X))),
%                            _40260990)))
%           )
%       ;   ( n(neg(puppy(_40274182))),
%             n(neg(different(_40259278, _40274182)))
%           ),
%           ( n(neg(puppy(_40276474))),
%             n(neg(different(_40259278, _40276474)))
%           ),
%           ( n(neg(puppy(_40278766))),
%             n(neg(different(_40259278, _40278766)))
%           ),
%           ( ( n(neg(puppy(_40259278))),
%               n(neg(puppy(_40283350)))
%             ),
%             n(neg(different(_40259278, _40283350)))
%           ),
%           n(neg(puppy(_40281058))),
%           n(neg(different(_40259278, _40281058)))
%       ).
% tlog_nnf_out_negated :-
%       n(~puppy(X))&n(nesc(skolem(X, count(5, inf, skF(skIsX_0FnSk, vv(KB, X))), _40260990)))v(n(nesc(puppy(X)))&n(~skolem(X, count(5, inf, skF(skIsX_0FnSk, vv(KB, X))), _40260990)))v(n(~puppy(_40274182))&n(~different(_40259278, _40274182))&(n(~puppy(_40276474))&n(~different(_40259278, _40276474))&(n(~puppy(_40278766))&n(~different(_40259278, _40278766))&(n(~puppy(_40259278))&n(~puppy(_40283350))&n(~different(_40259278, _40283350))&(n(~puppy(_40281058))&n(~different(_40259278, _40281058))))))).
proven_neg(puppy(Puppy1)) :-
        nesc(puppy(Puppy2)),
        dif_objs(Puppy1, Puppy2),
        nesc(puppy(Puppy3)),
        dif_objs(Puppy1, Puppy3),
        nesc(puppy(Puppy4)),
        dif_objs(Puppy1, Puppy4),
        nesc(puppy(Puppy5)),
        dif_objs(Puppy1, Puppy5),
        dif_objs(Puppy1, Puppy6),
        nesc(puppy(Puppy6)).
proven_neg(different(Puppy1, Puppy6)) :-
        nesc(puppy(Puppy6)),
        nesc(puppy(Puppy5)),
        dif_objs(Puppy1, Puppy5),
        nesc(puppy(Puppy4)),
        dif_objs(Puppy1, Puppy4),
        nesc(puppy(Puppy1)),
        nesc(puppy(Puppy2)),
        dif_objs(Puppy1, Puppy2),
        dif_objs(Puppy1, Puppy3),
        nesc(puppy(Puppy3)).
proven_tru(puppy(X)) :-
        skolem(X, count(5, inf, skF(skIsX_0FnSk, vv(KB, X))), KB).
make_existential(X, count(5, inf, skF(skIsX_0FnSk, vv(KB, X))), KB) :-
        ensure_cond(X, puppy(X)).
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/exactly_poss_cute_06.pfc.pl:20
% :- test_boxlog(forall(X_VAR, iff(cute(X_VAR), not(ugly(X_VAR))))).
% kif :-
%       all(X,  (cute(X)<=> ~ugly(X))).
% nnf :-
%       ~cute(X)v~ugly(X)&(ugly(X)v cute(X)).
% to_tnot :-
%       (   neg(cute(X))
%       ;   neg(ugly(X))
%       ),
%       (   nesc(ugly(X))
%       ;   nesc(cute(X))
%       ).
% tlog_nnf :-
%       (   n(neg(cute(X))),
%           n(neg(ugly(X)))
%       ;   n(nesc(ugly(X))),
%           n(nesc(cute(X)))
%       ).
% tlog_nnf_out_negated :-
%       n(~cute(X))&n(~ugly(X))v(n(nesc(ugly(X)))&n(nesc(cute(X)))).
proven_neg(cute(X)) :-
        nesc(ugly(X)).
proven_neg(ugly(X)) :-
        nesc(cute(X)).
proven_tru(cute(X)) :-
        falsify(ugly(X)).
proven_tru(ugly(X)) :-
        falsify(cute(X)).
% kbi_define(cute6:ugly/1).
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/exactly_poss_cute_06.pfc.pl:23
% :- test_boxlog(forall(X_VAR, if(puppy(X_VAR), ugly(X_VAR)v cute(X_VAR)))).
% kif :-
%       all(X,  (puppy(X)=>ugly(X)v cute(X))).
% nnf :-
%       ~puppy(X)v(ugly(X)v cute(X)).
% to_tnot :-
%       (   neg(puppy(X))
%       ;   nesc(ugly(X))
%       ;   nesc(cute(X))
%       ).
% tlog_nnf :-
%       n(neg(puppy(X))),
%       n(nesc(ugly(X))),
%       n(nesc(cute(X))).
% tlog_nnf_out_negated :-
%       n(~puppy(X))&(n(nesc(ugly(X)))&n(nesc(cute(X)))).
proven_neg(puppy(X)) :-
        falsify(ugly(X)),
        falsify(cute(X)).
proven_tru(cute(X)) :-
        falsify(ugly(X)),
        nesc(puppy(X)).
proven_tru(ugly(X)) :-
        falsify(cute(X)),
        nesc(puppy(X)).
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/exactly_poss_cute_06.pfc.pl:28
% :- test_boxlog({findall(X_VAR, puppy(X_VAR), L_VAR), length(L_VAR, 5)}).
% kif :-
%       { findall(X, puppy(X), L),
%         length(L, 5)
%       }.
% ensure_quantifiers :-
%       all(L, all(X, {findall(X, puppy(X), L), length(L, 5)})).
% nnf :-
%       {findall(X, puppy(X), L)}&{length(L, 5)}.
% to_tnot :-
%       nesc({findall(X, puppy(X), L)}),
%       nesc({length(L, 5)}).
% tlog_nnf :-
%       (   n(nesc({findall(X, puppy(X), L)}))
%       ;   n(nesc({length(L, 5)}))
%       ).
% tlog_nnf_out_negated :-
%       n(nesc({findall(X, puppy(X), L)}))v n(nesc({length(L, 5)})).
nesc({length(L, 5)}).
nesc({findall(X, puppy(X), L)}).
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/exactly_poss_cute_06.pfc.pl:31
% :- test_boxlog({ findall(X_VAR,
%                        ( puppy(X_VAR),
%                          cute(X_VAR)
%                        ),
%                        L_VAR),
%                length(L_VAR, 2)
%              }).
% kif :-
%       { findall(X,
%                 ( puppy(X),
%                   cute(X)
%                 ),
%                 L),
%         length(L, 2)
%       }.
% ensure_quantifiers :-
%       all(L, all(X, {findall(X,  (puppy(X), cute(X)), L), length(L, 2)})).
% nnf :-
%       {findall(X, puppy(X)&cute(X), L)}&{length(L, 2)}.
% to_tnot :-
%       nesc({findall(X,  (puppy(X), cute(X)), L)}),
%       nesc({length(L, 2)}).
% tlog_nnf :-
%       (   n(nesc({findall(X,  (puppy(X), cute(X)), L)}))
%       ;   n(nesc({length(L, 2)}))
%       ).
% tlog_nnf_out_negated :-
%       n(nesc({findall(X, puppy(X)&cute(X), L)}))v n(nesc({length(L, 2)})).
nesc({length(L, 2)}).
nesc({findall(X,  (puppy(X), cute(X)), L)}).
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/exactly_poss_cute_06.pfc.pl:34
% :- test_boxlog({ findall(X_VAR,
%                        ( puppy(X_VAR),
%                          poss(cute(X_VAR))
%                        ),
%                        L_VAR),
%                length(L_VAR, 3)
%              }).
% kif :-
%       { findall(X,
%                 ( puppy(X),
%                   poss(cute(X))
%                 ),
%                 L),
%         length(L, 3)
%       }.
% ensure_quantifiers :-
%       all(L, all(X, {findall(X,  (puppy(X), poss(cute(X))), L), length(L, 3)})).
% nnf :-
%       { findall(X, puppy(X)&poss(cute(X)), L)&length(L, 3)
%       }.
% to_tnot :-
%       nesc({findall(X,  (puppy(X), poss(cute(X))), L), length(L, 3)}).
% tlog_nnf :-
%       n(nesc({findall(X,  (puppy(X), poss(cute(X))), L), length(L, 3)})).
% tlog_nnf_out_negated :-
%       n(nesc({findall(X, puppy(X)&poss(cute(X)), L)&length(L, 3)})).
nesc({findall(X,  (puppy(X), poss(cute(X))), L), length(L, 3)}).
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/exactly_poss_cute_06.pfc.pl:37
% :- test_boxlog({ findall(X_VAR,
%                        ( puppy(X_VAR),
%                          poss(ugly(X_VAR))
%                        ),
%                        L_VAR),
%                length(L_VAR, 1)
%              }).
% kif :-
%       { findall(X,
%                 ( puppy(X),
%                   poss(ugly(X))
%                 ),
%                 L),
%         length(L, 1)
%       }.
% ensure_quantifiers :-
%       all(L, all(X, {findall(X,  (puppy(X), poss(ugly(X))), L), length(L, 1)})).
% nnf :-
%       { findall(X, puppy(X)&poss(ugly(X)), L)&length(L, 1)
%       }.
% to_tnot :-
%       nesc({findall(X,  (puppy(X), poss(ugly(X))), L), length(L, 1)}).
% tlog_nnf :-
%       n(nesc({findall(X,  (puppy(X), poss(ugly(X))), L), length(L, 1)})).
% tlog_nnf_out_negated :-
%       n(nesc({findall(X, puppy(X)&poss(ugly(X)), L)&length(L, 1)})).
nesc({findall(X,  (puppy(X), poss(ugly(X))), L), length(L, 1)}).
% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/exactly_poss_cute_06.pfc.pl:39
% kbi_define(cute6:option_setting/2).
% init_why(program).
% start_x_ide
% after_boot
% autoloading editline:use_foreign_library/1 from /home/prologmud_server/lib/swipl-7.5.15/library/shlib
cute6:  ?-


% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/452 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/exactly_puppy_02.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/EXACTLY_PUPPY_02/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3AEXACTLY_PUPPY_02 

