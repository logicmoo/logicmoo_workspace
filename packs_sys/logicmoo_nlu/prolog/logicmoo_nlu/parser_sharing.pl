% ===================================================================
% File 'parser_all.pl'
% Purpose: English to KIF conversions from SWI-Prolog  
% This implementation is incomplete
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'parser_all.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2002/06/06 15:43:15 $
% ===================================================================


:- module(parser_sharing,[
  % term_expansion/4,
   op(1150,fx,(share_mp)),
   op(1150,fx,(shared_parser_data)),
   op(1150,fx,(dynamic_multifile_exported))]).
/*
:- op(1150,fx,(share_mp)),
   op(1150,fx,(shared_parser_data)),
   op(1150,fx,(dynamic_multifile_exported)).
*/

% :- ensure_loaded(nl_pipeline).

:- absolute_file_name('../../ext/',Dir,[file_type(directory)]),
   asserta_new(user:file_search_path(logicmoo_nlu_ext,Dir)).
:- absolute_file_name('../../ext/',Dir,[file_type(directory)]),
   asserta_new(user:file_search_path(logicmoo,Dir)).
:- absolute_file_name('../../ext/pldata/',Dir,[file_type(directory)]),
   asserta_new(user:file_search_path(pldata,Dir)).



if_giveup_dcg(S,_):- var(S),!.
if_giveup_dcg(_,_,S,_):- var(S),!.
%nl_pred(F/A):-
% def_nl_pred(M,share_mp,A):- dumpST,break.
   
def_nl_pred(M,F,A):- 
  assert_if_new(nlfac:is_nl_pred(M,F,A)).

:- export(nl_call/1).
:- export(nl_call/2).
:- export(nl_call/3).
:- export(nl_call/4).
:- export(nl_call/5).
:- export(nl_call/6).
:- export(nl_call/7).
:- export(nl_call/8).
nl_call([F|Rest]):- !, nlfac:is_nl_pred(M,F,N),/*var(Rest)->*/length(Rest,N),M:apply(F,Rest).
nl_call(M:P):-!,nl_call_mp(M,P).
nl_call(P):- nl_call_mp(_,P).

nl_call_mp(M,P):- (nl_pred(P,M,F,A),nl_call_entr(M,F,A,P),      
   (nl_pred(P,M,F,A),on_x_rtrace((M:P))),nl_call_exit(M,F,A,P))*->true;
  (current_predicate(_,M:P),call(M:P)).

nl_pred(M,F,A):- var(F),!,nlfac:is_nl_pred(M,F,A).
nl_pred(M,F,A):- (nlfac:is_nl_pred(M,F,A))*->true;((must(current_predicate(M:F/A)),def_nl_pred(M,F,A)),!).

nl_pred(P,M,F,A):- (var(F),var(P)),!,nlfac:is_nl_pred(M,F,A),functor(P,F,A).
nl_pred(P,M,F,A):- var(F),!,functor(P,F,A),ignore(nlfac:is_nl_pred(M,F,A)).
nl_pred(P,M,F,A):- var(P),!,nl_pred(M,F,A),functor(P,F,A).
nl_pred(P,M,F,A):- functor(P,F,A),!,nl_pred(M,F,A).


nl_call_trusted([F|Rest]):- !, nlfac:is_nl_pred(M,F,N),/*var(Rest)->*/length(Rest,N),M:apply(F,Rest).
nl_call_trusted(M:P):-!,nl_call_trusted_mp(M,P).
nl_call_trusted(P):- nl_call_trusted_mp(_,P).

nl_call_trusted_mp(M,P):- (nl_pred_trusted(P,M,F,A),nl_call_entr(M,F,A,P),      
   (nl_pred_trusted(P,M,F,A),on_x_rtrace((M:P))),nl_call_exit(M,F,A,P))*->true;
  (current_predicate(_,M:P),call(M:P)).
nl_pred_trusted(M,F,A):- var(F),!,nlfac:is_nl_pred(M,F,A).
nl_pred_trusted(M,F,A):- (nlfac:is_nl_pred(M,F,A))*->true;(((current_predicate(M:F/A)),def_nl_pred(M,F,A)),!).

nl_pred_trusted(P,M,F,A):- (var(F),var(P)),!,nlfac:is_nl_pred(M,F,A),functor(P,F,A).
nl_pred_trusted(P,M,F,A):- var(F),!,functor(P,F,A),ignore(nlfac:is_nl_pred(M,F,A)).
nl_pred_trusted(P,M,F,A):- var(P),!,nl_pred_trusted(M,F,A),functor(P,F,A).
nl_pred_trusted(P,M,F,A):- functor(P,F,A),!,nl_pred_trusted(M,F,A).



make_nl_call_stubs:- forall((between(1,7,A),length(List,A),Head =.. [nl_call,F|List], Body =.. [call,M:F|List],   
   functor(Head,CF,CA),ignore(abolish(parser_sharing:CF,CA)),
   Enter =.. [nl_call_entr,M,F,A,Z],                                 
   Exit =.. [nl_call_exit,M,F,A,Z],
   nop(assertion( \+ predicate_property(parser_sharing:Head,(defined))))),
   ((dynamic(parser_sharing:CF/CA),
     assert(parser_sharing:(Head:- (nl_pred(M,F,A),Z=Body,Enter,Z,Exit))),
     module_transparent(parser_sharing:CF/CA),
     compile_predicates([parser_sharing:CF/CA]),
     export(parser_sharing:CF/CA)))).

nl_call_entr(_M,_F,_A,_P).
nl_call_exit(_M,_F,_A,_P).

:- make_nl_call_stubs.
% :- make_nl_call_stubs. 
%:- listing(nl_call).
% (predicate_property(P,number_of_clauses(N))
:- module_transparent(each_parser_module/1).
each_parser_module(M):- no_repeats(M,each_parser_module_0(M)).
:- module_transparent(each_parser_module_0/1).
each_parser_module_0(baseKB).
each_parser_module_0(parser_shared).
each_parser_module_0(parser_all).
each_parser_module_0(M):- each_parser_module_1(E),default_module(E,M).
%each_parser_module_0(M):- current_module(M).
:- module_transparent(each_parser_module_1/1).
each_parser_module_1(M):- strip_module(_,M,_).
each_parser_module_1(M):- '$current_source_module'(M).
each_parser_module_1(M):- '$current_typein_module'(M).
%each_parser_module_1(M):- current_module(M).

:- module_transparent(predicate_visible_home/2).
predicate_visible_home(H,M):- predicate_property(H,imported_from(M)),!.
predicate_visible_home(H,M):- predicate_property(H,defined),strip_module(H,M,_), \+ predicate_property(H,imported_from(_)),!.
predicate_visible_home(H,M):- each_parser_module(M),predicate_property(M:H,defined), \+ predicate_property(M:H,imported_from(_)),!. 

:- module_transparent(pi_p/2).
pi_p(X,_):- \+ compound(X),!,fail.
pi_p('//'(F,A),P):-!,atom(F),integer(A),AA is A +2,functor(P,F,AA).
pi_p(F/A,P):- !,atom(F),integer(A),functor(P,F,A).
pi_p(M:PI,M:P):-!,pi_p(PI,P).

:- module_transparent(pi_2_p/2).
pi_2_p(P,P):- \+ callable(P),!.
pi_2_p(M:I,M:I):-!.
pi_2_p(I,M:I):- pi_p(I,P),predicate_visible_home(P,M),!.
pi_2_p(P,P).


pi_splits(X,_,_):- \+ compound(X),!,fail.
pi_splits([X,Y],X,Y):-!.
pi_splits([],nil,nil):-!.
pi_splits([X],X,nil):-!.
pi_splits([X|Y],X,Y):-!,nonvar(X).
pi_splits((X,Y),X,Y):-!.
pi_splits(M:XY,M:X,M:Y):- pi_splits(XY,X,Y),!.


:- op(1150,fx,user:(share_mp)).
:- op(1150,fx,baseKB:(share_mp)).

:- module_transparent((share_mp)/1).
share_mp(X):- var(X),!,nop(dumpST),fail.
share_mp(nil):- !.
share_mp(XY):- pi_splits(XY,X,Y),!,share_mp(X),share_mp(Y).
share_mp(XY):- pi_p(XY,PI),!,share_mp(PI).
share_mp(MP):- strip_module(MP,M,P),share_mp(M,P).

import_and_export(CM,M:F/A):- 
   must(M:export(M:F/A)),
   (CM\==M-> CM:import(M:F/A) ; true),
   CM:export(M:F/A),!.


:- module_transparent((share_mp)/2).
share_mp(_,X):- var(X),!,nop(dumpST),fail.
share_mp(_,nil):-!.
share_mp(M,XY):- pi_splits(XY,X,Y),!,share_mp(M,X),share_mp(M,Y).
share_mp(CM,(M:P)):- !, atom(M),share_mp(M,P),(CM==M->true;import_and_export(CM,M:P)).
share_mp(M,PI):- pi_p(PI,P)->PI\==P,!,share_mp(M,P).
share_mp(M,P):- functor(P,F,A), share_mfa(M,F,A).

share_mfa(M,F,A):- MFA=M:F/A,   
   (M:multifile(MFA)), 
   (M:module_transparent(MFA)),
   (M:dynamic(MFA)),
   (M:export(MFA)),
   (M:public(MFA)),  
   def_nl_pred(M,F,A),
   share_mfa_pt2(M,F,A).

share_mfa_pt2(system,_,_):-!.
share_mfa_pt2(M,F,A):- MFA=M:F/A,   
 must_det_l((
   format(user_error,'~N% :- ~q.~n',[share_mfa_pt2(M,F,A)]),
   import_and_export(parser_sharing,MFA),
   import_and_export(parser_all,MFA),
   def_nl_pred(M,F,A),
   '$current_source_module'(SM),import_and_export(SM,MFA),
   '$current_typein_module'(CM),import_and_export(CM,MFA),
   import_and_export(system,MFA))),
   !.

:- share_mp((share_mp)/1).
:- share_mp((share_mp)/2).


:- op(1150,fx,user:(shared_parser_data)).
:- op(1150,fx,baseKB:(shared_parser_data)).
:- module_transparent((shared_parser_data)/1).

shared_parser_data(XY):- var(XY),!,nop(dumpST),fail.
shared_parser_data(XY):- assertion(compound(XY)),fail.
shared_parser_data(XY):- pi_splits(XY,X,Y),!,shared_parser_data(X),shared_parser_data(Y).
shared_parser_data(XY):- pi_p(XY,PI)-> XY\==PI,!,shared_parser_data(PI).
shared_parser_data(MP):- predicate_visible_home(MP,M)->strip_module(MP,Imp,P),MP\==M:P,!, functor(P,F,A),M:multifile(M:F/A),M:export(M:F/A),shared_parser_data(M:P),Imp:import(M:F/A).
shared_parser_data(M:P):- !,def_parser_data(M,P),strip_module(_,Imp,_),share_mp(M:P),functor(P,F,A),Imp:import(M:F/A).
% shared_parser_data(P):- each_parser_module(M),predicate_property(M:P,defined), \+ predicate_property(M:P,imported_from(_)),!,shared_parser_data(M:P).
shared_parser_data(P):-  get_query_from(SM),shared_parser_data(SM:P).
:- share_mp((shared_parser_data)/1).



:- op(1150,fx,user:(dynamic_multifile_exported)).
:- op(1150,fx,baseKB:(dynamic_multifile_exported)).
:- module_transparent((dynamic_multifile_exported)/1).
dynamic_multifile_exported(MP):- shared_parser_data(MP).
:- share_mp((dynamic_multifile_exported)/1).


:- module_transparent(find_predicate_module/2).
find_predicate_module(P,MP):-find_predicate_module_maybe(MP,P),!.
:- share_mp(find_predicate_module/2).

:- module_transparent(find_predicate_module_maybe/2).
find_predicate_module_maybe(MPO,F/A):-!,ground(F/A), functor(P,F,A),find_predicate_module_maybe(MPO,P).
find_predicate_module_maybe(MPO,M:F/A):-!, ground(F/A), functor(P,F,A),find_predicate_module_maybe(MPO,M:P).
find_predicate_module_maybe(M:P,MP):-  predicate_property(MP,imported_from(M)),!,strip_module(MP,_,P).
find_predicate_module_maybe(M:P,M:P):- !, predicate_property(M:P,defined), \+ predicate_property(M:P,imported_from(_)),!.
find_predicate_module_maybe(M:P,P):- each_parser_module(M),predicate_property(M:P,defined), \+ predicate_property(M:P,imported_from(_)),!.
find_predicate_module_maybe(MPO,P):- find_predicate_module_maybe(MPO,baseKB:P).
:- share_mp(find_predicate_module_maybe/2).

:- dynamic(using_shared_parser_data/2).
use_shared_parser_data(User,File):- parser_sharing:using_shared_parser_data(User,File),!.
use_shared_parser_data(User,File):- asserta(parser_sharing:using_shared_parser_data(User,File)),!.

:- module_transparent(use_shared_parser_data/0).
use_shared_parser_data:- 
   prolog_load_context(module,User),
   ignore((source_location(File,_), use_shared_parser_data(User,File))), 
   ignore((prolog_load_context(source,File2), use_shared_parser_data(User,File2))), 
   ignore((prolog_load_context(file,File3), use_shared_parser_data(User,File3))).

:- module_transparent(def_parser_data/2).
def_parser_data(M,F/A):- !, ground(F/A), assertion((atom(F),integer(A),functor(P,F,A))), def_parser_data(M,P).
def_parser_data(_,M:XY):- !, def_parser_data(M,XY).
def_parser_data(M,P):-
   use_shared_parser_data,
   functor(P,F,A),
   def_nl_pred(M,F,A),
   ( \+ predicate_property(M:P,defined) -> define_shared_loadable_pred(M,P) ; true ),!.   
/*
def_parser_data(M,P):- throw(old_code),
   ( \+ predicate_property(M:P,defined) -> define_shared_loadable_pred(M,P) ; true ),   
   kb_shared(M:P),
   share_mp(M,P).
*/
:- share_mp(def_parser_data/2).

:- module_transparent(define_shared_loadable_pred/2).
define_shared_loadable_pred(M,P):- current_prolog_flag(access_level,system),!,set_prolog_flag(access_level,user),
   define_shared_loadable_pred(M,P),set_prolog_flag(access_level,system).

% define_shared_loadable_pred(M,P):- !, mpred_ain(isBorked==>M:P).
define_shared_loadable_pred(M,F/A):- functor(P,F,A),define_shared_loadable_pred(M,P),!.
define_shared_loadable_pred(M,P):- % throw(old_code),
   '$current_source_module'(SM),'$current_typein_module'(CM),
   %mpred_ain(isBorked==>M:P),   
   functor(P,F,A),dmsg(def_parser_data(sm=SM,cm=CM,m=M,F/A)),
   def_nl_pred(M,F,A),
   MFA = M:F/A,
   dynamic(MFA),multifile(MFA),discontiguous(MFA).

:- module_transparent(show_shared_pred_info/1).
show_shared_pred_info(FA):- nonvar(FA),
   prolog_load_context(module,User),
   (pi_p(FA,P);P=FA),!,
   functor(P,F,A),      
   listing(nlfac:is_nl_pred(_,F,A)),
   ((User:predicate_property(P,defined))->
       (predicate_property(P,number_of_clauses(N)),
         (N<20 -> User:listing(FA) ; dmsg(big(User,F/A)));
    dmsg(unkonw_number_of_clauses(User,F/A)));dmsg(undefined(User,F/A))),
   findall(PP,User:predicate_property(P,PP),PPL),dmsg(FA=PPL),!.
:- share_mp(show_shared_pred_info/1).


:- module_transparent(importing_head/2).
importing_head(H,H):- \+ callable(H),!.
importing_head(M:H,M:H):- !.
importing_head(H,M:H):- predicate_visible_home(H,M),!.
importing_head(H,H).

:- module_transparent(importing_body/3).
importing_body(_CM,B,B):- \+ callable(B),!.
importing_body(CM,(A,B),(AA,BB)):-!,importing_body(CM,A,AA),importing_body(CM,B,BB).
importing_body(CM,B,BB):- compound(B),
  CM:predicate_property(B,meta_predicate(MP)),!,
  B=..[F|EL],MP=..[F|ML],
  maplist(importing_body_lit(CM,MP),ML,EL,EEL),
  BB=..[F|EEL].
importing_body(_CM,B,B).

:- module_transparent(importing_body_lit/5).
importing_body_lit(_CM,_B,_Me,E,E):- \+ callable(E),!.
importing_body_lit(CM,_B,Int,E,EE):- integer(Int),!,importing_body(CM,E,EE).
importing_body_lit(CM,_B,(*),E,EE):- !,importing_body(CM,E,EE).
importing_body_lit(CM, B,(:),[H|T],[HH|TT]):- Meta = (:), !,importing_body_lit(CM,B,Meta,H,HH),importing_body_lit(CM,B,Meta,T,TT).
importing_body_lit(CM, B,(:),E,M:E):- functor(B,_,1),pi_p(E,P),CM:predicate_visible_home(P,M),!.
%importing_body_lit(_,_B,(:),E,E):- !.
importing_body_lit(_CM,_B,_,E,E).


:- module_transparent(importing_clause_change/2).
importing_clause_change(I,OOO):- 
  notrace(importing_clause(I,O)),!, 
  differing_clauses(I,O,OOO),!.

:- module_transparent(differing_clauses/3).
differing_clauses(I,O,OOO):- 
 notrace((I\==O,
  expand_to_hb(I,IH,IB),
  expand_to_hb(O,OH,OB),
  strip_module(IH,WasIM,II),
  strip_module(OH,WasOM,OO),
  (II\==OO;IB\==OB;WasIM\==WasOM))),
  merge_hb(OO,OB,OHB),
  clean_te_module(IH,WasIM,II,OHB,WasOM,OOO),!.

merge_hb(OH,OB,OH):- OB==true,!.
merge_hb(M:OH,OB,M:(OH:-OB)):-!.
merge_hb(OH,OB,(OH:-OB)).

% clean_te_module(I,WasIM,II,O,WasOM,OO,OOO)
clean_te_module(I,WasIM,I,OO,WasOM,OO):- WasOM==WasIM,!.
clean_te_module(_, _   ,_,OO,WasOM,WasOM:OO).
  


:- module_transparent(importing_clause/2).
importing_clause(H,H):- \+ callable(H),!.
importing_clause((:-B),(:-BB)):- !,strip_module(B,CM,_),importing_body(CM,B,BB).
importing_clause((H:-B),(HH:-BB)):- !, importing_head(H,HH),!,strip_module(HH,CM,_),importing_body(CM,B,BB).
importing_clause((H), (H)):- is_leave_alone_ic(H),!.
importing_clause((H),(HH)):- importing_head(H,HH),!.
importing_clause((B),(BB)):- strip_module(B,CM,_),importing_body(CM,B,BB),!.
importing_clause(HB,HB).
:- share_mp(importing_clause/2).

is_leave_alone_ic(H):- compound(H),functor(H,F,A),is_leave_alone_ic(F,A).
%is_leave_alone_ic('--->',_).
%is_leave_alone_ic('-->',_).
is_leave_alone_ic(A,_):- upcase_atom(A,A).

:- export(try_maybe_p/1).
try_maybe_p(M:P):- P=..[F,_,R],!,try_maybe_f(F,M:P,R).
try_maybe_p(M:P):- P=..[F,_,_,R],!,try_maybe_f(F,M:P,R).
%try_maybe_p(M:P):- debugging, !, on_f_ftrace(M:P).
try_maybe_p(M:P):- !, call(M:P).
try_maybe_p(M:P):- !, P=..[F,_|List],try_maybe_pl(M:P,F,List).
try_maybe_p(P):-!,try_maybe_p(parser_chat80:P).

try_maybe_pl(_  ,F,List):- (member(E,List),compound(E),E=error(F,_)),!,print_reply(E),fail.
try_maybe_pl(M:P,F,List):- member(E,List),var(E),reverse(List,RList),member(R,RList),var(R),!,                 
  (R==E -> try_maybe_f(F,M:P,R);
  (try_maybe_f(F,M:P,E);try_maybe_f(F,M:P,R))).

:- export(try_maybe_p/3).
try_maybe_p(_,error(E1,E2),R):-!, (fail;true),R=error(E1,E2).
try_maybe_p(M:F,X,R):- P=..[F,X,R],!,try_maybe_f(F,M:P,R).

try_maybe_f(F,P,R):- P*->true;(fail;fail;R=error(F,P)).

%:- multifile(system:term_expansion/4).
%:- dynamic(system:term_expansion/4).
% system:term_expansion(G,I,GG,O):- notrace((nonvar(I),compound(G))),importing_clause_change(G,GG), I=O.

:- fixup_exports.
