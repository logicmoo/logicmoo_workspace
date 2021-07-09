
%:- mpred_unload_file.
:- set_fileAssertMt(baseKB).
% ensure this file does not get unloaded with mpred_reset
:- prolog_load_context(file,F), ain(mpred_unload_option(F,never)).
:- expects_dialect(pfc).

% asserting mpred_sv(p,2) causes p/2 to be treated as a mpred_sv, i.e.
% if p(foo,1)) is a fact and we assert_db p(foo,2), then the forrmer assertion
% is retracted.
% prologSingleValued(Pred)
:- kb_global(baseKB:mpred_sv/2).
arity(mpred_sv,2).
mpred_sv(Pred,Arity)==> arity(Pred,Arity),hybrid_support(Pred,Arity),singleValuedInArg(Pred,Arity).

:- dynamic(mpred_sv_shared/2).
mpred_sv_shared(Pred,Arity)==>({kb_shared(Pred/Arity)},mpred_sv(Pred,Arity)).
mpred_sv_shared(mpred_sv,2).
mpred_sv_shared(singleValuedInArg,2).
mpred_sv_shared(singleValuedInArgDefault,3).

( prologSingleValued(Pred), arity(Pred,Arity)/ ( \+ singleValuedInArg(Pred,_)))
   ==> singleValuedInArg(Pred,Arity).

%( \+ singleValuedInArg(Pred,_), prologSingleValued(Pred), arity(Pred,Arity)) 
%   ==> singleValuedInArg(Pred,Arity).

% THIS HAS A BUG!?!
%( prologSingleValued(Pred), arity(Pred,Arity), \+ singleValuedInArg(Pred,_))
%   ==> singleValuedInArg(Pred,Arity).


% prologSingleValued(Pred),arity(Pred,Arity) ==> hybrid_support(Pred,Arity).
% mdefault(((prologSingleValued(Pred),arity(Pred,Arity))==> singleValuedInArg(Pred,Arity))).
% singleValuedInArg(singleValuedInArg,2).

% TODO might we say this?
% Not really due to not every SingleValued pred have a cardinatity of 1 .. some might have no instances
% ((singleValuedInArg(Pred,N)/ ( \+ singleValuedInArgDefault(Pred,N,_))) ==> singleValuedInArgDefault(Pred,N,isMissing)).


arity(singleValuedInArgDefault, 3).
prologHybrid(singleValuedInArgDefault(prologSingleValued,ftInt,ftTerm)).
singleValuedInArg(singleValuedInArgDefault,3).

(singleValuedInArg(Pred,_)==>prologSingleValued(Pred)).

((singleValuedInArgDefault(SingleValued,ArgN,S1)/ground(S1)) ==> singleValuedInArg(SingleValued,ArgN)).

:- kb_shared(singleValuedInArgAX/3).

(singleValuedInArg(F, N),arity(F,A)) ==> singleValuedInArgAX(F,A,N).

%:- rtrace.
((singleValuedInArgAX(F,A,N), 
   {functor(P,F,A),arg(N,P,P_SLOT),replace_arg(P,N,Q_SLOT,Q)})
       ==> 
  ((( P,{P_SLOT\=isMissing, 
        call(dif:dif(Q_SLOT,P_SLOT)),call_u(Q),ground(Q)},Q)
        ==> (\+ Q, P)))).
%:- notrace,break.

unused ==> 
((singleValuedInArgAX(F,A,N), 
   {functor(P,F,A),arg(N,P,P_SLOT),replace_arg(P,N,Q_SLOT,Q)})
       ==> 
  (( P/ground(P) ==> 
        ({P_SLOT\==isMissing,call(dif:dif(Q_SLOT,P_SLOT) ),
          call_u(Q),ground(Q),ignore(retract(Q))})))).

unused ==> ((singleValuedInArgAX_maybe(F,A,N), 
   {functor(P,F,A),arg(N,P,P_SLOT),replace_arg(P,N,Q_SLOT,Q)})
       ==> 
  (( P/ground(P) ==> 
        ({P_SLOT\==isMissing,call(dif:dif(Q_SLOT,P_SLOT)),
          call_u(Q),ground(Q)}, (\+ Q, P, single_override_maybe(P,Q)))))).

((single_override(P,Q), {ignore(retract(Q))}) ==>
   ( (\+ P ==> (( \+ single_override(P,Q)), {mpred_supported(Q)}, Q)) )).


end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.


/*
(singleValuedInArgAX_0(F,A,N), 
   {functor(P,F,A),arg(N,P,P_SLOT),replace_arg(P,N,Q_SLOT,Q)})
       ==> 
  (( P ==> 
        ({call(dif:dif(Q_SLOT,P_SLOT)),call_u(Q),ground(Q), ignore(retract(Q))},  mdefault(Q)))).


(single_override0(P,Q), {retract(Q)}) ==>  mdefault(Q),\+ ~P.

((single_override1(P,Q),{ignore(retract(Q))}) ==> ( ( \+ P) ==> {}, Q)).

((single_override3(P,Q), {ignore(retract(Q))}) ==>
   (\+ P ==> (( \+ single_override(P,Q)), Q))).

((single_override4(P,Q), {ignore(retract(Q))}) ==>
   (\+ P ==> mdefault(Q))).
((single_override(P,Q)) ==> (mdefault(Q),P)).
(((single_override(P,Q), {ignore(retract(Q))}), \+P) ==> Q).

*/
% (\+ single_override(_P , Q)/nonvar(Q)) ==> Q.
/*
(singleValuedInArgAX(F,A,N), 
   {functor(P,F,A),arg(N,P,P_SLOT),replace_arg(P,N,Q_SLOT,Q)})
       ==> 
  (( P ==> 
        ({call(dif:dif(Q_SLOT,P_SLOT)),call_u(Q),ground(Q)}, single_override(P,Q)))).

*/







end_of_file.


:- if((fail,flag_call(runtime_debug == true) ;baseKB:startup_option(datalog,sanity);baseKB:startup_option(clif,sanity))).

:- ensure_loaded(pack(logicmoo_base/t/examples/pfc/'sanity_sv.pfc')).

:- endif.





% This would been fun! singleValuedInArgDefault(singleValuedInArgDefault,3,isMissing).

compilerDirective(somtimesBuggyFwdChaining,comment("Can sometimes be buggy when FwdChaining")).
compilerDirective(somtimesBuggyBackChaining,comment("Can sometimes be buggy when BackChaining")).
compilerDirective(somtimesBuggy,comment("Can sometimes be buggy")).

((somtimesBuggyFwdChaining==> ((
  ((singleValuedInArgDefault(P, 2, V), arity(P,2), argIsa(P,1,Most)) ==> relationMostInstance(P,Most,V)))))).


(somtimesBuggyFwdChaining==>
 ({FtInt=2},singleValuedInArgDefault(PrologSingleValued,FtInt,FtTerm),arity(PrologSingleValued,FtInt),
  argIsa(PrologSingleValued,1,Col)==>relationMostInstance(PrologSingleValued,Col,FtTerm))).

somtimesBuggyBackChaining ==> (((singleValuedInArgDefault(F, N, Q_SLOT)/is_ftNonvar(Q_SLOT), arity(F,A),
   {functor(P,F,A),replace_arg(P,N,Q_SLOT,Q),replace_arg(Q,N,R_SLOT,R)})
       ==> mdefault( Q <- ({ground(P)},~R/nonvar(R_SLOT))))).

(((singleValuedInArg(F, N), arity(F,A), \+ singleValuedInArgDefault(F, N, Q_SLOT),
   {functor(P,F,A),arg(N,P,P_SLOT),replace_arg(P,N,Q_SLOT,Q)})
       ==> (( P ==> 
          {dif:dif(Q_SLOT,P_SLOT), call_u(Q), ground(Q)}, \+Q, P)))).

sometimesBuggy==>((singleValuedInArgDefault(P, 2, V), arity(P,2), argIsa(P,1,Most)) <==> relationMostInstance(P,Most,V)).


