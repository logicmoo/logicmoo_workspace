%:- set_module(class(development)).
:- '$set_source_module'(baseKB).
:- use_module(library(pfc)).


:- sanity(ttRelationType(prologMultiValued)).

:- kb_shared(argsIsa/2).

feature_setting(N,V)/(feature_setting(N,Other),Other\==V)==> \+ feature_setting(N,Other). 

feature_setting(assume_wff, true).

% set false so make_wff/1 will be noticed (default is true)
feature_setting(make_wff,true)==> (feature_setting(add_admitted_arguments,true), feature_setting(assume_wff, false)).
feature_setting(add_admitted_arguments,true) ==> 
 ( (P/(compound(P),\+is_ftVar(P)) ==> 
  {with_current_why(P,ignore(\+ addAdmittedArguments(P)))})).

feature_setting(make_wff,true)==> 
 ((argIsa(P, N, T)/(nonvar(T),nonvar(P),integer(N)))==>
          (tCol(T),
          (admittedArgument(P, N, E)/nonvar(E)==> isa(E,T)),
          (poss(admittedArgument(P, N, E))/nonvar(E)==> (isa(E,T))))).

make_wff(true)==> (P/(compound(P),\+is_ftVar(P)) ==> {with_current_why(P,ignore(\+ deduceEachArgType(P)))}).

% default is false
% ==> feature_setting(add_admitted_arguments,true).

% default is false
==> feature_setting(admitted_arguments_modal,false).

prologHybrid(argIsa/3).

:- asserta(t_l:pfcExpansion).




%% argIsa( ?F, ?N, ?Type) is semidet.
%
% asserted Argument  (isa/2) known.
%

% WEIRD .. is needed? argIsa(F/_,N,Type):- nonvar(F),!,argIsa(F,N,Type).
argIsa(F,N,Type):- var(F),!,tRelation(F),argIsa(F,N,Type).
argIsa(F,N,Type):- var(N),arity_no_bc(F,A),!,system_between(1,A,N),argIsa(F,N,Type).
%argIsa(F,1,F):- tCol(F), arity_no_bc(F,1),!.
% Managed Arity Predicates.
% argIsa(Pred,N,ftVoprop) :- number(N),arity_no_bc(Pred,A),N>A,!.

==>argIsa(isEach(arity,arityMax,arityMin),2,ftInt).

/*
argIsa(F,_,ftTerm):-member(F/_, [argIsa/3,predProxyAssert/2,negate_wrapper0/2,mudFacing/_,registered_module_type/2,       
                                ruleBackward/2,formatted_resultIsa/2, '$pt'/_,rhs/_,'$nt'/_,'$bt'/_,bracket/3]),!.
argIsa(Prop,N1,Type):- is_2nd_order_holds(Prop),dmsg(todo(define(argIsa(Prop,N1,'Second_Order_TYPE')))),dumpST,dtrace,Type=argIsaFn(Prop,N1),!.
*/
/*
$mycont.set({V1=$a.value,V2=$b.value}/(VarIn)>>writeln(my_cont(V1,V2,VarIn))).
writeln($mycont).
*/

:- kb_shared(mpred_f/5).
:- kb_shared(mpred_f/6).
:- kb_shared(mpred_f/4).
:- kb_shared(mpred_f/7).

% :- rtrace.
%% argQuotedIsa( ?F, ?N, ?FTO) is semidet.
%
% Argument  (isa/2) Format Type.
%
:- kb_shared(argQuotedIsa/3).
prologHybrid(argQuotedIsa(tRelation,ftInt,ttExpressionType)).

% :- listing(argQuotedIsa/3).
% :- break.
% argQuotedIsa(F/_,N,Type):-nonvar(F),!,argQuotedIsa(F,N,Type).
argQuotedIsa(F,N,FTO):- argIsa(F,N,FT), must(to_format_type(FT,FTO)),!.
:- nortrace.

:- was_export(argIsa/3).

%= 	 	 

%% argIsa( ?F, ?N, ?Type) is semidet.
%
% Argument  (isa/2) call  Primary Helper.
%
argIsa(argIsa,1,tRelation).
argQuotedIsa(argIsa,2,ftInt).
argIsa(argIsa,3,tCol).  
argQuotedIsa(comment,2,ftString).
argQuotedIsa(isKappaFn,1,ftVar).
argQuotedIsa(isKappaFn,2,ftAskable).
%argIsa(isInstFn,1,tCol).


argQuotedIsa(quotedDefnIff,1,ftSpec).
argQuotedIsa(quotedDefnIff,2,ftCallable).
argQuotedIsa(meta_argtypes,1,ftSpec).


argIsa(isa,2,tCol).
%argIsa(mpred_isa,1,tPred).
%argIsa(mpred_isa,2,ftVoprop).
% argIsa(mpred_isa,3,ftVoprop).

argIsa(formatted_resultIsa,1,ttExpressionType).
argIsa(formatted_resultIsa,2,tCol).

argIsa(predicates,1,ftListFn(ftTerm)).
argIsa(resultIsa,2,tCol).

argIsa(predTypeMax,1,tPred).
argIsa(predTypeMax,2,tCol).
argIsa(predTypeMax,3,ftInt).

argIsa(predInstMax,1,tObj).
argIsa(predInstMax,2,tPred).
argQuotedIsa(predInstMax,3,ftInt).

argQuotedIsa(props,1,ftID).
argQuotedIsa(props,N,ftVoprop):- integer(N), system_between(2,31,N).

argIsa(apathFn,1,tRegion).
argIsa(apathFn,2,vtDirection).
argIsa(localityOfObject,1,tObj).
argIsa(localityOfObject,2,tSpatialThing).

argIsa(typeProps,1,tCol).
argIsa(typeProps,N,ftVoprop):-system_between(2,31,N).

argQuotedIsa(instTypeProps,1,ftProlog).
argIsa(instTypeProps,2,tCol).
argQuotedIsa(instTypeProps,N,ftVoprop):-system_between(3,31,N).


argIsa(must,1,ftCallable).

%:- break.
(argsIsa(F,Type),arity(F,A),{system_between(1,A,N)})==>argIsa(F,N,Type).



% argIsa(baseKB:agent_text_command,_,ftTerm).


argIsa('<=>',_,ftTerm).
argIsa(class_template,N,Type):- (N=1 -> Type=tCol;Type=ftVoprop).
==>argIsa(isEach(descriptionHere,mudDescription,nameString,mudKeyword),2,ftString).

% argQuotedIsa(F,N,Type)==>argIsa(F,N,Type).

% argQuotedIsa(F,N,Type):- functorDeclares(F),(N=1 -> Type=F ; Type=ftVoprop).
%argIsa(F,N,Type):- t(tCol,F),!,(N=1 -> Type=F ; Type=ftVoprop).
% :- sanity(listing(argQuotedIsa/3)).

/*
{source_file(M:P,_),functor(P,F,A),
  \+ predicate_property(M:P,imported_from(_))} 
   ==> functor_module(M,F,A).
:- show_count(functor_module/3).
*/

:- dynamic(functor_module/3).
rtArgsVerbatum(functor_module).


:- if((current_prolog_flag(runtime_debug,D),D>1)).
:- show_count(arity/2).
:- endif.


%= 	 	 

%% argsIsa( ?WP, ?VALUE2) is semidet.
%
% Argument  (isa/2) call Helper number 3..
%
==>argsIsa(isEach(predProxyRetract,predProxyAssert,predProxyQuery,genlInverse),tPred).
argsIsa(disjointWith,tCol).
argQuotedIsa(ftFormFn,ftTerm).
argQuotedIsa(mudTermAnglify,ftTerm).
argsIsa(genls,tCol).
argsIsa(subFormat,ttExpressionType).




