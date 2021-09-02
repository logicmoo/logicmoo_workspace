
/*
:- ensure_loaded(plarkc(mpred_sexpr_reader)).

:- parse_to_source(
  "(documentation instance EnglishLanguage \"An object is an &%instance of a &%SetOrClass if it is included in that &%SetOrClass. 
  An individual may be an instance of many classes, some of which may be subclasses of others. 
  Thus, there is no assumption in the meaning of &%instance about specificity or uniqueness.\")",
  Out),writeq(Out).
*/


% KIF BASED
:- export((
         must_map_preds/3,
         map_each_subterm/3,
         if_changed/3,
         sumo_to_pdkb_p5/2,
         is_kif_string/1,
         from_kif_string/2,
         convert_if_kif_string/2,
         sumo_to_pdkb/2)).


:- meta_predicate(if_changed(2,+,-)).
:- meta_predicate(map_each_subterm(2,+,-)).

:- ensure_loaded(common_logic_utils).

delay_rule_eval(InOut,_Wrap,InOut):-ground(InOut),!.
delay_rule_eval(In,Wrap,WIn):- WIn=..[Wrap,In].

% for SUMO
sumo_to_pdkb_const('Collection','ttSumoCollection').
sumo_to_pdkb_const(format,formatSumo).
% sumo_to_pdkb_const(documentation,comment).
sumo_to_pdkb_const('instance', isa).
sumo_to_pdkb_const('subclass', genls).
sumo_to_pdkb_const('inverse', genlInverse).
sumo_to_pdkb_const('domain', 'argIsa').
sumo_to_pdkb_const('disjoint', 'disjointWith').

sumo_to_pdkb_const('Atom', 'tSumoAtomMolecule').

sumo_to_pdkb_const('range', 'resultIsa').
sumo_to_pdkb_const('domainSubclass', 'argGenl').
sumo_to_pdkb_const('rangeSubclass', 'resultGenl').
sumo_to_pdkb_const(immediateInstance,nearestIsa).
sumo_to_pdkb_const('partition', 'sumo_partition').
sumo_to_pdkb_const('Entity','tThing').
sumo_to_pdkb_const('ListFn',vTheListFn).
sumo_to_pdkb_const('ListOrderFn',vSumoListOrderFn).
sumo_to_pdkb_const('AssignmentFn',uFn).
sumo_to_pdkb_const('SymbolicString',ftString).
sumo_to_pdkb_const('property','sumoProperty').
sumo_to_pdkb_const('attribute','sumoAttribute').
sumo_to_pdkb_const('Attribute','vtSumoAttribute').
sumo_to_pdkb_const('EnglishLanguage','vEnglishLanguage').
sumo_to_pdkb_const('Formula','ftFormula').
sumo_to_pdkb_const('Function','tFunction').
sumo_to_pdkb_const(forall,all).
sumo_to_pdkb_const(subrelation,genlPreds).
sumo_to_pdkb_const('Class','tSet').
sumo_to_pdkb_const('baseKB','baseKB').
sumo_to_pdkb_const('SetOrClass', 'tCol').
sumo_to_pdkb_const(v,v).
sumo_to_pdkb_const(&,&).
sumo_to_pdkb_const(~,~).
sumo_to_pdkb_const(=>,=>).
sumo_to_pdkb_const(U,U):- downcase_atom(U,U).
sumo_to_pdkb_const(U,U):- upcase_atom(U,U).
sumo_to_pdkb_const(I,O):- if_defined(builtin_rn_or_rn_new(I,O)),!.




%% is_kif_string( ?String) is det.
%
% If Is A Knowledge Interchange Format String.
%
is_kif_string([]):- !,fail.
is_kif_string(String):-atomic(String),name(String,Codes), memberchk(40,Codes),memberchk(41,Codes).




%% convert_if_kif_string( ?I, ?O) is det.
%
% Convert If Knowledge Interchange Format String.
%
convert_if_kif_string(I, O):-is_kif_string(I),sumo_to_pdkb(I,O),!, \+ is_list(O).


last_chance_doc(Wff0,WffO):- will_mws(Wff0),string_to_mws(Wff0,MWS),last_chance_doc(MWS,WffO),!.
last_chance_doc(Wff0,comment(Atom,NewStr)):- 
   Wff0=..[s,"(", "documentation",AntisymmetricRelation, "EnglishLanguage", "\""|REST],
         append(NOQUOTES,[_,_],REST),
         string_to_atom(AntisymmetricRelation,Atom),
         NewStr =..[s|NOQUOTES],!.
last_chance_doc(IO,IO).


%% convert_1_kif_string( ?String, ?Forms) is det.
%
% Converted From Knowledge Interchange Format String.
%
convert_1_kif_string(I,Wff):- with_kifvars(input_to_forms(I,Wff,Vs))->must(put_variable_names(Vs)),!.

from_kif_string(Wff,Wff):- \+ atomic(Wff), \+ is_list(Wff),!.
from_kif_string(I,Wff) :- string(I),convert_1_kif_string(string(I),Wff),!.
from_kif_string(I,Wff) :- atom(I),atom_contains(I,' '),convert_1_kif_string(atom(I),Wff),!.
from_kif_string([C|String],Wff) :- is_list(String),text_to_string_safe([C|String],Text),one_must(convert_1_kif_string(string(Text),Wff),codelist_to_forms(string(Text),Wff)),!.
from_kif_string(Wff,Wff).

                                                      

:- module_transparent(must_map_preds/3).
must_map_preds([],IO,IO):-!.
must_map_preds([one(Pred)|ListOfPreds],In,Out):- !, (on_x_rtrace(call(Pred,In))->must_map_preds(ListOfPreds,In,Out);must_map_preds(ListOfPreds,In,Out)).
must_map_preds([Pred|ListOfPreds],In,Out):- on_x_rtrace(call(Pred,In,Mid))->must_map_preds(ListOfPreds,Mid,Out);must_map_preds(ListOfPreds,In,Out).


:- thread_local(t_l:no_db_expand_props/0).

fully_expand_always(C0,C1):- locally_tl(no_db_expand_props,fully_expand('==>'(C0),C1)),!.

fail_on_bind:attr_unify_hook(_,_):- fail.
add_fail_on_bind(Var):- var(Var)->put_attr(Var,fail_on_bind,true); term_variables(Var,Vs),maplist(add_fail_on_bind,Vs),!.
del_fail_on_bind(Var):- var(Var)->del_attr(Var,fail_on_bind); term_variables(Var,Vs),maplist(del_fail_on_bind,Vs),!.
:- export(fully_expand_some_kif/2).

fully_expand_some_kif(I,O):- 
     compound(I),\+ is_list(I),
     term_variables(I,Vars),
     maplist(add_fail_on_bind,Vars),
     copy_term(I,Copy),
     term_variables(Copy,CopyVars),
     fully_expand_kif_0k(I,M),
     Vars=@=CopyVars,
     maplist(del_fail_on_bind,Vars),O=M.

fully_expand_kif_0k(isa(X,Y),Out):- atom(Y), Out=..[Y,X].
fully_expand_kif_0k(isa(X,Y),instanceOf(X,Y)).
fully_expand_kif_0k(uN(vTheListFn, ROW),ROW).
fully_expand_kif_0k(['ListFn'|ROW],ROW).


clif_to_modal_clif(In,THINOUT):-   
   pretty_numbervars_ground(In,In0),
   %add_history(kif_to_boxlog(In0)),
   % In=In0,
   sumo_to_pdkb(In0,WffIn),
   must_be_unqualified(WffIn),
   KB = '$KB',
   kif_optionally_e(true,as_dlog,WffIn,DLOGKIF),!,
   % in_cmt((write('dlog= ' ),display(DLOGKIF))),
   kif_optionally_e(false,existentialize_objs,DLOGKIF,EXTOBJ),
   kif_optionally_e(false,existentialize_rels,EXTOBJ,EXT),
   kif_optionally_e(false,ensure_quantifiers,EXT,OuterQuantKIF),
   check_is_kb(KB),
   %kif_optionally_e(true,un_quant3(KB),OuterQuantKIF,NormalOuterQuantKIF),
   kif_optionally_e(always,correct_special_quantifiers(KB),OuterQuantKIF,FullQuant),   
  b_setval('$nnf_outermost',FullQuant),
   qualify_modality(FullQuant,ModalKIF),
   kif_optionally_e(true,adjust_kif(KB),ModalKIF,ModalKBKIFM),
   kif_optionally_e(true,correct_special_quantifiers(KB),ModalKBKIFM,ModalKBKIF),   
   kif_optionally_e(false,removeQ_3(KB),ModalKBKIF,UnQ),   
   current_outer_modal_t(HOLDS_T),
   % true cause poss loss
   kif_optionally_e(false,to_tlog(HOLDS_T,KB),UnQ,UnQ666),
   kif_optionally_e(never,as_prolog_hook,UnQ666,THIN),
   if_debugging2(kif,ignore((In0\=@=THIN,wdmsg(kifi=In)))),
   if_debugging2(kif,wdmsg(kifm=THIN)),
   must(THINOUT=THIN),nop(THINOUT=THIN).

sumo_to_pdkb(CycL,CycL):- is_ftVar(CycL).
sumo_to_pdkb('$COMMENT'(A),'$COMMENT'(A)):- !.
sumo_to_pdkb(In,Out):-
         must_det_l((must_map_preds([ 
           from_kif_string,           
           sexpr_sterm_to_pterm,
           pretty_numbervars_ground,
           map_each_subterm(sumo_to_pdkb_p5),
           map_each_subterm(pfc_lib:fully_expand(show_kif)),
           cyc_to_pdkb_maybe,           
           sumo_to_pdkb_p9,
           map_each_subterm(pfc_lib:fully_expand(show_kif))],
           In,Out))).

cyc_to_pdkb_maybe(I,O):- if_defined(cyc_to_pdkb(I,O),I=O),!.

sumo_to_pdkb_p9(I,O):-map_each_subterm(sumo_to_pdkb_p9_e,I,O).

if_changed(Ex,I,O):- call(Ex,I,O)-> I\==O.


map_each_subterm_compound(Ex,SENT,SENTO):- 
  compound_name_arguments(SENT,CONNECTIVE,ARGS),
  map_each_subterm(Ex,ARGS,ARGSO),
  compound_name_arguments(SENTO,CONNECTIVE,ARGSO),!.

map_each_subterm(_ ,O,O):- is_ftVar(O),!.
map_each_subterm(Ex,I,O):- on_x_rtrace(call(Ex,I,M)),!, 
  ((compound(M),I\==M) -> map_each_subterm_compound(Ex,M,O); O = M).
map_each_subterm(_ ,O,O):- \+ compound(O),!.
map_each_subterm(Ex,[H|T],[HH|TT]):- !,map_each_subterm(Ex,H,HH),map_each_subterm(Ex,T,TT).
map_each_subterm(Ex,(H,T),(HH,TT)):- !,map_each_subterm(Ex,H,HH),map_each_subterm(Ex,T,TT).

map_each_subterm(Ex,SENT,SENTO):- 
  compound_name_arguments(SENT,CONNECTIVE,ARGS),
  map_each_subterm(Ex,[CONNECTIVE|ARGS],CARGSO),
  ((is_list(CARGSO),CARGSO=[C|ARGSO],atom(C))->compound_name_arguments(SENTO,C,ARGSO);SENTO=CARGSO),!.

map_each_subterm(_ ,IO,IO).

sumo_to_pdkb_p5(documentation(C,'vEnglishLanguage',S),comment(C,S)):-!.
sumo_to_pdkb_p5(Const,NConst):-atom(Const),(sumo_to_pdkb_const(Const,NConst)->true;Const=NConst),!.
sumo_to_pdkb_p5(Const,NConst):-string(Const),string_to_mws(Const,NConst),!.
sumo_to_pdkb_p5(I,O):-clause_b(ruleRewrite(I,O))->I\==O,!.

sumo_to_pdkb_p9_e([P|List],OUT):- 
  atom(P),
  \+ is_list(List),                
  op_type_head(P,TYPE),
  make_var_arg(TYPE,P,List,OUT),!.

op_type_head(P,uN):-atom(P), atom_concat(_,'Fn',P).
op_type_head(P,tN):-atom(P).


make_var_arg(TYPE,P,List,OUT):- is_ftVar(List),!,OUT=..[TYPE,P,List].
make_var_arg(TYPE,P,List,OUT):- is_list(List),!,must_maplist(sumo_to_pdkb_p9,List,ListO),OUT=..[TYPE,P|ListO].
make_var_arg(TYPE,P,[A0|List],OUT):- sumo_to_pdkb_p9(A0,A),!,
 (is_ftVar(List) -> OUT=..[TYPE,P,A,List];
    (append(Left,Var,List),is_ftVar(Var),!,
    must_maplist(sumo_to_pdkb_p9,Left,NewLeft),
    append(NewLeft,[Var],NewList),
    OUT=..[TYPE,P,A|NewList])),!.



:- use_module(library(logicmoo_motel)).


:- fixup_exports.
