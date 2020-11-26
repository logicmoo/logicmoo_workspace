% :-swi_module(user). 
:-swi_module(modHelp, [actHelp/0]).
/* * module * A command to tell an agent all the possible commands
% help.pl
% Douglas Miles 2014
*/
:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

isa(tHumanControlled,ttAgentType).
%genls(ttAgentType,tCol).

:- kb_global(baseKB:type_action_info/3).

type_action_info(tHumanControlled,actHelp(isOptional(ftString,"")), "shows this help").


:-export(get_all_templates/1).

get_all_templates(Templ):- no_repeats(get_all_templates0(Templ)).

:-export(good_template/1).
good_template(Templ):- \+ contains_singletons(Templ).

get_all_templates0(Templ):-get_good_templates(Templ).
get_all_templates0(Templ):-get_bad_templates(Templ),not(get_good_templates(Templ)).

get_good_templates(Templ):- isa(Templ,vtActionTemplate),good_template(Templ).
% get_good_templates(Templ):- no_repeats_old((action_info(Templ,_),good_template(Templ))).


get_bad_templates(Templ):- no_repeats_old((action_info(Templ,_),not(good_template(Templ)))).


:- sanity((fully_expand_real(foo,action_info(TEMPL, txtConcatFn(_Text,"does: ",do(_A2,TEMPL))),O),dmsg(O))).

% :- mpred_core:import(baseKB:get_agent_text_command_0/4).

/*
==> ((({between(1,5,L),length(Text,L),
     get_agent_text_command(_A,Text,A2,Goal),(ground(Goal)->TEMPL=Goal;TEMPL=Text)}==>
         action_info(TEMPL, txtConcatFn(Text,"does: ",do(A2,TEMPL)))))).
*/

(action_rules(_Agent,Verb,[Obj|Objs],List),{atomic(Verb),safe_univ(Syntax,[Verb,Obj|Objs])} ==> 
         action_info(Syntax, txtConcatFn(["makes","happen"|List]))).


to_param_doc(TEMPL,["Prolog", "looks", "like", ":",TEMPL]):-!.
to_param_doc(TEMPL,S):-sformat(S,'Prolog looks like: ~q',[TEMPL]).


first_pl((BODY,_),PL):- nonvar(BODY),!,
 first_pl(BODY,PL).
first_pl(PL,PL).

:- kb_shared(action_info_db/3).

action_info_db(TEMPL,INFO,WAS):- (PRED=agent_call_command(_,WAS);PRED=agent_text_command(_,_,_,WAS)) ,
   clause(PRED,BODY,REF),clause_property(REF,file(S)),
   (ground(WAS)->true;once(( ignore((nop(S=S),first_pl(BODY,PL),ignore(catch(((true;quietly(PL)),!),_,true)))),ground(WAS)))),
   
    (TEMPL=@=WAS -> ((clause_property(REF,line_count(LC)),INFO=line(LC:S))) ;  (not(not(TEMPL=WAS)) -> INFO=file(S) ; fail)).

% :-trace.
action_info_db(TEMPL,S,WAS) ==> if_missing(action_info(TEMPL,_Help), action_info(TEMPL,txtConcatFn(S,contains,WAS))).
% action_info(TEMPL,txtConcatFn(S,contains,WAS)) <= action_info_db(TEMPL,S,WAS),{not_asserted(action_info(TEMPL,_Help))}.


commands_list(ListS):- findall(Templ,get_all_templates(Templ),List),predsort(alpha_shorter_1,List,ListS).

alpha_shorter(OrderO, P1,P2):-arg(1,P1,O1),arg(1,P2,O2),!,alpha_shorter_1(OrderO, O1,O2),!.
alpha_shorter(OrderO, P1,P2):-alpha_shorter_1(OrderO, P1,P2),!.

alpha_shorter_1(OrderO, P1,P2):-functor_h(P1,F1,A1),functor_h(P2,F2,A2),compare(OrderF,F1,F2), 
 (OrderF \== '=' -> OrderO=OrderF ;
  (compare(OrderA,A1,A2), (OrderA \== '=' -> OrderO=OrderA ; compare(OrderO,P1,P2)))).


show_templ_doc(TEMPL):-findall(DOC,action_info(TEMPL,DOC),DOCL),nvfmt(TEMPL=DOCL).
show_templ_doc_all(TEMPL):-findall(DOC,action_info(TEMPL,DOC),DOCL),nvfmt(TEMPL=DOCL).

nvfmt([XX]):-!,nvfmt(XX).
nvfmt(XX=[YY]):-!,nvfmt(XX=YY).
nvfmt(XX):-copy_term(XX,X),numbervars(X,0,_,[attvar(bind),singletons(true)]),fmt(X).

% Help - A command to tell an agent all the possible commands
actHelp:- commands_list(ListS),forall(member(E,ListS),show_templ_doc(E)).
agent_call_command(_Agent,actHelp) :- actHelp.
agent_call_command(_Agent,actHelp(Str)) :-actHelp(Str).

actHelp(Str):-commands_list(ListS),forall(member(E,ListS),write_doc_if_contains(Str,E)).

write_doc_if_contains('',E):-!,show_templ_doc(E),!.
write_doc_if_contains([],E):-!,show_templ_doc(E),!.
write_doc_if_contains("",E):-!,show_templ_doc(E),!.
write_doc_if_contains(Must,E):-ignore((with_output_to(string(Str),show_templ_doc_all(E)),str_contains_all([Must],Str),fmt(Str))).


(vtActionTemplate(A),{nonvar(A),get_functor(A,Inst)} ==> isa(Inst,vtVerb)).

impl_coerce_hook(Text,vtVerb,Inst):- isa(Inst,vtVerb),name_text(Inst,Text).

%agent_text_command(Agent,[Who],Agent,Cmd):- nonvar(Who), get_all_templates(Syntax),Syntax=..[Who,isOptional(_,Default)],Cmd=..[Who,Default].
%agent_text_command(Agent,[Who,Type],Agent,Cmd):- get_all_templates(Syntax),nonvar(Who),Syntax=..[Who,isOptional(Type,_)],Cmd=..[Who,Type].

:- include(prologmud(mud_footer)).

% :-ain((({get_all_templates(Templ)})==>vtActionTemplate(Templ))).

:- kb_shared(baseKB:actParse/2).

(type_action_info(_,TEMPL,Help) ==> action_info(TEMPL,Help)).

(action_info(TEMPL,_Help) ==> vtActionTemplate(TEMPL)).

vtActionTemplate(TEMPL), \+ action_info(TEMPL,_)  ==> ({to_param_doc(TEMPL,S)},action_info(TEMPL,S)).




