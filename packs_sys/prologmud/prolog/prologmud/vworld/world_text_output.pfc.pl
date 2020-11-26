:-swi_module(world_text_output, []).
/*
* 
<module>  
%  Database pretty outputing controls 
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:- include(prologmud(mud_header)).


% :- include(prologmud(mud_header)).

% :- register_module_type (utility).

% live another day to fight (meaning repl_to_string/1 for now is in prolog)
% local_decl_db_prop(repl_writer(tAgent,term),[prologSingleValued,relationMostInstance(tAgent,default_repl_writer)]).
% local_decl_db_prop(repl_to_string(tAgent,term),[prologSingleValued,relationMostInstance(tAgent,default_repl_obj_to_string)]).

:-export(default_repl_buffer/4).
default_repl_buffer(Loc,_TL,_N,_Type,V):- arg(1,Loc,Prev),append(Prev,[V],New),
  nb_setarg(1,Loc,New).
default_repl_obj_to_string_buffer(Out,_Type,Out).

:-export(default_repl_writer/4).
default_repl_writer(TL,N,Type,V):- default_repl_writer_1(TL,N,Type,V),!. 
:-export(default_repl_obj_to_string/3).
default_repl_obj_to_string(O,Type,Out):- copy_term(Type,TypeO), ignore((TypeO = o )), ( TypeO == o -> Out=O ; Out = stringD(TypeO,O)).

default_repl_writer_1(_TL,N,_Type,mudDescription(V)):- N==ftText,compound(V),!,fmt('~N~w~n',[V]).
default_repl_writer_1(_TL,N,_Type,V):- N == text, compound(V),V=..[_,_,O],O==[],!.
default_repl_writer_1(_TL,N,Type,V):- 
  copy_term(Type,TypeO),ignore(TypeO=o),  ( TypeO == o -> fmt('~q= ~q.~n',[N,V]) ; fmt('~q=D(~w) ~q.~n',[N,TypeO,V])).


canUseEnglish:-true.


:-export(show_kb_preds/2).
show_kb_preds(Agent,List):- 
  ignore(mudAtLoc(Agent,LOC)),
  show_kb_preds(Agent,LOC,List),!.

:-export(show_kb_preds/3).
show_kb_preds(Agent,LOC,List):-
  ignore((once(mudAtLoc(Agent,LOC);localityOfObject(Agent,LOC)))),
  ignore((once(locationToRegion(LOC,Region);localityOfObject(Agent,Region)))),
  show_binds_kb_preds(Agent,[vHere=Region,isSelf=LOC,isSelfAgent=Agent],List).

show_kb_preds_to_buffer(Agent,LOC,List,Buffer):-
  ignore((once(mudAtLoc(Agent,LOC);localityOfObject(Agent,LOC)))),
  ignore((once(locationToRegion(LOC,Region);localityOfObject(Agent,Region)))),
  show_binds_kb_preds_to_buffer(Agent,[vHere=Region,isSelf=LOC,isSelfAgent=Agent],List,Buffer).


show_binds_kb_preds_to_buffer(_Agent,Substs,List,Buffer):-
    must_det_l((         
        notrace(subst_each(List,Substs,ListR)),
        dmsg(substs=Substs),
        show_kb_via_pred(default_repl_buffer(Buffer),
          default_repl_obj_to_string_buffer,ListR))),!.

show_binds_kb_preds(Agent,Substs,List):-
    must_det_l((
         once((t_l:repl_writer(Agent,WPred);WPred=default_repl_writer)),
         once((t_l:repl_to_string(Agent,ToSTR);ToSTR=default_repl_obj_to_string)),!,
        subst_each(List,Substs,ListR),
        dmsg(substs=Substs),
        show_kb_via_pred(WPred,ToSTR,ListR))),!.



:-export(show_kb_via_pred/3).
show_kb_via_pred(_,_,[]).
show_kb_via_pred(WPred,ToSTR,[L|List]):-!,must(show_kb_via_pred(WPred,ToSTR,L)),show_kb_via_pred(WPred,ToSTR,List),!.
show_kb_via_pred(WPred,ToSTR,L):-!,no_loop_check( 
   catch((ignore(show_failure(show_kb_via_pred_0(WPred,ToSTR,L)))),
   E,dmsg(error_failed(E,show_kb_via_pred_0(WPred,L))))),!.



:-export(show_kb_via_pred_0/3).

show_kb_via_pred_0(WPred,ToSTR,listof(Call)):- contains_var(value,Call),subst(Call,value,P,PCall),subst(Call,value,PS,PSCall),!,
                                               show_kb_via_pred_0(WPred,ToSTR,forEach(findall(P,no_repeats(PCall),PS),fmt(PSCall))).

show_kb_via_pred_0(WPred,ToSTR,listof(Call)):- !,show_kb_via_pred_format_call(WPred,ToSTR,Call,listof(Call)).
                                               

show_kb_via_pred_0(WPred,ToSTR,F = Call):- contains_var(value,Call),value\==Call,!,show_kb_via_pred_format_call(WPred,ToSTR,F = value, Call),!.
show_kb_via_pred_0(WPred,ToSTR,F = Call):- !,show_kb_via_pred_format_call(WPred,ToSTR, F = Call ,F = Call),!.
show_kb_via_pred_0(WPred,ToSTR,forEach(Call,Show)):-!, show_kb_via_pred_format_call(WPred,ToSTR, Show, forEach(Call)),!.
show_kb_via_pred_0(WPred,ToSTR,fmt(Show)):- !, show_kb_via_pred_format_call(WPred,ToSTR, Show ,true).
show_kb_via_pred_0(_WPred,_STR,call(Call)):- !,  with_output_to(string(Value),no_repeats(Call)),
      fmt(Value),!.
      % show_kb_via_pred_0(WPred,ToSTR,fmt(Value)).

show_kb_via_pred_0(WPred,ToSTR,once(Call)):- !,show_kb_via_pred_format_call(WPred,ToSTR,Call,once(Call)),!.
show_kb_via_pred_0(WPred,ToSTR,all(Call)):- !,functor(Call,F,_), show_kb_via_pred_1(WPred,ToSTR,F,all(Call)),!.
show_kb_via_pred_0(WPred,ToSTR,Call):- functor(Call,F,_), show_kb_via_pred_1(WPred,ToSTR,F,Call),!.

show_kb_via_pred_1(WPred,ToSTR,F,all(Call)):-!,show_kb_via_pred_2(WPred,ToSTR,F,Call).
show_kb_via_pred_1(WPred,ToSTR,F,once(Call)):-!,show_kb_via_pred_2(WPred,ToSTR,F,once(Call)).
show_kb_via_pred_1(_WPred,_ToSTR,_F,call(Call)):-!,on_x_debug(req1(Call)).
show_kb_via_pred_1(WPred,ToSTR,F,Call):-show_kb_via_pred_2(WPred,ToSTR,F,Call).


show_kb_via_pred_format_call(WPred0,ToSTRIn,Format0,Call0):-
   wsubst(Call0:Format0,value(ToSTR),value,Call:Format),
   ignore( ToSTR = (ToSTRIn) ),
   subst([WPred0,ToSTR,Format,Call],value,_NewValue,[WPred,ToSTROut,FormatOut,GCall]),
   show_kb_via_pred_fmt(WPred,ToSTROut,FormatOut,_UnkType,GCall).


show_kb_via_pred_fmt(WPred,ToSTR,SayIt,_Type,forEach(GCall)):-!,forall(req1(GCall),show_kb_via_pred_0(WPred,ToSTR,SayIt)).     
show_kb_via_pred_fmt(WPred,ToSTR,SayIt,Type,listof(GCall)):-!,findall(SayIt,catch(req1(GCall),Error,(dmsg(error(SayIt=Error:GCall)),fail)),Count),
    merge_list_on_p(WPred,ToSTR,SayIt,Type,GCall,_NewValue,Count).
show_kb_via_pred_fmt(WPred,ToSTR,SayIt,Type,GCall):-!,findall(SayIt,catch(req1(GCall),Error,(dmsg(error(SayIt=Error:GCall)),fail)),Count),
    merge_list_on_p(WPred,ToSTR,SayIt,Type,GCall,_NewValue,Count).


merge_list_on_p(_WPred,_ToSTR,_SayIt,_Type,_GCall,_NewValue,[]):-
  % fmt_holds_tcall(WPred,ToSTR,SayIt,Type,[noVal]).
  !. % dont print anything

merge_list_on_p(WPred,ToSTR,SayIt,Type,GCall,_NewValue,[]):-
  fmt_holds_tcall(WPred,ToSTR,SayIt,Type,notFound(f1SayIt,SayIt,Type,GCall)).


merge_list_on_p(WPred,ToSTR,SayIt,Type,_GCall,_NewValue,SayItList):-  SayIt = ( _ = _ ),!,
  findall(K,(member(KV,SayItList),arg(1,KV,K)),Keys),
  list_to_set(Keys,KeySet),!,
     forall(member(K,KeySet),
        (findall(V,(member(KV,SayItList),arg(1,KV,K),arg(2,KV,V)),VS),
          fmt_holds_tcall(WPred,ToSTR,K,Type,VS))).

merge_list_on_p(WPred,ToSTR,_SayIt,Type,listof(_GCall),_NewValue,SayItList):-  
  findall(K,(member(KV,SayItList),arg(1,KV,K)),Keys),
  list_to_set(Keys,KeySet),!,
     forall(member(K,KeySet),
        (findall(KV,(member(KV,SayItList),arg(1,KV,K)),VS),
          fmt_holds_tcall(WPred,ToSTR,K,Type,VS))).

merge_list_on_p(WPred,ToSTR, _SayIt ,Type,_GCall,_NewValue,SayItList):- fmt_holds_tcall(WPred,ToSTR,ftText,Type,SayItList).


% merge_list_on_p(WPred,ToSTR, SayIt ,Type,GCall,NewValue,SayItList):- forall(member(KV,SayItList),fmt_holds_tcall_pred_trans(WPred,ToSTR,SayIt,Type,KV)).

:-export(req1/1).
req1(O):- is_list(O),!,maplist(req1,O).
req1(O):- no_repeats(call_u(O)).

missing_out(_).


show_kb_via_pred_2(WPred0,ToSTRIn,F0,Call0):-
      wsubst(Call0,value(ToSTR),value,Call),
      ignore( ToSTR = (ToSTRIn) ),
      subst([WPred0,ToSTR,F0,Call],value,NewValue,[WPred,ToSTROut,F,GCall]),
      show_kb_via_pred_3(WPred,ToSTROut,F,_UnkType,GCall,NewValue),!.

show_kb_via_pred_3(WPred,ToSTR,fmt(SayIt),Type,GCall,NewValue):-!,
  % dmsg(show_kb_via_pred_3(WPred,ToSTR,F,GCall,NewValue)),
      findall(NewValue,(catch(req1(GCall),Error, NewValue=Error), 
             fmt((SayIt))),Count),
      (Count==[] ->
        missing_out(fmt_holds_tcall(WPred,ToSTR,F,Type,notFound(f1,F,Type))); true),!.

show_kb_via_pred_3(WPred,ToSTR,fmt,Type,GCall,NewValue):-!,
  % dmsg(show_kb_via_pred_3(WPred,ToSTR,F,GCall,NewValue)),
      findall(NewValue,(catch(req1(GCall),Error, NewValue=Error), 
             fmt(GCall)),Count),
      (Count==[] ->
        missing_out(fmt_holds_tcall(WPred,ToSTR,F,Type,notFound(f2,F,Type))); true),!.


show_kb_via_pred_3(WPred,ToSTR,output,Type,GCall,NewValue):-!,
  % dmsg(show_kb_via_pred_3(WPred,ToSTR,F,GCall,NewValue)),
      findall(NewValue,(catch(req1(GCall),Error, NewValue=Error), 
             fmt_holds_tcall(WPred,ToSTR,F,Type,NewValue)),Count),
      (Count==[] ->
        missing_out(fmt_holds_tcall(WPred,ToSTR,F,Type,notFound(f3,F,Type))); true),!.


show_kb_via_pred_3(WPred,ToSTR,F,Type,GCall,NewValue):- canUseEnglish,!,
  % dmsg(show_kb_via_pred_3(WPred,ToSTR,F,GCall,NewValue)),
      findall(NewValue,(catch(req1(GCall),Error, NewValue=Error), 
             fmt_holds_tcall(WPred,ToSTR,text,Type,GCall)),Count),!,
      (Count==[] ->
        (missing_out(fmt_holds_tcall(WPred,ToSTR,F,Type,notFound(f4,F,Type)))); true),!.

show_kb_via_pred_3(WPred,ToSTR,F,Type,GCall,NewValue):-
  % dmsg(show_kb_via_pred_3(WPred,ToSTR,F,GCall,NewValue)),
      findall(NewValue,(catch(req1(GCall),Error, NewValue=Error), 
             fmt_holds_tcall(WPred,ToSTR,F,Type,NewValue)),Count),
      (Count==[] ->
        missing_out(fmt_holds_tcall(WPred,ToSTR,F,Type,notFound(f5,F,Type))); true),!.


fmt_holds_tcall(WPred,ToSTR,N,Type, V):-  var(V),!,fmt_holds_tcall_pred_trans(WPred,ToSTR,N,Type,V),!.
fmt_holds_tcall(WPred,ToSTR,N,Type,[V]):- fmt_holds_tcall(WPred,ToSTR,N,Type,V),!.
fmt_holds_tcall(WPred,ToSTR,N,Type,[V|VV]):-  is_list([V|VV]), list_to_set([V|VV],Vs), fmt_holds_tcall_pred(WPred,ToSTR,N,Type,Vs),!.
fmt_holds_tcall(WPred,ToSTR,N,Type, V):-  fmt_holds_tcall_pred(WPred,ToSTR,N,Type,V),!.

% fmt_holds_tcall_pred(WPred,ToSTR,N,Type,[L|List]):-!, doall((member(V,[L|List]),fmt_holds_tcall_pred_trans(WPred,ToSTR,N,Type,V))).
fmt_holds_tcall_pred(WPred,ToSTR,N,Type,V0):-fmt_holds_tcall_pred_trans(WPred,ToSTR,N,Type,V0),!.

% fmt_holds_tcall_pred_trans(_, _ ,N,_ ,V):-!, fmt(N=V).
fmt_holds_tcall_pred_trans(WPred,ToSTR,N,Type,V0):-must((on_x_debug(call(ToSTR,V0,Type,V)),!,on_x_debug(call(WPred,_Tn,N,Type,V)))),!.


:- include(prologmud(mud_footer)).
