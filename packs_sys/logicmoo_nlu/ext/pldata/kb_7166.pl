:- module(kb7166,
  [setup7166/0,
 (forward/3),
 (forward/4),
 (forward/5),
 (forward/6),
 (forward/7),
 (forward/8),
 (forward/9),
 (forward/10),
 (forward/11),
 (forward/12),
 (backward/3),
 (backward/4),
 (backward/5),
 (backward/6),
 (backward/7),
 (backward/8),
 (backward/9),
 (backward/10),
 (backward/11),
 (backward/12),
 (code/3),
 (code/4),
 (code/5),
 (code/6),
 (code/7),
 (code/8),
 (code/9),
 (code/10),
 (code/11),
 (code/12),
 (deduction/3),
 (forward_default/4)
  ]).

:- multifile(forward/3).
:- multifile(forward/4).
:- multifile(forward/5).
:- multifile(forward/6).
:- multifile(forward/7).
:- multifile(forward/8).
:- multifile(forward/9).
:- multifile(forward/10).
:- multifile(forward/11).
:- multifile(forward/12).

:- multifile(backward/3).
:- multifile(backward/4).
:- multifile(backward/5).
:- multifile(backward/6).
:- multifile(backward/7).
:- multifile(backward/8).
:- multifile(backward/9).
:- multifile(backward/10).
:- multifile(backward/11).
:- multifile(backward/12).

:- multifile(code/3).
:- multifile(code/4).
:- multifile(code/5).
:- multifile(code/6).
:- multifile(code/7).
:- multifile(code/8).
:- multifile(code/9).
:- multifile(code/10).
:- multifile(code/11).
:- multifile(code/12).

:- dynamic(forward/3).
:- dynamic(forward/4).
:- dynamic(forward/5).
:- dynamic(forward/6).
:- dynamic(forward/7).
:- dynamic(forward/8).
:- dynamic(forward/9).
:- dynamic(forward/10).
:- dynamic(forward/11).
:- dynamic(forward/12).

:- dynamic(backward/3).
:- dynamic(backward/4).
:- dynamic(backward/5).
:- dynamic(backward/6).
:- dynamic(backward/7).
:- dynamic(backward/8).
:- dynamic(backward/9).
:- dynamic(backward/10).
:- dynamic(backward/11).
:- dynamic(backward/12).

:- dynamic(code/3).
:- dynamic(code/4).
:- dynamic(code/5).
:- dynamic(code/6).
:- dynamic(code/7).
:- dynamic(code/8).
:- dynamic(code/9).
:- dynamic(code/10).
:- dynamic(code/11).
:- dynamic(code/12).

:- multifile(deduction/3).
:- dynamic(deduction/3).


:- dynamic(forward_default/4).

:- dynamic(on_fin/1).


/*
:- call(asserta,((user:term_expansion(_, _):- !,fail))).
:- call(asserta,((user:goal_expansion(_, _):-!,fail))).
:- call(asserta,((system:term_expansion(_, _):-!,fail))).
:- call(asserta,((system:goal_expansion(_, _):-!,fail))).
:- call(asserta,((user:term_expansion(_,_, _,_):-!,fail))).
:- call(asserta,((user:goal_expansion(_,_, _,_):-!,fail))).
:- call(asserta,((system:term_expansion(_,_, _,_):-!,fail))).
:- call(asserta,((system:goal_expansion(_,_, _,_):-!,fail))).
*/

:- if(exists_source(pldata('kb_7166.pl-a3'))).
:- include(pldata('kb_7166.pl-a3')).
:- endif.

ra5(Often,PO) :-  
  must((must(expand_file_search_path((pldata('current_kb~/current_kb.pl')),Path)),
  exists_file(Path),
  open(Path,read,In))),!,
  repeat,
   once((rt(In,Wff,Vs),
   nb_setval('$has_var',[]),
   nb_setval('$variable_names',Vs),
   save_output(Often,Wff,Vs,PO))).



save_output(Often,Wff,Vs,PO):- 
  (baseKB:do_renames(Wff,P)->true;throw(do_renames(Wff,P))),!,
  (nb_current('$has_var',[])-> (PO = P,V2s=Vs) ; ((wt(string(S),P,Vs),rt(string(S),PO,V2s)))),
  nb_setval('$variable_names',V2s),!,
  once((V2s==[]->true;(functor(Wff,_,A),arg(A,Wff,ID),(maplist(arg(1),V2s,Names),wt(current_output,assertionVars(ID,Names),[]))))),!,
  wt(current_output,PO,V2s),
  ((nb_current('$has_var',t);(flag('$ett',X,X+1),0 is X rem Often))-> (wt(user_output,PO,V2s),trim_stacks) ; true).


rt(string(In),Wff,Vs):-!,catch(read_term_from_atom(In,Wff,[module(user),double_quotes(string),variable_names(Vs)]),E,(dmsg(E),dtrace,fail)).
rt(In,Wff,Vs):- catch(read_term(In,Wff,[module(user),double_quotes(string),variable_names(Vs)]),E,(dmsg(E),dtrace,fail)).
wt(string(O),P,Vs):- !, with_output_to(string(O), write_term(P,[variable_names(Vs),portrayed(true),quoted(true),fullstop(true),ignore_ops(true),nl(true),singletons(false)])).
wt(O,P,Vs):- write_term(O,P,[variable_names(Vs),portrayed(true),quoted(true),fullstop(true),ignore_ops(true),nl(true),singletons(false)]).

ra5:- tell(ra5),ra5(10000,E),E==end_of_file,!,told.
% ra5:- ra5(1,E),E==end_of_file,!.

:- set_prolog_flag(user:double_quotes,string).
:- set_prolog_flag(double_quotes,string).
:- set_prolog_flag(kb7166:double_quotes,string).
:- (compiling->ra5;true).
:- style_check(-singleton).
:- style_check(-discontiguous).
:- include(ra5).

:- if(current_predicate(_,on_fin(_))).
:- forall(call(retract,(on_fin(CALL))),call(CALL)).
:- endif.

setup7166:- call(retractall,backward(_,'[]',_)),call(retractall,code(_,'[]',_)),call(retractall, forward(_,'[]',_)).

/*
:- call(retract,((user:term_expansion(_, _):-!,fail))).
:- call(retract,((user:goal_expansion(_, _):-!,fail))).
:- call(retract,((system:term_expansion(_, _):-!,fail))).
:- call(retract,((system:goal_expansion(_, _):-!,fail))).
:- call(retract,((user:term_expansion(_,_, _,_):-!,fail))).
:- call(retract,((user:goal_expansion(_,_, _,_):-!,fail))).
:- call(retract,((system:term_expansion(_,_, _,_):-!,fail))).
:- call(retract,((system:goal_expansion(_,_, _,_):-!,fail))).
*/
