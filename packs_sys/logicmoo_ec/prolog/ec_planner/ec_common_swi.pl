

:- module(ec_common_swi,[]).

:- use_module(library(debug)).
:- use_module(library(listing)).
:- use_module(library(prolog_clause)).
:- use_module(library(logicmoo_common)).


%writeNoln(P):- write(P),nl. 
%writeNoln(P):- write(P),nl. 

ticks(Z1):-  statistics(runtime,[Z1,_]).


init_gensym(_).

prolog_flag(F,Old,New):- ignore(current_prolog_flag(F,Old)),set_prolog_flag(F,New).

:- style_check(-singleton).
/* Emulates the writeNoln(1) function */

:- fixup_exports.

:- multifile(system:clause_w_names/5).
system:clause_w_names(Head,Body,ClauseRef,file=line_of(Line,File),Vs):-   
  clause(Head,Body,ClauseRef),
  clause(CHead,CBody,ClauseRef),
  term_variables(CHead+CBody,LocalVars),
  call(((prolog_clause:clause_info(ClauseRef, File, _TermPos, VarRecs,[variable_names(Vs)]) 
  -> 
  (  must((VarRecs=_,term_variables(Vs,ClauseVars),
     ClauseVars = LocalVars))
  ) 
  ;
  (
     Vs= [clauseVars=LocalVars]
  )))),
  must(((_:Head)+Body = CHead+CBody)),
  ignore(clause_property(ClauseRef, line_count(Line))),
  ignore(clause_property(ClauseRef, file(File))),
  dmsg(clause_w_names(Head,Body,ClauseRef,file=line_of(Line,File),Vs)).


