/* <module> logicmoo_plarkc - special module hooks into the logicmoo engine allow
%   clif syntax to be recocogized via our CycL/KIF handlers 
% 
% Logicmoo Project: A LarKC Server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:- module(logicmoo_plarkc,[]).

% ==============================================
% [Required] Load the Logicmoo User System
% ==============================================
:- ensure_loaded(library(logicmoo_lib)).


:- multifile(baseKB:cycBetween/3).
:- dynamic(baseKB:cycBetween/3).
:- baseKB:export(baseKB:cycBetween/3).
:- system:import(baseKB:cycBetween/3).

:- system:reexport(library(logicmoo_clif)).

%:- set_fileAssertMt(baseKB).

:- asserta_new(user:file_search_path(pldata,'/opt/cyc/')).
:- asserta_new(user:file_search_path(pldata,library(pldata))).
:- asserta_new(user:file_search_path(logicmoo,library('.'))).

:- absolute_file_name(library('../ext/pldata/'),Dir,[file_type(directory)]),
   asserta_new(user:file_search_path(pldata,Dir)).

:- absolute_file_name(library('../ext/'),Dir,[file_type(directory)]),
   asserta_new(user:file_search_path(logicmoo,Dir)).


:- dynamic(baseKB:tinyKB/3).
:- multifile(baseKB:tinyKB/3).
:- baseKB:export(baseKB:tinyKB/3).
:- system:import(baseKB:tinyKB/3).
:- dynamic(baseKB:tinyKB/1).
:- multifile(baseKB:tinyKB/1).
:- baseKB:export(baseKB:tinyKB/1).
:- system:import(baseKB:tinyKB/1).

:- gripe_time(60,baseKB:ensure_loaded(library('logicmoo/plarkc/logicmoo_i_cyc_rewriting'))).
:- gripe_time(60,baseKB:ensure_loaded(library('logicmoo/plarkc/logicmoo_u_cyc_kb_tinykb'))).
:- gripe_time(60,baseKB:ensure_loaded(library('logicmoo/plarkc/logicmoo_i_cyc_kb'))).
% :- add_library_search_path('./logicmoo/plarkc/',[ 'logicmoo_i_*.pl']).

/*
Before I continue I rather ensure I am using the same terminolgy as the rest
First I need to understand what you see as the difference between FOL and 2-Sorted FOL
Are both Henkin?
Also is FOL,  1-Sorted FOL or non-Sorted FOL?
Between 2-Sorted FOL and a 1-Sorted FOL
Between 3-Sorted FOL and a 2-Sorted FOL
well what i meant is that Full SOL *should* be more expressive than basic FOL 
in fact i have no doubts SOL *is* more expressive if it is extending basic FOL  
but make basic FOL extremely expressive by allowing to exist a: 2-Sorted FOL 
I would like to belive there could be a possiblity of add SOL to 2-Sorted FOL  the same way as SOL can be added to non sorted-FOL 

I refer the "expressivness", I mean the ablity thru syntactical expression to:
1) say anything desired and predict exactly how a reasoner will understand the new expression
2) the ability to change the default supertype ("Class") that unqualified foralls assume to be selecting from
3) say things the reasoner will be able to interpret subsumption to apply this new expression to other expressions: refered to as "Class"
4) Extend [dis]equality
5) quantify over bags of axioms and other exisiting content: John McCarthy 1982(?) named "Microtheory"s

Next Inside each "Microtheory"

6) Visiblity between Microtheories
6a) When visibility may happen at all..  
6b) How the Classes may change when one Microtheory sees into another
7) create and destroy hypothetical Microtheoies per single logical expression
8) Ability to manipulate each proof by treating it as we do our "bag of assertions/axioms"
9) Do 2-5 locally to each MT
   
   

MLTT = is an extension to control the u ability to

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SETUP CYC KB EXTENSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loadNewTiny:-  \+ exists_file(tiny_kb_cache),
  tell(tiny_kb_cache),
  format('~q.~n',[:- set_how_virtualize_file(false)]),
  format('~q.~n',[:- multifile(tiny_kb/3)]),
  format('~q.~n',[:-   dynamic(tiny_kb/3)]),
  format('~q.~n',[:- style_check(-singleton)]),
  forall(call(call,tinyKB,C,MT,STR),
        must(( (tinykb_assertion_recipe_w(C,CycLOut),
         format('~q.~n',[tiny_kb(CycLOut,MT,STR)]),
         ignore((C\=@=CycLOut,dmsg(tiny_kb(CycLOut,MT,STR)))))))),
  told,
  consult(tiny_kb_cache).

loadNewTiny:- consult(tiny_kb_cache).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SAVE CYC KB EXTENSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%:- after_init(loadNewTiny).

%:- after_init(baseKB:qsave_lm(lm_repl3)).



:- if(false).
:- user:use_module(library('file_scope')).
:- baseKB:disable_mpred_expansion.
 % :- set_prolog_flag(subclause_expansion,false).
:- if(exists_source(pldata('kb_7166.qlf'))).
:- dmsg("loading kb_7166").
:- set_prolog_flag_until_eof(do_renames,term_expansion).
:- ensure_loaded(pldata('kb_7166.qlf')).
:- else.
:- dmsg("qcompile kb_7166").
%:- set_prolog_flag_until_eof(do_renames,term_expansion).
:- load_files(pldata(kb_7166),[if(not_loaded),qcompile(auto)]).
:- endif.
:- dmsg("done loading kb_7166").
:- set_module(kb_7166:class(library)).
:- set_module(class(library)).
:- enable_mpred_expansion.
 % :- set_prolog_flag(subclause_expansion,true).
:- endif.

end_of_file.
end_of_file.









end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.











end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.











end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.











end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.

/*
:- (current_prolog_flag(qcompile,PrevValue)->true;PrevValue=false),
   call(assert,on_fin(set_prolog_flag(qcompile,PrevValue))),
   set_prolog_flag(qcompile,large).
*/
 


:- baseKB:disable_mpred_expansion.
 % :- set_prolog_flag(subclause_expansion,false).
:- if(exists_source(rs)).
:- dmsg("loading rns").
:- load_files(rs,[if(not_loaded),qcompile(auto)]).
:- dmsg("done with rns").
:- endif.

:- dmsg("loading current_renames").
% :- time((user:load_files(pldata('kb_7166_current_renames'),[module(baseKB),redefine_module(false),qcompile(auto)]))).
:- retractall(renames(_)).
:- enable_mpred_expansion.
 % :- set_prolog_flag(subclause_expansion,true).
:- dmsg("done with current_renames").

%:- set_prolog_stack(local, limit(32*10**9)).
%:- set_prolog_stack(global, limit(32*10**9)).
%:- gripe_time(60,baseKB:ensure_loaded(library('logicmoo/plarkc/logicmoo_i_cyc_kb'))).




:- if(current_predicate(on_fin/1)).
:- forall(call(retract,on_fin(CALL)),call(CALL)).
:- endif.

:- if(current_predicate(setup7166/0)).
:- during_boot(setup7166).
:- endif.


end_of_file.
%
:- dmsg(call(in_cmt(listing(cwtdl/3)))).
:- dmsg("Loading tinyKB should take under a minute").
:- ltkb1.
:- must((mudSubPart(iExplorer2,Inst),isa(Inst,tHumanNeck))).
:- must((mudSubPart(iExplorer2,Inst),isa(Inst,tHumanHair))).


/*
:- transTiny(Form,(ground(Form),functor(Form,F,1),F\== ~)).

:- set_gui_debug(false).
:- set_no_debug.
:- set_no_debug_thread.

:- transfer_predicate(tinyK8(Form), ( \+ contains_term('$VAR'(_),Form)) , ain((Form))).

:- mpred_trace.

:- set_clause_compile(fwc).

load_later:- quietly((transfer_predicate(tinyK8(Form),writeq(Form),ignore(on_x_log_throw(cwtdl(ain(clif(Form)),500,10)))))).

:- mpred_notrace.

:- in_cmt(listing(cwtdl_failed/1)).

*/

