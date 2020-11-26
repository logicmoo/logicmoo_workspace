:- module(common_logic_loader,
	  [load_clif/1,
           kif_process/1,
           kif_io/2,          
           kif_process/2,
           kif_read/3]).
/** <module> common_logic_loader
% Provides interface for loading/interpretation of CLIF/KIF Files
%
%  t/N
%  hybridRule/2
%  
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- meta_predicate
   % common_logic_snark
   kif_process(*,*),
   % common_logic_snark
   kif_process(*).
   % common_logic_snark

:- meta_predicate with_ext_translation(+,*,:).


:- thread_local(t_l:kif_action_mode/1).
:- asserta_if_new(t_l:kif_action_mode(tell)).

:- thread_local(t_l:kif_reader_mode/1).
:- asserta_if_new(t_l:kif_reader_mode(lisp)).

:- if(exists_source(library(wam_cl/sreader))).
:- use_module(library(wam_cl/sreader)).
:- endif.

:- public(kif_io/0).
%% kif_io is det.
%
% Knowledge Interchange Format.
%
kif_io:- current_input(In),current_output(Out),!,kif_io(In,Out).


load_clif(File):- 
  zotrace(absolute_file_name(File,Found,[extensions(['','.clif','.ikl','.kif','.lisp','.lbase','.pfc','.pl']),access(read),expand(true),solutions(all)])),
  exists_file(Found),!,
  % with_lisp_translation_cached(Found, = , nop).
  file_name_extension(_,Ext,Found), 
  with_ext_translation(Found, Ext, kif_process_ignore).
load_clif(File):- trace_or_throw(missing(load_clif(File))).

with_ext_translation(Found,pl, Process):- !,process_script_file(Found,Process).
with_ext_translation(Found,pfc, Process):- !,process_script_file(Found,Process).
with_ext_translation(Found,_, Process):- with_lisp_translation(Found, Process).

kif_process_ignore(P):-must(once(kif_process(P))).
must_kif_process(P):-must(once(kif_process(P))).
must_kif_process_after_rename(Sent):-  if_defined(sumo_to_pdkb(Sent,SentM),=(Sent,SentM)),must_kif_process(SentM).

:- public(kif_process/1).

get_atom_or_kw(ModeIn,Mode):- trim_off_cll(':',ModeIn,Mode).
   trim_off_cll(Left,ModeIn,Mode):-atom_concat(Left,Mode,ModeIn),!.
   trim_off_cll(_,ModeIn,ModeIn).

%% kif_process( :GoalAssert) is det.
%
% Knowledge Interchange Format Process.
%
kif_process(Var):- is_ftVar(Var),!,wdmsg(warn(var_kif_process(Var))).
% kif_process(Mode):- atom(Mode),set_kif_mode(Mode).
kif_process(List):- is_list(List),must(sexpr_sterm_to_pterm(List,Wff)),t_l:kif_action_mode(Mode),!,ignore(show_failure(kif_process(Mode,Wff))),!.
kif_process(Wff):- t_l:kif_action_mode(Mode),!,ignore(show_failure(kif_process(Mode,Wff))),!.
kif_process(Wff):- ignore(show_failure(kif_process(tell,Wff))),!.

set_kif_mode(ModeIn):- ignore((atom(ModeIn),
  get_atom_or_kw(ModeIn,Mode),
  retractall(t_l:kif_action_mode(_)),
  asserta(t_l:kif_action_mode(Mode)),
  fmtl(t_l:kif_action_mode(Mode)))),!.


%% kif_process( ?Other, :GoalWff) is det.
%
% Knowledge Interchange Format Process.
%
kif_process(_,Var):- must_be(nonvar,Var),fail.
kif_process(_,'$COMMENT'([])):-!.
kif_process(_,'$COMMENT'([String])):-!, dmsg(String).
kif_process(_,'$COMMENT'(String)):-!, dmsg(String).
kif_process(_,'include'(String)):- !, load_clif(String).
kif_process(_,'dmsg'(String)):-!, dmsg(String).
kif_process(_,'wdmsg'(String)):-!, wdmsg(String).
kif_process(_,'kif-mode'(Mode)):- set_kif_mode(Mode).
kif_process(_,'kif_mode'(Mode)):- set_kif_mode(Mode).

kif_process(kif_add,Wff):- !, show_failure(kif_add(Wff)).
kif_process(kif_ask,Wff):- !, show_failure(kif_ask(Wff)).

kif_process(call_u,M:Wff):- !, show_call(call_u(M:Wff)).
kif_process(call_u,Wff):- !, show_call(call_u(Wff)).

kif_process(_,end_of_file):- !,signal_eof(kif_process),!.
kif_process(_,_:EOF):- EOF == end_of_file,!,signal_eof(kif_process),!.
kif_process(_,':-'(Call)):- !, kif_process(call,Call).
kif_process(_,'?-'(Goal)):- !, kif_process(ask,Goal).
kif_process(_,'ask'(Wff)):- !, kif_process(ask,Wff).
kif_process(_,'tell'(Wff)):- !, kif_process(tell,Wff).
kif_process(OP,'forall'(Vars,Wff)):- !, kif_process(OP,'all'(Vars,Wff)).
kif_process(_,'set-kif-option'(ModeIn)):-!,get_atom_or_kw(ModeIn,Mode), dmsg('set-kif-option'(Mode)).

kif_process(call,Was):- Was\=(_:_),!,prolog_load_context(module,Prev),kif_process(call,Prev:Was).

kif_process(_,From:prolog):- !, with_umt(From,prolog),!.



kif_process(call,Into:module(To,Exports)):- !,
  prolog_load_context(module,From),
  '$set_source_module'(To),
  maplist(To:export,Exports),
  maplist(From:import,Exports),
  maplist(Into:import,Exports),
  call_on_eof(kif_process,'$set_source_module'(From)).

kif_process(_,Atom):- atom(Atom),current_predicate(Atom/0),!,kif_process(call_u,Atom).
kif_process(_,Atom):- atom(Atom),current_predicate(Atom/1),fail,!,set_kif_mode(Atom).
kif_process(call,Call):- !,kif_process(call_u,Call).
kif_process(tell,Call):- is_static_predicate(Call),!,kif_process(call_u,Call).
kif_process(tell,Wff):- !,kif_process(kif_add,Wff).
kif_process(ask,Wff):- !,kif_process(kif_ask,Wff).
kif_process(Other,Wff):- wdmsg(error(missing_kif_process(Other,Wff))),fail.
kif_process(Pred1,Wff):- current_predicate(Pred1/1),!,call(Pred1,Wff).



%open_input(InS,InS):- is_stream(InS),!.
%open_input(string(InS),In):- text_to_string(InS,Str),string_codes(Str,Codes),open_chars_stream(Codes,In),!.


%% kif_read( ?InS, ?Wff, ?Vs) is det.
%
% Knowledge Interchange Format Read.
%
kif_read(In,Wff,Vs):- !, input_to_forms(In,Wff,Vs).
kif_read(In,Wff,Vs):- 
  (t_l:kif_reader_mode(lisp) ->
  without_must( catch(input_to_forms(In,Wff,Vs),E,(dmsg(E:kif_read_input_to_forms(In,Wff,Vs)),fail)))*-> true ;
      (catch(read_term(In,Wff,[module(user),double_quotes(string),variable_names(Vs)]),E,
                 (dmsg(E:kif_read_term_to_forms(In,Wff,Vs)),fail)))).

%= ===== to test program =====-
% :- ensure_loaded(logicmoo(snark/common_logic_sexpr)).

:- public(kif_io/2).
:- assert_until_eof(t_l:canonicalize_types).



%% kif_io( ?InS, ?Out) is det.
%
% Knowledge Interchange Format Input/output.
%
kif_io(In,Out):-
   repeat,
      on_x_debug((
          once((t_l:kif_action_mode(Mode),write(Out,Mode),write(Out,'> '))),
          once(on_x_debug(kif_read(In,Wff,Vs))),
          once(on_x_debug((put_variable_names( Vs), portray_clause(Out,Wff,[variable_names(Vs),quoted(true)])))),
          on_x_debug(kif_process(Wff)),
           Wff == end_of_file)),!.


:- assert_until_eof(t_l:canonicalize_types).


:- fixup_exports.

