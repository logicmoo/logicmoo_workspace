:-if(exists_source(library(logicmoo_utils))).
:- if((multifile(baseKB:ignore_file_mpreds/1),dynamic(baseKB:ignore_file_mpreds/1), (prolog_load_context(file,F) -> ain(baseKB:ignore_file_mpreds(F)) ; true))).
:-endif.
:-if((multifile(baseKB:mpred_is_impl_file/1),dynamic(baseKB:mpred_is_impl_file/1),prolog_load_context(file,F),call(assert_if_new,baseKB:mpred_is_impl_file(F)))).
:-endif.
:- endif.

:- module(eggdrop_next, [
add_maybe_static/2,bot_nick/1,call_in_thread/1,call_with_results/3,check_put_server_count/1,cit/0,close_ioe/3,consultation_codes/3,
  consultation_thread/2,ctcp/6,ctrl_nick/1,ctrl_pass/1,ctrl_port/1,ctrl_server/1,deregister_unsafe_preds/0,egg_go/0,
  egg_go_fg/0,eggdropConnect/0,eggdropConnect/2,eggdropConnect/4,eggdrop_bind_user_streams/0,escape_quotes/2,flush_all_output/0,
  flushed_privmsg/3,
  format_nv/2,get2react/1,get_session_prefix/1,ignore_catch/1,ignored_source/1,ircEvent/3,irc_call/4,irc_filtered/4,
  irc_receive/5,is_callable_egg/1,is_empty/1,is_lisp_call_functor/1,join/4,list_replace_egg/4,msgm/5,my_wdmsg/1,
  part/5,privmsg/2,privmsg_session/2,pubm/5,putnotice/2,read_codes_and_send/2,read_egg_term/3,read_from_agent_and_send/2,
  recordlast/3,remove_anons/2,remove_pred_egg/3,say/1,say/2,say/3,say/3,
  say/3,sayq/1,show_thread_exit/0,to_egg/1,to_egg/2,unreadable/1,unsafe_preds_egg/3,
  update_changed_files_eggdrop/0,vars_as_comma/0,vars_as_list/0,with_error_channel/2,with_error_to_output/1,with_input_channel_user/3,with_io/1,with_no_input/1,
  with_output_channel/2,with_rl/1,egg:stdio/3,lmcache:vars_as/1,ignored_channel/1,lmconf:chat_isWith/2,lmconf:chat_isRegistered/3,last_read_from/3,
  lmconf:chat_isChannelUserAct/4,
  attvar_to_dict_egg/2,
  % dict_to_attvar_egg/2,
  format_nv/2
  % eggdrop_e:stream_close/1,eggdrop_e:stream_read/2,eggdrop_e:stream_write/2,eggdrop_io:stream_close/1,eggdrop_io:stream_read/2,
  % eggdrop_io:stream_write/2,t_l:put_server_count/1,t_l:put_server_no_max/0,t_l:session_id/1
  ]).

:- set_module(class(library)).

:- user:ensure_loaded(library(clpfd)).
:- '@'((
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-')),user).


:- if((fail,exists_source(library(atts)))).
:- set_prolog_flag(metaterm,true).
:- use_module(library(atts)).
:- endif.

:- if(exists_source(library(logicmoo_utils))).
:- user:use_module(system:library(logicmoo_utils)).
:- endif.

:- meta_predicate
        call_in_thread(0),
        call_with_results(+, 0, ?),
        call_with_results_0(0, ?),
        call_with_results_nondet(0, ?),
        call_with_results_nondet_vars(0, ?),
        ignore_catch(0),
        with_error_channel(+, 0),
        with_error_to_output(0),
        with_input_channel_user(+, +, 0),
        with_io(0),
        with_no_input(0),
        with_output_channel(+, 0),
        with_rl(0).


:- user:use_module(library(logicmoo_util_common)).

same_streams(Alias1,Alias2):- stream_property(S1,alias(Alias1)),stream_property(S2,alias(Alias2)),!,S1==S2.

%% my_wdmsg( ?Msg) is det.
%
% My Wdmsg.
%
my_wdmsg(Msg):- notrace(my_wdmsg0(Msg)).

my_wdmsg0(List):-is_list(List),catch(text_to_string(List,CMD),_,fail),List\==CMD,!,my_wdmsg(CMD).
my_wdmsg0(Msg):- if_defined(real_user_output(S)),!,my_wdmsg(S,Msg).
my_wdmsg0(Msg):- stream_property(_,alias(main_user_error)),!,my_wdmsg(main_user_error,Msg).
my_wdmsg0(Msg):- get_main_error_stream(ERR),!,my_wdmsg(ERR,Msg).
my_wdmsg0(Msg):- debugm(Msg).

my_wdmsg(S,Msg):- string(Msg),format(S,'~N% ~w~N',[Msg]),flush_output_safe(S),!.
my_wdmsg(S,Msg):- format(S,'~N% ~q.~n',[Msg]),flush_output_safe(S),!.

:- dynamic(lmconf:chat_isWith/2).
:- thread_local(t_l:(disable_px)).

:- my_wdmsg("HI there").



egg_to_string(A,S):- notrace(egg_to_string0(A,S)).
egg_to_string0(A,S):- var(A),!,trace_or_throw(var_egg_to_string(A,S)).
egg_to_string0(A,S):- on_x_fail(text_to_string(A,S)),!.
egg_to_string0([A],S):- on_x_fail(atom_string(A,S)),!.
egg_to_string0(A,S):- on_x_fail(atom_string(A,S)),!.

:- dynamic(real_user_output/1).
:- volatile(real_user_output/1).
egg_booting :- source_file_property(reloading, true) ,!.
egg_booting :- thread_self(M), M \== main,!.
egg_booting :- ignore((stream_property(S,alias(user_output)),asserta(real_user_output(S)),set_stream(S,alias(main_user_output)))),
   ignore(( stream_property(S,alias(user_error)),asserta(real_user_output(S)),!,set_stream(S,alias(main_user_error)))).

:- during_boot(egg_booting).

% ===================================================================
% IRC CONFIG
% ===================================================================

:- if(exists_file('.ircbot.pl')).
:- include('.ircbot.pl').
:- else.



%% bot_nick( ?PrologMUD1) is det.
%
% Bot Nick.
%
bot_nick("PrologMUD").



%% ctrl_server( ?Localhost1) is det.
%
% Ctrl Server.
%
ctrl_server(localhost).



%% ctrl_nick( ?Swipl1) is det.
%
% Ctrl Nick.
%
ctrl_nick("swipl").



%% ctrl_pass( ?Top5ecret1) is det.
%
% Ctrl Pass.
%
ctrl_pass("top5ecret").



%% ctrl_port( ?Port) is det.
%
% Ctrl Port.
%
ctrl_port(3334).

:- endif.



 :- meta_predicate call_with_results_nondet_vars(0,*).
 :- meta_predicate call_with_results_nondet(0,*).
 :- meta_predicate call_with_results_0(0,*),with_rl(0).

% :- use_module(library(logicmoo/util/logicmoo_util_prolog_streams),[with_output_to_stream_pred/4]).

:- module_transparent(ircEvent/3).
% from https://github.com/logicmoo/PrologMUD/tree/master/src_lib/logicmoo_util
% supplies locally/2,atom_concats/2, dmsg/1, my_wdmsg/1, must/1, if_startup_script/0
:- ensure_loaded(system:library(logicmoo_utils)).
% :- use_module(library(resource_bounds)).
:- ensure_loaded(library(logicmoo_utils)).
/*
TODO

* one may consider letting it wait until a query is completed with a `.'

* consider using numbervars/[3,4] <http://www.swi-prolog.org/pldoc/man?section=manipterm#numbervars/3>,
  <http://www.swi-prolog.org/pldoc/man?section=termrw#write_term/2> (option numbervars)
   for printing the variables (named ones in query, and freshly generated ones)

* skip printing toplevel vars marked with "_" in  findall(_X, fact(_X), Xs).

* perhaps use "commalists" instead of ordinary lists for each solution ? (makes it look more like a traditional interactor reply, and looks more sensible, logically)

*/



%% lmconf:chat_isRegistered( ?Channel, ?Agent, ?Execute3) is det.
%
% If Is A Registered.
%
:- dynamic(lmconf:chat_isRegistered/3).
lmconf:chat_isRegistered(Channel,Agent,kifbot):-lmconf:chat_isWith(Channel,Agent).
lmconf:chat_isRegistered(_,"someluser",execute):-!,fail.
lmconf:chat_isRegistered("#ai",_,execute):-ignore(fail).
lmconf:chat_isRegistered("#pigface",_,execute):-ignore(fail).  % havent been there since 2001
lmconf:chat_isRegistered("#logicmoo",_,execute):-ignore(fail).
lmconf:chat_isRegistered("#kif",_,execute):-ignore(fail).
lmconf:chat_isRegistered("#rdfig",_,execute):-ignore(fail).
lmconf:chat_isRegistered("##prolog",_,execute):-!.
% all may execture since th ey are using ?-
lmconf:chat_isRegistered(_,_,execute):-!.

:- export((egg_go/0,ircEvent/3,lmconf:chat_isRegistered/3)).
% ===================================================================
% Deregister unsafe preds
% ===================================================================
:-use_module(library(process)).




%% unsafe_preds_egg( ?M, ?F, ?A) is det.
%
% Unsafe Predicates Egg.
%
unsafe_preds_egg(M,F,A):-M=files_ex,current_predicate(M:F/A),member(X,[delete,copy]),atom_contains(F,X).
%unsafe_preds_egg(M,F,A):-M=process,current_predicate(M:F/A),member(X,[kill,create]),atom_contains(F,X).
unsafe_preds_egg(M,F,A):-M=system,member(F,[shell,halt]),current_predicate(M:F/A).

:- export(remove_pred_egg/3).



%% remove_pred_egg( ?M, ?F, ?A) is det.
%
% Remove Predicate Egg.
%
remove_pred_egg(_,F,A):-member(_:F/A,[_:delete_common_prefix/4]),!.
remove_pred_egg(M,F,A):- functor(P,F,A),
  (current_predicate(M:F/A) -> ignore((catch(redefine_system_predicate(M:P),_,true),abolish(M:F,A)));true),
  M:asserta((P:-(my_wdmsg(error(call(P))),throw(permission_error(M:F/A))))).

% :-use_module(library(uid)).
% only if root



%% deregister_unsafe_preds is det.
%
% Deregister Unsafe Predicates.
%

deregister_unsafe_preds:-!.
deregister_unsafe_preds:- current_predicate(system:kill_unsafe_preds/0),!.
deregister_unsafe_preds:- if_defined(getuid(0),true),forall(unsafe_preds_egg(M,F,A),remove_pred_egg(M,F,A)).
deregister_unsafe_preds:-!.

% [Optionaly] Solve the Halting problem
:-redefine_system_predicate(system:halt).
:-abolish(system:halt,0).



%% halt is det.
%
% Hook To [system:halt/0] For Module Eggdrop.
% Halt.
%
system:halt:- format('the halting problem is now solved!').


% :- deregister_unsafe_preds.
% ===================================================================
% Eggrop interaction
% ===================================================================

:- use_module(library(socket)).
:- volatile(egg:stdio/3).
:- dynamic(egg:stdio/3).

:- ain(mtExact(lmcache)).
:- ain(mtExact(lmconf)).



%% eggdropConnect is det.
%
% Eggdrop Connect.
%
eggdropConnect:- eggdropConnect(_Host,_Port,_CtrlNick,_Pass).



%% eggdropConnect( ?CtrlNick, ?Port) is det.
%
% Eggdrop Connect.
%
eggdropConnect(CtrlNick,Port):-eggdropConnect(_Host,Port,CtrlNick,_Pass).



%% eggdropConnect( ?Host, ?Port, ?CtrlNick, ?Pass) is det.
%
% Eggdrop Connect.
%
eggdropConnect(Host,Port,CtrlNick,Pass):-
       ignore(ctrl_server(Host)),
       ignore(ctrl_port(Port)),
       ignore(ctrl_nick(CtrlNick)),
       ignore(ctrl_pass(Pass)),
       tcp_socket(SocketId),
       my_wdmsg(tcp_connect(SocketId,Host:Port)),
       tcp_connect(SocketId,Host:Port),
       tcp_open_socket(SocketId, IN, OutStream),

   set_stream(IN,type(binary)),
   set_stream(OutStream,type(binary)),
       format(OutStream,'~w\n',[CtrlNick]),flush_output_safe(OutStream),
       format(OutStream,'~w\n',[Pass]),flush_output_safe(OutStream),

%       format(OutStream,'~w\n',[CtrlNick]),flush_output(OutStream),
%       format(OutStream,'~w\n',[Pass]),flush_output(OutStream),
       retractall(egg:stdio(CtrlNick,_,_)),
       asserta((egg:stdio(CtrlNick,IN,OutStream))),!.

:-export(consultation_thread/2).



%% consultation_thread( ?CtrlNick, ?Port) is det.
%
% Consultation Thread.
%
consultation_thread(CtrlNick,Port):-
      eggdropConnect(CtrlNick,Port),
      to_egg('.echo off\n'),
      to_egg('.console ~w ""\n',[CtrlNick]),
      must(bot_nick(PrologMUDNick)),
      to_egg('.set nick ~w\n',[PrologMUDNick]),
      must(egg:stdio(CtrlNick,IN,_)),!,
      % loop
      to_egg('.console #logicmoo\n',[]),
      % say(dmiles,consultation_thread(CtrlNick,Port)),
      % say("#logicmoo","hi therre!"),
      repeat,
         nop(notrace(update_changed_files_eggdrop)),
         notrace(catch(read_line_to_codes(IN,Text),_,Text=end_of_file)),
          
         once(consultation_codes(CtrlNick,Port,Text)),
         Text==end_of_file,!.




%% is_callable_egg( ?CMD) is det.
%
% If Is A Callable Egg.
%
is_callable_egg(CMD):-var(CMD),!,fail.
is_callable_egg((A,B)):-!,is_callable_egg(A),is_callable_egg(B).
is_callable_egg((A;B)):-!,is_callable_egg(A),is_callable_egg(B).
is_callable_egg(CMD):- callable(CMD),
          functor(CMD,F,A),
          current_predicate(F/A),!.

%:-meta_predicate(module_call(+,0)).
%module_call(M,CMD):- CALL=M:call(CMD), '@'(catch(CALL,E,(my_wdmsg(E:CALL),throw(E))),M).
%:-meta_predicate(user_call(0)).
%user_call(M:CMD):-!,show_call(module_call(M,CMD)).
%user_call(CMD):-module_call('user',CMD).




%% consultation_codes( ?CtrlNick, ?Port, ?Codes) is det.
%
% Consultation Codes.
%
consultation_codes(CtrlNick,Port,end_of_file):-!,consultation_thread(CtrlNick,Port).
consultation_codes(_BotNick,_Port,Text):-
      notrace((text_to_string(Text,String),
      catch(read_term_from_atom(String,CMD,[]),_E,(my_wdmsg(String),!,fail)),!,
      is_callable_egg(CMD),
      my_wdmsg(maybe_call(CMD)))),!,
      catchv(CMD,E,my_wdmsg(E:CMD)).


% IRC EVENTS Bubble from here
:- export(get2react/1).



%% get2react( ?ARG1) is det.
%
% Get2react.
%
get2react([L|IST1]):- CALL =.. [L|IST1],functor(CALL,F,A),
  show_failure((current_predicate(F/A))),show_failure((CALL)).

:- thread_local(t_l: session_id/1).

% ===================================================================
% IRC interaction
% ===================================================================
:- thread_local t_l:default_channel/1, t_l:default_user/1, t_l:current_irc_receive/5.
% IRC EVENTS FROM CALL

chon(W,N):- nop(chon(_,W,N)).

%% part( ?USER, ?HOSTMASK, ?TYPE, ?DEST, ?MESSAGE) is det.
%
% Part.
%
part(USER, HOSTMASK,TYPE,DEST,MESSAGE):- irc_receive(USER, HOSTMASK,TYPE,DEST,part(USER, HOSTMASK,TYPE,DEST,MESSAGE)).



%% join( ?USER, ?HOSTMASK, ?TYPE, ?DEST) is det.
%
% Join.
%
join(USER, HOSTMASK,TYPE,DEST):- irc_receive(USER, HOSTMASK,TYPE,DEST,join(USER, HOSTMASK,TYPE,DEST)).



%% msgm( ?USER, ?HOSTMASK, ?TYPE, ?DEST, ?MESSAGE) is det.
%
% Msgm.
%
msgm(USER, HOSTMASK,TYPE,DEST,MESSAGE):- irc_receive(USER, HOSTMASK,TYPE,DEST,say(MESSAGE)).



%% ctcp( ?USER, ?HOSTMASK, ?FROM, ?DEST, ?TYPE, ?MESSAGE) is det.
%
% Ctcp.
%
ctcp(USER, HOSTMASK,_FROM,DEST,TYPE,MESSAGE):- irc_receive(USER, HOSTMASK,TYPE,DEST,ctcp(TYPE,MESSAGE)).



%% pubm( ?USER, ?HOSTMASK, ?TYPE, ?DEST, ?MESSAGE) is det.
%
% Pubm.
%
pubm(USER, HOSTMASK,TYPE,DEST,MESSAGE):- irc_receive(USER, HOSTMASK,TYPE,DEST,say(MESSAGE)).





%% irc_receive( ?USER, ?HOSTMASK, ?TYPE, ?DEST, ?MESSAGE) is det.
%
% Irc Receive.
%
irc_receive(USER,HOSTMASK,TYPE,DEST,MESSAGE):-
 my_wdmsg(irc_receive(USER,HOSTMASK,TYPE,DEST,MESSAGE)),!,
   agent_module(USER,ID),
   call_in_thread(
    with_engine(ID,
     % with_error_to_output
     (
     locally([
       t_l:put_server_count(0),
       t_l:default_channel(DEST),
       t_l:default_user(USER),
       t_l:session_id(ID),
       t_l:current_irc_receive(USER, HOSTMASK,TYPE,DEST,MESSAGE)],
        ((eggdrop_bind_user_streams, 
         must(t_l:default_channel(DEST)),
         ircEvent(DEST,USER,MESSAGE))))))).


be_engine:- repeat,engine_fetch(Term),once(Term),engine_yield(Term),fail.

with_engine(_,Goal):- !, Goal.
with_engine(E,Goal):- current_engine(EE),EE==E,!,engine_post(E,Goal),get_answers(E, R),my_wdmsg(r(E=R)).
with_engine(E,Goal):- catch(engine_create(success,Goal,E),_,(engine_destroy(E),engine_create(success,be_engine,E))), 
  get_answers(E,R),my_wdmsg(r(E=R)).

get_answers(E, [H|T]) :-
        engine_next(E, H), !,
        get_answers(E, T).
get_answers(_, []).


%% with_rl( :Call) is det.
%
% Using Resource Limit.
%

% :- use_module(library(resource_bounds)).
  %with_rl(Call):- thread_self(main),!,rtrace((guitracer,trace,Call)).
with_rl(Call):- thread_self(main),!,Call.
with_rl(Call):- !,nodebugx(Call).
% with_rl(Goal):- show_call(eggdrop,nodebugx(resource_bounded_call(Goal, 1000.0, _Status, []))).
with_rl(Call):- nodebugx(call_with_time_limit(30,Call)).

% ===================================================================
% IRC EVENTS
% ===================================================================

:- dynamic(last_read_from/3).

% convert all to strings



%% ignored_source( ?From) is det.
%
% Ignored Source.
%
ignored_source(From):-var(From),!,fail.
ignored_source("yesbot").
ignored_source(From):-not(string(From)),!,text_to_string(From,String),!,ignored_source(String).
%  from bot telnet
ignored_source(From):- atom(From),atom_length(From,1).
% from the process or bot
ignored_source(From):-
 bot_nick(BotNick),ctrl_nick(CtrlNick),arg(_,vv(BotNick,CtrlNick),Ignore),atom_contains(From,Ignore),!.

:-dynamic(lmconf:chat_isChannelUserAct/4).
:-dynamic(ignored_channel/1).
:-dynamic(user:irc_event_hooks/3).
:-multifile(user:irc_event_hooks/3).




%% user:irc_event_hooks( ?Channel, ?User, ?Stuff) is det.
%
% Hook To [user:irc_event_hooks/3] For Module Eggdrop.
% Irc Event Hooks.
%
user:irc_event_hooks(_Channel,_User,_Stuff):-fail.




%% recordlast( ?Channel, ?User, ?What) is det.
%
% Recordlast.
%
recordlast(Channel,User,say(What)):-!,retractall(lmconf:chat_isChannelUserAct(Channel,User,say,_)),asserta(lmconf:chat_isChannelUserAct(Channel,User,say,What)),!.
recordlast(Channel,User,What):-functor(What,F,_),retractall(lmconf:chat_isChannelUserAct(Channel,User,F,_)),asserta(lmconf:chat_isChannelUserAct(Channel,User,F,What)),!.

% awaiting some inputs



%% ircEvent( ?DEST, ?User, ?Event) is det.
%
% Irc Event.
%
ircEvent(DEST,User,say(W)):-
 term_to_atom(cu(DEST,User),QUEUE),
   message_queue_property(_, alias(QUEUE)),
     show_call(ircEvent,thread_send_message(QUEUE,W)).

% ignore some inputs
ircEvent(Channel,Agent,_):- (ignored_channel(Channel) ; ignored_source(Agent)) ,!.

% attention (notice the fail to disable)
ircEvent(Channel,Agent,say(W)):- fail,
               atom_contains(W,"goodbye"),!,retractall(lmconf:chat_isWith(Channel,Agent)).

ircEvent(Channel,Agent,say(W)):- fail,
               (bot_nick(BotNick),atom_contains(W,BotNick)),
		retractall(lmconf:chat_isWith(Channel,Agent)),!,
		asserta(lmconf:chat_isWith(Channel,Agent)),!,
		say(Channel,[hi,Agent,'I will answer you in',Channel,'until you say "goodbye"']).


ircEvent(Channel,Agent,Event):- once(doall(call_no_cuts(user:irc_event_hooks(Channel,Agent,Event)))),fail.


% Say -> Call
ircEvent(Channel,Agent,say(W)):-
 % with_dmsg_to_main
 ((
   forall(
   (read_egg_term(W,CMD,Vs),CMD\==end_of_file),
   % eggdrop:read_each_term_egg(W,CMD,Vs),
   ircEvent(Channel,Agent,call(CMD,Vs))))),!.


% Call -> call_with_results
ircEvent(Channel,Agent,call(CALL,Vs)):-
  with_dmsg_to_main((
   thread_self(Self), tnodebug(Self),
   use_agent_module(Agent),
   dmsg(irc_filtered(Channel,Agent,CALL,Vs)),
   once(((irc_filtered(Channel,Agent,CALL,Vs)) -> true ; dmsg(failed_irc_filtered(Channel,Agent,CALL,Vs)))),
   save_agent_module(Agent))),!.

ircEvent(Channel,User,Method):-recordlast(Channel,User,Method), my_wdmsg(unused(ircEvent(Channel,User,Method))).

:- dynamic(lmconf:chat_isModule/3).

:- module_transparent(use_agent_module/1).
:- module_transparent(save_agent_module/1).
:- if(true).
use_agent_module(AgentS):- any_to_atom(AgentS,Agent),source_and_module_for_agent(Agent,Module,CallModule),!,'$set_source_module'(Module),'$set_typein_module'(CallModule).
save_agent_module(AgentS):- any_to_atom(AgentS,Agent), retractall(lmconf:chat_isModule(Agent,_)), '$set_source_module'(Next,Next),'$module'(CallModule,CallModule),asserta(lmconf:chat_isModule(Agent,Next,CallModule)).
source_and_module_for_agent(Agent,Module,CallModule):- lmconf:chat_isModule(Agent,Module,CallModule),!.
source_and_module_for_agent(Agent,Agent,user):- maybe_add_import_module(Agent,user,end), maybe_add_import_module(Agent,eggdrop,end).
:- else.
use_agent_module(AgentS):- agent_module(AgentS,Agent), 
  source_and_module_for_agent(Agent,SM,CM),!,
 '$set_source_module'(_,SM),'$module'(_,CM),!.
save_agent_module(AgentS):- agent_module(AgentS,Agent), 
  retractall(lmconf:chat_isModule(Agent,_,_)),
  '$current_source_module'(SM),'$current_typein_module'(CM),asserta(lmconf:chat_isModule(Agent,SM,CM)).

source_and_module_for_agent(Agent,SM,CM):- lmconf:chat_isModule(Agent,SM,CM),!.
source_and_module_for_agent(Agent,SM,CM):- \+ atom(Agent),agent_module(Agent,AgentM),Agent\==AgentM,!,source_and_module_for_agent(AgentM,SM,CM).
source_and_module_for_agent(Agent,Agent,Agent):- maybe_add_import_module(Agent,user,end), maybe_add_import_module(Agent,eggdrop,end).

:- endif.


:-export(unreadable/1).



%% unreadable( ?UR) is det.
%
% Unreadable.
%
unreadable(UR):-my_wdmsg(unreadable(UR)).



:-export(eggdrop_bind_user_streams/0).




%% eggdrop_bind_user_streams is det.
%
% Eggdrop Bind User Streams.
%

% eggdrop_bind_user_streams :- !.
eggdrop_bind_user_streams :- thread_self(main),!.
eggdrop_bind_user_streams :-
  user:((
	user:open_prolog_stream(eggdrop_io, write, Out, []),
        user:open_prolog_stream(eggdrop_e, write, Err, []),
	set_stream(Out, buffer(line)),
        set_stream(Err, buffer(line)),
	open_prolog_stream(eggdrop_io, read,  In, []),
        set_input(In),
        set_output(Out),
/*        set_stream(In,  alias(user_input)),
        set_stream(Out, alias(user_output)),
        set_stream(Err, alias(user_error)),
	set_stream(In,  alias(current_input)),
        set_stream(Out, alias(current_output)),
        set_stream(Err, alias(current_error)),
  */
	thread_at_exit(eggdrop:close_ioe(In, Out, Err)))).

:- use_module(library(pengines)).


:- meta_predicate with_error_channel(+,0).
:- meta_predicate ignore_catch(0).
:- meta_predicate call_in_thread(0).
:- meta_predicate with_no_input(0).
:- meta_predicate with_output_channel(+,0).
:- meta_predicate with_input_channel_user(+,+,0).






%% stream_write( ?Stream, ?Out) is det.
%
% Hook To [eggdrop_e:stream_write/2] For Module Eggdrop.
% Stream Write.
%
eggdrop_io:stream_write(_Stream, Out) :- t_l:default_channel(RETURN),say(RETURN,Out).



%% stream_read( ?Stream, ?Data) is det.
%
% Hook To [eggdrop_e:stream_read/2] For Module Eggdrop.
% Stream Read.
%
eggdrop_io:stream_read(_Stream, "") :- !.
eggdrop_io:stream_read(_Stream, Data) :- prompt(Prompt, Prompt), pengines:pengine_input(_{type:console, prompt:Prompt}, Data).



%% stream_close( ?Stream) is det.
%
% Hook To [eggdrop_e:stream_close/1] For Module Eggdrop.
% Stream Close.
%
eggdrop_io:stream_close(_Stream).

eggdrop_e:stream_write(_Stream, Out) :- t_l:default_channel(RETURN),say(RETURN,Out).
eggdrop_e:stream_read(_Stream, "") :- !.
eggdrop_e:stream_read(_Stream, Data) :- prompt(Prompt, Prompt), pengines:pengine_input(_{type:console, prompt:Prompt}, Data).
eggdrop_e:stream_close(_Stream).




%% close_ioe( ?In, ?Out, ?Err) is det.
%
% Close Ioe.
%
close_ioe(In, Out, Err) :-
	close(In, [force(true)]),
        close(Err, [force(true)]),
	close(Out, [force(true)]).



:-export(add_maybe_static/2).



%% add_maybe_static( ?H, ?Vs) is det.
%
% Add Maybe Static.
%
add_maybe_static( H,Vs):- H \= (_:-_), !,add_maybe_static((H:-true),Vs).
add_maybe_static((H:-B),_Vs):- predicate_property(H,dynamic),!,assertz(((H:-B))).
add_maybe_static((H:-B),_Vs):- must_det_l((convert_to_dynamic(H),assertz(((H:-B))),functor(H,F,A),compile_predicates([F/A]))).

% ===================================================================
% IRC CALL/1
% ===================================================================
:-module_transparent(irc_filtered/4).
:-export(irc_filtered/4).



%% irc_filtered( ?Channel, ?Agent, ?CALL, ?Vs) is det.
%
% Irc Event Call Filtered.
%
irc_filtered(_Channel,_Agent,CALL,_Vs):-var(CALL),!.
irc_filtered(_Channel,_Agent,end_of_file,_Vs):-!.
irc_filtered(Channel,Agent,(H :- B ),Vs):- irc_call(Channel,Agent,add_maybe_static((H :- B),Vs),[]),!.
irc_filtered(Channel,Agent,((=>(H)) :- B ),Vs):- ((=>(H :- B)) \== ((=>(H)) :- B )),!,irc_filtered(Channel,Agent,(=>(H :- B)),Vs).
irc_filtered(Channel,Agent,'?-'(CALL),Vs):- nonvar(CALL),!,irc_call(Channel,Agent,CALL,Vs),!.
irc_filtered(Channel,Agent,'==>'(CALL),Vs):- nonvar(CALL),!,irc_call(Channel,Agent,ain(==>(CALL)),Vs),!.
irc_filtered(Channel,Agent,'=>'(CALL),Vs):- nonvar(CALL),!,irc_call(Channel,Agent,ain(CALL),Vs),!.
irc_filtered(Channel,Agent,lispy([S|TERM]),Vs):- is_list([S|TERM]),is_lisp_call_functor(S),!,
   (current_predicate(lisp_call/3), irc_call(Channel,Agent, lisp_call([S|TERM],Vs,R),['Result'=R|Vs]));
     my_wdmsg(cant_ircEvent_call_filtered(Channel,Agent,[S|TERM],Vs)).
irc_filtered(Channel,Agent,(CALL),Vs):- compound(CALL),!,irc_call(Channel,Agent,ain(CALL),Vs),!.
irc_filtered(Channel,Agent,CALL,Vs):- lmconf:chat_isRegistered(Channel,Agent,executeAll),!,irc_call(Channel,Agent,CALL,Vs),!.
irc_filtered(Channel,Agent,CALL,Vs):- my_wdmsg(unused_ircEvent_call_filtered(Channel,Agent,CALL,Vs)),!.




%% is_lisp_call_functor( ?FUNCTOR) is det.
%
% If Is A Lisp Call Functor.
%
is_lisp_call_functor('?-').
is_lisp_call_functor('?>').


:-module_transparent(irc_call/4).
:-export(irc_call/4).

agent_module(Agent,AgentModule):- 
  must(nonvar(Agent)),
  any_to_atom(Agent,AgentModule),
  must(atom(AgentModule)),
  (import_module(AgentModule,eggdrop) -> true ;
   ((add_import_module(AgentModule,baseKB,end),
   add_import_module(AgentModule,pfc,end),
   add_import_module(AgentModule,eggdrop,end),
   add_import_module(AgentModule,lmconf,end),
   add_import_module(AgentModule,clpfd,end),
   add_import_module(AgentModule,user,end)))).

  % tty_control,false


irc_expand_call(_AgentModule,Goal,Bindings,Goal,Bindings):-!.
irc_expand_call(AgentModule,Query,Bindings,Goal,ExpandedBindings):-
    must(( '$toplevel':call_expand_query(Query, ExpandedQuery,
                          Bindings, ExpandedBindings)
            ->  AgentModule:expand_goal(ExpandedQuery, Goal))).

%% irc_call( ?Channel, ?Agent, ?CALL, ?Vs) is det.
%
% Irc Event Call.
%

 % irc_call(Channel,Agent,Query,Bindings):- thread_self(main) -> with_error_to_output(irc_call(Channel,Agent,Query,Bindings)).
irc_call(Channel,Agent,Query,Bindings):-
   must(agent_module(Agent,AgentModule)),
  irc_expand_call(AgentModule,Query,Bindings,Goal,ExpandedBindings),  
   (with_error_to_output(( % When plain main
  % show_alias_streams,
     with_output_channel(Channel,          
        with_error_channel(Channel,
       ( 
        (( % show_alias_streams,
          my_wdmsg(do_irc_call(Channel,Agent,AgentModule:Goal,ExpandedBindings)),
          % srtrace, 
          (call_with_results(AgentModule,Goal,ExpandedBindings)))))))))),!.

irc_call(Channel,Agent,CALL,Vs):- once(my_wdmsg(failed_do_irc_call(Channel,Agent,CALL,Vs))),!.

% show_alias_streams:- \+ current_prolog_flag(runtime_debug,3),!.
show_alias_streams:- doall((member(Alias,[user_error,main_user_error,user_error,user_output,
  main_user_output,current_output]),
  on_x_fail((stream_property(Stream,alias(Alias)),debugm(Alias==Stream))))).

%% cit is det.
%
% Cit.
%
cit:- get_time(HH), call_in_thread(with_error_channel(dmiles:err,writeln(user_error,HH))).



%% cit2 is det.
%
% Cit Extended Helper.
%
cit2:- get_time(HH), rtrace(with_error_channel(dmiles:err,writeln(user_error,HH))).



%% cit3 is det.
%
% Cit3.
%
cit3:- get_time(HH), writeln(user_error,HH).




%% call_in_thread( :Call) is det.
%
% Call In Thread.
%
call_in_thread(CMD):- thread_self(main),!,CMD.
% call_in_thread(CMD):- !,CMD.
% call_in_thread(CMD):- thread_self(Self),thread_property(Self,alias(Alias)),Alias==egg_go,!,CMD.
call_in_thread(CMD):- thread_create(CMD,_,[detached(true)]),!.
call_in_thread(CMD):- thread_self(Self),thread_create(CMD,_,[detached(true),inherit_from(Self)]).


:- dynamic(lmcache:vars_as/1).
% :- thread_local lmcache:vars_as/1.



%% vars_as( ?VarType) is det.
%
% Hook To [lmcache:vars_as/1] For Module Eggdrop.
% Variables Converted To.
%
lmcache:vars_as(comma).

:-export(flush_all_output/0).



%% flush_all_output is det.
%
% Flush All Output.
%
flush_all_output:- on_x_fail((flush_output_safe,flush_output_safe(user_error),flush_output_safe(current_error))),!.
flush_all_output:- flush_output(user_error),flush_output.

:-export(vars_as_list/0).



%% vars_as_list is det.
%
% Variables Converted To List.
%
vars_as_list :- retractall(lmcache:vars_as(_)),asserta(lmcache:vars_as(list)).
:-export(vars_as_comma/0).



%% vars_as_comma is det.
%
% Variables Converted To Comma.
%
vars_as_comma :- retractall(lmcache:vars_as(_)),asserta(lmcache:vars_as(comma)).


attvar_to_dict_egg(AttVar,Dict):-
   get_attrs(AttVar,Att3s),
   attrs_to_pairs(Att3s,DictPairs),
   dict_create(Dict,AttVar,DictPairs).

attrs_to_pairs(att(N,V,Att3s),[N=V|DictPairs]):-!,attrs_to_pairs(Att3s,DictPairs).
attrs_to_pairs(DictPairs,DictPairs).
/*
dict_to_attvar_egg(MOD,Dict):- dict_to_attvar_egg(MOD,Dict,_),!.
dict_to_attvar_egg(MOD,_:Dict,Out):- \+ compound(Dict),!,Out=Dict.
dict_to_attvar_egg(MOD,Mod:Dict,Out):-
   is_dict(Dict),dict_pairs(Dict,M,Pairs),
   (atom(M)->atts_put(+,Out,M,Pairs);
   (var(M)-> (M=Out,put_atts(Out,Mod:Pairs)))),!.
dict_to_attvar_egg(MOD,Mod:Dict,Out):-
  compound_name_arguments(Dict,F,Args),
   maplist(dict_to_attvar_egg(MOD),Args,ArgsO),!,
   compound_name_arguments(Out,F,ArgsO).
*/



%% format_nv( ?N, ?V) is det.
%
% Format Nv.
%
format_nv(N,V):- format('~w=',[N]),write_v(V).

write_v(V):- attvar(V),if_defined(attvar_to_dict_egg(V,Dict)),writeq(Dict),!.
write_v(V):- var(V),(var_property(V,name(EN))->write(EN);writeq(V)),!.
write_v(V):- writeq(V).

:-export(write_varvalues2/1).



%% write_varvalues2( ?Vs) is det.
%
% Write Varvalues Extended Helper.
%
% write_varvalues2(Vs):-lmcache:vars_as(comma),!,write_varcommas2(Vs),write_residuals(Vs).

write_varvalues2([]):-!,flush_all_output.
write_varvalues2(Vs):-
   flush_all_output,
   write('% '),
   copy_term(Vs,Vs,Goals),
   write_varvalues3(Vs),
   write_goals(Goals),!,
   flush_all_output.

writeqln(G):-writeq(G),write(' ').

write_goals([]):-!.
write_goals(List):-write_goals0(List),!,write(' ').
write_goals0([G|Rest]):-write(' '),writeq(G),write_goals(Rest).
write_goals0([]).



%% write_varvalues3( ?ARG1) is det.
%
% Write Varvalues3.
%
write_varvalues3([N=V]):- format_nv(N,V),!.
write_varvalues3([N=V|Vs]):-format_nv(N,V),write(', '),write_varvalues3(Vs),!.





%% write_varcommas2( ?Vs) is det.
%
% Write Varcommas Extended Helper.
%
write_varcommas2(Vs):- copy_term(Vs,CVs),numbervars(CVs,6667,_,[singletons(true),attvar(skip)]),write_varcommas3(CVs).



%% write_varcommas3( ?ARG1) is det.
%
% Write Varcommas3.
%
write_varcommas3([N=V]):-format_nv(N,V),!.
write_varcommas3([N=V|Vs]):-format_nv(N,V), write(','),!,write_varcommas3(Vs),!.



:-export(remove_anons/2).



%% remove_anons( ?ARG1, ?VsRA) is det.
%
% Remove Anons.
%
remove_anons([],[]).
remove_anons([N=_|Vs],VsRA):-atom_concat('_',_,N),!,remove_anons(Vs,VsRA).
remove_anons([N=V|Vs],[N=V|VsRA]):-remove_anons(Vs,VsRA).


un_user1(user:P,P):-!.
un_user1(system:P,P):-!.
un_user1(eggdrop:CMDI,CMDI):-!.
un_user1(P,P).

%% call_with_results(Module, ?CMDI, ?Vs) is det.
%
% Call Using Results.
%
:-module_transparent(call_with_results/3).
:-export(call_with_results/3).
call_with_results(Module,CMDI0,Vs):- remove_anons(Vs,VsRA),!,
  un_user1(CMDI0,CMDI),
  b_setval('$term', :- CMDI), /* DRM: added for expansion hooks*/
  locally(t_l:disable_px,user:expand_goal(CMDI,CMD)),
  (CMD==CMDI->true;my_wdmsg(call_with_results(Module,CMDI->CMD))),
    show_call(call_with_results_0(Module:CMD,VsRA)),!.




%% call_with_results_0( :Call, ?Vs) is det.
%
% call Using results  Primary Helper.
%
:-module_transparent(call_with_results_0/2).
:-export(call_with_results_0/2).
call_with_results_0(CMD,Vs):-
 set_varname_list( Vs),
 b_setval('$goal_term', CMD), /* DRM: added for expansion hooks*/
 flag(num_sols,_,0),
 (call_with_results_nondet(CMD,Vs) *->
  (deterministic(X),flag(num_sols,N,0),(N\==0->YN='Yes';YN='No'), write(' '),(X=true->write(det(YN,N));write(nondet(YN,N)))) ;
     (deterministic(X),flag(num_sols,N,0),(N\==0->YN='Yes';YN='No'),write(' '),(X=true->write(det(YN,N));write(nondet(YN,N))))).




%% call_with_results_nondet( :CMDIN, ?Vs) is nondet.
%
% call Using results  Extended Helper.
%
:-module_transparent(call_with_results_nondet/2).
:-export(call_with_results_nondet/2).
call_with_results_nondet(CMDIN,Vs):-
   CMDIN = CMD,functor_h(CMD,F,A),A2 is A+1,CMD=..[F|ARGS],atom_concat(F,'_with_vars',FF),
   (current_predicate(FF/A2)-> (CMDWV=..[FF|ARGS],append_term(CMDWV,Vs,CCMD)); CCMD=CMD),!,
   call_with_results_nondet_vars(CCMD,Vs).
call_with_results_nondet(CCMD,Vs):- call_with_results_nondet_vars(CCMD,Vs).


%% call_with_results_nondet_vars( :CCMD, ?Vs) is nondet.
%
% Call Using Results Helper Number 3..
%
:-module_transparent(call_with_results_nondet_vars/2).
:-export(call_with_results_nondet_vars/2).
call_with_results_nondet_vars(CCMD,Vs):-
   user:show_call(eggdrop,(CCMD,flush_output_safe)), flag(num_sols,N,N+1), deterministic(Done),
     (once((Done==true -> (once(\+ \+ write_varvalues2(Vs)),write('% ')) ; (once(\+ \+ write_varvalues2(Vs)),N>28)))).


%% with_output_channel( +Channel, :Call) is det.
%
% Using Output Channel.
%
:-export(with_output_channel/2).
:-module_transparent(with_output_channel(+,0)).
% with_output_channel(Channel,CMD):- CMD.
with_output_channel(Channel,CMD):-
  with_output_to_predicate(say(Channel),CMD).



%% with_error_channel( +Agent, :Call) is det.
%
% Using Error Channel.
%
:- export(with_error_channel/2).
:- module_transparent(with_error_channel/2).
/* *
with_error_channel(_Agent, CMD):-  !, CMD.
with_error_channel(Agent,CMD):- fail,
   current_input(IN),current_output(OUT),
   get_main_error_stream(MAINERROR),
   set_prolog_IO(IN,OUT,MAINERROR),
   new_memory_file(MF),
   open_memory_file(MF, write, ERR), 
   set_prolog_IO(IN,OUT,ERR),!,
   setup_call_cleanup_each(CMD,(ignore_catch(flush_output(ERR)),ignore_catch(close(ERR)),read_from_agent_and_send(Agent,MF))).
* */
with_error_channel(Agent, CMD):- !,  with_error_to_predicate(say(Agent),CMD).
with_error_channel(_Agent, CMD):-  !, CMD.
with_error_channel(_Agent,CMD):- !, with_error_to_output(CMD).




%% with_input_channel_user( +Channel, +User, :Call) is det.
%
% Using Input Channel User.
%
with_input_channel_user(_,_,CMD):- !, with_no_input(CMD).
with_input_channel_user(Channel,User,CMD):-
  with_input_from_predicate(last_read_from(Channel,User),CMD).

:-export(with_io/1).
:-meta_predicate(with_io(0)).



%% with_io( :Call) is det.
%
% Using Input/output.
%
with_io(CMD):-!,CMD.
with_io(CMD):-
 with_dmsg_to_main((
  current_input(IN),current_output(OUT),get_thread_current_error(Err),
  call_cleanup(set_prolog_IO(IN,OUT,Err),CMD,(set_input(IN),set_output(OUT),set_error_stream(Err))))).


%% with_no_input( :Call) is det.
%
% Using No Input.
%


% with_no_input(CMD):- CMD.
with_no_input(CMD):-  current_input(Prev), open_chars_stream([e,n,d,'_',o,f,'_',f,i,l,e,'.'],In),set_input(In),!,call_cleanup(CMD,set_input(Prev)).
with_no_input(CMD):- open_chars_stream([e,n,d,'_',o,f,'_',f,i,l,e,'.'],In),current_output(OUT), set_prolog_IO(In,OUT,user_error ),CMD.






%% ignore_catch( :CALL) is det.
%
% Ignore Catch.
%
ignore_catch(CALL):-ignore(catch(CALL,E,my_wdmsg(ignore_catch(E:CALL)))).


:- meta_predicate with_error_to_output(0).



%% with_error_to_output( :Call) is det.
%
% Using Error Converted To Output.
%
with_error_to_output(CMD):-
   current_input(IN),current_output(OUT),!,
   with_io((set_prolog_IO(IN,OUT,OUT), CMD)).



%% read_from_agent_and_send( ?Agent, ?MF) is det.
%
% Read Converted From Agent And Send.
%
read_from_agent_and_send(Agent,MF):- open_memory_file(MF, read, Stream,[ free_on_close(true)]),ignore_catch(read_codes_and_send(Stream,Agent)),ignore_catch(close(Stream)).




%% read_codes_and_send( ?IN, ?Agent) is det.
%
% Read Text And Send.
%
read_codes_and_send(IN,Agent):- at_end_of_stream(IN),!,my_wdmsg(say(Agent,done)).
read_codes_and_send(IN,Agent):- repeat,read_line_to_string(IN,Text),say(Agent,Text),at_end_of_stream(IN),!.

%:-servantProcessCreate(killable,'Consultation Mode Test (KIFBOT!) OPN Server',consultation_thread(swipl,3334),Id,[]).




%% update_changed_files_eggdrop is det.
%
% Update Changed Files Eggdrop.
%

  % update_changed_files_eggdrop :- !.
update_changed_files_eggdrop :-
 with_dmsg_to_main(( with_no_dmsg((
        set_prolog_flag(verbose_load,true),
        ensure_loaded(library(make)),
	findall(File, make:modified_file(File), Reload0),
	list_to_set(Reload0, Reload),
        update_changed_files_eggdrop(Reload))))),!.

update_changed_files_eggdrop([]):-sleep(0.005).
update_changed_files_eggdrop(Reload):-
	(   prolog:make_hook(before, Reload)
	->  true
	;   true
	),
	print_message(silent, make(reload(Reload))),
	make:maplist(reload_file, Reload),
	print_message(silent, make(done(Reload))),
	(   prolog:make_hook(after, Reload)
	->  true
	;
           true %(list_undefined,list_void_declarations)
	),!.


% ===================================================================
% IRC OUTPUT
% ===================================================================
:-export(sayq/1).



%% sayq( ?D) is det.
%
% Sayq.
%
sayq(D):-sformat(S,'~q',[D]),!,say(S),!.

:-export(say/1).



%% say( ?D) is det.
%
% Say.
%
say(D):- t_l:default_channel(C),say(C,D),!.
say(D):- say("#logicmoo",D),!.





%% is_empty( ?A) is det.
%
% If Is A Empty.
%
is_empty(A):-egg_to_string(A,S),string_length(S,0).


%% get_session_prefix( ?ID) is det.
%
% Get Session Prefix.
%
get_session_prefix(ID):-t_l:session_id(ID),!.
get_session_prefix(ID):-t_l:default_user(ID),!.
get_session_prefix('').


%% put_server_count( ?Current) is det.
%
% Hook To [t_l:put_server_count/1] For Module Eggdrop.
% Put Server Count.
%
:-thread_local t_l: put_server_count/1.
:-thread_local t_l: put_server_no_max/0.
t_l:put_server_count(0).




%% check_put_server_count( ?Max) is det.
%
% Check Put Server Count.
%
check_put_server_count(0):- if_defined(t_l:put_server_no_max),retractall(t_l:put_server_count(_)),asserta(t_l:put_server_count(0)).
check_put_server_count(Max):-retract(t_l:put_server_count(Was)),Is is Was+1,asserta(t_l:put_server_count(Is)),!,Is =< Max.
% 


%% to_egg( ?X) is det.
%
% Converted To Egg.
%
to_egg(X):-to_egg('~w',[X]),!.



%% to_egg( ?X, ?Y) is det.
%
% Converted To Egg.
%
to_egg(X,Y):-once(egg:stdio(_Agent,_InStream,OutStream)),once((sformat(S,X,Y),format(OutStream,'~s\n',[S]),!,flush_output_safe(OutStream))).




%% escape_quotes( ?LIST, ?ISO) is det.
%
% Escape Quotes.
%
escape_quotes(LIST,ISO):-
           %dmsg(q(I)),
            %    term_string(I,IS),!,
		%string_to_list(IS,LIST),!,
		list_replace_egg(LIST,92,[92,92],LISTM),
		list_replace_egg(LISTM,34,[92,34],LISTM2),
                list_replace_egg(LISTM2,91,[92,91],LIST3),
                list_replace_egg(LIST3,36,[92,36],LISTO),
                =(LISTO,ISO),!.
		% text_to_string(LISTO,ISO),!.




%% list_replace_egg( ?List, ?Char, ?Replace, ?NewList) is det.
%
% List Replace Egg.
%
list_replace_egg(List,Char,Replace,NewList):-
	append(Left,[Char|Right],List),
	append(Left,Replace,NewLeft),
	list_replace_egg(Right,Char,Replace,NewRight),
	append(NewLeft,NewRight,NewList),!.
list_replace_egg(List,_Char,_Replace,List):-!.







%% say(?Channel, ?List) is det.
%
% Say List.
%
:- export(say/2).
say(Channel,List):-
  get_session_prefix(Prefix),!,
  say(Channel,Prefix,List),!.








split_at(Text,At,LCodes,RCodes):- number(At),!, sub_atom(Text,0,At,_,LCodes),sub_atom(Text,At,_,0,RCodes),!.
split_at(Text,At,LCodes,RCodes):- sub_atom(Text,Before,1,_After,At),sub_atom(Text,0,Before,_,LCodes),sub_atom(Text,Before,_,0,RCodes),!.



%% flushed_privmsg( ?Channel, ?Fmt, ?Args) is det.
%
% Flushed Privmsg.
%
flushed_privmsg(Channel,Fmt,Args):-
  format(string(NS),Fmt,Args),
  privmsg(Channel,NS),!.

empty_prefix(''):-!.
empty_prefix(""):-!.
empty_prefix(``):-!.

%% say(+Channel, +Agent, ?List) is det.
%
% Say List.
%
:-export(say/3).
say(Channel:Prefix,_ID,List):-nonvar(Channel),!,say(Channel,Prefix,List).
say(Channel,Prefix,Data):- empty_prefix(Prefix),!,say(Channel,Channel,Data).
say(Channel,Prefix,Data):- empty_prefix(Channel),!,say(Prefix,Prefix,Data).
say(_Channel,_Prefix,Data):- empty_prefix(Data),!.
say(Channel,Prefix,[N|L]):- !,maplist(say(Channel,Prefix),[N|L]).
say(Channel,Prefix,Data):- (egg_to_string(Channel,S)-> Channel\==S),!,say(S,Prefix,Data).
say(Channel,Prefix,Data):- (egg_to_string(Prefix,S)-> Prefix\==S),!,say(Channel,S,Data).
say(Channel,Prefix,Data):- \+ string(Data), egg_to_string(Data, S)-> S\==Data,!,say(Channel,Prefix,S),!.
say(Channel,Prefix,Data):- \+ string(Data), sformat(SF,'~p',[Data]), say(Channel,Prefix,SF),!.

%say(Channel,Text):- my_wdmsg(will_say(Channel,Text)),fail.
% say(_,NonList,Data):-is_stream(NonList),!,say(NonList,"console",Data),!.


say(Channel,Prefix,Text):- split_at(Text,'\n',LCodes,RCodes), say(Channel,Prefix,LCodes), say(Channel,Prefix,RCodes).
say(Channel,_Prefix,Text):- privmsg(Channel,Text),!.
%say(Channel,Text):- my_wdmsg(will_say(Channel,Text)),fail.





%% privmsg( ?Channel, ?Text) is det.
%
% Privmsg Primary Helper.
%
privmsg(Channel,Text):-atom_length(Text,Len),Len>430,
   split_at(Text,430,LCodes,RCodes),privmsg1(Channel,LCodes),!,privmsg(Channel,RCodes).
privmsg(Channel,Text):-privmsg1(Channel,Text).

/*
% privmsg2(Channel,Text):-on_f_log_ignore(format(OutStream,'\n.msg ~s ~s\n',[Channel,Text])).

% privmsg2(Channel,Text):- escape_quotes(Text,N),ignore(catch(format(OutStream,'\n.tcl putserv "PRIVMSG ~s :~s" ;  return "noerror ."\n',[Channel,N]),_,fail)),!.


say(Agent,Prefix,Out):-sformat(SF,'~w: ~p',[Prefix,Out]), say(Agent,SF),!.

say(Channel,[Channel,': '|Data]):-nonvar(Data),say(Channel,Data),!.


   must(flushed_privmsg(Channel,'~w: ~w',[Prefix,N])),
	say(Channel,Prefix,L),!.

*/

%% privmsg1(  ?Channel, ?Text) is det.
%
% Privmsg Secondary Helper.
%
privmsg1(Channel,Text):-check_put_server_count(30)->privmsg2(Channel,Text);
  ignore(check_put_server_count(100)->privmsg_session(Channel,Text);true).


%% privmsg2(  ?Channel, ?Text) is det.
%
% Privmsg Extended Helper.
%
privmsg2(Channel:_,Text):-nonvar(Channel),!,privmsg2(Channel,Text).
privmsg2(_:Channel,Text):-nonvar(Channel),!,privmsg2(Channel,Text).
privmsg2(Channel,Text):- sleep(0.2),escape_quotes(Text,N), to_egg('.tcl putquick "PRIVMSG ~s :~s"\n',[Channel,N]),!.
privmsg2(Channel,Text):- sleep(0.2), to_egg('.tcl putquick "PRIVMSG ~s :~w"\n',[Channel,Text]),!.

%% putnotice( ?Channel, ?Text) is det.
%
% Putnotice.
%
putnotice(Channel,Text):- escape_quotes(Text,N),to_egg('.tcl putserv "NOTICE ~s :~w"\n',[Channel,N]).


%% privmsg_session(  ?Channel, ?Text) is det.
%
% Privmsg Session.
%
privmsg_session(Channel,Text):- t_l:session_id(ID),(ID==Channel->privmsg2(Channel,Text);privmsg2(ID,Text)).

% ===================================================================
% Startup
% ===================================================================




%% show_thread_exit is det.
%
% Show Thread Exit.
%
show_thread_exit:- my_wdmsg(warn(eggdrop_show_thread_exit)).




%% egg_go_fg is det.
%
% Egg Go Fg.
%
egg_go_fg:-
  %deregister_unsafe_preds,
  %set_prolog_flag(xpce,false),
  %with_no_x
  consultation_thread(swipl,3334).


%% egg_go is det.
%
% Egg Go.
%

% egg_go:- egg_go_fg,!.
egg_go:- thread_property(R,status(running)),R == egg_go,!.
egg_go:- thread_property(_,alias(egg_go)),threads,fail.
egg_go:- thread_create(egg_go_fg,_,[alias(egg_go),detached(true),an_exit(show_thread_exit)]).


egg_nogo:-thread_signal(egg_go,thread_exit(requested)).
/*
:- source_location(S,_),forall(source_file(H,S),ignore(( ( \+predicate_property(H,PP),member(PP,[(multifile),built_in]) ),
 functor(H,F,A),module_transparent(F/A),export(F/A),user:import(H)))).
*/



%% read_one_term_egg( ?Stream, ?CMD, ?Vs) is det.
%
% Read One Term Egg.
%
:-export(read_one_term_egg/3).
:-module_transparent(read_one_term_egg/3).

read_one_term_egg(Stream,CMD,Vs):- \+ is_stream(Stream),l_open_input(Stream,InStream),!, 
       with_stream_pos(InStream,show_entry(read_one_term_egg(InStream,CMD,Vs))).
read_one_term_egg(Stream,CMD,_ ):- at_end_of_stream(Stream),!,CMD=end_of_file,!.
% read_one_term_egg(Stream,CMD,Vs):- catch((input_to_forms(Stream,CMD,Vs)),_,fail),CMD\==end_of_file,!.
read_one_term_egg(Stream,CMD,Vs):- catch((read_term(Stream,CMD,[variable_names(Vs)])),_,fail),CMD\==end_of_file,!.
read_one_term_egg(Stream,unreadable(String),_):-catch((read_stream_to_codes(Stream,Text),string_codes(String,Text)),_,fail),!.
read_one_term_egg(Stream,unreadable(String),_):-catch((read_pending_input(Stream,Text,[]),string_codes(String,Text)),_,fail),!.


:-export(read_each_term_egg/3).
:-module_transparent(read_each_term_egg/3).



%% read_each_term_egg( ?S, ?CMD, ?Vs) is det.
%
% Read Each Term Egg.
%
read_each_term_egg(S,CMD,Vs):-   
  show_failure(( l_open_input(S,Stream),  
      findall(CMD-Vs,(
       repeat,
       read_one_term_egg(Stream,CMD,Vs),
       (CMD==end_of_file->!;true)),Results),!,
  ((member(CMD-Vs,Results),CMD\==end_of_file)*->true;read_one_term_egg(S,CMD,Vs)))).

%:- ensure_loaded(library(logicmoo/common_logic/common_logic_sexpr)).



%% read_egg_term( ?S, ?CMD, ?Vs) is nondet.
%
% Read Each Term Egg.
%
:-export(read_egg_term/3).
:-module_transparent(read_egg_term/3).
:- use_module(library(sexpr_reader)).

read_egg_term(S,CMD0,Vs0):- text_to_string(S,String),
   split_string(String,""," \r\n\t",[SString]),
   atom_concat(_,'.',SString),
   open_string(SString,Stream),
   findall(CMD0-Vs0,(
       catch(read_term(Stream,CMD,[double_quotes(string),module(eggdrop),variable_names(Vs)]),_,fail),!,
       ((CMD=CMD0,Vs=Vs0);
        read_egg_term_more(Stream,CMD0,Vs0))),List),!,
   member(CMD0-Vs0,List).

read_egg_term(S,lispy(CMD),Vs):- text_to_string(S,String),
   input_to_forms(String,CMD,Vs),!,is_list(CMD).

read_egg_term_more(Stream,CMD,Vs):- 
       repeat, 
        catch(read_term(Stream,CMD,[double_quotes(string),module(eggdrop),variable_names(Vs)]),_,CMD==err),
        (CMD==err->(!,fail);true),
        (CMD==end_of_file->!;true).

:- user:import(read_egg_term/3).

:- module_transparent((egg_go)/0).
:- module_transparent((egg_go_fg)/0).
:- module_transparent((show_thread_exit)/0).
:- module_transparent((list_replace_egg)/4).
:- module_transparent((escape_quotes)/2).
:- module_transparent((to_egg)/2).
:- module_transparent((to_egg)/1).
:- module_transparent((check_put_server_count)/1).
:- module_transparent((is_empty)/1).
:- module_transparent((get_session_prefix)/1).
:- module_transparent((say)/3).
:- module_transparent((say)/2).
:- module_transparent((say)/1).
:- module_transparent((sayq)/1).
:- module_transparent((update_changed_files_eggdrop)/0).
:- module_transparent((read_codes_and_send)/2).
:- module_transparent((read_from_agent_and_send)/2).
:- module_transparent((remove_anons)/2).
:- module_transparent((write_varcommas3)/1).
:- module_transparent((write_varcommas2)/1).
:- module_transparent((write_varvalues3)/1).
:- module_transparent((write_varvalues2)/1).
:- module_transparent((format_nv)/2).
:- module_transparent((vars_as_comma)/0).
:- module_transparent((vars_as_list)/0).
:- module_transparent((flush_all_output)/0).
:- module_transparent((cit3)/0).
:- module_transparent((cit2)/0).
:- module_transparent((cit)/0).
:- module_transparent((is_lisp_call_functor)/1).
:- module_transparent((add_maybe_static)/2).
:- module_transparent((close_ioe)/3).
:- module_transparent((eggdrop_bind_user_streams)/0).
:- module_transparent((unreadable)/1).
:- module_transparent((recordlast)/3).
:- module_transparent((ignored_channel)/1).
:- module_transparent((ignored_source)/1).
:- module_transparent((last_read_from)/3).
:- module_transparent((irc_receive)/5).
:- module_transparent((pubm)/5).
:- module_transparent((ctcp)/6).
:- module_transparent((msgm)/5).
:- module_transparent((join)/4).
:- module_transparent((part)/5).
:- module_transparent((get2react)/1).
:- module_transparent((consultation_codes)/3).
:- module_transparent((is_callable_egg)/1).
:- module_transparent((consultation_thread)/2).
:- module_transparent((eggdropConnect)/4).
:- module_transparent((eggdropConnect)/2).
:- module_transparent((eggdropConnect)/0).
:- module_transparent((deregister_unsafe_preds)/0).
:- module_transparent((remove_pred_egg)/3).
:- module_transparent((unsafe_preds_egg)/3).
:- module_transparent((my_wdmsg)/1).
:- module_transparent((ctrl_port)/1).
:- module_transparent((ctrl_pass)/1).
:- module_transparent((ctrl_nick)/1).
:- module_transparent((ctrl_server)/1).
:- module_transparent((bot_nick)/1).
:- module_transparent((write_v)/1).
:- module_transparent((source_and_module_for_agent)/3).
:- module_transparent((with_rl)/1).
:- module_transparent((with_no_input)/1).
:- module_transparent((with_io)/1).
:- module_transparent((with_input_channel_user)/3).
:- module_transparent((with_error_to_output)/1).
:- module_transparent((ignore_catch)/1).
:- module_transparent((call_in_thread)/1).


:- ignore((source_location(S,_),prolog_load_context(module,M),module_property(M,class(library)),
 forall(source_file(M:H,S),
 ignore((functor(H,F,A),
  ignore(((\+ atom_concat('$',_,F),(export(F/A) , current_predicate(system:F/A)->true; system:import(M:F/A))))),
  ignore(((\+ predicate_property(M:H,transparent), module_transparent(M:F/A), \+ atom_concat('__aux',_,F),debug(modules,'~N:- module_transparent((~q)/~q).~n',[F,A]))))))))).


:- source_location(S,_),prolog_load_context(module,M),
 forall(source_file(M:H,S),ignore((functor(H,F,A),
   nop((\+ mpred_database_term(F,A,_))),
   F\=='$mode',
   F\=='$pldoc',
   ignore(((\+ atom_concat('$',_,F),export(F/A)))),
   \+ predicate_property(M:H,transparent),
   ignore(((\+ atom_concat('__aux',_,F),format('~N:- module_transparent((~q)/~q).~n',[F,A])))),
   M:multifile(M:F/A),
   M:module_transparent(M:F/A)))).

% :- ircEvent("dmiles","dmiles",say("(?- (a b c))")).

