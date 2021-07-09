:- module(eggdrop, [egg_go_maybe/0,

  add_maybe_static/2,bot_nick/1,call_in_thread_ed/2,call_for_results/2,check_put_server_count/1,cit/0,close_ioe/3,consultation_codes/3,
  consultation_thread/2,ctcp/6,ctrl_nick/1,ctrl_pass/1,ctrl_port/1,ctrl_server/1,deregister_unsafe_preds/0,egg_go/0,
  egg_go_fg/0,eggdropConnect/0,eggdropConnect/2,eggdropConnect/4,eggdrop_bind_user_streams/0,escape_quotes/2,flush_all_output/0,
  privmsg_prefixed/2,irc_process/3,
  privmsg_prefixed/2,
  privmsg1/2,
  privmsg2/2,
  say_owner/1,
  format_nv/2,get2react/1,get_session_prefix/1,ignore_catch/1,ignored_source/1,
  ircEventUsed/3,ircEventNow/3,ircEvent/3,irc_really_call/4,irc_filtered_call/4,
  irc_receive/5,is_callable_egg/1,is_empty/1,is_lisp_call_functor/1,join/4,list_replace_egg/4,msgm/5,my_wdmsg/1,
  part/5,privmsg/2,privmsg_session/2,pubm/5,
  notc/5,
  put_notice/2,read_codes_and_send/2,read_each_term_egg/3,read_from_agent_and_send/2,
  read_one_term_egg/4,recordlast/3,remove_anons/2,remove_pred_egg/3,say/1,say/2,say/2,
  sayq/1,show_thread_exit/0,put_egg/1,put_egg/2,unreadable/1,unsafe_preds_egg/3,
  update_changed_files_eggdrop/0,vars_as_comma/0,vars_as_list/0,with_error_channel/2,with_error_to_output/1,with_input_channel_user/3,with_io/1,with_no_input/1,
  with_output_channel/2,with_rl/1,egg:stdio/3,lmcache:vars_as/1,ignored_channel/1,lmconf:chat_isWith/3,lmconf:chat_isRegistered/3,last_read_from/3,
  lmconf:chat_isChannelUserAct/4,tg_name_text/3,
  attvar_to_dict_egg/2,
  % dict_to_attvar_egg/2,
  format_nv/2
  % eggdrop_e:stream_close/1,eggdrop_e:stream_read/2,eggdrop_e:stream_write/2,eggdrop_io:stream_close/1,eggdrop_io:stream_read/2,
  % eggdrop_io:stream_write/2,t_l:put_server_count/1,t_l:put_server_no_max/0,t_l:session_id/1
  ]).

:- set_module(class(library)).

:- if(exists_source(library(logicmoo_web_long_message))).
:- use_module(library(logicmoo_web_long_message)).
:- endif.

maybe_http_port(Port):-
 thread_propery(_,alias(Alias)),atom_concat('httpd@',X,Alias),atom_concat(APort,'_1',X),atom_number(APort,Port).

:- dynamic(lmconf:irc_bot_nick/1).
lmconf:irc_bot_nick("PrologMUD").
reg_egg_builtin(PIs):- % baseKB:ain(prologBuiltin(PIs)),
  baseKB:assert_if_new(baseKB:rtArgsVerbatum(PIs)),export(PIs).

:- module_transparent(egg_dmsg_to_main/1).
:- export(egg_dmsg_to_main/1).
egg_dmsg_to_main(G):- egg_trace,!, call(G).
egg_dmsg_to_main(G):- with_dmsg_to_main(G).


:- module_transparent(egg_notrace/1).
:- export(egg_notrace/1).
egg_notrace(G):- call(G).

:- export(egg_trace/0).
egg_trace :-  \+ is_in_egg.

is_in_egg:- \+ thread_self(main),  \+ thread_self(swipl), !.
% is_in_egg.

:- if(exists_source(irc_hooks)).
:- reexport(irc_hooks).
:- endif.

:- system:use_module(library(occurs)).
:- system:use_module(library(logicmoo_common)).
:- system:use_module(library(backcomp)).
:- system:use_module(library(readutil)).
:- system:use_module(library(prolog_stream)).
:- system:use_module(library(predicate_streams)).
%:- use_module(library(clpfd),except([sum/3])).
%:- use_module(library(clpfd),except([sum/3,op(_,_,_)])).

:- if((fail,exists_source(library(atts)))).
:- set_prolog_flag(metaterm,true).
:- use_module(library(atts)).
:- endif.

:- if(exists_source(library(logicmoo_utils_all))).
:- user:use_module(library(logicmoo_utils_all)).
:- endif.

:- if(exists_source(library(sexpr_reader))).
% :- ensure_loaded(library(sexpr_reader)).
:- endif.

/*
:- if(\+ current_predicate(rdf_rewrite:arity/2)).
:- multifile(rdf_rewrite:arity/2).
:- module_transparent(rdf_rewrite:arity/2).
:- dynamic(rdf_rewrite:arity/2).
:- endif.

:- kb_shared(baseKB:prologHybrid/1).
:- kb_shared(baseKB:prologBuiltin/1).
*/
:- export(baseKB:rtArgsVerbatum/1).
:- import(baseKB:rtArgsVerbatum/1).
:- export(rdf_rewrite:arity/2).
:- import(rdf_rewrite:arity/2).
:- export(baseKB:mtExact/1).
:- import(baseKB:mtExact/1).



% :- set_prolog_flag(dialect_pfc,cwc).
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
 op(300,fx,'-')),eggdrop).

:- meta_predicate
        call_in_thread_ed(+,0),
        call_for_results(0, ?),
        call_for_results_0(0, ?),
        call_for_results_1(0, ?),
        call_for_results_2(0, ?),
        call_for_results_3(0, ?),
        ignore_catch(0),
        with_error_channel(+, 0),
        with_error_to_output(0),
        with_input_channel_user(+, +, 0),
        with_io(0),
        with_no_input(0),
        with_output_channel(+, 0),
        with_rl(0).


%% my_wdmsg( ?Msg) is det.
%
% My Wdmsg.
%
my_wdmsg(Msg):- ignore(quietly(my_wdmsg0(Msg))),!.

:- dynamic(lmcache:real_user_output/1).
:- volatile(lmcache:real_user_output/1).
:- dynamic(lmcache:real_user_error/1).
:- volatile(lmcache:real_user_error/1).

my_wdmsg0(List):-is_list(List),catch(text_to_string(List,CMD),_,fail),List\==CMD,!,my_wdmsg(CMD).
my_wdmsg0(Msg):- if_defined(lmcache:real_user_error(S)),!,nonvar(S), my_wdmsg(S,Msg).
my_wdmsg0(Msg):- stream_property(S,alias(main_user_error)),!,my_wdmsg(S,Msg).
my_wdmsg0(Msg):- get_main_error_stream(ERR),!,my_wdmsg(ERR,Msg).
my_wdmsg0(Msg):- debugm(Msg).

my_wdmsg(S,Msg):- string(Msg),format(S,'~N% ~w~N',[Msg]),flush_output_safe(S),!.
my_wdmsg(S,Msg):- format(S,'~N% ~q.~n',[Msg]),flush_output_safe(S),!.

:- dynamic(lmconf:chat_isWith/3).
:- thread_local(t_l:disable_px/0).

egg_to_string(A,S):- quietly(egg_to_string0(A,S)).
egg_to_string0(A,S):- var(A),!,trace_or_throw(var_egg_to_string(A,S)).
egg_to_string0(A,S):- on_x_fail(text_to_string(A,S)),!.
egg_to_string0([A],S):- on_x_fail(atom_string(A,S)),!.
egg_to_string0(A,S):- on_x_fail(atom_string(A,S)),!.

save_egg_booting_data :- source_file_property(reloading, true) ,!,my_wdmsg(skip_save_egg_booting_data(source_file_property(reloading, true))).
save_egg_booting_data :- thread_self(M), M \== main,!,my_wdmsg(skip_save_egg_booting_data(thread_self(M))).
save_egg_booting_data :- stream_property(S,alias(main_user_output)),!,is_stream(S),my_wdmsg(skip_save_egg_booting_data(main_user_output)).
save_egg_booting_data :-
 \+ \+ ignore((stream_property(S,alias(user_output)), asserta(lmcache:real_user_output(S)),set_stream(S,alias(main_user_output)))),
 \+ \+ ignore((stream_property(S,alias(user_error )), asserta(lmcache:real_user_error(S)),!,set_stream(S,alias(main_user_error)))),
   my_wdmsg(save_egg_booting_data("HI there")).

:- during_boot(save_egg_booting_data).

%:- my_wdmsg("HI there").

%:- use_module(library(logicmoo/virtualize_source)).
%:- virtualize_source_file.


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
ctrl_server('localhost').



%% ctrl_nick( ?Swipl1) is det.
%
% Ctrl Nick.
%
ctrl_nick("swipl").



%% ctrl_pass( ?Top5ecret1) is det.
%
% Ctrl Pass.
%
ctrl_pass("swipl123").



%% ctrl_port( ?Port) is det.
%
% Ctrl Port.
%
ctrl_port(3334).

:- endif.


 :- meta_predicate call_for_results_3(0,*).
 :- meta_predicate call_for_results_2(0,*).
 :- meta_predicate call_for_results_1(0,*),with_rl(0).


:- module_transparent(ircEvent/3).
% from https://github.com/logicmoo/PrologMUD/tree/master/src_lib/logicmoo_util
% supplies locally/2,atom_concats/2, dmsg/1, my_wdmsg/1, must/1, if_startup_script/0

/*
TODO

* one may consider letting it wait until a query is completed with a `.'

* consider using numbervars/[3,4] <http://www.swi-prolog.org/pldoc/man?section=manipterm#numbervars/3>,
  <http://www.swi-prolog.org/pldoc/man?section=termrw#write_term/2> (option numbervars)
   for printing the variables (named ones in query, and freshly generated ones)

* skip printing toplevel vars marked with "_" in  findall(_X, fact(_X), Xs).

* perhaps use "commalists" instead of ordinary lists for each solution ? (makes it look more like a traditional interactor reply, and looks more sensible, logically)

*/




%% lmconf:chat_isRegistered( ?Channel, ?Agent, ?Type) is nondet.
%
% If Is A Registered.
%
lmconf:chat_isRegistered(Channel,Agent,What):- lmconf:chat_isWith(What,Channel,Agent).
lmconf:chat_isRegistered(Channel,Agent,_):- (ground(Channel);ground(Agent)), lmconf:chat_isWith(ignored,Channel,Agent),!,fail.
lmconf:chat_isRegistered(_,"someluser",Execute):- Execute==execute,!,fail.
%lmconf:chat_isRegistered("#ai",_, execute):-ignore(fail).
lmconf:chat_isRegistered("#pigface",_,execute):-ignore(fail).  % havent been there since 2001
lmconf:chat_isRegistered("#logicmoo",_,execute):-ignore(fail).
lmconf:chat_isRegistered("#kif",_,execute):-ignore(fail).
%lmconf:chat_isRegistered("#rdfig",_,execute):-ignore(fail).
%lmconf:chat_isRegistered("##prolog",_,execute).
lmconf:chat_isRegistered("#logicmoo_mud",_,execute).
lmconf:chat_isRegistered("#logicmoo_mud",_,mud).
% all may execture since they are using ?-
%lmconf:chat_isRegistered(_,_,execute):-!.

get_isRegistered(Channel,Agent,What):- lmconf:chat_isRegistered(Channel,Agent,What).

:- export((egg_go/0,ircEvent/3,call_for_results/2,get_isRegistered/3,lmconf:chat_isRegistered/3)).
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
  M:asserta((P:- (my_wdmsg(error(call(P))),throw(permission_error(M:F/A))))).

% :-use_module(library(uid)).
% only if root



%% deregister_unsafe_preds is det.
%
% Deregister Unsafe Predicates.
%

deregister_unsafe_preds:- current_predicate(kill_unsafe_preds/0),!.
deregister_unsafe_preds:-!.
deregister_unsafe_preds:- if_defined(getuid(0),true),forall(unsafe_preds_egg(M,F,A),whenever(run_network,remove_pred_egg(M,F,A))).
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

%:- ain(mtExact(lmcache)).
%:- ain(mtExact(lmconf)).



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
       format(OutStream,'~w\n',[CtrlNick]),flush_output(OutStream),
       format(OutStream,'~w\n',[Pass]),flush_output(OutStream),
       retractall(egg:stdio(CtrlNick,_,_)),
       asserta((egg:stdio(CtrlNick,IN,OutStream))),!.

:- reg_egg_builtin(consultation_thread/2).



%% consultation_thread( ?CtrlNick, ?Port) is det.
%
% Consultation Thread.
%
consultation_thread(CtrlNick,Port):-
      eggdropConnect(CtrlNick,Port),
      put_egg('.echo off\n'),
      put_egg('.console ~w ""\n',[CtrlNick]),
      must(lmconf:irc_bot_nick(PrologMUDNick)),
      put_egg('.set nick ~w\n',[PrologMUDNick]),
      must(egg:stdio(CtrlNick,IN,_)),!,
      % loop
      put_egg('.msg nickserv identify ~w\n',[CtrlNick]),
      put_egg('.-chan ##AGI\n',[]),
      put_egg('.console #logicmoo\n',[]),
      join_chans,
      say_owner("hi therre!"),
      repeat,
        % nop(quietly(update_changed_files_eggdrop)),
         once(read_line_to_codes(IN,Text)),
         Text\==end_of_file,
         once(quietly(consultation_codes(CtrlNick,Port,Text))),
         fail.

join_chans:- maplist(join,['##prolog','#ai','##narrative-ai','##logic','#logicmoo']).

:- dynamic(tmp:last_say_owner/1).
say_owner(Info):- get_time(Date), ignore(if_catch_fail(say_owner(Info,Date))).
% throttle to once every 10 seconds
say_owner(_Info,Date):- tmp:last_say_owner(Was), Ten is Was+10, Ten > Date, !.
say_owner(Info, Date):-
  ignore_catch(((
      retractall(tmp:last_say_owner(_)),
      asserta(tmp:last_say_owner(Date)),
      format_time(atom(DateAtom),'%a, %d %b %Y %T GMT', Date, posix),
      say(dmiles,say_owner(DateAtom,Info))))).


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
%module_call(M,CMD):- CALL=M:call(CMD), '@'(catch(CALL,E,(my_wdmsg(caught(E,CALL)),throw(E))),M).
%:-meta_predicate(user_call(0)).
%user_call(M:CMD):-!,show_call(module_call(M,CMD)).
%user_call(CMD):-module_call('user',CMD).




%% consultation_codes( ?CtrlNick, ?Port, ?Codes) is det.
%
% Consultation Codes.
%
consultation_codes(CtrlNick,Port,end_of_file):-!,consultation_thread(CtrlNick,Port).
consultation_codes(_BotNick,_Port,Codes):-
      text_to_string(Codes,String),
      catch(read_term_from_atom(String,CALL,[double_quotes(string),module(eggdrop)]),_,(my_wdmsg(String),!,fail)),!,
      is_callable_egg(CALL),
      my_wdmsg((CALL)),!,
      ignore(if_catch_fail(CALL)),!.


% IRC EVENTS Bubble from here
:- export(get2react/1).


%% get2react( ?ARG1) is det.
%
% Get2react.
%
get2react([L|IST1]):- CALL =.. [L|IST1],functor(CALL,F,A),
  show_failure((current_predicate(F/A))),!,show_failure((CALL)).

get2react([flud,NICK,UHOST,HAND,TYPE,CHANNEL]):- !,
  get2react([fludr,NICK,UHOST,HAND,CHANNEL,TYPE]).

get2react([L,NICK,HOSTMASK,UHANDLE,CHANNEL|IST1]):- INFO=..[L|IST1],
  show_failure(irc_receive(NICK, HOSTMASK,UHANDLE,CHANNEL, INFO)).

:- thread_local(t_l: session_id/1).
:- thread_local(t_l: session_prefix/1).

% ===================================================================
% IRC interaction
% ===================================================================
:- thread_local t_l:default_channel/1, t_l:default_user/1, t_l:current_irc_receive/5.
% IRC EVENTS FROM CALL

/*

#NEED (stackable)
proc react-need {channel which} {
    if {$which=="op"} { return 1}
    get2react "need" "[escapeString $channel],[escapeString $which]"
}
proc react-splt {nick uhost hand channel} { get2react "split" "[escapeString $nick],[escapeString $uhost],[escapeString $hand],[escapeString $channel]"  }
proc react-rejn {nick uhost hand channel} {  get2react "rejoin" "[escapeString $nick],[escapeString $uhost],[escapeString $hand],[escapeString $channel]" }
proc react-kick {nick uhost hand channel target reason} { get2react "kick" "[escapeString $nick],[escapeString $uhost],[escapeString $hand],[escapeString $channel],[escapeString $target],[escapeString $reason]"}
proc react-nick {nick uhost hand channel new_nick} { get2react "nick" "[escapeString $nick],[escapeString $uhost],[escapeString $hand],[escapeString $channel],[escapeString $new_nick]"}
proc react-mode {nick uhost hand channel mode_change victim} {get2react "mode" "[escapeString $nick],[escapeString $uhost],[escapeString $hand],[escapeString $channel],[escapeString $mode_change],[escapeString $victim]" }
proc react-topc {nick uhost hand channel topic} {  get2react "topc" "[escapeString $nick],[escapeString $uhost],[escapeString $hand],[escapeString $channel],[escapeString $topic]" }
proc react-flud {nick uhost hand type chan} { get2react "flud" "[escapeString $nick],[escapeString $uhost],[escapeString $hand],[escapeString $type],[escapeString $chan]" return 1 }
proc react-ctcr {nick uhost hand dest key arg} { get2react "ctcr" "[escapeString $nick],[escapeString $uhost],[escapeString $hand],[escapeString $dest],[escapeString $key],[escapeString $arg]" return 0 }
proc react-raw {from keyword arg} { get2react "raw" "[escapeString $from],[escapeString $keyword],[escapeString $arg]" return 0 }
proc react-bot {from keyword arg} { get2react "bot" "[escapeString $from],[escapeString $keyword],[escapeString $arg]" return 0 }
proc react-fil {hand idx txt} { get2react "fil" "[escapeString $hand],[escapeString $idx],[escapeString $txt]" return 0 }
proc react-wall {hand msg} { get2react "wall" "[escapeString $hand],[escapeString $msg]" return 0 }
proc react-chof {hand idx} { get2react "chof" "[escapeString $hand],[escapeString $idx]" return 0 }
*/

chon(W,N):- nop(chon(_,W,N)).
choff(W,N):- nop(chof(_,W,N)).

%% part( ?USER, ?HOSTMASK, ?UHANDLE, ?DEST, ?MESSAGE) is det.
%
% Part.
%
part(USER, HOSTMASK,UHANDLE,DEST,MESSAGE):- irc_receive(USER, HOSTMASK,UHANDLE,DEST,part(USER, HOSTMASK,UHANDLE,DEST,MESSAGE)).



evnt(Str):- call_no_cuts(irc_hooks:on_irc_connect(Str)).


%% join( ?USER, ?HOSTMASK, ?UHANDLE, ?DEST) is det.
%
% Join.
%
join(USER, HOSTMASK,UHANDLE,DEST):- irc_receive(USER, HOSTMASK,UHANDLE,DEST,join(USER, HOSTMASK,UHANDLE,DEST)).


%% msgm( ?USER, ?HOSTMASK, ?UHANDLE, ?DEST, ?MESSAGE) is det.
%
% Msgm.
%
msgm(USER, HOSTMASK,UHANDLE,DEST,MESSAGE):- irc_receive(USER, HOSTMASK,UHANDLE,DEST,say(MESSAGE)).

%% ctcp( ?USER, ?HOSTMASK, ?UHANDLE, ?DEST, ?TYPE, ?MESSAGE) is det.
%
% Ctcp.
%
ctcp(USER, HOSTMASK,UHANDLE,DEST,TYPE,MESSAGE):- irc_receive(USER, HOSTMASK,UHANDLE,DEST,ctcp(TYPE,MESSAGE)).

%% notc( ?USER, ?HOSTMASK, ?HANDLE, ?MESSAGE, ?DEST) is det.
%
%  "NickServ","NickServ@services.","*","You are already logged in as \002\PrologMUD\002\.","PrologMUD"
% Ctcp.
%
notc(USER, HOSTMASK, UHANDLE, MESSAGE, DEST):- TYPE = "NOTICE", irc_receive(USER,HOSTMASK,UHANDLE,DEST,ctcp(TYPE,MESSAGE)).

%% pubm( ?USER, ?HOSTMASK, ?UHANDLE, ?DEST, ?MESSAGE) is det.
%
% Pubm.
%
pubm(USER, HOSTMASK,UHANDLE,DEST,MESSAGE):- irc_receive(USER, HOSTMASK,UHANDLE,DEST,say(MESSAGE)).




%% irc_receive( ?USER, ?HOSTMASK, ?UHANDLE, ?DEST, ?MESSAGE) is det.
%
% Irc Receive.
%
irc_receive(USER,HOSTMASK,UHANDLE,DEST,MESSAGE):-
 my_wdmsg(irc_receive(USER,HOSTMASK,UHANDLE,DEST,MESSAGE)),!,
   string_to_atom(USER,ID),
   string_to_atom(DEST,DESTID),
   (call_in_thread_ed(DESTID,
    (
     locally([
       t_l:put_server_count(0),
       t_l:default_channel(DEST),
       t_l:default_user(USER),
       t_l:session_id(ID),
       t_l:current_irc_receive(USER, HOSTMASK,UHANDLE,DEST,MESSAGE)],
        with_rl((eggdrop_bind_user_streams, quietly(ignore(once(ircEvent(DEST,USER,MESSAGE)))))))))),!.




%% with_rl( :Call) is det.
%
% Using Resource Limit.
%

 % :- use_module(library(resource_bounds)).
 %with_rl(Call):- thread_self(main),!,rtrace((guitracer,trace,Call)).
% UNDO with_rl(Call):- egg_trace,!,call(Call).
% UNDO with_rl(Call):- thread_self(main),!,call(Call).
with_rl(Call):- call(Call).
% UNDO with_rl(Call):- !,nodebugx(Call).
% with_rl(Goal):- show_call(eggdrop,nodebugx(resource_bounded_call(Goal, 1000.0, _Status, []))).
% UNDO with_rl(Call):- nodebugx(call_with_time_limit(30,Call)).

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
 lmconf:irc_bot_nick(BotNick),ctrl_nick(CtrlNick),arg(_,vv(BotNick,CtrlNick),Ignore),atom_contains(From,Ignore),!.

:-dynamic(lmconf:chat_isChannelUserAct/4).
:-dynamic(ignored_channel/1).
:-dynamic(irc_hooks:on_irc_msg/3).
:-multifile(irc_hooks:on_irc_msg/3).




%% irc_hooks:on_irc_msg( ?Channel, ?User, ?Stuff) is det.
%
% Hook To [irc_hooks:on_irc_msg/3] For Module Eggdrop.
% Irc Event Hooks.
%

irc_hooks:on_irc_msg(_Channel,_User,_Stuff):-fail.




%% recordlast( ?Channel, ?User, ?What) is det.
%
% Recordlast.
%
recordlast(Channel,User,say(What)):-!,retractall(lmconf:chat_isChannelUserAct(Channel,User,say,_)),asserta(lmconf:chat_isChannelUserAct(Channel,User,say,What)),!.
recordlast(Channel,User,What):-functor(What,F,_),retractall(lmconf:chat_isChannelUserAct(Channel,User,F,_)),asserta(lmconf:chat_isChannelUserAct(Channel,User,F,What)),!.

/*
% tg_name_name_text(StringIn, _Name, _Value) :- atom_concat(" ", _, StringIn),!,fail.
tg_name_name_text(User, String, User, Value) :-
 % \+ t_l:session_prefix(_),
  sub_string(String, Before, _, _, ': '), Before>1,!,
  sub_string(String, 0, Before, _, ChannelTrim),
  \+ atom_contains(ChannelTrim, " "),
  filter_tg_name(ChannelTrim, ChannelPad),
  trim(ChannelPad,ChannelTrimPad),atom_codes(Channel,ChannelTrimPad),
  set_session_prefix(Channel),
  After is Before+2,
  sub_string(String, After,_, 0, Value), !.

tg_name_name_text(_User, String, Name, Value) :-
  tg_name_text( String, Name, Value),!.
*/

% awaiting some inputs

name_value_split(String, Splitter, Name, Value) :-
        sub_string(String, Before, _, After, Splitter), !, Before>0, After>0,
        get_text_restore_pred(String,Restore),
        sub_string(String, 0, Before, _, NameR),
        sub_string(String, _, After, 0, ValueR),
        call(Restore, NameR, Name),
        call(Restore, ValueR, Value),!.


tg_name_text(StringIn, Name, Value) :-
        atom_concat("<", String0, StringIn),
        replace_in_string("> [edited] ", "> ",String0,String),
        name_value_split(String, "> ", NameA, Value),
        filter_tg_name(NameA, Name).

filter_tg_name(StringIn, Name):- arg(_,v("<",">"," ","[m]","[d]"),R),replace_in_string(R,"",StringIn,String),StringIn\==String,!,filter_tg_name(String, Name).
filter_tg_name(NameString, Name):- filter_chars(is_printing_alpha_char,NameString, Name).

filter_chars(How,NameString, Name):- get_text_restore_pred(NameString,DataPred),!,
   any_to_charlist(NameString,Chars),include(How,Chars,NameChars),call(DataPred,NameChars,Name).

is_printing_alpha_char(' '):- !,fail.
is_printing_alpha_char('_'):-!.
is_printing_alpha_char('-'):-!.
is_printing_alpha_char('#'):-!.
is_printing_alpha_char(X):- char_type(X,alpha),!.
is_printing_alpha_char(X):- char_type(X,digit),!.


%% ircEvent( ?DEST, ?User, ?Event) is det.
%
% Irc Event.
%

ircEvent(Channel,_Agent,say(W)):-
   name_value_split(W, '> ', Ctx, Call),
   atom_contains(Ctx,'<'),
   filter_tg_name(Ctx, Name),
   locally(t_l:session_prefix(Name),
     ircEvent(Channel,Name,say(Call))),!.

ircEvent(Channel,User,Method):-  my_wdmsg((ircEventNow(Channel,User,Method))),fail.
ircEvent(Channel,Agent,Event) :- format(codes(Codes),"~w",[ircEvent(Channel,Agent,Event)]),
   member(E,Codes),integer(E),
   E > 127, !,
   wdmsg(ircEvent(Channel,Agent,Event)),!.

ircEvent(Channel,Agent,Event):- filter_tg_name(Agent, Name), Agent\=@=Name,!,ircEvent(Channel,Name,Event).
ircEvent(DEST,User,Stuff):- string(User),string_lower(User,DUser),User\=@=DUser,!,ircEvent(DEST,DUser,Stuff).
ircEvent(DEST,User,Stuff):- atom(User),downcase_atom(User,DUser),User\=@=DUser,!,ircEvent(DEST,DUser,Stuff).
ircEvent(DEST,User,say(W)):- \+ string(W), if_catch_fail(text_to_string(W,S)),!,ircEvent(DEST,User,say(S)).


ircEvent(DEST,User,say(W)):- any_to_codelist(W,CodeList),
  include(>(128),CodeList,NewCodeList),CodeList\==NewCodeList,!,any_to_string(NewCodeList,NewW),
  ircEvent(DEST,User,say(NewW)).

%  "<@\003\1Douglas Miles\017\> ?- member(Z,[a])."
% irc_receive("tglm","~IRChuu@c-73-67-179-188.hsd1.wa.comcast.net","*","#logicmoo",say("<@\003\1Douglas Miles\017\> 123")).

/*
ircEvent(DEST,User,say(W)):-
   tg_name_name_text(User, W, Name, Value),
   User\==Name,
   W\==Value,!,
   locally(t_l:default_user(Name),
      ircEvent(DEST,Name,say(Value))).
*/

ircEvent(DEST,_User,ctcp("ACTION",W)):-
   tg_name_text(W, Name, Value),W\==Value,!,
   locally(t_l:default_user(Name),
      ircEvent(DEST,Name,ctcp("ACTION",Value))).

ircEvent(DEST,_User,say(W)):-
   tg_name_text(W, Name, Value),W\==Value,!,
   locally(t_l:default_user(Name),
      ircEvent(DEST,Name,say(Value))).

ircEvent(DEST,User,say(W)):- fail,
 term_to_atom(cu(DEST,User),QUEUE),
   message_queue_property(_, alias(QUEUE)),
     show_call(ircEvent,thread_send_message(QUEUE,W)).

% ignore some inputs
ircEvent(Channel,Agent,_):- (ignored_channel(Channel) ; ignored_source(Agent)) ,!.

ircEvent(Channel,Agent,Event) :-
 %  ignore_catch(ignore(once(doall(call_no_cuts(irc_hooks:on_irc_msg(Channel,Agent,Event)))))),!,
   ignore_catch(ignore(ircEventNow(Channel,Agent,Event))),!,
   ignore_catch(recordlast(Channel,Agent,Event)),!.

ircEventNow(Channel,Agent,ctcp(TYPE,MSG)):- /*TYPE=="NOTICE",*/
   if_catch_fail(say_owner(ircEvent(Channel,Agent,ctcp(TYPE,MSG)))), fail.
/*
ircEventNow(Channel,User,Method):-
 ((if_catch_fail(ircEventUsed(Channel,User,Method)),!)
  -> true  ; my_wdmsg(unused(ircEventNow(Channel,User,Method)))).
*/
ircEventNow(Channel,User,Method):-  if_catch_fail(ircEventUsed(Channel,User,Method)),!,my_wdmsg(ircEventUsed(Channel,User,Method)).
%ircEventNow(Channel,User,Method):-  Method = say(_),  rtrace(if_catch_fail(ircEventUsed(Channel,User,Method))),!.
ircEventNow(Channel,User,Method):-  my_wdmsg(unused(ircEventNow(Channel,User,Method))).

is_reg_with_nonignore(Channel,Agent):- \+ get_isRegistered(Channel,Agent, ignored), get_isRegistered(Channel,Agent, _Type).

starts_white(W):- text_to_string_safe(W,Str), atom_codes(Str,[WS|_]), char_type(WS,white).

dont_allow_whitespace_if_unregisterd(Channel,Agent, W):- starts_white(W),!, is_reg_with_nonignore(Channel,Agent).
%dont_allow_whitespace_if_unregisterd(Channel,Agent, _):- is_reg_with_nonignore(Channel,Agent),!.
dont_allow_whitespace_if_unregisterd(_Channel,_Agent, _):- !.


% Say -> Call with_dmsg_to_main
ircEventUsed(Channel,Agent,say(W)):- ircEventUsed_as_call(Channel,Agent,say(W)),!.
ircEventUsed(Channel,Agent,say(W)):- notrace(maybe_chat_command(Channel,Agent,say,W)).

ircEventUsed(Channel,Agent,ctcp(ACTION,W)):- notrace(maybe_chat_command(Channel,Agent,ctcp(ACTION),W)).

% Call -> call_for_results
ircEventUsed(Channel,Agent,call(CALL,Vs)):- irc_filtered_call(Channel,Agent,CALL,Vs).

ircEventUsed_as_call(Channel,Agent,say(W)):-
   name_value_split(W, ': ', Ctx, Call),
   \+ atom_contains(Ctx,' '),
   locally(t_l:session_prefix(Ctx),
     ircEventUsed_as_call(Channel,Agent,say(Call))),!.

ircEventUsed_as_call(Channel,Agent,say(W)):-
  dont_allow_whitespace_if_unregisterd(Channel, Agent, W),
  DidAny = did(false),
  source_and_module_for_agent(Agent,SourceModule,_CallModule),
  forall(
    read_egg_term(SourceModule,W,CALL,Vs), % read_each_term_egg(W,CALL,Vs)
     (irc_filtered_call(Channel,Agent,CALL,Vs),nb_setarg(1,DidAny,true))),
  ((DidAny = did(true)) -> ! ; fail).


ci_concat_text(Left,Right,All):- var(Right),!, string_lower(All,LAll),string_lower(Left,LLeft),
   sub_string(LAll, 0, _Len, After, LLeft),sub_string(All, _, After, 0, Right).

is_irc_cmd_ok( Channel, Agent,_Cmd):- get_isRegistered(Channel,Agent, Type), Type==ignored, !, fail.
is_irc_cmd_ok(_Channel,_Agent, Cmd):- atom_concat(irc_invoke_,Cmd,CmdOut), \+ current_predicate(irc_cmd:CmdOut/_), !, fail.
is_irc_cmd_ok( Channel, Agent, Cmd):- get_isRegistered(Channel,Agent, Type), Type == Cmd.
%is_irc_cmd_ok( Channel, Agent,_Cmd):- get_isRegistered(Channel,Agent, execute),!.
is_irc_cmd_ok(_Channel,_Agent,_Cmd).

trim_text(Text,L,_,TextO):- sub_term(T,L),L\=[],atomic(T),string_concat(T,MText,Text),!,trim_text(MText,TextO).
trim_text(Text,_,R,TextO):- sub_term(T,R),R\=[],atomic(T),string_concat(MText,T,Text),!,trim_text(MText,TextO).
trim_text(Text,_,_, Text):-!.

trim_text(Text,TextO):- trim_text(Text,ltrim(","," ",":"),rtrim(","," ",":"),TextO).

:-multifile(irc_cmd:irc_invoke_fallback/4).
:-dynamic(irc_cmd:irc_invoke_fallback/4).

irc_cmd:irc_invoke_bot(Channel,Agent, Modality,W1):- quietly(irc_cmd_invoke_bot(Channel,Agent, Modality,W1)).

% attention (notice the fail to disable)
irc_cmd_invoke_bot(Channel,Agent, Modality,W1):- member(Sep,[" ",".",","]), string_concat(W2,Sep,W1),!,irc_cmd_invoke_bot(Channel,Agent, Modality,W2).
irc_cmd_invoke_bot(Channel,Agent, Modality,W1):- string_lower(W1,W2),W1\==W2,!,irc_cmd_invoke_bot(Channel,Agent, Modality,W2).
irc_cmd_invoke_bot(Channel,Agent,_Modality,""):-
   show_call_backs(Channel,Agent).
irc_cmd_invoke_bot(Channel,Agent,_Modality,"goodbye"):-
   retractall(lmconf:chat_isWith(_Type,_OldChannel,Agent)),!,
   say(Channel,s([goodbye,Agent,'I will not answer you in',Channel])).
irc_cmd_invoke_bot(Channel,Agent, Modality,W1):- string_concat("+",W2,W1),!,do_and_show_fallbacks(+,Channel,Agent, Modality,W2).
irc_cmd_invoke_bot(Channel,Agent, Modality,W1):- string_concat("-",W2,W1),!,do_and_show_fallbacks(-,Channel,Agent, Modality,W2).
irc_cmd_invoke_bot(Channel,Agent, Modality,W2):- maybe_chat_command(Channel,Agent, Modality,W2),!.
irc_cmd_invoke_bot(Channel,Agent, Modality,W2):- maybe_chat_command(Channel,Agent, Modality,W2," "),!.

into_what(W2, _):- \+ atom(W2),!,fail.
into_what('_',Type):- Type=_.
into_what('all',Type):- Type=_.
into_what(W2,Type):- Type=W2.

egg_subst_string(In,Before,After,Out):- atomics_to_string(List,Before,In),atomics_to_string(List,After,Out).

do_and_show_fallbacks(PM,Channel,Agent, Modality,W2):-
  do_fallbacks(PM,Channel,Agent, Modality,W2),
  show_call_backs(Channel,Agent).

show_call_backs(Channel,Agent):-
  findall(Type,(get_isRegistered(Channel2,Agent2, Type),Agent2==Agent,Channel2==Channel),List),
  ( List==[]-> say(Channel,s([hi,Agent,'no callbacks in',Channel, 'are set'])) ;
    List=[all]-> say(Channel,s([Agent,'all callbacks in',Channel,'until you say "goodbye"']));
    % List=[One]-> say(Channel,s([Agent,'one callback in',Channel,': "', One, '" which is your default'])) ;
    say(Channel,s([Agent,'callbacks in',Channel,': ', List]))).


:- baseKB:import(eggdrop:say/2).

do_fallbacks(_,_Channel,_Agent,_Modality,''):-!.
do_fallbacks(+,Channel,Agent,_Modality,W2):- into_what(W2,Type), ignore((retract(lmconf:chat_isWith(Type,Channel,Agent)),fail)),
                                                 asserta(lmconf:chat_isWith(W2,Channel,Agent)).
do_fallbacks(-,Channel,Agent,_Modality,W2):- into_what(W2,Type), ignore((retract(lmconf:chat_isWith(Type,Channel,Agent)),fail)),
                                                 !.
do_fallbacks(PM,Channel,Agent, Modality,W2):- egg_subst_string(W2,","," ",W3),atomic_list_concat(ListR," ",W3),
                                        reverse(ListR,List),maplist(do_fallbacks(PM,Channel,Agent, Modality),List).

maybe_chat_command(Channel,Agent, Modality,W0):- trim_text(W0,W1),W1\==W0, !, maybe_chat_command(Channel,Agent, Modality,W1).
maybe_chat_command(Channel,Agent, Modality,W1):- bot_nick(BotNick), ci_concat_text(BotNick,Right,W1),trim_text(Right,W2),
 do_irc_cmd_now(Channel,Agent, Modality,bot,W2).
maybe_chat_command(Channel,Agent, Modality,W1):- string_concat('.',W,W1), maybe_chat_command(Channel,Agent, Modality,W," "),!.
maybe_chat_command(Channel,Agent, Modality,W1):- maybe_chat_command(Channel,Agent, Modality,W1,":"),!.
maybe_chat_command(Channel,Agent,_Modality,W1):- atom_contains(W1,"goodbye"), forall(retract(lmconf:chat_isWith(_Any_,Channel,Agent)),true),!.
maybe_chat_command(Channel,Agent, Modality,W1):- do_irc_cmd_now(Channel,Agent, Modality,fallback,W1),!.

maybe_chat_command(Channel,Agent, Modality,W, Split):-
     sub_string(W, Before, 1, After, Split),
     sub_atom(W, 0, Before, _, TxtBefore),
     filter_chars(is_printing_alpha_char, TxtBefore, Cmd0), TxtBefore==Cmd0,
     downcase_atom(Cmd0, Cmd),
     is_irc_cmd_ok(Channel,Agent,Cmd),
     sub_string(W, _, After, 0, TxtAfter),
     trim_text(TxtAfter,Text),
     do_irc_cmd_now(Channel,Agent, Modality,Cmd,Text).

do_irc_cmd_now(Channel,Agent, Modality,Cmd,W1):- downcase_atom(Cmd,DC), DC\==Cmd,!, do_irc_cmd_now(Channel,Agent, Modality,DC,W1).
do_irc_cmd_now(Channel,Agent, Modality,Cmd,W1):-
  trim_text(W1,Args),
  atom_concat(irc_invoke_,Cmd,CmdOut),
  P=..[CmdOut,Channel,Agent, Modality,Args],
  predicate_property(irc_cmd:P, defined),!,
  irc_process(Channel,Agent,call(irc_cmd:P)).

irc_process(Channel,Agent,G):-
  with_error_channel(Agent,
   egg_dmsg_to_main(( (thread_self(Self), (Self\==main -> nop(tnodebug(Self)) ; true)),
     with_output_channel(Channel,
       with_no_input(G))))).





:- dynamic(lmconf:chat_isModule/3).

:- module_transparent(use_agent_module/1).
:- module_transparent(save_agent_module/1).

use_agent_module(AgentS):-
   source_and_module_for_agent(AgentS,SourceModule,CallModule),!,
   '$set_source_module'(SourceModule),
   '$set_typein_module'(CallModule).

save_agent_module(AgentS):- agent_to_letters_atom(AgentS,Agent),
   retractall(lmconf:chat_isModule(Agent,_,_)),
   once('$current_source_module'(SourceModule);'$set_source_module'(SourceModule,SourceModule)),
   once('$current_typein_module'(CallModule);'$module'(CallModule,CallModule)),
   asserta(lmconf:chat_isModule(Agent,SourceModule,CallModule)).

agent_to_letters_atom(AgentS,DCAtom):-
  any_to_atom(AgentS,Agent),
  filter_chars(is_letter_atom,Agent,Atom),
  downcase_atom(Atom,DCAtom).

is_letter_atom(C):- char_type(C,alpha).

source_and_module_for_agent(AgentS,SourceModule,CallModule):-
    agent_to_letters_atom(AgentS,Agent),
    lmconf:chat_isModule(Agent,SourceModule,CallModule),!.
source_and_module_for_agent(AgentS,SourceModule,CallModule):-
    agent_to_letters_atom(AgentS,Agent),
    create_agent_module(Agent,SourceModule),
    create_agent_module(Agent,CallModule),
    asserta(lmconf:chat_isModule(Agent,SourceModule,CallModule)).

create_agent_module(AgentS,AgentModule):-
   any_to_atom(AgentS,Agent),
   (var(AgentModule) -> AgentModule = Agent ; true),
    % too many people wish to define themselves


   (AgentModule \==Agent -> add_import_module(AgentModule,Agent,start) ; true),
   promiscuous_module(AgentModule).



:- create_agent_module(dmiles,_).


:- reg_egg_builtin(unreadable/1).



%% unreadable( ?UR) is det.
%
% Unreadable.
%
unreadable(UR):-my_wdmsg(unreadable(UR)).

:- reg_egg_builtin(eggdrop_bind_user_streams/0).




%% eggdrop_bind_user_streams is det.
%
% Eggdrop Bind User Streams.
%

% eggdrop_bind_user_streams :- !.
eggdrop_bind_user_streams :- egg_trace,!.
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
:- meta_predicate call_in_thread_ed(+, 0).
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



:- reg_egg_builtin(add_maybe_static/2).



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
:-module_transparent(irc_filtered_call/4).
:- reg_egg_builtin(irc_filtered_call/4).



%% irc_filtered_call( ?Channel, ?Agent, ?CALL, ?Vs) is det.
%
% Irc Event Call Filtered.
%
irc_filtered_call(_Channel,_Agent,End_of_file,_Vs):- notrace(End_of_file == end_of_file),!,fail.
irc_filtered_call(Channel,Agent,_CALL,_Vs):- get_isRegistered(Channel,Agent, Type), Type==ignored, !, fail.
irc_filtered_call(_Channel,_Agent,CALL,_Vs):- var(CALL),!,fail.
irc_filtered_call(_Channel,_Agent,(H :- B ),Vs):-
  add_maybe_static((H :- B),Vs),!.
irc_filtered_call(Channel,Agent,((=>(H)) :- B ),Vs):-
  ((=>(H :- B)) \== ((=>(H)) :- B )),!,
  irc_filtered_call(Channel,Agent,(=>(H :- B)),Vs).

irc_filtered_call(Channel,Agent,'?-'(CALL),Vs):- !, nonvar(CALL),
  if_catch_fail(irc_really_call(Channel,Agent,must(call_for_results(CALL,Vs)), Vs)).

irc_filtered_call(Channel,Agent,lispy(Lispy),Vs):- get_isRegistered(Channel,Agent, execute),
  irc_really_call(Channel,Agent,lisp_compiled_eval([print,Lispy],_R),Vs),!.

irc_filtered_call(Channel,Agent,AIN,Vs):- is_ained(AIN),!,irc_really_call(Channel,Agent,ain_expanded(AIN),Vs),!.
/*
irc_filtered_call(Channel,Agent,lispy(Lispy),Vs):- irc_really_call(Channel,Agent,lisp_compiled_eval([print,Lispy],R),['LispRes'=R|Vs]),!.
irc_filtered_call(Channel,Agent,[S|TERM],Vs):- is_list([S|TERM]),is_lisp_call_functor(S),!,
   (current_predicate(lisp_call/3) -> irc_really_call(Channel,Agent,lisp_call([S|TERM],Vs,R),['Result'=R|Vs]);
     my_wdmsg(cant_ircEvent_call_filtered(Channel,Agent,[S|TERM],Vs))).
*/
% irc_filtered_call(Channel,Agent,CALL,Vs):- get_isRegistered(Channel,Agent,executeAll),!,irc_really_call(Channel,Agent,CALL,Vs),!.
irc_filtered_call(Channel,Agent,CALL,Vs):- my_wdmsg(unused_ircEvent_call_filtered(Channel,Agent,CALL,Vs)),!,fail.

is_ained(AIN):- \+ compound(AIN),!,fail.
is_ained(==>(_)).
is_ained(==>(_,_)).
is_ained(<==>(_,_)).
is_ained(=>(_)).
is_ained(=>(_,_)).
is_ained(<=>(_,_)).
%is_ained(AIN):- is_ain_clause(AIN).
is_ained(M:A):-atom(M),!,is_ained(A).


is_ained((_ :- _)).
is_ained((_ --> _)).

is_ained2(user:_).
is_ained2(exists(_,_)).
is_ained2(all(_,_)).
is_ained2(q(_,_,_)).



%% is_lisp_call_functor( ?FUNCTOR) is det.
%
% If Is A Lisp Call Functor.
%
is_lisp_call_functor('?-').
is_lisp_call_functor('?>').


:-module_transparent(irc_really_call/4).
:- reg_egg_builtin(irc_really_call/4).

%% irc_really_call( ?Channel, ?Agent, ?CALL, ?Vs) is det.
%
% Irc Event Call.
%


irc_really_call(Channel,Agent,CALL, Vs):-
   source_and_module_for_agent(Agent, SourceModule, CallModule),
   (SourceModule == CallModule -> AgentModule = CallModule ; AgentModule = (SourceModule:CallModule)),
   my_wdmsg(irc_really_call(Channel,Agent,AgentModule:CALL,Vs)),
  % debug(_), % gtrace,
   irc_process(Channel,Agent,
     with_error_channel(Channel,
       setup_call_cleanup(
       (b_setval('$variable_names',Vs), b_setval('$term',CALL),
                    use_agent_module(Agent)),
             (nop((stream_property(X,alias(current_output)),set_stream(X,alias(user_output)))),
                catch(emlmp(AgentModule:CALL),E,
                 (((my_wdmsg(E),say(Agent,[Channel,': ',E])),!,fail)))),
                    save_agent_module(Agent)))),
   !.


emlmp(Goal):- current_predicate(maybe_long_message_printer/2),!,maybe_long_message_printer(4, Goal).
emlmp(Goal):- call(Goal).

%% cit is det.
%
% Cit.
%
cit:- get_time(HH), call_in_thread_ed(dmiles,with_error_channel(dmiles,writeln(current_error,HH))).



%% cit2 is det.
%
% Cit Extended Helper.
%
cit2:- get_time(HH), rtrace(with_error_channel(dmiles,writeln(current_error,HH))).



%% cit3 is det.
%
% Cit3.
%
cit3:- get_time(HH), writeln(current_error,HH).






%% call_in_thread_ed(+ID, :Call) is det.
%
% Call In Thread.
%

call_in_thread_ed(_ ,CMD):- egg_trace,!,call(CMD).
% call_in_thread_ed(ID,CMD):- !,in_threaded_engine(ID,CMD).
call_in_thread_ed(_ ,CMD):- !,call(CMD).
% above cut all
call_in_thread_ed(_ ,CMD):- thread_create(CMD,_,[detached(true)]).
call_in_thread_ed(_,CMD):- thread_self(Self),thread_create(CMD,_,[detached(true),inherit_from(Self)]).

%in_threaded_engine(ID,CMD):- logtalk:threaded_engine_create(CMD,CMD,ID),threaded_engine_next(ID,CMD).

:- dynamic(lmcache:vars_as/1).
% :- thread_local lmcache:vars_as/1.

in_threaded_engine_loop :-
	logtalk:threaded_engine_fetch(Task),
	call(Task),
        logtalk:threaded_engine_yield(result(Task)),
	in_threaded_engine_loop.

ensure_threaded_engine_loop:- \+ current_predicate(current_logtalk_flag/2),!.
ensure_threaded_engine_loop:- logtalk:threaded_engine(looped_threaded_engine_worker),!.
ensure_threaded_engine_loop:- logtalk:threaded_engine_create(none, in_threaded_engine_loop, looped_threaded_engine_worker).

:- multifile(logtalk:'$lgt_engine_term_queue_'/2).
:- dynamic(logtalk:'$lgt_engine_term_queue_'/2).
:- volatile(logtalk:'$lgt_engine_term_queue_'/2).

:- multifile(logtalk:'$lgt_current_engine_'/4).
:- dynamic(logtalk:'$lgt_current_engine_'/4).
:- volatile(logtalk:'$lgt_current_engine_'/4).


:- during_boot(ensure_threaded_engine_loop).

%% vars_as( ?VarType) is det.
%
% Hook To [lmcache:vars_as/1] For Module Eggdrop.
% Variables Converted To.
%
lmcache:vars_as(comma).

:- reg_egg_builtin(flush_all_output/0).



%% flush_all_output is det.
%
% Flush All Output.
%
flush_all_output:- flush_output_safe,flush_output_safe(user_error),flush_output_safe(current_error),!.
flush_all_output:- flush_output(current_error),flush_output.

:- reg_egg_builtin(vars_as_list/0).



%% vars_as_list is det.
%
% Variables Converted To List.
%
vars_as_list :- retractall(lmcache:vars_as(_)),asserta(lmcache:vars_as(list)).
:- reg_egg_builtin(vars_as_comma/0).



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

:- reg_egg_builtin(write_varvalues2/1).



%% write_varvalues2( ?Vs) is det.
%
% Write Varvalues Extended Helper.
%
% write_varvalues2(Vs):-lmcache:vars_as(comma),!,write_varcommas2(Vs),write_residuals(Vs).

write_varvalues2([]):-!,flush_all_output.
write_varvalues2(Vs):-
   flush_all_output,
   write(' % '),
   write_varvalues3(Vs),
   copy_term(Vs,Vs,Goals),
   write_goals(Goals),!,
   flush_all_output.

writeqln(G):-writeq(G),write(' '),nl.

write_goals([]):-!.
write_goals(List):- write('\n%    Residual Goals: '),write_goals0(List),!,write(' ').
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



:- reg_egg_builtin(remove_anons/2).



%% remove_anons( ?ARG1, ?VsRA) is det.
%
% Remove Anons.
%
remove_anons([],[]).
remove_anons([N=_|Vs],VsRA):-atom_concat('_',_,N),!,remove_anons(Vs,VsRA).
remove_anons([N=V|Vs],[N=V|VsRA]):-remove_anons(Vs,VsRA).

:-module_transparent(call_for_results/2).
:- reg_egg_builtin(call_for_results/2).



%% call_for_results( ?CMDI, ?Vs) is det.
%
% Call Using Results.
%

call_for_results(Query,Bindings):-
     notrace('$toplevel':call_expand_query(Query, ExpandedQuery, Bindings, ExpandedBindings)), !,
     call_for_results_0(ExpandedQuery,ExpandedBindings).


call_for_results_0(CMDI,Vs):-
 % BORKED b_setval('$term', (vars(Vs):-CMD)), /* DRM: added for expansion hooks*/
 ((
   notrace((
   remove_anons(Vs,VsRA),!,
   locally_tl(disable_px,      expand_goal(CMDI,CMDM)),
   locally_tl(disable_px,(user:expand_goal(CMDM, CMD))),
  (CMD==CMDI->true;my_wdmsg(expand_goal_call_for_results(CMDI->CMD))))),
  forall(show_call(call_for_results_1(CMD,VsRA)),true))),!.

:-module_transparent(call_for_results_1/2).
:- reg_egg_builtin(call_for_results_1/2).


%% call_for_results_1( :GoalCMD, ?Vs) is det.
%
% call Using results  Primary Helper.
%
call_for_results_1(CMD,Vs):-
 set_varname_list( Vs),
 % b_setval('$variable_names',Vs),
 flag(num_sols,_,0),
 (call_for_results_2(CMD,Vs) *->
  (deterministic(X),flag(num_sols,N,0),(N\==0->YN='Yes';YN='No'), write(' '),(X=true->write(det(YN,N));write(nondet(YN,N)))) ;

     (deterministic(X),flag(num_sols,N,0),(N\==0->YN='Yes';YN='No'),write(' '),(X=true->write(det(YN,N));write(nondet(YN,N))))),
 flush_all_output.




:-module_transparent(call_for_results_2/2).
:- reg_egg_builtin(call_for_results_2/2).



%% call_for_results_2( :GoalCMDIN, ?Vs) is det.
%
% call Using results  Extended Helper.
%
call_for_results_2(CMDIN0,Vs):-
  strip_module(CMDIN0,M,CMDIN),
   CMDIN = CMD,functor_h(CMD,F,A),A2 is A+1,CMD=..[F|ARGS],atom_concat(F,'_with_vars',FF),
   (M:current_predicate(FF/A2)-> (CMDWV=..[FF|ARGS],append_term(CMDWV,Vs,CCMD)); CCMD=CMD),!,
   call_for_results_3(M:CCMD,Vs).
call_for_results_2(CCMD,Vs):- call_for_results_3(CCMD,Vs).

:-module_transparent(call_for_results_3/2).
:- reg_egg_builtin(call_for_results_3/2).


%% call_for_results_3( :GoalCCMD, ?Vs) is det.
%
% Call Using Results Helper Number 3..
%
call_for_results_3(CCMD,Vs):-
   user:show_call(eggdrop,(CCMD,flush_output)), flag(num_sols,N,N+1), deterministic(Done),
     once(once((Done==true -> (once(\+ \+ write_varvalues2(Vs)),write('% ')) ; (once(\+ \+ write_varvalues2(Vs)),N>28)))).




%% with_output_channel( +Channel, :GoalCMD) is det.
%
% Using Output Channel.
%

:- reg_egg_builtin(with_output_channel/2).
:-meta_predicate(with_output_channel(+,0)).
:-module_transparent(with_output_channel/2).
% with_output_channel(_Channel,CMD):- egg_trace,!,call(CMD).
with_output_channel(Channel,CMD):-
  with_output_to_predicate(say(Channel),CMD).




%% with_input_channel_user( +Channel, +User, :GoalCMD) is det.
%
% Using Input Channel User.
%
with_input_channel_user(_,_,CMD):- egg_trace,!,call(CMD).
with_input_channel_user(_,_,CMD):- !, with_no_input(CMD).
with_input_channel_user(Channel,User,CMD):-
  with_input_from_predicate(last_read_from(Channel,User),CMD).

:- reg_egg_builtin(with_io/1).
:-meta_predicate(with_io(0)).



%% with_io( :GoalCMD) is det.
%
% Using Input/output.
%

% with_io(CMD):- \+ is_in_egg,!,call(CMD).
with_io(CMD):-
 egg_dmsg_to_main((
  current_input(IN),current_output(OUT),get_thread_current_error(Err),
  setup_call_cleanup(set_prolog_IO(IN,OUT,Err),CMD,(set_input(IN),set_output(OUT),set_current_error(Err))))).




%% with_no_input( :GoalCMD) is det.
%
% Using No Input.
%
%with_no_input(CMD):-  current_input(Prev), open_chars_stream([e,n,d,'_',o,f,'_',f,i,l,e,'.'],In),set_input(In),!,call_cleanup(CMD,set_input(Prev)).
% with_no_input(CMD):- open_chars_stream([e,n,d,'_',o,f,'_',f,i,l,e,'.'],In),current_output(OUT), set_prolog_IO(In,OUT,current_error ),CMD.
with_no_input(CMD):- ignore(egg_trace), call(CMD).






%% ignore_catch( :GoalCALL) is det.
%
% Ignore Catch.
%
% ignore_catch(CALL):- catch((CALL*->true;rtrace(CALL)),E, my_wdmsg(caught(E,CALL))).
ignore_catch(CALL):- catch(CALL,E, my_wdmsg(caught(ignore_catch,E,CALL))).
if_catch_fail(CALL):- catch(CALL,E,(my_wdmsg(caught(if_catch_fail,E,CALL)),fail)).



:- meta_predicate(with_error_to_output(0)).



%% with_error_to_output( :GoalCMD) is det.
%
% Using Error Converted To Output.
%

% with_error_to_output(CMD):- egg_trace, !, call(CMD).
with_error_to_output(CMD):-
   current_input(IN),current_output(OUT),!,
   with_io((set_prolog_IO(IN,OUT,OUT), CMD)).

:- export(with_error_channel/2).
:- module_transparent(with_error_channel/2).




%% with_error_channel( +Agent, :GoalCMD) is det.
%
% Using Error Channel.
%

% with_error_channel(_Agent, CMD):- \+ is_in_egg,!,call(CMD).
/*
with_error_channel(_Agent, CMD):-  !, CMD.
with_error_channel(Agent,CMD):- fail,
   current_input(IN),current_output(OUT),
   get_main_error_stream(MAINERROR),
   set_prolog_IO(IN,OUT,MAINERROR),
   new_memory_file(MF),
   open_memory_file(MF, write, ERR),
   set_prolog_IO(IN,OUT,ERR),!,
   each_call_cleanup(CMD,(ignore_catch(flush_output(ERR)),ignore_catch(close(ERR)),read_from_agent_and_send(Agent,MF))).
*/
with_error_channel(Agent, CMD):- !,  with_error_to_predicate(say(Agent),CMD).
with_error_channel(_Agent, CMD):-  !, call(CMD).
with_error_channel(_Agent,CMD):- !, with_error_to_output(CMD).






%% read_from_agent_and_send( ?Agent, ?MF) is det.
%
% Read Converted From Agent And Send.
%
read_from_agent_and_send(Agent,MF):- open_memory_file(MF, read, Stream,[ free_on_close(true)]),ignore_catch(read_codes_and_send(Stream,Agent)),ignore_catch(close(Stream)).




%% read_codes_and_send( ?IN, ?Agent) is det.
%
% Read Codes And Send.
%
read_codes_and_send(IN,Agent):- at_end_of_stream(IN),!,my_wdmsg(say(Agent,done)).
read_codes_and_send(IN,Agent):- repeat,read_line_to_string(IN,Codes),say(Agent,Codes),at_end_of_stream(IN),!.

%:-servantProcessCreate(killable,'Consultation Mode Test (KIFBOT!) OPN Server',consultation_thread(swipl,3334),Id,[]).




%% update_changed_files_eggdrop is det.
%
% Update Changed Files Eggdrop.
%

update_changed_files_eggdrop :- egg_trace, !.
update_changed_files_eggdrop :-
 egg_dmsg_to_main(( with_no_dmsg((
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
% IRC PERFORM
% ===================================================================

irc_action(IDEA):- say([':ACTION ',IDEA]).

set_irc_nick(Nick):- text_to_string(Nick,SNick),put_egg('.set botnick ~s\n',[SNick]),
  retractall(lmconf:irc_bot_nick(_)),asserta(lmconf:irc_bot_nick(SNick)).

set_irc_serv(NamePort):- put_egg('.jump ~w\n',[NamePort]).

irc_connect :- egg_go,call_no_cuts(call_no_cuts(irc_hooks:on_irc_connect("The eggdrop is always connected"))).

join(ChannelName):- put_egg('.+chan ~w\n',[ChannelName]).

% ===================================================================
% IRC OUTPUT
% ===================================================================

%% sayq( ?D) is det.
%
% Sayq.
%
:- reg_egg_builtin(sayq/1).
sayq(D):-sformat(S,'~q',[D]),!,say(S),!.


%% say( ?D) is det.
%
% Modality.
%
:- reg_egg_builtin(say/1).
say(D):- t_l:default_channel(C),say(C,D),!.
say(D):- say("#logicmoo",D),!.


is_bot(Bot):- member(BotName,['logicmoo-','PrologM','bot','relay']),atom_contains(Bot,BotName).
not_bot(Bot,A,A):- is_bot(Bot),!.
not_bot(A,Bot,A):- is_bot(Bot),!.

:- export(say/2).

:- reg_egg_builtin(say/2).

%% say( ?OutStream, ?Channel, ?List) is det.
%
% Modality List.
%

%% say( ?Agent, ?Out) is det.
%
% Modality Prefixed.
%

% say(CC,Text):- notrace((stream_property(S,file_no(2)),flush_output(S), format(S,'~N~q~N',[say(CC,Text)]),flush_output(S))),fail.

say(C:CC,Text):-nonvar(C),C=CC,!,say(C,Text).
say(A1:A2,Out):- not_bot(A1,A2,A3),say(A3,Out),!.
say(C:A,Text):- nonvar(A), !, locally(t_l:session_id(A), say(C,Text)).

say( Channel,Data):- (var(Data);cyclic_term(Data)),!,sformat(String,'~p',[Data]),say(Channel,String).
say( Channel,[Channel,': '|Data]):-nonvar(Data),say(Channel,Data),!.
say( Channel,Out):- string(Out),!, any_to_codelist(Out, Codes), privmsg( Channel,Codes).
say(_Channel,[]).
say(_Channel,['']):- !.
say( Channel,[S,'']):-!, say( Channel,[S]).
say( Channel,PlString):- is_charlist(PlString),text_to_string_safe(PlString,String),!,say(Channel,String).
say( Channel,PlString):- is_codelist(PlString),text_to_string_safe(PlString,String),!,say(Channel,String).
say( Channel,[N|L]):- (\+ is_list(L); compound(N) ), !, say( Channel,each([N|L])).
say(_Channel,each(NIL)):- NIL == [],!.
say( Channel,each(OUT)):- var(OUT), !, say( Channel,OUT),!.
say( Channel,each([N|L])):- say( Channel,N), say( Channel,each(L)),!.
say( Channel,List):- is_list(List),if_catch_fail(any_to_string(List,String)),!,say(Channel,String).
say( Channel,s(Data)):- if_catch_fail(any_to_string(s(Data),String)),!,say(Channel,String).
say( Channel,NotString):- if_catch_fail(egg_to_string(NotString,String)),!,say(Channel,String).
say( Channel,Data):- sformat(String,'~p',[Data]),say(Channel,String).
%say(Channel,Data):- \+(is_list(Channel)),text_to_string_safe(Channel, S),string_codes(S,Codes),!,say(Codes,Data),!.
% say(Channel,[S|L]):- \+ is_codelist([S|L]), on_x_fail(atom_string(N,S)),atom_concat('\t',Front,N), atom_concat('   ',Front,NEW),!,say(Channel,[NEW|L]).

%% get_session_prefix( ?ID) is det.
%
% Get Session Prefix.
%

set_session_prefix(ID):- asserta(t_l:session_prefix(ID)),!.

get_session_prefix(ID):-t_l:session_prefix(ID),!.
get_session_prefix(ID):-t_l:session_id(ID),!.
%get_session_prefix(ID):-t_l:default_user(ID),!.
get_session_prefix('').

get_session_prefix_maybe(ID):-t_l:session_prefix(ID),!.
%get_session_prefix_maybe(ID):-t_l:default_user(ID),!.
get_session_prefix_maybe(ID):- get_session_prefix(ID),ID\==''.
%get_session_prefix_maybe(ID):-t_l:default_channel(ID),!.


same_channels(Prefix,Channel):- egg_to_string(Prefix,C1),egg_to_string(Channel,C2),!,C1=C2.


%% is_empty( ?A) is det.
%
% If Is A Empty.
%
is_empty(A):-egg_to_string(A,S),string_length(S,0).


squelch_empty(For,Channel,[]):- squelch_empty(For,Channel,'  ').
squelch_empty(For,Channel,[13]):- squelch_empty(For,Channel,'  ').
squelch_empty(For,Channel,[10]):- squelch_empty(For,Channel,'  ').
squelch_empty(For,Channel,'  '):- nb_current(For,privmsg(Channel,'  ')),!.
squelch_empty(For,Channel,Text):-  nb_setval(For,privmsg(Channel,Text)),fail.


:-thread_local t_l: put_server_count/1.
:-thread_local t_l: put_server_no_max/0.




%% put_server_count( :GoalGOAL1) is det.
%
% Hook To [t_l:put_server_count/1] For Module Eggdrop.
% Put Server Count.
%
t_l:put_server_count(0).




%% check_put_server_count( ?Max) is det.
%
% Check Put Server Count.
%
check_put_server_count(0):- if_defined(t_l:put_server_no_max),retractall(t_l:put_server_count(_)),asserta(t_l:put_server_count(0)).
check_put_server_count(Max):-retract(t_l:put_server_count(Was)),Is is Was+1,asserta(t_l:put_server_count(Is)),!,Is =< Max.
%


:- reg_egg_builtin(privmsg/1).
privmsg(D):- get_session_prefix_maybe(C),privmsg(C,D),!.
privmsg(D):- privmsg("#logicmoo",D),!.


%% privmsg( ?Channel, ?Fmt, ?Args) is det.
%
% Flushed Privmsg.
%
privmsg(Channel,Fmt,Args):-
  format(string(NS),Fmt,Args),
  privmsg(Channel,NS).


%% privmsg( ?Channel, ?Codes) is det.
%
% Privmsg Primary.
%
privmsg(Channel:_,Text):-nonvar(Channel),!,privmsg(Channel,Text).
privmsg(_:Channel,Text):-nonvar(Channel),!,privmsg(Channel,Text).
privmsg(Channel, [] ):- !, privmsg_prefixed(Channel,'  ').
privmsg(Channel,Text):- \+ is_codelist(Text), !, any_to_codelist(Text,Codes), privmsg(Channel, Codes).
privmsg(Channel,Text):- append(List,[Char],Text), code_type(Char, end_of_line), !, privmsg(Channel,List).
privmsg(Channel,Codes):- append(LCodes,[10|RCodes],Codes), privmsg(Channel,LCodes), privmsg(Channel,RCodes).
privmsg(Channel,Codes):- length(Codes,Len), Len>430,
   length(LCodes,400), append(LCodes,RCodes,Codes), !,
   privmsg_prefixed(Channel,LCodes), !, privmsg(Channel,RCodes).
privmsg(Channel,Codes):- privmsg_prefixed(Channel,Codes).


privmsg_prefixed(Channel, Data):- squelch_empty('$privmsg_prefixed',Channel,Data),!.
privmsg_prefixed(Channel, Text) :- any_to_codelist(Text, Out),
   ((get_session_prefix_maybe(Prefix), \+ same_channels(Prefix,Channel)) ->
     (format(codes(Data),'~w: ~s',[Prefix,Out]),privmsg1(Channel,Data));privmsg1(Channel,Out)).

%% privmsg( ?Channel, ?Codes) is det.
%
% Privmsg Primary Helper.
%

privmsg1(Channel,Data):- squelch_empty('$privmsg1',Channel,Data),!.
privmsg1(Channel,Text):-
   once(check_put_server_count(50)->privmsg2(Channel,Text);
   ignore(check_put_server_count(100)->privmsg_session(Channel,Text);true)).


%% privmsg2( ?OutStream, ?Channel, ?Text) is det.
%
% Privmsg Extended Helper.
%
privmsg2(Channel,Data):- squelch_empty('$privmsg2',Channel,Data),!.
privmsg2(Channel,Text):- any_to_codelist(Channel, EChannel), any_to_codelist(Text, EText), privmsg3(EChannel, EText),!.
/*
privmsg2(Channel,Text):- egg_to_string(Channel,CS),escape_quotes(Text,N),
  sleep(0.2),sformat(S,'\n.tcl putquick "PRIVMSG ~s :~s"\n',[CS,N]),
  dmsg(put_egg(S)),
  put_egg(S),!.
% privmsg2(Channel,Text):- escape_quotes(Text,N),ignore(catch(format(OutStream,'\n.tcl putserv "PRIVMSG ~s :~s" ;  return "noerror ."\n',[Channel,N]),_,fail)),!.
*/
escape_channel_name(C0,D):- text_to_string(C0,C), replace_in_string("[","\\[",C,CD),replace_in_string("]","\\]",CD,D).


%privmsg3(EChannel, EText):- put_egg('\n.msg ~s ~s\n',[EChannel, EText]),!.
privmsg3(Channel,  Text):- sleep(0.2), escape_quotes(Text, EText), escape_channel_name(Channel, EChannel), put_egg('\n.tcl putquick "PRIVMSG ~s :~s"\n',[EChannel, EText]),!.
%privmsg3(EChannel,  Text):- escape_quotes(Text, EText), put_egg('\n.tcl putserv "PRIVMSG ~s :~s"\n',[EChannel, EText]),!.





%% put_notice( ?OutStream, ?Channel, ?Text) is det.
%
% Putnotice.
%
put_notice(Channel,Data):- squelch_empty('$put_notice',Channel,Data),!.
put_notice(Channel,Text):-
   any_to_codelist(Channel, EChannel), escape_quotes(Text, EText),
   put_egg('\n.tcl putserv "NOTICE ~s :~s"\n',[EChannel, EText]),!.




%% privmsg_session( ?OutStream, ?Channel, ?Text) is det.
%
% Privmsg Session.
%
privmsg_session(Channel,Text):- t_l:session_id(ID),(ID==Channel->privmsg2(Channel,Text);privmsg2(ID,Text)).



%% put_egg( ?X) is det.
%
% Converted To Egg.
%
put_egg(X):-put_egg('~w',[X]),!.



%% put_egg( ?X, ?Y) is det.
%
% Converted To Egg.
%
put_egg(X,Y):-once(egg:stdio(_Agent,_InStream,OutStream)),
  once((sformat(S,X,Y),format(OutStream,'~s\n',[S]),!,flush_output(OutStream))).



%% escape_quotes( ?LIST, ?ISO) is det.
%
% Escape Quotes.
%
escape_quotes(Any,LISTO):- any_to_codelist(Any,LIST),
   list_replace_egg(LIST,92,[92,92],LISTM),
   list_replace_egg(LISTM,34,[92,34],LISTM2),
   list_replace_egg(LISTM2,91,[92,91],LIST3),
   list_replace_egg(LIST3,36,[92,36],LISTO),!.




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

skip_egg_go:- app_argv('--noirc'),!.
skip_egg_go:- app_argv('--nonet'),!.
skip_egg_go:- \+ app_argv('--irc'), \+ app_argv('--all'),!.

egg_go_maybe:- skip_egg_go,!.
egg_go_maybe:- egg_go.

egg_go:- thread_property(R,status(running)),R == egg_go,!.
egg_go:- thread_property(_,alias(egg_go)),threads,fail.
egg_go:- thread_create(egg_go_fg,_,[alias(egg_go),detached(true),an_exit(show_thread_exit)]).


egg_nogo:-thread_signal(egg_go,thread_exit(requested)).
/*
:- source_location(S,_),forall(source_file(H,S),ignore(( ( \+predicate_property(H,PP),member(PP,[(multifile),built_in]) ),
 functor(H,F,A),module_transparent(F/A),export(F/A),user:import(H)))).
*/



%% read_one_term_egg( SourceModule, ?Stream, ?CMD, ?Vs) is det.
%
% Read One Term Egg.
%
:- reg_egg_builtin(read_one_term_egg/4).
:-module_transparent(read_one_term_egg/4).

read_one_term_egg( SourceModule,Stream,CMD,Vs):- \+ is_stream(Stream),l_open_input(Stream,InStream),!,
       with_stream_pos(InStream,show_entry(read_one_term_egg( SourceModule,InStream,CMD,Vs))).
read_one_term_egg(_SourceModule,Stream,CMD,_ ):- at_end_of_stream(Stream),!,CMD=end_of_file,!.
% read_one_term_egg( SourceModule,Stream,CMD,Vs):- catch((input_to_forms(Stream,CMD,Vs)),_,fail),CMD\==end_of_file,!.
read_one_term_egg( SourceModule,Stream,CMD,Vs):- catch((read_term(Stream,CMD,[variable_names(Vs),module(SourceModule)])),_,fail),CMD\==end_of_file,!.
read_one_term_egg(_SourceModule,Stream,unreadable(String),_):-catch((read_stream_to_codes(Stream,Text),string_codes(String,Text)),_,fail),!.
read_one_term_egg(_SourceModule,Stream,unreadable(String),_):-catch((read_pending_input(Stream,Text,[]),string_codes(String,Text)),_,fail),!.


:- reg_egg_builtin(read_each_term_egg/3).
:-module_transparent(read_each_term_egg/3).



%% read_each_term_egg( ?S, ?CMD, ?Vs) is det.
%
% Read Each Term Egg.
%
read_each_term_egg(S,CMD,Vs):-
  show_failure(( l_open_input(S,Stream),
      findall(CMD-Vs,(
       repeat,
       read_one_term_egg( SourceModule,Stream,CMD,Vs),
       (CMD==end_of_file->!;true)),Results),!,
  ((member(CMD-Vs,Results),CMD\==end_of_file)*->true;read_one_term_egg( SourceModule,S,CMD,Vs)))).

%:- ensure_loaded(library(logicmoo/common_logic/common_logic_sexpr)).



%% read_egg_term( SourceModule, ?S, ?CMD, ?Vs) is nondet.
%
% Read Each Term Egg.
%
:- reg_egg_builtin(read_egg_term/4).
:-module_transparent(read_egg_term/4).
:- module_transparent(read_egg_term_0/4).

read_egg_term( SourceModule,String,CMD0,Vs0):-
  quietly(read_egg_term_0( SourceModule,String,CMD0,Vs0)).

read_egg_term_0( SourceModule,String,CMD0,Vs0):- string(String),!,
   split_string(String,""," \r\n\t",[SString]),
   atom_concat(_,'.',SString),
   open_string(SString,Stream),
   findall(CMD0-Vs0,(
       notrace(catch(read_term(Stream,CMD,[double_quotes(string),module(SourceModule),variable_names(Vs)]),_,fail)),!,
       ((CMD=CMD0,Vs=Vs0);
        (read_egg_term_more( SourceModule,Stream,CMD0,Vs0),CMD0\==end_of_file))),
      List),!,
   member(CMD0-Vs0,List).

read_egg_term_0(SourceModule,S,lispy(CMD),Vs):- text_to_string(S,String),
   '$current_source_module'(SourceModuleWas),
   setup_call_cleanup('$set_source_module'(SourceModule), notrace(catch((input_to_forms(String,CMD,Vs)),_,fail)),
    '$set_source_module'(SourceModuleWas)),!.

read_egg_term_more( SourceModule,Stream,CMD,Vs):-
       repeat,
        catch(read_term(Stream,CMD,[double_quotes(string),module(SourceModule),variable_names(Vs)]),_,CMD=err),
        (CMD==err->(!,fail);true),
        (CMD==end_of_file->!;true).

:- user:import(read_egg_term/4).


:- fixup_exports.

% :- ircEvent("dmiles","dmiles",say("(?- (a b c))")).

% :- bot_nick(Nick),set_irc_nick(Nick).

