/*
 * &gt;module> 
% NPC_Interface for calling actions
%
% "command"s are often text or a description of proposed actions
% "action" is a MUD understood command (GOAL)
%
% Douglas Miles
% Dec 13, 2035
%
*/
% :-swi_module(world_agent,[get_session_id/1]).

:- include(prologmud(mud_header)).
/*
:-swi_module(world_agent, [ agent_call_unparsed/1, agent_call_unparsed/2,  agent_call_command_now/2 ]).


tAgent - Players and Bot Bodies

Sessions - places players control things
      - irc clients (no threads/consoles)
      - telnet users

agent->nick



*/
:- multifile baseKB:agent_action_queue/3.
:- dynamic baseKB:agent_action_queue/3.

immediate_session(_P,_C,_O):-!.

do_agent_action_queue(P):- agent_action_queue(P,C,O),must(with_session(O,agent_call_unparsed(P,C))),retract(agent_action_queue(P,C,O)).
do_agent_action_queue(_). % was empty already


:-export(enqueue_agent_action/1).
enqueue_agent_action(C):-enqueue_agent_action(_,C).

:-export(enqueue_agent_action/2).
enqueue_agent_action(P,C):-foc_current_agent(P),get_agent_session(P,O),enqueue_agent_action(P,C,O).

:-export(enqueue_agent_action/3).
:-dynamic(enqueue_agent_action/3).
enqueue_agent_action(P,C,O):- immediate_session(P,C,O),!, do_agent_action(P,C,O).
enqueue_agent_action(P,C,O):- assertz(agent_action_queue(P,C,O)),must(once(ain(agent_action_queue(P,C,O)))),!.

:-export(do_agent_action/1).
do_agent_action(C):-enqueue_agent_action(C).
:-export(do_agent_action/2).
do_agent_action(P,C):-enqueue_agent_action(P,C).
:-export(do_agent_action/2).
do_agent_action(P,C,O):- \+ immediate_session(P,C,O), !, enqueue_agent_action(P,C,O).
do_agent_action(P,C,_):- var(C),!,fmt('unknown_var_command(~q,~q).',[P,C]).
do_agent_action(_,EOF,_):- end_of_file == EOF, !, npc_tick_tock.
do_agent_action(_,'',_):-!, npc_tick_tock.
do_agent_action(P,C,O):- do_gc,with_session(O,agent_call_unparsed(P, C)),!.
do_agent_action(P,C,_):-wdmsg("skipping_unknown_player_action(~q,~q).~n",[P,C]),!.

%check_word(SVERB):- var_non_attvar(SVERB),!, when(nonvar(SVERB),check_word(SVERB)),!.
check_word(SVERB):- atom(SVERB), atom_concat('[',_,SVERB),trace_or_throw(bad_parse_agent_text_command(SVERB)).
check_word(_).

:-export(parse_agent_text_command_checked/5).
parse_agent_text_command_checked(Agent,VERB,ARGS,NewAgent,CMD):- 
  % check_word(VERB), 
  catch(( parse_agent_text_command(Agent,VERB,ARGS,NewAgent,CMD),
         nonvar(CMD),must(nonvar(NewAgent))),'$aborted',true),
         ignore((CMD=actTick)),ignore((NewAgent=Agent)).

parse_agent_text_command_checked(Agent,VERB,ARGS,NewAgent,CMD):- 
   debugging_logicmoo(logicmoo(parser)), parse_agent_text_command(Agent,VERB,ARGS,NewAgent,CMD).

must_ac(G):- show_failure(must(G)).

:-  message_queue_property(_Queue, alias(waq)) -> true;message_queue_create(_,[alias(waq)]).

thread_signal_blocked(ID,Goal):- thread_self(ID),!,Goal.
thread_signal_blocked(ID,Goal):- message_queue_property(Queue, alias(waq)),thread_self(Waiter), thread_signal(ID,(Goal,thread_send_message(Queue,done(Goal,Waiter),[]))),thread_get_message(Queue,done(Goal,Waiter)).


do_agent_action_queues:- repeat,sleep(0.25),once(on_x_log_cont(do_agent_action_queue(_))),fail.

start_agent_action_thread:- 
  (thread_property(T,alias(agent_action_queue_thread)) ->
   (thread_property(T,status(running))->true;(thread_join(agent_action_queue_thread,_);
     thread_create(do_agent_action_queues,_,[alias(agent_action_queue_thread)])));
    thread_create(do_agent_action_queues,_,[alias(agent_action_queue_thread)])).


   

% restarts if it it died
one_minute_timer_tick:- start_agent_action_thread.

with_session(ID,CALL):-locally(t_l:session_id(ID),CALL).


% =====================================================================================================================
% agent_call_unparsed --> agent_call_words --> agent_call_command_now
% =====================================================================================================================

agent_call_unparsed(C):-foc_current_agent(A),!,agent_call_unparsed(A,C).

agent_call_unparsed(A,C):-  locally(tlbugger:old_no_repeats, must(agent_call_unparsed_0(A,C))).

agent_call_unparsed_0(Agent,Var):-var(Var),trace_or_throw(var_agent_call_unparsed(Agent,Var)).

agent_call_unparsed_0(_Gent,Atom):- (Atom='[]' ;Atom='''' ;Atom='""' ; (atomic(Atom);(atom(Atom),atom_length(Atom,0)))),!.
% execute a prolog command including prolog/0
agent_call_unparsed_0(_Gent,Atom):- atomic(Atom), catch((
   (once((on_x_fail(read_term_from_atom(Atom,OneCmd,[variables(VARS)])),
      predicate_property(OneCmd,_),
      fmt('doing command ~q~n',[OneCmd]))),!, doall((OneCmd,fmt('Yes: ~w',[VARS]))))),E,(dmsg(E),fail)).


% lists
agent_call_unparsed_0(A,Atom):-to_word_list(Atom,List),must(is_list(List)),!,agent_call_words(A,List).

agent_call_words(_,Words):- Words==[],!.
agent_call_words(A,Words):- (\+ is_list(Words)),must(agent_call_unparsed(A,Words)),!.
agent_call_words(Agent,Text):- text_to_string_safe(Text,String)->Text\=@=String,!,agent_call_unparsed(Agent,String).


% remove period at end
agent_call_words(A,PeriodAtEnd):-append(New,[(.)],PeriodAtEnd),!,agent_call_words(A,New).

% concat the '@'
agent_call_words(Ag,[A,B|REST]):- atom(A),atom(B),A=='@',atom_concat(A,B,C),!,agent_call_words(Ag,[C|REST]).

agent_call_words(Agent,[VERB|ARGS]):-

  check_word(VERB),
  %sanity(freeze(ARGS,must(is_list(ARGS)))),  
  %sanity(freeze(CMD,sanity(callable(CMD)))),

      must(on_x_debug(parse_agent_text_command_checked(Agent,VERB,ARGS,NewAgent,CMD))),
      must_ac(agent_call_command_now(NewAgent,CMD)),!.

agent_call_words(A,[CMD]):- !, must_ac(agent_call_command_now(A,CMD)),!.
agent_call_words(A,CMD):- must_ac(agent_call_command_now(A,CMD)),!.

:-export(where_atloc/2).
where_atloc(Agent,Where):-mudAtLoc(Agent,Where).
where_atloc(Agent,Where):-localityOfObject(Agent,Where).
where_atloc(Agent,Where):-mudAtLoc(Agent,Loc),!,locationToRegion(Loc,Where).
where_atloc(Agent,'OffStage'):-fail,nonvar(Agent).


% All Actions must be called from here!
agent_call_command_now(Agent,CMD  ):- var(CMD),!,trace_or_throw(var_agent_call_command_now(Agent,CMD)).
agent_call_command_now(Agent,Text ):- text_to_string_safe(Text,String)->show_call(loop_check(agent_call_unparsed(Agent,String))),!.
agent_call_command_now(Agent,CMD  ):- subst(CMD,isSelfAgent,Agent,NewCMD),CMD\=@=NewCMD,!,agent_call_command_now(Agent,NewCMD).
agent_call_command_now(Agent,Words):- is_list(Words),maplist(check_word,Words),loop_check(agent_call_words(Agent,Words)).
agent_call_command_now(Agent,CMD  ):- correctCommand(Agent,CMD,NewCMD),CMD\=@=NewCMD,!,agent_call_command_now(Agent,NewCMD).
agent_call_command_now(Agent,CMD  ):- \+ where_atloc(Agent,_),!, agent_call_command_now_2(Agent,CMD),!.
agent_call_command_now(Agent,CMD  ):- where_atloc(Agent,Where),
   % start event
   must(raise_location_event(Where,actNotice(reciever,begin(Agent,CMD)))),
   (call(on_x_debug(agent_call_command_now_2(Agent,CMD)) ->
   % event done
     send_command_completed_message(Agent,Where,done,CMD);
   % event fail
     send_command_completed_message(Agent,Where,failed,CMD))),!.

agent_call_command_now_2(Agent,CMD):- loop_check((agent_call_command_now_3(Agent,CMD)),dmsg(looped(agent_call_command_now_2(Agent,CMD)))).
agent_call_command_now_3(Agent,CMD):-
   with_agent(Agent,
     locally(t_l:side_effect_ok,
     locally(t_l:agent_current_action(Agent,CMD),
  ((
  % call_no_cuts(agent_call_command(Agent,CMD))
    find_and_call(agent_call_command(Agent,CMD))
     *->true;agent_call_command_all_fallback(Agent,CMD)))))),
  padd(Agent,mudLastCommand(CMD)).

agent_call_command_all_fallback(Agent,CMD):- if_defined(agent_call_command_fallback(Agent,CMD)),!.
agent_call_command_all_fallback(_Agent,CMD):- fail, nop(xlisting(CMD)).

:-export(send_command_completed_message/4).
send_command_completed_message(Agent,Where,Done,CMD):-
     ignore((must_det_l((flush_output,renumbervars_prev(CMD,SCMD),Message =..[Done,Agent,SCMD],
                raise_location_event(Where,actNotice(reciever,Message)),flush_output)))),!.



correctCommand(_,CMD,CMD):-!.
correctCommand(Who,CMD,OUT):-compound(CMD),show_failure(correctCommand_0(Who,CMD,OUT)),!.
correctCommand(_,CMD,CMD).

correctEachTypeOrFail( Who, F, Q,ARGS,TYPES,NEWS):- is_list(TYPES),!,maplist(correctEachTypeOrFail(Who,F,Q),ARGS,TYPES,NEWS).
correctEachTypeOrFail(_Who,_F,_Q,Arg,Type,Inst):- not(is_ephemeral(Arg)),not(is_ephemeral(Type)),isa(Arg,Type),!,Inst = Arg.
correctEachTypeOrFail(_Who,_F,_Q,Arg,Type,Inst):- not(is_ephemeral(Arg)),not(is_ephemeral(Type)), must(coerce(Arg,Type,Inst)),not(is_ephemeral(Inst)),!.
correctEachTypeOrFail(_Who,_F,_Q,Arg,Type,Inst):- not(is_ephemeral(Arg)), show_failure(coerce(Arg,Type,Inst)),not(is_ephemeral(Inst)),!.
correctEachTypeOrFail(_Who,_F,_Q,Arg,Type,Inst):- coerce(Arg,Type,Inst),not(is_ephemeral(Inst)),!.
correctEachTypeOrFail(_Who,_F,_Q,Arg,Type,Inst):- !,acceptableArg(Arg,Type),!,Inst = Arg.

correctCommand_0(Who,CMD,OUT):-
   compound(CMD),
   must(current_agent(Who)),
   CMD=..[F|ARGS],   
   functor(CMD,F,A),
   functor(MATCH,F,A),!,
   vtActionTemplate(MATCH),compound(MATCH),MATCH=..[F|TYPES],
   correctEachTypeOrFail(Who,F,query(t, must),ARGS,TYPES,NEWS),!,
   OUT=..[F|NEWS],!.

acceptableArg(Arg,Type):-dmsg(acceptableArg(Arg,Type)).

:-export(current_agent/1).
current_agent(PIn):- get_session_id(O),get_agent_session(P,O),!,P=PIn.
% :-mpred_core:import(current_agent/1).

:-export(current_agent_or_var/1).
current_agent_or_var(P):- once(current_agent(PIn)),P=PIn,!.
current_agent_or_var(_).

interesting_to_player(Type,Agent,C):- contains_var(C,Agent),dmsg(agent_database_hook(Type,C)),!.
interesting_to_player(Type,Agent,C):-is_asserted(localityOfObject(Agent,Region)),contains_var(C,Region),dmsg(region_database_hook(Type,C)),!.
interesting_to_player(Type,Agent,C):-is_asserted(localityOfObject(Agent,Region)),is_asserted(localityOfObject(Other,Region)),contains_var(C,Other),!,dmsg(other_database_hook(Type,C)),!.

decl_database_hook(Type,C):- current_agent(Agent),interesting_to_player(Type,Agent,C).

get_agent_input_stream(P,In):-no_repeats(P-In,(get_agent_session(P,O),lmcache:session_io(O,In,_,_))).

get_agent_input_thread(P,Id):-no_repeats(P-Id,(get_agent_input_stream(P,In),lmcache:session_io(_,In,_,Id))).

with_agent(P,CALL):-with_agent0(P,CALL).

with_agent0(P,CALL):-
 get_session_id(TS),must(nonvar(TS)),
 thread_self(Self),
 get_agent_session(P,O),lmcache:session_io(O,In,_Out,Id),Id=Self,current_input(In),!,
 locally([t_l:put_server_no_max,lmcache:session_agent(TS,P),lmcache:agent_session(P,TS)],CALL).

with_agent0(P,CALL):-
 get_session_id(TS),must(nonvar(TS)),
 thread_self(Self),
 ((get_agent_session(P,O),lmcache:session_io(O,_In,_Out,Id),Id\=Self)->Wrap=thread_signal_blocked(Id);Wrap=call),!,
  call(Wrap, 
    locally([t_l:put_server_no_max,lmcache:session_agent(TS,P),lmcache:agent_session(P,TS)],
      with_output_to_predicate(deliver_event(P),CALL))).

has_tty(O):-no_repeats(O,lmcache:session_io(O,_,_,_)).

:-export(get_agent_session/2).
get_agent_session(P,O):-get_session_id(O),get_agent_sessions(P,O),!.
get_agent_session(P,O):-get_agent_sessions(P,O),has_tty(O).
:-export(get_agent_sessions/2).
get_agent_sessions(P,O):- no_repeats(P-O,(lmcache:session_agent(O,P);lmcache:agent_session(P,O);(baseKB:irc_user_plays(P,O,C),ground(baseKB:irc_user_plays(P,O,C))))).

:-export(foc_current_agent/1).
foc_current_agent(P):- current_agent(P),nonvar(P),!.
foc_current_agent(P):- nonvar(P),tAgent(P),!,become_player(P),!.
foc_current_agent(P):- 
  must_det_l((    
             get_session_id(_),
             once((get_dettached_npc(NPC),NPC=P);generate_new_player(P)),!,
             become_player(P))),!.
               

:-user:ensure_loaded(library(http/http_session)).

get_session_id(IDIn):-guess_session_ids(ID),nonvar(ID),!,ID=IDIn.
:-export(get_session_id/1).
:-system:import(get_session_id/1).

% return any thread locally set session
guess_session_ids(ID):-t_l:session_id(ID).
% irc session
guess_session_ids(ID):-if_defined(chat_config:chat_isWith(_,ID),true).
% telnet session
guess_session_ids(ID):-thread_self(TID),lmcache:session_io(ID,_,_,TID).
% returns http sessions as well
guess_session_ids(ID):-if_defined(http_in_session:http_in_session(ID)).
% tcp session
guess_session_ids(ID):-thread_self(TID),thread_property(TID,alias(ID)).
% anonymous sessions
guess_session_ids(ID):-thread_self(ID), \+ thread_property(ID,alias(ID)).
% anonymous sessions
guess_session_ids(In):-thread_self(ID),call(call,thread_util:has_console(ID,In,_Out,_Err)).


:-export(my_random_member/2).
my_random_member(ELE,[LOC]):-nonvar(LOC),!,ELE=LOC.
my_random_member(LOC,LOCS):- must_det((length(LOCS,Len),Len>0)),random_permutation(LOCS,LOCS2),!,member(LOC,LOCS2).

:-meta_predicate(random_instance_no_throw(+,-,0)).
random_instance_no_throw(Type,Value,Test):- random_instance_no_throw0(Type,Value,Test).

random_instance_no_throw0(Type,Value,Test):-var(Test),!,random_instance_no_throw(Type,Value,isa(Test,Type)).

random_instance_no_throw0(Type,Value,Test):- fail, copy_term(ri(Type,Value,Test),ri(RType,RValue,RTest)),
  Responded = responded(_),
  (  ((call_u(hooked_random_instance(RType,RValue,RTest)) *-> nb_setarg(1,Responded,answered) ; fail)) ;
     (ground(Responded)->(!,fail);fail)),
   checkAnyType(query(_,_),RValue,Type,Value),
   once(Test).

random_instance_no_throw0(Type,Value,Test):- atom(Type),atom_concat('random_',Type,Pred),
   Fact=..[Pred,Value],current_predicate(Pred/1),!,call(Fact),Test.

random_instance_no_throw0(Type,Value,Test):- compound(Type),get_functor(Type,F),
  atom_concat('random_',F,Pred),current_predicate(F/1),
  Fact=..[Pred,Value],
  current_predicate(Pred/1),!,Fact,Test.

random_instance_no_throw0(Type,Value,Test):- compound(Type),get_functor(Type,F,A),functor(Formatted,F,A),t(meta_argtypes,Formatted),
                         Formatted=..[F|ArgTypes],functor(Value,F,A),Value=..[F|ValueArgs],
                           must((maplist(random_instance_no_throw,ArgTypes,ValueArgs,_),Test)).

random_instance_no_throw0(Type,Value,Test):-
   must(( findall(V,isa(V,Type),Possibles),Possibles\==[])),!,must((my_random_member(Value,Possibles),Test)).


get_dettached_npc(P):- random_instance_no_throw(tAgent,P, \+ isa(P,tHumanControlled)).


:-multifile(system:random_instance/3).
:-dynamic(system:random_instance/3).
:-export(system:random_instance/3).
%:- rtrace.
system:random_instance(Type,Value,Test):- cwc, must(random_instance_no_throw(Type,Value,Test)).
%:- nortrace.
%:- break.


generate_new_player(P):- var(P),!,must_det_l((gensym(iExplorer,N), \+ ((isa_asserted(N,tAgent))),P=N,ensure_new_player(P))),!.
generate_new_player(P):- ensure_new_player(P),!.

ensure_new_player(P):- must_det_l([nonvar(P),assert_isa(P,mobExplorer),assert_isa(P,tHumanControlled),assert_isa(P,tAgent)]),!.

assumed_detached_player(P):- lmcache:agent_session(P,_),!,trace_or_throw(assumed_detached_player(P)).
assumed_detached_player(_).

:-export(become_player/1).
become_player(P):- once(current_agent(Was)),Was=P,!.
become_player(P):- get_session_id(O),retractall(lmcache:agent_session(_,O)),
  assert_isa(P,tHumanControlled),must(create_agent(P))->
  assumed_detached_player(P),asserta_new(lmcache:agent_session(P,O)),!,
  put_in_world(P).

:-export(become_player/2).
become_player(_Old,NewName):-become_player(NewName).

% Lists all the agents in the run. Except for other monsters.
list_agents(Agents) :- agent_list(Agents), !.
list_agents(Agents) :- % build cache
	findall(NearAgent,req1(tAgent(NearAgent)),Agents),
	assert(agent_list(Agents)),!.

:-export((agent_into_corpse/1, display_stats/1)).

tCol(tCorpse).

% When an agent dies, it turns into a tCorpse.
% corpse is defined as an object in the *.objects.pl files
agent_into_corpse(Agent) :-
        must(mudAtLoc(Agent,LOC)),
        Newthing = iCorpseFn(Agent),
        assert_isa(Newthing,tCorpse),
	ain(mudAtLoc(Newthing,LOC)),
        del(mudAtLoc(Agent,LOC)),
	clr(mudStr(Agent,_)),
	clr(mudHeight(Agent,_)),
	clr(mudStm(Agent,_)),
	clr(mudSpd(Agent,_)).

% Displays all the agents stats. Used at end of a run.
display_stats(Agents) :-
	forall(member(Agent,Agents),
	          (mudEnergy(Agent,Chg),
		  mudHealth(Agent,Dam),
		  mudScore(Agent,Scr),
		  findall(Obj,mudPossess(Agent,Obj),Inv),
		  write('Agent = '), write(Agent), nl,
		  write('Charge = '), write(Chg), write('  '),
		  write('Dam= ' ), write(Dam), write('  '),
		  write('Score = '), write(Scr), nl,
		  write('Inventory = '), write(Inv), nl)).

:- fixup_exports.
:- all_source_file_predicates_are_transparent.



