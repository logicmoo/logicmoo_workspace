:- if(current_prolog_flag(xref,true);(prolog_load_context(file,F),prolog_load_context(source,F))).
:- module(discord_chat,[]).
:- endif.
% ====================================
% Discord Chat Calls
% ====================================

self_user("PrologMUD").

dont_reply_user("irc0").
dont_reply_user("LM489").
dont_reply_user("jllykifsh").
dont_reply_user(X):- self_user(X).

add_discord_chat_event(ID,Message):- assertz(tmp:discord_chat_event(ID,Message)).

author_user(MID,Who):- 
 (discord_ddd(MID,author_username,Who), 
   \+ discord_ddd(MID,recipient_name,Who),
   \+ self_user(Who))*-> true; discord_ddd(MID,author_username,Who).

handle_discord_chat_event(ID,_Message):- discord_ddd(_,referenced_message,ID),!.
handle_discord_chat_event(ID,_Message):- discord_ddd(ID,referenced_message,_),!.
handle_discord_chat_event(ID,content(Message)):-  
  author_user(ID,User), !, 
  handle_discord_chat_event(ID,say(User,Message)).

handle_discord_chat_event(ID,Message):- remember_task(handle_chat_events), my_wdmsg(chat_event(ID,Message)),fail.
handle_discord_chat_event(ID, _):- \+ number(ID),!.

handle_discord_chat_event(ID,say(User,_Message)):- fail, self_user(User),!,
  retract(t_l:discord_msg_id(DEST)),
  flush_channel_output_buffer(DEST),
  assert(t_l:discord_msg_id(ID)),!.


handle_discord_chat_event(_ID,say(User,_Message)):- dont_reply_user(User),!.
handle_discord_chat_event(ID,say(User,Message)):- !,
 trim_message(Message,Str), 
 discord_set_typing(ID),
 thread_id_user_say(ID,User,Str),!. 
handle_discord_chat_event(ID,Message):- dmsg(failed_chat_event(ID,Message)).

:- dynamic(tmp:output_predicate_stream_for_uid/2).

output_stream_for_dest(User,Err):- tmp:output_predicate_stream_for_uid(User,Err),!.
output_stream_for_dest(User,Err):- 
  new_predicate_output_stream(discord_chat_say_buff(User), Err),
  set_stream(Err,buffer(false)),
  assert(tmp:output_predicate_stream_for_uid(User,Err)).

%input_stream_for_dest(User,Err):- tmp:input_predicate_stream_for_uid(User,Err),!.
input_stream_for_dest(DEST,User,Err):- 
  new_predicate_input_stream(discord_chat_hear_buff(DEST,User), Err),
  assert(tmp:input_predicate_stream_for_uid(DEST,User,Err)).

discord_chat_hear_buff(DEST,User,Input):- 
 any_to_channel_id(DEST,CID),get_time(Now),
 setup_call_cleanup(asserta(tmp:hook_user_chat(DEST,User,CID,Now),Clause),
  get_hear_buff(DEST,User,CID,Now,Input),
  erase(Clause)).

get_hear_buff(DEST,User,CID,Now,Input):-
 sformat(S,"Reading input from: ~w in ~w after ~w (reply to this message)",[User,CID,Now]),
 discord_say(DEST,S),
 repeat,
  sleep(1),
  once((
  discord_ddd(MID,author_username,User),
  nop(discord_ddd(MID,channel_id,CID)),
  discord_ddd(MID,content,InputS),
  discord_ddd(MID,timestamp,Time),
  Time>Now,
  dmsg(m(MID,User,Now,Time,InputS)))),!,
  feed_input(InputS,Input).

feed_input(InputS,Input):- 
  member(Input,[InputS,'\n']).


get_stdio(DEST,User,In,Out,Err):-   
  input_stream_for_dest(DEST,User,In),
  %stream_property(In,file_no(0)),
  output_stream_for_dest(DEST,Out),
  %stream_property(Err,file_no(2)),
  output_stream_for_dest(User,Err),
  !.
get_stdio(_DEST,_User,In,Out,Err):-
   %stream_property(In,file_no(0)),
   current_input(In),
   %current_output(Out), 
   stream_property(Out,file_no(1)),
   %stream_property(Err,file_no(1)), % this is user_output (not user_error)
   %get_thread_current_error(Err),
   stream_property(Err,file_no(2)),
   !.


thread_id_user_say(DEST,User,Str):-
   G = id_user_say(DEST,User,say(Str)),
   discord_debug_cvt(G,Info),
   term_to_atom(Info,A),
   get_stdio(DEST,User,In,Out,Err),
   thread_create(
    call_cleanup(
       discord_in_thread(DEST,G,In,Out,Err),
       flush_channel_output_buffer(DEST)),
   _,[alias(A),detached(true),debug(false)]).

:- dynamic thread_util:has_console/4.

discord_in_thread(DEST,G,In,Out,Err):- 
  thread_self(Self),
  HasConsole = thread_util:has_console(Self, In, Out, Err),
  locally(t_l:discord_msg_id(DEST),
   ignore((
   set_prolog_IO(In,Out,Err),
   %nodebug,
   setup_call_cleanup(
    assert(HasConsole),
    discord_no_debug(G),
    retractall(HasConsole))))).

discord_no_debug(G):- !, call(G).
discord_no_debug(G):- with_no_xdbg(G).

term_to_goal(?- Goal, Goal).

chat80d:- id_user_say(893480190062239765,dmiles,term(?- test_chat80,[])).

discord_really_call(Channel,Agent,CALL,Vs):- 
   source_and_module_for_agent(Agent, SourceModule, CallModule),
   (SourceModule == CallModule -> AgentModule = CallModule ; AgentModule = (SourceModule:CallModule)),
   Debug = discord_really_call(Channel,Agent,AgentModule:CALL,Vs),
   my_wdmsg(Debug),
  % debug(_), % gtrace,
   setup_call_cleanup(
        (b_setval('$variable_names',Vs), b_setval('$term',CALL),use_agent_module(Agent)),
         catch(ignore(do_full_goal(AgentModule:CALL,Vs)),E,
             (((my_wdmsg(E),my_wdmsg(Debug),
                sformat(S,"~q caused by ~q",[E,Debug]),discord_say(Agent,S),!,fail)))),
         save_agent_module(Agent)),!.


do_full_goal(Goal,Vs):-
 flag(ans,_,0),
 weto(((Goal, 
       deterministic(Done),
       flag(ans,N,N+1),
       discord_show_each_result(Goal,Done,N,Vs)))),!.
do_full_goal(_Goal,_Vs):- flag(ans,W,W),
   (W==0 -> writeln(' % No. ');
   (W==1 -> writeln(' % Yes. ');
    format(' % Yes ~d answers',[W]))),!.

id_user_say(DEST,User,say(Str)):- notrace_catch(read_term_from_atom(Str,Term,[variable_names(Vs)])),!,id_user_say(DEST,User,term(Term,Vs)).
id_user_say(DEST,User,term(Term,Vs)):- 
  term_to_goal(Term,Goal),!,
  with_output_to_predicate(discord_chat_say_buff(DEST),
    catch(discord_no_debug(discord_really_call(DEST,User,Goal,Vs)),Err,(sformat(S,"~p",[(Goal --> Err)]),discord_say(User,S),throw(Err)))).

id_user_say(DEST,User,Str):- 
  without_ddbg(external_chat_event(discord_client:discord_chat_override,DEST,User,Str)).

  
trim_message(A,C):- replace_in_string(['\n          \n'='\n'],A,B),A\==B,!,trim_message(B,C).
trim_message(A,C):- \+ string(A),!,any_to_string(A,S),trim_message(S,C).
trim_message(A,C):- split_string(A, "", "`\s\t\n", [B]), A\==B,!,trim_message(B,C).
trim_message(A,A).

discord_set_typing(ID):- any_to_chan_id(ID,CID),ID\==CID,!,discord_set_typing(CID).
discord_set_typing(ID):- how_long_ago(discord_set_typing(ID),Ago),number(Ago), Ago < 5,!. % only sending typing idicator every 5 seconds
discord_set_typing(ID):- 
  remember_task(discord_set_typing(ID)),
  discord_http(channels/ID/typing,[method(post)]).

:- use_module(library(eggdrop)).

:- dynamic(t_l:discord_msg_id/1).
:- dynamic(tmp:channel_output_buffer/2).

:- meta_predicate(disco_call(:)).
disco_call(G):- wots(S,G),discord_say(S).

discord_write_done(det('Yes',_)):-!. 
discord_write_done(Data):- write(' '),write(Data), write(' ').

discord_show_each_result(_CMD,Done,_,[]):- !, Done==true,  
  get_channel_output_buffer(Str),trim_message(Str,Trimed),
  (Trimed == "" -> write(" % Yes. ") ; true).

discord_show_each_result(_CMD,Done,N,Vs):- 
 (current_prolog_flag(discord_max_results,MAX);MAX=1000;MAX=inf),!,
  once(once((Done==true -> (once(\+ \+ write_varbindings_discord(Vs))) ; 
    (once(\+ \+ write_varbindings_discord(Vs)),N>MAX)))).

write_varbindings_discord(Vs):- 
 wots(Str,eggdrop:write_varbindings(Vs)),
 discord_chat_say(Str).

discord_chat_say(Str):- current_discord_channel(DEST), discord_chat_say_buff(DEST,Str),!.
discord_chat_say(_,Msg):- current_discord_channel(DEST), discord_chat_say_buff(DEST,Msg),!.

discord_chat_say_buff(DEST,Msg):-
 text_to_string(Msg,Str),
 discord_set_typing(DEST),
 my_wdmsg(discord_chat_say_buff(DEST,Str)),
 assertz(tmp:channel_output_buffer(DEST,Str)),
 ignore((fail,contains_nls(Str),flush_channel_output_buffer(DEST))),!.

contains_nls(Str):- atomic_list_concat(List,'\n',Str),length(List,L),L>3.
   
deque_discord_chat_sends:- sleep(1), tmp:channel_output_buffer(DEST,_),flush_channel_output_buffer(DEST), fail.
deque_discord_chat_sends:- sleep(3),
   tmp:channel_output_buffer(DEST,_),   %discord_set_typing(DEST),
   flush_part_channel_output_buffer(DEST).
deque_discord_chat_sends.
  
flush_part_channel_output_buffer(ID):-
  fix_desination(ID,DEST),
  with_channel_output_buffer(DEST,call,Msg),
  string_list_concat(List,'\n',Msg),
  length(List,L),
  ignore((L>1,
    split_msg(L,List,Left,Right),
    %my_wdmsg(split_msg(L,List,Left,Right)),
    atomics_to_string( Left,'\n',LP),
    atomics_to_string(Right,'\n',RP),
    retractall(tmp:channel_output_buffer(DEST,_)),
    asserta(tmp:channel_output_buffer(ID,RP)),
    discord_say(ID,LP))),!.

count_sep("",_,-1):-!.
count_sep(A,S,0):- \+ atom_contains(A,S),!.
count_sep(A,S,AN):- atom_contains(A,'\t'),!,replace_in_string(['\t'='  '],A,R),!,count_sep(R,S,AN).
count_sep(A,S,AN):- string_list_concat([_|L],S,A),append(LL,[R|RR],L),(R \== "";RR=[]),!, length(LL,AN).
count_sep(_,_,1).

min(X,Y,X):- X<Y,!.
min(_,X,X).

bet(N, M, _, K) :- N =< M, K = N.
bet(N, M, Inc,  K) :- N < M, N1 is N+Inc, bet(N1, M, Inc, K).

split_msg(N,L,Left,Right):- N < 95, Left=[_|_], split_msgl(L,Left,Right), length(Left,K), K < 96, !.
split_msg(N,L,Left,Right):- min(96,N,Min), bet(Min, 1, -1, K), length(Left,K), split_msgl(L,Left,Right),!.
split_msg(N,L,Left,Right):- N > 95, length(Left,96),append(Left,Right,L),!. 

split_msgl(L,Left,Right):- 
  L = [A,B|Rest],
  \+ sub_string(A,0,1,_," "),sub_string(B,0,1,_," "), 
  nth0(N,Rest,C), (C==""->(!,fail);true),
  \+ sub_string(C,0,1,_," "),
  N>1,
  length(Some,N),
  Left=[A,B|Some],
  append(Left,Right,L),!.

split_msgl(L,Left,Right):- 
  L = [A,B|Rest],
  sub_string(A,0,2,_,Start), Start\==" ", sub_string(B,0,2,_,Start),
  nth0(N,Rest,C), (C==""->(!,fail);true),
  sub_string(C,0,2,_,CStart),
  CStart\==Start, N>1,
  length(Some,N),
  Left=[A,B|Some],
  append(Left,Right,L),!.

split_msgl(L,Left,Right):- append(Left,["",""|Right],L). 
split_msgl(L,Left,Right):- append(Left,[""|Right],L). 
split_msgl(L,Left,[A|Right]):- append(Left,[A|Right],L),atom_contains(A,'/*').
split_msgl(L,Left,Right):- append(Before,[A|Right],L),atom_contains(_,'*/',A),append(Before,[A],Left).
split_msgl(L,Left,[A|Right]):- append(Left,[A|Right],L),atom_concat('?-',_,A).
split_msgl(L,Left,Right):- append(Before,[A|Right],L),atom_concat(_,'.',A),append(Before,[A],Left).
split_msgl(L,Left,[D|Right]):- append(Before,[A,B,C,D|Right],L),count_sep(A," ",AN),count_sep(B," ",BN),count_sep(C," ",CN),count_sep(D," ",DN), AN>=BN,BN>CN,CN<DN, append(Before,[A,B,C],Left).

flush_channel_output_buffer(DEST):- 
  ignore((with_channel_output_buffer(DEST,retract,Msg),
         discord_say(DEST,Msg))).

%  with_output_to_predicate(write,(current_output(O),predicate_streams:set_error(O),ls,flush_output(O))).


current_discord_channel(DEST):- t_l:discord_msg_id(DEST).

get_channel_output_buffer(Str):- current_discord_channel(DEST), get_channel_output_buffer(DEST,Str),!.
get_channel_output_buffer(DEST,Str):- with_channel_output_buffer(DEST,call,Str).

fix_desination(DEST,DEST):-!.
fix_desination(_DEST,_ID).

with_channel_output_buffer(DEST,Call,Str):- 
 fix_desination(DEST,ID),
 findall(Msg,call(Call,tmp:channel_output_buffer(ID,Msg)),MsgL),
 atomics_to_string_nl(MsgL,Str).

atomics_to_string_nl([],""):-!.
atomics_to_string_nl(MsgL,Str):- member(Msg,MsgL),atom_contains(Msg,"\n"),!,atomics_to_string(MsgL,Str).
atomics_to_string_nl(MsgL,Str):- atomics_to_string(MsgL,"\n",Str).

%discord_chat_override(Goal):- dmsg(discord_chat_override(Goal)),fail.
discord_chat_override(put_msg(DEST,Msg)):- !, discord_chat_say(DEST,Msg).
discord_chat_override(put_notice(DEST,Msg)):- !, discord_chat_say(DEST,Msg).
discord_chat_override(emlmp(Goal)):- !, discord_no_debug(notrace(Goal)). % overrides `maybe_long_message_printer(4, Goal).`
discord_chat_override(Goal):- functor(Goal,F,A),atom_concat('discord_',F,DF), discord_client:current_predicate(DF/A), !,
  Goal=..[F|Args],DGoal=..[DF|Args],!,discord_no_debug(DGoal).
discord_chat_override(Goal):- dmsg(skipped_chat_override(Goal)).
%discord_chat_override(Goal):- discord_client:call(Goal). % in-case there are new ones


discord_say(X):- discord_say('dmiles',X),discord_say('prologmud_bot_testing',X).
% https://discord.com/oauth2/authorize?client_id=772113231757574185&scope=bot&permissions=268823638

:- dynamic(tmp:msg_refed_once/1).

discord_say(Channel,Msg):- atom_or_string(Channel),atom_number(Channel,ID),!,discord_say(ID,Msg).
discord_say(Channel,Msg):- \+ integer(Channel),any_to_chan_id(Channel,ID),!,discord_say(ID,Msg).
discord_say(MID,Msg):- discord_name_id_type(Name,MID,members),any_to_chan_id(Name,ID),!,discord_say_text(ID,Msg,[]).
discord_say(MID,Msg):- \+ discord_id_type(MID,channels), discord_ddd(MID,channel_id,ID), 
  (tmp:msg_refed_once(MID) -> discord_say_text(ID,Msg,[]) ;
    (assert(tmp:msg_refed_once(MID)),
     discord_say_text(ID,Msg,_{message_reference:_{message_id: MID}}))),!.
discord_say(ID,Msg):- discord_say_text(ID,Msg,[]),!.

must_split_long_message(Msg,LP,RP):- 
  atomic_list_concat(List,'\n',Msg),length(List,N),N>98,
  length(Left,97),append(Left,Right,List),
  atomics_to_string( Left,'\n',LP),
  atomics_to_string(Right,'\n',RP),!.


discord_say_text(Channel,Msg):- any_to_chan_id(Channel,ID), discord_say_text(ID,Msg, []).

discord_say_text(_Channel,Msg,_):- trim_message(Msg,Msg2), Msg2=="",!.
discord_say_text(Channel,Msg, AddMentions):- must_split_long_message(Msg,LP,RP),!, 
  discord_say_text(Channel,LP, AddMentions), discord_say_text(Channel,RP, AddMentions).
discord_say_text(Channel,Msg, AddMentions):- must_use_file_upload(Msg), !, discord_say_text_as_file(Channel,Msg, AddMentions),!.
discord_say_text(Channel,Msg, AddMentions):- backquote_multiline_string(Msg,Str), !,
   add_to_dict(_{content: Str},AddMentions,NewDict),
   discord_http(channels/Channel/messages,[post(json(NewDict))]),!.

must_use_file_upload(Str):- atom_length(Str,L),L>1500,!.
must_use_file_upload(Str):- string_list_concat(Lines,'\n',Str),!,length(Lines,L), L>7. 


discord_say_text_as_file(Channel,Msg):- any_to_chan_id(Channel,ID), discord_say_text_as_file(ID,Msg, []).

discord_say_text_as_file(Channel,Msg, AddMentions):-
 add_to_dict(_{content: ""},AddMentions,NewDict),
 tmp_file('discord_say',Tmp), atom_concat(Tmp,'.txt',File),
 setup_call_cleanup(
   setup_call_cleanup(open(File,write,O),write(O,Msg),close(O)),
   discord_say_file(Channel,File,NewDict),
   delete_file(File)),!.

% todoo fix this!
discord_say_file(ID,File,JSON):- fail,
  json_to_string(JSON,Str),
  discord_http(channels/ID/messages,[
       post([ payload_json =  Str,
              filename     =  file(File)])]),!.

discord_say_file(Channel,File,JSON):-  
 http_discord_token_string(Token),
 json_to_string(JSON,Str),
 sformat(S,
   "curl -H 'Authorization: ~w' -H 'User-Agent: DiscordBot' -F 'payload_json=~w' -F 'filename=@~w' ~w/channels/~w/messages",
   [Token,Str,File,"https://discord.com/api/v9", Channel]), 
 ignore((catch((
  shello(S,Output),
  string_to_dict(Output,Dict),
  discord_add(gateway,Dict)),E,(wdmsg(E=S),fail)))),!.

shello(S):- shello(S,Output),write('\n'),write(Output).
shello(S,Output):-
 setup_call_cleanup(
   process_create(path(bash),['-c',S],[stdout(pipe(Out))]),
   read_string(Out, _, Output),
   close(Out)).
/*
  Dict=
    _{content: StrO,
      
    %embeds: [_{ title: "Hello, Embed!", description: "This is an embedded message."},
    tts: false},
 %sformat(S,'~q',[Dict]),
 %ddbg_always(post=S),
 discord_http(channels/ID/messages,[post(json(Dict))]),!.
*/


discord_say :- discord_say('prologmud_bot_testing',"test message to prologmud_bot_testing").
discord_say0:- 
 discord_say( asserted( exists( Y2,
                                   ( info( 'XVAR_NP_X_1_1', [
                                       loc(1), pos('NP'),equals('XVAR_NP_X_1_1'),
                                       words([w(x,[alt(pos(nn)),root(x),pos(nnp),loc(1),lnks(2),txt("X"),truecase('LOWER'),link(1,'NP',r('NP',seg(1,1))),link(2,'S',r('S',seg(1,3))),lex_winfo])]), seg(1,1),phrase('NP'),size(1),lnks(1),
                                       #(r('NP',seg(1,1))),txt(["X"]),childs(0),
                                       link(1,'S',r('S',seg(1,3)))])  ,
                                     span( [ seg(1,3), phrase('S'),size(3),lnks(0),#(r('S',seg(1,3))),
                                             txt(["X","is","Y"]),childs(2),
                                             child(1,'NP',r('NP',seg(1,1))),
                                             child(2,'VP',r('VP',seg(2,3)))]) ,
                                     span( [ seg(2,3), phrase('VP'),size(2),lnks(1),
                                             #(r('VP',seg(2,3))),txt(["is","Y"]),
                                             childs(1),child(1,'VP',r('VP',seg(3,3))),
                                             link(1,'S',r('S',seg(1,3)))]) ,
                                     span( [ seg(3,3), phrase('VP'),size(1),lnks(2),
                                             #(r('VP',seg(3,3))),txt(["Y"]),childs(0),
                                             link(1,'VP',r('VP',seg(2,3))),link(2,'S',r('S',seg(1,3)))]) ,
                                     iza(Y2,'Y') ,
                                     equalsVar(Y2,'XVAR_NP_X_1_1'))))).



discord_say1:- discord_say(call(ls)).
discord_say2:- forall(discord_say2(X),discord_say2a(X)).
discord_say3:- discord_say("|| || \n spoiler\n||line 1||\n||line 2||\nexcera").


discord_say2(232781767809957888). discord_say2('#prologmud_bot_testing'). discord_say2('dmiles'). discord_say2('@dmiles').
discord_say2("dmiles"). discord_say2("@dmiles"). discord_say2('prologmud_bot_testing').
discord_say2a(X):- sformat(S,"test message to ~q.",[X]), discord_say(X,S).


as_msg_string(call(Msg),Str):- wots(SF,call(Msg)),!,as_msg_string(SF,Str).
as_msg_string(Msg,Str):- compound(Msg),wots(SF,print_tree(Msg)),!,as_msg_string(SF,Str).
as_msg_string(Msg,Str):- string(Msg),!,Msg=Str.
as_msg_string(Msg,Str):- notrace_catch(text_to_string(Msg,SF)),!,as_msg_string(SF,Str).
as_msg_string(Msg,Str):- any_to_string(Msg,SF),!,as_msg_string(SF,Str).


backquote_multiline_string(Msg,Str):- \+ atom_contains(Msg,'```'), \+ atom_contains(Msg,'||'),  atom_contains(Msg,'\n'), sformat(Str,'```~w```',[Msg]),!.
backquote_multiline_string(Msg,Msg).

%discord_say:- discord_say(_,'From Prolog').
% {"id":3,"type":"message","channel":"D3U47CE4W","text":"hi there"}

/*
discord_say(ID,Str):- atom_length(Str,L),L>20,!,
  sub_string(Str,0,100,_,Sub),
  replace_in_string(['"'='\\"'],Sub,SubR),
  sformat(S,'--boundary
Content-Disposition: form-data; name="payload_json"
Content-Type: application/json
{ "content": "~w" }
--boundary
Content-Disposition: form-data; name="file"; filename="discord_say.txt"
Content-Type:application/octet-stream
~w
--boundary--
',[SubR,Str]),
   replace_in_string(['\n'='\r\n"'],Sub,SubR),
    discord_http(channels/ID/messages,[post(S)]).
*/

:- eggdrop:import(discord_client:discord_chat_override/1).

