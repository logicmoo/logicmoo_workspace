:- if(current_prolog_flag(xref,true);(prolog_load_context(file,F),prolog_load_context(source,F))).
:- module(discord_find,[]).
:- endif.
% ====================================
% message/channel handing and retrival
% ====================================

is_guild_chan(ID):- (discord_ddd(ID,parent_id,PID),PID\==null,PID>0).

m_to_c(MID,CID):- discord_ddd(MID,channel_id,CID).
m_to_c(MID,CID):- discord_ddd(CID,last_message_id,MID).

check_seen:- forall(discord_ddd(MID,content,_),check_seen(MID)).

check_seen(MID):- discord_ddd(MID, seen, true),!.
check_seen(MID):- discord_ddd(MID, timestamp, When), get_time(Now), When + ( 86400 * 3) < Now, !. % over 3 days old
check_seen(MID):- \+ discord_ddd(MID,content,_), rtrv_message(MID),fail.
check_seen(MID):- add_seen(MID).

rtrv_message(MID):- discord_ddd(MID,content,_),!.
% rtrv_message(MID):- discord_ddd(MID,seen,true),!.
rtrv_message(MID):- m_to_c(MID,CID),!,rtrv_message(MID,CID).
rtrv_message(_):- !.

rtrv_message(MID,_):- discord_ddd(MID,content,_),!.
rtrv_message(_,_):- \+ ima_bot,!.
rtrv_message(MessageID,ChannelID):- ignore(notrace_catch(discord_http(channels/ChannelID/messages/MessageID))).

add_seen(ID):- discord_ddd(ID, seen, true),!.
add_seen(ID):- discord_addd(ID, seen, true), fail.
add_seen(ID):- m_to_c(ID,CID),discord_http(channels/CID/messages/ID/reactions/'%F0%9F%91%80'/'@me',[method(put)]).
add_unseen(ID):- m_to_c(ID,CID),discord_http(channels/CID/messages/ID/reactions/'%F0%9F%91%80'/'@me',[method(delete)]).

rtrv_new_messages :- 
 forall(
 (discord_ddd(ChannelID,last_message_id,MessageID),
  \+ discord_ddd(MessageID,content,_)),
  (sleep(0.2),rtrv_message(MessageID,ChannelID))).


% https://discord.com/oauth2/authorize?response_type=code&client_id=157730590492196864&scope=identify+guilds.join&state=15773059ghq9183habn&redirect_uri=https%3A%2F%2Fgoogle.com&prompt=consent
rtrv_messages_chan:- \+ thread_self(discord_message_checking_01), forall(any_to_chan_id(_Channel,ID),  rtrv_messages_chan(ID)).
rtrv_messages_chan(ID):- discord_ddd(ID,last_message_id,MessageID),discord_ddd(MessageID,content,_),!.
rtrv_messages_chan(ID):- discord_http(channels/ID/'messages?limit=20'),sleep(1).

discord_join_subchannel(ID):- discord_http(channels/ID/'thread-members'/'@me',[method(put)]).

% ===============================================
% Searching database state
% ===============================================
discord_is_secret(R):- compound(R),!, functor(R,_,A),arg(A,R,E),discord_is_secret(E).
discord_is_secret(T):- (atom(T);string(T)),!,atom_contains(T,"Nzc").

discord_id_type(ID,Type):- discord_ddd(ID,instanceOf,Type).

has_id(H):- get_prop(H,id,V),from_string(V,N),!,integer(N).

matches_info(S,(A;B)):-!, matches_info(S,A);matches_info(S,B).
matches_info(S,(A,B)):-!, matches_info(S,A),matches_info(S,B).
matches_info(S,Str):- sub_string(S, _Offset0, _Length, _After, Str).

name_type_string(Name,Type,S):- atom_concat(Type2,'s',Type),!,name_type_string(Name,Type2,S).
name_type_string(Name,Type,S):- atom_concat('@',Name2,Name),!,name_type_string(Name2,Type,S).
name_type_string(Name,Type,S):- atom_concat('#',Name2,Name),!,name_type_string(Name2,Type,S).
name_type_string(Name,member,S):- format(atom(S),'@~w',[Name]),!.
name_type_string(Name,role,S):- format(atom(S),'@~w',[Name]),!.
name_type_string(Name,channel,S):- format(atom(S),'#~w',[Name]),!.
name_type_string(Name,Type,S):- format(atom(S),'~w:~w',[Type,Name]).

%753344235805343785
int_to_name(S,V):- S>1420070400,get_time(T),TT is T + 6000,TT>S,int_to_date(S,VV),format(atom(V),'~w',[VV]).
%int_to_name(S,V):- discord_ddd(S,name,VV),!,sformat(V,'~w{~w}',[VV,S]).
int_to_name(S,V):- discord_name_id_type(Name,S,Type),!,name_type_string(Name,Type,VVV),
   %format(atom(V),'<~w/~w>',[VVV,S]).
   format(atom(V),'~w',[VVV]).
int_to_name(S,V):- discord_ddd(S,name,VV),!,format(atom(V),'name(~q)',[VV]).
int_to_name(S,V):- discord_ddd(S,username,VV),!,format(atom(V),'username(~q)',[VV]).
int_to_name(S,V):- discord_ddd(S,user_username,VV),!,format(atom(V),'user_username(~q)',[VV]).
%int_to_name(S,V):- discord_ddd(Vc,ontent,S),!.
%int_to_name(S,V):- % S> 4194304,  S > 1178066625, id_to_time(S,T),int_to_date(T,TT),sformat(V,'{~w?~w}',[S,TT]).

id_to_name(S,V):- discord_ddd(S,name,V).
id_to_name(S,V):- discord_ddd(S,username,V).
id_to_name(S,V):- discord_name_id_type(V,S,_Type).

discord_name_id_type(Name,ID,Type):- maybe_into_string(Name,NameS), !, discord_name_id_type(NameS,ID,Type).
discord_name_id_type(Name,ID,Type):- var(Name), nonvar(Type),  !,  discord_ddd(ID,instanceOf,Type), discord_ddd(ID,name,Name).
discord_name_id_type(Name,ID,Type):- discord_ddd(ID,name,Name), integer(ID), once(discord_id_type(ID,Type)).
discord_name_id_type(Name,ID,Type):- discord_ddd(ID,username,Name), integer(ID), once(discord_id_type(ID,Type)).

same_ids(ID,IDS):-any_to_id(ID,IDA),any_to_id(IDS,IDB),IDA==IDB.

any_to_chan_id(Channel,ID):- atom(Channel),!,atom_string(Channel,Name),!,any_to_chan_id(Name,ID).
any_to_chan_id(Channel,ID):- (var(Channel)->!;true), discord_name_id_type(Channel,ID,channels).
any_to_chan_id(Channel,ID):- string(Channel),atom_number(Channel,Name),!,any_to_chan_id(Name,ID).
any_to_chan_id(Channel,ID):- string(Channel),string_concat('#',Name,Channel),!,any_to_chan_id(Name,ID).
any_to_chan_id(Channel,ID):- string(Channel),string_concat('@',Name,Channel),!,any_to_chan_id(Name,ID).
any_to_chan_id(Channel,ID):- integer(Channel),discord_name_id_type(Name,Channel, members),!,any_to_chan_id(Name,ID).
any_to_chan_id(Channel,ID):- integer(Channel),discord_id_type(Channel,messages),discord_ddd(Channel,channel_id,ID).
any_to_chan_id(Channel,ID):- integer(Channel),discord_id_type(Channel,threads),!,Channel=ID.
any_to_chan_id(Channel,ID):- integer(Channel),discord_id_type(Channel,channels),!,Channel=ID.
any_to_chan_id(Channel,ID):- integer(Channel),!,Channel=ID. % Cache miss
% @TODO locate DMs
any_to_chan_id(Channel,ID):- discord_ddd(ID,recipient_name,Channel),!.
any_to_chan_id(Channel, _):- rtrv_dm_handle(Channel),fail.
any_to_chan_id(Channel,ID):- discord_ddd(ID,recipient_name,Channel),!.
%any_to_chan_id("prologmud_bot_testing",892806433970716692).
any_to_chan_id(Channel,ID):- any_to_id(Channel,ID),!.


any_to_id(Name,ID):-var(Name),!, fail,ID=Name.
any_to_id(Name,ID):-integer(Name),!,ID=Name.
any_to_id(Name,ID):- maybe_into_string(Name,SName), !, any_to_id(SName,ID).
any_to_id(Name,ID):- discord_name_id_type(Name,ID,_),integer(ID),!.
any_to_id(Name,ID):- from_string(Name,ID),integer(ID),!.
%any_to_id(Name,ID):-text_to_string(Name,NameS),discord_dd(_,hasValue,ID), discord_dd(ID,_,NameS),!.



